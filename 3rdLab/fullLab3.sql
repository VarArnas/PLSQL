@C:\Users\arnas\Desktop\semestras6\PLSQL\3rdLab\exceptions.sql
@C:\Users\arnas\Desktop\semestras6\PLSQL\3rdLab\tableCreation.sql
@C:\Users\arnas\Desktop\semestras6\PLSQL\3rdLab\tableIntialization.sql

CREATE OR REPLACE PACKAGE validation_pkg
IS
    FUNCTION is_marriage_succesful_by_pair(i_marriage santuoka%ROWTYPE) RETURN BOOLEAN;
    FUNCTION is_marriage_succesful_by_year(i_marriage santuoka%ROWTYPE) RETURN BOOLEAN;
END validation_pkg;
/

CREATE OR REPLACE PACKAGE BODY validation_pkg
IS
    TYPE succesful_marriages_aat IS TABLE OF sekmingaPora%ROWTYPE
                                        INDEX BY PLS_INTEGER;
    TYPE succesful_year_aat IS TABLE OF sekmingiMoteruMetai%ROWTYPE
                                        INDEX BY PLS_INTEGER;

    g_succesful_marriages_cache succesful_marriages_aat;
    g_women_succesful_years_cache succesful_year_aat;
    g_men_succesful_years_cache succesful_year_aat;

    FUNCTION find_count(i_table_name VARCHAR2) 
    RETURN PLS_INTEGER
    IS
        sql_stmt table_creation_pkg.sql_string_t := 'SELECT COUNT(*) FROM ' || i_table_name;
        l_count PLS_INTEGER;
    BEGIN
        EXECUTE IMMEDIATE sql_stmt INTO l_count;
        RETURN l_count;
    EXCEPTION
        WHEN OTHERS THEN
            exception_pkg.handle_exception(sql_stmt);
            RAISE;
    END find_count;

    PROCEDURE fetch_pairs
    IS
        CURSOR cur_succesful_marriages IS SELECT * FROM sekmingaPora; 
        l_table_name CONSTANT VARCHAR2(30) := 'sekmingaPora';
        l_count PLS_INTEGER;
    BEGIN
        l_count := find_count(l_table_name);

        <<refetch>>
        BEGIN
            IF l_count <> g_succesful_marriages_cache.COUNT THEN
                OPEN cur_succesful_marriages;
                FETCH cur_succesful_marriages BULK COLLECT INTO g_succesful_marriages_cache;
                IF cur_succesful_marriages%ISOPEN THEN CLOSE cur_succesful_marriages; END IF; 
            END IF;
        EXCEPTION
            WHEN OTHERS THEN
                exception_pkg.handle_exception;
                IF cur_succesful_marriages%ISOPEN THEN CLOSE cur_succesful_marriages; END IF;
                RAISE;
        END refetch;
    END fetch_pairs;

    --- check if pairs succesful
    FUNCTION is_marriage_succesful_by_pair(i_marriage santuoka%ROWTYPE)
    RETURN BOOLEAN
    IS
    BEGIN
        fetch_pairs;

        <<check_if_succesful>>
        DECLARE
            l_temp PLS_INTEGER := g_succesful_marriages_cache.FIRST;
        BEGIN
            WHILE l_temp IS NOT NULL
            LOOP
                IF g_succesful_marriages_cache(l_temp).vyroZenklas = i_marriage.vyroZenklas
                    AND g_succesful_marriages_cache(l_temp).motersZenklas = i_marriage.motersZenklas THEN
                     RETURN TRUE;
                END IF;
                l_temp := g_succesful_marriages_cache.NEXT(l_temp);
            END LOOP;
        END check_if_succesful;

        RETURN FALSE;
    END is_marriage_succesful_by_pair;

    PROCEDURE fetch_years(i_table_name VARCHAR2)
    IS
        sql_stmt table_creation_pkg.sql_string_t := 'SELECT * FROM ' || i_table_name;
        cur_years SYS_REFCURSOR;
        l_count PLS_INTEGER;
    BEGIN
        l_count := find_count(i_table_name);
        OPEN cur_years FOR sql_stmt;

        <<refetch>>
        BEGIN
            IF i_table_name = 'sekmingiMoteruMetai' THEN
                IF l_count <> g_women_succesful_years_cache.COUNT THEN
                    FETCH cur_years BULK COLLECT INTO g_women_succesful_years_cache;
                END IF;
            ELSE
                IF l_count <> g_men_succesful_years_cache.COUNT THEN
                    FETCH cur_years BULK COLLECT INTO g_men_succesful_years_cache;
                END IF;
            END IF;
            IF cur_years%ISOPEN THEN CLOSE cur_years; END IF; 
        EXCEPTION
            WHEN OTHERS THEN
                exception_pkg.handle_exception(sql_stmt);
                IF cur_years%ISOPEN THEN CLOSE cur_years; END IF;
                RAISE;
        END refetch;
    END fetch_years;

    FUNCTION check_years(i_succesful_years succesful_year_aat, i_year NUMBER, i_sign santuoka.motersZenklas%TYPE) RETURN BOOLEAN
    IS
        l_index PLS_INTEGER := i_succesful_years.FIRST; 
    BEGIN
        WHILE l_index IS NOT NULL
        LOOP
            IF i_succesful_years(l_index).metai = i_year
                AND i_succesful_years(l_index).zenklas = i_sign THEN
                    RETURN TRUE;
            END IF;
            l_index := i_succesful_years.NEXT(l_index);
        END LOOP;
        RETURN FALSE;  
    END check_years;

    -- check if years succesful
    FUNCTION is_marriage_succesful_by_year(i_marriage santuoka%ROWTYPE)
    RETURN BOOLEAN
    IS
        l_marriage_year CONSTANT NUMBER(4) := EXTRACT(YEAR FROM i_marriage.registracijosData);
    BEGIN
        fetch_years('sekmingiMoteruMetai');
        fetch_years('sekmingiVyruMetai');

        IF NOT check_years(g_men_succesful_years_cache, l_marriage_year, i_marriage.vyroZenklas) THEN RETURN FALSE; END IF;
        IF NOT check_years(g_women_succesful_years_cache, l_marriage_year, i_marriage.motersZenklas) THEN RETURN FALSE; END IF;

        RETURN TRUE;
    END is_marriage_succesful_by_year;
END validation_pkg;
/

CREATE OR REPLACE PACKAGE marriage_operations_pkg
IS
    TYPE deletion_aat IS TABLE OF PLS_INTEGER
                            INDEX BY VARCHAR2(128);

    PROCEDURE insert_marriage(i_marriage santuoka%ROWTYPE);
    PROCEDURE save_succesful_marriages;
    FUNCTION remove_unsuccesful_marriages RETURN deletion_aat;
END marriage_operations_pkg;
/

CREATE OR REPLACE PACKAGE BODY marriage_operations_pkg
IS
    PROCEDURE insert_marriage(i_marriage santuoka%ROWTYPE)
    IS
    BEGIN
        IF validation_pkg.is_marriage_succesful_by_pair(i_marriage) AND validation_pkg.is_marriage_succesful_by_year(i_marriage) THEN
            INSERT INTO santuoka VALUES i_marriage;
        END IF;
    END insert_marriage;

    PROCEDURE save_succesful_marriages
    IS
        l_marriages table_initialization_pkg.marriage_nt := table_initialization_pkg.marriage_nt();
        CURSOR cur_marriages IS SELECT * FROM santuoka;
        c_limit CONSTANT PLS_INTEGER := 100;
    BEGIN
        OPEN cur_marriages;
        LOOP
            FETCH cur_marriages BULK COLLECT INTO l_marriages LIMIT c_limit;
            EXIT WHEN l_marriages.COUNT = 0;

            <<validate_marriages>>
            DECLARE
                l_index PLS_INTEGER := l_marriages.FIRST;
            BEGIN
                WHILE l_index IS NOT NULL
                LOOP
                    IF NOT (validation_pkg.is_marriage_succesful_by_pair(l_marriages(l_index)) AND validation_pkg.is_marriage_succesful_by_year(l_marriages(l_index))) THEN
                        l_marriages.DELETE(l_index);
                    END IF;
                    l_index := l_marriages.NEXT(l_index);
                END LOOP;
            END validate_marriages;

            <<save_marriages>>
            BEGIN
                FORALL i IN INDICES OF l_marriages SAVE EXCEPTIONS
                    INSERT INTO sekmingaSantuoka VALUES (sekmingaSantuoka_seq.NEXTVAL, l_marriages(i).registracijosData, l_marriages(i).motersZenklas, l_marriages(i).vyroZenklas);
            EXCEPTION
                WHEN exception_pkg.exec_dml_errors THEN
                    DBMS_OUTPUT.PUT_LINE ('During sekmingaSantuoka insertion multiple exceptions were raised');
                    FOR i in 1.. SQL%BULK_EXCEPTIONS.COUNT
                    LOOP
                        DBMS_OUTPUT.PUT_LINE(SQL%BULK_EXCEPTIONS(i).ERROR_INDEX);
                        DBMS_OUTPUT.PUT_LINE(SQL%BULK_EXCEPTIONS(i).ERROR_CODE);
                        DBMS_OUTPUT.PUT_LINE(SQLERRM(-1*SQL%BULK_EXCEPTIONS(i).ERROR_CODE));
                    END LOOP;
                    RAISE;
                WHEN OTHERS THEN
                    exception_pkg.handle_exception;
                    RAISE;
            END save_marriages;
        END LOOP;
        IF cur_marriages%ISOPEN THEN CLOSE cur_marriages; END IF; 
    EXCEPTION
            WHEN OTHERS THEN
                exception_pkg.handle_exception;
                IF cur_marriages%ISOPEN THEN CLOSE cur_marriages; END IF;
                RAISE;
    END save_succesful_marriages;

    FUNCTION remove_unsuccesful_marriages
    RETURN deletion_aat
    IS
        l_marriages table_initialization_pkg.marriage_nt := table_initialization_pkg.marriage_nt();
        CURSOR cur_marriages IS SELECT * FROM santuoka;
        c_limit CONSTANT PLS_INTEGER := 100;
        l_deletions deletion_aat;
    BEGIN
        l_deletions('Santuoka') := 0;
        OPEN cur_marriages;
        LOOP
            FETCH cur_marriages BULK COLLECT INTO l_marriages LIMIT c_limit;
            EXIT WHEN l_marriages.COUNT = 0;

            <<keep_bad_marriages>>
            DECLARE
                l_index PLS_INTEGER := l_marriages.FIRST;
            BEGIN
                WHILE l_index IS NOT NULL
                LOOP
                    IF validation_pkg.is_marriage_succesful_by_pair(l_marriages(l_index)) AND validation_pkg.is_marriage_succesful_by_year(l_marriages(l_index)) THEN
                        l_marriages.DELETE(l_index);
                    END IF;
                    l_index := l_marriages.NEXT(l_index);
                END LOOP;
            END keep_bad_marriages;

            <<delete_marriages>>
            BEGIN
                FORALL i IN INDICES OF l_marriages SAVE EXCEPTIONS
                    DELETE FROM Santuoka WHERE numeris = l_marriages(i).numeris;
                l_deletions('Santuoka') := l_deletions('Santuoka') + SQL%ROWCOUNT;
            EXCEPTION
                WHEN exception_pkg.exec_dml_errors THEN
                    DBMS_OUTPUT.PUT_LINE ('During sekmingaSantuoka deletion multiple exceptions were raised');
                    FOR i in 1.. SQL%BULK_EXCEPTIONS.COUNT
                    LOOP
                        DBMS_OUTPUT.PUT_LINE(SQL%BULK_EXCEPTIONS(i).ERROR_INDEX);
                        DBMS_OUTPUT.PUT_LINE(SQL%BULK_EXCEPTIONS(i).ERROR_CODE);
                        DBMS_OUTPUT.PUT_LINE(SQLERRM(-1*SQL%BULK_EXCEPTIONS(i).ERROR_CODE));
                    END LOOP;
                    RAISE;
                WHEN OTHERS THEN
                    exception_pkg.handle_exception;
                    RAISE;
            END delete_marriages;
        END LOOP;
        IF cur_marriages%ISOPEN THEN CLOSE cur_marriages; END IF; 

        RETURN l_deletions;
    EXCEPTION
        WHEN OTHERS THEN
                exception_pkg.handle_exception;
                IF cur_marriages%ISOPEN THEN CLOSE cur_marriages; END IF;
                RAISE;
    END remove_unsuccesful_marriages;
END marriage_operations_pkg;
/

<<do_task>>
DECLARE
    l_index VARCHAR2(128);
    l_deleted marriage_operations_pkg.deletion_aat;
BEGIN
    marriage_operations_pkg.save_succesful_marriages;
    l_deleted := marriage_operations_pkg.remove_unsuccesful_marriages;
    l_index := l_deleted.FIRST;
    WHILE l_index IS NOT NULL
    LOOP
        DBMS_OUTPUT.PUT_LINE('From table ' || l_index || ', ' || l_deleted(l_index) || ' rows were deleted.');
        l_index := l_deleted.NEXT(l_index);
    END LOOP;
END do_task;
/