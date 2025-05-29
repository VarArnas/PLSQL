ALTER SESSION SET NLS_DATE_FORMAT = 'DD-MON-YYYY';

--------------table initialization package------------

CREATE OR REPLACE PACKAGE table_initialization_pkg
AUTHID CURRENT_USER
IS
    TYPE insert_values_nt IS TABLE OF VARCHAR2(4000);
    TYPE sign_name_nt IS TABLE OF VARCHAR2(128);
    TYPE pairs_nt IS TABLE OF sekmingaPora%ROWTYPE;
    TYPE signs_nt IS TABLE OF zenklas%ROWTYPE;
    TYPE years_nt IS TABLE OF sekmingiVyruMetai%ROWTYPE;
    TYPE marriage_nt IS TABLE OF santuoka%ROWTYPE;

    c_default_sign_values CONSTANT sign_name_nt := sign_name_nt('Aries', 'Taurus', 'Gemini', 'Cancer', 'Leo', 'Virgo', 'Libra', 'Scorpio', 'Sagittarius', 'Capricorn', 'Aquarius', 'Pisces');
    c_min_year CONSTANT NUMBER(4) := 2020;
    c_max_year CONSTANT NUMBER(4) := 2025;
    c_start_date CONSTANT DATE := TO_DATE('01-JAN-2020', 'DD-MON-YYYY');
    
    PROCEDURE dynamic_insert(i_table_name VARCHAR2, i_column_values insert_values_nt);
    FUNCTION create_random_sekmingos_poros(i_amount PLS_INTEGER) RETURN pairs_nt;
    FUNCTION create_random_sekmingi_metai(i_amount PLS_INTEGER, i_table_name VARCHAR2) RETURN years_nt;
    FUNCTION create_random_santuoka(i_amount PLS_INTEGER) RETURN marriage_nt;
END table_initialization_pkg;
/

CREATE OR REPLACE PACKAGE BODY table_initialization_pkg
IS
    l_signs sign_name_nt;

    PROCEDURE fetch_signs 
    IS
        CURSOR cur_signs IS SELECT zenklas FROM zenklas;
    BEGIN
        IF l_signs IS NULL THEN
            OPEN cur_signs;
            FETCH cur_signs BULK COLLECT INTO l_signs;
            CLOSE cur_signs;
        END IF;
    EXCEPTION
        WHEN OTHERS THEN
            exception_pkg.handle_exception;
            IF cur_signs%ISOPEN THEN CLOSE cur_signs; END IF;
            RAISE;
    END fetch_signs;

    FUNCTION create_random_santuoka(i_amount PLS_INTEGER) 
    RETURN marriage_nt
    IS
        l_marriages marriage_nt := marriage_nt();
    BEGIN
        fetch_signs;

        <<generate_random_years>>
        DECLARE
            l_currval NUMBER;
        BEGIN
            FOR indx IN 1..i_amount
            LOOP
                l_marriages.EXTEND;
                SELECT santuoka_seq.NEXTVAL INTO l_currval FROM dual;
                l_marriages(indx).numeris := l_currval;
                l_marriages(indx).registracijosData := c_start_date + TRUNC(DBMS_RANDOM.VALUE(0, SYSDATE - c_start_date + 1));
                l_marriages(indx).motersZenklas := l_signs(TRUNC(DBMS_RANDOM.VALUE(1, l_signs.COUNT + 1)));
                l_marriages(indx).vyroZenklas := l_signs(TRUNC(DBMS_RANDOM.VALUE(1, l_signs.COUNT + 1)));
            END LOOP;
        END generate_random_years;

        RETURN l_marriages;
    END create_random_santuoka;

    FUNCTION create_random_sekmingi_metai(i_amount PLS_INTEGER, i_table_name VARCHAR2) 
    RETURN years_nt
    IS
        l_years years_nt := years_nt();
    BEGIN
        fetch_signs;

        <<generate_random_years>>
        DECLARE
            l_currval NUMBER;
            sql_stmt table_creation_pkg.sql_string_t := 'SELECT ' || i_table_name || '_seq.NEXTVAL FROM dual';
        BEGIN
            FOR indx IN 1..i_amount
            LOOP
                l_years.EXTEND;
                EXECUTE IMMEDIATE sql_stmt INTO l_currval;
                l_years(indx).numeris := l_currval;
                l_years(indx).metai := TRUNC(DBMS_RANDOM.VALUE(c_min_year, c_max_year + 1));
                l_years(indx).zenklas := l_signs(TRUNC(DBMS_RANDOM.VALUE(1, l_signs.COUNT + 1)));
            END LOOP;
        EXCEPTION
            WHEN OTHERS THEN
                exception_pkg.handle_exception(sql_stmt);
                RAISE; 
        END generate_random_years;

        RETURN l_years;
    END create_random_sekmingi_metai;

    FUNCTION create_random_sekmingos_poros(i_amount PLS_INTEGER)
    RETURN pairs_nt
    IS
        l_pairs pairs_nt := pairs_nt();
    BEGIN
        fetch_signs;

        <<generate_random_poros>>
        DECLARE
            l_currval NUMBER;
        BEGIN
            FOR indx IN 1..i_amount 
            LOOP
                l_pairs.EXTEND;
                SELECT sekmingaPora_seq.NEXTVAL INTO l_currval from dual;
                l_pairs(indx).numeris := l_currval;
                l_pairs(indx).motersZenklas :=  l_signs(TRUNC(DBMS_RANDOM.VALUE(1, l_signs.COUNT + 1)));
                l_pairs(indx).vyroZenklas := l_signs(TRUNC(DBMS_RANDOM.VALUE(1, l_signs.COUNT + 1)));
            END LOOP;
        END generate_random_poros;

        RETURN l_pairs;
    END;

    PROCEDURE dynamic_insert(
        i_table_name VARCHAR2,
        i_column_values insert_values_nt
    ) IS
        l_cursor      INTEGER;
        l_columns     insert_values_nt;
        l_result      INTEGER;
        l_sql         table_creation_pkg.sql_string_t;
        l_col_count   NUMBER;
        l_batch_count NUMBER;
        CURSOR cur_column_names(p_table_name IN VARCHAR2) IS SELECT column_name
                                                            FROM user_tab_columns
                                                            WHERE table_name = UPPER(p_table_name)
                                                            ORDER BY column_id; 
    BEGIN
        <<get_column_names>>
        BEGIN
            OPEN cur_column_names(i_table_name);
            FETCH cur_column_names BULK COLLECT INTO l_columns;
            CLOSE cur_column_names;
        EXCEPTION
            WHEN OTHERS THEN
                exception_pkg.handle_exception;
                IF cur_column_names%ISOPEN THEN
                    CLOSE cur_column_names;
                END IF;
                RAISE;
        END get_column_names;
        
        l_col_count := l_columns.COUNT;
        l_batch_count := i_column_values.COUNT / l_columns.COUNT;
        
        l_sql := 'INSERT ALL ';
        <<construct_row_insertion>>
        DECLARE
            l_start PLS_INTEGER := 1;
            l_end PLS_INTEGER := l_col_count;
        BEGIN
            <<iterate_each_row>>
            FOR indx IN 1..l_batch_count
            LOOP
                l_sql := l_sql || 'INTO ' || i_table_name || ' VALUES (';

                <<iterate_each_column>>
                FOR i IN l_start..l_end LOOP
                    l_sql := l_sql || ':' || i;
                    IF i < l_end THEN l_sql := l_sql || ','; END IF;
                END LOOP iterate_each_column;

                l_start := l_end + 1;
                l_end := l_end + l_col_count;
                l_sql := l_sql || ') ';
            END LOOP iterate_each_row;
        END construct_row_insertion;
        l_sql := l_sql || ' SELECT * FROM dual';
        
        l_cursor := DBMS_SQL.OPEN_CURSOR;
        DBMS_SQL.PARSE(l_cursor, l_sql, DBMS_SQL.NATIVE);
        
        FOR i IN 1..i_column_values.COUNT LOOP
            DBMS_SQL.BIND_VARIABLE(l_cursor, ':' || i, i_column_values(i));
        END LOOP;
        
        l_result := DBMS_SQL.EXECUTE(l_cursor);
        DBMS_SQL.CLOSE_CURSOR(l_cursor);    
    EXCEPTION
        WHEN OTHERS THEN
            IF DBMS_SQL.IS_OPEN(l_cursor) THEN
                DBMS_SQL.CLOSE_CURSOR(l_cursor);
            END IF;
            RAISE;
    END dynamic_insert;
END table_initialization_pkg;
/

<<do_task>>
DECLARE
    l_values table_initialization_pkg.insert_values_nt;
    l_pairs table_initialization_pkg.pairs_nt := table_initialization_pkg.pairs_nt(); 
    l_years_men table_initialization_pkg.years_nt := table_initialization_pkg.years_nt();
    l_years_women table_initialization_pkg.years_nt := table_initialization_pkg.years_nt();
    l_marriages table_initialization_pkg.marriage_nt := table_initialization_pkg.marriage_nt();
BEGIN

    -- initialize Zenklas
    l_values := table_initialization_pkg.insert_values_nt();
    l_values.EXTEND(table_initialization_pkg.c_default_sign_values.COUNT*2);
    FOR indx IN 1..table_initialization_pkg.c_default_sign_values.COUNT LOOP
        l_values((indx-1)*2 + 1) := table_initialization_pkg.c_default_sign_values(indx);
        l_values((indx-1)*2 + 2) := 'Something about this sign';
    END LOOP;
    table_initialization_pkg.dynamic_insert('Zenklas', l_values);

    -- initialize sekmingaPora
    l_pairs := table_initialization_pkg.create_random_sekmingos_poros(100);
    l_values := table_initialization_pkg.insert_values_nt();
    l_values.EXTEND(l_pairs.COUNT*3);
    FOR indx IN 1..l_pairs.COUNT LOOP
        l_values((indx-1)*3 + 1) := l_pairs(indx).numeris;
        l_values((indx-1)*3 + 2) := l_pairs(indx).motersZenklas;
        l_values((indx-1)*3 + 3) := l_pairs(indx).vyroZenklas;
    END LOOP;
    table_initialization_pkg.dynamic_insert('sekmingaPora', l_values);

    -- initialize sekmingiVyruMetai
    l_years_men := table_initialization_pkg.create_random_sekmingi_metai(50, 'sekmingiVyruMetai');
    l_values := table_initialization_pkg.insert_values_nt();
    l_values.EXTEND(l_years_men.COUNT*3);
    FOR indx IN 1..l_years_men.COUNT LOOP
        l_values((indx-1)*3 + 1) := l_years_men(indx).numeris;
        l_values((indx-1)*3 + 2) := l_years_men(indx).metai;
        l_values((indx-1)*3 + 3) := l_years_men(indx).zenklas;
    END LOOP;
    table_initialization_pkg.dynamic_insert('sekmingiVyruMetai', l_values);

    -- initialize sekmingiMoteruMetai
    l_years_women := table_initialization_pkg.create_random_sekmingi_metai(50, 'sekmingiMoteruMetai');
    l_values := table_initialization_pkg.insert_values_nt();
    l_values.EXTEND(l_years_women.COUNT*3);
    FOR indx IN 1..l_years_women.COUNT LOOP
        l_values((indx-1)*3 + 1) := l_years_women(indx).numeris;
        l_values((indx-1)*3 + 2) := l_years_women(indx).metai;
        l_values((indx-1)*3 + 3) := l_years_women(indx).zenklas;
    END LOOP;
    table_initialization_pkg.dynamic_insert('sekmingiMoteruMetai', l_values);

    -- initialize Santuoka
    l_marriages := table_initialization_pkg.create_random_santuoka(100);
    l_values := table_initialization_pkg.insert_values_nt();
    l_values.EXTEND(l_marriages.COUNT*4);
    FOR indx IN 1..l_marriages.COUNT LOOP
        l_values((indx-1)*4 + 1) := l_marriages(indx).numeris;
        l_values((indx-1)*4 + 2) := TO_CHAR(l_marriages(indx).registracijosData, 'DD-MON-YYYY');
        l_values((indx-1)*4 + 3) := l_marriages(indx).motersZenklas;
        l_values((indx-1)*4 + 4) := l_marriages(indx).vyroZenklas;
    END LOOP;
    table_initialization_pkg.dynamic_insert('santuoka', l_values);
END do_task;
/
 