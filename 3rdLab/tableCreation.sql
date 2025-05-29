SET SERVEROUTPUT ON

---------------------------table creation pkg---------------------------------

CREATE OR REPLACE PACKAGE table_creation_pkg
AUTHID CURRENT_USER
IS 
    SUBTYPE sql_string_t IS VARCHAR2(32767);

    TYPE g_table_create_row_rt IS RECORD(
        g_column_name VARCHAR2(128) NOT NULL DEFAULT ' ',
        g_column_type VARCHAR2(128) NOT NULL DEFAULT ' ',
        g_column_null_constraint VARCHAR2(128) DEFAULT ' ',
        g_column_fk_constraint VARCHAR2(128) DEFAULT ' '
    );

    TYPE g_table_create_nt IS TABLE OF g_table_create_row_rt;

    c_default_pk CONSTANT g_table_create_row_rt := g_table_create_row_rt('numeris', 'NUMBER');

    PROCEDURE create_table(i_table_rows g_table_create_nt, 
                            i_table_name VARCHAR2,
                            i_pk g_table_create_row_rt DEFAULT c_default_pk);

    PROCEDURE create_seq(i_table_name VARCHAR2);    
END table_creation_pkg;
/

CREATE OR REPLACE PACKAGE BODY table_creation_pkg
IS

    PROCEDURE check_if_exists(i_object_name VARCHAR2, i_type_name VARCHAR2) 
    IS
        v_object_exists NUMBER;
        sql_drop_string VARCHAR2(200);
        sql_stmt sql_string_t := 'SELECT COUNT(*) FROM USER_' || 
                                i_type_name || 
                                'S WHERE ' || 
                                i_type_name || 
                                '_NAME = UPPER(:name)';
    BEGIN
        <<find_tables>>
        BEGIN
            EXECUTE IMMEDIATE sql_stmt
            INTO v_object_exists
            USING i_object_name; 
        EXCEPTION
            WHEN OTHERS THEN
                exception_pkg.handle_exception(sql_stmt);
                RAISE;
        END find_tables;

        <<drop_if_exists>>
        IF v_object_exists = 1 THEN
            sql_drop_string := 'DROP ' || i_type_name || ' ' || i_object_name;

            <<check_if_table>>
            IF i_type_name = 'TABLE' THEN
                sql_drop_string := sql_drop_string || ' CASCADE CONSTRAINTS';
            END IF check_if_table;

            <<drop_object>>
            BEGIN
                EXECUTE IMMEDIATE sql_drop_string;
            EXCEPTION
                WHEN OTHERS THEN
                    exception_pkg.handle_exception(sql_drop_string);
                    RAISE;
            END drop_object;
        END IF drop_if_exists;
    END check_if_exists;

    PROCEDURE create_table(i_table_rows g_table_create_nt, i_table_name VARCHAR2, i_pk g_table_create_row_rt DEFAULT c_default_pk)
    IS
        PRAGMA AUTONOMOUS_TRANSACTION;
        l_create_table_sql_string sql_string_t := 'CREATE TABLE ' || i_table_name || ' ( ';
    BEGIN
        check_if_exists(i_table_name, 'TABLE');
        l_create_table_sql_string := l_create_table_sql_string || 
                                        i_pk.g_column_name || ' ' || 
                                        i_pk.g_column_type || ' PRIMARY KEY ';
        <<populate_sql_string>>
        FOR indx IN i_table_rows.FIRST..i_table_rows.LAST
        LOOP
            IF i_table_rows(indx).g_column_name <> ' ' THEN
                l_create_table_sql_string := l_create_table_sql_string || ', ' || 
                                                i_table_rows(indx).g_column_name || ' ' || 
                                                i_table_rows(indx).g_column_type || ' ' || 
                                                i_table_rows(indx).g_column_null_constraint || ' ' || 
                                                i_table_rows(indx).g_column_fk_constraint || ' ';
            END IF;
        END LOOP populate_sql_string;

        l_create_table_sql_string := l_create_table_sql_string || ' )';

        EXECUTE IMMEDIATE l_create_table_sql_string;
        COMMIT;
    EXCEPTION
        WHEN OTHERS THEN
            exception_pkg.handle_exception(l_create_table_sql_string);
            RAISE;
    END create_table;

    PROCEDURE create_seq(i_table_name VARCHAR2) 
    IS
        PRAGMA AUTONOMOUS_TRANSACTION;
        l_sql_string sql_string_t := 'CREATE SEQUENCE ' || i_table_name || '_seq INCREMENT BY 1';
    BEGIN
        check_if_exists(i_table_name || '_seq', 'SEQUENCE');
        EXECUTE IMMEDIATE l_sql_string;
    EXCEPTION
        WHEN OTHERS THEN
            exception_pkg.handle_exception(l_sql_string);
            RAISE;
    END create_seq;
END table_creation_pkg;
/

<<do_task>>
DECLARE
    l_table_rows table_creation_pkg.g_table_create_nt := table_creation_pkg.g_table_create_nt();
    l_temp table_creation_pkg.g_table_create_row_rt;
BEGIN
    l_table_rows.EXTEND(8);

    -- Zenklas creation
    l_table_rows(1) := table_creation_pkg.g_table_create_row_rt('aprasas', 'VARCHAR2(1000)', 'NOT NULL');
    l_temp := table_creation_pkg.g_table_create_row_rt('zenklas', 'VARCHAR2(128)');
    table_creation_pkg.create_table(l_table_rows, 'Zenklas', l_temp);

    -- sekmingiVyruMetai creation
    l_table_rows(1) := table_creation_pkg.g_table_create_row_rt('metai', 'NUMBER(4)', 'NOT NULL');
    l_table_rows(2) := table_creation_pkg.g_table_create_row_rt('zenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    table_creation_pkg.create_table(l_table_rows, 'sekmingiVyruMetai');
    table_creation_pkg.create_seq('sekmingiVyruMetai');

    -- sekmingiMoteruMetai creation
    table_creation_pkg.create_table(l_table_rows, 'sekmingiMoteruMetai');
    table_creation_pkg.create_seq('sekmingiMoteruMetai');

    -- sekmingaPora creation
    l_table_rows(1) := table_creation_pkg.g_table_create_row_rt('motersZenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    l_table_rows(2) := table_creation_pkg.g_table_create_row_rt('vyroZenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    table_creation_pkg.create_table(l_table_rows, 'sekmingaPora');
    table_creation_pkg.create_seq('sekmingaPora');

    -- Santuoka creation
    l_table_rows(1) := table_creation_pkg.g_table_create_row_rt('registracijosData', 'DATE', 'NOT NULL');
    l_table_rows(2) := table_creation_pkg.g_table_create_row_rt('motersZenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    l_table_rows(3) := table_creation_pkg.g_table_create_row_rt('vyroZenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    table_creation_pkg.create_table(l_table_rows, 'Santuoka');
    table_creation_pkg.create_seq('Santuoka');

    -- sekmingaSantuoka creation
    l_table_rows(1) := table_creation_pkg.g_table_create_row_rt('registracijosData', 'DATE', 'NOT NULL');
    l_table_rows(2) := table_creation_pkg.g_table_create_row_rt('motersZenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    l_table_rows(3) := table_creation_pkg.g_table_create_row_rt('vyroZenklas', 'VARCHAR2(128)', 'NOT NULL', 'REFERENCES Zenklas(zenklas)');
    table_creation_pkg.create_table(l_table_rows, 'sekmingaSantuoka');
    table_creation_pkg.create_seq('sekmingaSantuoka');
END do_task;
/