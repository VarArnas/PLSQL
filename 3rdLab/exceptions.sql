CREATE OR REPLACE PACKAGE exception_pkg
IS
    c_array_sizes_do_not_match CONSTANT NUMBER := -20001;
    c_array_sizes_do_not_match_message CONSTANT error_log.error_message%TYPE := 'Two arrays were passed for search efficiencies, which sizes do not match';
    exec_array_sizes_do_not_match EXCEPTION;

    c_non_existent_search CONSTANT NUMBER := -20002;
    c_non_existent_search_message CONSTANT error_log.error_message%TYPE := 'A non existent search option was given for getting search efficiencies';
    exec_non_existent_search EXCEPTION;

    c_empty_array_passed CONSTANT NUMBER := -20003;
    c_empty_array_message CONSTANT error_log.error_message%TYPE := 'The passed in array did not have any elements';
    exec_empty_array_passed EXCEPTION;

    exec_dml_errors EXCEPTION;
    c_dml_errors CONSTANT NUMBER := -24831;

    PRAGMA EXCEPTION_INIT(exec_array_sizes_do_not_match, c_array_sizes_do_not_match);
    PRAGMA EXCEPTION_INIT(exec_non_existent_search, c_non_existent_search);
    PRAGMA EXCEPTION_INIT(exec_empty_array_passed, c_empty_array_passed);
    PRAGMA EXCEPTION_INIT(exec_dml_errors, c_dml_errors);

    c_default_message CONSTANT error_log.error_message%TYPE := 'An exception was raised which does not exist';

    PROCEDURE handle_exception(i_sql_string VARCHAR2 DEFAULT NULL);
END exception_pkg;
/

CREATE OR REPLACE PACKAGE BODY exception_pkg
IS
    TYPE l_backtrace_rt IS RECORD(
        l_owner error_log.package_owner%TYPE,
        l_package error_log.package_name%TYPE,
        l_line_number error_log.line_number%TYPE
    );

    PROCEDURE save_exception(i_exception error_log%ROWTYPE)
    IS
        PRAGMA AUTONOMOUS_TRANSACTION;
    BEGIN
        INSERT INTO error_log VALUES i_exception;
        COMMIT;
    EXCEPTION 
        WHEN OTHERS THEN
            RAISE;
    END;

    FUNCTION map_data_to_error_log(i_code IN error_log.error_code%TYPE,
            i_msg IN error_log.error_message%TYPE,
            i_backtrace IN l_backtrace_rt,
            i_username IN error_log.username%TYPE,
            i_time IN error_log.error_time%TYPE,
            i_sql_string IN VARCHAR2)
    RETURN error_log%ROWTYPE
    IS
        l_exception error_log%ROWTYPE;
        l_currval PLS_INTEGER;
    BEGIN
        SELECT error_log_seq.NEXTVAL INTO l_currval FROM DUAL;
        l_exception.error_id := l_currval; 
        l_exception.error_code := i_code;
        l_exception.error_message := i_msg;
        l_exception.package_owner := i_backtrace.l_owner;
        l_exception.package_name := i_backtrace.l_package;
        l_exception.line_number := i_backtrace.l_line_number;
        l_exception.username := i_username;
        l_exception.error_time := i_time;
        IF i_sql_string IS NOT NULL THEN l_exception.sql_string := i_sql_string; END IF;
        RETURN l_exception;
    END;

    FUNCTION get_error_message(i_code IN PLS_INTEGER) 
    RETURN error_log.error_message%TYPE
    IS
        l_err_msg error_log.error_message%TYPE; 
    BEGIN
         l_err_msg :=
            CASE 
                WHEN i_code = -20001 THEN c_array_sizes_do_not_match_message
                WHEN i_code = -20002 THEN c_non_existent_search_message
                WHEN i_code = -20003 THEN c_empty_array_message
                ELSE SQLERRM(i_code)
            END;
            RETURN l_err_msg;
    END;

    FUNCTION parse_backtrace(i_backtrace IN VARCHAR2)
    RETURN l_backtrace_rt
    IS
        SUBTYPE l_backtrace_line_t IS VARCHAR2(4000);
        l_backtrace l_backtrace_rt;
        l_first_line l_backtrace_line_t;
    BEGIN
        l_first_line := REGEXP_SUBSTR(i_backtrace, '^.*(at ".*", line [0-9]+)', 1, 1, NULL, 1);
        l_backtrace.l_owner := REGEXP_SUBSTR(l_first_line, 'at "([^\.]+)\.', 1, 1, NULL, 1);
        l_backtrace.l_package := REGEXP_SUBSTR(l_first_line, 'at "[^\.]+\.([^"]+)"', 1, 1, NULL, 1);
        l_backtrace.l_line_number := TO_NUMBER(REGEXP_SUBSTR(l_first_line, 'line ([0-9]+)', 1, 1, NULL, 1));
        RETURN l_backtrace;
    END;

    PROCEDURE handle_exception(i_sql_string VARCHAR2 DEFAULT NULL)
    IS
        l_err_msg error_log.error_message%TYPE;
        l_backtrace l_backtrace_rt;
        l_exception error_log%ROWTYPE;
        SUBTYPE l_backtrace_t IS VARCHAR2(4000);

        curr_backtrace l_backtrace_t := DBMS_UTILITY.FORMAT_ERROR_BACKTRACE;
        l_code PLS_INTEGER := SQLCODE;
        l_user error_log.username%TYPE := USER;
        l_date error_log.error_time%TYPE := SYSDATE;
    BEGIN
        l_err_msg := get_error_message(l_code);
        l_backtrace := parse_backtrace(curr_backtrace);
        l_exception := map_data_to_error_log(l_code, l_err_msg, l_backtrace, l_user, l_date, i_sql_string);
        save_exception(l_exception);
    END;
END exception_pkg;
/