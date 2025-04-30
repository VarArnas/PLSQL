SET SERVEROUTPUT ON

-------------------------GLOBAL TYPES PACKAGE-------------------------------

CREATE OR REPLACE PACKAGE global_types_consts_pkg
IS
    c_precision CONSTANT NUMBER := 5; 
    c_scale CONSTANT NUMBER := 2; 

    SUBTYPE g_random_number_elem_t IS NUMBER(c_precision, c_scale);
    SUBTYPE g_average_search_t IS NUMBER(38,2);
    SUBTYPE g_search_option_t IS VARCHAR2(20);

    TYPE g_random_number_nt IS TABLE OF g_random_number_elem_t;
    TYPE g_average_search_aat IS TABLE OF g_average_search_t
                            INDEX BY g_search_option_t;
    TYPE g_search_option_aat IS TABLE OF PLS_INTEGER
                            INDEX BY g_search_option_t;

    c_existing_searches CONSTANT g_search_option_aat := g_search_option_aat('binary' => 1, 'linear' => 2);
END global_types_consts_pkg;
/

-----------------EXCEPTION PACKAGE-------------------

CREATE OR REPLACE PACKAGE exception_pkg
IS
    c_non_positive_input CONSTANT NUMBER := -20001;
    c_non_positive_input_message CONSTANT error_log.error_message%TYPE := 'A non positive number was given as random unique array size';
    exec_non_positive_input EXCEPTION;

    c_non_existent_search CONSTANT NUMBER := -20002;
    c_non_existent_search_message CONSTANT error_log.error_message%TYPE := 'A non existent search option was given for getting search efficiencies';
    exec_non_existent_search EXCEPTION;

    c_empty_array_passed CONSTANT NUMBER := -20003;
    c_empty_array_message CONSTANT error_log.error_message%TYPE := 'The passed in array did not have any elements';
    exec_empty_array_passed EXCEPTION;

    PRAGMA EXCEPTION_INIT(exec_non_positive_input, c_non_positive_input);
    PRAGMA EXCEPTION_INIT(exec_non_existent_search, c_non_existent_search);
    PRAGMA EXCEPTION_INIT(exec_empty_array_passed, c_empty_array_passed);

    c_default_message CONSTANT error_log.error_message%TYPE := 'An exception was raised which does not exist';

    PROCEDURE handle_exception(i_backtrace IN VARCHAR2, i_code IN PLS_INTEGER, i_user IN VARCHAR2, i_date IN DATE);
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
        INSERT INTO error_log(
            error_code,
            error_message, 
            package_owner,
            package_name, 
            line_number, 
            username, 
            error_time
        )
        VALUES (
            i_exception.error_code, 
            i_exception.error_message, 
            i_exception.package_owner, 
            i_exception.package_name, 
            i_exception.line_number, 
            i_exception.username, 
            i_exception.error_time
        );

        COMMIT;
    EXCEPTION 
        WHEN OTHERS THEN
            ROLLBACK;
            RAISE;
    END;

    FUNCTION map_data_to_error_log(i_code IN error_log.error_code%TYPE,
            i_msg IN error_log.error_message%TYPE,
            i_backtrace IN l_backtrace_rt,
            i_username IN error_log.username%TYPE,
            i_time IN error_log.error_time%TYPE)
    RETURN error_log%ROWTYPE
    IS
        l_exception error_log%ROWTYPE;
    BEGIN
        l_exception.error_code := i_code;
        l_exception.error_message := i_msg;
        l_exception.package_owner := i_backtrace.l_owner;
        l_exception.package_name := i_backtrace.l_package;
        l_exception.line_number := i_backtrace.l_line_number;
        l_exception.username := i_username;
        l_exception.error_time := i_time;
        RETURN l_exception;
    END;

    FUNCTION get_error_message(i_code IN PLS_INTEGER) 
    RETURN error_log.error_message%TYPE
    IS
        l_err_msg error_log.error_message%TYPE; 
    BEGIN
         l_err_msg :=
            CASE 
                WHEN i_code BETWEEN -20999 AND -20001 THEN
                    CASE i_code
                        WHEN -20001 THEN c_non_positive_input_message
                        WHEN -20002 THEN c_non_existent_search_message
                        WHEN -20003 THEN c_empty_array_message
                        ELSE c_default_message
                    END
                ELSE
                    SQLERRM(i_code)
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

    PROCEDURE handle_exception
    (i_backtrace IN VARCHAR2,
    i_code IN PLS_INTEGER,
    i_user IN VARCHAR2,
    i_date IN DATE) 
    IS
        l_err_msg error_log.error_message%TYPE;
        l_backtrace l_backtrace_rt;
        l_exception error_log%ROWTYPE;
    BEGIN
        l_err_msg := get_error_message(i_code);
        l_backtrace := parse_backtrace(i_backtrace);
        l_exception := map_data_to_error_log(i_code, l_err_msg, l_backtrace, i_user, i_date);
        save_exception(l_exception);
    END;
END exception_pkg;
/

--------------------INPUT PACKAGE---------------------------------

CREATE OR REPLACE PACKAGE input_pkg
IS
    SUBTYPE g_number_read_t IS VARCHAR2(5);

    c_default_value CONSTANT PLS_INTEGER := 10;

    FUNCTION handle_input(i_input IN g_number_read_t) RETURN PLS_INTEGER;
END input_pkg;
/

CREATE OR REPLACE PACKAGE BODY input_pkg
IS
    FUNCTION handle_input(i_input IN g_number_read_t)
    RETURN PLS_INTEGER
    IS
        l_n PLS_INTEGER;
        c_default_input CONSTANT PLS_INTEGER := 10;
    BEGIN
        l_n := TO_NUMBER(i_input);

        <<check_if_non_positive>>
        IF l_n <= 0 THEN
            RAISE exception_pkg.exec_non_positive_input; 
        END IF check_if_non_positive;

        RETURN l_n;
    EXCEPTION
        WHEN exception_pkg.exec_non_positive_input THEN
            exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);
            RETURN (l_n * -1) + 1;
        WHEN VALUE_ERROR THEN
            exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);
            RETURN c_default_input;
    END;
END input_pkg;
/

-----------------ARRAY MANAGEMENT PACKAGE-----------------------

CREATE OR REPLACE PACKAGE array_management_pkg
IS
    FUNCTION random_unique_nt_generation
    (i_count IN PLS_INTEGER)
    RETURN global_types_consts_pkg.g_random_number_nt;

    FUNCTION insertion_sort
    (i_random_numbers IN global_types_consts_pkg.g_random_number_nt)
    RETURN global_types_consts_pkg.g_random_number_nt; 

    PROCEDURE print_number_array
    (i_array IN global_types_consts_pkg.g_random_number_nt,
    i_msg IN VARCHAR2);
END array_management_pkg;
/

CREATE OR REPLACE PACKAGE BODY array_management_pkg
IS

    c_default_numbers CONSTANT global_types_consts_pkg.g_random_number_nt := global_types_consts_pkg.g_random_number_nt(-50, -40, -30, -20, -10, 10, 20, 30, 40, 50);

    FUNCTION random_unique_nt_generation
    (i_count IN PLS_INTEGER)
    RETURN global_types_consts_pkg.g_random_number_nt
    IS
        l_random_numbers global_types_consts_pkg.g_random_number_nt := global_types_consts_pkg.g_random_number_nt();
        c_default_count CONSTANT PLS_INTEGER := 10;
    BEGIN
        l_random_numbers.EXTEND(i_count);

        <<handle_value_error>>
        BEGIN
            IF l_random_numbers.COUNT = 0 THEN
                RAISE VALUE_ERROR;
            END IF;
        EXCEPTION
            WHEN VALUE_ERROR THEN
                exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);
                l_random_numbers.EXTEND(c_default_count);
            WHEN OTHERS THEN
                RAISE;
        END handle_value_error;
        
        <<for_each_index_generate_unique_random>>
        DECLARE
            SUBTYPE l_unique_check_element_t IS VARCHAR2(100);
            TYPE l_unique_check_aat IS TABLE OF BOOLEAN 
                                                    INDEX BY l_unique_check_element_t;
            l_unique_checks l_unique_check_aat;
        BEGIN
            FOR indx IN l_random_numbers.FIRST .. l_random_numbers.LAST
            LOOP
                <<loop_while_until_max_attempts_reached_or_unique_generated>>
                DECLARE 
                    l_random global_types_consts_pkg.g_random_number_elem_t;
                    l_attempts PLS_INTEGER := 0;
                    c_max_attempts CONSTANT PLS_INTEGER := 1000;
                    l_found_unique BOOLEAN := FALSE;
                BEGIN
                    WHILE l_attempts < c_max_attempts AND NOT l_found_unique
                    LOOP
                        <<generate_random>>
                        DECLARE
                            l_min_value_generate global_types_consts_pkg.g_random_number_elem_t := 
                                -(POWER(10, global_types_consts_pkg.c_precision - global_types_consts_pkg.c_scale) - POWER(10, -global_types_consts_pkg.c_scale));

                            l_max_value_generate global_types_consts_pkg.g_random_number_elem_t := 
                                POWER(10, global_types_consts_pkg.c_precision - global_types_consts_pkg.c_scale) - POWER(10, -global_types_consts_pkg.c_scale);
                        BEGIN
                            l_random := dbms_random.value(l_min_value_generate, l_max_value_generate);
                        END generate_random;

                        <<check_if_unique_and_assign>>
                        CASE
                            WHEN NOT l_unique_checks.EXISTS(TRIM(TO_CHAR(l_random))) THEN
                                l_unique_checks(TRIM(TO_CHAR(l_random))) := TRUE;
                                l_random_numbers(indx) := l_random;
                                l_found_unique := TRUE;
                            ELSE 
                                l_attempts := l_attempts + 1;
                        END CASE check_if_unique_and_assign;
                    END LOOP;
                END loop_while_until_max_attempts_reached_or_unique_generated;
            END LOOP;
        END for_each_index_generate_unique_random;
        RETURN l_random_numbers;
    END;

    FUNCTION insertion_sort
    (i_random_numbers IN global_types_consts_pkg.g_random_number_nt)
    RETURN global_types_consts_pkg.g_random_number_nt 
    IS
        l_sorted_random_numbers global_types_consts_pkg.g_random_number_nt;
    BEGIN
        <<check_if_invalid>>
        BEGIN
            <<check_if_null>>
            IF i_random_numbers IS NULL THEN
                RAISE COLLECTION_IS_NULL;
            END IF check_if_null;
        
            <<check_if_empty>>
            IF i_random_numbers.COUNT = 0 THEN
                RAISE exception_pkg.exec_empty_array_passed;
            END IF check_if_empty;     

            l_sorted_random_numbers := i_random_numbers;       
        EXCEPTION
            WHEN COLLECTION_IS_NULL OR exception_pkg.exec_empty_array_passed THEN
                exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);
                l_sorted_random_numbers := c_default_numbers;
            WHEN OTHERS THEN
                RAISE;
        END check_if_invalid;

        
        <<sort_each_element_starting_from_second>>
        DECLARE 
            l_current_elem global_types_consts_pkg.g_random_number_elem_t;
            l_inner_indx PLS_INTEGER;
        BEGIN
            FOR indx IN 2..l_sorted_random_numbers.COUNT 
            LOOP
                l_current_elem := l_sorted_random_numbers(indx);
                l_inner_indx := indx - 1;
                
                <<while_current_element_is_larger_move_previous_forward>>
                WHILE l_inner_indx >= 1 AND l_current_elem < l_sorted_random_numbers(l_inner_indx)
                LOOP
                    l_sorted_random_numbers(l_inner_indx + 1) := l_sorted_random_numbers(l_inner_indx);
                    l_inner_indx := l_inner_indx - 1;
                END LOOP while_current_element_is_larger_move_previous_forward;
                
                l_sorted_random_numbers(l_inner_indx + 1) := l_current_elem;
            END LOOP;
        END sort_each_element_starting_from_second;
        RETURN l_sorted_random_numbers;
    END;

    PROCEDURE print_number_array
    (i_array IN global_types_consts_pkg.g_random_number_nt,
    i_msg IN VARCHAR2) 
    IS
        l_array global_types_consts_pkg.g_random_number_nt;
    BEGIN 
       <<check_if_invalid>>
        BEGIN
            <<check_if_null>>
            IF i_array IS NULL THEN
                RAISE COLLECTION_IS_NULL;
            END IF check_if_null;
        
            <<check_if_empty>>
            IF i_array.COUNT = 0 THEN
                RAISE exception_pkg.exec_empty_array_passed;
            END IF check_if_empty;     

            l_array := i_array;       
        EXCEPTION
            WHEN COLLECTION_IS_NULL OR exception_pkg.exec_empty_array_passed THEN
                exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);
                l_array := c_default_numbers;
            WHEN OTHERS THEN
                RAISE;
        END check_if_invalid;

        DBMS_OUTPUT.PUT_LINE(chr(10) || i_msg);

        <<print_each_element>>
        DECLARE 
            indx PLS_INTEGER := l_array.FIRST;
        BEGIN
            WHILE indx IS NOT NULL
            LOOP
                DBMS_OUTPUT.PUT_LINE('Number is: ' || l_array(indx));
                indx := l_array.NEXT(indx);
            END LOOP;
        END print_each_element;
    END;
END array_management_pkg;
/

--------------SEARCH PACKAGE--------------------------

CREATE OR REPLACE PACKAGE search_pkg
IS
    FUNCTION get_search_efficiencies
    (i_unordered_array IN global_types_consts_pkg.g_random_number_nt,
    i_ordered_array IN global_types_consts_pkg.g_random_number_nt,
    i_searches IN global_types_consts_pkg.g_search_option_aat)
    RETURN global_types_consts_pkg.g_average_search_aat;
END search_pkg;
/

CREATE OR REPLACE PACKAGE BODY search_pkg
IS  
    TYPE l_target_iterations_rt IS RECORD (
        l_target global_types_consts_pkg.g_random_number_elem_t,
        l_iteration_num PLS_INTEGER
    );
    TYPE l_search_result_aat IS TABLE OF l_target_iterations_rt
                          INDEX BY PLS_INTEGER;

    FUNCTION binary_search 
    (i_sorted_random_numbers IN global_types_consts_pkg.g_random_number_nt,
    i_target IN global_types_consts_pkg.g_random_number_elem_t) 
    RETURN l_target_iterations_rt
    IS
        l_targets_iterations l_target_iterations_rt := l_target_iterations_rt(i_target, 0);
    BEGIN
        <<loop_until_element_found_or_boundaries_overlap>>
        DECLARE
            l_low PLS_INTEGER := 1;
            l_high PLS_INTEGER := i_sorted_random_numbers.COUNT;
            l_mid PLS_INTEGER;
            l_iterations PLS_INTEGER := 0;
        BEGIN
            WHILE l_low <= l_high 
            LOOP
                l_iterations := l_iterations + 1;
                l_mid := TRUNC((l_low + l_high) / 2);
                
                <<return_element_found_or_adjust_boundaries>>
                CASE 
                    WHEN i_sorted_random_numbers(l_mid) = i_target THEN
                        l_targets_iterations.l_iteration_num := l_iterations;
                        RETURN l_targets_iterations;
                    WHEN i_sorted_random_numbers(l_mid) < i_target THEN
                        l_low := l_mid + 1; 
                    ELSE
                        l_high := l_mid - 1; 
                END CASE return_element_found_or_adjust_boundaries;
            END LOOP;
        END loop_until_element_found_or_boundaries_overlap;
        RETURN l_targets_iterations;
    END binary_search;

    FUNCTION linear_search 
    (i_sorted_random_numbers IN global_types_consts_pkg.g_random_number_nt,
    i_target IN global_types_consts_pkg.g_random_number_elem_t) 
    RETURN l_target_iterations_rt
    IS
        l_targets_iterations l_target_iterations_rt := l_target_iterations_rt(i_target, 0);
    BEGIN
        <<cycle_through_array_until_target_found>>
        DECLARE
            l_iterations PLS_INTEGER := 0;
        BEGIN
            FOR indx IN 1..i_sorted_random_numbers.COUNT
            LOOP
                l_iterations := l_iterations + 1;
                
                <<return_if_found>>
                IF i_sorted_random_numbers(indx) = i_target THEN
                    l_targets_iterations.l_iteration_num := l_iterations;
                    RETURN l_targets_iterations;
                END IF return_if_found;
            END LOOP;
        END cycle_through_array_until_target_found;
        RETURN l_targets_iterations;
    END;

    FUNCTION apply_search
    (i_unordered_array IN global_types_consts_pkg.g_random_number_nt,
    i_ordered_array IN global_types_consts_pkg.g_random_number_nt,
    i_search_type IN VARCHAR2)
    RETURN l_search_result_aat
    IS
        search_results l_search_result_aat;
    BEGIN
        <<apply_each_search_to_each_element>>
        FOR indx IN 1..i_unordered_array.COUNT
        LOOP
            search_results(indx) := 
                CASE i_search_type
                    WHEN 'binary' THEN binary_search(i_ordered_array, i_unordered_array(indx))
                    WHEN 'linear' THEN linear_search(i_ordered_array, i_unordered_array(indx))
                END;
        END LOOP apply_each_search_to_each_element;
        RETURN search_results;
    END;

    FUNCTION calculate_search_efficiency
    (i_search_results IN l_search_result_aat)
    RETURN global_types_consts_pkg.g_average_search_t
    IS
        l_search_efficiency global_types_consts_pkg.g_average_search_t := 0;
        l_search_iterations_sum PLS_INTEGER := 0;
    BEGIN
        <<calculate_total_iterations>>
        DECLARE 
            indx PLS_INTEGER := i_search_results.FIRST;
        BEGIN
            WHILE indx IS NOT NULL
            LOOP
                l_search_iterations_sum := l_search_iterations_sum + i_search_results(indx).l_iteration_num;
                indx := i_search_results.NEXT(indx);
            END LOOP;
        END calculate_total_iterations;

        l_search_efficiency := l_search_iterations_sum / i_search_results.COUNT;
        RETURN l_search_efficiency;
    END;

    FUNCTION get_search_efficiencies
    (i_unordered_array IN global_types_consts_pkg.g_random_number_nt,
    i_ordered_array IN global_types_consts_pkg.g_random_number_nt,
    i_searches IN global_types_consts_pkg.g_search_option_aat)
    RETURN global_types_consts_pkg.g_average_search_aat 
    IS
        l_average_searches global_types_consts_pkg.g_average_search_aat;
        search_results l_search_result_aat;
        l_searches global_types_consts_pkg.g_search_option_aat := i_searches;
    BEGIN
        <<check_search_types_valid>>
        DECLARE
            l_iterator global_types_consts_pkg.g_search_option_t := i_searches.FIRST;
        BEGIN
            WHILE l_iterator IS NOT NULL
            LOOP
                IF NOT global_types_consts_pkg.c_existing_searches.EXISTS(l_iterator) THEN
                    RAISE exception_pkg.exec_non_existent_search;
                END IF;
                l_iterator := i_searches.NEXT(l_iterator);
            END LOOP;
        EXCEPTION
            WHEN exception_pkg.exec_non_existent_search THEN
                exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);
                l_searches := global_types_consts_pkg.c_existing_searches;
            WHEN OTHERS THEN
                RAISE;
        END check_search_types_valid;

        <<get_search_results>>
        DECLARE
            l_iterator global_types_consts_pkg.g_search_option_t := l_searches.FIRST;
        BEGIN
            WHILE l_iterator IS NOT NULL
            LOOP
                search_results := apply_search(i_unordered_array, i_ordered_array, l_iterator);
                l_average_searches(l_iterator) := calculate_search_efficiency(search_results);
                l_iterator := l_searches.NEXT(l_iterator);
            END LOOP;
        END get_search_results;
        RETURN l_average_searches;
    END;
END search_pkg;
/

---------------------DEMO PACKAGE------------------------------

CREATE OR REPLACE PACKAGE demo_pkg AS
  PROCEDURE run_search_efficiency_demo(i_input IN input_pkg.g_number_read_t);
END demo_pkg;
/

CREATE OR REPLACE PACKAGE BODY demo_pkg 
IS
  PROCEDURE run_search_efficiency_demo(i_input IN input_pkg.g_number_read_t) 
  IS
    l_n PLS_INTEGER;
    l_random_array global_types_consts_pkg.g_random_number_nt;
    l_random_sorted_array global_types_consts_pkg.g_random_number_nt;
    l_search_efficiencies global_types_consts_pkg.g_average_search_aat;
    l_search_options global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.g_search_option_aat('binary' => 1, 'linear' => 2);
    l_all_search_options global_types_consts_pkg.g_search_option_aat;

    -- invalid data for exception testing
    temp global_types_consts_pkg.g_random_number_nt := global_types_consts_pkg.g_random_number_nt();
    temp1 global_types_consts_pkg.g_random_number_nt;
    temp2 global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.g_search_option_aat('asdawdas' => 1, 'linear' => 2);
    temp3 global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.g_search_option_aat('linear' => 1);
    temp4 PLS_INTEGER := 0;
  BEGIN
    l_n := input_pkg.handle_input(i_input);

    l_random_array := array_management_pkg.random_unique_nt_generation(l_n);

    l_random_sorted_array := array_management_pkg.insertion_sort(l_random_array);

    l_search_efficiencies := search_pkg.get_search_efficiencies(
      i_unordered_array => l_random_array,
      i_ordered_array => l_random_sorted_array,
      i_searches => temp3
    );

    array_management_pkg.print_number_array(l_random_array, 'Unsorted array: ');
    array_management_pkg.print_number_array(l_random_sorted_array, 'Sorted array: ');

    <<check_if_all_search_options_got_created>>
    DECLARE
        l_iterator global_types_consts_pkg.g_search_option_t := global_types_consts_pkg.c_existing_searches.FIRST;
    BEGIN
        WHILE l_iterator IS NOT NULL
        LOOP
            IF NOT l_search_efficiencies.EXISTS(l_iterator) THEN
                RAISE NO_DATA_FOUND;
            END IF;
            l_iterator := global_types_consts_pkg.c_existing_searches.NEXT(l_iterator);
        END LOOP;
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            exception_pkg.handle_exception(DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, SQLCODE, USER, SYSDATE);

            <<add_non_existent_search_efficiencies>>
            DECLARE
                l_iterator global_types_consts_pkg.g_search_option_t := global_types_consts_pkg.c_existing_searches.FIRST;
                c_default_search_efficiency PLS_INTEGER := 0;
            BEGIN
                WHILE l_iterator IS NOT NULL
                LOOP
                    IF NOT l_search_efficiencies.EXISTS(l_iterator) THEN
                        l_search_efficiencies(l_iterator) := c_default_search_efficiency;
                    END IF;
                    l_iterator := global_types_consts_pkg.c_existing_searches.NEXT(l_iterator);
                END LOOP;
            END add_non_existent_search_efficiencies;
        WHEN OTHERS THEN
            RAISE;
    END check_if_all_search_options_got_created;

    <<print_all_search_results>>
    DECLARE
        l_iterator global_types_consts_pkg.g_search_option_t  := global_types_consts_pkg.c_existing_searches.FIRST;
    BEGIN
        WHILE l_iterator IS NOT NULL
        LOOP
            DBMS_OUTPUT.PUT_LINE(chr(10) || l_iterator || ' average: ' || l_search_efficiencies(l_iterator) || chr(10));
            l_iterator := global_types_consts_pkg.c_existing_searches.NEXT(l_iterator);
        END LOOP;
    END print_all_search_results;
  END run_search_efficiency_demo;
END demo_pkg;
/

-------------------------ANONYMOUS BLOCK------------------------------------

<<paieskos_algoritmu_efektyvumo_tyrimas>>
BEGIN
    demo_pkg.run_search_efficiency_demo('&l_n');
END paieskos_algoritmu_efektyvumo_tyrimas;
/