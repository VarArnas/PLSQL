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
    -- custom exceptions
    c_non_positive_input CONSTANT NUMBER := -20001;
    exec_non_positive_input EXCEPTION;

    c_non_existent_search CONSTANT NUMBER := -20002;
    exec_non_existent_search EXCEPTION;

    c_empty_array_passed CONSTANT NUMBER := -20003;
    exec_empty_array_passed EXCEPTION;

    PRAGMA EXCEPTION_INIT(exec_non_positive_input, c_non_positive_input);
    PRAGMA EXCEPTION_INIT(exec_non_existent_search, c_non_existent_search);
    PRAGMA EXCEPTION_INIT(exec_empty_array_passed, c_empty_array_passed);

    -- custom exceptions initializing
    PROCEDURE check_input_non_positive(i_input IN PLS_INTEGER);
    PROCEDURE check_search_option_non_existent(i_input_options IN global_types_consts_pkg.g_search_option_aat);
    PROCEDURE check_if_array_invalid(i_array IN global_types_consts_pkg.g_random_number_nt);

    -- custom exceptions handling
    FUNCTION handle_non_positive_input(i_input IN PLS_INTEGER) RETURN PLS_INTEGER;
    FUNCTION handle_non_existent_search RETURN global_types_consts_pkg.g_search_option_aat;
    PROCEDURE handle_empty_array_passed;

     -- oracle initializing
    PROCEDURE check_if_assign_empty_array_to_index(i_count IN PLS_INTEGER);
    PROCEDURE check_if_all_keys_of_aat_valid(i_searches IN global_types_consts_pkg.g_average_search_aat);

    -- oracle standard exceptions handling
    FUNCTION handle_uninitialized_random_nt RETURN global_types_consts_pkg.g_random_number_nt;
    FUNCTION handle_assigning_empty_array_to_index RETURN PLS_INTEGER;
    FUNCTION handle_not_full_search_efficiencies(i_searches IN global_types_consts_pkg.g_average_search_aat) RETURN global_types_consts_pkg.g_average_search_aat;
END exception_pkg;
/

CREATE OR REPLACE PACKAGE BODY exception_pkg
IS
    -- helper private constructs for handling all exceptions
    PROCEDURE save_exception(i_exception error_log%ROWTYPE)
    IS
        PRAGMA AUTONOMOUS_TRANSACTION;
    BEGIN
        INSERT INTO error_log(error_code, error_message, backtrace, username, error_time)
        VALUES (i_exception.error_code, i_exception.error_message, i_exception.backtrace, i_exception.username, i_exception.error_time);
        COMMIT;
    EXCEPTION 
        WHEN OTHERS THEN
            ROLLBACK;
            RAISE;
    END;

    FUNCTION map_data_to_error_log(i_code IN error_log.error_code%TYPE,
            i_msg IN error_log.error_message%TYPE,
            i_backtrace IN error_log.backtrace%TYPE,
            i_username IN error_log.username%TYPE,
            i_time IN error_log.error_time%TYPE)
    RETURN error_log%ROWTYPE
    IS
        l_exception error_log%ROWTYPE;
    BEGIN
        l_exception.error_code := i_code;
        l_exception.error_message := i_msg;
        l_exception.backtrace := i_backtrace;
        l_exception.username := i_username;
        l_exception.error_time := i_time;
        RETURN l_exception;
    END;

    PROCEDURE handle_exception(i_msg IN error_log.error_message%TYPE) 
    IS
        l_exception error_log%ROWTYPE;
    BEGIN
        l_exception := map_data_to_error_log(SQLCODE, i_msg, DBMS_UTILITY.FORMAT_ERROR_BACKTRACE, USER, SYSTIMESTAMP);
        save_exception(l_exception);
    END;

    -- public initializing and handling constructs
    PROCEDURE check_input_non_positive(i_input IN PLS_INTEGER) IS
    BEGIN
        IF i_input <= 0 THEN
            RAISE exec_non_positive_input;
        END IF check_if_more_than_zero;   
    END;

    FUNCTION handle_non_positive_input(i_input IN PLS_INTEGER)
    RETURN PLS_INTEGER
    IS
        l_err_msg  error_log.error_message%TYPE;
    BEGIN
        l_err_msg := 'A non positive number was given for the creation of random unique array size';
        handle_exception(l_err_msg);
        RETURN (i_input * -1) + 1;
    END;


    PROCEDURE check_search_option_non_existent(i_input_options IN global_types_consts_pkg.g_search_option_aat) 
    IS
        l_iterator global_types_consts_pkg.g_search_option_t := i_input_options.FIRST;
        l_existing_search global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.c_existing_searches;
    BEGIN
        WHILE l_iterator IS NOT NULL
        LOOP
            IF NOT l_existing_search.EXISTS(l_iterator) THEN
                RAISE exec_non_existent_search;
            END IF;
            l_iterator := i_input_options.NEXT(l_iterator);
        END LOOP;
    END;

    FUNCTION handle_non_existent_search
    RETURN global_types_consts_pkg.g_search_option_aat
    IS
        l_err_msg error_log.error_message%TYPE;
    BEGIN
        l_err_msg := 'A non-existent search option was given for finding search efficiencies';
        handle_exception(l_err_msg);
        RETURN global_types_consts_pkg.c_existing_searches;
    END;


    PROCEDURE check_if_array_invalid(i_array IN global_types_consts_pkg.g_random_number_nt) IS
    BEGIN
        <<check_if_null>>
        IF i_array IS NULL THEN
            RAISE COLLECTION_IS_NULL;
        END IF check_if_null;
    
        <<check_if_empty>>
        IF i_array.COUNT = 0 THEN
            RAISE exec_empty_array_passed;
        END IF check_if_empty;
    END;

    FUNCTION handle_uninitialized_random_nt
    RETURN global_types_consts_pkg.g_random_number_nt
    IS
        l_err_msg error_log.error_message%TYPE;
        l_default_random_nested CONSTANT global_types_consts_pkg.g_random_number_nt := global_types_consts_pkg.g_random_number_nt(1 => 1, 2 => 1, 3 => 1);
    BEGIN
        l_err_msg := SQLERRM(SQLCODE);
        handle_exception(l_err_msg);
        RETURN  l_default_random_nested;
    END;

    PROCEDURE handle_empty_array_passed
    IS
        l_err_msg error_log.error_message%TYPE;
    BEGIN
        l_err_msg := 'The passed array did not have any elements';
        handle_exception(l_err_msg);
        RAISE_APPLICATION_ERROR(SQLCODE, l_err_msg);
    END;


    PROCEDURE check_if_assign_empty_array_to_index(i_count IN PLS_INTEGER) IS
    BEGIN
        IF i_count <= 0 THEN
            RAISE VALUE_ERROR;
        END IF;
    END;

    FUNCTION handle_assigning_empty_array_to_index
    RETURN PLS_INTEGER
    IS
        l_err_msg error_log.error_message%TYPE;
        c_default_count CONSTANT PLS_INTEGER := 10;
    BEGIN
        l_err_msg := SQLERRM(SQLCODE);
        handle_exception(l_err_msg);
        RETURN c_default_count;
    END;


    PROCEDURE check_if_all_keys_of_aat_valid(i_searches IN global_types_consts_pkg.g_average_search_aat) 
    IS
        l_existing_searches global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.c_existing_searches;
        l_iterator global_types_consts_pkg.g_search_option_t := l_existing_searches.FIRST;
    BEGIN
        WHILE l_iterator IS NOT NULL
        LOOP
            IF NOT i_searches.EXISTS(l_iterator) THEN
                RAISE NO_DATA_FOUND;
            END IF;
            l_iterator := l_existing_searches.NEXT(l_iterator);
        END LOOP;
    END;

    FUNCTION handle_not_full_search_efficiencies(i_searches IN global_types_consts_pkg.g_average_search_aat) 
    RETURN global_types_consts_pkg.g_average_search_aat
    IS
        l_existing_searches global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.c_existing_searches;
        l_iterator global_types_consts_pkg.g_search_option_t := l_existing_searches.FIRST;
        l_modified_efficiencies global_types_consts_pkg.g_average_search_aat := i_searches;
        l_err_msg error_log.error_message%TYPE;
    BEGIN
        l_err_msg := SQLERRM(SQLCODE);
        handle_exception(l_err_msg);

        <<add_non_existent_search_efficiencies>>
        WHILE l_iterator IS NOT NULL
        LOOP
            IF NOT i_searches.EXISTS(l_iterator) THEN
                l_modified_efficiencies(l_iterator) := 0;
            END IF;
            l_iterator := l_existing_searches.NEXT(l_iterator);
        END LOOP add_non_existent_search_efficiencies;
        RETURN l_modified_efficiencies;
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
    BEGIN
        l_n := TO_NUMBER(i_input);
        exception_pkg.check_input_non_positive(l_n);   
        RETURN l_n;
    EXCEPTION
        WHEN exception_pkg.exec_non_positive_input THEN
            RETURN exception_pkg.handle_non_positive_input(l_n);
        WHEN VALUE_ERROR THEN
            RETURN exception_pkg.handle_assigning_empty_array_to_index;
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
    FUNCTION random_unique_nt_generation
    (i_count IN PLS_INTEGER)
    RETURN global_types_consts_pkg.g_random_number_nt
    IS
        l_random_numbers global_types_consts_pkg.g_random_number_nt := global_types_consts_pkg.g_random_number_nt();
        l_count PLS_INTEGER;
    BEGIN
        <<handle_value_error>>
        BEGIN
            exception_pkg.check_if_assign_empty_array_to_index(i_count);
            l_count := i_count;
        EXCEPTION
            WHEN VALUE_ERROR THEN
                l_count := exception_pkg.handle_assigning_empty_array_to_index;
        END handle_value_error;

        l_random_numbers.EXTEND(l_count);
        
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
            exception_pkg.check_if_array_invalid(i_random_numbers);
            l_sorted_random_numbers := i_random_numbers;
        EXCEPTION
            WHEN COLLECTION_IS_NULL THEN
                l_sorted_random_numbers := exception_pkg.handle_uninitialized_random_nt;
            WHEN exception_pkg.exec_empty_array_passed THEN
                exception_pkg.handle_empty_array_passed;
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
       <<check_if_array_valid>>
       BEGIN
            exception_pkg.check_if_array_invalid(i_array);
            l_array := i_array;
       EXCEPTION
            WHEN COLLECTION_IS_NULL THEN
                l_array := exception_pkg.handle_uninitialized_random_nt;
            WHEN exception_pkg.exec_empty_array_passed THEN
                exception_pkg.handle_empty_array_passed;
       END check_if_array_valid;

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
        BEGIN
            exception_pkg.check_search_option_non_existent(i_searches);
        EXCEPTION
            WHEN exception_pkg.exec_non_existent_search THEN
                l_searches := exception_pkg.handle_non_existent_search;
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
  BEGIN
    l_n := input_pkg.handle_input(i_input);

    l_random_array := array_management_pkg.random_unique_nt_generation(l_n);

    l_random_sorted_array := array_management_pkg.insertion_sort(l_random_array);

    l_search_efficiencies := search_pkg.get_search_efficiencies(
      i_unordered_array => l_random_array,
      i_ordered_array => l_random_sorted_array,
      i_searches => l_search_options
    );

    l_all_search_options := global_types_consts_pkg.c_existing_searches;

    array_management_pkg.print_number_array(l_random_array, 'Unsorted array: ');
    array_management_pkg.print_number_array(l_random_sorted_array, 'Sorted array: ');

    <<check_if_all_search_options_got_created>>
    BEGIN
        exception_pkg.check_if_all_keys_of_aat_valid(l_search_efficiencies);
    EXCEPTION
        WHEN NO_DATA_FOUND THEN
            l_search_efficiencies := exception_pkg.handle_not_full_search_efficiencies(l_search_efficiencies);
    END check_if_all_search_options_got_created;

    <<print_all_search_results>>
    DECLARE
        l_iterator global_types_consts_pkg.g_search_option_t  := l_all_search_options.FIRST;
    BEGIN
        WHILE l_iterator IS NOT NULL
        LOOP
            DBMS_OUTPUT.PUT_LINE(chr(10) || l_iterator || ' average: ' || l_search_efficiencies(l_iterator) || chr(10));
            l_iterator := l_all_search_options.NEXT(l_iterator);
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