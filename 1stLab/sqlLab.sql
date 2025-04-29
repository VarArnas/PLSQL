SET SERVEROUTPUT ON
SPOOL results.txt

<<paieskos_algoritmu_efektyvumo_tyrimas>>
DECLARE
  l_n PLS_INTEGER;

  c_precision CONSTANT NUMBER := 5;
  c_scale CONSTANT NUMBER := 2;
  SUBTYPE l_random_number_elem_t IS NUMBER(c_precision, c_scale);

  TYPE l_random_number_nt IS TABLE OF l_random_number_elem_t;
  l_random_numbers l_random_number_nt := l_random_number_nt();
  l_sorted_random_numbers l_random_number_nt;
  
  TYPE l_target_iterations_rt IS RECORD (
    l_target l_random_number_elem_t,
    l_iteration_num PLS_INTEGER
  );
  
  SUBTYPE l_average_search_t IS NUMBER(38,2);
  l_linear_search_efficiency l_average_search_t;
  l_binary_search_efficiency l_average_search_t;

  TYPE l_search_result_aat IS TABLE OF l_target_iterations_rt
                          INDEX BY PLS_INTEGER;


  -- generation of random nt, if count is <= 0, then return empty nt
 FUNCTION random_unique_nt_generation
  (i_count IN PLS_INTEGER,
  i_min_value_generate IN l_random_number_elem_t,
  i_max_value_generate IN l_random_number_elem_t)
  RETURN l_random_number_nt
  IS
    l_random_numbers l_random_number_nt := l_random_number_nt();
  BEGIN

    <<check_if_count_valid>>
      IF i_count <= 0 THEN
        RETURN l_random_numbers;
      END IF check_if_more_than_zero;

    l_random_numbers.EXTEND(i_count);
    
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
          l_random l_random_number_elem_t;
          l_attempts PLS_INTEGER := 0;
          c_max_attempts CONSTANT PLS_INTEGER := 1000;
          l_found_unique BOOLEAN := FALSE;
        BEGIN
          WHILE l_attempts < c_max_attempts AND NOT l_found_unique
          LOOP
          
            <<generate_random>>
            BEGIN
              l_random := dbms_random.value(i_min_value_generate, i_max_value_generate);
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


  -- insertion sort implementation
  FUNCTION insertion_sort
  (i_random_numbers IN l_random_number_nt)
  RETURN l_random_number_nt 
  IS
    l_sorted_random_numbers l_random_number_nt := i_random_numbers;
  BEGIN
  
    <<check_if_one_or_zero_element>>
    IF l_sorted_random_numbers.COUNT = 0 THEN
        RETURN l_sorted_random_numbers;
    END IF check_if_one_or_zero_element;
    
    <<sort_each_element_starting_from_second>>
    DECLARE 
      l_current_elem l_random_number_elem_t;
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
  
  
  -- binary search implementation, if not found or 0 elements return number of iterations as 0
  FUNCTION binary_search 
  (i_sorted_random_numbers IN l_random_number_nt,
  i_target IN l_random_number_elem_t) 
  RETURN l_target_iterations_rt
  IS
    l_targets_iterations l_target_iterations_rt := l_target_iterations_rt(i_target, 0);
  BEGIN

    <<check_if_empty_array>>
    IF i_sorted_random_numbers.COUNT = 0 THEN
      RETURN l_targets_iterations;
    END IF check_if_empty_array;
    
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
  
  -- linear search implementation, return 0 iterations if not found
  FUNCTION linear_search 
  (i_sorted_random_numbers IN l_random_number_nt,
  i_target IN l_random_number_elem_t) 
  RETURN l_target_iterations_rt
  IS
    l_targets_iterations l_target_iterations_rt := l_target_iterations_rt(i_target, 0);
  BEGIN
    
    <<check_if_empty_array>>
    IF i_sorted_random_numbers.COUNT = 0 THEN
      RETURN l_targets_iterations;
    END IF check_if_empty_array;
  
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

  -- printing array
  PROCEDURE print_number_array
  (i_array IN l_random_number_nt,
  i_msg IN VARCHAR2) IS
  BEGIN 
  
    <<check_if_empty>>
    IF i_array.COUNT = 0 THEN
      DBMS_OUTPUT.PUT_LINE('Array is empty');
    END IF check_if_empty;

    DBMS_OUTPUT.PUT_LINE(chr(10) || i_msg);

    <<print_each_element>>
    DECLARE 
      indx PLS_INTEGER := i_array.FIRST;
    BEGIN
      WHILE indx IS NOT NULL
      LOOP
        DBMS_OUTPUT.PUT_LINE('Number is: ' || i_array(indx));
        indx := i_array.NEXT(indx);
      END LOOP;
    END print_each_element;

  END;

  -- apply search algorithm to unordered array inside an ordered array
  FUNCTION apply_search
  (i_unordered_array IN l_random_number_nt,
  i_ordered_array IN l_random_number_nt,
  i_search_type IN VARCHAR2)
  RETURN l_search_result_aat
  IS
    search_results l_search_result_aat;
  BEGIN

    <<check_if_arrays_valid>>
    IF i_unordered_array.COUNT = 0 OR i_unordered_array.COUNT <> i_ordered_array.COUNT THEN
      RETURN search_results; --EXCEPTION!!!!!!!!!!
    END IF check_if_arrays_valid;

    <<apply_each_search_to_each_element>>
    FOR indx IN 1..i_unordered_array.COUNT
    LOOP
        search_results(indx) := 
          CASE i_search_type
            WHEN 'binary' THEN binary_search(i_ordered_array, i_unordered_array(indx))
            WHEN 'linear' THEN linear_search(i_ordered_array, i_unordered_array(indx))
            ELSE NULL --EXCEPTION!!!!!!!!!!
          END;
    END LOOP apply_each_search_to_each_element;

    RETURN search_results;
  END;

  -- calculate average of iterations of a search algorithm
  FUNCTION calculate_search_efficiency
  (i_search_results IN l_search_result_aat)
  RETURN l_average_search_t
  IS
    l_search_efficiency l_average_search_t := 0;
    l_search_iterations_sum PLS_INTEGER := 0;
  BEGIN
    <<check_if_array_valid>>
    IF i_search_results.COUNT = 0 THEN
      RETURN l_search_efficiency; --EXCEPTION!!!!!!!!!!
    END IF check_if_array_valid;

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

BEGIN

  <<handle_input>>
  DECLARE 
    SUBTYPE l_number_read_t IS VARCHAR2(5);
    l_n_string l_number_read_t;
  BEGIN
  
    <<read_input>>
    BEGIN --EXCEPTION!!!!!!!!!!
      l_n_string := '&l_n_string';
      l_n := TO_NUMBER(l_n_string);
    END read_input;
    
    <<check_if_more_than_zero>>
    IF l_n <= 0 THEN --EXCEPTION!!!!!!!!!!
      RAISE_APPLICATION_ERROR(-20001, 'Value must be more than 0.');
    END IF check_if_more_than_zero;
    
  END handle_input;
  
  <<generate_random_unique_nt>>
  DECLARE
    l_min_value_generate l_random_number_elem_t;
    l_max_value_generate l_random_number_elem_t;
  BEGIN

    l_min_value_generate := - (POWER(10, c_precision - c_scale) - POWER(10, -c_scale));
    l_max_value_generate := POWER(10, c_precision - c_scale) - POWER(10, -c_scale);

    l_random_numbers := random_unique_nt_generation(i_count => l_n, 
                                                    i_min_value_generate => l_min_value_generate, 
                                                    i_max_value_generate => l_max_value_generate);    
  END generate_random_unique_nt;
  
  l_sorted_random_numbers := insertion_sort(l_random_numbers);
  
  <<get_search_efficiencies>>
  DECLARE
    l_binary_search_results l_search_result_aat;
    l_linear_search_results l_search_result_aat;
  BEGIN
    l_binary_search_results := apply_search(i_unordered_array => l_random_numbers,
                                            i_ordered_array => l_sorted_random_numbers,
                                            i_search_type => 'binary');
    
    l_linear_search_results := apply_search(i_unordered_array => l_random_numbers,
                                            i_ordered_array => l_sorted_random_numbers,
                                            i_search_type => 'linear');

    l_binary_search_efficiency := calculate_search_efficiency(l_binary_search_results);
    l_linear_search_efficiency := calculate_search_efficiency(l_linear_search_results);  
  END get_search_efficiencies;
  
  print_number_array(l_random_numbers, 'Unsorted array: ');
  
  print_number_array(l_sorted_random_numbers, 'Sorted array: ');
  
  DBMS_OUTPUT.PUT_LINE(chr(10) || 'Linear average: ' || l_linear_search_efficiency || ', binary average: ' || l_binary_search_efficiency);
  
END paieskos_algoritmu_efektyvumo_tyrimas;
/

SPOOL OFF