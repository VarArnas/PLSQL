-------------------------ANONYMOUS BLOCK------------------------------------

<<paieskos_algoritmu_efektyvumo_tyrimas>>
DECLARE
    l_n CONSTANT PLS_INTEGER := 30;
    l_random_array global_types_consts_pkg.g_random_number_nt;
    l_random_sorted_array global_types_consts_pkg.g_random_number_nt;
    l_search_efficiencies global_types_consts_pkg.g_average_search_aat;
    l_search_options global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.g_search_option_aat('binary' => 1, 'linear' => 2);

    -- invalid data for exception testing
    temp global_types_consts_pkg.g_random_number_nt := global_types_consts_pkg.g_random_number_nt();
    temp1 global_types_consts_pkg.g_random_number_nt;
    temp2 global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.g_search_option_aat('asdawdas' => 1, 'linear' => 2);
    temp3 global_types_consts_pkg.g_search_option_aat := global_types_consts_pkg.g_search_option_aat('linear' => 1);
    temp4 PLS_INTEGER := 0;
BEGIN
    l_random_array := array_management_pkg.random_unique_nt_generation(l_n);

    l_random_sorted_array := array_management_pkg.insertion_sort(l_random_array);

    l_search_efficiencies := search_pkg.get_search_efficiencies(
      i_unordered_array => l_random_array,
      i_ordered_array => l_random_sorted_array,
      i_searches => l_search_options
    );

    array_management_pkg.print_number_array(l_random_array, 'Unsorted array: ');
    array_management_pkg.print_number_array(l_random_sorted_array, 'Sorted array: ');

    search_pkg.print_all_search_efficiencies(l_search_efficiencies);
END paieskos_algoritmu_efektyvumo_tyrimas;
/