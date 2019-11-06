cat sample_list | awk '{if ($1<9) print $0}' >sample_list_less_info
