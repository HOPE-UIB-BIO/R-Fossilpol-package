utils::globalVariables("where")
#' @title Detect and exclude duplicates
#' @param data_source Data.frame with sequences
#' @param source_var Character. Name of a column that indicates source of data 
#' @param n_subgroups Number of subgroups to split the data based on the
#' geography. Sequences within subgroup will be tested only if subgroup
#' contains sequences form different `source_var` 
#' @param maximal_distance Maximal Euclidean distance between two sequences to 
#' consider them as material for comparison 
#' @return Data.frame with the suggested duplicates of sequences 
#' `distance` = Euclidean distance between sequences
#' `similarity` = mean number of the exactly same columns
#' @description  Function will split sequences based on their geographic 
#' location into `n_subgroups`. If subgroup contains data from multiple sources,
#' Euclidean `distance` between all sequences combination will be calculated.
#' Only combination with the distance up to `maximal_distance` will be kept.
#' Next, selected columns will be compared between sequences and the average number of
#' the exact same columns is returned `similarity`.  
proc_detect_duplicates <- 
  function(
    data_source, 
    source_var = "source_of_data", 
    n_subgroups = 1, 
    maximal_distance = 5) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names("data_source", c("lat", "long", eval(source_var)))
    
    util_check_class("source_var", "character")
    
    util_check_class("n_subgroups", "numeric")
    
    assertthat::assert_that(
      n_subgroups > 0,
      msg = "'n_subgroups' must be larger than 0")
    
    util_check_class("maximal_distance", "numeric")
    
    assertthat::assert_that(
      maximal_distance > 0,
      msg = "'maximal_distance' must be larger than 0")
    
    # helper function
    euclidean_dist <- 
      function(x, y) sqrt(sum((x - y)^2))
    
    data_coord <-
      data_source %>% 
      dplyr::select(
        dplyr::all_of(
          c("long", "lat"))) %>% 
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          scale))
    
    clusters_selection <- 
      stats::kmeans(data_coord, centers = n_subgroups)
    
    data_w <-
      data_source %>% 
      dplyr::mutate(subgroup = clusters_selection$cluster)
    
    util_check_col_names("data_w", "subgroup")
    
    # detect all the possible sources of data
    source_var_levels <- 
      data_w %>% 
      dplyr::select(
        dplyr::all_of(source_var)) %>% 
      purrr::pluck(1) %>% 
      unique()
    
    if(length(source_var_levels) < 2){
      util_output_comment(
        msg = paste("There is only 1 possible source of data:", source_var_levels))
      return(NA)
      
    }
    
    assertthat::assert_that(
      length(source_var_levels) <= 2,
      msg = "Current function can handle only a comparison of max 2 sources of data")
    
    intresting_subgroups <- 
      data_w %>% 
      dplyr::group_by(subgroup, get(source_var)) %>% 
      dplyr::summarise(.groups = "keep", N = dplyr::n()) %>% 
      tidyr::pivot_wider(names_from = `get(source_var)`, values_from = N) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        dplyr::across(
          where(is.numeric),
          ~ tidyr::replace_na(.x, 0))) %>% 
      dplyr::mutate(interesting = get(source_var_levels[1]) > 0 & get(source_var_levels[2]) > 0) %>% 
      dplyr::filter(interesting == TRUE) %>% 
      purrr::pluck("subgroup")
    
    if (length(intresting_subgroups) > 0 ) {
      
      util_output_comment(
        msg = paste0(
          "Detected potential regions of duplicates, N = ",
          length(intresting_subgroups)))
      
      for (i in 1:length(intresting_subgroups)) {
        
        cat(paste0(" - processing group ", i), "\n")
        
        data_w_subgroup <-
          data_w %>% 
          dplyr::filter(subgroup == intresting_subgroups[i])
        
        variable_list_full <-
          c("dataset_id", "handle", "siteid", "sitename", "long", "lat",
            "altitude", "chron_control", "n_chron_control", "sample_depth",
            "raw_counts", "n_sample_counts", "source_of_data", "subgroup")
        
        variable_list <-
          variable_list_full[variable_list_full %in%  names(data_w_subgroup)]
        
        dataset_A <- 
          data_w_subgroup %>% 
          dplyr::filter(get(source_var) == source_var_levels[1]) %>% 
          dplyr::select(
            dplyr::any_of(variable_list))
        
        dataset_B <- 
          data_w_subgroup %>% 
          dplyr::filter(get(source_var) == source_var_levels[2]) %>% 
          dplyr::select(
            dplyr::any_of(variable_list))
        
        dataset_A_coord <-
          dataset_A %>% 
          dplyr::select(dataset_id, long, lat, altitude)
        
        dataset_B_coord <-
          dataset_B %>% 
          dplyr::select(dataset_id, long, lat, altitude)
        
        combination_grid <- 
          expand.grid(
            dataset_A = dataset_A %>% 
              purrr::pluck("dataset_id"),
            dataset_B = dataset_B %>% 
              purrr::pluck("dataset_id")) %>% 
          tibble::as_tibble() %>% 
          dplyr::mutate(
            dplyr::across(
              where(is.factor),
              as.character)) %>% 
          dplyr::distinct(dataset_A, dataset_B, .keep_all = TRUE) %>% 
          dplyr::rowwise() %>% 
          dplyr::mutate(
            distance = purrr::map2_dbl(
              .x = dataset_A,
              .y = dataset_B,
              .f = ~ {
                
                data_a <-
                  dataset_A_coord %>% 
                  dplyr::filter(dataset_id == .x)
                
                data_a_list <- 
                  data_a %>%
                  dplyr::slice(1) %>% 
                  dplyr::select(long, lat) %>% 
                  unlist()
                
                data_b <-
                  dataset_B_coord %>% 
                  dplyr::filter(dataset_id == .y)
                
                data_b_list <- 
                  data_b %>% 
                  dplyr::slice(1) %>% 
                  dplyr::select(long, lat) %>%
                  unlist()
                
                dist <-
                  euclidean_dist(
                    data_a_list,
                    data_b_list)
                
                return(dist)  
              })) %>% 
          dplyr::ungroup()
        
        util_check_col_names(
          "combination_grid",
          c("distance",
            "dataset_A",
            "dataset_B")
        )
        
        posible_candidates <-
          combination_grid %>% 
          dplyr::arrange(distance) %>% 
          dplyr::filter(distance < maximal_distance) %>% 
          dplyr::distinct(dataset_A, dataset_B, .keep_all = TRUE)
        
        names(dataset_A)[-1] <- 
          paste0("A_",names(dataset_A)[-1])
        
        names(dataset_B)[-1] <-
          paste0("B_",names(dataset_B)[-1])
        
        joined_table <-
          posible_candidates %>% 
          dplyr::left_join(., dataset_A, by = c("dataset_A" = "dataset_id")) %>%
          dplyr::left_join(., dataset_B, by = c("dataset_B" = "dataset_id"))
        
        variable_selection <-
          variable_list[!variable_list %in% 
                          c("dataset_id", "source_of_data", "subgroup", "lat", "long")] 
        
        comparison_table_temp <-
          tibble::tibble(
            var = variable_selection,
            different = TRUE)
        
        util_check_col_names(
          "comparison_table_temp",
          c("var", "different"))
        
        posible_candidates$similarity <- 0
        
        for (j in 1:nrow(posible_candidates)) {
          
          sel_row <- joined_table[j, ]
          
          comparison_table <-
            comparison_table_temp %>% 
            dplyr::rowwise() %>% 
            dplyr::mutate(
              different = purrr::map_lgl(
                .x = var,
                .f = ~ {
                  pair <- 
                    dplyr::select(sel_row, tidyselect::contains(.x))
                  same <- 
                    all(all.equal(pair[1,1], pair[1,2], check.names = FALSE) == TRUE)
                  return(same)
                })) 
          
          util_check_col_names("comparison_table", "different")
          
          posible_candidates$similarity[j] <- mean(comparison_table$different)
        }
        
        if (i == 1) {
          final_result <- posible_candidates
        } else {
          final_result <-
            dplyr::bind_rows(
              final_result, posible_candidates
            )
          
        }
      }
      
      util_output_comment(
        msg = paste0(
          "Returning ", nrow(final_result), " of potential duplicates"))
      
      final_result %>% 
        dplyr::arrange(dplyr::desc(similarity), distance) %>% 
        dplyr::mutate(distance = round(distance, digits = 2)) %>% 
        return()
      
    } else {
      
      util_output_comment(
        msg = "Did not detect any potential duplicates")
      
      return(NA)
      
    }
    
    
  } 

