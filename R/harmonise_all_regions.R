#' @title Harmonise each region by its harmonisation table
#' @param data_source Data.frame with `dataset_id`, `harmonisation_region`, and
#' `raw_counts`
#' @param harmonisation_tables Data.frame with `harmonisation_region` and 
#' `harm_table`
#' @param original_name Character. Name of the column in harmonisation table 
#' to be used.
#' @param harm_level Name of the column to use for harmonisation
#' @param exclude_taxa Character. Name of the taxa, which should be omitted.
#' @param pollen_grain_test Logical. Test for difference in sum of total 
#' pollen sum.
#' @export
harmonise_all_regions <-
  function(data_source,
           harmonisation_tables,
           original_name = "taxon_name",
           harm_level = "level_1",
           exclude_taxa = "delete",
           pollen_grain_test = TRUE) {
    
    util_check_class("data_source", "data.frame")
 
    util_check_col_names(
      "data_source",
      c(
        "dataset_id",
        "harmonisation_region",
        "raw_counts"
      ))
       
    util_check_class("harmonisation_tables", "data.frame")
    
    util_check_col_names(
      "harmonisation_tables",
      c(
        "harmonisation_region",
        "harm_table"
      ))
    
    util_check_class("original_name", "character")
    
    util_check_class("harm_level", "character")
    
    util_check_class("exclude_taxa", "character")
    
    util_check_class("pollen_grain_test", "logical")
    
    util_output_message(
      msg = "Start harmonisation of taxa")
    
    data_harmonised <-
      data_source %>%
      dplyr::left_join(
        harmonisation_tables,
        by = "harmonisation_region") %>% 
      dplyr::mutate(
        counts_harmonised = pmap(
          .l = list(dataset_id, raw_counts, harm_table),
          .f = ~ {
            
            util_output_comment(
              msg = paste("Dataset", ..1)
            )
            
            harmonise_taxa(
              data_source = ..2,
              harmonisation_table = ..3,
              original_name = original_name,
              harm_name = harm_level, 
              exclude_taxa = exclude_taxa,
              pollen_grain_test = pollen_grain_test 
            ) %>% 
              return()
            
          })) %>% 
      dplyr::select(-harm_table)
    
    util_check_col_names("data_harmonised", "counts_harmonised")
    
    util_output_comment(
      msg = "Harmonisation completed")
    
    # test of number of levels before and after harmonisation
    util_stop_if_not(
      all(
        purrr::map_dbl(data_harmonised$counts_harmonised, nrow) ==
          purrr::map_dbl(data_harmonised$raw_counts, nrow)),
      false_msg = "Harmonisation unsuccessfull. Detected changes in number of levels",
      true_msg = paste(
        "Not detected any changes in number of levels between before",
        "and after harmonisation."))
    
    return(data_harmonised)
  }
