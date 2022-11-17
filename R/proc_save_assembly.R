#' @title Save the final data assemblage
#' @param data_source Data.frame with `dataset_id` and other variables
#' @param select_final_variables Logical. Should the final variables be selected
#' by interactively via R console? If FALSE, all variables will be selected
#' @param user_sel_variables Vector with variables, which have to be present in the
#' final data assembly
#' @param dir Path to the data storage folder
#' @export
proc_save_assembly <-
  function(data_source,
           select_final_variables,
           user_sel_variables = c(),
           dir) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names("data_source", "dataset_id")

    RUtilpol::check_class("select_final_variables", "logical")

    RUtilpol::check_class(
      "user_sel_variables",
      c(
        "NULL",
        "character"
      )
    )

    RUtilpol::check_class("dir", "character")


    if (
      select_final_variables == TRUE
    ) {
      RUtilpol::output_heading(
        msg = "Start selection of data variables"
      )

      full_var_list <-
        data_source %>%
        dplyr::select(
          !any_of(
            c("dataset_id", user_sel_variables)
          )
        ) %>%
        names()

      selected_vars <-
        tibble::tibble(
          var = full_var_list
        ) %>%
        dplyr::mutate(
          include = purrr::map_lgl(
            .x = var,
            .f = ~ util_confirm(
              msg = paste(
                "Do you want to include:",
                RUtilpol::paste_as_vector(.x)
              )
            )
          )
        )

      RUtilpol::check_col_names(
        "selected_vars",
        c("var", "include")
      )

      var_to_include <-
        selected_vars %>%
        dplyr::filter(include == TRUE) %>%
        purrr::pluck("var")

      data_assembly <-
        data_filtered %>%
        dplyr::select(
          dplyr::any_of(
            c("dataset_id", var_to_include, user_sel_variables)
          )
        )
    } else {
      data_assembly <- data_source
    }

    util_save_if_latests(
      file_name = "data_assembly",
      dir = paste0(dir, "/Outputs/Data/")
    )
  }
