utils::globalVariables("where")
#' @title Plot pollen diagram for all sequences
#' @param data_source Data.frame with `dataset_id`, `counts_harmonised`, `levels`,
#' and `region`
#' @param dir Path to the data storage folder
#' @param min_n_occur Min occurrence of taxa to be displayed
#' @param max_taxa Number of taxa to plotted per one page
#' @param y_var The name of the variable to be used as Y-axis in the chart
#' @param date Date to be used to save the figures
#' @export
plot_all_pollen_diagrams <-
  function(data_source,
           dir,
           min_n_occur = 3,
           max_taxa = 20,
           y_var = c("age", "depth"),
           date) {
    util_check_class("data_source", "data.frame")

    util_check_col_names(
      "data_source",
      c(
        "dataset_id",
        "counts_harmonised",
        "levels",
        "region"
      )
    )

    util_check_class("dir", "character")

    util_check_class("min_n_occur", "numeric")

    util_check_class("max_taxa", "numeric")

    util_check_class("y_var", "character")

    util_check_the_latest_file("y_var", c("depth", "age"))

    y_var <- match.arg(y_var)


    util_check_class("date", "Date")

    util_output_comment(
      msg = "Transforming data to percentages"
    )

    data_filtered_percentages <-
      data_source %>%
      dplyr::mutate(
        data_percent = purrr::map(
          .x = counts_harmonised,
          .f = ~ .x %>%
            dplyr::mutate(
              row_sum = .x %>%
                dplyr::select(
                  !dplyr::any_of("sample_id")
                ) %>%
                rowSums()
            ) %>%
            dplyr::mutate(
              dplyr::across(
                where(is.numeric),
                ~ (.x / row_sum) * 100
              )
            ) %>%
            dplyr::select(-row_sum)
        )
      )

    util_check_col_names("data_filtered_percentages", "data_percent")

    # Plot pollen diagrams -----

    util_output_message(
      msg = "Plotting of data"
    )

    pollen_dir <-
      paste0(dir, "/Outputs/Figures/Pollen_diagrams")

    sel_regions <-
      data_filtered_percentages$region %>%
      unique() %>%
      sort()

    util_make_fig_dir(
      dir = pollen_dir,
      region_vector = sel_regions,
      sel_date = date
    )

    most_recent_folder <-
      util_check_the_latest_file(
        file_name = NA,
        dir = pollen_dir,
        folder = TRUE
      )

    save_path <- paste0(
      pollen_dir, "/", most_recent_folder, "/"
    )


    data_filtered_percentages %>%
      purrr::pwalk(
        .l = list(
          .$data_percent, # ..1
          .$levels, # ..2
          .$dataset_id, # ..3
          .$region # ..4
        ),
        .f = ~ {
          util_output_comment(
            msg = paste0("Saving figure for dataset ", ..3)
          )

          plot_pollen_diagram_rioja(
            data_percentages = ..1,
            levels = ..2,
            dataset_id = ..3,
            min_n_occur = min_n_occur,
            max_taxa = max_taxa,
            dir = paste0(save_path, ..4, "/")
          )
        }
      )

    util_open_dir(
      dir = save_path
    )
  }
