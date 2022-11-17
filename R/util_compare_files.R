#' @title Compare two datasets
#' @param file_name_a The name of the object to compare in quotes
#' @param file_name_b The name of the object to compare in quotes
#' @param file_a_envir The environment where is the `file_a` present
#' @param file_b_envir The environment where is the `file_b` present
#' @return Logical. Are those file the same?
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' Compare the two files and return if same
util_compare_files <-
  function(file_name_a,
           file_name_b,
           file_a_envir = NULL,
           file_b_envir = NULL) {
    lifecycle::deprecate_warn(
      "0.0.2", "util_compare_files()", "RUtilpol::compare_files()"
    )
    RUtilpol::check_class("file_name_a", "character")

    RUtilpol::check_class("file_name_b", "character")

    RUtilpol::check_class(
      "file_a_envir",
      c(
        "NULL",
        "environment"
      )
    )

    RUtilpol::check_class(
      "file_b_envir",
      c(
        "NULL",
        "environment"
      )
    )

    current_frame <- sys.nframe()
    parent_frame <- sys.parent()

    current_env <- sys.frame(which = current_frame)
    parent_env <- sys.frame(which = parent_frame)

    if (
      is.null(file_a_envir)
    ) {
      file_a_envir <- parent_env
    }

    if (
      is.null(file_b_envir)
    ) {
      file_b_envir <- parent_env
    }

    # assume that datasets are different
    are_datasets_same <- FALSE

    # class of the data
    file_class_a <-
      file_name_a %>%
      get(., envir = file_a_envir) %>%
      class()

    file_class_b <-
      file_name_b %>%
      get(., envir = file_b_envir) %>%
      class()

    # lists
    if (
      any(file_class_a == "list") | any(file_class_b == "list")
    ) {
      # size
      file_length_a <-
        file_name_a %>%
        get(., envir = file_a_envir) %>%
        length()

      file_length_b <-
        file_name_b %>%
        get(., envir = file_b_envir) %>%
        length()

      if (
        file_length_a == file_length_b
      ) {
        full_list_a <-
          file_name_a %>%
          get(., envir = file_a_envir) %>%
          unlist()

        full_list_b <-
          file_name_b %>%
          get(., envir = file_b_envir) %>%
          unlist()

        if (
          all(full_list_a == full_list_b)
        ) {
          are_datasets_same <- TRUE
        }
      }
    }

    # data frames
    if (
      any(file_class_a == "data.frame") | any(file_class_b == "data.frame")
    ) {
      # size of two datasets
      dim_a <-
        file_name_a %>%
        get(., envir = file_a_envir) %>%
        dim()

      dim_b <-
        file_name_b %>%
        get(., envir = file_b_envir) %>%
        dim()

      # if same size
      if (
        all(dim_a == dim_b)
      ) {
        are_datasets_same <-
          all.equal(
            get(file_name_a, envir = file_a_envir),
            get(file_name_b, envir = file_b_envir),
            check.attributes = FALSE
          )

        are_datasets_same <- all(are_datasets_same == TRUE)
      }
    }

    return(are_datasets_same)
  }
