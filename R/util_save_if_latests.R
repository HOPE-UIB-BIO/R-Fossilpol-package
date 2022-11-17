#' @title Save the newest version of dataset
#' @param file_name Name of the object to save in quotes
#' @param dir Directory path
#' @param prefered_format format to save as: "rda" or "csv"
#' @param system_setting_file Character. Name of the object in global environments
#' which contains a description of setting
#' @param compress Should `gz` compression be applied?
#' @return NULL
#' @keywords internal
#' @description
#' `r lifecycle::badge("deprecated")`
#' Look into the folder and find the version of the file with
#'  the most recent name. Compare the last saved file and selected file and
#'   save file if something changed since recent
util_save_if_latests <- function(file_name,
                                 dir,
                                 system_setting_file = "current_setting",
                                 prefered_format = c("rds", "csv"),
                                 compress = TRUE) {
  lifecycle::deprecate_warn(
    "0.0.2", "util_save_if_latests()", "RUtilpol::save_latest_file()"
  )
  current_frame <- sys.nframe()
  parent_frame <- sys.parent()

  current_env <- sys.frame(which = current_frame)
  parent_env <- sys.frame(which = parent_frame)

  RUtilpol::check_class("file_name", "character")

  RUtilpol::check_class("dir", "character")

  RUtilpol::check_class("prefered_format", "character")

  RUtilpol::check_vector_values("prefered_format", c("rds", "csv"))

  prefered_format <- match.arg(prefered_format)

  RUtilpol::check_class("compress", "logical")

  has_setting <- TRUE

  if (
    !exists(system_setting_file, envir = .GlobalEnv)
  ) {
    usethis::ui_info(
      paste(
        "File", system_setting_file, "not found.",
        "Not going to attach global setting"
      )
    )
    has_setting <- FALSE
  }

  latest_file_name <-
    RUtilpol::get_latest_file_name(
      file_name = file_name,
      dir = dir,
      silent = TRUE
    )

  if (
    is.na(latest_file_name)
  ) {
    usethis::ui_done(
      paste(
        "Have not find previous version of the file. Saved the file with",
        prefered_format, "format."
      )
    )

    if (
      prefered_format == "rds" & has_setting == TRUE
    ) {
      file_to_save <-
        list(
          data = get(file_name, envir = parent_env),
          setting = get(system_setting_file, envir = .GlobalEnv)
        )
    } else {
      file_to_save <- get(file_name, envir = parent_env)
    }

    assign("file_to_save", file_to_save, envir = current_env)

    RUtilpol::check_if_loaded(
      file_name = "file_to_save",
      env = current_env,
      silent = TRUE
    )

    if (
      compress == TRUE
    ) {
      save_command <-
        paste0(
          "write_", prefered_format, "(file_to_save, '", dir, "/",
          file_name, "-", current_date, ".", prefered_format,
          "',compress = 'gz')"
        )
    } else {
      save_command <-
        paste0(
          "write_", prefered_format, "(file_to_save, '", dir, "/",
          file_name, "-", current_date, ".", prefered_format,
          "')"
        )
    }

    eval(parse(text = save_command), envir = current_env)

    # stop the funtcion
    return("Done")
  }

  lastest_file_format <-
    stringr::str_extract(latest_file_name, "\\..*") %>%
    stringr::str_replace(., "\\.", "")

  # construct the command to load the file
  load_command <-
    paste0(
      "lastest_file <- read_", lastest_file_format,
      "('", dir, "/", latest_file_name, "')"
    )

  # load the file (evaluate )
  eval(parse(text = load_command), envir = current_env)

  # get rid of the setting if present
  if (
    any(names(lastest_file) == "data")
  ) {
    lastest_file <- lastest_file$data
  }

  assign("lastest_file", lastest_file, envir = current_env)

  # compare the files
  is_the_lastest_same <-
    util_compare_files(
      file_name_a = file_name,
      file_a_envir = parent_env,
      file_name_b = "lastest_file",
      file_b_envir = current_env
    )

  # if there are changes is the current file to the saved one
  if (
    is_the_lastest_same == FALSE
  ) {
    usethis::ui_done("Found older file, overwriting it with new changes.")

    if (
      lastest_file_format == "rds" & has_setting == TRUE
    ) {
      file_to_save <-
        list(
          data = get(file_name, envir = parent_env),
          setting = get(system_setting_file, envir = .GlobalEnv)
        )
    } else {
      file_to_save <- get(file_name, envir = parent_env)
    }

    assign("file_to_save", file_to_save, envir = current_env)

    RUtilpol::check_if_loaded(
      file_name = "file_to_save",
      env = current_env,
      silent = TRUE
    )

    if (
      compress == TRUE
    ) {
      save_command <-
        paste0(
          "write_", lastest_file_format, "(file_to_save, '",
          dir, "/", file_name, "-", current_date, ".", lastest_file_format,
          "',compress = 'gz')"
        )
    } else {
      save_command <-
        paste0(
          "write_", lastest_file_format, "(file_to_save, '",
          dir, "/", file_name, "-", current_date, ".", lastest_file_format,
          "')"
        )
    }

    eval(parse(text = save_command), envir = current_env)

    return("Done")
  }

  usethis::ui_info("Found older file but did not detect any changes since last version. Not saving.")
  return("Done")
}
