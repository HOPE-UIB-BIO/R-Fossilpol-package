#' @title Confirm the general setting
#' @param setting_name Name of the setting variable
#' @description Offer the user to temporarily change one logical variable
#' @keywords internal
util_confirm_global_setting <-
  function(setting_name) {
    RUtilpol::check_class(setting_name, "logical")

    assertthat::assert_that(
      exists(setting_name, envir = .GlobalEnv),
      msg = paste(
        RUtilpol::paste_as_vector(setting_name),
        "is not detected in global enviroment"
      )
    )

    user_confirm <-
      util_confirm(
        msg = paste(
          "Please select your preferred setting for",
          RUtilpol::paste_as_vector(setting_name)
        )
      )

    RUtilpol::output_comment(
      paste(
        "Setting", RUtilpol::paste_as_vector(setting_name),
        "has been temporarily set to:", user_confirm
      )
    )


    return(user_confirm)
  }
