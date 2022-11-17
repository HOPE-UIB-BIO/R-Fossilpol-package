#' @title Open menu to confirm TRUE/FALSE
#' @param msg Message to be shown with the menu
#' @param default_value Default value of answer
#' @return return logical
#' @export
#' @keywords internal
#' 
util_confirm <-
  function(msg = "",
           default_value = TRUE) {
    RUtilpol::check_class("msg", "character")

    RUtilpol::check_class("default_value", "logical")

    # pre-set to default_value
    confirm <- default_value

    # open custom menu to select confirmation
    confirm <-
      switch(utils::menu(
        choices = c("Yes", "No"),
        title = msg
      ),
      TRUE,
      FALSE
      )

    return(confirm)
  }
