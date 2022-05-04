#' @title Testing any criteria and either stop or produce msg
#' @param ... Unnamed expressions that describe the conditions to be tested
#' @param env The environment in which to evaluate the test
#' @param false_msg Error message to be printed when stopping
#' @param true_msg Comment to be printed when positive result of test
#' @description Test a selected criteria an output comment or stop
#' @export
util_stop_if_not <- 
  function(...,
           env = parent.frame(),
           false_msg = NULL,
           true_msg = NULL){
    
    res <- assertthat::see_if(..., env = env, msg = false_msg)
    
    if (res){
      cat("\n")
      usethis::ui_done(true_msg)
      cat("\n")
    } else {
      cat("\n")
      usethis::ui_oops(
        paste("WARNING:", attr(res, "msg")))
      cat("\n")
      util_stop_quietly()  
    } 
  }




