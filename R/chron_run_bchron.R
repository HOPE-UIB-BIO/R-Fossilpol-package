#' @title Run Bchron age-depth model estimation
#' @param data_source Chronology control table
#' @param n_iterations The number of iterations to run
#' @param n_burn The number of starting iterations to discard
#' @param n_thin The step size for every iteration to keep beyond the burnin
#' @return output `Bchron` class object
#' @description Try to run Bchron. If not successful, return NA
#' @export
chron_run_bchron <-
  function(data_source,
           n_iterations = 10e3,
           n_burn = 2e3,
           n_thin = 8) {
    RUtilpol::check_class("data_source", "data.frame")

    RUtilpol::check_col_names(
      "data_source",
      c(
        "chroncontrolid",
        "chroncontrolage",
        "error",
        "depth",
        "cal_curves",
        "thickness"
      )
    )

    RUtilpol::check_class("n_iterations", "numeric")

    RUtilpol::check_class("n_burn", "numeric")

    RUtilpol::check_class("n_thin", "numeric")

    current_frame <- sys.nframe()

    current_env <- sys.frame(which = current_frame)

    try(
      successful_result <-
        Bchron::Bchronology(
          ages = data_source$chroncontrolage,
          ageSds = data_source$error,
          positions = data_source$depth,
          calCurves = data_source$cal_curves,
          positionThicknesses = data_source$thickness,
          ids = data_source$chroncontrolid,
          iterations = n_iterations,
          burn = n_burn,
          thin = n_thin
        ),
      silent = TRUE
    )

    if (
      !exists("successful_result", envir = current_env)
    ) {
      successful_result <- NA_real_
    }

    return(successful_result)
  }
