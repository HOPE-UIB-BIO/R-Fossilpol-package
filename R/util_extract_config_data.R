#' @title Extract Config information from Global environment
#' @return List with all the important data-related settings
#' @description Gather information about current setting config.
#' @export
#' @keywords internal
util_extract_config_data <- function() {

  # helper function to extract variable from global environment and replace it
  # with NA if not available
  get_safely <- function(var_name) {
    result <- NA
    tryCatch(
      result <- get(
        var_name,
        envir = .GlobalEnv
      ),
      error = function(e) NULL
    )
    return(result)
  }

  res_list <-
    list(
      version = get_safely("workflow_version"),
      date = get_safely("current_date"),
      general = list(
        geography_criteria = get_safely("long_min"),
        long_max = get_safely("long_max"),
        lat_min = get_safely("lat_min"),
        lat_max = get_safely("lat_max"),
        alt_min = get_safely("alt_min"),
        alt_max = get_safely("alt_max"),
        other_data = get_safely("other_data")
      ),
      Neotoma = list(
        dataset_type = get_safely("dataset_type"),
        sel_var_element = get_safely("sel_var_element"),
        chron_order = get_safely("chron_order")
      ),
      age_depth_models = list(
        min_n_of_control_points = get_safely("min_n_of_control_points"),
        default_thickness = get_safely("default_thickness"),
        default_error = get_safely("default_error"),
        max_age_error = get_safely("max_age_error"),
        guess_depth = get_safely("guess_depth"),
        default_iteration = get_safely("default_iteration"),
        default_burn = get_safely("default_burn"),
        default_thin = get_safely("default_thin"),
        iteration_multiplier = get_safely("iteration_multiplier")
      ),
      site_filtering = list(
        pollensum = list(
          filter_by_pollen_sum = get_safely("filter_by_pollen_sum"),
          min_n_grains = get_safely("min_n_grains"),
          target_n_grains = get_safely("target_n_grains"),
          percentage_samples = get_safely("percentage_samples")
        ),
        filter_by_age_limit = get_safely("filter_by_age_limit"),
        extrapolation = list(
          filter_by_extrapolation = get_safely("filter_by_extrapolation"),
          maximum_age_extrapolation = get_safely("maximum_age_extrapolation"),
          filter_by_interest_region = get_safely("filter_by_interest_region"),
          n_levels = list(
            filter_by_number_of_levels = get_safely("filter_by_number_of_levels"),
            min_n_levels = get_safely("min_n_levels")
          ),
          use_age_quantiles = get_safely("use_age_quantiles"),
          use_bookend_level = get_safely("use_bookend_level")
        )
      ),
      session_info = utils::sessionInfo()
    )

  RUtilpol::check_class("res_list", "list")

  return(res_list)
}
