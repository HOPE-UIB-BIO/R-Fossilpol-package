#' @title Extract calibration curves and copy them into Bchron package
#' @param first_curve_proportion The proportion of contribution of the IntCal curve. Default is 0.5
#' @export
chron_prepare_cal_curves <-
  function(first_curve_proportion = 0.5) {
    RUtilpol::check_class("first_curve_proportion", "numeric")

    assertthat::assert_that(
      first_curve_proportion <= 1 & first_curve_proportion >= 0,
      msg = "'first_curve_proportion' must be betwen 0 and 1"
    )

    # create mixed curve and add to Bchron
    calmixed <-
      rcarbon::mixCurves(
        calCurve1 = "intcal20",
        calCurve2 = "shcal20",
        p = first_curve_proportion, # using the default p of 0.5
        resOffsets = 0,
        resErrors = 0
      )

    # wrap function in so it does not output mesage
    capture.output(
      # creta a mix curve and place is inside of Bchron package
      Bchron::createCalCurve(
        name = "calmixed",
        calAges = calmixed[, 1],
        uncalAges = calmixed[, 2],
        oneSigma = calmixed[, 3],
        pathToCalCurves = system.file("data", package = "Bchron")
      ),
      file = "NUL"
    )

    # check if there is a mix curve file
    name_of_mix_curve_file <-
      util_check_the_latest_file(
        file_name = "calmixed",
        dir = system.file("data", package = "Bchron")
      )

    RUtilpol::stop_if_not(
      is.na(name_of_mix_curve_file) == FALSE,
      false_msg = "The mix curve was not copied in Bchron package",
      true_msg = "Mix curve was succesfully copied to Bchron package"
    )


    # Add post-bomb calibration curves to Bchron  -----

    # Side note: Postbomb calcurves are derived from IntCal package
    # Note:  ?IntCal::copyCalibrationCurve is not updated but there you can see
    #   the setting inside the function:
    # if (postbomb) {
    #   if (cc == 1)
    #     fl <- "postbomb_NH1.14C"
    #   else if (cc == 2)
    #     fl <- "postbomb_NH2.14C"
    #   else if (cc == 3)
    #     fl <- "postbomb_NH3.14C"
    #   else if (cc == 4)
    #     fl <- "postbomb_SH1-2.14C"
    #   else if (cc == 5)
    #     fl <- "postbomb_SH3.14C"
    #   else stop("calibration curve doesn't exist\n",
    #             call. = FALSE)
    # }

    # define the curve names
    postbomb_curve_names <-
      c("nh_zone_1", "nh_zone_2", "nh_zone_3", "sh_zone_1_2", "sh_zone_3")

    postbomb_curve_names_missing <-
      rep(TRUE, length(postbomb_curve_names))

    # save each curve into the Bchron package
    for (i in seq_along(postbomb_curve_names)) {
      selected_curve <-
        IntCal::ccurve(cc = i, postbomb = TRUE)

      # wrap function in so it does not output message
      capture.output(
        Bchron::createCalCurve(
          name = postbomb_curve_names[i],
          calAges = selected_curve[, 1],
          uncalAges = selected_curve[, 2],
          oneSigma = selected_curve[, 3],
          pathToCalCurves = system.file("data", package = "Bchron")
        ),
        file = "NUL"
      )

      # save if the postbomb curve is missing
      postbomb_curve_names_missing[i] <-
        util_check_the_latest_file(
          file_name = postbomb_curve_names[i],
          dir = system.file("data", package = "Bchron")
        ) %>%
        is.na()
    }

    # check if all postbomb curves were successfully copied to Bchron
    RUtilpol::stop_if_not(
      all(!postbomb_curve_names_missing),
      false_msg = paste(
        "The following postbomb curves were NOT copied to Bchron package:",
        util.paste.as.vector(postbomb_curve_names[postbomb_curve_names_missing])
      ),
      true_msg = "Postbomb curves were succesfully copied to Bchron package"
    )
  }
