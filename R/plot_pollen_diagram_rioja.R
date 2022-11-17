#' @title Plot a pollen diagram using the rioja package
#' @param data_percentages Data.frame pollen percentages
#' @param levels Data.frame with level information
#' @param y_var The name of the variable to be used as Y-axis in the chart
#' @param dataset_id Unique ID of the sequences
#' @param min_n_occur Min occurrence of taxa to be displayed
#' @param max_taxa Number of taxa to plotted per one page
#' @param dir Folder to save the figure to
#' @description Plot a pollen diagram using the `rioja` package
plot_pollen_diagram_rioja <-
  function(data_percentages,
           levels,
           y_var = c("age", "depth"),
           dataset_id,
           min_n_occur = 3,
           max_taxa = 20,
           dir) {
    RUtilpol::check_class("data_percentages", "data.frame")

    RUtilpol::check_col_names("data_percentages", "sample_id")

    RUtilpol::check_class("levels", "data.frame")

    RUtilpol::check_col_names(
      "levels",
      c(
        "sample_id",
        "depth",
        "age",
        "upper",
        "lower"
      )
    )

    RUtilpol::check_class("y_var", "character")

    util_check_the_latest_file("y_var", c("depth", "age"))

    y_var <- match.arg(y_var)

    RUtilpol::check_class("dataset_id", "character")

    RUtilpol::check_class("min_n_occur", "numeric")

    RUtilpol::check_class("max_taxa", "numeric")

    RUtilpol::check_class("dir", "character")

    data_percentages_work <-
      data_percentages %>%
      dplyr::filter(sample_id %in% levels$sample_id) %>%
      dplyr::select(
        !dplyr::any_of("sample_id")
      )

    if (
      ncol(data_percentages_work) < 1
    ) {
      RUtilpol::output_comment(
        msg = paste0("WARNING: Dataset ", dataset_id, " not valid - not saved")
      )

      return()
    }

    # replace all NA with zeros
    data_percentages_work[is.na(data_percentages_work)] <- 0

    # select only taxa which have a minimal number occurrences
    data_percentages_sel <-
      data_percentages_work[, colSums(data_percentages_work > 0) > min_n_occur]

    if (
      nrow(data_percentages_sel) < 0 | ncol(data_percentages_sel) < 1
    ) {
      RUtilpol::output_comment(
        msg = paste0("WARNING: Dataset ", dataset_id, " not valid - not saved")
      )

      return()
    }

    ages <-
      levels$age %>%
      round(., 0)

    depths <-
      levels$depth

    switch(y_var,
      "age" = {
        y_var_val <- ages
      },
      "depth" = {
        y_var_val <- depths
      },
      rlang::abort(
        paste(y_var, "has not been defined", sep = " ")
      )
    )

    switch(y_var,
      "age" = {
        y_val_name <- "Age (yrs BP)"
      },
      "depth" = {
        y_val_name <- "Depth (m)"
      },
      rlang::abort(
        paste(y_var, "has not been defined", sep = " ")
      )
    )

    ntaxa <- ncol(data_percentages_sel)

    need_plots <- ceiling(ntaxa / max_taxa)

    graphics::layout(matrix(1, 2, byrow = FALSE), widths = c(1, 1), heights = c(1, 1))

    grDevices::pdf(paste0(dir, dataset_id, ".pdf"))

    for (i in 1:need_plots) {
      keep <- 1:ntaxa > (i - 1) / need_plots * ntaxa & 1:ntaxa <= i / need_plots * ntaxa
      ex1 <- colSums(data_percentages_sel[, keep]) < 10
      tk1 <- seq(min(y_var_val), max(y_var_val), by = 100)
      c_0 <- tk1[seq(1, length(tk1), 5)]

      graphics::par(fig = c(0.03, 1, 0.007, 1))

      rioja::strat.plot(
        d = data_percentages_sel[, keep],
        yvar = y_var_val,
        y.tks = c_0,
        exag = ex1,
        exag.mult = 10,
        col.exag = "darkgrey",
        wa.order = "bottomleft",
        y.rev = TRUE,
        y.axis = FALSE,
        plot.line = FALSE,
        plot.poly = TRUE,
        col.poly.line = "black",
        plot.bar = TRUE,
        col.poly = "darkgreen",
        scale.percent = TRUE,
        ylabel = "",
        cex.xlabel = 0.7,
        cex.axis = 0.7,
        cex.yaxis = 0.6,
        xLeft = 0.1,
        yTop = 0.75,
        srt.xlabel = 55
      )

      graphics::mtext(
        text = paste0("dataset_id = ", dataset_id),
        side = 3, line = 2.5, adj = 0.025, cex = 1.5
      )

      graphics::par(fig = c(0, 1, 0.075, 0.752))
      graphics::axis(side = 2, at = tk1, labels = FALSE)
      graphics::axis(side = 2, at = c_0, labels = c_0, las = 2, cex.axis = 0.7)
      graphics::mtext(y_val_name, side = 2, line = 2.5)
    }
    grDevices::graphics.off()
  }
