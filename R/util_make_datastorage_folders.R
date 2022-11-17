#' @title Test presence and create folders if necessary
#' @param dir Path to the selected directory
#' @keywords internal
#' @export 
util_make_datastorage_folders <-
  function(dir) {
    if (
      !any(list.files(dir) %in% c("Data"))
    ) {
      dir.create(
        path = paste0(dir, "/Data")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data")) %in% c("Input"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Chronology_setting"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Chronology_setting")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input/Chronology_setting")) %in% c("Bchron_crash"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Chronology_setting/Bchron_crash")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input/Chronology_setting")) %in% c("Chron_control_point_types"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Chronology_setting/Chron_control_point_types")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input/Chronology_setting")) %in% c("Percentage_radiocarbon"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Chronology_setting/Percentage_radiocarbon")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Depositional_environment"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Depositional_environment")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input/Depositional_environment")) %in% c("Neotoma"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Depositional_environment/Neotoma")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input/Depositional_environment")) %in% c("Other"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Depositional_environment/Other")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Eco_group"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Eco_group")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Harmonisation_tables"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Harmonisation_tables")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Neotoma_download"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Neotoma_download")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Potential_duplicates"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Potential_duplicates")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Private"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Private")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Regional_age_limits"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Regional_age_limits")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Input")) %in% c("Author_info"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Input/Author_info")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data")) %in% c("Personal_database_storage"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Personal_database_storage")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data")) %in% c("Processed"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Chronology"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Chronology")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Chronology")) %in% c("Chron_tables_prepared"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Chronology/Chron_tables_prepared")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Chronology")) %in% c("Models_full"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Chronology/Models_full")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Chronology")) %in% c("Predicted_ages"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Chronology/Predicted_ages")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Chronology")) %in% c("Temporary_output"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Chronology/Temporary_output")
      )
      cat("This is a temporary file and will be probabyl changed often",
        file = paste0(dir, "/Data/Processed/Chronology/Temporary_output/README_NOTE.txt"),
        sep = "\n"
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Data_filtered"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Data_filtered")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Data_harmonised"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Data_harmonised")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Data_merged"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Data_merged")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Data_with_chronologies"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Data_with_chronologies")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Neotoma_processed"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Neotoma_processed")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Neotoma_processed")) %in% c("Neotoma_chron_control"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Neotoma_processed/Neotoma_chron_control")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Neotoma_processed")) %in% c("Neotoma_dep_env"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Neotoma_processed/Neotoma_dep_env")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed/Neotoma_processed")) %in% c("Neotoma_meta"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Neotoma_processed/Neotoma_meta")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Data/Processed")) %in% c("Private"))
    ) {
      dir.create(
        path = paste0(dir, "/Data/Processed/Private")
      )
    }

    if (
      !any(list.files(paste0(dir)) %in% c("Outputs"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Outputs")) %in% c("Data"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs/Data")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Outputs")) %in% c("Figures"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs/Figures")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Outputs/Figures")) %in% c("Chronology"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs/Figures/Chronology")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Outputs/Figures")) %in% c("Pollen_diagrams"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs/Figures/Pollen_diagrams")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Outputs")) %in% c("Tables"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs/Tables")
      )
    }

    if (
      !any(list.files(paste0(dir, "/Outputs/Tables")) %in% c("Meta_and_references"))
    ) {
      dir.create(
        path = paste0(dir, "/Outputs/Tables/Meta_and_references")
      )
    }
  }
