#' @title Recalibrate all age-depth models base on chronology control tables 
#' @param data_source Data.frame containing `dataset_id` and `chron_control_format`
#' @param batch_size Number of individual sequences re-calibrate in a single 
#' batch using parallel computation
#' @param number_of_cores Number of CPU cores to use in parallel computation
#' @param default_iteration The number of iterations used by Bchron 
#' @param default_burn The number of starting iterations to discard used by Bchron 
#' @param default_thin The step size for every iteration to keep beyond 
#' the burnin used by Bchron 
#' @param iteration_multiplier Value to be used to multiply `iteration`, `burn`,
#' `thin` to keep the ratio between them same
#' @param set_seed User-defined seed for randomisations
#' @param dir Path to the data storage folder
#' @param batch_attempts Number of tries each batch should be considered 
#' before skipping it
#' @param time_per_sequence Time (in sec) dedicated for each sequence to estimate 
#' age-depth model. If it takes computer longer that selected value, estimation 
#' is considered as unsuccessful and skipped. The time value is mutliplied by 
#' `iteration_multiplier` as more itiration required more time. Time for whole 
#' batch is calculated as `time_per_sequence` multiplied by `iteration_multiplier` 
#' multiplied by the number of sequences per batch (which is estimated based on 
#' `number_of_cores`)    
#' @export
chron_recalibrate_ad_models <-
  function(data_source,
           batch_size = 1,
           number_of_cores = 1,
           default_iteration = 10e3,
           default_burn = 2e3,
           default_thin = 8,
           iteration_multiplier = 5,
           set_seed = 1234,
           batch_attempts = 3,
           time_per_sequence = 120,
           dir) {
    
    util_check_class("data_source", "data.frame")
    
    util_check_col_names(
      "data_source", 
      c(
        "dataset_id",
        "chron_control_format"
      ))
    
    util_check_class("batch_size", "numeric")
    
    util_check_class("number_of_cores", "numeric")
    
    util_check_class("default_iteration", "numeric")
    
    util_check_class("default_burn", "numeric")
    
    util_check_class("default_thin", "numeric")
    
    util_check_class("iteration_multiplier", "numeric")
    
    util_check_class("set_seed", "numeric")
    
    util_check_class("dir", "character")  
    
    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)
    
    temp_path <- "/Data/Processed/Chronology/Temporary_output/"
    
    # Variables definition for computation  -----
    
    set.seed(set_seed)
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
    
    n_iterations <- default_iteration * iteration_multiplier
    n_burn <- default_burn * iteration_multiplier
    n_thin <- default_thin * iteration_multiplier
    
    
    # test if there are any sequences to run AD modelling
    if(nrow(data_source) < 1) {
      
      util_output_comment(
        msg = paste(
          "There are no sequences for AD calculation,",
          "re-calibration will be skipped in current run"))
      
      # create an empty column for data without sequences 
      chron_output <-
        data_source %>% 
        dplyr::mutate(
          bchron_mod = NA_real_)
      
      util_check_col_names("chron_output", "bchron_mod")
      
      return(chron_output)
    }
    
    # detect the number of batches needed to split the data into base on the
    #   `batch_size` selected by user
    number_of_batches <- 
      ceiling( nrow(data_source) / batch_size ) 
    
    # a dummy data.freme to keep track of progress and number of tries for each batch
    batch_success_table <-
      tibble::tibble(
        batch_number = 1:number_of_batches,
        batch_name = paste0("batch_", formatC(batch_number, width = 3, flag = 0)),
        # list of sequences in each batch
        sequence_list = purrr::map(
          .x = batch_number,
          .f = ~ data_source %>% 
            dplyr::slice(
              seq(from = batch_size * (.x - 1) + 1,
                  to = min(c(batch_size * (.x), nrow(data_source))),
                  by = 1)) %>% 
            purrr::pluck("dataset_id")),
        # write sequences as vector
        sequence_vec = purrr::map_chr(
          .x = sequence_list,
          .f = ~ util_paste_as_vector(.x, sep = "")),
        # nubmer of sequecnes in each batch
        batch_size = purrr::map_dbl(
          .x = sequence_list,
          .f = length),
        # set a value for the process to wait (in seconds) for each batch
        time_to_stop = (batch_size * time_per_sequence) * max(c((floor(iteration_multiplier/2)), 1)), 
        done = FALSE, 
        .rows = number_of_batches)
    
    util_check_if_loaded(
      file_name = "batch_success_table",
      env = current_env)
    
    util_check_col_names(
      "batch_success_table",
      c("batch_number",
        "batch_name",
        "sequence_list",
        "batch_size",
        "time_to_stop",
        "done")
    )
    
    # open custom menu to select confirmation
    style_selection <-
      switch(
        utils::menu(
          choices = c("both (batches and then individual) - recommended", "batches", "individual"),
          title = cat(
            "User can specify which kind of age-depth modeling wants to do", "\n",
            "\n",
            "batches = Several age-depth models are then created",
            "at the same time using parralel computation.",
            "This is done by splitting the sequences into batches, with each",
            "batch containing a certain number of sequences", "\n",
            "\n",
            "individual = age-depth models for sequencesare estimated one by one","\n",
            "\n",
            "both = Workflow will try to estimate age-deptj models in batches",
            "and then are “failed” batches estimated one by one.",
            "\n"
          )),
        "both", "batches", "individual")
    
    if(
      style_selection != "individual"
    ) {
      
      rerun_batches <-
        util_confirm_based_on_presence(
          dir = paste0(dir, temp_path),
          file_name = "chron_result_batch",
          msg = "Detected previsous batch results, do you want to rerun them?")
      
      if(
        rerun_batches == TRUE
      ) {
        
        util_output_comment( 
          paste(
            "Age-depth re-calibration will be done in", number_of_batches, "batches\n",
            "Each batch will be have", batch_attempts, "attempts to calculate.",
            "In the case that the whole bach is unsuccessful all", batch_attempts, "times,",
            "another subroutine can be used to calculate age-depth models",
            "for each sequences individually."))
        
        chron_result_batch <-
          chron_recalibrate_in_batches(
            data_source_chron = data_source,
            data_source_batch = batch_success_table,
            dir = dir,
            n_iterations = n_iterations, 
            n_burn = n_burn,
            n_thin = n_thin,
            number_of_cores = number_of_cores,
            set_seed = set_seed,
            maximum_number_of_loops = batch_attempts)
      } else {
        
        chron_result_batch <-
          readr::read_rds(
            file = paste0(dir, temp_path, "chron_result_batch.rds"))
      }
      
      chron_output_batch <- 
        chron_result_batch %>% 
        purrr::pluck("bchron_output")
      
      batch_success_table <- 
        chron_result_batch %>% 
        purrr::pluck("batch_success_table")
      
    }
    
    if(
      style_selection == "batches"
    ) {
      
      util_check_col_names("chron_output_batch", "bchron_mod")
      
      return(chron_output_batch)
    }
    
    if(
      style_selection != "batches"
    ) {
      if(
        all(batch_success_table$done) == FALSE
      ) {
        
        # calculate the number of failed batches
        number_of_fails <- 
          batch_success_table %>% 
          dplyr::filter(done == FALSE) %>% 
          purrr::pluck("batch_size") %>% 
          sum()
        
        util_output_comment(
          msg = paste(
            "Individual calculation will be done for", number_of_fails, "sequences"))
        
        chron_output_individual <- 
          chron_recalibrate_individual(
            data_source_chron = data_source,
            data_source_batch = batch_success_table,
            n_iterations = n_iterations, 
            n_burn = n_burn,
            n_thin = n_thin,
            dir = dir)
        
      } else {
        
        chron_output_individual <- NULL
        
      }
    }
    
    if(
      style_selection == "individual"
    ) {
      
      util_check_col_names("chron_output_individual", "bchron_mod")
      
      return(chron_output_individual)
    }
    
    if(
      style_selection == "both"
    ){
      
      bchron_output <- 
        dplyr::bind_rows(
          chron_output_batch,
          chron_output_individual) %>% 
        dplyr::distinct(dataset_id, .keep_all = TRUE)
      
      util_check_col_names("bchron_output", "bchron_mod")
      
      return(bchron_output)
    }
  }
