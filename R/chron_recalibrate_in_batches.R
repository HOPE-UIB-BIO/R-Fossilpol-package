#' @title Recalibrate the selected sequences in defined batches using parallel 
#' computation
#' @param data_source_chron Data.frame containing `dataset_id` and `chron_control_format`
#' @param data_source_batch Data.frame with the description of the status
#' of the individual batches 
#' @param dir Path to the data storage folder 
#' @param n_iterations The number of iterations used by Bchron 
#' @param n_burn The number of starting iterations to discard used by Bchron 
#' @param n_thin The step size for every iteration to keep beyond 
#' the burnin used by Bchron 
#' @param number_of_cores Number of CPU cores to use in parallel computation
#' @param set_seed User-defined seed for randomisations
#' @param maximum_number_of_loops Number of tries each batch should be considered 
#' before skipping it
#' @return Named list with `bchron_output`, `batch_success_table`
chron_recalibrate_in_batches <- 
  function(data_source_chron,
           data_source_batch,
           dir,
           n_iterations = 10e3, 
           n_burn = 2e3,
           n_thin = 8,
           number_of_cores = 1,
           set_seed = 1234,
           maximum_number_of_loops = 3) {
    
    util_check_class("data_source_chron", "data.frame")
    
    util_check_col_names("data_source_chron", "chron_control_format")
    
    util_check_class("data_source_batch", "data.frame")
    
    util_check_col_names(
      "data_source_batch", 
      c(
        "batch_number",
        "batch_name",
        "batch_size",
        "done"))
    
    util_check_class("n_iterations", "numeric")
    
    util_check_class("n_burn", "numeric")
    
    util_check_class("n_thin", "numeric")
    
    util_check_class("number_of_cores", "numeric")
    
    util_check_class("set_seed", "numeric")
    
    util_check_class("maximum_number_of_loops", "numeric")
    
    current_frame <- sys.nframe()
    current_env <- sys.frame(which = current_frame)
    
    temp_path <- "/Data/Processed/Chronology/Temporary_output/"
    
    # Check previous results -----
    
    # detect previous batches
    prev_batch <- 
      list.files(
        path = paste0(dir, temp_path),
        pattern = "batch.") %>% 
      stringr::str_replace(., ".rds", "")
    
    if(
      length(prev_batch) > 0
    ){
      util_output_comment(
        msg = paste("Decteted n =", length(prev_batch), "previous batches"))
      
      # change all batches which are deteced from last time to TRUE
      data_source_batch[data_source_batch$batch_name %in% prev_batch, ]$done <- TRUE
    }
    
    # create a loop counter, which starts at 1
    loop_counter <- 1
    
    number_of_batches <- 
      data_source_batch %>% 
      dplyr::distinct(batch_number) %>% 
      nrow()
    
    # Chronology computation  -----
    
    # start the loop
    repeat{
      
      util_output_comment(
        paste("Attempt number", loop_counter, "for all batches")
      )
      
      # For all batches that are not done
      for(i in seq_along(data_source_batch$batch_name)){  
        
        current_batch_n <- data_source_batch$batch_number[i]
        current_batch_name <- data_source_batch$batch_name[i]
        
        if(
          data_source_batch$done[i] == FALSE
        ) {
          
          util_output_comment(
            msg = paste("batch", current_batch_n, "out of", number_of_batches))
          
          # subset data in the selected batch
          temp_data <- 
            data_source_chron %>%
            dplyr::filter(dataset_id %in% data_source_batch$sequence_list[[i]])
          
          # setup plan for parallel computation
          future::plan(
            strategy = future::multisession(), 
            workers = number_of_cores)
          
          # try to run Chronology
          try(expr = {
            # compute within time period. If longer than `time_to_stop`, 
            #   stop computation
            R.utils::withTimeout({
              bchron_temp_run <- 
                temp_data %>%
                dplyr::mutate(
                  bchron_mod = furrr::future_map(
                    .x = chron_control_format,
                    .f = ~ chron_run_bchron(
                      data_source = .x,
                      n_iterations = n_iterations, #[config_criteria] 
                      n_burn = n_burn, #[config_criteria] 
                      n_thin = n_thin), #[config_criteria] 
                    .options = furrr::furrr_options(seed = set_seed))) #[config_criteria]  
              
              # assign the test run temporary result 
              assign("bchron_temp", bchron_temp_run, envir = current_env)
              
              # delete test run
              rm(bchron_temp_run, envir = current_env)
              
            },
            timeout = data_source_batch$time_to_stop[[i]],
            onTimeout = "silent")
          },
          silent = TRUE)
          
          util_output_comment(
            msg = paste("batch", current_batch_n, "finished"))
          
          # if temporary result is produced
          if(
            exists("bchron_temp", envir = current_env) == TRUE
          ) { 
            
            util_check_col_names("bchron_temp", "bchron_mod")
            
            # save the batch as temporarily 
            readr::write_rds(
              bchron_temp,
              paste0(dir, temp_path,
                     current_batch_name,".rds"),
              compress = "gz")
            
            if(
              list.files(
                path = paste0(dir, temp_path),
                pattern = "batch.") %>% 
              stringr::str_detect(., current_batch_name) %>% 
              any()
            ){
              # mark status of batch
              data_source_batch$done[i] <- TRUE
              
              util_output_comment(
                msg = "attempt = successful")
            } 
            
            # remove the temporary result
            rm(bchron_temp, envir = current_env)
            
          } else {
            util_output_comment(
              msg = "attempt = unsuccessful")
          }
        }
      }
      
      # if all batches are marked as succesfull, break 
      if(all(data_source_batch$done) == TRUE) break
      
      # if cycle runs more times than it is a maximum number, break
      if(loop_counter >= maximum_number_of_loops) break
      
      # loop is finished, increase loop counter
      loop_counter <- loop_counter + 1
    }
    
    # Merge all the successful batches together  -----
    
    # load the batches
    pres_batch <- 
      list.files(
        path = paste0(dir, temp_path),
        pattern = "batch.") %>% 
      stringr::str_replace(., ".rds", "")
    
    # detect the number of successful batches
    number_of_successes <- 
      length(pres_batch)
    
    util_output_comment(
      msg = paste(
        "Chronology was successfully calculated for", number_of_successes, "batches"))
    
    # merge all successfully batches together
    bchron_output <-
      purrr::map_dfr(
        .x = pres_batch,
        .f = ~  readr::read_rds(
          paste0(dir, temp_path,
                 .x,".rds"))) %>% 
      dplyr::bind_rows()
    
    
    util_stop_if_not(
      exists("bchron_output", envir = current_env),
      true_msg = "All batches were successfully merged together",
      false_msg = "there has been an issue with loading of individual batch files"
    )
    
    chron_result_batch <-
      list(
        bchron_output,
        data_source_batch) %>% 
      purrr::set_names(
        nm = c("bchron_output", "batch_success_table")
      )
    
    util_output_comment("Saving temporarily output")
    
    readr::write_rds(
      x = chron_result_batch,
      file = paste0(dir, temp_path, "chron_result_batch.rds")
    )
    
    util_output_comment("Deleting temporarily batches output")
    
    # delete all the batch files
    purrr::walk(
      .x = pres_batch,
      .f = ~ file.remove(
        paste0(dir, temp_path, .x, ".rds")
      ))
    
    return(chron_result_batch)
    
  }
