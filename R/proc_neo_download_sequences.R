#' @title Download all sequences from Neotoma
#' @param allds Data.frame with all dataset IDs
#' @param n_tries Number of tries to download each dataset
#' @export
proc_neo_download_sequences <- 
  function(allds, n_tries = 10) {
     
    util_check_class("allds", "data.frame")
    
    util_check_col_names("allds", "dsid")
    
    util_check_class("n_tries", "numeric")
    
    ds_vector <- allds$dsid
    
    rawdownload <- "https://api.neotomadb.org/v2.0/data/downloads"
    
    result_list <- 
      purrr::map(
        .x = seq_along(ds_vector),
        .f = ~ {
          
          # repeat for 'n_tries' or until successfully download the sequence
          for(i in 1:n_tries) {
            
            current_frame <- sys.nframe()
            current_env <- sys.frame(which = current_frame)
            
            # output progress
            cat(paste0(.x, " in ", length(ds_vector)))
            
            # download the sequence 
            res <- 
              httr::GET(paste0(rawdownload, '/', ds_vector[.x]))
            
            # if it was successful download 
            if(res$status_code == 200) { # NOTE: status_code 200 = success
              
              # extract the data
              output <- 
                httr::content(res)$data[[1]]
              
              # output 'success' 
              cat(" - success", "\n")
              
              # break from loop
              break
              
            } else {
              
              # save output as nothing
              output <- NULL
              
              # output 'success'
              cat(" - fail", "\n")
            }
            
            # output NULL for the last try
            if(i == n_tries){
              return(output)
            }
            
            # delete the result and output
            rm(res, output, envir = current_env)
          }
          
          return(output)
        }) %>% 
      purrr::set_names( # add the names of sequences
        nm = ds_vector) 
    
    
    # extract the sequences which were not successfully downloaded
    cannot_download <-
      allds %>% 
      dplyr::filter(purrr::map_lgl(result_list, is.null)) %>% 
      purrr::pluck("dsid")
    
    if(length(cannot_download) == 0){
      usethis::ui_done("All selected sequences were downloaded")
    } else {
      usethis::ui_oops(
        paste(
          length(cannot_download), "out of", nrow(allds),
          "sequences were NOT downloaded.", "\n",
          "Specifically, dataset IDs:",
          util_paste_as_vector(cannot_download)))
    } 
    
    # save the final list as 'neotoma_download' object filtering out unsuccessfull
    #   sequences
    neotoma_download <- 
      result_list[!purrr::map_lgl(result_list, is.null)]
    
    return(neotoma_download)
    
  }
