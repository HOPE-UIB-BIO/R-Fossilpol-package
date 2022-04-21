#' @title Extract the selected site into Neotoma-style data.frame
#' @param selected_file A name of the site to import in quotes
#' @param dir Directory path
#' @param suffix A character, which would add to all new IDs 
#' (dataset, authors, affiliations, etc).
#' @param project_db File including the project dataset database
#' @return Neotoma-style data.frame
#' @description Import the dataset in excel format, check if dataset 
#' is unique and assign unique author and affiliation IDs, add count data, 
#' chronology control points, and publication information, and make data.frame 
#' consistent with Neotoma-processed style data.
extract_dataset_from_excel <- 
  function(selected_file, 
           dir,
           suffix = "",
           project_db){
    
    util_check_class("selected_file", "character")
    util_check_class("dir", "character")
    util_check_class("suffix", "character")
    util_check_class("project_db", "proj_db_class")
    
    #----------------------------------------------------------#
    # 0. Check if dataset already in database -----
    #----------------------------------------------------------#
    
    # define the required variables
    site_meta_var_sel <- 
      c("site_name", "lat", "long", "elev", "dept_env")
    
    # get the meta data
    suppressWarnings(
      site_meta <-
        readxl::read_excel(
          path = paste0(dir, "/", selected_file),
          sheet = "site_meta",
          col_names = TRUE) %>% 
        dplyr::select(
          dplyr::all_of(site_meta_var_sel)) %>% 
        dplyr::mutate(
          site_name = as.character(site_name),
          lat = as.double(lat),
          long = as.double(long),
          elev = as.double(elev),
          dept_env = as.character(dept_env))
    )
    
    util_check_class("site_meta", "data.frame")
    util_check_col_names("site_meta", site_meta_var_sel)
    
    is_already_in_database <-
      site_meta %>% 
      purrr::pluck("site_name") %in%
      db_Datasets_id_tab(project_db)$sitename
    
    if(is_already_in_database) {
      cat(" - Dataset already in the database", "\n")
    }
    
    
    #----------------------------------------------------------#
    # 1. Get author information -----
    #----------------------------------------------------------#
    
    if (
      is_already_in_database == FALSE
    ) {
      
      # Define number of authors and affiliations in the database
      n_author_id <-  0
      n_affiliations_id <-  0
      
      if( 
        nrow(db_Authors(project_db)) > 0
      ) {
        n_author_id <-
          db_Authors(project_db) %>%
          dplyr::distinct(author_id) %>% 
          tidyr::drop_na() %>% 
          purrr::pluck("author_id") %>% 
          length()
        
      }
      
      if( 
        nrow(db_Affiliations(project_db)) > 0
      ) {
        n_affiliations_id <-
          db_Affiliations(project_db) %>% 
          dplyr::distinct(affiliation_id) %>% 
          tidyr::drop_na() %>% 
          purrr::pluck("affiliation_id") %>% 
          length()
      }
      
      # define the required variables
      author_sheet_var_sel <-
        c("author_ID",
          "author_number",
          "first_name",
          "middle_letters",
          "last_name",
          "email",
          "orcid",
          "affiliation")
      
      # get all authors
      suppressWarnings(
        author_sheet <-
          readxl::read_excel(
            path = paste0(dir, "/", selected_file),
            sheet = "authors",
            col_names = TRUE) %>%
          dplyr::select(
            dplyr::all_of(author_sheet_var_sel)) %>% 
          dplyr::mutate(
            author_ID = as.character(author_ID),
            author_number = as.numeric(author_number),
            first_name = as.character(first_name),
            middle_letters = as.character(middle_letters),
            last_name = as.character(last_name),
            email = as.character(email),
            orcid = as.character(orcid),
            affiliation = as.character(affiliation)) %>% 
          dplyr::rename(
            author_id = author_ID) %>% 
          tidyr::drop_na(author_number, last_name)
      )
      
      util_check_class("author_sheet", "data.frame")
      util_check_col_names("author_sheet", 
                                       stringr::str_replace(author_sheet_var_sel,
                                                            "author_ID",
                                                            "author_id"))
      
      
      #----------------------------------------#
      # 1.1.1. Check if Author is not already in database ----
      #----------------------------------------#
      
      # pre-allocate space
      author_sheet <-
        author_sheet %>%
        dplyr::mutate(
          author_already_in_database = FALSE,
          matching_var = vector("list", length = nrow(.))
        )
      
      util_check_col_names("author_sheet", "author_already_in_database")
      
      for (j in 1:nrow(author_sheet)){
        
        database_info <-
          db_Authors(project_db) %>%
          dplyr::filter(last_name == author_sheet$last_name[j])
        
        if(
          nrow(database_info) > 0
        ) {
          
          for(l in 1:nrow(database_info)){
            
            database_info_selected <-
              database_info[l,] %>%
              dplyr::select(first_name, middle_letters, email, orcid) %>%
              unlist()
            
            file_info <-
              author_sheet[j, ] %>%
              dplyr::select(first_name, middle_letters, email, orcid) %>%
              unlist()
            
            vars_matching <- 
              database_info_selected == file_info
            
            matching_var <- 
              names(database_info_selected)[which(vars_matching)]
            
            author_sheet$matching_var[[j]] <- 
              c(author_sheet$matching_var[[j]], matching_var) %>% 
              unique()
            
            author_sheet$author_already_in_database[j] <-
              ifelse(author_sheet$author_already_in_database[j] == TRUE,
                     TRUE,
                     any(vars_matching, na.rm = TRUE))
          }
        }
      }
      
      if(
        any(author_sheet$author_already_in_database, na.rm = TRUE)
      ) {
        
        for( k in 1:nrow(author_sheet)) {
          
          if(
            author_sheet$author_already_in_database[k] == TRUE
          ) {
            
            cat(" - Author already detected in database", "\n")  
            
            match_var <- author_sheet$matching_var[[k]]
            
            # create a list of author names and ids
            author_database_simplified <-
              db_Authors(project_db) %>%
              dplyr::select(last_name, author_id, eval(match_var)) %>% 
              dplyr::distinct() 
            
            # add author id 
            author_sheet$author_id[k] <- 
              author_sheet[k, ] %>% 
              dplyr::select(last_name, eval(match_var)) %>% 
              dplyr::left_join(author_database_simplified, by = c("last_name", match_var)) %>% 
              purrr::pluck("author_id") 
            
            rm(author_database_simplified, match_var)
            
          }
        }
      } 
      
      #----------------------------------------#
      # 1.1.2. Add unique author id ----
      #----------------------------------------#
      
      n_authors_present <- 
        sum(author_sheet$author_already_in_database)
      
      author_sheet <-
        author_sheet %>% 
        dplyr::arrange(author_id) %>% 
        dplyr::mutate(
          order = dplyr::row_number(),
          author_id = ifelse(
            is.na(author_id) == TRUE,
            paste0(formatC(n_author_id + order - n_authors_present,
                           width = 3,
                           flag = 0),
                   "_",
                   suffix),
            author_id)) %>% 
        dplyr::select(-order)
      
      util_check_col_names("author_sheet", "author_id")
      
      #----------------------------------------#
      # 1.1.3. Affiliation ----
      #----------------------------------------#
      
      # define the required variables
      affiliation_sheet_var_sel <-
        c("affiliation_number",
          "Department",
          "Company",
          "BusinessStreet1",
          "BusinessStreet2",
          "BusinessStreet3",
          "BusinessCity",
          "BusinessState",
          "BusinessPostalCode",
          "BusinessCountryRegion")
      
      # load affiliations
      suppressWarnings(
        affiliation_sheet <-
          readxl::read_excel(
            path = paste0(dir, "/", selected_file),
            sheet = "affiliations",
            col_names = TRUE) %>% 
          dplyr::select(
            dplyr::all_of(affiliation_sheet_var_sel)) %>% 
          dplyr::mutate(
            affiliation_number = as.numeric(affiliation_number),
            Department = as.character(Department),
            Company = as.character(Company),
            BusinessStreet1 = as.character(BusinessStreet1),
            BusinessStreet2 = as.character(BusinessStreet2),
            BusinessStreet3 = as.character(BusinessStreet3),
            BusinessCity = as.character(BusinessCity),
            BusinessState = as.character(BusinessState),
            BusinessPostalCode = as.character(BusinessPostalCode),
            BusinessStreet2 = as.character(BusinessStreet2)) %>% 
          tidyr::drop_na(affiliation_number)
      )
      
      util_check_class("affiliation_sheet", "data.frame")
      util_check_col_names("affiliation_sheet", affiliation_sheet_var_sel)
      
      # create transform table
      tranlation_table <- 
        author_sheet %>% 
        dplyr::distinct(author_number, affiliation) %>% 
        tidyr::drop_na(affiliation) %>% 
        dplyr::mutate(
          affiliation_list = purrr::map(
            .x = affiliation, 
            .f = ~ tibble::tibble(
              affiliation_number = stringr::str_split(.x, pattern = ",") %>% 
                unlist() %>% 
                as.numeric()))) %>% 
        tidyr::unnest(affiliation_list) %>% 
        dplyr::distinct(author_number, affiliation_number)
      
      # pre-allocate space
      affiliation_sheet <-
        affiliation_sheet %>%
        dplyr::distinct(affiliation_number, .keep_all = TRUE) %>%
        dplyr::mutate(
          affiliations_already_in_database = FALSE,
          matching_var = vector("list", length = nrow(.)),
          affiliation_id = NA_character_,
          Department = as.character(ifelse(is.na(Department) == TRUE, Company, Department)))
      
      util_check_col_names(
        "affiliation_sheet",
        c("affiliations_already_in_database",
          "affiliation_id",
          "Department"))
      
      for(j in 1:nrow(affiliation_sheet)){
        
        database_info <-
          db_Affiliations(project_db) %>% 
          tidyr::drop_na(Department) %>% 
          dplyr::filter(Department == affiliation_sheet$Department[j])
        
        if(
          nrow(database_info) > 0
        ) {
          
          for(l in 1:nrow(database_info)){
            
            database_info_selected <-
              database_info[l,] %>%
              dplyr::select(Company, BusinessStreet1, BusinessStreet2,
                            BusinessStreet3, BusinessCity, BusinessState,
                            BusinessPostalCode, BusinessCountryRegion ) %>%
              unlist()
            
            file_info <-
              affiliation_sheet[j, ] %>%
              dplyr::select(Company, BusinessStreet1, BusinessStreet2,
                            BusinessStreet3, BusinessCity, BusinessState,
                            BusinessPostalCode, BusinessCountryRegion ) %>%
              unlist()
            
            vars_matching <- 
              database_info_selected == file_info
            
            matching_var <- 
              names(database_info_selected)[which(vars_matching)]
            
            affiliation_sheet$matching_var[[j]] <- 
              c(affiliation_sheet$matching_var[[j]], matching_var) %>% 
              unique()
            
            affiliation_sheet$affiliations_already_in_database[j] <-
              ifelse(affiliation_sheet$affiliations_already_in_database[j] == TRUE,
                     TRUE,
                     any(vars_matching, na.rm = TRUE))
          }
        }
      }
      
      if(
        any(affiliation_sheet$affiliations_already_in_database, na.rm = TRUE)
      ) {
        
        for( k in 1:nrow(affiliation_sheet)) {
          
          if(
            affiliation_sheet$affiliations_already_in_database[k] == TRUE
          ) {
            
            cat(" - Affiliation already detected in database", "\n")
            
            match_var <- affiliation_sheet$matching_var[[k]]
            
            # create a list of author names and ids
            aff_database_simplified <-
              db_Affiliations(project_db) %>%
              tidyr::drop_na(Department) %>%
              dplyr::select(Department, eval(match_var), affiliation_id) %>% 
              dplyr::distinct()
            
            # add affiliation id 
            affiliation_sheet$affiliation_id[k] <- 
              affiliation_sheet[k, ] %>% 
              dplyr::select(Department, eval(match_var)) %>% 
              dplyr::left_join(aff_database_simplified, by = c("Department", match_var)) %>% 
              purrr::pluck("affiliation_id") 
            
            rm(aff_database_simplified, match_var)
            
          }
        }
      } 
      
      n_aff_present <- 
        sum(affiliation_sheet$affiliations_already_in_database)
      
      # add unique affiliation id
      affiliation_sheet <-
        affiliation_sheet %>%
        dplyr::arrange(affiliation_id) %>% 
        dplyr::mutate(
          order = dplyr::row_number(),
          affiliation_id = ifelse(
            is.na(affiliation_id) == TRUE,
            paste0(formatC(n_affiliations_id + order - n_aff_present,
                           width = 3,
                           flag = 0),
                   "_",
                   suffix),
            as.character(affiliation_id)) %>% 
            as.character()) %>%
        dplyr::select(-order) %>% 
        dplyr::relocate(affiliation_id)
      
      util_check_col_names("affiliation_sheet", "affiliation_id")
      
      # Adjust the translation table using the unique id's
      formated_tranlation_table <-
        author_sheet %>%
        dplyr::select(author_number, author_id) %>%
        dplyr::inner_join(
          tranlation_table,
          by = "author_number") %>%
        dplyr::inner_join(
          affiliation_sheet %>%
            dplyr::select(affiliation_number, affiliation_id),
          by = "affiliation_number") %>%
        dplyr::select(author_id, affiliation_id)
      
      #----------------------------------------#
      # 1.1.4. Add to database ----
      #----------------------------------------#
      
      # add author sheet to database
      db_Authors(project_db) <-
        author_sheet
      
      # add affiliation_sheet to database
      db_Affiliations(project_db) <- 
        affiliation_sheet
      
      # add Translation_table to database
      db_Auth_aff_tab(project_db) <- formated_tranlation_table
      
    }
    
    #----------------------------------------------------------#
    # 2. Dataset id database -----
    #----------------------------------------------------------#
    
    if(
      is_already_in_database == FALSE
    ) {
      
      n_datasets_id <-
        db_Datasets_id_tab(project_db) %>% 
        dplyr::distinct(dataset_id) %>% 
        purrr::pluck("dataset_id") %>% 
        length()
      
      datasets_id_table <-
        site_meta %>%
        dplyr::mutate(
          dataset_id = paste0(formatC(n_datasets_id + dplyr::row_number(),
                                      width = 3,
                                      flag = 0),
                              "_",
                              suffix)) %>%
        dplyr::select(dataset_id, sitename = site_name)
      
      util_check_col_names(
        "datasets_id_table",
        c("dataset_id",
          "sitename")
      )
      
      db_Datasets_id_tab(project_db) <-
        datasets_id_table
      
      author_dataset_table <-
        author_sheet %>%
        dplyr::select(author_id) %>%
        dplyr::mutate(
          dataset_id = datasets_id_table$dataset_id)
      
      db_Auth_dataset_tab(project_db) <-
        author_dataset_table
      
      site_meta <-
        site_meta %>%
        dplyr::mutate(
          dataset_id = datasets_id_table$dataset_id) %>%
        dplyr::relocate(dataset_id)
      
      util_check_col_names("site_meta", "dataset_id")
      
      #--------------------------------------------------#
      # 2.1. Add publication to database -----
      #--------------------------------------------------#
      
      # define the required variables
      site_publication_var_sel <-
        c("publ_number",
          "publication")
      
      suppressWarnings(
        site_publication <-
          readxl::read_excel(
            path = paste0(dir, "/", selected_file),
            sheet = "publication") %>%
          dplyr::select(
            dplyr::all_of(site_publication_var_sel)) %>% 
          dplyr::mutate(
            publ_number = as.numeric(publ_number),
            publication = as.character(publication))
      )
      
      util_check_class("site_publication", "data.frame")
      util_check_col_names("site_publication", site_publication_var_sel)
      
      site_publication <-
        site_publication %>% 
        dplyr::mutate(
          dataset_id  = datasets_id_table$dataset_id) %>%
        dplyr::select(dataset_id, publication)
      
      util_check_col_names("site_publication", "dataset_id")
      
      db_Dataset_pub(project_db) <-
        site_publication
      
    } else {
      
      dataset_id <-
        db_Datasets_id_tab(project_db) %>% 
        dplyr::filter(sitename == site_meta$site_name) %>%
        purrr::pluck("dataset_id") 
      
      site_meta <-
        site_meta %>%
        dplyr::mutate(
          dataset_id = dataset_id[1]) %>%
        dplyr::relocate(dataset_id)
      
      util_check_col_names("site_meta", "dataset_id")
      
    }
    
    
    #----------------------------------------------------------#
    # 2. Count table information -----
    #----------------------------------------------------------#
    
    # extract pollen count sheet
    site_counts <-
      readxl::read_excel(
        path = paste0(dir, "/", selected_file),
        sheet = "counts",
        col_names = TRUE) 
    
    util_check_col_names("site_counts", "depth")
    
    # create sample depth table
    sample_depth <-
      site_counts %>%
      dplyr::select(depth) %>%
      dplyr::mutate(
        sample_id = as.integer(dplyr::row_number())) %>%
      dplyr::select(sample_id, depth)
    
    util_check_col_names(
      "sample_depth",
      c("sample_id", "depth"))
    
    # create raw count table
    suppressWarnings(
      raw_counts <-
        site_counts %>%
        dplyr::select(!dplyr::any_of("depth")) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::everything(),
            as.numeric)) %>% 
        dplyr::mutate(
          sample_id = as.integer(dplyr::row_number())) %>%
        dplyr::relocate(sample_id)
    )
    
    util_check_col_names("raw_counts", "sample_id")
    
    #----------------------------------------------------------#
    # 3. Chronologies information -----
    #----------------------------------------------------------#
    
    # extract chronology sheet
    suppressWarnings(
      chron_control <-
        readxl::read_excel(
          path = paste0(dir, "/", selected_file),
          sheet = "chronology",
          col_names = TRUE) %>%
        dplyr::rename(
          chroncontroltype = control_type,
          chroncontrolid = lab_number) %>%
        dplyr::mutate(
          depth = as.numeric(depth),
          thickness = as.numeric(thickness),
          age = as.numeric(age),
          age_error = as.numeric(age_error),
          chroncontroltype = as.character(chroncontroltype),
          chroncontrolid = as.character(chroncontrolid)) %>%
        dplyr::mutate(
          chroncontrolage = purrr::map_dbl(
            .x = age,
            .f = ~ ifelse(is.numeric(.x), .x, NA_real_)),
          age_error = purrr::map_dbl(
            .x = age_error,
            .f = ~ ifelse(is.numeric(.x), .x, NA_real_)),
          agelimityounger = purrr::map2_dbl(
            .x = age_error,
            .y = age,
            .f = ~ ifelse(is.na(.x), NA_real_ ,.y - .x)),
          agelimitolder = purrr::map2_dbl(
            .x = age_error,
            .y = age,
            .f = ~ ifelse(is.na(.x), NA_real_ ,.y + .x))) %>%
        dplyr::select(
          dplyr::any_of(
            c("depth", 
              "thickness", 
              "chroncontrolage", 
              "agelimitolder",
              "agelimityounger", 
              "chroncontrolid", 
              "chroncontroltype")))
      
      # do not check for variablea as they might be missing
    )
    
    
    #----------------------------------------------------------#
    # 4. Construct the final table -----
    #----------------------------------------------------------#
    
    final_table <-
      tibble::tibble(
        site_meta %>% 
          dplyr::rename(
            sitename = site_name,
            altitude = elev,
            depositionalenvironment = dept_env),
        sample_depth = list(sample_depth),
        chron_control = list(chron_control),
        raw_counts = list(raw_counts)) %>%
      dplyr::mutate(
        n_sample_counts = purrr::map_dbl(raw_counts, nrow),
        n_chron_control = purrr::map_dbl(chron_control, nrow))
    
    util_check_col_names(
      "final_table",
      c("sitename",
        "altitude",
        "depositionalenvironment",
        "sample_depth",
        "chron_control",
        "raw_counts",
        "n_sample_counts",
        "n_chron_control"))
    
    res <-
      list(data = final_table,
           db = project_db)
    
    return(res)
    
  }
