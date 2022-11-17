#  S4 structure for private data manipulation

# proj_db_class -----
#' @title Project dataset database
#' @slot Authors data.frame with information about authors
#' @slot Affiliations data.frame with affiliation details
#' @slot Auth_aff_tab data.frame linking authors and affiliations
#' @slot Datasets_id_tab data.frame with information about individual datasets
#' @slot Auth_dataset_tab data.frame linking authors and datasets
#' @slot Dataset_pub data.frame with publications for datasets
#' @export
proj_db_class <-
  setClass(
    "proj_db_class",
    slots = c(
      Authors = "data.frame",
      Affiliations = "data.frame",
      Auth_aff_tab = "data.frame",
      Datasets_id_tab = "data.frame",
      Auth_dataset_tab = "data.frame",
      Dataset_pub = "data.frame"
    ),
    prototype = list(
      Authors = tibble::tibble(
        author_id = character(),
        first_name = character(),
        middle_letters = character(),
        last_name = character(),
        email = character(),
        orcid = character()
      ),
      Affiliations = tibble::tibble(
        affiliation_id = character(),
        Department = character(),
        Company = character(),
        BusinessStreet1 = character(),
        BusinessStreet2 = character(),
        BusinessStreet3 = character(),
        BusinessCity = character(),
        BusinessState = character(),
        BusinessPostalCode = character(),
        BusinessCountryRegion = character()
      ),
      Auth_aff_tab = tibble::tibble(
        author_id = character(),
        affiliation_id = character()
      ),
      Datasets_id_tab = tibble::tibble(
        dataset_id = character(),
        sitename = character()
      ),
      Auth_dataset_tab = tibble::tibble(
        dataset_id = character(),
        author_id = character()
      ),
      Dataset_pub = tibble::tibble(
        dataset_id = character(),
        publication = character()
      )
    ),
    validity = function(object) {
      check_Authors <- db_Authors(object)

      check_Affiliations <- db_Affiliations(object)

      check_Auth_aff_tab <- db_Auth_aff_tab(object)

      check_Datasets_id_tab <- db_Datasets_id_tab(object)

      check_Auth_dataset_tab <- db_Auth_dataset_tab(object)

      check_Dataset_pub <- db_Dataset_pub(object)

      all(
        c(
          RUtilpol::check_class("check_Authors", "data.frame"),
          RUtilpol::check_col_names(
            "check_Authors",
            c(
              "author_id",
              "first_name",
              "middle_letters",
              "last_name",
              "email",
              "orcid"
            )
          ),
          RUtilpol::check_class("check_Affiliations", "data.frame"),
          RUtilpol::check_col_names(
            "check_Affiliations",
            c(
              "affiliation_id",
              "Department",
              "Company",
              "BusinessStreet1",
              "BusinessStreet2",
              "BusinessStreet3",
              "BusinessCity",
              "BusinessState",
              "BusinessPostalCode",
              "BusinessCountryRegion"
            )
          ),
          RUtilpol::check_class("check_Auth_aff_tab", "data.frame"),
          RUtilpol::check_col_names(
            "check_Auth_aff_tab",
            c(
              "author_id",
              "affiliation_id"
            )
          ),
          RUtilpol::check_class("check_Datasets_id_tab", "data.frame"),
          RUtilpol::check_col_names(
            "check_Datasets_id_tab",
            c(
              "dataset_id",
              "sitename"
            )
          ),
          RUtilpol::check_class("check_Auth_dataset_tab", "data.frame"),
          RUtilpol::check_col_names(
            "check_Auth_dataset_tab",
            c(
              "dataset_id",
              "author_id"
            )
          ),
          RUtilpol::check_class("check_Dataset_pub", "data.frame"),
          RUtilpol::check_col_names(
            "check_Dataset_pub",
            c(
              "dataset_id",
              "publication"
            )
          )
        )
      )
    }
  )


#' @title Get the Author table
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Authors",
  def = function(theObject) standardGeneric("db_Authors")
)

#' @title Get the Author table
#' @param theObject object of proj_db_class class
setMethod(
  f = "db_Authors",
  signature = "proj_db_class",
  definition = function(theObject) theObject@Authors
)

#' @title Insert in Author table
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Authors<-",
  def = function(theObject, value) standardGeneric("db_Authors<-")
)

#' @title Insert in Author table
#' @param theObject object of proj_db_class class
#' @description New data will be added but duplicates will be removed
setMethod(
  f = "db_Authors<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {
    theObject@Authors <-
      dplyr::bind_rows(
        theObject@Authors,
        value %>%
          dplyr::select(
            dplyr::any_of(
              names(theObject@Authors)
            )
          )
      ) %>%
      dplyr::distinct(author_id, .keep_all = TRUE) %>%
      dplyr::arrange(last_name, first_name, author_id)
    validObject(theObject)
    return(theObject)
  }
)

#' @title Get the Affiliation table
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Affiliations",
  def = function(theObject) standardGeneric("db_Affiliations")
)

#' @title Get the Affiliation table
#' @param theObject object of proj_db_class class
setMethod(
  f = "db_Affiliations",
  signature = "proj_db_class",
  definition = function(theObject) theObject@Affiliations
)

#' @title Insert in the Affiliation table
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Affiliations<-",
  def = function(theObject, value) standardGeneric("db_Affiliations<-")
)

#' @title Insert in the Affiliation table
#' @param theObject object of proj_db_class class
#' @description New data will be added but duplicates will be removed
setMethod(
  f = "db_Affiliations<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {
    theObject@Affiliations <-
      dplyr::bind_rows(
        theObject@Affiliations,
        value %>%
          dplyr::select(
            dplyr::any_of(
              names(theObject@Affiliations)
            )
          )
      ) %>%
      dplyr::distinct(affiliation_id, .keep_all = TRUE) %>%
      dplyr::arrange(affiliation_id, Department)
    validObject(theObject)
    return(theObject)
  }
)

#' @title Get the table linking Authors and Affiliations
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Auth_aff_tab",
  def = function(theObject) standardGeneric("db_Auth_aff_tab")
)

#' @title Get the table linking Authors and Affiliations
#' @param theObject object of proj_db_class class
setMethod(
  f = "db_Auth_aff_tab",
  signature = "proj_db_class",
  definition = function(theObject) theObject@Auth_aff_tab
)

#' @title Insert in table linking Authors and Affiliations
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Auth_aff_tab<-",
  def = function(theObject, value) standardGeneric("db_Auth_aff_tab<-")
)

#' @title Insert in table linking Authors and Affiliations
#' @param theObject object of proj_db_class class
#' @description New data will be added but duplicates will be removed
setMethod(
  f = "db_Auth_aff_tab<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {
    theObject@Auth_aff_tab <-
      dplyr::bind_rows(
        theObject@Auth_aff_tab,
        value %>%
          dplyr::select(
            dplyr::any_of(
              names(theObject@Auth_aff_tab)
            )
          )
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(author_id, affiliation_id)
    validObject(theObject)
    return(theObject)
  }
)

#' @title Get the table with datasets information
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Datasets_id_tab",
  def = function(theObject) standardGeneric("db_Datasets_id_tab")
)

#' @title Get the table with datasets information
#' @param theObject object of proj_db_class class
setMethod(
  f = "db_Datasets_id_tab",
  signature = "proj_db_class",
  definition = function(theObject) theObject@Datasets_id_tab
)

#' @title Insert in  the table with datasets information
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Datasets_id_tab<-",
  def = function(theObject, value) standardGeneric("db_Datasets_id_tab<-")
)

#' @title Insert in the table with datasets information
#' @param theObject object of proj_db_class class
#' @description New data will be added but duplicates will be removed
setMethod(
  f = "db_Datasets_id_tab<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {
    theObject@Datasets_id_tab <-
      dplyr::bind_rows(
        theObject@Datasets_id_tab,
        value %>%
          dplyr::select(
            dplyr::any_of(
              names(theObject@Datasets_id_tab)
            )
          )
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dataset_id, sitename)
    validObject(theObject)
    return(theObject)
  }
)

#' @title Get the table linking Authors and Datasets
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Auth_dataset_tab",
  def = function(theObject) standardGeneric("db_Auth_dataset_tab")
)

#' @title Get the table linking Authors and Datasets
#' @param theObject object of proj_db_class class
setMethod(
  f = "db_Auth_dataset_tab",
  signature = "proj_db_class",
  definition = function(theObject) theObject@Auth_dataset_tab
)

#' @title Insert in the table linking Authors and Datasets
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Auth_dataset_tab<-",
  def = function(theObject, value) standardGeneric("db_Auth_dataset_tab<-")
)

#' @title Insert in the table linking Authors and Datasets
#' @param theObject object of proj_db_class class
#' @description New data will be added but duplicates will be removed
setMethod(
  f = "db_Auth_dataset_tab<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {
    theObject@Auth_dataset_tab <-
      dplyr::bind_rows(
        theObject@Auth_dataset_tab,
        value %>%
          dplyr::select(
            dplyr::any_of(
              names(theObject@Auth_dataset_tab)
            )
          )
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dataset_id, author_id) %>%
      tidyr::drop_na()
    validObject(theObject)
    return(theObject)
  }
)

#' @title Get the table with publication linked to datasets
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Dataset_pub",
  def = function(theObject) standardGeneric("db_Dataset_pub")
)

#' @title Get the table with publication linked to datasets
#' @param theObject object of proj_db_class class
setMethod(
  f = "db_Dataset_pub",
  signature = "proj_db_class",
  definition = function(theObject) theObject@Dataset_pub
)

#' @title Insert in the table with publication linked to datasets
#' @param theObject object of proj_db_class class
methods::setGeneric(
  name = "db_Dataset_pub<-",
  def = function(theObject, value) standardGeneric("db_Dataset_pub<-")
)

#' @title Insert in the table with publication linked to datasets
#' @param theObject object of proj_db_class class
#' @description New data will be added but duplicates will be removed
setMethod(
  f = "db_Dataset_pub<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {
    theObject@Dataset_pub <-
      dplyr::bind_rows(
        theObject@Dataset_pub,
        value %>%
          dplyr::select(
            dplyr::any_of(
              names(theObject@Dataset_pub)
            )
          )
      ) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dataset_id)
    validObject(theObject)
    return(theObject)
  }
)

#' @title Add data to the database
#' @param theObject object of proj_db_class class
#' @description New data will be automatically added to the specific tables
#' but duplicates will be removed
methods::setGeneric(
  name = "db_add_data<-",
  def = function(theObject, value) standardGeneric("db_add_data<-")
)

#' @title Add data to the database
#' @param theObject object of proj_db_class class
#' @description New data will be automatically added to the specific tables
#' but duplicates will be removed
#' @export
setMethod(
  f = "db_add_data<-",
  signature = "proj_db_class",
  definition = function(theObject, value) {

    # add authors
    theObject@Authors <-
      dplyr::bind_rows(
        theObject@Authors,
        value %>%
          dplyr::select(
            dplyr::any_of(
              c(names(theObject@Authors))
            )
          ) %>%
          dplyr::distinct()
      ) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(author_id) %>%
      dplyr::arrange(last_name, first_name, author_id)

    # add Affiliations
    theObject@Affiliations <-
      dplyr::bind_rows(
        theObject@Affiliations,
        value %>%
          dplyr::select(
            dplyr::any_of(
              c(names(theObject@Affiliations))
            )
          ) %>%
          dplyr::distinct()
      ) %>%
      dplyr::distinct() %>%
      tidyr::drop_na(affiliation_id) %>%
      dplyr::arrange(affiliation_id, Department)

    # add Auth_aff_tab
    theObject@Auth_aff_tab <-
      dplyr::bind_rows(
        theObject@Auth_aff_tab,
        value %>%
          dplyr::select(
            dplyr::any_of(
              c(names(theObject@Auth_aff_tab))
            )
          ) %>%
          dplyr::distinct()
      ) %>%
      dplyr::distinct() %>%
      tidyr::drop_na() %>%
      dplyr::arrange(author_id, affiliation_id)

    # add Auth_dataset_tab
    theObject@Auth_dataset_tab <-
      dplyr::bind_rows(
        theObject@Auth_dataset_tab,
        value %>%
          dplyr::select(
            dplyr::any_of(
              c(names(theObject@Auth_dataset_tab))
            )
          ) %>%
          dplyr::distinct() %>%
          tidyr::drop_na()
      ) %>%
      dplyr::distinct() %>%
      tidyr::drop_na() %>%
      dplyr::arrange(dataset_id, author_id)

    # add Datasets_id_tab
    theObject@Datasets_id_tab <-
      dplyr::bind_rows(
        theObject@Datasets_id_tab,
        value %>%
          dplyr::select(
            dplyr::any_of(
              c(names(theObject@Datasets_id_tab))
            )
          ) %>%
          dplyr::distinct() %>%
          tidyr::drop_na()
      ) %>%
      dplyr::distinct(dataset_id, .keep_all = TRUE) %>%
      tidyr::drop_na(dataset_id) %>%
      dplyr::arrange(dataset_id, sitename)


    validObject(theObject)

    return(theObject)
  }
)

default_show <-
  function(theObject) {
    authors_table <-
      attr(theObject, "Authors") %>%
      dplyr::distinct()

    affiliation_table <-
      attr(theObject, "Affiliations") %>%
      dplyr::distinct()

    dataset_table <-
      attr(theObject, "Datasets_id_tab") %>%
      dplyr::distinct(dataset_id)

    cat(
      paste(
        "Database has:",
        nrow(dataset_table), "datasets",
        nrow(authors_table), "authors,",
        nrow(affiliation_table), "affiliations"
      )
    )
  }

#' @title Show summary of the database
#' @param theObject object of proj_db_class class
setMethod(
  f = "show",
  signature = "proj_db_class",
  definition = function(object) {
    default_show(object)
  }
)
