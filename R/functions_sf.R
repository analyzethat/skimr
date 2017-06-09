#' @include stats_sfc.R
NULL

sfc_point_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)

sfc_linestring_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)

sfc_polygon_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)

sfc_multipoint_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)

sfc_multilinestring_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)

sfc_multipolygon_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid)
)

sfc_geometry_funs<-list(
  missing = n_missing,
  complete = n_complete,
  n = length,
  n_unique = purrr::compose(length, n_unique),
  valid = purrr::compose(sum, sf::st_is_valid),
  funny = funny
)


.sfc <- list(
  sfc_POINT = sfc_point_funs,
  sfc_LINESTRING = sfc_linestring_funs,
  sfc_POLYGON = sfc_polygon_funs,
  sfc_MULTIPOINT = sfc_multipoint_funs,
  sfc_MULTILINESTRING = sfc_multilinestring_funs,
  sfc_MULTIPOLYGON= sfc_multipolygon_funs,
  sfc_GEOMETRY = sfc_geometry_funs
)


# Build environment for storing functions ---------------------------------

functions <- new.env()
functions$default <- c(.default, .sfc)
functions$current <- c(.default, .sfc)

#' Set or add the summary functions for a particular type of data
#' 
#' @param ... A list of functions, with an argument name that matches a
#'   particular data type.
#' @param append Whether the provided functions should be in addition to the
#'   defaults already in skim.
#' @export

# skim_with <- function(..., append = TRUE) {
#   funs <- list(...)
#   nms <- purrr::map(funs, names)
#   has_null <- purrr::map_lgl(nms, ~any(is.null(.x)))
#   if (any(has_null)) {
#     msg <- paste(names(funs)[has_null], collapse = ", ")
#     stop("A function is missing a name within this type: ", msg)
#   }
#   all <- purrr::map2(names(funs), funs, set_functions, append)
# }
# 
# set_functions <- function(type, newfuns, append) {
#   exists <- type %in% names(functions$current)
#   
#   if (!exists) {
#     message("Adding new summary functions for type: ", type)
#   } else if (append) {
#     old <- functions$current[[type]]
#     newfuns <- c(old, newfuns)
#   }
#   
#   functions$current[[type]] <- newfuns
# }


#' @describeIn skim_with Use the default functions within skim
#' @export

# skim_with_defaults <- function() {
#   assign("current", .default, envir = functions)
# }
# 

#' Show summary functions currently used, by column type
#' 
#' @return Nothing. \code{invisible()}
#' @export

# show_skimmers <- function() {
#   lapply(functions$current, names)
# }


# A method for getting a set of summary functions, by type
#
# @param type The type of summary functions to extract
# @return A list of summary functions
# @keywords internal
# @export

# get_funs <- function(type) {
#   all <- functions$current[type]
#   purrr::detect(all, purrr::compose(`!`, is.null))
# }

# A method for getting the name of set of summary functions, by type
#
# @param type The type of summary functions to extract
# @return A list of summary functions
# @keywords internal
# @export

# get_fun_names <- function(type) {
#   all <- functions$current[type]
#   id <- purrr::detect_index(all, purrr::compose(`!`, is.null))
#   names(all)[id]
# }