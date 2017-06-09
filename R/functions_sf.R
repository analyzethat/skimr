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
