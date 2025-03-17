#' @rdname AdbiConnection-class
#' @inheritParams DBI::dbDataType
#' @usage NULL
dbDataType_AdbiConnection <- function(dbObj, obj, ...) {
  db_data_type(obj, dbObj@connection$database$driver)
}

#' @rdname AdbiConnection-class
#' @export
setMethod("dbDataType", "AdbiConnection", dbDataType_AdbiConnection)

db_data_type <- function(x, drv) {
  if (inherits(x, "blob")) {
    db_data_type_blob(drv)
  } else {
    if (is.logical(x)) {
      # most adbi drivers, at least snowflake, has a BOOLEAN type
      # https://github.com/r-dbi/adbi/issues/15
      'BOOLEAN'
    } else {
      dbDataType(ANSI(), x)
    }
  }
}
