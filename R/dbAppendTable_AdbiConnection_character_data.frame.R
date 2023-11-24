#' @rdname DBI
#' @usage NULL
dbAppendTable_AdbiConnection <- function(conn, name, value, ...,
    row.names = NULL) {

  if (!is.null(row.names)) {
    stop("Can't pass `row.names` to `dbAppendTable()`", call. = FALSE)
  }

  # TODO: how about using dbWriteTable(..., append = TRUE) here?

  query <- sqlAppendTable(
    con = conn,
    table = name,
    values = value,
    row.names = row.names,
    ...
  )

  dbExecute(conn, query)
}

#' @rdname DBI
#' @export
setMethod(
  "dbAppendTable",
  signature("AdbiConnection"),
  dbAppendTable_AdbiConnection
)