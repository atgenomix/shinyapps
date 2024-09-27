#' Title
#' @param pattern
#' @param sc
#' @param db
#' @param tbls
#' @return
#' @examples
#' @noRd
#' @importFrom glue glue
read_table <- function(pattern, sc, db, tbls) {
  n <- tbls$tableName[grepl(pattern, tbls$tableName)]
  sdf_sql(sc, glue("SELECT * FROM {db}.{n}"))
}