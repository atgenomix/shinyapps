#' Title
#' @param pattern
#' @param sc
#' @param db
#' @param tbls
#' @return
#' @examples
#' @noRd
#' @importFrom glue glue
read_table <- function(pattern, db_tbls) {
  n <- db_tbls$tbls$tableName[grepl(pattern, db_tbls$tbls$tableName)]
  sdf_sql(db_tbls$sc, glue("SELECT * FROM {db_tbls$db}.{n}"))
}


get_db_tables <- function(sc, db) {
  tbl_change_db(sc, db)
  list(sc = sc, db = db, tbls = dbGetQuery(sc, glue("SHOW TABLES IN {db}")))
}
