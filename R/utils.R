read_table <- function(pattern, sc, db, tbls) {
  n <- tbls$tableName[grepl(pattern, tbls$tableName)]
  sdf_sql(sc, glue("SELECT * FROM {db}.{n}"))
}