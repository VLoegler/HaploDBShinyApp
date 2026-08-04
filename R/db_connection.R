# PostgreSQL connection utilities

create_postgres_conn <- function() {

  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    port = as.integer(Sys.getenv("DB_PORT")),
    sslmode = "require"
  )

}