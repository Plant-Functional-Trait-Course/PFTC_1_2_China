#' Makes connection to MYSQL database
#' 
#' @param username is the username
#' @param password is password
#' @param host is host
#' @param port is port
#' @param dbname Database name
#' @details Use dbDisconnect(con) to close connection
#' @return Connection to database
#' @examples
#' \dontrun{make_connection()}


#' @export
#' @importFrom DBI dbConnect


make_connection<-function(username = "", password = "", host = "127.0.0.1", port = 3306, dbname = "transplant"){
  con <- dbConnect (RMySQL::MySQL(),
                    username = username,
                    password = password,
                    host = host,
                    port = port,
                    dbname = dbname)
  con
}
