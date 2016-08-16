#' Create R connection object to default local MAMP MySQL database
#'
#' This function returns a MySQLConnection object to the
#' named MAMP database (assumes default settings on OS X 10.11.5).
#'
#' You must have MAMP installed and running to use this function.
#'
#' Thank you to Andrew Knight for \href{http://apknightcode.blogspot.com/2012/09/first-post.html}{blogging about this}.
#'
#' @param dbname a string corresponding to a database name in MAMP.
#' @return A MySQLConnection object is returned, and should be passed
#'    to a new variable when calling mampConnect.
#' @examples
#' A database named 'test' must exist in MAMP in order for the example to work.
#' con <- mampConnect("test")
#' @export
mampConnect <- function(dbname) {
  requireNamespace("RMySQL", quietly = TRUE)
  con <- RMySQL::dbConnect(RMySQL::MySQL()
                           , username="root"
                           , host="localhost"
                           , dbname=dbname
                           , password="root"
                           , unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock")
  return(con)
}

