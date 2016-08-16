#' Open a file in the Operating System
#'
#' Open a file in the operating system, using it's default program.
#' \code{sys_open} should work on Ubuntu (and other Linux variants), OSX and
#' Windows.
#'
#' @param f The path of the file to open
#' @return Nothing. Used for it's side effect.
#'
#' @author Copied directory from Brendan Rocks' personal package,
#'  \code{devtools::install("brendan-r/brocks")}. His citations follow:
#'  Based very heavily on the function \code{openFileInOS} from the
#'  package \code{pander} (v0.5.2), written by Gergely Daroczi
#'  (\email{daroczig@@rapporter.net}), itself based on the \code{convert}
#'  function in the package \code{ascii}, written by David Hajage
#'  (\email{dhajage@@gmail.com}).
#'
#' @export
sys_open <- function (f){
  if (missing(f))
    stop("No file to open!")
  f <- normalizePath(f)
  if (!file.exists(f))
    stop("File not found!")
  if (grepl("w|W", .Platform$OS.type)) {
    shell.exec(f)
  }
  else {
    if (grepl("darwin", version$os)) {
      system(paste(shQuote("open"), shQuote(f)), wait = FALSE,
             ignore.stderr = TRUE)
    } else {
      system(paste(shQuote("/usr/bin/xdg-open"), shQuote(f)),
             wait = FALSE, ignore.stdout = TRUE)
    }
  }
}
