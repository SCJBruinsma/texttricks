#' A .pdf extract Function
#'
#' This function allows you to convert a .pdf file to a .txt file
#' @param filename Filename of the .pdf file
#' @keywords .pdf
#' @export
#' @examples
#' extract_pdf()

extract_pdf <- function(filename) {
  require(pdftools)
  print(filename)
  try({
    text <- pdf_text(filename)
  })
  title <- gsub("(.*)/([^/]*).pdf", "\\2", filename)
  txt_directory <- getwd()
  write(text, file.path(txt_directory, paste0(title, ".txt")))
}
