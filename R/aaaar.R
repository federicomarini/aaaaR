#' aaaar
#'
#' Transform your input string character into a string composed by aminoacids
#'
#' @param mystring A character string
#'
#' @return A character string where the letters composing your string have
#' been replaced by aminoacids - where possible
#' @export
#'
#' @examples
#' aaaar("damn")
aaaar <- function(mystring = "damn") {
  # it has to be a string
  stopifnot(is.character(mystring))
  # transform everything to upper
  mystring <- toupper(mystring)
  # check whether you can say this with aminoacids only
  myletters <- unlist(strsplit(mystring,split=""))
  if(all(myletters %in% c(aatable$oneletter," "))) {
    message("Yep, can say that")
    myaas <- aatable$Aminoacid[match(myletters,aatable$oneletter)]
    aawords <- paste(myaas, sep=" ",collapse = ", ")
  }
  else {
    message("Nope, I can't say the full word")
    myaas <- myletters
    myaas[myletters %in% aatable$oneletter] <-
      aatable$Aminoacid[match(myletters[myletters %in% aatable$oneletter],aatable$oneletter)]
    aawords <- paste(myaas, sep=" ",collapse = ", ")
  }
  return(aawords)
}
