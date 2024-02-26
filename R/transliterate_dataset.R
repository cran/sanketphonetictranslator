#' sanketphonetictranslator
#'
#' @title sanketphonetictranslator
#'
#' @description  Facilitate phonetic transliteration between different languages. With support for both Hindi and English, this package provides a way to convert text between hindi and english dataset. Whether you're working with multilingual data or need to convert dataset for analysis or presentation purposes, it offers a simple and efficient solution. Harness the power of phonetic transliteration in your projects with this versatile package.
#'
#' @usage
#' transliterate_dataset(dataset, direction)
#'
#' @param dataset = The name of dataset to be transliterated.
#' @param direction = The direction of transliteration. Use "hindi2english" to transliterate from Hindi to English, or "english2hindi" to transliterate from English to Hindi.
#'
#' @name transliterate_dataset
#' @docType package
#'
#' @importFrom stringi stri_trans_general
#' @importFrom readr read_csv
#' @importFrom utils data
#' @import Rcpp
#' @import stringr
#' @author  Sanket Gharat
#'
#' @export transliterate_dataset
#'
#' @examples
#' # Load the dataset (assuming it's already loaded or available in your environment)
#' data <- dataset
#'
#' # Transliterate the dataset from Hindi to English
#' translated_data <- transliterate_dataset(data, "hindi2english")
#'
#' # View the first few rows of the translated dataset
#' head(translated_data)
#'
#' @return A data frame with the transliterated text.
#'
#' @seealso
#' \code{\link{dataset}}

transliterate_dataset <- function(dataset, direction) {
  valid_directions <- c("hindi2english", "english2hindi")
  if (!(direction %in% valid_directions)) {
    stop("Invalid direction. Use 'hindi2english' or 'english2hindi'.")
  }


  transliterate_function <- switch(
    direction,
    "hindi2english" = function(text) {
      hindi_code_points <- stri_trans_general(text, "Devanagari-Latin")
      english_text <-
        stri_trans_general(hindi_code_points, "Latin-ASCII")
      return(english_text)
    },
    "english2hindi" = function(text) {
      english_code_points <- stri_trans_general(text, "Latin-Devanagari")
      hindi_text <-
        stri_trans_general(english_code_points, "Any-Devanagari")
      return(hindi_text)
    }
  )


  colnames_transliterated <-
    lapply(colnames(dataset), transliterate_function)


  dataset_transliterated <- lapply(dataset, function(col) {
    if (is.character(col)) {
      return(transliterate_function(col))
    } else {
      return(col)
    }
  })


  dataset_transliterated <- as.data.frame(dataset_transliterated)


  colnames(dataset_transliterated) <- colnames_transliterated

  return(dataset_transliterated)
}
