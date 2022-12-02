#' Prepare a data file that is in standard form.
#'
#' Expects that the first two columns correspond to the item and fragment information, and that the remaining columns are the features.
#'
#' @param data Inputted data.frame.
#'
#' @return A list of split data.
#' @export list
#'
prepare_data <- function(data){
  items = factor(data[,1])
  split(data, items)
}
