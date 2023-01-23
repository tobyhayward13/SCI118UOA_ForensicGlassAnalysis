#' prepare_data
#'
#' Prepare a data file that is in standard form for partition.multi.
#'
#'
#' @param data Inputted data.frame.
#' @param label Column corresponding to the label wished to be grouped by.
#'
#' @return A list of split data.
#' @export prepare_data
#'
prepare_data <- function(data, label = NA){
  # Will only support data.frame objects.
  if (class(data)[1] != 'data.frame') stop('"data" must be of class "data.frame".
                                           Note: Tibbles must be converted into data.frames before parsing.')

  if (length(label) < 2) if (is.na(label)) {
    warning('No column assigned to be the label. First column is used.')
    label = 1L
  }
  # Convert to numeric if character is given (just easier)
  if (typeof(label) == 'character') label = which(colnames(data) %in% label)
  if (length(label) > 1) {
    items = apply(data[,label], 1, paste, collapse = '.')
    data = cbind(items, data[,-label])
  }
  else items = factor(data[,label])

  split(data[,which(sapply(data, class) == 'numeric')], items)
}
