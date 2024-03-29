#' has.children
#'
#' For internal use only.
#' Determines if a node in the Partition tree has a child.
#'
#' @param part Node in partition Tree.
#'
#' @return Logical determining if the node has any children.
#'
has.children <- function(part){
  # Would rather not have this one exported to the package.
  class(part[[1]]) == 'list'
}

