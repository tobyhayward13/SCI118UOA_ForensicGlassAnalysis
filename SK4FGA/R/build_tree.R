#' Plot S3method for objects of type "sk_partition_tree".
#'
#' S3method for plotting the resulting tree formed by the partitioning algorithms in the SK4FGA package.
#'
#' @param x Output from the function "partition()"
#' @param ... Extra details for the plot. Unused.
#'
#' @return Plot of the decision tree that is formed by the sk_partition_tree object returned by partition and partition.multi.
#' @export plot.sk_partition_tree
#'
#' @examples
#'
#' data = generate_indices()
#' part = partition(data)
#' plot.sk_partition_tree(part)
#'
#' data(glass)
#' data.multi = prepare_data(glass, 1)[1:3]
#' part = partition.multi(data.multi)
#' plot(part)
#'
plot.sk_partition_tree <- function(x, ...){
  # Calculate positions of the groups by iterating through tree object and recording the level and position with respect to parent.

  # Change x to part for the sake of passing CRAN checks.
  part = x

  # Initialise x and y coordinates corresponding to the vertices of the tree.
  x = numeric()
  y = numeric()
  # Initialise x and y coordinates corresponding to the vertices of the parent node in the tree.
  x.parent = numeric()
  y.parent = numeric()
  # Initialise contents of such a vertex
  contents = character()

  # Initialise a stack object
  a.stack = vector('list')
  # Append initial partition to stack and it's level (1)
  a.stack = append(a.stack, list(part$tree))

  # Stack like x's and y's for the current node. Initialise root at (0, 0)
  x.temp = 0
  y.temp = 0
  # Include Parent
  x.parent = append(x.parent, 0)
  y.parent = append(y.parent, 0)

  while (length(a.stack) > 0) {
    # Pop top of stack
    current = a.stack[[1]]
    a.stack = a.stack[-1]
    current.x = x.temp[1]
    x.temp = x.temp[-1]
    current.y = y.temp[1]
    y.temp= y.temp[-1]

    # Record position of node and contents (if it has children)
    x = append(x, current.x)
    y = append(y, current.y)

    if (has.children(current)) contents = append(contents, '')
    else contents = append(contents, paste(current$ix, collapse = ' '))

    if (has.children(current)){
      # Record parent
      x.parent = append(x.parent, rep(current.x, 2))
      y.parent = append(y.parent, rep(current.y, 2))

      a.stack = append(a.stack, list(current[[1]]))
      x.temp = append(x.temp, current.x + 1/2^current.y)
      y.temp = append(y.temp, current.y + 1)

      a.stack = append(a.stack, list(current[[2]]))
      x.temp = append(x.temp, current.x - 1/2^current.y)
      y.temp = append(y.temp, current.y + 1)
      next
    }


  }


  # Invert Y
  y = -y
  y.parent = -y.parent


  # Plot the tree
  plot.new()
  plot.window(xlim = range(x),
              ylim = range(y) - c(0.1, 0))
  points(x, y)
  text(x, y-0.1, contents)

  # Draw segments
  segments(x, y, x.parent, y.parent)

  # Title
  title(paste(attr(part, 'alg'), 'Algorithm Splitting Tree'))

}


