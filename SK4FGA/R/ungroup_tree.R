has.children <- function(part){
  # Would rather not have this one exported to the package.
  class(part[[1]]) == 'list'
}


ungroup.partition <- function(part){
  # If partition has no children, return partition.


  groups = vector('list')
  groups.levels = numeric()

  # Initialise a stack and a levels object
  a.stack = vector('list')
  levels = numeric()
  # Append initial partition to stack and it's level (1)
  a.stack = append(a.stack, list(part))
  levels = append(levels, 1)

  while (length(a.stack) > 0) {
    # Pop top of stack
    current = a.stack[[1]]
    a.stack = a.stack[-1]
    # Pop off current level
    current.level = levels[1]
    levels = levels[-1]


    if (has.children(current)){
      a.stack = append(a.stack, list(current[[1]]))
      levels = append(levels, current.level + 1)
      a.stack = append(a.stack, list(current[[2]]))
      levels = append(levels, current.level + 1)
      next
    }

    # Otherwise append the group to the groups list.
    groups = append(groups, list(current))
    groups.levels = append(groups.levels, current.level)

  }

  list(
    groups = groups,
    levels = groups.levels
  )
}

