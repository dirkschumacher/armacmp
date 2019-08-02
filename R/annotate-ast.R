#' @include ast-classes.R
annotate_ast <- function(ast, symbol_bound_to_arma_type = function(x) TRUE) {

  # TODO: a lot of small rules are here. Good candidate to further improve
  annotate_ast_rec <- function(current_sexp, scope, parent) {
    if (length(current_sexp) <= 1L) {
      if (is.name(current_sexp) && !is.null(scope)) {
        if (scope$is_name_defined(current_sexp)) {
          return(scope$get_node_by_name(current_sexp))
        }
      }
      node <- ast_node_terminal$new(sexp = current_sexp, head = current_sexp)
      node$set_parent(parent)
      node$set_scope(scope)
      # false positives might occur
      if (is.numeric(current_sexp) ||
        is.name(current_sexp) && !symbol_bound_to_arma_type(current_sexp)) {
        node$set_cpp_type("auto")
      }
      return(node)
    }

    first_element <- current_sexp[[1L]]
    is_namespaced_function <- is.call(first_element)
    if (is_namespaced_function) {
      # is namespaced function
      first_element_chr <- deparse(first_element)
    } else {
      first_element_chr <- as.character(first_element)
    }
    current_node <- new_node_by_chr(first_element_chr)$new(sexp = current_sexp, head = first_element)
    current_node$set_scope(scope)
    current_node$set_parent(parent)
    is_block <- first_element_chr == "{"
    if (is_block) {
      scope <- current_node
    }
    is_assigment <- first_element_chr == "<-"
    tail_elements <- lapply(seq_along(current_sexp)[-1L], function(i) {
      annotate_ast_rec(current_sexp[[i]], scope, current_node)
    })
    # for assignments we really want to check rhs
    # and then also assign cpp type to LHS as well as expression
    # for later symbol lookup
    if (is_assigment) {
      tail_elements[[1L]]$set_cpp_type(tail_elements[[2L]]$get_cpp_type())
      current_node$set_cpp_type(tail_elements[[2L]]$get_cpp_type())
      # can be NULL but only if it should throw an error. Fix later
      if (!scope$is_name_defined(tail_elements[[1L]]$get_sexp())) {
        current_node$set_initial_assignment(TRUE)
      }
      scope$register_new_name(tail_elements[[1L]])
    } else {
      all_auto <- all(vapply(tail_elements, function(x) x$has_auto_cpp_type(), logical(1L)))
      if (all_auto) {
        current_node$set_cpp_type("auto")
      }
    }

    current_node$set_tail_elements(tail_elements)
    current_node
  }
  tree <- annotate_ast_rec(ast, NULL, NULL)

  # annotated_ast might not have brackets if the inital function
  # was a one liner.
  # we then add a new parent bracket node
  if (!("ast_node_block" %in% class(tree))) {
    new_root <- ast_node_block$new(sexp = `{`, head = `{`)
    new_root$set_tail_elements(list(tree))
    tree$set_scope(new_root)
    tree$set_parent(new_root)
    tree <- new_root
  }
  tree
}
