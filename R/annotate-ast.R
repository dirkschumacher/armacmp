#' @include ast-classes.R
annotate_ast <- function(ast) {

  # process
  # build AST
  # only terminals have types
  # scope is being set up
  # names do not have types but use the value_map to get the type of the value at first assignment
  # register function parameters in the scope and typemap

  # second pass
  # deduce types from the ground up.
  # at assignment nodes, register the deduced type in the type map if not there already
  # at function nodes, take the return value
  # at call nodes, check if the call is a previously defined lambda, then take that call

  # TODO: a lot of small rules are here. Good candidate to further improve
  # will be fixed once the API converges to something stable
  annotate_ast_rec <- function(current_sexp, scope, parent) {
    # current_sexp can also be of type function
    # so we need to hack (unless I find a better way)
    # this converts the function into something generic including a custom
    # pairlist node (big TODO: for refactoring)
    is_function_call <- is.call(current_sexp) && current_sexp[[1L]] == "function"
    if (is.function(current_sexp) || is_function_call) {
      current_sexp <- function_to_sexp(current_sexp)
    }
    if (length(current_sexp) < 1L) {#empty element in pairlist
      return(ast_node_empty$new(current_sexp, current_sexp))
    }
    if (length(current_sexp) == 1L) {
      class_type <- ast_node_terminal
      if (is.call(current_sexp)) {
        if (current_sexp == "next") {
          class_type <- ast_node_next
        } else if (current_sexp == "break") {
          class_type <- ast_node_break
        } else if (current_sexp[[1L]] == "arma_pairlist") {
          class_type <- ast_node_arma_pairlist
        } else {
          class_type <- ast_node_function_call
        }
      }

      if (is.name(current_sexp)) {
        class_type <- ast_node_name
      }
      node <- class_type$new(sexp = current_sexp, head = current_sexp)
      node$set_parent(parent)
      node$set_scope(scope)

      if (is.numeric(current_sexp)) {
        node$set_cpp_type("auto")
      }
      return(node)
    }

    first_element <- current_sexp[[1L]]
    first_element_chr <- as.character(first_element)
    current_node_class <- new_node_by_chr(first_element_chr,
                                    is_call = is.call(current_sexp))
    current_node <- current_node_class$new(sexp = current_sexp, head = first_element)
    current_node$set_scope(scope)
    current_node$set_parent(parent)

    is_block <- first_element_chr == "{"
    if (is_block) {
      scope <- current_node
    }

    tail_elements <- lapply(seq_along(current_sexp)[-1L], function(i) {
      annotate_ast_rec(current_sexp[[i]], scope, current_node)
    })
    current_node$set_tail_elements(tail_elements)

    is_function <- "ast_node_function" %in% class(current_node)
    if (is_function) {
      current_node$ensure_has_block()
      current_node$register_input_variables_in_child_scope()
    }

    is_pairlist <- "ast_node_arma_pairlist" %in% class(current_node)
    if (is_pairlist) {
      current_node$update_parameter_types()
    }

    # for assignments we really want to check rhs
    # and then also assign cpp type to LHS as well as expression
    # for later symbol lookup
    is_assigment <- first_element_chr == "<-"
    lhs_is_name <- "ast_node_name" %in% class(tail_elements[[1L]])
    if (is_assigment && lhs_is_name) {
      current_node$check_and_register_assignment()
    }
    current_node
  }
  tree <- annotate_ast_rec(ast, NULL, NULL)
  deduce_types_rec <- function(current_node) {
    tail_elements <- current_node$get_tail_elements()
    is_assignment <- "ast_node_assignment" %in% class(current_node)
    is_function <- "ast_node_function" %in% class(current_node)
    is_function_call <- "ast_node_function_call" %in% class(current_node)
    is_return <- "ast_node_return" %in% class(current_node)
    is_name <- "ast_node_name" %in% class(current_node)

    # first deduce type of children
    for (el in tail_elements) {
      deduce_types_rec(el)
    }

    if (is_function) {
      current_node$update_return_type()
    }
    if (is_return) {
      current_node$update_return_type()
    }
    if (is_function_call) {
      current_node$update_return_type()
    }

    all_auto <- all(vapply(tail_elements, function(x) x$has_auto_cpp_type(), logical(1L)))
    if (length(tail_elements) > 0L && all_auto && !is_assignment && !is_function && !is_name && !is_function_call) {
      current_node$set_cpp_type("auto")
    }
  }

  deduce_types_rec(tree)

  tree
}

function_to_sexp <- function(sexp) {
  is_function_call <- is.call(sexp) && sexp[[1L]] == "function"
  sexp <- c(
    if (!is_function_call) list(as.name("function")),
    as.list(sexp)
  )
  if (is_function_call) {
    # if it is a function call then the last element is a srcref
    sexp <- sexp[-length(sexp)]
  }

  # problem the pairlist has been flattend and we need to revert that
  if (length(sexp) > 2L) {
    # we also create a new internal sexp just for a pairlist
    pairlist_sexp <- if (is_function_call) {
      c(
        list(as.name("arma_pairlist")),
        as.list(sexp[[2L]])
      )
    } else {
      c(
        list(as.name("arma_pairlist")),
        as.pairlist(sexp[2:(length(sexp) - 1L)])
      )
    }
    pairlist_sexp <- as.call(pairlist_sexp)
    sexp <- list(
      sexp[[1L]],
      pairlist_sexp,
      sexp[[length(sexp)]]
    )
  } else {
    sexp <- list(
      sexp[[1L]],
      as.call(list(as.name("arma_pairlist"))),
      sexp[[length(sexp)]]
    )
  }
  sexp
}
