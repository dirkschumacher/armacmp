#' @include ast-classes.R
annotate_ast <- function(ast) {

  # process
  # build AST
  # only terminals have types
  # scope is being set up
  # names do not have types but use the value_map to get the type of the value at first assignment
  # register function parameters in the scope and typemap

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
    if (length(current_sexp) < 1L) { # empty element in pairlist
      return(ast_node_empty$new(current_sexp, current_sexp))
    }

    if (length(current_sexp) == 1L) {
      class_type <- ast_node_terminal
      if (is.call(current_sexp)) {

        # TODO: generalize
        if (current_sexp == "next") {
          class_type <- ast_node_next
        } else if (current_sexp == "break") {
          class_type <- ast_node_break
        } else if (current_sexp[[1L]] == "arma_pairlist") {
          class_type <- ast_node_arma_pairlist
        } else {
          class_type <- new_node_by_chr(
            paste0(deparse(current_sexp[[1L]]), collapse = ""),
            is_call = TRUE
          )
        }
      }

      if (is.name(current_sexp)) {
        if (current_sexp == "pi") {
          class_type <- ast_node_pi
        } else {
          class_type <- ast_node_name
        }
      }

      if (is.logical(current_sexp)) {
        class_type <- if (current_sexp) {
          ast_node_true
        } else {
          ast_node_false
        }
      }

      if (is.numeric(current_sexp)) {
        class_type <- if (is.integer(current_sexp)) {
          ast_node_scalar_int
        } else {
          ast_node_scalar_double
        }
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
      is_call = is.call(current_sexp)
    )
    current_node <- current_node_class$new(sexp = current_sexp, head = first_element)
    current_node$set_scope(scope)
    current_node$set_parent(parent)

    is_block <- first_element_chr == "{"
    if (is_block) {
      scope <- current_node
    }


    # TODO: create a function for all these %in% class calls
    is_function <- "ast_node_function" %in% class(current_node)
    is_for_loop <- "ast_node_for" %in% class(current_node)
    is_while_loop <- "ast_node_while" %in% class(current_node)
    is_if <- "ast_node_if" %in% class(current_node)

    if (is_function || is_while_loop || is_if) {
      current_sexp <- ensure_has_block(current_sexp, 3L)
      current_node$set_sexp(current_sexp)
    }

    if (is_for_loop) {
      current_sexp <- ensure_has_block(current_sexp, 4L)
      current_node$set_sexp(current_sexp)
    }
    tail_elements <- lapply(seq_along(current_sexp)[-1L], function(i) {
      annotate_ast_rec(current_sexp[[i]], scope, current_node)
    })
    current_node$set_tail_elements(tail_elements)

    if (is_function) {
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

  scope <- ast_node_global_scope$new(ast)
  parent <- scope

  tree <- annotate_ast_rec(ast, scope, parent)

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

ensure_has_block <- function(sexp, index_el_to_be_wrapped) {
  body <- sexp[[index_el_to_be_wrapped]]
  is_block <- all("{" %in% as.character(body[[1L]]))
  if (!is_block) {
    sexp_block <- bquote({
      .(body)
    })
    sexp[[index_el_to_be_wrapped]] <- sexp_block
  }
  sexp
}
