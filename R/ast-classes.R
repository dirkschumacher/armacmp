utils::globalVariables(c("private", "self"))
ast_node <- R6::R6Class("ast_node",
  public = list(
    initialize = function(sexp, head, tail_elements = NULL) {
      private$sexp <- sexp
      self$set_head(head)
      self$set_tail_elements(tail_elements)
    },
    set_head = function(node) {
      private$head <- node
    },
    set_scope = function(node) {
      private$scope <- node
    },
    has_scope = function() {
      !is.null(self$get_scope())
    },
    set_parent = function(node) {
      private$parent <- node
    },
    get_parent = function() {
      private$parent
    },
    get_head = function() {
      private$head
    },
    get_tail_elements = function() {
      private$tail_elements
    },
    set_tail_elements = function(nodes) {
      private$tail_elements <- nodes
    },
    get_scope = function() {
      private$scope
    },
    get_sexp = function() {
      private$sexp
    },
    set_sexp = function(sexp) {
      private$sexp <- sexp
    },
    get_cpp_type = function() {
      private$cpp_type
    },
    is_armadillo_cpp_type = function() {
      grepl("^arma", self$get_cpp_type())
    },
    is_scalar_cpp_type = function() {
      !self$is_armadillo_type() && !self$is_auto_type()
    },
    is_auto_cpp_type = function() {
      self$has_auto_cpp_type()
    },
    # TODO: only one function
    has_auto_cpp_type = function() {
      self$get_cpp_type() == "auto"
    },
    set_cpp_type = function(type) {
      private$cpp_type <- type
    },
    emit = function(...) {
      do.call(paste0, list(...))
    },
    compile = function() {
      stop(error_msg_unkown_expr(self$get_sexp()), call. = FALSE)
    },
    find_all_returns = function() {
      private$find_nodes("ast_node_return", stop_at = "ast_node_function")
    },
    find_all_names = function() {
      private$find_nodes("ast_node_name")
    }
  ),
  private = list(
    sexp = NULL,
    head = NULL,
    tail_elements = list(),
    scope = NULL,
    parent = NULL,
    cpp_type = "auto",
    find_nodes = function(node_type, stop_at = NULL) {
      find_elements_rec <- function(node) {
        if (!is.null(stop_at) && any(stop_at %in% class(node))) {
          return(list())
        }
        found_node <- list()
        if (any(node_type %in% class(node))) {
          found_node <- list(node)
        }
        c(found_node, lapply(node$get_tail_elements(), find_elements_rec))
      }
      unlist(find_elements_rec(self))
    }
  )
)

error_msg_unkown_expr <- function(sexp) {
  paste0(
    "Sorry, but the expression:\n\n",
    paste0(deparse(sexp), collapse = "\n"),
    "\n\ncannot be translated into a C++ construct.\n",
    "Not all R functions are supported."
  )
}

ast_node_arma_pairlist <- R6::R6Class(
  classname = "ast_node_arma_pairlist",
  inherit = ast_node,
  public = list(
    compile = function(fun_name = NULL, overwrite_return_type = NULL) {
      fun_args <- self$get_parameter_types()
      input_params <- generate_cpp_input_parameters_code(
        fun_args,
        is_reassigned = function(param_name) {
          self$get_scope()$count_assignments(param_name) > 0L
        }
      )
      self$emit(
        "(", paste0(input_params, collapse = ", "), ")"
      )
    },
    get_parameter_types = function() {
      fun_args <- self$get_tail_elements()
      names <- vapply(fun_args, function(x) x$get_name(), character(1L))
      setNames(lapply(fun_args, function(x) {
        x$get_cpp_type()
      }), names)
    },
    # needs to be done ones
    update_parameter_types = function() {
      tail <- self$get_tail_elements()
      var_names <- self$get_parameter_names()
      for (i in seq_along(tail)) {
        param <- tail[[i]]
        var_name <- as.name(var_names[[i]])
        stopifnot(any(c("ast_node_function_call","ast_node_type_spec", "ast_node_terminal") %in% class(param)))
        x <- param$get_sexp()
        type <- if (!missing(x) && "ast_node_type_spec" %in% class(param)) {
          param$get_cpp_type()
        } else {
          ast_node_type_matrix$new(as.name("type_matrix"), as.name("type_matrix"))$get_cpp_type()
        }
        new_param <- ast_node_name$new(var_name, var_name)
        new_param$set_cpp_type(type)
        new_param$set_parent(self)
        new_param$set_scope(self$get_scope())
        tail[[i]] <- new_param
      }
      self$set_tail_elements(tail)
    },
    get_parameter_names = function() {
      names(self$get_sexp()[-1L])
    }
  )
)

ast_node_function <- R6::R6Class(
  classname = "ast_node_function",
  inherit = ast_node,
  public = list(
    register_input_variables_in_child_scope = function() {
      arguments <- self$get_function_parameters()
      # also set the function body as the scope of the function parameters
      body <- self$get_function_body()
      arguments$set_scope(body)
      parameters <- arguments$get_tail_elements()
      stopifnot("ast_node_block" %in% class(body))
      for (i in seq_along(parameters)) {
        param <- parameters[[i]]
        stopifnot("ast_node_name" %in% class(param))
        dummy_value <- ast_node_dummy$new()
        dummy_value$set_cpp_type(param$get_cpp_type())
        dummy_value$set_parent(self)
        dummy_value$set_scope(body)
        body$register_new_name(param$get_name(), dummy_value)
        # Also we need to change the scope of the input variables to the body
        # as we register the names there
        # TODO: revisit that
        param$set_scope(body)
      }
    },
    is_recursive = function(assigned_name) {
      # check if the assigned name is used as a function call in the function
      length(private$find_all_function_calls(of = assigned_name)) > 0L
    },
    get_cpp_function_type = function() {
      return_type <- self$get_cpp_type()
      if (return_type == "auto") {
        stop("Recursive functions must have an explicit return type.", call. = FALSE)
      }
      argument_types <- self$get_function_parameters()$get_parameter_types()
      # TODO: the following line should be handled by the pairlist, not here
      argument_types <- generate_cpp_input_types(argument_types)
      paste0(
        "std::function<", return_type,
        "(",
        paste0(argument_types, collapse = ", "),
        ")>"
      )
    },
    get_function_body = function() {
      self$get_tail_elements()[[2L]]
    },
    get_function_parameters = function() {
      self$get_tail_elements()[[1L]]
    },
    get_cpp_type = function() {
      body <- self$get_function_body()
      return_types <- body$find_all_returns()
      return_types <- lapply(return_types, function(x) x$get_cpp_type())
      return_type <- if (length(return_types) == 0L) {
        "void"
      } else {
        if (length(unique(return_types)) != 1L) {
          stop("There seem to be multiple possible return types in your function. ",
            "Consider adding type annotations to all `return` statements.",
            "\n\n",
            paste0(deparse(body$get_sexp()), collapse = "\n"),
            call. = FALSE
          )
        }
        return_types[[1L]]
      }
      return_type
    },
    compile = function(fun_name = NULL, overwrite_return_type = NULL, is_recursive = FALSE) {
      stopifnot(length(self$get_tail_elements()) == 2L)
      arguments <- self$get_function_parameters()
      body <- self$get_function_body()
      is_lambda <- is.null(fun_name)
      # first compile arguments and body
      # currently the compilation can alter the scope
      body_compiled <- body$compile()
      args_compiled <- arguments$compile()

      return_type <- if (!is.null(overwrite_return_type)) {
        overwrite_return_type
      } else {
        self$get_cpp_type()
      }
      if (is_lambda) {
        function_prefix_code <- "[&]"
        function_suffix_code <- if (grepl("^arma", return_type) || is_recursive) {
          paste0(" mutable -> ", return_type)
        } else {
          " mutable"
        }
      } else {
        function_prefix_code <- paste0(return_type, " ", fun_name)
        function_suffix_code <- ""
      }
      self$emit(
        function_prefix_code,
        args_compiled,
        function_suffix_code,
        "\n",
        body_compiled
      )
    }
  ),
  private = list(
    find_all_function_calls = function(of) {
      # TODO: implement a get_name or something for function_calls
      Filter(function(x) {
        paste0(deparse(x$get_head()), collapse = "") == of
      }, super$find_nodes("ast_node_function_call"))
    }
  )
)

ast_node_function_call <- R6::R6Class(
  classname = "ast_node_function_call",
  inherit = ast_node,
  public = list(
    get_name = function() {
      paste0(deparse(self$get_sexp()[[1L]]), collapse = "")
    },
    compile = function() {
      val <- self$get_name()
      if (self$has_scope()) {
        if (!self$get_scope()$is_name_defined(val)) {
          # this is an unknown function call
          stop(error_msg_unkown_expr(self$get_sexp()), call. = FALSE)
        }
      }
      arguments <- vapply(self$get_tail_elements(), function(x) {
        x$compile()
      }, character(1L))
      suffix <- if ("ast_node_block" %in% class(self$get_parent())) {
        ";\n"
      } else {
        ""
      }
      self$emit(
        as.character(val),
        "(",
        paste0(arguments, collapse = ", "),
        ")",
        suffix
      )
    },
    get_cpp_type = function() {
      if (self$has_scope() && is.call(self$get_sexp())) {
        scope <- self$get_scope()
        fun_name <- self$get_sexp()[[1L]]
        if (scope$is_name_defined(fun_name)) {
          fun <- scope$get_value_node_for_name(fun_name)
          return(fun$get_cpp_type())
        }
      }
      private$cpp_type
    }
  )
)

ast_node_terminal <- R6::R6Class(
  classname = "ast_node_terminal",
  inherit = ast_node,
  public = list(
    compile = function() {
      val <- self$get_head()
      # val %% 1 == 0, TRUE if val is integer like otherwise FALSE
      # see https://stackoverflow.com/a/3477158/2798441
      # TODO: move to ast_node_scalar
      if (is.numeric(val) && !is.integer(val) && val %% 1 == 0) {
        self$emit(val, ".0")
      } else {
        self$emit(as.character(val))
      }
    }
  )
)

ast_node_scalar <- R6::R6Class(
  classname = "ast_node_scalar",
  inherit = ast_node_terminal
)

ast_node_scalar_double <- R6::R6Class(
  classname = "ast_node_scalar",
  inherit = ast_node_scalar,
  public = list(
    get_cpp_type = function() {
      "double"
    }
  )
)

ast_node_scalar_int <- R6::R6Class(
  classname = "ast_node_int",
  inherit = ast_node_scalar,
  public = list(
    get_cpp_type = function() {
      "int"
    }
  )
)

ast_node_scalar_bool <- R6::R6Class(
  classname = "ast_node_bool",
  inherit = ast_node_scalar,
  public = list(
    get_cpp_type = function() {
      "bool"
    }
  )
)

ast_node_pi <- R6::R6Class(
  classname = "ast_node_pi",
  inherit = ast_node_scalar_double,
  public = list(
    compile = function() {
      self$emit("arma::datum::pi")
    }
  )
)

ast_node_true <- R6::R6Class(
  classname = "ast_node_true",
  inherit = ast_node_scalar_bool,
  public = list(
    compile = function() {
      self$emit("true")
    }
  )
)

ast_node_false <- R6::R6Class(
  classname = "ast_node_false",
  inherit = ast_node_scalar_bool,
  public = list(
    compile = function() {
      self$emit("false")
    }
  )
)

ast_node_name <- R6::R6Class(
  classname = "ast_node_name",
  inherit = ast_node_terminal,
  public = list(
    get_cpp_type = function() {
      scope <- self$get_scope()
      if (self$has_scope() && scope$is_name_defined(self$get_name())) {
        value_node <- scope$get_value_node_for_name(self$get_name())
        # TODO: check for self reference
        return(value_node$get_cpp_type())
      }
      private$cpp_type
    },
    get_name = function() {
      paste0(deparse(self$get_head()), collapse = "")
    }
  )
)

ast_node_empty <- R6::R6Class(
  classname = "ast_node_empty",
  inherit = ast_node_terminal
)

# a dummy node is just used to store a type
ast_node_dummy <- R6::R6Class(
  classname = "ast_node_dummy",
  inherit = ast_node_terminal,
  public = list(
    initialize = function() {
      super$initialize(quote(`dummy`), quote(`dummy`))
    }
  )
)

ast_node_assignment <- R6::R6Class(
  classname = "ast_node_assignment",
  inherit = ast_node,
  public = list(
    compile = function() {
      operands <- self$get_tail_elements()
      stopifnot(length(operands) == 2L)
      lhs_compiled <- operands[[1L]]$compile()

      lhs_is_name <- "ast_node_name" %in% class(operands[[1L]])
      is_initial_assignment <- self$is_initial_assignment()
      if (is_initial_assignment && lhs_is_name) {
        private$register_assignment()
      }

      # check if assignment is a QR decomposition
      rhs <- operands[[2L]]
      if (any(c("ast_node_qr_init", "ast_node_svd_init") %in% class(rhs))) {
        return(rhs$compile(lhs_compiled))
      }


      lhs_is_call <- "ast_node_function_call" %in% class(operands[[1L]]) ||
        "ast_node_element_access" %in% class(operands[[1L]])
      type <- if (is_initial_assignment && !lhs_is_call) {
        type <- operands[[2L]]$get_cpp_type()
        # we use auto for anything non armadillo
        type <- auto_or_arma_type(type)
        # also let the name point to this inital expression
        # operands[[1L]]$set_value_node(operands[[2L]])
        paste0(type, " ")
      } else {
        ""
      }

      # lambda
      is_lambda <- "ast_node_function" %in% class(rhs)
      is_lambda_recursive <- FALSE
      if (is_lambda) {
        if (rhs$is_recursive(lhs_compiled)) {
          is_lambda_recursive <- TRUE
          type <- paste0(rhs$get_cpp_function_type(), " ")
        } else {
          type <- paste0("auto", " ")
        }
      }

      # something wrong here
      if (is_lambda && is_lambda_recursive) {
        self$emit(
          type, lhs_compiled, ";\n",
          lhs_compiled, " = ",
          operands[[2L]]$compile(is_recursive = TRUE),
          ";"
        )
      } else {
        self$emit(
          type,
          lhs_compiled,
          " = ",
          operands[[2L]]$compile(),
          ";"
        )
      }
    },
    check_and_register_assignment = function() {
      tail_elements <- self$get_tail_elements()
      stopifnot(length(tail_elements) == 2L)
      tail_elements[[1L]]$set_cpp_type(tail_elements[[2L]]$get_cpp_type())
      self$set_cpp_type(tail_elements[[2L]]$get_cpp_type())
    },
    is_initial_assignment = function() {
      var_name <- paste0(deparse(self$get_tail_elements()[[1L]]$get_sexp()), collapse = "")
      if (self$has_scope()) {
        if (self$get_scope()$is_name_defined(var_name)) {
          return(FALSE)
        }
        if (self$get_scope()$has_scope()) {
          # maybe it was previously already defined
          already_defined <- self$get_scope()$get_scope()$is_name_defined(var_name)
          if (already_defined) {
            return(FALSE)
          }
        }
      }
      TRUE
    }
  ),
  private = list(
    register_assignment = function() {
      if (self$has_scope()) {
        var_name <- self$get_tail_elements()[[1L]]$get_name()
        self$get_scope()$register_new_name(var_name, self$get_tail_elements()[[2L]])
      }
    }
  )
)

ast_node_return <- R6::R6Class(
  classname = "ast_node_return",
  inherit = ast_node,
  public = list(
    compile = function() {
      operands <- self$get_tail_elements()
      stopifnot(length(operands) %in% c(1L, 2L))
      self$emit(
        "return ",
        operands[[1L]]$compile(),
        ";"
      )
    },
    get_cpp_type = function() {
      operands <- self$get_tail_elements()
      if (length(operands) == 2L) {
        return(operands[[2]]$get_cpp_type())
      }
      operands[[1L]]$get_cpp_type()
    }
  )
)

make_binary_operator_class <- function(name, op) {
  R6::R6Class(
    classname = name,
    inherit = ast_node,
    public = list(
      compile = function() {
        operands <- self$get_tail_elements()
        stopifnot(length(operands) %in% c(1:2))
        if (length(operands) == 1L) {
          self$emit(op, operands[[1L]]$compile())
        } else {
          self$emit(
            operands[[1L]]$compile(),
            " ", op, " ",
            operands[[2L]]$compile()
          )
        }
      },
      get_cpp_type = function() {
        operands <- self$get_tail_elements()
        arma_type <- if (operands[[1L]]$is_armadillo_cpp_type()) {
          operands[[1L]]
        } else if (length(operands) == 2L && operands[[2L]]$is_armadillo_cpp_type()) {
          operands[[2L]]
        }
        if (!is.null(arma_type)) {
          if (op %in% c("*", "/", "+", "-")) {
            # TODO: HEURISTIC: we return the type of the first armadillo type
            return(arma_type$get_cpp_type())
          }
          if (op %in% c("<=", ">=", "<", ">", "==")) {
            # it is an arma::umat
            # TODO: can we figure out if rowvec/colvec?
            return("arma::umat")
          }
        }
        private$cpp_type
      }
    )
  )
}
ast_node_plus <- make_binary_operator_class("ast_node_plus", "+")
ast_node_minus <- make_binary_operator_class("ast_node_minus", "-")
ast_node_div <- make_binary_operator_class("ast_node_div", "/")
ast_node_greater <- make_binary_operator_class("ast_node_greater", ">")
ast_node_less <- make_binary_operator_class("ast_node_less", "<")
ast_node_leq <- make_binary_operator_class("ast_node_leq", "<=")
ast_node_geq <- make_binary_operator_class("ast_node_geq", ">=")
ast_node_equal <- make_binary_operator_class("ast_node_equal", "==")
ast_node_nequal <- make_binary_operator_class("ast_node_nequal", "!=")
ast_node_logical_and <- make_binary_operator_class("ast_node_logical_and", "&&")
ast_node_logical_or <- make_binary_operator_class("ast_node_logical_or", "||")

ast_node_block <- R6::R6Class(
  classname = "ast_node_block",
  inherit = ast_node,
  public = list(
    initialize = function(...) {
      super$initialize(...)
      private$value_map <- new.env(parent = emptyenv())
    },
    compile = function() {
      self$emit(
        "{\n",
        paste0(lapply(self$get_tail_elements(), function(x) x$compile()), collapse = "\n"),
        "\n}\n"
      )
    },
    register_new_name = function(name_chr, value_node) {
      # no reassignments, first one counts
      name_still_free <- !self$has_scope() || !self$get_scope()$is_name_defined(name_chr)
      if (is.null(private$value_map[[name_chr]]) && name_still_free) {
        private$value_map[[name_chr]] <- value_node
      }
    },
    new_random_variable_id = function() {
      prefix <- if (self$has_scope()) {
        self$get_scope()$new_random_variable_id()
      } else {
        ""
      }
      var_id <- paste0(prefix, private$rand_variable_name_count)
      private$rand_variable_name_count <- private$rand_variable_name_count + 1L
      var_id
    },
    is_name_defined = function(name) {
      if (nchar(name) == 0L) {
        return(FALSE)
      }
      !is.null(private$value_map[[as.character(name)]]) ||
        (!is.null(self$get_scope()) && self$get_scope()$is_name_defined(name))
    },
    get_value_node_for_name = function(name) {
      el <- private$value_map[[as.character(name)]]
      if (is.null(el) && self$has_scope()) {
        self$get_scope()$get_value_node_for_name(name)
      } else {
        el
      }
    },
    find_all_parent_names = function(name) {
      unique(c(
        names(private$name_store),
        if (self$has_scope()) {
          self$get_scope()$find_all_parent_names()
        }
      ))
    },
    count_assignments = function(of_name) {
      assignments <- private$find_nodes("ast_node_assignment")
      assignments <- Filter(function(assignment) {
        elements <- assignment$get_tail_elements()
        lhs <- elements[[1L]]
        "ast_node_name" %in% class(lhs) && lhs$get_name() == of_name ||
          (
            "ast_node_element_access" %in% class(lhs) &&
              "ast_node_name" %in% class(lhs$get_tail_elements()[[1L]]) &&
              lhs$get_tail_elements()[[1L]]$get_name() == of_name
          )
      }, assignments)
      length(assignments)
    }
  ),
  private = list(
    value_map = NULL,
    rand_variable_name_count = 0L
  )
)

ast_node_global_scope <- R6::R6Class(
  classname = "ast_node_global_scope",
  inherit = ast_node_block,
  public = list(
    initialize = function(sexp) {
      super$initialize(sexp, quote(`{`))
    }
  )
)

ast_node_length <- R6::R6Class(
  classname = "ast_node_length",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        self$get_tail_elements()[[1L]]$compile(), ".n_elem"
      )
    },
    get_cpp_type = function() {
      "int"
    }
  )
)

ast_node_ncol <- R6::R6Class(
  classname = "ast_node_ncol",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        self$get_tail_elements()[[1L]]$compile(), ".n_cols"
      )
    },
    get_cpp_type = function() {
      "int"
    }
  )
)

ast_node_nrow <- R6::R6Class(
  classname = "ast_node_nrow",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        self$get_tail_elements()[[1L]]$compile(), ".n_rows"
      )
    },
    get_cpp_type = function() {
      "int"
    }
  )
)

ast_node_if <- R6::R6Class(
  classname = "ast_node_if",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) %in% c(2L, 3L))
      elements <- self$get_tail_elements()
      self$emit(
        "\nif ( ", elements[[1L]]$compile(), " ) \n",
        elements[[2L]]$compile(),
        if (length(elements) > 2L && !is.null(elements[[3L]])) {
          paste0(" else ", elements[[3L]]$compile())
        }
      )
    }
  )
)

ast_node_mult <- R6::R6Class(
  classname = "ast_node_mult",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      elements <- self$get_tail_elements()

      op <- private$get_mult_operator()
      self$emit(
        elements[[1L]]$compile(),
        " ",
        op,
        " ",
        elements[[2L]]$compile()
      )
    },
    get_cpp_type = function() {
      elements <- self$get_tail_elements()
      if (private$get_mult_operator() == "%") {
        # TODO: need to figure out types for imat, umat, mat combinations
        return(elements[[1L]]$get_cpp_type())
      } else {
        arma_element <- if (elements[[1L]]$is_armadillo_cpp_type()) {
          elements[[1L]]
        } else if (elements[[2L]]$is_armadillo_cpp_type()) {
          elements[[2L]]
        }
        if (!is.null(arma_element)) {
          return(arma_element$get_cpp_type())
        } else {
          # TODO: figure more combinations
          # # "Otherwise, if either operand is double, the other shall be converted to double."
          #
          # if ("double" %in% c(elements[[2L]]$get_cpp_type(), elements[[2L]]$get_cpp_type())) {
          #   return("double")
          # }
        }
      }
      private$cpp_type
    }
  ),
  private = list(
    get_mult_operator = function() {
      elements <- self$get_tail_elements()
      # only use armadillo element-wise multiplication
      # if both operands are armadillo types
      if (!elements[[1L]]$is_armadillo_cpp_type() ||
        !elements[[2L]]$is_armadillo_cpp_type()) {
        "*"
      } else {
        "%"
      }
    }
  )
)

ast_node_matmul <- R6::R6Class(
  classname = "ast_node_matmul",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      elements <- self$get_tail_elements()
      self$emit(
        elements[[1L]]$compile(),
        " * ",
        elements[[2L]]$compile()
      )
    },
    get_cpp_type = function() {
      arma_type <- find_arma_cpp_types(self$get_tail_elements())
      arma_cpp_types <- vapply(arma_type, function(x) x$get_cpp_type(), character(1L))
      if (length(arma_type) == 2L) {
        is_integer_arma_type <- function(type) {
          grepl("^arma::i", type)
        }
        is_unsigned_integer_arma_type <- function(type) {
          grepl("^arma::u", type)
        }
        is_double_arma_type <- function(type) {
          type %in% paste0("arma::", c("vec", "colvec", "rowvec", "mat"))
        }
        # TODO: revisit these rules and contemplate
        if (any(sapply(arma_cpp_types, is_double_arma_type))) {
          return("arma::mat")
        }
        if (any(sapply(arma_cpp_types, is_integer_arma_type))) {
          return("arma::imat")
        }
        if (all(sapply(arma_cpp_types, is_unsigned_integer_arma_type))) {
          return("arma::umat")
        }
      }
      if (length(arma_type) == 1L) {
        return(arma_type[[1L]]$get_cpp_type())
      }
      "arma::mat"
    }
  )
)

ast_node_pow <- R6::R6Class(
  classname = "ast_node_pow",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      elements <- self$get_tail_elements()
      fun <- if (elements[[1L]]$is_armadillo_cpp_type()) {
        # we assume the second argument is compatible
        "arma::pow"
      } else {
        "std::pow"
      }
      self$emit(
        fun, "(",
        elements[[1L]]$compile(),
        ", ",
        elements[[2L]]$compile(),
        ")"
      )
    },
    get_cpp_type = function() {
      self$get_tail_elements()[[1L]]$get_cpp_type()
    }
  )
)

ast_node_bracket <- R6::R6Class(
  classname = "ast_node_bracket",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        "(",
        self$get_tail_elements()[[1L]]$compile(),
        ")"
      )
    },
    get_cpp_type = function() {
      self$get_tail_elements()[[1L]]$get_cpp_type()
    }
  )
)

ast_node_colsums <- R6::R6Class(
  classname = "ast_node_colsums",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit("arma::sum( ", self$get_tail_elements()[[1L]]$compile(), ", 0 )")
    },
    get_cpp_type = function() {
      input_type <- self$get_tail_elements()[[1L]]$get_cpp_type()
      arma_type <- arma_element_type(input_type)
      if (!is.null(arma_type)) {
        get_arma_type("rowvec", elements = arma_type)
      } else {
        private$cpp_type
      }
    }
  )
)

ast_node_rowsums <- R6::R6Class(
  classname = "ast_node_rowsums",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit("arma::sum( ", self$get_tail_elements()[[1L]]$compile(), ", 1 )")
    },
    get_cpp_type = function() {
      input_type <- self$get_tail_elements()[[1L]]$get_cpp_type()
      arma_type <- arma_element_type(input_type)
      if (!is.null(arma_type)) {
        get_arma_type("colvec", elements = arma_type)
      } else {
        private$cpp_type
      }
    }
  )
)

ast_node_colmeans <- R6::R6Class(
  classname = "ast_node_colmeans",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit("arma::mean( ", self$get_tail_elements()[[1L]]$compile(), ", 0 )")
    },
    get_cpp_type = function() {
      "arma::rowvec"
    }
  )
)

ast_node_rowmeans <- R6::R6Class(
  classname = "ast_node_rowmeans",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit("arma::mean( ", self$get_tail_elements()[[1L]]$compile(), ", 1 )")
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

ast_node_forwardsolve <- R6::R6Class(
  classname = "ast_node_forwardsolve",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      elements <- self$get_tail_elements()
      self$emit(
        "arma::solve(arma::trimatl(",
        elements[[1L]]$compile(), "), ",
        elements[[2L]]$compile(),
        " )"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

ast_node_backsolve <- R6::R6Class(
  classname = "ast_node_backsolve",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      elements <- self$get_tail_elements()
      self$emit(
        "arma::solve(arma::trimatu(",
        elements[[1L]]$compile(), "), ",
        elements[[2L]]$compile(),
        " )"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

ast_node_solve <- R6::R6Class(
  classname = "ast_node_solve",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) %in% 1:2)
      elements <- self$get_tail_elements()
      if (length(elements) == 1L) {
        self$emit("arma::inv(", elements[[1L]]$compile(), ")")
      } else {
        self$emit(
          "arma::solve(",
          elements[[1L]]$compile(),
          ", ",
          elements[[2L]]$compile(),
          ")"
        )
      }
    },
    get_cpp_type = function() {
      if (length(self$get_tail_elements()) == 1L) {
        "arma::mat"
      } else {
        "arma::colvec"
      }
    }
  )
)

ast_node_crossprod <- R6::R6Class(
  classname = "ast_node_crossprod",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) %in% 1:2)
      elements <- self$get_tail_elements()
      if (length(elements) == 1L) {
        expr1 <- elements[[1L]]$compile()
        expr2 <- expr1
      } else {
        expr1 <- elements[[1L]]$compile()
        expr2 <- elements[[2L]]$compile()
      }
      self$emit("arma::trans(", expr1, ") * (", expr2, ")")
    },
    get_cpp_type = function() {
      "arma::mat"
    }
  )
)

ast_node_tcrossprod <- R6::R6Class(
  classname = "ast_node_tcrossprod",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) %in% 1:2)
      elements <- self$get_tail_elements()
      if (length(elements) == 1L) {
        expr1 <- elements[[1L]]$compile()
        expr2 <- expr1
      } else {
        expr1 <- elements[[1L]]$compile()
        expr2 <- elements[[2L]]$compile()
      }
      self$emit("(", expr1, ") * arma::trans(", expr2, ")")
    },
    get_cpp_type = function() {
      "arma::mat"
    }
  )
)

ast_node_while <- R6::R6Class(
  classname = "ast_node_while",
  inherit = ast_node,
  public = list(
    compile = function() {
      elements <- self$get_tail_elements()
      stopifnot(length(elements) == 2L)
      self$emit(
        "while (", elements[[1L]]$compile(), ")\n",
        elements[[2L]]$compile()
      )
    }
  )
)

ast_node_for <- R6::R6Class(
  classname = "ast_node_for",
  inherit = ast_node,
  public = list(
    compile = function() {
      elements <- self$get_tail_elements()
      stopifnot(length(elements) == 3L)
      # we currently allow just one pattern
      # for (i in X) { ... }
      # will be translated to
      # for (const auto& i : X) {
      #   ...
      # }
      stopifnot("ast_node_name" %in% class(elements[[1L]]))
      iter_var_name <- elements[[1L]]$compile()
      container <- elements[[2L]]
      body <- elements[[3L]]
      is_iter_var_used <- private$is_loop_var_used(iter_var_name, body)
      if (is_iter_var_used) {
        # TODO: this modifies the input tree during compilation mmmmh
        # This means we now have to run the type deduction again for the body
        # Think about creating the structure during construction of the ast
        stopifnot("ast_node_block" %in% class(body))
        body_scope <- self$get_scope()
        dummy_iter_var <- ast_node_dummy$new()
        dummy_iter_var$set_cpp_type("auto")
        body_scope$register_new_name(iter_var_name, dummy_iter_var)
        self$emit(
          "for (const auto& ", iter_var_name,
          " : ", container$compile(), ")\n",
          body$compile(),
          "\n"
        )
      } else {

        container_var_name <- make_random_var_name(self$get_scope()$new_random_variable_id())
        iter_var_name <- make_random_var_name(self$get_scope()$new_random_variable_id())

        self$emit(
          "\nauto&& ", container_var_name, " = ", container$compile(), ";\n",
          "for (auto ",
          iter_var_name, " = ", container_var_name, ".begin();",
          iter_var_name, " != ", container_var_name, ".end();",
          "++", iter_var_name, ")\n",
          body$compile(),
          "\n"
        )
      }
    }
  ),
  private = list(
    is_loop_var_used = function(var_name, body) {
      all_names <- body$find_all_names()
      for (name_node in all_names) {
        if (var_name == name_node$get_head()) {
          return(TRUE)
        }
      }
      FALSE
    }
  )
)

ast_node_qr_init <- R6::R6Class(
  classname = "ast_node_qr_init",
  inherit = ast_node,
  public = list(
    compile = function(var_name) {
      stopifnot(is.character(var_name))
      stopifnot(length(self$get_tail_elements()) == 1L)
      rhs <- self$get_tail_elements()[[1L]]$compile()
      var_name_q <- paste0(var_name, "__Q")
      var_name_r <- paste0(var_name, "__R")
      self$emit(
        "arma::mat ", var_name_q, ", ", var_name_r, ";\n",
        "arma::qr_econ( ", var_name_q, ", ", var_name_r, ", ", rhs, " );\n"
      )
    }
  )
)

ast_node_dollar <- R6::R6Class(
  classname = "ast_node_dollar",
  inherit = ast_node,
  public = list(
    compile = function() {
      lhs_val <- private$get_lhs_value()
      if (!is.null(lhs_val)) {
        rhs <- self$get_tail_elements()[[2L]]
        self$emit(
          lhs_val$dollar(rhs)$compile()
        )
      }
    },
    get_cpp_type = function() {
      lhs_val <- private$get_lhs_value()
      if (!is.null(lhs_val)) {
        rhs <- self$get_tail_elements()[[2L]]
        return(lhs_val$dollar(rhs)$get_cpp_type())
      }
      private$cpp_type
    }
  ),
  private = list(
    get_lhs_value = function() {
      elements <- self$get_tail_elements()
      lhs <- elements[[1L]]
      rhs <- elements[[2L]]
      stopifnot("ast_node_name" %in% class(lhs))
      stopifnot("ast_node_name" %in% class(rhs))
      #stopifnot(self$get_scope()$is_name_defined(lhs$get_name()))
      self$get_scope()$get_value_node_for_name(lhs$get_name())
    }
  )
)

ast_node_svd_init <- R6::R6Class(
  classname = "ast_node_svd_init",
  inherit = ast_node,
  public = list(
    compile = function(assigned_var_name) {
      stopifnot(length(self$get_tail_elements()) == 1L)
      stopifnot("ast_node_assignment" %in% class(self$get_parent()))
      rhs <- self$get_tail_elements()[[1L]]$compile()
      var_name <- make_random_var_name(self$get_scope()$new_random_variable_id())
      private$set_base_varname(var_name)
      var_name_u <- private$get_varname("u")
      var_name_v <- private$get_varname("v")
      var_name_d <- private$get_varname("d")
      self$emit(
        "arma::mat ", var_name_u, ", ", var_name_v, ";\n",
        "arma::colvec ", var_name_d, ";\n",
        "arma::svd_econ(", var_name_u, ", ", var_name_d, ", ", var_name_v, ", ", rhs, ");\n"
      )
    },
    dollar = function(node) {
      stopifnot("ast_node_name" %in% class(node))
      type <- node$get_name()
      var_name <- private$get_varname(type)
      var_name_node <- ast_node_name$new(var_name, var_name)
      cpp_type <- if (type %in% c("u", "v")) {
        "arma::mat"
      } else {
        "arma::colvec"
      }
      var_name_node$set_cpp_type(cpp_type)
      var_name_node$set_parent(self)
      var_name_node$set_scope(self$get_scope())
      var_name_node
    }
  ),
  private = list(
    base_var_name = NULL,
    set_base_varname = function(name) {
      private$base_var_name <- name
    },
    get_varname = function(type) {
      stopifnot(type %in% c("u", "v", "d"))
      paste0(private$base_var_name, "__", type)
    }
  )
)

ast_node_qr_q <- R6::R6Class(
  classname = "ast_node_qr_q",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(self$get_tail_elements()[[1L]]$compile(), "__Q")
    },
    get_cpp_type = function() {
      "arma::mat"
    }
  )
)

ast_node_qr_r <- R6::R6Class(
  classname = "ast_node_qr_q",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(self$get_tail_elements()[[1L]]$compile(), "__R")
    },
    get_cpp_type = function() {
      "arma::mat"
    }
  )
)

ast_node_seq_len <- R6::R6Class(
  classname = "ast_node_seq_len",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      until <- self$get_tail_elements()[[1L]]$compile()
      self$emit(
        "arma::linspace<arma::colvec>(1, ", until, ", ", until, ")"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)


ast_node_seq <- R6::R6Class(
  classname = "ast_node_seq",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 3L)
      elements <- self$get_tail_elements()
      from <- elements[[1L]]$compile()
      to <- elements[[2L]]$compile()
      len_out <- elements[[3L]]$compile()
      self$emit(
        "arma::linspace<arma::colvec>(", from, ", ", to, ", ", len_out, ")"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

ast_node_colon <- R6::R6Class(
  classname = "ast_node_colon",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      elements <- self$get_tail_elements()
      from <- elements[[1L]]$compile()
      to <- elements[[2L]]$compile()
      self$emit(
        "arma::linspace<arma::colvec>(", from, ", ", to, ", ", to, " - ", from, " + 1)"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

ast_node_break <- R6::R6Class(
  classname = "ast_node_break",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 0L)
      self$emit("break;\n")
    }
  )
)

ast_node_next <- R6::R6Class(
  classname = "ast_node_next",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 0L)
      self$emit("continue;\n")
    }
  )
)

ast_node_element_access <- R6::R6Class(
  classname = "ast_node_element_access",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) %in% 2:3)
      args <- self$get_tail_elements()[[2L]]$compile()
      if (length(self$get_tail_elements()) == 3L) {
        args <- c(args, self$get_tail_elements()[[3L]]$compile())
      }
      # counting starts at 0
      args <- paste0(args, " - 1")
      self$emit(
        self$get_tail_elements()[[1L]]$compile(),
        "(", paste0(args, collapse = ", "), ")"
      )
    },
    get_cpp_type = function() {
      # TODO: depends on matrix type
      "auto"
    }
  )
)

ast_node_norm <- R6::R6Class(
  classname = "ast_node_norm",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        "arma::norm(", self$get_tail_elements()[[1L]]$compile(), ")"
      )
    },
    get_cpp_type = function() {
      "double"
    }
  )
)

ast_node_diag <- R6::R6Class(
  classname = "ast_node_diag",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        "arma::diagmat(", self$get_tail_elements()[[1L]]$compile(), ")"
      )
    },
    get_cpp_type = function() {
      "arma::mat"
    }
  )
)

ast_node_rep_int <- R6::R6Class(
  classname = "ast_node_rep_int",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 2L)
      self$emit(
        "arma::colvec(std::vector<double>(",
        self$get_tail_elements()[[2L]]$compile(),
        ", ",
        self$get_tail_elements()[[1L]]$compile(),
        "))"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

make_generic_unary_function_class <- function(class_name, fun) {
  R6::R6Class(
    classname = paste0("ast_node_generic_unary_function_", class_name),
    inherit = ast_node_function_call,
    public = list(
      compile = function() {
        stopifnot(length(self$get_tail_elements()) == 1L)
        fun_name <- if (is.character(fun)) {
          fun
        } else {
          arma_type <- self$get_tail_elements()[[1L]]$is_armadillo_cpp_type()
          if (arma_type) {
            fun$arma
          } else {
            fun$std
          }
        }
        self$emit(fun_name, "(", self$get_tail_elements()[[1L]]$compile(), ")")
      },
      get_cpp_type = function() {
        # the assumption is that the input type is the same as the return type
        # for these kind of functions
        self$get_tail_elements()[[1L]]$get_cpp_type()
      }
    )
  )
}

ast_node_sum <- R6::R6Class(
  classname = "ast_node_sum",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit("arma::accu(", self$get_tail_elements()[[1L]]$compile(), ")")
    },
    get_cpp_type = function() {
      # TODO: depends on data type
      "auto"
    }
  )
)

ast_node_type_spec <- R6::R6Class(
  classname = "ast_node_type_spec",
  inherit = ast_node
)

ast_node_type_matrix <- R6::R6Class(
  classname = "ast_node_type_matrix",
  inherit = ast_node_type_spec,
  public = list(
    get_cpp_type = function() {
      "arma::mat"
    }
  )
)

ast_node_type_colvec <- R6::R6Class(
  classname = "ast_node_type_colvec",
  inherit = ast_node_type_spec,
  public = list(
    get_cpp_type = function() {
      "arma::colvec"
    }
  )
)

ast_node_type_rowvec <- R6::R6Class(
  classname = "ast_node_type_rowvec",
  inherit = ast_node_type_spec,
  public = list(
    get_cpp_type = function() {
      "arma::rowvec"
    }
  )
)

ast_node_type_scalar_integer <- R6::R6Class(
  classname = "ast_node_type_scalar_integer",
  inherit = ast_node_type_spec,
  public = list(
    get_cpp_type = function() {
      "int"
    }
  )
)

ast_node_type_scalar_logical <- R6::R6Class(
  classname = "ast_node_type_scalar_logical",
  inherit = ast_node_type_spec,
  public = list(
    get_cpp_type = function() {
      "bool"
    }
  )
)

ast_node_type_scalar_numeric <- R6::R6Class(
  classname = "ast_node_type_scalar_numeric",
  inherit = ast_node_type_spec,
  public = list(
    get_cpp_type = function() {
      "double"
    }
  )
)

element_type_map <- new.env(parent = emptyenv())
element_type_map[["<-"]] <- ast_node_assignment
element_type_map[["{"]] <- ast_node_block
element_type_map[["function"]] <- ast_node_function
element_type_map[["arma_pairlist"]] <- ast_node_arma_pairlist
element_type_map[["["]] <- ast_node_element_access
element_type_map[["$"]] <- ast_node_dollar
element_type_map[["+"]] <- ast_node_plus
element_type_map[["-"]] <- ast_node_minus
element_type_map[["^"]] <- ast_node_pow
element_type_map[["*"]] <- ast_node_mult
element_type_map[["/"]] <- ast_node_div
element_type_map[["<"]] <- ast_node_less
element_type_map[[">"]] <- ast_node_greater
element_type_map[["<="]] <- ast_node_leq
element_type_map[[">="]] <- ast_node_geq
element_type_map[["=="]] <- ast_node_equal
element_type_map[["!="]] <- ast_node_nequal
element_type_map[["&&"]] <- ast_node_logical_and
element_type_map[["||"]] <- ast_node_logical_or
element_type_map[["%*%"]] <- ast_node_matmul
element_type_map[["("]] <- ast_node_bracket
element_type_map[["svd"]] <- ast_node_svd_init
element_type_map[["qr"]] <- ast_node_qr_init
element_type_map[["qr.Q"]] <- ast_node_qr_q
element_type_map[["qr.R"]] <- ast_node_qr_r
element_type_map[["solve"]] <- ast_node_solve
element_type_map[["sum"]] <- ast_node_sum
element_type_map[["nrow"]] <- ast_node_nrow
element_type_map[["ncol"]] <- ast_node_ncol
element_type_map[["length"]] <- ast_node_length
element_type_map[["seq_len"]] <- ast_node_seq_len
element_type_map[["seq"]] <- ast_node_seq
element_type_map[[":"]] <- ast_node_colon
element_type_map[["rep.int"]] <- ast_node_rep_int
element_type_map[["norm"]] <- ast_node_norm
element_type_map[["diag"]] <- ast_node_diag
element_type_map[["crossprod"]] <- ast_node_crossprod
element_type_map[["tcrossprod"]] <- ast_node_tcrossprod
element_type_map[["colSums"]] <- ast_node_colsums
element_type_map[["rowSums"]] <- ast_node_rowsums
element_type_map[["colMeans"]] <- ast_node_colmeans
element_type_map[["rowMeans"]] <- ast_node_rowmeans
element_type_map[["return"]] <- ast_node_return
element_type_map[["backsolve"]] <- ast_node_backsolve
element_type_map[["forwardsolve"]] <- ast_node_forwardsolve
element_type_map[["if"]] <- ast_node_if
element_type_map[["for"]] <- ast_node_for
element_type_map[["while"]] <- ast_node_while
element_type_map[["type_matrix"]] <- ast_node_type_matrix
element_type_map[["type_colvec"]] <- ast_node_type_colvec
element_type_map[["type_rowvec"]] <- ast_node_type_rowvec
element_type_map[["type_scalar_integer"]] <- ast_node_type_scalar_integer
element_type_map[["type_scalar_numeric"]] <- ast_node_type_scalar_numeric
element_type_map[["type_scalar_logical"]] <- ast_node_type_scalar_logical

# TODO: not pretty
multi_dispatch_fun <- function(arma, std) {
  structure(
    list(arma = arma, std = std),
    class = "multi_dispatch_fun"
  )
}
unary_function_mapping <- new.env(hash = TRUE, parent = emptyenv())
unary_function_mapping$t <- "arma::trans"
unary_function_mapping$`!` <- "!"
unary_function_mapping$exp <- multi_dispatch_fun(arma = "arma::exp", std = "std::exp")
unary_function_mapping$abs <- multi_dispatch_fun(arma = "arma::abs", std = "std::abs")
unary_function_mapping$log <- multi_dispatch_fun(arma = "arma::log", std = "std::log")
unary_function_mapping$chol <- "arma::chol"
unary_function_mapping$cumsum <- "arma::cumsum"
unary_function_mapping$sqrt <- multi_dispatch_fun(arma = "arma::sqrt", std = "std::sqrt")
unary_function_mapping$floor <- multi_dispatch_fun(arma = "arma::floor", std = "std::floor")
unary_function_mapping$ceiling <- multi_dispatch_fun(arma = "arma::ceil", std = "std::ceil")
unary_function_mapping$sort <- "arma::sort"
unary_function_mapping$unique <- "arma::unique"
unary_function_mapping$pnorm <- "arma::normcdf"
unary_function_mapping$det <- "arma::det"
unary_function_mapping$cos <- multi_dispatch_fun(arma = "arma::cos", std = "std::cos")
unary_function_mapping$acos <- "arma::acos"
unary_function_mapping$cosh <- "arma::cosh"
unary_function_mapping$acosh <- "arma::acosh"
unary_function_mapping$sin <- multi_dispatch_fun(arma = "arma::sin", std = "std::sin")
unary_function_mapping$asin <- "arma::asin"
unary_function_mapping$sinh <- "arma::sinh"
unary_function_mapping$asinh <- "arma::asinh"
unary_function_mapping$tan <- multi_dispatch_fun(arma = "arma::tan", std = "std::tan")
unary_function_mapping$atan <- "arma::atan"
unary_function_mapping$tanh <- "arma::tanh"
unary_function_mapping$atanh <- "arma::atanh"
unary_function_mapping$cumsum <- "arma::cumsum"
unary_function_mapping$cumprod <- "arma::cumprod"

new_node_by_chr <- function(head_chr, is_call = FALSE) {
  if (!is.null(element_type_map[[head_chr]])) {
    return(element_type_map[[head_chr]])
  }
  if (!is.null(unary_function_mapping[[head_chr]])) {
    return(make_generic_unary_function_class(head_chr, unary_function_mapping[[head_chr]]))
  }
  if (is_call) {
    return(ast_node_function_call)
  }
  ast_node
}

make_random_var_name <- function(id) {
  paste0("arma___tmp_", id)
}

# our type system
auto_or_arma_type <- function(type) {
  if (grepl("^arma", type)) {
    type
  } else {
    "auto"
  }
}

arma_element_type <- function(type) {
  dbl_types <- c("arma::mat", "arma::colvec", "arma::rowvec")
  int_types <- c("arma::imat", "arma::icolvec", "arma::irowvec")
  uint_types <- c("arma::umat", "arma::ucolvec", "arma::urowvec")
  if (type %in% dbl_types) {
    "double"
  } else if (type %in% int_types) {
    "int"
  } else if (type %in% uint_types) {
    "uint"
  }
}

get_arma_type <- function(base_type, elements) {
  prefix <- ""
  if (elements == "int") prefix <- "i"
  if (elements == "uint") prefix <- "u"
  paste0("arma::", prefix, base_type)
}

find_arma_cpp_types <- function(nodes) {
  Filter(function(x) {
    x$is_armadillo_cpp_type()
  }, nodes)
}
