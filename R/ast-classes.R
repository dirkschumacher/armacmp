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
    set_parent = function(node) {
      private$parent <- node
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
    get_cpp_type = function() {
      private$cpp_type
    },
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
      stop("Sorry, but the expression:\n\n",
        paste0(deparse(self$get_sexp()), collapse = "\n"),
        "\n\ncannot be translated into a C++ construct.\n",
        "Not all R functions are supported.",
        call. = FALSE
      )
    },
    find_all_returns = function() {
      private$find_nodes("ast_node_return")
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
    cpp_type = "arma::mat",
    find_nodes = function(node_type) {
      find_elements_rec <- function(node) {
        if (node_type %in% class(node)) {
          return(list(node))
        }
        lapply(node$get_tail_elements(), find_elements_rec)
      }
      unlist(find_elements_rec(self))
    }
  )
)

ast_node_terminal <- R6::R6Class(
  classname = "ast_node_terminal",
  inherit = ast_node,
  public = list(
    compile = function() {
      as.character(self$get_head())
    }
  )
)

ast_node_name <- R6::R6Class(
  classname = "ast_node_name",
  inherit = ast_node_terminal
)

ast_node_assignment <- R6::R6Class(
  classname = "ast_node_assignment",
  inherit = ast_node,
  public = list(
    compile = function() {
      operands <- self$get_tail_elements()
      stopifnot(length(operands) == 2L)

      symbol <- operands[[1L]]$get_sexp()
      stopifnot(is.name(symbol))

      # check if assignment is a QR decomposition
      rhs <- operands[[2L]]
      if ("ast_node_qr_init" %in% class(rhs)) {
        return(rhs$compile(as.character(symbol)))
      }

      type <- if (self$is_initial_assignment()) {
        paste0(operands[[2L]]$get_cpp_type(), " ")
      } else {
        ""
      }
      self$emit(
        type,
        as.character(symbol),
        " = ",
        operands[[2L]]$compile(),
        ";"
      )
    },
    is_initial_assignment = function() {
      private$inital_assignment
    },
    set_initial_assignment = function(val) {
      private$inital_assignment <- val
    }
  ),
  private = list(
    inital_assignment = FALSE
  )
)

ast_node_return <- R6::R6Class(
  classname = "ast_node_return",
  inherit = ast_node,
  public = list(
    compile = function() {
      operands <- self$get_tail_elements()
      stopifnot(length(operands) %in% c(1L, 2L))
      if (length(operands) == 2L) {
        type_spec <- eval(operands[[2]]$get_sexp())
        self$set_cpp_type(type_spec$cpp_type)
      }
      self$emit(
        "return ",
        operands[[1L]]$compile(),
        ";"
      )
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

ast_node_block <- R6::R6Class(
  classname = "ast_node_block",
  inherit = ast_node,
  public = list(
    initialize = function(...) {
      super$initialize(...)
      private$name_store <- new.env(parent = emptyenv())
    },
    compile = function() {
      self$emit(
        "{\n",
        paste0(lapply(self$get_tail_elements(), function(x) x$compile()), collapse = "\n"),
        "\n}\n"
      )
    },
    register_new_name = function(node) {
      # we assume it is always a name
      stopifnot(is.name(node$get_sexp()))
      # no reassignments, first one counts
      if (is.null(private$name_store[[as.character(node$get_sexp())]])) {
        private$name_store[[as.character(node$get_sexp())]] <- node
      }
    },
    is_name_defined = function(name) {
      !is.null(private$name_store[[as.character(name)]]) ||
        (!is.null(self$get_scope()) && self$get_scope()$is_name_defined(name))
    },
    get_node_by_name = function(name) {
      el <- private$name_store[[as.character(name)]]
      if (is.null(el)) {
        self$get_scope()$get_node_by_name(name)
      } else {
        el
      }
    }
  ),
  private = list(
    name_store = NULL
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
      "arma::mat"
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
      "arma::mat"
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
      op <- if (elements[[1L]]$has_auto_cpp_type() &&
        elements[[2L]]$has_auto_cpp_type()) {
        "*"
      } else {
        "%"
      }
      self$emit(
        elements[[1L]]$compile(),
        " ",
        op,
        " ",
        elements[[2L]]$compile()
      )
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
      fun <- if (elements[[1L]]$has_auto_cpp_type() &&
        elements[[2L]]$has_auto_cpp_type()) {
        "std::pow"
      } else {
        "arma::pow"
      }
      self$emit(
        fun, "( ",
        elements[[1L]]$compile(),
        ", ",
        elements[[2L]]$compile(),
        " )"
      )
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
      "arma::rowvec"
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
      "arma::colvec"
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
      "arma::colvec"
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
      "arma::rowvec"
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
      self$emit("arma::trans(", expr1, ") * ", expr2)
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
        self$emit(
          "for (const auto& ", iter_var_name,
          " : ", container$compile(), ")\n",
          body$compile(),
          "\n"
        )
      } else {
        container_var_name <- random_var_name()
        iter_var_name <- random_var_name()
        # TODO: this creates non-deterministic source code
        # might not be a good idea ...
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

ast_node_qr_q <- R6::R6Class(
  classname = "ast_node_qr_q",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(self$get_tail_elements()[[1L]]$compile(), "__Q")
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
    }
  )
)

ast_node_seq_len <- R6::R6Class(
  classname = "ast_node_seq_len",
  inherit = ast_node,
  public = list(
    compile = function() {
      stopifnot(length(self$get_tail_elements()) == 1L)
      self$emit(
        "arma::vec(Rcpp::seq_len(",
        self$get_tail_elements()[[1L]]$compile(),
        "))"
      )
    },
    get_cpp_type = function() {
      "arma::colvec"
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
    inherit = ast_node,
    public = list(
      compile = function() {
        stopifnot(length(self$get_tail_elements()) == 1L)
        fun_type <- if (is.character(fun)) {
          fun
        } else {
          auto_type <- self$get_tail_elements()[[1L]]$has_auto_cpp_type()
          if (auto_type) {
            fun$std
          } else {
            fun$arma
          }
        }
        self$emit(fun_type, "(", self$get_tail_elements()[[1L]]$compile(), ")")
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
      "auto"
    }
  )
)

# note to self:
# count depth of scopes and use it for indentation of
# for, assignment, return and if

element_type_map <- new.env(parent = emptyenv())
element_type_map[["<-"]] <- ast_node_assignment
element_type_map[["{"]] <- ast_node_block
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
element_type_map[["%*%"]] <- ast_node_matmul
element_type_map[["("]] <- ast_node_bracket
element_type_map[["qr"]] <- ast_node_qr_init
element_type_map[["qr.Q"]] <- ast_node_qr_q
element_type_map[["qr.R"]] <- ast_node_qr_r
element_type_map[["solve"]] <- ast_node_solve
element_type_map[["sum"]] <- ast_node_sum
element_type_map[["nrow"]] <- ast_node_nrow
element_type_map[["ncol"]] <- ast_node_ncol
element_type_map[["seq_len"]] <- ast_node_seq_len
element_type_map[["rep.int"]] <- ast_node_rep_int
element_type_map[["crossprod"]] <- ast_node_crossprod
element_type_map[["colSums"]] <- ast_node_colsums
element_type_map[["rowSums"]] <- ast_node_rowsums
element_type_map[["colMeans"]] <- ast_node_colmeans
element_type_map[["rowMeans"]] <- ast_node_rowmeans
element_type_map[["return"]] <- ast_node_return
element_type_map[["backsolve"]] <- ast_node_backsolve
element_type_map[["forwardsolve"]] <- ast_node_forwardsolve
element_type_map[["if"]] <- ast_node_if
element_type_map[["for"]] <- ast_node_for


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
unary_function_mapping$diag <- "arma::diagmat"
unary_function_mapping$sqrt <- multi_dispatch_fun(arma = "arma::sqrt", std = "std::sqrt")
unary_function_mapping$sort <- "arma::sort"
unary_function_mapping$unique <- "arma::unique"
unary_function_mapping$pnorm <- "arma::normcdf"
unary_function_mapping$det <- "arma::det"
unary_function_mapping$cos <- "arma::cos"
unary_function_mapping$acos <- "arma::acos"
unary_function_mapping$cosh <- "arma::cosh"
unary_function_mapping$acosh <- "arma::acosh"
unary_function_mapping$sin <- "arma::sin"
unary_function_mapping$asin <- "arma::asin"
unary_function_mapping$sinh <- "arma::sinh"
unary_function_mapping$asinh <- "arma::asinh"
unary_function_mapping$tan <- "arma::tan"
unary_function_mapping$atan <- "arma::atan"
unary_function_mapping$tanh <- "arma::tanh"
unary_function_mapping$atanh <- "arma::atanh"
unary_function_mapping$cumsum <- "arma::cumsum"
unary_function_mapping$cumprod <- "arma::cumprod"

new_node_by_chr <- function(head_chr) {
  if (!is.null(element_type_map[[head_chr]])) {
    return(element_type_map[[head_chr]])
  }
  if (!is.null(unary_function_mapping[[head_chr]])) {
    return(make_generic_unary_function_class(head_chr, unary_function_mapping[[head_chr]]))
  }
  ast_node
}

random_var_name <- function() {
  paste0("__armacmp_", substr(digest::sha1(stats::runif(1)), 1, 6))
}
