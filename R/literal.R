new_literal_class <- function(value) {
  constructor <- function() {
    value
  }

  validator <- function(object) {
    if (!identical(object, value)) {
      sprintf("Object must be a literal %s not <%s>", deparse(value), base_class(object))
    }
  }

  out  <- list(
    class = deparse(value),
    value = value,
    constructor = constructor,
    validator = validator
  )
  class(out) <- "R7_literal_class"
  out
}

is_literal_value <- function(x) {
  length(x) == 1L && is.atomic(x) && is.null(dim(x))
}

is_literal_class <- function(x) inherits(x, "R7_literal_class")

#' @export
print.R7_literal_class <- function(x, ...) {
  cat("<R7_literal_class>: ", class_desc(x), "\n", sep = "")
  invisible(x)
}

#' @export
str.R7_literal_class <- function(object, ..., nest.lev = 0) {
  cat(if (nest.lev > 0) " ")
  print(object, ..., nest.lev = nest.lev)
}

#' Literal classes
#'
#' @description
#' Literal classes represent objects whose class is determined by their literal
#' value. They are mostly useful in class unions, where they can model a finite
#' set of valid values.
#'
#' @examples
#' class_literal("GET")
#'
#' class_http_method <- class_literal_union("GET", "POST", "PUT", "DELETE")
#' class_http_method
#'
#' Request <- new_class("Request", properties = list(method = class_http_method))
#' Request
#' Request(method = "GET")
#' try(Request(method = "foo"))
#' @name literal_classes
NULL

#' @param value A literal value to construct a class for.
#' @rdname literal_classes
#' @export
class_literal <- function(value) {
  if (!is_literal_value(value)) {
    stop("`value` must be a dimensionless length 1 atomic vector.", call. = FALSE)
  }
  new_literal_class(value)
}

#' @param ... Literal values to construct a class union for.
#' @rdname literal_classes
#' @export
class_literal_union <- function(...) {
  do.call("new_union", lapply(list(...), class_literal))
}
