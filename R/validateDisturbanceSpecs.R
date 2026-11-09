utils::globalVariables(c("eventTime", "eventType", "moduleName", "eventPriority"))

#' Validate and normalize disturbance specifications
#'
#' Checks the user-supplied `disturbances` parameter and fills in defaults. Every problem
#' that would cause a disturbance to run at the wrong time, or not at all, is reported as
#' an error here rather than being tolerated, because the resulting simulation would run
#' to completion and produce plausible-looking but wrong output.
#'
#' @param disturbances a list of disturbance specifications, as supplied to the module's
#'   `disturbances` parameter. Each element is a named list; see the module manual for the
#'   fields it may contain.
#' @param sim a `simList` object, used only to resolve function references.
#'
#' @return a named list of normalized specifications, named by each specification's
#'   `name`. Returns an empty list when no disturbances were supplied.
#'
#' @keywords internal
#' @rdname validateDisturbanceSpecs
validateDisturbanceSpecs <- function(disturbances, sim) {
  if (is.null(disturbances) || length(disturbances) == 0L) {
    return(list())
  }

  if (!is.list(disturbances)) {
    stop("The 'disturbances' parameter must be a list of disturbance specifications, ",
         "but it is of class '", paste(class(disturbances), collapse = "/"), "'.")
  }

  ## A single spec passed without wrapping it in an outer list is an easy mistake, and
  ## would otherwise be read as a list of malformed specs.
  if (!is.null(disturbances$fn)) {
    stop("The 'disturbances' parameter looks like a single disturbance specification ",
         "rather than a list of them. Wrap it in a list, e.g. ",
         "disturbances = list(list(fn = ..., name = ..., after = ...)).")
  }

  specs <- lapply(seq_along(disturbances), function(i) {
    validateOneSpec(disturbances[[i]], index = i)
  })

  specNames <- vapply(specs, function(x) x$name, character(1))

  dups <- unique(specNames[duplicated(specNames)])
  if (length(dups) > 0L) {
    stop("Each disturbance specification needs a unique 'name', because the name is used ",
         "as the event name. Duplicated: ", paste(dups, collapse = ", "), ".")
  }

  ## Two specs following the same upstream event with the same offset would be scheduled
  ## at an identical time and priority. Ties fall back to the order events happened to be
  ## added to the queue, which depends on module load order, so the running order would be
  ## effectively arbitrary and could change between runs.
  collisionKey <- vapply(specs, function(x) {
    paste(x$after, x$afterEvent, format(x$offset, digits = 15), sep = "\r")
  }, character(1))
  collisions <- unique(collisionKey[duplicated(collisionKey)])
  for (key in collisions) {
    clashing <- specNames[collisionKey == key]
    parts <- strsplit(key, "\r", fixed = TRUE)[[1]]
    stop("Disturbance specifications ", paste(clashing, collapse = " and "),
         " all follow the '", parts[2], "' event of module '", parts[1],
         "' with the same offset (", parts[3], "), so they would be scheduled at an ",
         "identical time and priority. Their running order would then depend on module ",
         "load order rather than on anything you have specified. Give them different ",
         "'offset' values to fix their order explicitly.")
  }

  names(specs) <- specNames
  specs
}

#' Validate one disturbance specification
#'
#' @param spec a single disturbance specification: a named list.
#' @param index integer. The specification's position in the `disturbances` list, used to
#'   identify it in error messages before its name is known to be valid.
#'
#' @return the specification, with defaults filled in and `fn` resolved to a function.
#'
#' @keywords internal
#' @rdname validateOneSpec
validateOneSpec <- function(spec, index) {
  label <- paste0("Disturbance specification ", index)

  if (!is.list(spec)) {
    stop(label, " must be a named list, but is of class '",
         paste(class(spec), collapse = "/"), "'.")
  }

  known <- c("fn", "name", "after", "afterEvent", "offset", "triggerObj",
             "returnsAs", "args")
  unknown <- setdiff(names(spec), known)
  if (length(unknown) > 0L) {
    ## Silently ignoring an unknown field would quietly discard a setting the user meant
    ## to take effect, e.g. a misspelled 'triggerObj'.
    stop(label, " has unrecognized field(s): ", paste(unknown, collapse = ", "),
         ". Valid fields are: ", paste(known, collapse = ", "), ".")
  }

  for (required in c("name", "after", "afterEvent")) {
    value <- spec[[required]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      stop(label, " needs '", required, "' to be a single non-empty character string.")
    }
  }

  label <- paste0("Disturbance specification '", spec$name, "'")

  spec$fn <- resolveDisturbanceFn(spec$fn, label = label)

  if (is.null(spec$offset)) {
    spec$offset <- 0.1
  }
  if (!is.numeric(spec$offset) || length(spec$offset) != 1L || is.na(spec$offset)) {
    stop(label, " needs 'offset' to be a single number.")
  }
  if (spec$offset <= 0) {
    ## A zero or negative offset would place the disturbance at or before the upstream
    ## event, so it would act on the previous timestep's results, or on nothing at all.
    stop(label, " has offset ", spec$offset, ", which would schedule it at or before the ",
         "'", spec$afterEvent, "' event it is supposed to follow (lower priority values ",
         "run first). Use a positive offset.")
  }

  if (!is.null(spec$triggerObj)) {
    if (!is.character(spec$triggerObj) || length(spec$triggerObj) != 1L ||
        is.na(spec$triggerObj) || !nzchar(spec$triggerObj)) {
      stop(label, " needs 'triggerObj' to be a single non-empty character string naming ",
           "a simList object, or NULL to run every time.")
    }
  }

  if (!is.null(spec$returnsAs)) {
    if (!is.character(spec$returnsAs) || length(spec$returnsAs) != 1L ||
        is.na(spec$returnsAs) || !nzchar(spec$returnsAs)) {
      stop(label, " needs 'returnsAs' to be a single non-empty character string naming ",
           "the simList object to assign an unnamed return value to, or NULL.")
    }
  }

  if (is.null(spec$args)) {
    spec$args <- list()
  }
  if (!is.list(spec$args)) {
    stop(label, " needs 'args' to be a named list of extra arguments, or NULL.")
  }
  if (length(spec$args) > 0L &&
      (is.null(names(spec$args)) || any(!nzchar(names(spec$args))))) {
    stop(label, " needs every element of 'args' to be named, since the elements are ",
         "matched to the function's arguments by name.")
  }

  ## Which calling convention the function uses. A function with a `sim` argument is
  ## given the simList and expected to return it; any other function has its arguments
  ## resolved by name and its return value written back. Note that several real
  ## disturbance functions *mention* `sim` in their argument defaults, e.g.
  ## `cohortData = copy(sim$cohortData)`, without having a `sim` argument themselves --
  ## only an actual `sim` argument counts here.
  spec$takesSim <- "sim" %in% names(formals(spec$fn))

  spec
}

#' Resolve a disturbance function reference
#'
#' @param fn either a function, or a character string naming one, optionally namespaced as
#'   `"pkg::fun"`.
#' @param label character. How to refer to the specification in error messages.
#'
#' @return a function.
#'
#' @keywords internal
#' @rdname resolveDisturbanceFn
resolveDisturbanceFn <- function(fn, label) {
  if (is.function(fn)) {
    return(fn)
  }

  if (is.null(fn)) {
    stop(label, " needs 'fn': the function to run, given either as a function object or ",
         "as a character string such as \"pkg::fun\".")
  }

  if (!is.character(fn) || length(fn) != 1L || is.na(fn) || !nzchar(fn)) {
    stop(label, " needs 'fn' to be a function, or a single character string naming one.")
  }

  ## Resolved here rather than at module load so that a package named in a string
  ## reference need only be installed when that disturbance is actually used.
  resolved <- tryCatch(eval(parse(text = fn)), error = function(e) e)

  if (inherits(resolved, "error")) {
    stop(label, " could not resolve its function \"", fn, "\": ",
         conditionMessage(resolved),
         ". Check that the name is spelled correctly and that any package it belongs to ",
         "is installed.")
  }
  if (!is.function(resolved)) {
    stop(label, " resolved \"", fn, "\" to an object of class '",
         paste(class(resolved), collapse = "/"), "' rather than a function.")
  }

  resolved
}
