#' Should this disturbance run in this timestep?
#'
#' Tests a specification's trigger object, the way a fire module's burn map is checked
#' before post-fire effects are applied. A specification with no `triggerObj` always runs.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param verbose logical. Whether to report a skipped disturbance.
#'
#' @return `TRUE` if the disturbance function should be called.
#'
#' @keywords internal
#' @rdname disturbanceTriggered
disturbanceTriggered <- function(sim, spec, verbose = TRUE) {
  if (is.null(spec$triggerObj)) {
    return(TRUE)
  }

  if (!spec$triggerObj %in% ls(sim)) {
    if (verbose) {
      message("Biomass_genericDisturbance: skipping disturbance '", spec$name, "' at time ",
              time(sim), " because its trigger object '", spec$triggerObj,
              "' is not in the simList.")
    }
    return(FALSE)
  }

  trigger <- sim[[spec$triggerObj]]

  ## Only emptiness is tested, not content: this module cannot know what a non-empty
  ## trigger means for any particular disturbance. Note this detects an absent or empty
  ## object, but cannot tell an object written this timestep from one left over from an
  ## earlier one.
  isEmpty <- is.null(trigger) ||
    (is.atomic(trigger) && length(trigger) == 0L) ||
    (is.data.frame(trigger) && nrow(trigger) == 0L)

  if (isEmpty) {
    if (verbose) {
      message("Biomass_genericDisturbance: skipping disturbance '", spec$name, "' at time ",
              time(sim), " because its trigger object '", spec$triggerObj, "' is empty.")
    }
    return(FALSE)
  }

  TRUE
}

#' Call a disturbance function and write its results into the simList
#'
#' Supports both calling conventions: a function with a `sim` argument is handed the
#' `simList` and expected to return it, while any other function has its arguments
#' resolved from the `simList` by name and its return value written back.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param verbose logical. Whether to report each object written to the `simList`.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname callDisturbanceFn
callDisturbanceFn <- function(sim, spec, verbose = TRUE) {
  if (isTRUE(spec$takesSim)) {
    result <- do.call(spec$fn, c(list(sim), spec$args))

    if (!inherits(result, "simList")) {
      ## Assigning a non-simList over the simulation state would discard the simulation.
      stop("Disturbance '", spec$name, "' uses a function that takes 'sim' as an ",
           "argument, so it is expected to return the simList. It returned an object of ",
           "class '", paste(class(result), collapse = "/"), "' instead.")
    }

    if (verbose) {
      ## A function given the simList modifies it directly, so there is no way to report
      ## which objects it changed; only that it ran.
      message("Biomass_genericDisturbance: disturbance '", spec$name,
              "' was given the simList and returned it. Objects it changed are not ",
              "listed individually, because a function of this kind writes to the ",
              "simList itself.")
    }

    return(invisible(result))
  }

  args <- resolveDisturbanceArgs(sim, spec)
  result <- do.call(spec$fn, args)

  applyDisturbanceResult(sim, spec, result, verbose = verbose)
}

#' Match a disturbance function's arguments to simList objects
#'
#' Arguments are matched by name, in order of precedence: the current simulation time, any
#' extra arguments given in the specification, then objects of the same name in the
#' `simList`. Anything still unmatched is left to the function's own default.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#'
#' @return a named list of arguments.
#'
#' @keywords internal
#' @rdname resolveDisturbanceArgs
resolveDisturbanceArgs <- function(sim, spec) {
  argNames <- names(formals(spec$fn))

  ## A function taking `...` cannot have its arguments matched reliably by name, since
  ## the names it accepts are not declared.
  argNames <- setdiff(argNames, "...")

  args <- list()

  ## Every disturbance function examined needs to know which timestep it is acting on,
  ## and none can obtain it without being told.
  if ("currentTime" %in% argNames) {
    args$currentTime <- time(sim)
  }

  simObjects <- ls(sim)

  for (argName in setdiff(argNames, names(args))) {
    if (argName %in% names(spec$args)) {
      next  # supplied explicitly below; the specification wins
    }
    if (argName %in% simObjects) {
      args[[argName]] <- sim[[argName]]
    }
    ## Otherwise left out, so the function's own default applies. Note that a default
    ## written in terms of `sim`, such as `cohortData = copy(sim$cohortData)`, cannot be
    ## evaluated from here: `sim` is not visible where the function is defined. Such
    ## arguments are matched by name above, which is why the simList is searched for
    ## every argument rather than only those the caller names.
  }

  ## Explicit arguments last, so they override anything found in the simList.
  args[names(spec$args)] <- spec$args

  args
}

#' Write a disturbance function's return value into the simList
#'
#' Every object returned is added to the `simList`. The module does not interpret the
#' return value; it only needs a name for each part of it.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param result whatever the disturbance function returned.
#' @param verbose logical. Whether to report each object written.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname applyDisturbanceResult
applyDisturbanceResult <- function(sim, spec, result, verbose = TRUE) {
  if (is.null(result)) {
    if (verbose) {
      message("Biomass_genericDisturbance: disturbance '", spec$name,
              "' returned NULL, so nothing was added to the simList.")
    }
    return(invisible(sim))
  }

  ## A simList returned by a function without a `sim` argument: accept it, since the
  ## intent is unambiguous.
  if (inherits(result, "simList")) {
    return(invisible(result))
  }

  isPlainList <- is.list(result) && !is.data.frame(result)

  if (isPlainList) {
    nms <- names(result)

    if (is.null(nms) || any(!nzchar(nms)) || any(is.na(nms))) {
      ## Guessing names for the unnamed elements could overwrite unrelated simulation
      ## objects, so this is an error rather than a partial assignment.
      stop("Disturbance '", spec$name, "' returned a list in which ",
           if (is.null(nms)) "no elements are named" else "some elements are unnamed",
           ". Each element's name is used as the simList object name, so every element ",
           "must be named. If the function is meant to return a single object rather ",
           "than a list of them, add 'returnsAs' to its disturbance specification.")
    }

    for (nm in nms) {
      sim[[nm]] <- result[[nm]]
      if (verbose) {
        message("Biomass_genericDisturbance: disturbance '", spec$name,
                "' wrote sim$", nm, " (", paste(class(result[[nm]]), collapse = "/"), ").")
      }
    }

    return(invisible(sim))
  }

  ## A single object: the specification must say what to call it.
  if (is.null(spec$returnsAs)) {
    stop("Disturbance '", spec$name, "' returned a single object of class '",
         paste(class(result), collapse = "/"), "' rather than a named list, so there is ",
         "no name to store it under in the simList. Add 'returnsAs' to its disturbance ",
         "specification, giving the simList object name to assign it to.")
  }

  sim[[spec$returnsAs]] <- result
  if (verbose) {
    message("Biomass_genericDisturbance: disturbance '", spec$name, "' wrote sim$",
            spec$returnsAs, " (", paste(class(result), collapse = "/"), ").")
  }

  invisible(sim)
}
