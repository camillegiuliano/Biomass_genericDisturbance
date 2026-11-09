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
  if (is.null(spec[["triggerObj"]])) {
    return(TRUE)
  }

  if (!spec[["triggerObj"]] %in% ls(sim)) {
    if (verbose) {
      message("Biomass_genericDisturbance: skipping disturbance '", spec[["name"]], "' at time ",
              time(sim), " because its trigger object '", spec[["triggerObj"]],
              "' is not in the simList.")
    }
    return(FALSE)
  }

  trigger <- sim[[spec[["triggerObj"]]]]

  ## Only emptiness is tested, not content: this module cannot know what a non-empty
  ## trigger means for any particular disturbance. Note this detects an absent or empty
  ## object, but cannot tell an object written this timestep from one left over from an
  ## earlier one.
  isEmpty <- is.null(trigger) ||
    (is.atomic(trigger) && length(trigger) == 0L) ||
    (is.data.frame(trigger) && nrow(trigger) == 0L)

  if (isEmpty) {
    if (verbose) {
      message("Biomass_genericDisturbance: skipping disturbance '", spec[["name"]], "' at time ",
              time(sim), " because its trigger object '", spec[["triggerObj"]], "' is empty.")
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
  ## A looping specification calls the function once per element of a list, threading the
  ## simList through, so each call acts on the results of the one before.
  if (!is.null(spec[["loopOver"]])) {
    return(callDisturbanceFnLooped(sim, spec, verbose = verbose))
  }

  callDisturbanceFnOnce(sim, spec, verbose = verbose)
}

#' Call a disturbance function once per element of a list
#'
#' Some disturbance functions act on one part of a disturbance at a time -- one species'
#' harvest map, for example -- and must be called repeatedly, each call acting on the
#' results of the last. This runs that loop.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification, with a `loopOver` field.
#' @param verbose logical. Whether to report each iteration.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname callDisturbanceFnLooped
callDisturbanceFnLooped <- function(sim, spec, verbose = TRUE) {
  if (!spec[["loopOver"]] %in% ls(sim)) {
    stop("Disturbance '", spec[["name"]], "' is set to loop over sim$", spec[["loopOver"]],
         ", but there is no such object in the simList. Check the name given in ",
         "'loopOver', and that the module producing it runs before this disturbance.")
  }

  items <- sim[[spec[["loopOver"]]]]

  if (is.null(items) || length(items) == 0L) {
    if (verbose) {
      message("Biomass_genericDisturbance: disturbance '", spec[["name"]], "' has nothing to ",
              "loop over at time ", time(sim), ": sim$", spec[["loopOver"]], " is empty.")
    }
    return(invisible(sim))
  }

  ## Named elements are the usual case and carry meaning -- a list of harvest maps is
  ## named by species, and that name is often what the function needs. Positions are used
  ## only when there are no names.
  itemNames <- names(items)
  haveNames <- !is.null(itemNames) && all(nzchar(itemNames))

  if (!is.null(spec[["loopNameArg"]]) && !haveNames) {
    stop("Disturbance '", spec[["name"]], "' passes each element's name to the function's '",
         spec[["loopNameArg"]], "' argument, but sim$", spec[["loopOver"]],
         " has unnamed elements. Name every element, or drop 'loopNameArg'.")
  }

  if (verbose) {
    message("Biomass_genericDisturbance: disturbance '", spec[["name"]], "' looping over ",
            length(items), " element(s) of sim$", spec[["loopOver"]],
            if (haveNames) paste0(": ", paste(itemNames, collapse = ", ")) else "", ".")
  }

  for (i in seq_along(items)) {
    itemName <- if (haveNames) itemNames[[i]] else as.character(i)

    ## Each iteration is given the simList as left by the previous one, so that changes
    ## accumulate rather than each call overwriting the last one's work.
    sim <- callDisturbanceFnOnce(sim, spec, verbose = verbose,
                                 item = items[[i]], itemName = itemName)
  }

  invisible(sim)
}

#' Call a disturbance function once
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param verbose logical. Whether to report what was written to the `simList`.
#' @param item the current element, when looping. `NULL` otherwise.
#' @param itemName character. The current element's name, when looping. `NULL` otherwise.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname callDisturbanceFnOnce
callDisturbanceFnOnce <- function(sim, spec, verbose = TRUE, item = NULL, itemName = NULL) {
  loopArgs <- loopArguments(sim, spec, item = item, itemName = itemName)

  if (isTRUE(spec[["takesSim"]])) {
    result <- do.call(spec[["fn"]], c(list(sim), modifyList(spec[["args"]], loopArgs)))

    if (!inherits(result, "simList")) {
      ## Assigning a non-simList over the simulation state would discard the simulation.
      stop("Disturbance '", spec[["name"]], "' uses a function that takes 'sim' as an ",
           "argument, so it is expected to return the simList. It returned an object of ",
           "class '", paste(class(result), collapse = "/"), "' instead.")
    }

    if (verbose) {
      ## A function given the simList modifies it directly, so there is no way to report
      ## which objects it changed; only that it ran.
      message("Biomass_genericDisturbance: disturbance '", spec[["name"]],
              "' was given the simList and returned it. Objects it changed are not ",
              "listed individually, because a function of this kind writes to the ",
              "simList itself.")
    }

    return(invisible(result))
  }

  args <- resolveDisturbanceArgs(sim, spec, loopArgs = loopArgs)
  result <- do.call(spec[["fn"]], args)

  applyDisturbanceResult(sim, spec, result, verbose = verbose, itemName = itemName)
}

#' Build the arguments contributed by one loop iteration
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param item the current element of the list being looped over.
#' @param itemName character. The current element's name.
#'
#' @return a named list of arguments, empty when not looping.
#'
#' @keywords internal
#' @rdname loopArguments
loopArguments <- function(sim, spec, item = NULL, itemName = NULL) {
  if (is.null(spec[["loopOver"]]) || is.null(itemName)) {
    return(list())
  }

  args <- list()

  if (!is.null(spec[["loopArg"]])) {
    args[[spec[["loopArg"]]]] <- item
  }
  if (!is.null(spec[["loopNameArg"]])) {
    args[[spec[["loopNameArg"]]]] <- itemName
  }

  ## `argsFn` covers the case where an argument has to be *built* from the element rather
  ## than passed straight through -- for instance a one-row table naming the species the
  ## current raster belongs to, together with the current year.
  if (!is.null(spec[["argsFn"]])) {
    built <- spec[["argsFn"]](item = item, itemName = itemName, currentTime = time(sim),
                         sim = sim)
    if (!is.list(built) || (length(built) > 0L &&
                            (is.null(names(built)) || any(!nzchar(names(built)))))) {
      stop("Disturbance '", spec[["name"]], "' has an 'argsFn' that must return a named list ",
           "of arguments, but it returned ",
           if (!is.list(built)) paste0("an object of class '",
                                       paste(class(built), collapse = "/"), "'")
           else "a list with unnamed elements", ".")
    }
    args <- modifyList(args, built)
  }

  args
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
resolveDisturbanceArgs <- function(sim, spec, loopArgs = list()) {
  argNames <- names(formals(spec[["fn"]]))

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
    if (argName %in% names(spec[["args"]]) || argName %in% names(loopArgs)) {
      next  # supplied explicitly below
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

  ## Explicit arguments last, so they override anything found in the simList. The current
  ## loop element wins over everything, since it is the whole point of the iteration.
  args[names(spec[["args"]])] <- spec[["args"]]
  args[names(loopArgs)] <- loopArgs

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
#' @param itemName character. The current element's name, when looping. `NULL` otherwise.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname applyDisturbanceResult
applyDisturbanceResult <- function(sim, spec, result, verbose = TRUE, itemName = NULL) {
  ## When looping, name the element in messages so each iteration is distinguishable.
  specLabel <- if (is.null(itemName)) spec[["name"]] else paste0(spec[["name"]], " [", itemName, "]")
  if (is.null(result)) {
    if (verbose) {
      message("Biomass_genericDisturbance: disturbance '", specLabel,
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
      stop("Disturbance '", spec[["name"]], "' returned a list in which ",
           if (is.null(nms)) "no elements are named" else "some elements are unnamed",
           ". Each element's name is used as the simList object name, so every element ",
           "must be named. If the function is meant to return a single object rather ",
           "than a list of them, add 'returnsAs' to its disturbance specification.")
    }

    for (nm in nms) {
      sim[[nm]] <- result[[nm]]
      if (verbose) {
        message("Biomass_genericDisturbance: disturbance '", specLabel,
                "' wrote sim$", nm, " (", paste(class(result[[nm]]), collapse = "/"), ").")
      }
    }

    return(invisible(sim))
  }

  ## A single object: the specification must say what to call it.
  if (is.null(spec[["returnsAs"]])) {
    stop("Disturbance '", spec[["name"]], "' returned a single object of class '",
         paste(class(result), collapse = "/"), "' rather than a named list, so there is ",
         "no name to store it under in the simList. Add 'returnsAs' to its disturbance ",
         "specification, giving the simList object name to assign it to.")
  }

  sim[[spec[["returnsAs"]]]] <- result
  if (verbose) {
    message("Biomass_genericDisturbance: disturbance '", specLabel, "' wrote sim$",
            spec[["returnsAs"]], " (", paste(class(result), collapse = "/"), ").")
  }

  invisible(sim)
}
