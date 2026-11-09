## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.

## This module is deliberately generic: it contains no disturbance logic of its own.
## The user supplies, via the `disturbances` parameter, one or more "specs" that each say
## "run function F after module M's event E". See the .Rmd manual for the full format.
##
## Because the functions are user-supplied, the objects this module reads and writes are
## not known until run time. That is why `inputObjects` and `outputObjects` below are
## empty -- the module's real interface is the `disturbances` parameter, and it is
## validated during the `init` event instead of by `simInit()`.

defineModule(sim, list(
  name = "Biomass_genericDisturbance",
  description = paste(
    "Applies user-supplied disturbance functions to simulation objects, scheduling each",
    "one to run immediately after the module that generates its disturbance information.",
    "The module holds no disturbance logic itself: the user names a function and the",
    "upstream module and event it should follow, and this module schedules the function,",
    "calls it, and adds whatever it returns to the simList. Any number of disturbance",
    "types (harvest, fire, insect outbreak, thermokarst, ...) can be handled by a single",
    "instance of this module in one simulation. The `Biomass_` prefix reflects the suite",
    "of modules it is typically used alongside; the module itself has no LandR dependency."
  ),
  keywords = c("disturbance", "generic", "scheduling", "event", "SpaDES"),
  authors = structure(list(list(given = c("First", "Middle"), family = "Last", role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(Biomass_genericDisturbance = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "Biomass_genericDisturbance.Rmd"),
  ## `LandR` and `disturbanceMgmt` are listed so that the disturbance functions users
  ## most often reach for are installed and available. The module itself calls neither:
  ## it works with any function supplied through the `disturbances` parameter.
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 3.2.1.9053)",
                  "data.table",
                  "PredictiveEcology/LandR@development (>= 1.2.0.9064)",
                  "camillegiuliano/disturbanceMgmt@main (>= 0.0.1.9001)"),
  parameters = bindrows(
    defineParameter(
      "disturbances", "list", list(), NA, NA,
      paste(
        "The module's main input: a list of disturbance specifications, each a named list",
        "describing one disturbance function and the upstream event it follows. Supply it",
        "from the project's global script. Elements of each specification:",
        "`fn` (required) the function to run, given either as a function object or as a",
        "character string such as \"pkg::fun\";",
        "`name` (required) a unique name, used as the event name and in messages;",
        "`after` (required) the name of the upstream module to follow;",
        "`afterEvent` (required) which of that module's events to follow;",
        "`offset` (default 0.1) how far below the upstream event's priority to run,",
        "where a smaller number means sooner; lower priority values run first;",
        "`triggerObj` (optional) the name of a simList object that must be present and",
        "non-empty for the function to be called in a given timestep; when NULL the",
        "function is called every time the upstream event runs;",
        "`returnsAs` (optional) the simList object name to assign an unnamed return value",
        "to; required only when the function returns a single object rather than a named",
        "list or a simList;",
        "`args` (optional) a named list of extra arguments passed to the function, which",
        "take precedence over objects of the same name found in the simList."
      )
    ),
    defineParameter(
      "verboseDisturbance", "logical", TRUE, NA, NA,
      paste(
        "If TRUE (the default), report each disturbance call and every object written to",
        "the simList as a message. Because this module declares no output objects, these",
        "messages are the only record of what it changed, so turning them off is not",
        "recommended."
      )
    ),
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should caching of events or module be used? Disturbances are",
                          "often stochastic, so caching this module's events is generally",
                          "not appropriate."))
  ),
  inputObjects = bindrows(
    ## Intentionally empty: the objects read are determined by the user-supplied
    ## functions in the `disturbances` parameter, and cannot be known in advance.
    ## Validated during `init` instead. See the .Rmd manual.
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bindrows(
    ## Intentionally empty, for the same reason: this module writes whatever the
    ## user-supplied functions return. Note that downstream modules therefore cannot
    ## declare `expectsInput()` on objects produced here.
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

doEvent.Biomass_genericDisturbance = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)

      ## The first occurrence of each disturbance event cannot be scheduled here.
      ## simInit() schedules every module's `init` at start(sim) with the same priority,
      ## so an upstream module whose `init` has not yet run has not scheduled its events
      ## yet, and there is nothing in the queue to follow. The `bootstrap` event below
      ## runs at the lowest priority, i.e. after every other module's `init`, by which
      ## point the queue is complete.
      sim <- scheduleEvent(sim, start(sim), "Biomass_genericDisturbance",
                           "bootstrap", .lowest())
    },
    bootstrap = {
      sim <- Bootstrap(sim)
    },
    ## All disturbance events share one handler; the event name identifies the spec.
    ## Note the assignment: the simList's objects are held in an environment and so are
    ## shared, but its event queue is not, so a scheduled event is lost unless the
    ## returned simList is kept.
    sim <- RunDisturbance(sim, eventType)
  )
  return(invisible(sim))
}

#' Validate the disturbance specifications
#'
#' Runs during the `init` event. Checks the simulation's time unit, then validates and
#' normalizes every disturbance specification, storing the result in `mod$specs` for the
#' `bootstrap` and disturbance events to use.
#'
#' @param sim a `simList` object.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname Init
Init <- function(sim) {
  ## This module reads upstream event times from `events(sim)` and passes them straight to
  ## `scheduleEvent()`. Those two interpret times in different units -- the simulation's
  ## time unit and the calling module's time unit respectively -- so they must agree.
  ## They do when both are "year". If they disagree, nothing errors: the simulation runs
  ## and the disturbances simply happen in the wrong years, which is invisible in the
  ## output. Hence a hard stop rather than a conversion.
  simUnit <- SpaDES.core::timeunit(sim)
  if (!identical(simUnit, "year")) {
    stop("Biomass_genericDisturbance assumes annual time ('year'), but this simulation's ",
         "time unit is '", simUnit, "'. Disturbance events are scheduled by mirroring ",
         "upstream events read from the event queue, and mixing time units would place ",
         "them in the wrong timestep without raising an error. Either run the simulation ",
         "with timeunit 'year', or adapt this module to convert units explicitly.")
  }

  mod$specs <- validateDisturbanceSpecs(P(sim)$disturbances, sim = sim)

  if (length(mod$specs) == 0L) {
    message("Biomass_genericDisturbance: no disturbances were supplied via the ",
            "'disturbances' parameter, so this module will do nothing. Supply a list of ",
            "disturbance specifications to use it; see the module manual for the format.")
  } else {
    message("Biomass_genericDisturbance: validated ", length(mod$specs),
            " disturbance specification(s): ",
            paste(names(mod$specs), collapse = ", "), ".")
  }

  return(invisible(sim))
}

#' Schedule the first occurrence of each disturbance event
#'
#' Runs once, at `start(sim)`, at the lowest priority so that every other module's `init`
#' has already run and the event queue is complete. Each specification's first occurrence
#' is derived from the upstream event it follows.
#'
#' @param sim a `simList` object.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname Bootstrap
Bootstrap <- function(sim) {
  for (specName in names(mod$specs)) {
    sim <- scheduleNextDisturbance(sim, mod$specs[[specName]], isFirst = TRUE)
  }
  return(invisible(sim))
}

#' Run one disturbance and schedule its next occurrence
#'
#' The shared handler for every disturbance event. Checks that the event is still ordered
#' correctly against the upstream event, calls the user-supplied function unless the
#' specification's trigger object is absent, writes the results into the `simList`, then
#' schedules the next occurrence by mirroring the upstream event.
#'
#' @param sim a `simList` object.
#' @param specName character. The name of the disturbance specification, which is also the
#'   event name.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname RunDisturbance
RunDisturbance <- function(sim, specName) {
  spec <- mod$specs[[specName]]

  if (is.null(spec)) {
    warning(noEventWarning(sim))
    return(invisible(sim))
  }

  verbose <- isTRUE(P(sim)$verboseDisturbance)

  ## Re-check ordering every time rather than once, in case an upstream module changes
  ## the priority of its own events part-way through a simulation.
  checkDisturbanceLadder(sim, spec)

  if (disturbanceTriggered(sim, spec, verbose = verbose)) {
    if (verbose) {
      message("Biomass_genericDisturbance: running disturbance '", spec$name,
              "' at time ", time(sim), ", after ", spec$after, "'s '",
              spec$afterEvent, "' event.")
    }
    sim <- callDisturbanceFn(sim, spec, verbose = verbose)
  }

  sim <- scheduleNextDisturbance(sim, spec, isFirst = FALSE)

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  ## This module has no default inputs to create: everything it reads is determined by the
  ## user-supplied functions named in the `disturbances` parameter, and is expected to be
  ## created by the upstream disturbance modules. Validation happens during `init`.
  return(invisible(sim))
}
