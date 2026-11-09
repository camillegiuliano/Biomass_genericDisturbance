#' Find an upstream event in the event queue
#'
#' Looks up the event a disturbance specification follows, so the disturbance can be
#' scheduled to run immediately after it. This is what lets the module mirror whatever
#' cadence the upstream module uses -- annual, decadal, or irregular -- instead of
#' assuming one.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param from numeric. Only events scheduled at or after this time are considered.
#'   Defaults to the current simulation time.
#'
#' @return a list with `found` (logical), `moduleScheduled` (logical: whether the upstream
#'   module has any events in the queue at all), and, when `found` is `TRUE`, the upstream
#'   event's `eventTime` and `eventPriority`.
#'
#' @keywords internal
#' @rdname findUpstreamEvent
findUpstreamEvent <- function(sim, spec, from = time(sim)) {
  queue <- upstreamQueue(sim)

  ## Distinguishing "this module is not in the simulation, or the name is misspelled"
  ## from "this module is in the simulation but has not scheduled this event yet" matters:
  ## the first is a configuration error, the second is normal for a module that starts
  ## later than the others.
  moduleScheduled <- any(queue$moduleName == spec[["after"]])

  matches <- queue[queue$moduleName == spec[["after"]] &
                     queue$eventType == spec[["afterEvent"]] &
                     !is.na(queue$eventTime) &
                     queue$eventTime >= from, ]

  if (nrow(matches) == 0L) {
    return(list(found = FALSE, moduleScheduled = moduleScheduled))
  }

  ## The soonest occurrence; among simultaneous ones, the earliest to run.
  ord <- order(matches$eventTime, matches$eventPriority)
  first <- matches[ord[1], ]

  list(
    found = TRUE,
    moduleScheduled = moduleScheduled,
    eventTime = first$eventTime,
    eventPriority = first$eventPriority
  )
}

#' Read the event queue in this module's time unit
#'
#' @param sim a `simList` object.
#'
#' @return a `data.frame` of the scheduled events, with times expressed in years.
#'
#' @keywords internal
#' @rdname upstreamQueue
upstreamQueue <- function(sim) {
  ## The unit is given explicitly rather than relying on the default. `events()` defaults
  ## to the *simulation's* time unit, whereas `scheduleEvent()` interprets a bare time in
  ## the *calling module's* unit. Since times read here are passed straight to
  ## `scheduleEvent()`, they must be in this module's unit, which `Init()` has already
  ## confirmed is "year".
  queue <- SpaDES.core::events(sim, unit = "year")

  if (NROW(queue) == 0L) {
    return(data.frame(eventTime = numeric(0), moduleName = character(0),
                      eventType = character(0), eventPriority = numeric(0),
                      stringsAsFactors = FALSE))
  }

  data.frame(
    eventTime = as.numeric(queue$eventTime),
    moduleName = as.character(queue$moduleName),
    eventType = as.character(queue$eventType),
    eventPriority = as.numeric(queue$eventPriority),
    stringsAsFactors = FALSE
  )
}

#' Schedule a disturbance event's next occurrence
#'
#' Mirrors the upstream event: the disturbance is scheduled at the same time as the next
#' occurrence of the event it follows, at a slightly lower priority so that it runs
#' immediately afterwards.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#' @param isFirst logical. Whether this is the specification's first occurrence, scheduled
#'   during the `bootstrap` event.
#'
#' @return the `simList`, invisibly.
#'
#' @keywords internal
#' @rdname scheduleNextDisturbance
scheduleNextDisturbance <- function(sim, spec, isFirst = FALSE) {
  verbose <- isTRUE(P(sim)$verboseDisturbance)

  ## On later passes the occurrence just handled must be excluded, or the module would
  ## reschedule itself at the same time forever.
  from <- if (isFirst) time(sim) else time(sim) + .Machine$double.eps^0.5

  upstream <- findUpstreamEvent(sim, spec, from = from)

  ## `bootstrap` runs at the lowest priority, so an upstream event scheduled for the very
  ## first timestep has already run by the time it fires, and has left the queue. Its
  ## results are sitting in the simList unused, so the disturbance is placed at the
  ## current time to pick them up. Without this, the first timestep's disturbance is
  ## silently skipped.
  if (isFirst) {
    alreadyRan <- upstreamRanThisTimestep(sim, spec)
    if (!is.null(alreadyRan) && (!upstream$found || upstream$eventTime > time(sim))) {
      upstream <- list(found = TRUE, moduleScheduled = TRUE,
                       eventTime = time(sim), eventPriority = alreadyRan)
    }
  }

  if (!upstream$found) {
    if (!upstream$moduleScheduled) {
      if (isFirst) {
        ## Fatal: a module that is absent from the queue entirely at bootstrap time, when
        ## every module's `init` has run, is either not in the simulation or is misnamed.
        ## Falling back to a default schedule here would apply the disturbance at a time
        ## unrelated to the data it depends on.
        stop("Disturbance '", spec[["name"]], "' is set to follow the '", spec[["afterEvent"]],
             "' event of module '", spec[["after"]], "', but that module has no events in the ",
             "queue, even though every module's init event has now run. Check that '",
             spec[["after"]], "' is spelled correctly and is included in this simulation's ",
             "list of modules.")
      }
      if (verbose) {
        message("Biomass_genericDisturbance: module '", spec[["after"]], "' has no further ",
                "events scheduled, so disturbance '", spec[["name"]], "' will not run again.")
      }
      return(invisible(sim))
    }

    ## The module is present but this event is not scheduled yet, which is normal for a
    ## module that begins later in the simulation. Retry next timestep rather than
    ## failing, since the event may yet appear.
    if (isFirst) {
      if (verbose) {
        message("Biomass_genericDisturbance: module '", spec[["after"]], "' is present but has ",
                "not yet scheduled its '", spec[["afterEvent"]], "' event, so disturbance '",
                spec[["name"]], "' cannot be placed yet. Will check again next year.")
      }
      retryTime <- time(sim) + 1
      attr(retryTime, "unit") <- "year"
      sim <- scheduleEvent(sim, retryTime, "Biomass_genericDisturbance",
                           "bootstrap", .lowest())
      return(invisible(sim))
    }

    if (verbose) {
      message("Biomass_genericDisturbance: module '", spec[["after"]], "' has no further '",
              spec[["afterEvent"]], "' events scheduled, so disturbance '", spec[["name"]],
              "' will not run again.")
    }
    return(invisible(sim))
  }

  ## The time read from the queue is absolute, but `scheduleEvent()` treats a plain
  ## number as an offset from the start of the simulation, adding it to the current time.
  ## Marking the unit tells it the time is already absolute. Without this, each
  ## disturbance drifts later and later: scheduling "year 2" while in year 1 would
  ## otherwise land in year 3.
  nextTime <- upstream$eventTime
  attr(nextTime, "unit") <- "year"
  nextPriority <- upstream$eventPriority + spec[["offset"]]

  if (verbose) {
    message("Biomass_genericDisturbance: scheduling disturbance '", spec[["name"]],
            "' for time ", nextTime, " at priority ", nextPriority,
            ", following '", spec[["after"]], "' event '", spec[["afterEvent"]],
            "' at priority ", upstream$eventPriority, ".")
  }

  sim <- scheduleEvent(sim, nextTime, "Biomass_genericDisturbance",
                       spec[["name"]], nextPriority)

  return(invisible(sim))
}

#' Check that a disturbance is still ordered after its upstream event
#'
#' Run each time a disturbance fires, in case an upstream module changes the priority of
#' its own events part-way through a simulation.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#'
#' @return `NULL`, invisibly. Called for the error it may raise.
#'
#' @keywords internal
#' @rdname checkDisturbanceLadder
checkDisturbanceLadder <- function(sim, spec) {
  queue <- upstreamQueue(sim)

  ## Only events at this timestep matter: the question is whether the upstream event ran
  ## before this one, now. An upstream event still sitting in the queue at the current
  ## time with a priority at or below this disturbance's own means it has not run yet.
  current <- queue[queue$moduleName == spec[["after"]] &
                     queue$eventType == spec[["afterEvent"]] &
                     !is.na(queue$eventTime) &
                     abs(queue$eventTime - time(sim)) < .Machine$double.eps^0.5, ]

  if (nrow(current) == 0L) {
    return(invisible(NULL))
  }

  stop("Disturbance '", spec[["name"]], "' is running at time ", time(sim),
       ", but the '", spec[["afterEvent"]], "' event of module '", spec[["after"]],
       "' is still scheduled at this same time and has not run yet. The disturbance ",
       "would be applied to out-of-date information. This usually means '", spec[["after"]],
       "' changed the priority of its own events during the simulation. Adjust the ",
       "'offset' of this disturbance specification.")
}

#' Find an upstream event that has already run in the current timestep
#'
#' Used only when scheduling a disturbance for the first time, to catch an upstream event
#' that ran before the `bootstrap` event and has therefore left the queue.
#'
#' @param sim a `simList` object.
#' @param spec a validated disturbance specification.
#'
#' @return the priority the upstream event ran at, or `NULL` if it has not run in this
#'   timestep.
#'
#' @keywords internal
#' @rdname upstreamRanThisTimestep
upstreamRanThisTimestep <- function(sim, spec) {
  done <- SpaDES.core::completed(sim, unit = "year")

  ## `completed()` is empty when options(spades.keepCompleted = FALSE), which is
  ## occasionally set for very long runs. The disturbance then simply waits for the
  ## upstream event's next occurrence rather than running in the first timestep.
  if (NROW(done) == 0L || is.null(done$eventTime)) {
    return(NULL)
  }

  matches <- done[done$moduleName == spec[["after"]] &
                    done$eventType == spec[["afterEvent"]] &
                    !is.na(done$eventTime) &
                    abs(as.numeric(done$eventTime) - time(sim)) < .Machine$double.eps^0.5, ]

  if (NROW(matches) == 0L) {
    return(NULL)
  }

  as.numeric(matches$eventPriority[NROW(matches)])
}
