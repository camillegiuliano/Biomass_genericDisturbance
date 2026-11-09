# Biomass_genericDisturbance 0.0.0.9000

First working version.

## New

* Applies user-supplied disturbance functions to a simulation, with no disturbance logic
  built into the module. Functions and the events they follow are configured through the
  `disturbances` parameter, so one instance of the module can handle every disturbance
  type in a simulation.
* Each disturbance is scheduled to run immediately after the module event it follows, at a
  priority worked out from that event rather than one supplied by hand. The upstream
  event's schedule is re-read each time, so irregular and non-annual cadences are followed
  exactly rather than assumed.
* Two kinds of disturbance function are accepted: functions taking named arguments and
  returning a named list, and functions taking and returning the simulation itself.
* Every object written to the simulation is reported as a message, since the module
  declares no outputs and these messages are the only record of what it changed.
* Misconfigurations that would otherwise produce a complete but wrong simulation — an
  unknown upstream module, two disturbances scheduled at the same time and priority, a
  disturbance running before the event it follows, or a return value with no usable name —
  stop the simulation rather than warning.
* Can call a disturbance function once per element of a list, for planning modules that
  produce one disturbance layer per species. Set `loopOver` to the list's name, and use
  `loopArg`, `loopNameArg` or `argsFn` to pass each element, its name, or arguments built
  from them. Each call acts on the results of the previous one, so changes accumulate
  across the list.
* Lists `LandR` and `disturbanceMgmt` among its required packages, so that commonly used
  disturbance functions such as `FireDisturbance()` and `processPartialDist()` are
  installed and available to name in a disturbance specification. The module does not call
  either package itself.

## Known limitations

* The module declares no input or output objects, because they depend on the supplied
  functions. Downstream modules therefore cannot declare `expectsInput()` on objects it
  produces.
* The `triggerObj` check detects a missing or empty object, but cannot tell one written in
  the current timestep from one left over from an earlier one.
* The simulation must use a `"year"` time unit; the module stops otherwise rather than
  risk converting times incorrectly.

---

Drafted with assistance from Claude (Posit Assistant).
