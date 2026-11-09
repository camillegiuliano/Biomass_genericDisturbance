# Biomass_genericDisturbance

A SpaDES module that applies **your** disturbance function at the right point in a
simulation.

Most disturbance work happens in two stages: one module decides where and when something
happened (which stands were cut, which pixels burned), and a second decides what that does
to the forest. This module handles the second stage for *any* disturbance. You tell it, in
your global script, "run function `F` immediately after module `M`'s event `E`", and it
schedules the function, calls it, and adds whatever it returns to the simulation.

One instance can look after every disturbance in a simulation — harvest, fire, insect
outbreak, thermokarst — because none of them is built into it. The module contains no
disturbance science of its own.

```r
simInit(
  modules = list("myHarvestModule", "Biomass_genericDisturbance"),
  params = list(Biomass_genericDisturbance = list(
    disturbances = list(
      list(fn         = myHarvestEffectFunction,
           name       = "harvestDist",
           after      = "myHarvestModule",
           afterEvent = "harvest",
           triggerObj = "rstCurrentHarvest")
    )
  ))
)
```

The `Biomass_` prefix reflects the suite of modules this is typically used alongside.

The module's own machinery needs only `SpaDES.core` and `data.table`. It additionally
installs [`LandR`](https://github.com/PredictiveEcology/LandR) and
[`disturbanceMgmt`](https://github.com/camillegiuliano/disturbanceMgmt), which supply the
disturbance functions most often used with it — `FireDisturbance()`,
`PeatlandThermokarst()`, `processPartialDist()` and others. The module never calls them
itself, and works equally well with functions of your own.

Full documentation, including the function contract, how event ordering is worked out, and
the module's limitations, is in
[`Biomass_genericDisturbance.Rmd`](Biomass_genericDisturbance.Rmd).

---

Everything in this repository was drafted with assistance from Claude (Posit Assistant).
