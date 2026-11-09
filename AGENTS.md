# Biomass_genericDisturbance — agent guide

Adapted from the `PredictiveEcology/AI` template (https://github.com/PredictiveEcology/AI).
Scope: this one SpaDES module folder. Sibling repos under `~/GitHub` (e.g. `LandRCBM`,
`spadesCBM`) are **outside this folder and read-only** unless the user grants write access
for a specific path — re-ask each time.

## What this module is

A SpaDES module, currently a **bare `newModule()` scaffold**: nothing has been
implemented. The disturbance process it will represent is not yet specified. See "Open
decisions" below — do not fill these in by guessing.

Scaffold contents: `Biomass_genericDisturbance.R` (metadata, `doEvent`, stub `Init` and
`.inputObjects`), `Biomass_genericDisturbance.Rmd` (manual), `tests/unitTests.R` +
`tests/testthat/`, `data/CHECKSUMS.txt`, `NEWS.md`, `README.md`, `citation.bib`,
`LICENSE.md`, `.github/workflows/render-module-rmd.yaml`. Not yet a git repository.

## Where the knowledge lives

- **SpaDES toolkit** (framework): https://spades.predictiveecology.org/. A simulation is a
  `simList`; `simInit()` builds it, `spades()` runs it. Modules communicate **only**
  through named `simList` objects — one module's `createsOutput` is another's
  `expectsInput`. That name is a contract.
- **LandR** (forest-landscape model system built *on* the toolkit) is the likely family
  here, given the `Biomass_*` name. LandR-specific mechanics (cohort data, pixel groups,
  species/ecoregion) differ from toolkit-general concerns; keep the direction of
  dependency straight when tracing a change's blast radius.
- **Skills** (install via `/plugin`, marketplace `PredictiveEcology/AI`):
  `spades-module-anatomy`, `spades-module-development`, `landr-code-development`,
  `testing`, `code-documentation`. LandR skills win over SpaDES skills where topics
  overlap. Get these before writing new ones.

## Working rules

### The user owns the science

The user is the authority on the ecological process being modelled; the assistant
contributes programming knowledge. If a request is vague about the phenomenon ("add the
disturbance effect"), **ask what process, at what spatial and temporal scale, with what
inputs and outputs** before writing code. Do not invent ecological behaviour.

Flag — do not silently implement — anything that contradicts process logic: timestep/rate
mismatches, unit or per-pixel/per-hectare confusion, impossible states (negative biomass
or age), or biomass/carbon created or destroyed without a corresponding flux.

### Changes to existing code need approval

Explain what will change, why, and the downstream effects (including on other modules via
shared `simList` objects), then wait. Prefer the smallest change that does the job; do not
refactor opportunistically. Fix defects where they live, naming the root cause with
`file:line` — a workaround at the call site is not a fix, and never stack a patch on a
patch.

### Editing files

- **Edit `.R` files with `bash` (heredoc) or an in-place `python3` script — never the
  `edit`/`write` editor tools.** Format-on-save can reformat the whole file and produce
  large unwanted diffs. Markdown, YAML, and `.Rmd` are fine through the editor tools.
- **After any change, verify the diff**: `git diff <file>` and `git diff --stat`, and
  confirm only what was asked changed. For documentation-only R edits,
  `git diff <file> | grep '^[+-]' | grep -v '^[+-]#'` should return nothing.
- Do not reformat or restyle unasked. Match the file's existing brace style, operator
  spacing, and blank-line layout.
- Never `git commit` unless explicitly told to. Stage specific paths; never `git add -A`
  or `git add .`. Dry-run destructive commands first.

### Module anatomy

- `Biomass_genericDisturbance.R` holds the `defineModule()` metadata, the
  `doEvent.Biomass_genericDisturbance()` dispatcher, and the event functions the dispatcher
  calls directly. **Smaller helpers go in `R/`** — a closure defined in a large environment
  drags that environment into memory.
- `init` is run automatically and is the **only** event that may schedule another event for
  the first time; every other event may only reschedule itself. A module may schedule only
  its **own** events.
- `.inputObjects` runs automatically during `simInit()` and is never called explicitly.
  Provide sensible defaults there, guarded with `suppliedElsewhere("obj", sim)`; retrieve a
  declared source with `extractURL("obj", sim)` and pass it to `prepInputs()`.
- Every `defineParameter`, `expectsInput`, and `createsOutput` needs a `desc` stating
  meaning, **units**, and structure (e.g. the columns of a `data.table`).
- A data-prep or calibration module may need only `init`. Do not add recurring events, or
  split into more events or modules, out of habit — each carries real overhead.
- When adding an input or output, update the metadata block, the code that reads/writes it,
  the `.Rmd`, and `data/CHECKSUMS.txt` if a data file changed. Bump `version` and add a
  `NEWS.md` entry for behavioural changes.

### Coding conventions

- `data.table` and the base pipe `|>` (not `%>%`), for performance and memory at landscape
  scale.
- `P(sim)$x` for parameters, `sim$objName` for shared state, `mod` for module-local state.
  Parameters and module functions are module-scoped; `simList` objects are global.
- `reproducible::Cache()` and `prepInputs()` for expensive or downloaded steps.
  `prepInputs()` does not cache on its own — wrap it in `Cache()`. Generally **do not cache
  stochastic events**.
- **Memory-efficient R:** avoid `Map()`, `do.call()`, and the `apply` family — use `purrr`
  (`map()`, `map2()`, `pmap()`, `walk()`). Avoid `as.formula()` (it captures its
  environment); build a `call()` instead.
- Order same-time events with named `eventPriority` variables at the top of `doEvent`, not
  magic numbers. Recurring-event intervals and per-step rates must match the module's
  `timeunit`.
- `message()` for progress, `warning()` for a relaxed assumption or fallback, `stop()` for
  conditions that make continuing unsafe. **State which was chosen and why**, and flag the
  tradeoff — a `warning()` is easy to miss in a long batch or HPC run.
- Check for an existing function before writing a new one: this module, other modules, the
  project's packages, then CRAN. **Flag any new `reqdPkgs` dependency before adding it**,
  with the reason and whether an existing dependency could serve.
- Ask whether logic belongs in the **module** (specific to its behaviour) or a **package**
  (used in more than one place). Prefer a package when it will have more than one caller,
  so there is a single source of truth.

### Testing

Do not create tests unless asked. When asked, follow `tests/testthat/` conventions: small
fixed in-memory `data.table`s and tiny rasters with known expected values rather than
random data; if randomness is unavoidable, set a seed (a random integer, not 42). Reach
module-internal functions via `sim$.mods$Biomass_genericDisturbance$<fn>`. Prompt the user
to run integration tests after any change that crosses a module or package boundary.

### CI

Use the shared composite actions at the shared ref, `PredictiveEcology/actions@main`. Never
hand-roll or hand-patch a per-repo copy of setup the shared action already does. When one
fails, check the pin before blaming the action.

### Writing for people

Documentation targets **ecologists and other scientists, not software engineers**:
minimize jargon, define any technical term you must use, and match the existing file's
tone and markup rather than restyling it. PRs, issues, and comments lead with the outcome
and are usually three to six sentences — plain English, no "comprehensive", "robust", or
"leverage", no headers or bullets in a short comment, and never close by calling the work
"ready".

Comment the *why*, not the *what*; in a pipeline, put the comment on the line before the
step. `man/` and `NAMESPACE` are generated — never hand-edited. Use `@keywords internal`
for non-exported functions; ask the user first before adding it to an exported one.

### Session hygiene

Keep a durable session log from the point real work starts, updated at milestones — a
decision, a settled question, an abandoned direction — with **what was ruled out** and
**the exact next step**. Verify claims rather than inferring them: "probably flaky" is a
hypothesis, not a finding. When the session grows long, suggest `/compact` and
`/savememory`.

## Open decisions (ask, do not assume)

1. What the disturbance does to the landscape — cohort mortality, stand-replacing reset,
   partial biomass removal with transfers to a carbon pool, or something else.
2. Where the spatial pattern comes from — externally supplied rasters/polygons, simulated
   from a rate or probability map, or an object produced by another module.
3. Which chain it joins (LandRCBM, a standard `Biomass_core` chain, or standalone) and
   which modules sit on either side, so shared object names follow their patterns.
4. Module `description`, `keywords`, `authors`, `timeunit`, license, and `citation.bib`.
5. Whether to make this folder a git repository.

---

Drafted with assistance from Claude (Posit Assistant).
