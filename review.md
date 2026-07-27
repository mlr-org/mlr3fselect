# Critical code review — mlr3fselect 1.6.0.9000

Scope: full package (`R/`, `tests/`, `inst/`, docs, `DESCRIPTION`, `NEWS.md`) at `5a4475c7`.
Every "Confirmed" finding below was reproduced in a live R session against `devtools::load_all()`.

---

## Summary

The architecture is sound and the bbotk/mlr3 integration is idiomatic. What is not sound is the
**numeric correctness of the optimizers and the result post-processing**. Four separate places
compute "the best thing" and only two of them respect `codomain$direction`. The result is that
`fs("rfecv")` — a headline feature with its own man page — **systematically selects the worst
number of features for every minimizing measure**, which is every default measure in mlr3
(`classif.ce`, `regr.mse`). It ships that way today because *every single rfecv test uses a
maximizing dummy measure*. That is not bad luck; that is a test suite that cannot see the bug class
it most needs to catch.

Beyond that: two documented parameter combinations crash with `Please report to the data.table
issues tracker`, `EnsembleFSResult$pareto_front()` returns dominated points and silently drops real
ones, `extract_inner_fselect_results()` corrupts the objects it reads, and the SVM-RFE guard
happily accepts the exact configuration its own documentation says is unsupported.

The pervasive theme is **silent wrongness over loud failure**. Almost none of these bugs throw. They
return a plausible-looking `data.table` with the wrong numbers in it.

**Verdict: Request Changes.**

---

## Critical issues (Blocking)

### 1. `fs("rfecv")` selects the worst feature-set size for any minimizing measure

`R/FSelectorBatchRFECV.R:167-169`

```r
aggr = archive$data[, list("y" = mean(unlist(.SD))), by = "batch_nr", .SDcols = archive$cols_y]
best_batch = aggr[order(get("y"), decreasing = TRUE), head(.SD, 1)]$batch_nr
```

`decreasing = TRUE` is hardcoded. `archive$data[[cols_y]]` holds the **raw** measure value;
`codomain$direction` is never consulted. For `minimize = TRUE` measures this picks the batch with
the *highest* mean error.

Compare `ArchiveBatchFSelect$best()` (`R/ArchiveBatchFSelect.R:187`), which gets it right:
`y = tab[[self$cols_y]] * -self$codomain$direction`. Two implementations of the same concept,
one of them wrong.

**Confirmed.** `tsk("sonar")` (8 features), `lrn("classif.rpart")`, `rsmp("cv", folds = 3)`,
`msr("classif.ce")`:

| batch | mean classif.ce |
|-------|-----------------|
| 3     | 0.4038  ← correct choice |
| ...   | ... |
| 7     | 0.4709  ← **what the code picks** |

`codomain$direction` was `1` (minimize) throughout.

**Why it survived:** every rfecv test uses `msr("dummy")`, and `MeasureDummy`
(`inst/testthat/helper_misc.R:41`) defaults to `minimize = FALSE`. The test suite exercises
exactly the one direction where the bug is invisible. See also *Test coverage* below.

**Fix:**

```r
aggr = archive$data[, list(y = mean(unlist(.SD))), by = "batch_nr", .SDcols = archive$cols_y]
best_batch = aggr[which_max(y * -archive$codomain$direction, ties_method = "first")]$batch_nr
```

---

### 2. `fs("rfecv", recursive = FALSE)` crashes with an internal data.table error

`R/FSelectorBatchRFE.R:280`

```r
set(archive$data, archive$n_evals, "importance", map(importances, function(x) x[seq(j)]))
```

`archive$n_evals` is a single row index, but with `folds > 1` (which is exactly how RFECV calls
`rfe_workhorse`) `importances` has `folds` elements. The recursive branch immediately above uses
`archive$data[list(archive$n_batch), "importance" := importances, on = "batch_nr"]` and handles
this correctly; the non-recursive branch does not.

**Confirmed:**

```
> fselect(fs("rfecv", recursive = FALSE, n_features = 2, feature_number = 1), ...)
Error: Internal error in memrecycle: recycle length error not caught earlier.
slen=3 len=1. Please report to the data.table issues tracker.
```

`recursive` is a documented control parameter of `mlr_fselectors_rfecv`. Shipping a documented
parameter that produces an *"please report to data.table"* message is the worst possible failure
mode: the user files the bug against the wrong project.

**Fix:** mirror the recursive branch —
`archive$data[list(archive$n_batch), "importance" := map(importances, function(x) x[seq(j)]), on = "batch_nr"]`.

---

### 3. `fs("rfe")` / `fs("rfecv")` crash when `store_benchmark_result = FALSE`

`R/FSelectorBatchRFE.R:243, 270`

```r
uhashes = archive$data[list(archive$n_batch), "uhash", on = "batch_nr"][[1]]
```

The `uhash` column only exists when `store_benchmark_result = TRUE`.

**Confirmed:**

```
> fselect(fs("rfe", n_features = 2), task, lrn("classif.rpart"), rsmp("holdout"),
+         msr("classif.ce"), store_benchmark_result = FALSE)
Error: Internal error in [.data.table: column(s) not found: uhash.
Please report to the data.table issues tracker.
```

Again a user-facing flag combination surfacing as an internal data.table bug report request.

Related: the `"requires_model"` property machinery
(`R/FSelectorBatch.R:84`, `R/ObjectiveFSelectBatch.R:82`) forces `store_models`, but nothing forces
`store_benchmark_result`, even though the RFE workhorse cannot function without the benchmark
result.

**Fix:** in `FSelectorBatch$optimize()`, when `"requires_model" %in% self$properties`, set
`store_benchmark_result = TRUE` alongside `.model_required`, or fail fast with an actionable
message:
`"fs('rfe') requires 'store_benchmark_result = TRUE' to read importance scores."`

---

### 4. `EnsembleFSResult$pareto_front()` returns dominated points and drops real ones

`R/EnsembleFSResult.R:490-516`

```r
data = if (minimize) {
  data[order(n_features, -get(measure_id))]   # worst-first within each n_features
} else {
  data[order(n_features,  get(measure_id))]   # also worst-first
}
...
for (i in seq_row(data)) {
  condition = if (minimize) data[[measure_id]][i] < best_score else ... > best_score
  if (condition) { pf = rbind(pf, data[i]); best_score = data[[measure_id]][i] }
}
```

The sort places the **worst** score first inside each `n_features` group, so the strictly-improving
scan admits every point in a group, not just its best. `n_features` is never compared, so
same-size dominated points are all declared Pareto-optimal.

**Confirmed.** Input: `(1, 0.5) (1, 0.4) (1, 0.3) (2, 0.45) (2, 0.2) (3, 0.1)`

*Minimizing (`classif.ce`) — returns 5 points, 2 of them dominated:*

```
   n_features classif.ce
1:          1        0.5   <- dominated by (1, 0.3)
2:          1        0.4   <- dominated by (1, 0.3)
3:          1        0.3
4:          2        0.2
5:          3        0.1
```

*Maximizing (`classif.acc`) — worse, the true front is thrown away:*

```
   n_features classif.acc
1:          1        0.3   <- dominated
2:          1        0.4   <- dominated
3:          1        0.5
```

The correct answer is the single point `(1, 0.5)`. Everything at `n_features` 2 and 3 vanished.

This corrupts every downstream consumer: `knee_points()`, the `"estimated"` linear-model fit
(fitted to dominated noise), and any user plotting the front.

**Fix:** invert both sort directions and keep the running optimum with `<=`/`>=` semantics per
group, or drop the loop entirely:

```r
setorderv(data, c("n_features", measure_id), c(1L, if (minimize) 1L else -1L))
data[, .SD[1L], by = "n_features"][
  if (minimize) cummin(get(measure_id)) == get(measure_id)
  else          cummax(get(measure_id)) == get(measure_id)]
```

(The `rbind`-in-a-loop is also O(n²) — see Suggestions.)

---

### 5. `knee_points()` silently returns a row of `NA`

`R/EnsembleFSResult.R:585-588`

```r
nfeats = (n_features - min(n_features)) / (max(n_features) - min(n_features)),
perf   = (get(measure_id) - min(...)) / (max(...) - min(...))
```

When the Pareto front has one distinct `n_features` (or one distinct score) the denominator is `0`,
every distance is `NaN`, `which_max()` yields `NA`, and `pf[NA]` returns a row of `NA`s.

**Confirmed:**

```
> e$knee_points()
   n_features classif.ce
1:         NA         NA
```

No warning, no error. A user reading `$knee_points()$n_features` gets `NA` and has no idea why.
Combined with bug #4 (which readily produces fronts where every point shares one `n_features`),
this is easy to hit.

**Fix:** guard degenerate fronts — if `nrow(pf) == 1L` or the range is zero, return `pf[1L]` with
an informative `cli` message rather than propagating `NaN`.

---

### 6. `extract_inner_fselect_results()` mutates the objects it reads

`R/extract_inner_fselect_results.R:63-69`

```r
data = setalloccol(learner$fselect_result)   # NOT a copy
set(data, j = "iteration", value = i)
if (fselect_instance) set(data, j = "fselect_instance", value = list(learner$fselect_instance))
```

`setalloccol()` over-allocates in place; it does not copy. Both `set()` calls therefore write into
the `FSelectInstance`'s own `private$.result`.

**Confirmed:**

```
result cols BEFORE: V1, V2, V3, V4, features, n_features, classif.ce
result cols AFTER : ..., classif.ce, iteration
result cols AFTER2: ..., classif.ce, iteration, fselect_instance
circular ref? result$fselect_instance[[1]] is the instance: TRUE
```

Three distinct failure modes:

1. **Non-idempotent.** After one call with `fselect_instance = TRUE`, a later call with
   `fselect_instance = FALSE` *still* returns the instance column — it is baked into the source.
2. **Circular reference.** `instance$result$fselect_instance[[1]]` **is** the instance. Serializing
   the `ResampleResult` (`saveRDS`, parallel workers, `mlr3batchmark`) now walks a cycle and
   duplicates the whole archive.
3. A read-only extractor silently changes what `learner$fselect_result` returns for the rest of
   the session.

**Fix:** `data = copy(learner$fselect_result)`. `setalloccol()` buys nothing here — the table is
about to be `rbindlist`ed anyway.

---

### 7. The SVM-RFE guard accepts exactly the configuration it documents as unsupported

`R/mlr_callbacks.R:89`

```r
if (isTRUE(params$type != "C-classification") || isTRUE(params$kernel != "linear")) {
  stop("Only SVMs with `type = 'C-classification'` and `kernel = 'linear'` are supported.")
}
```

When `type` or `kernel` is unset, `params$type` is `NULL`, `NULL != "C-classification"` is
`logical(0)`, and `isTRUE(logical(0))` is `FALSE`. The guard passes.

**Confirmed:**

```
params = list()                  -> guard triggers? FALSE
params = list(kernel = "radial") -> guard triggers? TRUE
```

`lrn("classif.svm")` defaults to `kernel = "radial"` in e1071 *without setting the param*, so the
common case — a user who forgot to configure the SVM — sails straight through. The importance
formula `t(model$coefs) %*% model$SV` is only meaningful for a linear kernel; for anything else the
callback drives the entire RFE on numbers that mean nothing, and never says so.

**Fix:**

```r
if (!identical(params$type, "C-classification") || !identical(params$kernel, "linear")) stop(...)
```

**Also in the same callback:**

- `sort(x[1, ], decreasing = TRUE)` (line 105) takes row 1 of the weight matrix. For multiclass
  C-classification, `model$coefs` holds one-vs-one sub-problems, so this is the importance of a
  single binary pair presented as the importance of the whole model. Either restrict to binary
  tasks or aggregate across sub-problems.
- `LearnerClassifSVMRFE` is defined **inside** `on_optimization_begin` (line 93), so a fresh R6
  generator is created on every optimization run. Class identity and learner hashes differ between
  otherwise-identical runs. Move it to package scope.
- `packageVersion("mlr3") > "0.20.2"` (line 115) is dead — `DESCRIPTION` requires `mlr3 (>= 1.0.1)`.
  The dead `else` branch (`learner_rfe$encapsulate = learner$encapsulate`) would *error* if reached,
  since `$encapsulate` is a method in current mlr3. Delete both.
- `requireNamespace("mlr3learners")` (line 84) discards its return value; a missing package produces
  a warning and then a cryptic `::` failure. Use `require_namespaces("mlr3learners")`.

---

### 8. `mlr3fselect.one_se_rule` crashes on small archives

`R/mlr_callbacks.R:172-176`

```r
y  = data[[archive$cols_y]]
se = sd(y) / sqrt(length(y))
if (se == 0) {
```

`sd()` of a length-1 vector is `NA`.

**Confirmed:**

```
> fselect(..., term_evals = 1, callbacks = clbk("mlr3fselect.one_se_rule"))
Error in if (se == 0) { : missing value where TRUE/FALSE needed
```

The same crash occurs for any `NA` in `y`.

Two further problems in this callback:

- **Statistically the wrong standard error.** The one-standard-error rule (Kuhn & Johnson 2013 —
  the cited source) uses the standard error of the *resampling folds of the best model*. This uses
  the standard deviation across *all evaluated feature sets*, which measures how much the search
  space varies, not how uncertain the best estimate is. With a wide search the window is far too
  large and the rule degenerates to "return the smallest feature set evaluated"; with a narrow
  search it is too small and the rule is a no-op. **Verify** the intent, but as written it does not
  implement the cited rule.
- **Type corruption.** `data[, "n_features" := map(get("features"), length)]` (line 169) writes a
  **list** column, and it is written straight into `private$.result`. Confirmed: with the callback
  active, `instance$result$n_features` is `<list>`, whereas without it the same field is `<int>`.
  Anything doing arithmetic on `fselect_result$n_features` downstream breaks. Use `lengths()` or
  `map_int()`.

---

### 9. `always_included` is silently ignored by the asynchronous objective

`R/ObjectiveFSelectAsync.R:31-40` vs. `R/ObjectiveFSelectBatch.R:64-71`

The batch objective honours the col role that this package itself registers in `zzz.R:23`:

```r
always_included = task$col_roles$always_included
task$set_col_roles(always_included, "feature")
task$select(c(state, always_included))
```

The async objective just does `self$task$select(names(private$.xs)[as.logical(private$.xs)])`.
`grep -rn always_included R/` returns hits only in `ObjectiveFSelectBatch.R` and `zzz.R`.

**Confirmed** (Redis + mirai, `tsk("sonar")` reduced to V1-V4, `task$set_col_roles("V1",
"always_included")`, features read back off `rpart`'s `model$terms`):

```
=== BATCH  (fs("random_search")) ===        === ASYNC (fs("async_random_search")) ===
  eval 1 trained on: V3,V1                    eval 1 trained on: V3
  eval 2 trained on: V2,V3,V4,V1              eval 2 trained on: V2,V4
  eval 3 trained on: V2,V3,V4,V1              eval 3 trained on: V3,V2,V4
```

`V1` is in every batch model and in no async model. Same task, same col role, silently different
results. Because `always_included` also strips the column from `task$feature_names`, the async path
does not merely fail to force the feature in — it **excludes it entirely**, which is the exact
opposite of the requested semantics.

Tests cover only the batch path (`tests/testthat/test_FSelectInstanceSingleCrit.R:64,93`).

**Fix:** hoist the col-role handling into a shared helper and call it from both objectives; add the
async equivalents of the two existing tests.

---

## Required changes

### Correctness and API

**`archive$best()` returns zero rows under its default tie method when a score is `NA`**
`R/ArchiveBatchFSelect.R:188-192`, `R/ArchiveAsyncFSelect.R:170-174`

```r
if (ties_method == "least_features") {
  ii = which(y == max(y))          # NA-unsafe
  ...
} else {
  ii = which_max(y, ties_method = "random")   # NA-safe
}
```

Confirmed: with one `NA` in a 4-row archive, `best()` returns **0 rows** under
`"least_features"` (the default) and 1 row under `"random"`. Two branches of one method with
different NA semantics is indefensible; use `max(y, na.rm = TRUE)` or route both through
`which_max()`. bbotk's `add_evals()` currently blocks `NA` from entering the batch archive, which
is the only reason this is not already a production incident — but the async archive and any
callback that writes `aggregated_performance` bypass that.

**`ensemble_fselect()` / `embedded_ensemble_fselect()` throw away the result of `as_learners()`**
`R/ensemble_fselect.R:93`, `R/embedded_ensemble_fselect.R:65`

```r
assert_learners(as_learners(learners), task = task)   # return value discarded
```

`learners` is then used unconverted at lines 101, 107, 115. Pass a single `Learner` and:

```
Error: no applicable method for 'as_learner' applied to an object of class "environment"
```

`lrns()` returns a list so the happy path works, but `lrn()` — the far more common call — produces
that. Assign: `learners = assert_learners(as_learners(learners), task = task)`.

**`embedded_ensemble_fselect()` instantiates the caller's resampling by reference**
`R/embedded_ensemble_fselect.R:71`

```r
init_resampling$instantiate(task)
```

Confirmed: `rs$is_instantiated` flips `FALSE -> TRUE` on the caller's object. Reusing that
`Resampling` for another task now silently reuses row ids from the first. `ensemble_fselect()` does
*not* do this — the two sibling functions disagree. Clone first.

**`ArchiveAsyncFSelect$best()` ignores the `ties_method` set at construction**
`R/ArchiveAsyncFSelect.R:166-167`

```r
best = function(n_select = 1, ties_method = "least_features") {
  ties_method = assert_choice(...) %??% private$.ties_method
```

The `%??%` fallback can never fire because the default is a literal, not `NULL`. Confirmed: the
batch class defaults to `NULL` (correct), the async class to `"least_features"` (dead
`private$.ties_method`). So `fsi_async(..., ties_method = "random")` is silently ignored.

**`extract_inner_fselect_archives(exclude_columns = ...)` is silently ignored**
`R/extract_inner_fselect_archives.R:72`

```r
data = as.data.table(learner$archive, exclude_columns)
```

`as.data.table.ArchiveBatchFSelect(x, ..., exclude_columns, measures)` declares `exclude_columns`
**after** `...`, so a positional argument lands in `...` and is dropped. Confirmed:
`extract_inner_fselect_archives(rr, exclude_columns = NULL)` still excludes `uhash`. Pass by name.

**`as.data.table(archive)` returns `n_features` as a list column**
`R/ArchiveBatchFSelect.R:237`

```r
tab[, "n_features" := map(get("features"), length)]
```

Confirmed: `class(tab$n_features)` is `"list"`; `sort(tab$n_features)` errors with
`'x' must be atomic`. The documented data structure implies a scalar. Use `lengths(get("features"))`.
Same defect at `R/mlr_callbacks.R:169`.

**`Sequential$optimization_path()` reports the wrong rows**
`R/FSelectorBatchSequential.R:72`

```r
res = archive$data[, head(.SD, 1), by = get("batch_nr")]
```

`head(.SD, 1)` is the *first* evaluation of each batch, not the best. Confirmed:

```
reported path : 0.5072  0.3913  0.3913
actual best   : 0.3623  0.3623  0.3913
```

The final selected set (`V5`, 0.3623) does not appear anywhere in its own "optimization path".
`FSelectorBatchShadowVariableSearch$optimization_path()` (`R/FSelectorBatchShadowVariableSearch.R:84`)
gets this right and even carries the comment *"we have to use the best method to get the same tie
breaking as in the optimize method"* — the correct approach was known and not applied here. Also
use `by = "batch_nr"`, not `by = get("batch_nr")`.

**Shadow variable search leaks state into the instance and across runs**
`R/FSelectorBatchShadowVariableSearch.R:92-166`

`.optimize()` mutates `inst$objective$task` (`task$cbind(data)`), the domain, and the search space;
`.assign_result()` restores them. But when `.optimize()` throws at line 131, `.assign_result()`
never runs. Confirmed after the abort:

```
objective task features: V1..V5, permuted__V1..permuted__V5
```

The instance is permanently corrupted. Two aggravating factors:

- The saved originals live on the **FSelector** (`private$.task`, `private$.domain`), so reusing one
  `fs("shadow_variable_search")` across instances restores the *wrong* task into the second one.
- `private$.task = suppressWarnings(task$clone(deep = TRUE))` (line 95) suppresses an unnamed
  warning with no comment. Either handle the specific condition or document which one and why.

Use `on.exit()` for restoration and keep the saved state on the instance, not the optimizer.

**`AutoFSelector` swallows every error when setting `predict_type`**
`R/AutoFSelector.R:307-314`

```r
# Catches 'Error: Field/Binding is read-only' bug
tryCatch({ self$model$learner$predict_type = rhs }, error = function(cond) {})
```

A bare catch-all that discards the condition. If the trained learner legitimately rejects the
predict type, `private$.predict_type` is still updated and the object is left inconsistent —
`afs$predict_type` claims `"prob"` while the wrapped learner still predicts `"response"`, and the
failure only surfaces later inside `$predict()`. Guard the actual condition:

```r
if (!is.null(self$model$learner)) self$model$learner$predict_type = rhs
```

**`AutoFSelector$phash` contradicts its own documentation**
`R/AutoFSelector.R:335-342` — documented as *"excluding some components which are varied
systematically ... (feature names)"*, implemented as `self$hash`. Confirmed identical. Either
implement it or document that it is deliberately equal. Related: `$hash` digests
`self$instance_args` (raw R6 `Learner`/`Resampling`/`Measure`/`Terminator` objects) rather than
their `$hash` fields — brittle and potentially unstable across sessions.

**`AutoFSelector` active bindings are not read-only**
`R/AutoFSelector.R:275-294` — `archive`, `learner`, `fselect_instance`, `fselect_result` take no
`rhs`. Confirmed: `afs$archive = 1` errors with `unused argument (base::quote(1))` instead of the
project-standard read-only message. `extra-rules/mlr3.md` mandates `assert_ro_binding(rhs)`.

**RFECV never restores the objective's resampling**
`R/FSelectorBatchRFECV.R:156-174` overwrites `inst$objective$constants$values$resampling` twice and
leaves it as `list(resampling_insample)`. `inst$objective$resampling` still reports the original CV,
so the instance is left in a self-contradictory state and any later `eval_batch()` silently
resamples in-sample. Restore in an `on.exit()`.

**Backup callback deletes the good backup before writing the new one**
`R/mlr_callbacks.R:36-39`

```r
if (file.exists(callback$state$path)) unlink(callback$state$path)
saveRDS(context$instance$archive$benchmark_result, callback$state$path)
```

`saveRDS` overwrites, so the `unlink` buys nothing — except a window where the previous backup is
gone and the new one is not yet written. A crash there loses the whole run. That is the precise
scenario this callback exists to prevent. Write to `paste0(path, ".tmp")` then `file.rename()`.
Also, the default path `"bmr.rds"` (line 30) writes into the user's working directory.

**`stability()` cache ignores `stability_args`**
`R/EnsembleFSResult.R:416-425` keys the cache on `stability_measure` only. Confirmed: calling
`$stability("jaccard")` then `$stability("jaccard", stability_args = list(impute.na = 0))` returns
the *identical* value. The docstring admits this ("the cache must be reset") — documenting a
footgun is not fixing it. Key on `calculate_hash(stability_measure, stability_args)`.

**Documented arguments that do not exist**

- `R/EnsembleFSResult.R:13` documents `as.data.table.EnsembleFSResult(x, benchmark_result = TRUE)`.
  Confirmed signature: `function (x, ...)`. The argument is neither accepted nor honoured — the
  method always attaches the benchmark columns. Either implement it or fix the docs.
- `R/ArchiveAsyncFSelect.R:76-77` documents `@param check_values` for a constructor with no such
  argument.
- Every `print()` method documenting `@param ... (ignored)` while declaring `print = function()`:
  `R/AutoFSelector.R:246`, `R/ArchiveBatchFSelect.R:157`, `R/ArchiveAsyncFSelect.R:127`,
  `R/ArchiveAsyncFSelectFrozen.R:107`, `R/FSelector.R:75`. Confirmed:
  `print(archive, digits = 3)` → `Error: unused argument (digits = 3)`. Add `...`.
  `R/AutoFSelector.R:246` is additionally missing its `#' @description` tag.
- `R/embedded_ensemble_fselect.R:31` — *"If `NULL`, default measure is used"*. `measure` has no
  default and `assert_measure(NULL)` fails.

**`mlr_fselectors_rfecv` documentation is copy-pasted from RFE**
`R/FSelectorBatchRFECV.R:44` — `@templateVar id rfe` on the **rfecv** page, so the dictionary
section instructs `fs("rfe")`. Line 116: `label = "Recursive Feature Elimination"`, identical to
RFE's, so both appear the same in `as.data.table(mlr_fselectors)`.

### Slop and duplication

**`fselect()` is a 65-line copy-paste of `fsi()` and `fsi_async()`**
`R/fselect.R:98-153` reimplements the exact four-way branch that `R/sugar.R` already provides.
Two copies to keep in sync, and they have already drifted: `fselect()` passes `measure = measures`
by name, `fsi()` passes it positionally (`R/sugar.R:76`) — the latter breaks silently if the
constructor's formals are ever reordered. Collapse `fselect()` to
`instance = if (inherits(fselector, "FSelectorAsync")) fsi_async(...) else fsi(...)`.

Related: `fselect()` accepts `rush` (ignored for batch) and `ties_method` (ignored for multi-crit)
and says nothing when they are dropped.

**`extract_runtime()` exists and is not used where it should be**
`R/helper.R:21` is called only from `ObjectiveFSelectAsync.R:75`. `ObjectiveFSelectBatch.R:112-116`
open-codes the same logic. Call the helper.

**Dead paradox-compatibility branch**
`R/helper.R:11-17` — `if ("set_id" %in% names(ps()))` targets paradox < 1.0.0; `DESCRIPTION`
requires `>= 1.0.0`. It also constructs a throwaway `ps()` once *per measure*. Delete the branch and
the `get("ParamDbl")` indirection.

**`class(x)[1] == "..."` instead of `inherits()`**
`R/ensemble_fselect.R:95, 165`, `R/embedded_ensemble_fselect.R:68`. A user subclass of
`ResamplingSubsampling` is rejected, and a subclass of `FSelectorBatchRFE` silently loses its
`importance` column. `assert_choice(class(init_resampling)[1], ...)` also produces a useless
message (`Assertion on 'class(init_resampling)[1]' failed`). Use `assert_multi_class()` /
`inherits()`.

**Style rule violations in `R/mlr_callbacks.R:212-223`**
`mlr3misc::map`, `mlr3misc::transpose_list`, `mlr3misc::get_private`, `data.table::set` — all four
packages are imported wholesale, and `CLAUDE.md` explicitly forbids the `::` prefix. Line 228 in the
same callback uses a bare `set()`, so the file is not even internally consistent.

**Lazy naming and cargo cult**
`R/FSelectorBatchSequential.R:107-117`:

```r
x = ifelse(pars$strategy == "sfs", FALSE, TRUE)
y = ifelse(pars$strategy == "sfs", TRUE, FALSE)
z = if (pars$strategy == "sfs") !best_state else best_state
states = map_dtr(seq_along(best_state)[z], function(i) {
  if (best_state[i] == x) { ... }        # z already guarantees this
})
```

`x`/`y`/`z` communicate nothing (`drop_value`/`add_value`/`candidates`); `ifelse()` is the
vectorised form applied to scalars (use `if`/`else`, or just `pars$strategy == "sbs"`); and the
inner `if` re-tests a condition `z` already filtered on.

`R/helper.R:14,16` also uses scalar `ifelse()`.

`({ ... })` wrappers around the `repeat` bodies: `R/FSelectorBatchSequential.R:98,120` and
`R/FSelectorBatchShadowVariableSearch.R:124,149`. No effect; delete.

`R/FSelectorBatchShadowVariableSearch.R:135` — `archive = inst$archive` re-assigns the same value
already bound at line 115.

`R/mlr_callbacks.R:161` — `callback = callback_batch_fselect(...)` assigns to a variable that is
never read (flagged by `lintr`). The four sibling loaders return directly.

`R/AutoFSelector.R:191,207` — `stopf("Learner ''%s' cannot calculate important scores.", ...)`:
doubled quote and *"important scores"* should be *"importance scores"*.

`R/AutoFSelector.R:352-376` — two near-identical 11-line `imap` blocks differing only in
`train`/`test`. Extract a helper. This whole block is also arguably dead: the constructor asserts
`assert_resampling(resampling, instantiated = FALSE)` (line 140) and the class docs state *"it is
not feasible to pass an instantiated Resampling here"*, yet 25 lines exist to validate one.

**`R/EnsembleFSResult.R:217`** — `stop("No inner_measure was defined during initialization")`; also
`R/FSelectorBatchSequential.R:69`, `R/FSelectorBatchShadowVariableSearch.R:81`, and the five
read-only bindings in `R/FSelector.R:98-140`. `CLAUDE.md` mandates `cli` for errors. Confirmed:
`set_active_measure("inner")` raises a bare `simpleError`.

**`R/zzz.R:19,26`** — `x$optimizer_properties = c(x$optimizer_properties, "requires_model")` and
`x$loaded_packages = c(x$loaded_packages, "mlr3fselect")` append without `unique()`, so repeated
`devtools::load_all()` accumulates duplicates. Line 23 two lines below *does* use `unique()`.

**`R/faggregate.R:75`** — `if (is.null(prediction) && length(measure$predict_sets))` — the second
conjunct is guaranteed true by the early return at line 61.

**Private-API reach-through into mlr3**
`R/ObjectiveFSelectBatch.R:113`, `R/helper.R:22-23`, `R/faggregate.R:47`,
`R/mlr_callbacks.R:216` all use `get_private(rr)$.data$...`. This is precisely what broke in
`4cc56134` ("uses `finish_tasks` instead of the removed `push_results`"). Not fixable in this PR,
but it deserves a comment naming the mlr3 version each depends on, and ideally an upstream request
for public accessors.

---

## Suggestions

- **`pareto_front()` grows a `data.table` with `rbind` inside a loop** (`R/EnsembleFSResult.R:513`)
  — O(n²) copying. The vectorised `cummin`/`cummax` form in the fix for #4 removes both the
  performance problem and the correctness problem.
- **`ObjectiveFSelectAsync.R:70-74`** calls `learner_states()` twice to count warnings and errors,
  and — unlike the batch objective — omits the `$.view` argument. Compute once; match the batch call.
- **`ensemble_fselect()`/`embedded_ensemble_fselect()` score the benchmark twice**
  (`bmr$score()$learner`, then `bmr$score(measure)`). The first call computes the *default* measure
  across every iteration purely to reach `$learner`.
- **`ensemble_fselect()` hardcodes `benchmark(design, store_models = TRUE)`** (line 125) regardless
  of `store_models`/`store_benchmark_result`. Necessary to reach the `AutoFSelector`s afterwards,
  but it means peak memory holds every inner model even when the user asked for none. Worth a note
  in the docs.
- **`ensemble_fselect()` has no failure path.** If any learner fails to train,
  `afs$fselect_result$features[[1]]` (line 131) hits `NULL[[1]]` → `subscript out of bounds`, and
  the entire ensemble run is lost. Detect and skip, or fail with a message naming the learner.
  Likewise `map_dbl` at line 140 assumes `archive$best()` returns exactly one row.
- **`ensemble_fselect()`/`rm_zero_features()` rely on an undocumented ordering invariant** —
  that `bmr$score()` rows align 1:1 and in order with `bmr$resample_results`. `rm_zero_features()`
  (`R/EnsembleFSResult.R:178-187`) walks both in lockstep. Add an assertion or join on `uhash`.
- **`FSelectorBatchExhaustiveSearch`** (`R/FSelectorBatchExhaustiveSearch.R:59-71`) materialises all
  `2^n - 1` subsets before the first evaluation. At n = 25 that is ~33M rows. The terminator cannot
  help because generation precedes it. Generate lazily per batch.
- **`FSelectorBatchSequential` with `min_features = k`** issues `choose(n, k)` evaluations in the
  *first* batch. Undocumented; worth a warning in the Control Parameters section.
- **`rfe_subsets()`** (`R/FSelectorBatchRFE.R:222-224`) with `feature_fraction = 0` (allowed:
  `p_dbl(lower = 0, ...)`) yields `log(0) = -Inf`, `rep(0, 0)`, and a single subset — the whole RFE
  degenerates to one evaluation, silently. Same for `n_features >= n`. Add feasibility checks.
- **`ArchiveAsyncFSelect$benchmark_result`** (`R/ArchiveAsyncFSelect.R:205-212`) rebuilds the entire
  benchmark result by folding N resample results on every access past the cache threshold; and if
  `finished_data$resample_result` is `NULL`, `Reduce` over an empty list returns `NULL`, leaving the
  field un-typed so `$learners()` fails with `attempt to apply non-function`.
- **`as.data.table.ArchiveAsyncFSelect`** (`R/ArchiveAsyncFSelect.R:240-250`) builds `tab` from
  `x$data_with_state()` but the extra-measure scores from `x$data`. If those differ in row count the
  `cbind` recycles or errors. Use one source.
- **`ContextBatchFSelect`** — four active bindings identical but for the private field name. A small
  generator would remove ~40 lines.
- **`assert_fselector()`** (`R/assertions.R:11`) is the only member of the family that is neither
  exported nor documented, while `assert_fselectors()` (which calls it) is both.
- **`bibentries.R:141,145`** exceed the 120-character limit (`lintr`).

---

## Test coverage

Baseline on a clean checkout: `FAIL 0 | WARN 1 | SKIP 19 | PASS 6097`. Green — which is the point.
Every blocking bug above sits inside a passing suite.

The gaps are structural, not incidental:

1. **No rfecv test uses a minimizing measure.** `MeasureDummy` defaults to `minimize = FALSE`
   (`inst/testthat/helper_misc.R:41`) and every rfecv test relies on it. Bug #1 is invisible to the
   entire file. **Add a `minimize = TRUE` variant to every optimizer test that selects a "best"
   anything** — this bug class will recur otherwise.
2. **No test for `recursive = FALSE`** on rfecv (bug #2), despite it being a documented parameter.
3. **No test for `store_benchmark_result = FALSE`** with rfe/rfecv (bug #3).
4. **No degenerate-input tests for `EnsembleFSResult`**: single Pareto point, one archive entry,
   all-equal scores (bugs #4, #5, #8).
5. **No async test for `always_included`** (bug #9); the batch path has two.
6. **No test asserts that extractors leave their input unchanged** (bug #6). A single
   `expect_equal(names(before), names(after))` would have caught it.

---

## What I could not verify

- The `ArchiveAsyncFSelect$benchmark_result` rebuild cost and the `as.data.table` row-source
  mismatch are reasoned from the source, not reproduced — I ran async optimizations against Redis
  but did not construct the degenerate inputs (`store_benchmark_result = FALSE`, partially finished
  archives) that would trigger them.
- `feature_ranking()` with `use_weights = TRUE` computes `1 / scores` for minimizing measures
  (`R/EnsembleFSResult.R:361`). A score of exactly `0` — a perfect `classif.ce` — yields an `Inf`
  weight. `fastVoteR` is not installed here, so I could not confirm the downstream behaviour, but
  the docstring warns only about *negative* scores, not zero. **Verify.**
- Whether the one-standard-error definition in `mlr3fselect.one_se_rule` is a deliberate departure
  from Kuhn & Johnson or an oversight. **Verify.**

---

## Verdict

**Request Changes.**

Blocking items 1-9 must be fixed before release; 1, 2, 3, and 7 are the ones that will generate
user-visible incidents. Item 1 in particular means published results produced with `fs("rfecv")` and
a default measure are wrong, which is worth a `NEWS.md` entry stating so plainly rather than a
neutral "fix:" bullet.
