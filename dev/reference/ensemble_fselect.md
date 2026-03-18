# Wrapper-based Ensemble Feature Selection

Ensemble feature selection using multiple learners. The ensemble feature
selection method is designed to identify the most predictive features
from a given dataset by leveraging multiple machine learning models and
resampling techniques. Returns an
[EnsembleFSResult](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md).

## Usage

``` r
ensemble_fselect(
  fselector,
  task,
  learners,
  init_resampling,
  inner_resampling,
  inner_measure,
  measure,
  terminator,
  callbacks = NULL,
  store_benchmark_result = TRUE,
  store_models = FALSE
)
```

## Source

Saeys, Yvan, Abeel, Thomas, Van De Peer, Yves (2008). “Robust feature
selection using ensemble feature selection techniques.” *Machine
Learning and Knowledge Discovery in Databases*, **5212 LNAI**, 313–325.
[doi:10.1007/978-3-540-87481-2_21](https://doi.org/10.1007/978-3-540-87481-2_21)
.

Abeel, Thomas, Helleputte, Thibault, Van de Peer, Yves, Dupont, Pierre,
Saeys, Yvan (2010). “Robust biomarker identification for cancer
diagnosis with ensemble feature selection methods.” *Bioinformatics*,
**26**, 392–398. ISSN 1367-4803,
[doi:10.1093/BIOINFORMATICS/BTP630](https://doi.org/10.1093/BIOINFORMATICS/BTP630)
.

Pes, Barbara (2020). “Ensemble feature selection for high-dimensional
data: a stability analysis across multiple domains.” *Neural Computing
and Applications*, **32**(10), 5951–5973. ISSN 14333058,
[doi:10.1007/s00521-019-04082-3](https://doi.org/10.1007/s00521-019-04082-3)
.

## Arguments

- fselector:

  ([FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md))  
  Optimization algorithm.

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

- learners:

  (list of
  [mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  The learners to be used for feature selection.

- init_resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  The initial resampling strategy of the data, from which each train set
  will be passed on to the
  [auto_fselector](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md)
  to optimize the learners and perform feature selection. Each test set
  will be used for prediction on the final models returned by
  [auto_fselector](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md).
  Can only be
  [mlr3::ResamplingSubsampling](https://mlr3.mlr-org.com/reference/mlr_resamplings_subsampling.html)
  or
  [mlr3::ResamplingBootstrap](https://mlr3.mlr-org.com/reference/mlr_resamplings_bootstrap.html).

- inner_resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  The inner resampling strategy used by the
  [FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md).

- inner_measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  The inner optimization measure used by the
  [FSelector](https://mlr3fselect.mlr-org.com/dev/reference/FSelector.md).

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure used to score each trained learner on the test sets generated
  by `init_resampling`.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the feature selection.

- callbacks:

  (Named list of lists of
  [CallbackBatchFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackBatchFSelect.md))  
  Callbacks to be used for each learner. The lists must be named by the
  learner ids.

- store_benchmark_result:

  (`logical(1)`)  
  Whether to store the benchmark result in
  [EnsembleFSResult](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md)
  or not.

- store_models:

  (`logical(1)`)  
  Whether to store models in
  [auto_fselector](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md)
  or not.

## Value

an
[EnsembleFSResult](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md)
object.

## Details

The method begins by applying an initial resampling technique specified
by the user, to create **multiple subsamples** from the original dataset
(train/test splits). This resampling process helps in generating diverse
subsets of data for robust feature selection.

For each subsample (train set) generated in the previous step, the
method performs **wrapped-based feature selection**
([auto_fselector](https://mlr3fselect.mlr-org.com/dev/reference/auto_fselector.md))
using each provided learner, the given inner resampling method, inner
performance measure and optimization algorithm. This process
generates 1) the best feature subset and 2) a final trained model using
these best features, for each combination of subsample and learner. The
final models are then scored on their ability to predict on the
resampled test sets.

Results are stored in an
[EnsembleFSResult](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md).

The result object also includes the performance scores calculated during
the inner resampling of the training sets, using models with the best
feature subsets. These scores are stored in a column named
`{measure_id}_inner`.

## Note

The **active measure** of performance is the one applied to the test
sets. This is preferred, as inner resampling scores on the training sets
are likely to be overestimated when using the final models. Users can
change the active measure by using the `set_active_measure()` method of
the
[EnsembleFSResult](https://mlr3fselect.mlr-org.com/dev/reference/ensemble_fs_result.md).

## Examples

``` r
# \donttest{
  efsr = ensemble_fselect(
    fselector = fs("random_search"),
    task = tsk("sonar"),
    learners = lrns(c("classif.rpart", "classif.featureless")),
    init_resampling = rsmp("subsampling", repeats = 2),
    inner_resampling = rsmp("cv", folds = 3),
    inner_measure = msr("classif.ce"),
    measure = msr("classif.acc"),
    terminator = trm("evals", n_evals = 10)
  )
  efsr
#> 
#> ── <EnsembleFSResult> with 2 learners and 2 initial resamplings ────────────────
#>    resampling_iteration                    learner_id n_features
#>                   <int>                        <char>      <int>
#> 1:                    1       classif.rpart.fselector         49
#> 2:                    2       classif.rpart.fselector         10
#> 3:                    1 classif.featureless.fselector         10
#> 4:                    2 classif.featureless.fselector          1
# }
```
