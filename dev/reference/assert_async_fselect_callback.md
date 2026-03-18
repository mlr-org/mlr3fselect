# Assertions for Callbacks

Assertions for
[CallbackAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackAsyncFSelect.md)
class.

## Usage

``` r
assert_async_fselect_callback(callback, null_ok = FALSE)

assert_async_fselect_callbacks(callbacks)
```

## Arguments

- callback:

  ([CallbackAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackAsyncFSelect.md)).

- null_ok:

  (`logical(1)`)  
  If `TRUE`, `NULL` is allowed.

- callbacks:

  (list of
  [CallbackAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackAsyncFSelect.md)).

## Value

\[CallbackAsyncFSelect \| List of
[CallbackAsyncFSelect](https://mlr3fselect.mlr-org.com/dev/reference/CallbackAsyncFSelect.md)s.
