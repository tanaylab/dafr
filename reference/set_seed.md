# Set a seed both in Julia and R

Set a seed both in Julia and R

## Usage

``` r
set_seed(seed)
```

## Arguments

- seed:

  seed to be used

## Value

No return value, called for side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
## Needs previous call to `setup_daf` which is time
## consuming and requires Julia
setup_daf(installJulia = TRUE, seed = 123)
set_seed(123)
} # }
```
