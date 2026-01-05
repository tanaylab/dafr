# Use default or custom Julia environment

Force JuliaCall to use your specified Julia environment instead of
creating a new one. By default JuliaCall creates its own separate
independent environment, which means that it re-downloads and
re-installs all the dependency packages there. This function allows
using the default Julia environment or a custom one.

## Usage

``` r
use_default_julia_environment(env_path = "default")
```

## Arguments

- env_path:

  Either "default" to use the default Julia environment, or a path to a
  custom environment

## Value

No return value, called for side effects.
