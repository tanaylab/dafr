# Set up a global logger

Setup a global logger that will print into a specified output stream
(stdout or stderr), with configurable options for timestamps and other
information.

## Usage

``` r
setup_logger(
  io = stderr,
  level = "Warn",
  show_time = TRUE,
  show_module = TRUE,
  show_location = FALSE
)
```

## Arguments

- io:

  Output stream, either stdout or stderr (default: stderr)

- level:

  Log level, one of "Debug", "Info", "Warn", "Error", or an integer
  (default: "Warn")

- show_time:

  Whether to show timestamp (default: TRUE)

- show_module:

  Whether to show module name (default: TRUE)

- show_location:

  Whether to show source location (default: FALSE)

## Value

No return value, called for side effects.

## Examples

``` r
if (FALSE) { # \dontrun{
# Setup logger with default settings
setup_logger()

# Setup logger with custom settings
setup_logger(io = stdout, level = "Info", show_location = TRUE)
} # }
```
