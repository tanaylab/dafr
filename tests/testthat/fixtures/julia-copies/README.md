# Julia copies/concat fixture

Generated from DataAxesFormats.jl at `49fbba140437387a378217c2fa658d4231d0c8c1`.

Regenerate:

```
conda run -n dafr-mcview julia --project=~/src/dafr-mcview \
    dev/scripts/regen-julia-copies-fixture.jl
```

Payloads:

- `copy_all_fixture.json` — `copy_all!` roundtrip with cell-axis
  superset and empty fills for `cell|age` and `cell|gene|UMIs`.
- `concat_fixture.json` — `concatenate!` of two sources on the
  `cell` axis with `prefix = true`.
