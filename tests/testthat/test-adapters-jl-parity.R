# Literal port of adapters.jl. 1 leaf.

test_that("adapters", {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "INPUT", 1L)
    result <- adapter(
        d,
        function(adapted) {
            set_scalar(adapted, "output", get_query(adapted, ". input"))
            7L
        },
        input_data  = list(list("input",  ". INPUT")),
        output_data = list(list("OUTPUT", ". output"))
    )
    expect_identical(result, 7L)
    expect_identical(get_scalar(d, "OUTPUT"), 1L)
})
