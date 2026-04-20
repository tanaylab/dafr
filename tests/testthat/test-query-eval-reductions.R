test_that(">| Sum reduces matrix columns to vector (per-column totals)", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
  expect_equal(v, c(g1 = 3, g2 = 7))
})

test_that(">- Mean reduces matrix rows to vector (per-row means)", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs >- Mean")
  expect_equal(v, c(c1 = 2, c2 = 3))
})
