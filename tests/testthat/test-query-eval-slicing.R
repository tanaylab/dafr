test_that("SquareRowIs slices a matrix to one row", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs @- c1")
  expect_equal(v, c(g1 = 1, g2 = 3))
})

test_that("SquareColumnIs slices a matrix to one column", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs @| g2")
  expect_equal(v, c(c1 = 3, c2 = 4))
})
