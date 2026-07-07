hill_number <- getFromNamespace("hill_number", "wpur")
molinar_index <- getFromNamespace("molinar_index", "wpur")

testthat::test_that("hill_number computes Laakso-Taagepera for p 2", {
    shares <- c(0.6, 0.4)

    testthat::expect_equal(
        hill_number(c(60, 40), p = 2),
        1 / sum(shares^2),
        tolerance = 1e-12
    )
})

testthat::test_that("hill_number computes richness for p 0 and Shannon effective count for p 1", {
    shares <- c(0.6, 0.4)

    testthat::expect_equal(
        hill_number(c(60, 40), p = 0),
        2
    )
    testthat::expect_equal(
        hill_number(c(60, 40), p = 1),
        exp(-sum(shares * log(shares))),
        tolerance = 1e-12
    )
})

testthat::test_that("hill_number ignores zero votes and returns NA for no positive votes", {
    testthat::expect_equal(
        hill_number(c(0, 100), p = 2),
        1
    )
    testthat::expect_true(is.na(hill_number(c(NA, NA), p = 2)))
})

testthat::test_that("molinar_index matches the largest-share adjusted formula", {
    shares <- c(0.6, 0.4)
    lt <- hill_number(c(60, 40), p = 2)

    testthat::expect_equal(
        molinar_index(c(60, 40)),
        1 + lt^2 * (sum(shares^2) - max(shares)^2),
        tolerance = 1e-12
    )
    testthat::expect_equal(
        molinar_index(c(0, 100)),
        1
    )
})
