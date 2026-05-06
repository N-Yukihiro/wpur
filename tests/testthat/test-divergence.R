alpha_divergence <- getFromNamespace("alpha_divergence", "wpur")
alpha_weight <- getFromNamespace("alpha_weight", "wpur")
conditional_alpha_divergence <- getFromNamespace("conditional_alpha_divergence", "wpur")

testthat::test_that("alpha_weight returns v * (s / v)^alpha", {
    testthat::expect_equal(
        alpha_weight(seat_share = 0.6, vote_share = 0.3, alpha = 2),
        0.3 * (0.6 / 0.3)^2,
        tolerance = 1e-12
    )

    testthat::expect_equal(
        alpha_weight(seat_share = 0.25, vote_share = 0.25, alpha = 10),
        0.25,
        tolerance = 1e-12
    )
})

testthat::test_that("alpha_divergence is zero when seat_share equals vote_share", {
    seat_share <- c(0.2, 0.3, 0.5)
    vote_share <- c(0.2, 0.3, 0.5)

    for (a in c(0, 1, 2, 10)) {
        testthat::expect_equal(
            alpha_divergence(seat_share, vote_share, a),
            0,
            tolerance = 1e-12
        )
    }
})

testthat::test_that("alpha_divergence matches manual formulas", {
    seat_share <- c(0.6, 0.4)
    vote_share <- c(0.5, 0.5)

    expected_alpha_2 <- sum(
        (1 / (2 * (2 - 1))) *
            vote_share *
            ((seat_share / vote_share)^2 - 1)
    )
    expected_alpha_0 <- sum(vote_share * (-log(seat_share / vote_share)))
    expected_alpha_1 <- sum(
        vote_share *
            ((seat_share / vote_share) * log(seat_share / vote_share))
    )

    testthat::expect_equal(
        alpha_divergence(seat_share, vote_share, alpha = 2),
        expected_alpha_2,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        alpha_divergence(seat_share, vote_share, alpha = 0),
        expected_alpha_0,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        alpha_divergence(seat_share, vote_share, alpha = 1),
        expected_alpha_1,
        tolerance = 1e-12
    )
})

testthat::test_that("alpha_divergence is non-negative and asymmetric in standard cases", {
    seat_share <- c(0.7, 0.3)
    vote_share <- c(0.5, 0.5)

    for (a in c(0, 1, 2, 10)) {
        testthat::expect_gte(
            alpha_divergence(seat_share, vote_share, a),
            0
        )
    }

    testthat::expect_false(
        isTRUE(all.equal(
            alpha_divergence(seat_share, vote_share, alpha = 2),
            alpha_divergence(vote_share, seat_share, alpha = 2)
        ))
    )
})

testthat::test_that("conditional_alpha_divergence matches normalized alpha_divergence", {
    seat <- c(6, 4)
    vote <- c(5, 5)

    for (a in c(0, 1, 2, 10)) {
        expected <- alpha_divergence(
            seat_share = seat / sum(seat),
            vote_share = vote / sum(vote),
            alpha = a
        )

        testthat::expect_equal(
            conditional_alpha_divergence(
                seat = seat,
                vote = vote,
                alpha = a
            ),
            expected,
            tolerance = 1e-12
        )
    }
})

testthat::test_that("conditional_alpha_divergence is zero for matching normalized distributions", {
    seat <- c(2, 3, 5)
    vote <- c(20, 30, 50)

    for (a in c(0, 1, 2, 10)) {
        testthat::expect_equal(
            conditional_alpha_divergence(
                seat = seat,
                vote = vote,
                alpha = a
            ),
            0,
            tolerance = 1e-12
        )
    }
})

testthat::test_that("conditional_alpha_divergence is invariant to common rescaling", {
    seat_1 <- c(6, 4)
    vote_1 <- c(5, 5)
    seat_2 <- seat_1 * 10
    vote_2 <- vote_1 * 100

    for (a in c(0, 1, 2, 10)) {
        testthat::expect_equal(
            conditional_alpha_divergence(
                seat = seat_1,
                vote = vote_1,
                alpha = a
            ),
            conditional_alpha_divergence(
                seat = seat_2,
                vote = vote_2,
                alpha = a
            ),
            tolerance = 1e-12
        )
    }
})

testthat::test_that("conditional_alpha_divergence handles zero-total edge cases", {
    for (a in c(0, 1, 2, 10)) {
        testthat::expect_equal(
            conditional_alpha_divergence(
                seat = c(0, 0, 0),
                vote = c(10, 20, 30),
                alpha = a
            ),
            0,
            tolerance = 1e-12
        )

        testthat::expect_true(is.na(
            conditional_alpha_divergence(
                seat = c(1, 2, 3),
                vote = c(0, 0, 0),
                alpha = a
            )
        ))
    }
})

testthat::test_that("conditional_alpha_divergence returns finite non-negative values when alpha > 0", {
    seat <- c(2, 0, 1)
    vote <- c(1, 1, 1)

    for (a in c(1, 2, 10)) {
        result <- conditional_alpha_divergence(
            seat = seat,
            vote = vote,
            alpha = a
        )

        testthat::expect_true(is.finite(result))
        testthat::expect_gte(result, 0)
    }
})

testthat::test_that("conditional_alpha_divergence returns Inf for alpha = 0 when vote share is positive and seat share is zero", {
    result <- conditional_alpha_divergence(
        seat = c(2, 0, 1),
        vote = c(1, 1, 1),
        alpha = 0
    )

    testthat::expect_true(is.infinite(result))
    testthat::expect_gt(result, 0)
})

testthat::test_that("conditional_alpha_divergence is zero for a one-cell group and generally asymmetric", {
    for (a in c(0, 1, 2, 10)) {
        testthat::expect_equal(
            conditional_alpha_divergence(seat = 3, vote = 50, alpha = a),
            0,
            tolerance = 1e-12
        )
    }

    testthat::expect_false(isTRUE(all.equal(
        conditional_alpha_divergence(
            seat = c(8, 2),
            vote = c(5, 5),
            alpha = 2
        ),
        conditional_alpha_divergence(
            seat = c(5, 5),
            vote = c(8, 2),
            alpha = 2
        )
    )))
})
