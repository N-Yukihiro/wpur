fixture_perfect_representation <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 20, 1,
    "D1", "A", 20, 1,
    "D1", "A", 20, 1,
    "D1", "B", 20, 1,
    "D1", "B", 20, 1
)

fixture_identity_positive_cells <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 60, 1,
    "D1", "B", 40, 1,
    "D2", "A", 40, 1,
    "D2", "B", 60, 1
)

fixture_candidate_level_input <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 60, 1,
    "D1", "A", 60, 1,
    "D1", "B", 40, 0,
    "D2", "A", 40, 0,
    "D2", "B", 60, 1
)

fixture_party_level_input <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 120, 2,
    "D1", "B", 40, 0,
    "D2", "A", 40, 0,
    "D2", "B", 60, 1
)

fixture_party_vs_independent <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "P1", 35, 1,
    "D1", "P1", 35, 1,
    "D1", "P2", 10, 0,
    "D1", "無所属", 20, 0,
    "D2", "P1", 10, 0,
    "D2", "P2", 25, 1,
    "D2", "P2", 25, 1,
    "D2", "無所属", 40, 0
)

fixture_independent_label_collision <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "無所属", 40, 1,
    "D1", "無所属1", 60, 1
)

fixture_multicandidate_bug_detector <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 30, 1,
    "D1", "A", 30, 0,
    "D1", "B", 40, 0
)

fixture_with_no_contest <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 60, 1,
    "D1", "B", 40, 1,
    "D2", "A", NA, 1
)

fixture_partially_missing_votes <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", 60, 1,
    "D1", "B", NA, 0
)

fixture_all_no_contest <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "A", NA, 1,
    "D1", "B", NA, 0,
    "D2", "A", NA, 1
)

prepare_election_context <- getFromNamespace("prepare_election_context", "wpur")

call_decompose <- function(df,
                           alpha,
                           group_decomposition = "none",
                           election_info = FALSE) {
    decompose_unequal_representation(
        data = df,
        party_var = party,
        district_var = district,
        votes_var = votes,
        elected_var = elected,
        alpha = alpha,
        group_decomposition = group_decomposition,
        independent_label = "無所属",
        election_info = election_info
    )
}

testthat::test_that("returns a one-row tibble", {
    res <- call_decompose(fixture_perfect_representation, alpha = 2)

    testthat::expect_s3_class(res, "tbl_df")
    testthat::expect_equal(nrow(res), 1L)
})

testthat::test_that("base output columns exist in none mode", {
    res <- call_decompose(fixture_perfect_representation, alpha = 2)

    testthat::expect_true(all(c(
        "group_decomposition",
        "whole_picture_of_unequal_representation",
        "disproportionality",
        "intra_party_unequal_representation",
        "malapportionment",
        "wasted_votes"
    ) %in% names(res)))

    testthat::expect_equal(res$group_decomposition, "none")
    testthat::expect_false("between_group_disproportionality" %in% names(res))
    testthat::expect_false("weighted_within_category_disproportionality" %in% names(res))
})

testthat::test_that("party-level input can be used directly", {
    candidate_res <- call_decompose(fixture_candidate_level_input, alpha = 2)
    party_res <- call_decompose(fixture_party_level_input, alpha = 2)

    testthat::expect_equal(
        party_res,
        candidate_res,
        tolerance = 1e-12
    )
})

testthat::test_that("grouped input is treated the same as ungrouped input", {
    grouped_res <- call_decompose(
        dplyr::group_by(fixture_party_vs_independent, district),
        alpha = 2,
        group_decomposition = "party_vs_independent",
        election_info = TRUE
    )
    ungrouped_res <- call_decompose(
        fixture_party_vs_independent,
        alpha = 2,
        group_decomposition = "party_vs_independent",
        election_info = TRUE
    )

    testthat::expect_equal(grouped_res, ungrouped_res, tolerance = 1e-12)
})

testthat::test_that("election context exposes prepared candidate and party data", {
    context <- prepare_election_context(
        data = fixture_with_no_contest,
        party_var = party,
        district_var = district,
        votes_var = votes,
        elected_var = elected,
        independent_label = "無所属"
    )

    testthat::expect_s3_class(context$candidate_status, "tbl_df")
    testthat::expect_s3_class(context$contested_candidate_data, "tbl_df")
    testthat::expect_true("district_no_contest" %in% names(context$candidate_status))
    testthat::expect_equal(nrow(context$contested_candidate_data), 2)
    testthat::expect_false(any(context$contested_candidate_data$district == "D2"))
    testthat::expect_true(all(c(
        "party_key",
        "party_label",
        "is_independent"
    ) %in% names(context$analysis_data)))
})

testthat::test_that("group decomposition columns exist when grouping is requested", {
    res_party_independent <- call_decompose(
        fixture_party_vs_independent,
        alpha = 2,
        group_decomposition = "party_vs_independent"
    )
    res_multicandidate <- call_decompose(
        fixture_multicandidate_bug_detector,
        alpha = 2,
        group_decomposition = "multicandidate_vs_singlecandidate"
    )

    testthat::expect_true(all(c(
        "between_group_disproportionality",
        "weighted_within_category_disproportionality",
        "disproportionality_within_independent",
        "disproportionality_within_party"
    ) %in% names(res_party_independent)))
    testthat::expect_false("weighted_within_group_disproportionality" %in% names(res_party_independent))
    testthat::expect_equal(
        res_party_independent$group_decomposition,
        "party_vs_independent"
    )

    testthat::expect_true(all(c(
        "between_group_disproportionality",
        "weighted_within_category_disproportionality",
        "disproportionality_within_multicandidate",
        "disproportionality_within_singlecandidate"
    ) %in% names(res_multicandidate)))
    testthat::expect_false("weighted_within_group_disproportionality" %in% names(res_multicandidate))
    testthat::expect_false("disproportionality_within_other" %in% names(res_multicandidate))
    testthat::expect_equal(
        res_multicandidate$group_decomposition,
        "multicandidate_vs_singlecandidate"
    )
})

testthat::test_that("group decomposition dynamic columns have stable order", {
    res_party_independent <- call_decompose(
        fixture_party_vs_independent,
        alpha = 2,
        group_decomposition = "party_vs_independent"
    )
    res_multicandidate <- call_decompose(
        fixture_multicandidate_bug_detector,
        alpha = 2,
        group_decomposition = "multicandidate_vs_singlecandidate"
    )

    testthat::expect_lt(
        match("disproportionality_within_independent", names(res_party_independent)),
        match("disproportionality_within_party", names(res_party_independent))
    )
    testthat::expect_lt(
        match("disproportionality_within_multicandidate", names(res_multicandidate)),
        match("disproportionality_within_singlecandidate", names(res_multicandidate))
    )
})

testthat::test_that("perfect proportionality returns zeros for all base components", {
    for (a in c(0, 1, 2, 10)) {
        res <- call_decompose(fixture_perfect_representation, alpha = a)

        testthat::expect_equal(res$whole_picture_of_unequal_representation, 0, tolerance = 1e-12)
        testthat::expect_equal(res$disproportionality, 0, tolerance = 1e-12)
        testthat::expect_equal(res$intra_party_unequal_representation, 0, tolerance = 1e-12)
        testthat::expect_equal(res$malapportionment, 0, tolerance = 1e-12)
        testthat::expect_equal(res$wasted_votes, 0, tolerance = 1e-12)
    }
})

testthat::test_that("whole-picture decomposition identities hold when all aggregated cells are positive", {
    for (a in c(0, 1, 2, 10)) {
        res <- call_decompose(fixture_identity_positive_cells, alpha = a)

        lhs <- res$disproportionality + res$intra_party_unequal_representation
        rhs <- res$malapportionment + res$wasted_votes

        testthat::expect_equal(
            res$whole_picture_of_unequal_representation,
            lhs,
            tolerance = 1e-10
        )
        testthat::expect_equal(
            res$whole_picture_of_unequal_representation,
            rhs,
            tolerance = 1e-10
        )
    }
})

testthat::test_that("grouped identity holds for party_vs_independent", {
    res <- call_decompose(
        fixture_party_vs_independent,
        alpha = 2,
        group_decomposition = "party_vs_independent"
    )

    testthat::expect_equal(
        res$disproportionality,
        res$between_group_disproportionality +
            res$weighted_within_category_disproportionality,
        tolerance = 1e-10
    )
})

testthat::test_that("grouped identity holds for multicandidate_vs_singlecandidate", {
    res <- call_decompose(
        fixture_multicandidate_bug_detector,
        alpha = 2,
        group_decomposition = "multicandidate_vs_singlecandidate"
    )

    testthat::expect_equal(
        res$disproportionality,
        res$between_group_disproportionality +
            res$weighted_within_category_disproportionality,
        tolerance = 1e-10
    )
})

testthat::test_that("single aggregated party with multiple candidates is classified as multicandidate", {
    res <- call_decompose(
        fixture_multicandidate_bug_detector,
        alpha = 2,
        group_decomposition = "multicandidate_vs_singlecandidate"
    )

    testthat::expect_equal(
        res$weighted_within_category_disproportionality,
        0,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res$between_group_disproportionality,
        res$disproportionality,
        tolerance = 1e-10
    )
    testthat::expect_gt(res$disproportionality, 0)
})

testthat::test_that("election_info adds expected metadata columns", {
    res <- call_decompose(
        fixture_identity_positive_cells,
        alpha = 2,
        election_info = TRUE
    )
    district_shares <- c(0.6, 0.4)
    district_lt <- 1 / sum(district_shares^2)
    district_molinar <- 1 + district_lt^2 * (sum(district_shares^2) - max(district_shares)^2)
    candidate_overall_shares <- c(0.3, 0.2, 0.2, 0.3)
    candidate_overall_lt <- 1 / sum(candidate_overall_shares^2)

    testthat::expect_true(all(c(
        "districts_w_contest",
        "districts_no_contest",
        "party_count",
        "overall_effective_parties_lt",
        "overall_effective_parties_molinar",
        "mean_effective_parties_lt",
        "mean_effective_parties_molinar",
        "candidates_with_votes_total",
        "candidates_with_votes_w_contest",
        "overall_effective_candidates_lt",
        "overall_effective_candidates_molinar",
        "mean_effective_candidates_lt",
        "mean_effective_candidates_molinar",
        "total_valid_votes",
        "total_seats_w_contest",
        "total_seats_no_contest",
        "seats_per_district",
        "votes_per_seat",
        "votes_per_district"
    ) %in% names(res)))

    testthat::expect_equal(res$districts_w_contest, 2)
    testthat::expect_equal(res$districts_no_contest, 0)
    testthat::expect_equal(res$party_count, 2)
    testthat::expect_equal(res$overall_effective_parties_lt, 2, tolerance = 1e-12)
    testthat::expect_equal(res$overall_effective_parties_molinar, 2, tolerance = 1e-12)
    testthat::expect_equal(res$mean_effective_parties_lt, district_lt, tolerance = 1e-12)
    testthat::expect_equal(res$mean_effective_parties_molinar, district_molinar, tolerance = 1e-12)
    testthat::expect_equal(res$candidates_with_votes_total, 4)
    testthat::expect_equal(res$candidates_with_votes_w_contest, 4)
    testthat::expect_equal(
        res$overall_effective_candidates_lt,
        candidate_overall_lt,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res$overall_effective_candidates_molinar,
        1 + candidate_overall_lt^2 * (
            sum(candidate_overall_shares^2) - max(candidate_overall_shares)^2
        ),
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res$mean_effective_candidates_lt,
        district_lt,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res$mean_effective_candidates_molinar,
        district_molinar,
        tolerance = 1e-12
    )
    testthat::expect_false(any(c(
        "effective_parties_lt",
        "effective_parties_molinar",
        "valid_candidates_total",
        "valid_candidates_w_contest",
        "valid_candidates_no_contest",
        "candidates_with_votes_no_contest",
        "mean_effective_candidates_hill_q2"
    ) %in% names(res)))
    testthat::expect_equal(res$total_valid_votes, 200)
    testthat::expect_equal(res$total_seats_w_contest, 4)
    testthat::expect_equal(res$total_seats_no_contest, 0)
})

testthat::test_that("no-contest districts are excluded from the decomposition and counted in election_info", {
    res_full <- call_decompose(
        fixture_with_no_contest,
        alpha = 2,
        election_info = TRUE
    )
    res_contested_only <- call_decompose(
        dplyr::filter(fixture_with_no_contest, !is.na(votes)),
        alpha = 2,
        election_info = FALSE
    )
    district_shares <- c(0.6, 0.4)
    district_lt <- 1 / sum(district_shares^2)
    district_molinar <- 1 + district_lt^2 * (sum(district_shares^2) - max(district_shares)^2)

    testthat::expect_equal(
        dplyr::select(
            res_full,
            group_decomposition,
            whole_picture_of_unequal_representation,
            disproportionality,
            intra_party_unequal_representation,
            malapportionment,
            wasted_votes
        ),
        res_contested_only
    )

    testthat::expect_equal(res_full$districts_w_contest, 1)
    testthat::expect_equal(res_full$districts_no_contest, 1)
    testthat::expect_equal(res_full$party_count, 2)
    testthat::expect_equal(res_full$candidates_with_votes_total, 2)
    testthat::expect_equal(res_full$candidates_with_votes_w_contest, 2)
    testthat::expect_equal(res_full$overall_effective_parties_lt, district_lt, tolerance = 1e-12)
    testthat::expect_equal(res_full$overall_effective_parties_molinar, district_molinar, tolerance = 1e-12)
    testthat::expect_equal(res_full$mean_effective_parties_lt, district_lt, tolerance = 1e-12)
    testthat::expect_equal(res_full$mean_effective_parties_molinar, district_molinar, tolerance = 1e-12)
    testthat::expect_equal(res_full$overall_effective_candidates_lt, district_lt, tolerance = 1e-12)
    testthat::expect_equal(res_full$overall_effective_candidates_molinar, district_molinar, tolerance = 1e-12)
    testthat::expect_equal(
        res_full$mean_effective_candidates_lt,
        district_lt,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res_full$mean_effective_candidates_molinar,
        district_molinar,
        tolerance = 1e-12
    )
    testthat::expect_false("candidates_with_votes_no_contest" %in% names(res_full))
    testthat::expect_equal(res_full$total_valid_votes, 100)
    testthat::expect_equal(res_full$total_seats_w_contest, 2)
    testthat::expect_equal(res_full$total_seats_no_contest, 1)
})

testthat::test_that("election_info adds party-vs-independent group counts", {
    res <- call_decompose(
        fixture_party_vs_independent,
        alpha = 2,
        group_decomposition = "party_vs_independent",
        election_info = TRUE
    )

    testthat::expect_equal(res$official_party_count, 2)
    testthat::expect_equal(res$independent_count, 2)
    party_shares <- c(0.4, 0.3, 0.1, 0.2)
    effective_parties_lt <- 1 / sum(party_shares^2)

    testthat::expect_equal(
        res$overall_effective_parties_lt,
        effective_parties_lt,
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res$overall_effective_parties_molinar,
        1 + effective_parties_lt^2 * (sum(party_shares^2) - max(party_shares)^2),
        tolerance = 1e-12
    )
    testthat::expect_equal(
        res$official_party_count + res$independent_count,
        res$party_count
    )
})

testthat::test_that("independent party keys do not collide with real party labels", {
    res <- call_decompose(
        fixture_independent_label_collision,
        alpha = 2,
        group_decomposition = "party_vs_independent",
        election_info = TRUE
    )
    party_shares <- c(0.4, 0.6)

    testthat::expect_equal(res$party_count, 2)
    testthat::expect_equal(res$official_party_count, 1)
    testthat::expect_equal(res$independent_count, 1)
    testthat::expect_equal(
        res$overall_effective_parties_lt,
        1 / sum(party_shares^2),
        tolerance = 1e-12
    )
})

testthat::test_that("election_info adds multicandidate-vs-singlecandidate group counts", {
    res <- call_decompose(
        fixture_multicandidate_bug_detector,
        alpha = 2,
        group_decomposition = "multicandidate_vs_singlecandidate",
        election_info = TRUE
    )

    testthat::expect_equal(res$multicandidate_party_count, 1)
    testthat::expect_equal(res$singlecandidate_party_count, 1)
    testthat::expect_false("other_count" %in% names(res))
    testthat::expect_equal(
        res$multicandidate_party_count + res$singlecandidate_party_count,
        res$party_count
    )
})

testthat::test_that("partially missing votes within a district raise an error", {
    testthat::expect_error(
        call_decompose(fixture_partially_missing_votes, alpha = 2),
        "must be either fully observed or fully missing within each district"
    )
})

testthat::test_that("all no-contest districts raise an error", {
    testthat::expect_error(
        call_decompose(fixture_all_no_contest, alpha = 2),
        "No contested districts remain"
    )
})

testthat::test_that("legacy multicandidate_vs_others mode is rejected", {
    testthat::expect_error(
        call_decompose(
            fixture_multicandidate_bug_detector,
            alpha = 2,
            group_decomposition = "multicandidate_vs_others"
        ),
        "`group_decomposition` must be one of"
    )
})

testthat::test_that("invalid alpha values raise clear errors", {
    testthat::expect_error(
        decompose_unequal_representation(
            data = fixture_identity_positive_cells,
            party_var = party,
            district_var = district,
            votes_var = votes,
            elected_var = elected,
            alpha = c(1, 2)
        ),
        "`alpha` must be a single finite numeric value."
    )

    testthat::expect_error(
        decompose_unequal_representation(
            data = fixture_identity_positive_cells,
            party_var = party,
            district_var = district,
            votes_var = votes,
            elected_var = elected,
            alpha = NA_real_
        ),
        "`alpha` must be a single finite numeric value."
    )
})

testthat::test_that("invalid scalar arguments raise clear errors", {
    testthat::expect_error(
        decompose_unequal_representation(
            data = fixture_identity_positive_cells,
            party_var = party,
            district_var = district,
            votes_var = votes,
            elected_var = elected,
            independent_label = c("A", "B")
        ),
        "`independent_label` must be a single non-missing character string."
    )

    testthat::expect_error(
        decompose_unequal_representation(
            data = fixture_identity_positive_cells,
            party_var = party,
            district_var = district,
            votes_var = votes,
            elected_var = elected,
            election_info = NA
        ),
        "`election_info` must be a single non-missing logical value."
    )
})
