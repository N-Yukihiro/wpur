#' Decompose unequal representation
#'
#' `decompose_unequal_representation()` aggregates candidate-level election results
#' to party-by-district totals and decomposes alpha-divergence between
#' seat shares and vote shares into disproportionality, malapportionment,
#' wasted votes, and intra-party unequal representation.
#'
#' Districts where `votes_var` is entirely missing are treated
#' as no-contest districts and excluded from the decomposition.
#'  Within a district, `votes_var` must be either fully observed or fully missing.
#'
#' @param data A data frame with one row per candidate.
#' @param party_var,district_var,votes_var,elected_var Unquoted column names
#'   identifying the party, district, votes, and election outcome variables.
#' @param alpha Numeric scalar controlling the alpha-divergence.
#' `alpha = 0` and `alpha = 1` use their limiting forms.
#' @param group_decomposition Character string selecting optional decomposition
#'   of party-level disproportionality. One of `"none"`,
#'   `"party_vs_independent"`, or `"multicandidate_vs_singlecandidate"`.
#' @param independent_label Character scalar used to identify independents in `party_var`.
#'   When `group_decomposition = "party_vs_independent"`,
#'   matching candidates are renumbered internally so
#'   each independent is treated as a separate party.
#' @param election_info Logical scalar indicating whether election-level
#'   metadata should be added to the returned tibble.
#'
#' @return A one-row tibble. It always includes `group_decomposition`,
#'   `whole_picture_of_unequal_representation`, `disproportionality`,
#'   `intra_party_unequal_representation`, `malapportionment`, and
#'   `wasted_votes`. Additional columns are added when grouped decomposition
#'   and/or `election_info = TRUE` are requested.
#'
#' @importFrom rlang .data
#'
#' @examples
#' election <- tibble::tribble(
#'     ~district, ~party, ~votes, ~elected,
#'     "D1", "Party A", 60, 1,
#'     "D1", "Party B", 40, 1,
#'     "D2", "Party A", 40, 1,
#'     "D2", "Party B", 60, 1
#' )
#'
#' decompose_unequal_representation(
#'     data = election,
#'     party_var = party,
#'     district_var = district,
#'     votes_var = votes,
#'     elected_var = elected,
#'     alpha = 2,
#'     election_info = TRUE
#' )
#'
#' @export
decompose_unequal_representation <- function(data,
                                             party_var,
                                             district_var,
                                             votes_var,
                                             elected_var,
                                             alpha = 2,
                                             group_decomposition = "none",
                                             independent_label = "Independent",
                                             election_info = FALSE) {
    group_decomposition <- rlang::arg_match(
        arg = group_decomposition,
        values = c(
            "none",
            "party_vs_independent",
            "multicandidate_vs_singlecandidate"
        )
    )

    validate_decompose_inputs(
        alpha = alpha,
        independent_label = independent_label,
        election_info = election_info
    )

    prepared_results <- prepare_candidate_results(
        data = data,
        party_var = {{ party_var }},
        district_var = {{ district_var }},
        votes_var = {{ votes_var }},
        elected_var = {{ elected_var }},
        independent_label = independent_label
    )

    analysis_data <- prepared_results$analysis_data
    district_status <- prepared_results$district_status

    total_votes <- sum(analysis_data$votes, na.rm = TRUE)
    total_seats <- sum(analysis_data$seats, na.rm = TRUE)

    overall_summary <- analysis_data |>
        dplyr::transmute(
            vote_share = .data$votes / total_votes,
            seat_share = .data$seats / total_seats
        ) |>
        dplyr::summarise(
            whole_picture_of_unequal_representation =
                alpha_divergence(
                    seat_share = .data$seat_share,
                    vote_share = .data$vote_share,
                    alpha = alpha
                )
        )

    party_summary <- summarise_party_results(
        analysis_data = analysis_data,
        total_votes = total_votes,
        total_seats = total_seats,
        alpha = alpha
    )

    district_summary <- summarise_district_results(
        analysis_data = analysis_data,
        total_votes = total_votes,
        total_seats = total_seats,
        alpha = alpha
    )

    result <- tibble::tibble(
        group_decomposition = group_decomposition,
        whole_picture_of_unequal_representation =
            overall_summary$whole_picture_of_unequal_representation,
        disproportionality = alpha_divergence(
            seat_share = party_summary$party_seat_share,
            vote_share = party_summary$party_vote_share,
            alpha = alpha
        ),
        intra_party_unequal_representation = sum(
            party_summary$intra_party_unequal_representation_contribution,
            na.rm = TRUE
        ),
        malapportionment = alpha_divergence(
            seat_share = district_summary$district_seat_share,
            vote_share = district_summary$district_vote_share,
            alpha = alpha
        ),
        wasted_votes = sum(
            district_summary$wasted_votes_contribution,
            na.rm = TRUE
        )
    )

    party_group_lookup <- NULL

    if (group_decomposition != "none") {
        grouped_results <- summarise_group_results(
            analysis_data = analysis_data,
            party_summary = party_summary,
            total_votes = total_votes,
            total_seats = total_seats,
            alpha = alpha,
            group_decomposition = group_decomposition,
            independent_label = independent_label
        )

        group_summary <- grouped_results$group_summary
        party_group_lookup <- grouped_results$party_group_lookup

        result <- result |>
            dplyr::mutate(
                between_group_disproportionality = alpha_divergence(
                    seat_share = group_summary$group_seat_share,
                    vote_share = group_summary$group_vote_share,
                    alpha = alpha
                ),
                weighted_within_category_disproportionality = sum(
                    group_summary$weighted_within_category_disproportionality_contribution,
                    na.rm = TRUE
                )
            ) |>
            dplyr::bind_cols(
                group_summary |>
                    dplyr::select(
                        "group_label",
                        "weighted_within_category_disproportionality_contribution"
                    ) |>
                    tidyr::pivot_wider(
                        names_from = "group_label",
                        values_from = "weighted_within_category_disproportionality_contribution",
                        names_prefix = "disproportionality_within_"
                    )
            )
    }

    if (election_info) {
        result <- append_election_info(
            result = result,
            analysis_data = analysis_data,
            district_status = district_status,
            total_votes = total_votes,
            total_seats = total_seats,
            group_decomposition = group_decomposition,
            party_group_lookup = party_group_lookup
        )
    }

    result
}
