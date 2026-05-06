validate_decompose_inputs <- function(alpha, independent_label, election_info) {
    if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || !is.finite(alpha)) {
        rlang::abort("`alpha` must be a single finite numeric value.")
    }

    if (!is.character(independent_label) || length(independent_label) != 1L || is.na(independent_label)) {
        rlang::abort("`independent_label` must be a single non-missing character string.")
    }

    if (!is.logical(election_info) || length(election_info) != 1L || is.na(election_info)) {
        rlang::abort("`election_info` must be a single non-missing logical value.")
    }
}

prepare_candidate_results <- function(data,
                                      party_var,
                                      district_var,
                                      votes_var,
                                      elected_var,
                                      independent_label) {
    candidate_data <- data |>
        dplyr::transmute(
            party_raw = {{ party_var }},
            district = {{ district_var }},
            votes = {{ votes_var }},
            elected = {{ elected_var }}
        ) |>
        dplyr::filter(
            !is.na(.data$party_raw),
            !is.na(.data$district),
            !is.na(.data$elected)
        )

    district_status <- candidate_data |>
        dplyr::group_by(.data$district) |>
        dplyr::summarise(
            any_vote_observed = any(!is.na(.data$votes)),
            any_vote_missing = any(is.na(.data$votes)),
            seats_in_district = sum(.data$elected, na.rm = TRUE),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            district_no_contest = !.data$any_vote_observed
        )

    partially_missing_vote_districts <- district_status |>
        dplyr::filter(
            .data$any_vote_observed & .data$any_vote_missing
        )

    if (nrow(partially_missing_vote_districts) > 0) {
        rlang::abort(
            paste0(
                "`votes_var` must be either fully observed or fully missing within each district. ",
                "Found district(s) with partially missing votes."
            )
        )
    }

    contested_districts <- district_status |>
        dplyr::filter(!.data$district_no_contest) |>
        dplyr::select("district")

    analysis_data <- candidate_data |>
        dplyr::semi_join(
            contested_districts,
            by = "district"
        ) |>
        dplyr::mutate(
            party_raw = as.character(.data$party_raw),
            party = dplyr::if_else(
                .data$party_raw == independent_label,
                paste0(
                    independent_label,
                    cumsum(.data$party_raw == independent_label)
                ),
                .data$party_raw
            )
        ) |>
        dplyr::group_by(.data$district, .data$party) |>
        dplyr::summarise(
            votes = sum(.data$votes, na.rm = TRUE),
            seats = sum(.data$elected, na.rm = TRUE),
            candidate_count = dplyr::n(),
            .groups = "drop"
        )

    if (nrow(analysis_data) == 0) {
        rlang::abort(
            "No contested districts remain after excluding districts with `votes_var = NA` (treated as no-contest districts)."
        )
    }

    list(
        analysis_data = analysis_data,
        district_status = district_status
    )
}

summarise_party_results <- function(analysis_data, total_votes, total_seats, alpha) {
    analysis_data |>
        dplyr::group_by(.data$party) |>
        dplyr::summarise(
            party_vote_total = sum(.data$votes, na.rm = TRUE),
            party_seat_total = sum(.data$seats, na.rm = TRUE),
            district_divergence_within_party = conditional_alpha_divergence(
                seat = .data$seats,
                vote = .data$votes,
                alpha = alpha
            ),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            party_vote_share = .data$party_vote_total / total_votes,
            party_seat_share = .data$party_seat_total / total_seats,
            party_alpha_weight = alpha_weight(
                seat_share = .data$party_seat_share,
                vote_share = .data$party_vote_share,
                alpha = alpha
            ),
            intra_party_unequal_representation_contribution =
                .data$party_alpha_weight *
                    .data$district_divergence_within_party
        )
}

summarise_district_results <- function(analysis_data, total_votes, total_seats, alpha) {
    analysis_data |>
        dplyr::group_by(.data$district) |>
        dplyr::summarise(
            district_vote_total = sum(.data$votes, na.rm = TRUE),
            district_seat_total = sum(.data$seats, na.rm = TRUE),
            party_divergence_within_district = conditional_alpha_divergence(
                seat = .data$seats,
                vote = .data$votes,
                alpha = alpha
            ),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            district_vote_share = .data$district_vote_total / total_votes,
            district_seat_share = .data$district_seat_total / total_seats,
            district_alpha_weight = alpha_weight(
                seat_share = .data$district_seat_share,
                vote_share = .data$district_vote_share,
                alpha = alpha
            ),
            wasted_votes_contribution =
                .data$district_alpha_weight *
                    .data$party_divergence_within_district
        )
}

summarise_group_results <- function(analysis_data,
                                    party_summary,
                                    total_votes,
                                    total_seats,
                                    alpha,
                                    group_decomposition,
                                    independent_label) {
    if (group_decomposition == "party_vs_independent") {
        party_group_lookup <- analysis_data |>
            dplyr::distinct(.data$party) |>
            dplyr::mutate(
                group_label = dplyr::if_else(
                    stringr::str_detect(
                        string = as.character(.data$party),
                        pattern = paste0("^", stringr::str_escape(independent_label), "\\d*$")
                    ),
                    "independent",
                    "party"
                )
            )
    } else {
        party_group_lookup <- analysis_data |>
            dplyr::group_by(.data$party) |>
            dplyr::summarise(
                total_candidate_count = sum(.data$candidate_count, na.rm = TRUE),
                .groups = "drop"
            ) |>
            dplyr::transmute(
                party = .data$party,
                group_label = dplyr::if_else(
                    .data$total_candidate_count > 1,
                    "multicandidate",
                    "other"
                )
            )
    }

    group_summary <- party_summary |>
        dplyr::left_join(
            party_group_lookup,
            by = "party"
        ) |>
        dplyr::group_by(.data$group_label) |>
        dplyr::summarise(
            group_vote_total = sum(.data$party_vote_total, na.rm = TRUE),
            group_seat_total = sum(.data$party_seat_total, na.rm = TRUE),
            within_group_disproportionality = conditional_alpha_divergence(
                seat = .data$party_seat_total,
                vote = .data$party_vote_total,
                alpha = alpha
            ),
            .groups = "drop"
        ) |>
        dplyr::mutate(
            group_vote_share = .data$group_vote_total / total_votes,
            group_seat_share = .data$group_seat_total / total_seats,
            group_alpha_weight = alpha_weight(
                seat_share = .data$group_seat_share,
                vote_share = .data$group_vote_share,
                alpha = alpha
            ),
            weighted_within_group_disproportionality_contribution =
                .data$group_alpha_weight *
                    .data$within_group_disproportionality
        )

    list(
        group_summary = group_summary,
        party_group_lookup = party_group_lookup
    )
}

append_election_info <- function(result,
                                 analysis_data,
                                 district_status,
                                 total_votes,
                                 total_seats,
                                 group_decomposition,
                                 party_group_lookup = NULL) {
    district_count <- dplyr::n_distinct(analysis_data$district)
    party_count <- dplyr::n_distinct(analysis_data$party)
    districts_no_contest <- sum(district_status$district_no_contest, na.rm = TRUE)
    total_seats_no_contest <- district_status |>
        dplyr::filter(.data$district_no_contest) |>
        dplyr::summarise(total = sum(.data$seats_in_district, na.rm = TRUE)) |>
        dplyr::pull("total")

    result <- result |>
        dplyr::mutate(
            districts_w_contest = district_count,
            districts_no_contest = districts_no_contest,
            party_count = party_count,
            total_valid_votes = total_votes,
            total_seats_w_contest = total_seats,
            total_seats_no_contest = total_seats_no_contest,
            seats_per_district = total_seats / district_count,
            votes_per_seat = total_votes / total_seats,
            votes_per_district = total_votes / district_count,
            .before = 1
        )

    if (group_decomposition == "party_vs_independent" && !is.null(party_group_lookup)) {
        result <- result |>
            dplyr::mutate(
                official_party_count = sum(
                    party_group_lookup$group_label == "party",
                    na.rm = TRUE
                ),
                independent_count = sum(
                    party_group_lookup$group_label == "independent",
                    na.rm = TRUE
                ),
                .after = party_count
            )
    }

    if (group_decomposition == "multicandidate_vs_others" && !is.null(party_group_lookup)) {
        result <- result |>
            dplyr::mutate(
                multicandidate_party_count = sum(
                    party_group_lookup$group_label == "multicandidate",
                    na.rm = TRUE
                ),
                other_count = sum(
                    party_group_lookup$group_label == "other",
                    na.rm = TRUE
                ),
                .after = party_count
            )
    }

    result
}
