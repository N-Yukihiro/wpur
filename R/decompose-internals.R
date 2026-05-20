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
        dplyr::summarise(
            any_vote_observed = any(!is.na(.data$votes)),
            any_vote_missing = any(is.na(.data$votes)),
            seats_in_district = sum(.data$elected, na.rm = TRUE),
            .by = dplyr::all_of("district")
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
        dplyr::summarise(
            votes = sum(.data$votes, na.rm = TRUE),
            seats = sum(.data$elected, na.rm = TRUE),
            candidate_count = dplyr::n(),
            .by = dplyr::all_of(c("district", "party"))
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

summarise_axis_results <- function(analysis_data,
                                   group_col,
                                   vote_col,
                                   seat_col,
                                   total_votes,
                                   total_seats,
                                   alpha,
                                   prefix,
                                   divergence_name,
                                   contribution_name) {
    rename_map <- rlang::set_names(
        rlang::syms(c(
            "total_votes_axis",
            "total_seats_axis",
            "within_divergence",
            "vote_share_axis",
            "seat_share_axis",
            "alpha_weight_axis",
            "contribution_axis"
        )),
        c(
            paste0(prefix, "_vote_total"),
            paste0(prefix, "_seat_total"),
            divergence_name,
            paste0(prefix, "_vote_share"),
            paste0(prefix, "_seat_share"),
            paste0(prefix, "_alpha_weight"),
            contribution_name
        )
    )

    analysis_data |>
        dplyr::summarise(
            total_votes_axis = sum(.data[[vote_col]], na.rm = TRUE),
            total_seats_axis = sum(.data[[seat_col]], na.rm = TRUE),
            within_divergence = conditional_alpha_divergence(
                seat = .data[[seat_col]],
                vote = .data[[vote_col]],
                alpha = alpha
            ),
            .by = dplyr::all_of(group_col)
        ) |>
        dplyr::mutate(
            vote_share_axis = .data$total_votes_axis / total_votes,
            seat_share_axis = .data$total_seats_axis / total_seats,
            alpha_weight_axis = alpha_weight(
                seat_share = .data$seat_share_axis,
                vote_share = .data$vote_share_axis,
                alpha = alpha
            ),
            contribution_axis =
                .data$alpha_weight_axis *
                    .data$within_divergence
        ) |>
        dplyr::rename(!!!rename_map)
}

summarise_party_results <- function(analysis_data, total_votes, total_seats, alpha) {
    summarise_axis_results(
        analysis_data = analysis_data,
        group_col = "party",
        vote_col = "votes",
        seat_col = "seats",
        total_votes = total_votes,
        total_seats = total_seats,
        alpha = alpha,
        prefix = "party",
        divergence_name = "district_divergence_within_party",
        contribution_name = "intra_party_unequal_representation_contribution"
    )
}

summarise_district_results <- function(analysis_data, total_votes, total_seats, alpha) {
    summarise_axis_results(
        analysis_data = analysis_data,
        group_col = "district",
        vote_col = "votes",
        seat_col = "seats",
        total_votes = total_votes,
        total_seats = total_seats,
        alpha = alpha,
        prefix = "district",
        divergence_name = "party_divergence_within_district",
        contribution_name = "wasted_votes_contribution"
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
            dplyr::summarise(
                total_candidate_count = sum(.data$candidate_count, na.rm = TRUE),
                .by = dplyr::all_of("party")
            ) |>
            dplyr::transmute(
                party = .data$party,
                group_label = dplyr::if_else(
                    .data$total_candidate_count > 1,
                    "multicandidate",
                    "singlecandidate"
                )
            )
    }

    group_summary <- party_summary |>
        dplyr::left_join(
            party_group_lookup,
            by = "party"
        ) |>
        summarise_axis_results(
            group_col = "group_label",
            vote_col = "party_vote_total",
            seat_col = "party_seat_total",
            total_votes = total_votes,
            total_seats = total_seats,
            alpha = alpha,
            prefix = "group",
            divergence_name = "within_group_disproportionality",
            contribution_name = "weighted_within_category_disproportionality_contribution"
        ) |>
        dplyr::arrange(.data$group_label)

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

    if (group_decomposition == "multicandidate_vs_singlecandidate" && !is.null(party_group_lookup)) {
        result <- result |>
            dplyr::mutate(
                multicandidate_party_count = sum(
                    party_group_lookup$group_label == "multicandidate",
                    na.rm = TRUE
                ),
                singlecandidate_party_count = sum(
                    party_group_lookup$group_label == "singlecandidate",
                    na.rm = TRUE
                ),
                .after = party_count
            )
    }

    result
}
