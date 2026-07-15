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

normalise_candidate_data <- function(data,
                                     party_var,
                                     district_var,
                                     votes_var,
                                     elected_var,
                                     independent_label) {
    data |>
        dplyr::ungroup() |>
        dplyr::transmute(
            candidate_id = dplyr::row_number(),
            party_label = as.character({{ party_var }}),
            district = {{ district_var }},
            votes = {{ votes_var }},
            elected = {{ elected_var }}
        ) |>
        dplyr::filter(
            !is.na(.data$party_label),
            !is.na(.data$district),
            !is.na(.data$elected)
        ) |>
        dplyr::mutate(
            is_independent = .data$party_label == independent_label,
            party_key = dplyr::if_else(
                .data$is_independent,
                paste0("__independent_candidate__", .data$candidate_id),
                paste0("__party__", .data$party_label)
            )
        )
}

summarise_district_status <- function(candidate_data) {
    candidate_data |>
        dplyr::summarise(
            any_vote_observed = any(!is.na(.data$votes)),
            any_vote_missing = any(is.na(.data$votes)),
            seats_in_district = sum(.data$elected, na.rm = TRUE),
            .by = dplyr::all_of("district")
        ) |>
        dplyr::mutate(
            district_no_contest = !.data$any_vote_observed
        )
}

validate_vote_missingness <- function(district_status) {
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
}

add_candidate_status <- function(candidate_data, district_status) {
    candidate_data |>
        dplyr::left_join(
            district_status |>
                dplyr::select("district", "district_no_contest"),
            by = "district"
        )
}

filter_contested_candidates <- function(candidate_status) {
    candidate_status |>
        dplyr::filter(!.data$district_no_contest)
}

summarise_analysis_data <- function(contested_candidate_data) {
    contested_candidate_data |>
        dplyr::summarise(
            votes = sum(.data$votes, na.rm = TRUE),
            seats = sum(.data$elected, na.rm = TRUE),
            candidate_count = dplyr::n(),
            party_label = dplyr::first(.data$party_label),
            is_independent = dplyr::first(.data$is_independent),
            .by = dplyr::all_of(c("district", "party_key"))
        )
}

prepare_election_context <- function(data,
                                     party_var,
                                     district_var,
                                     votes_var,
                                     elected_var,
                                     independent_label) {
    candidate_data <- normalise_candidate_data(
        data = data,
        party_var = {{ party_var }},
        district_var = {{ district_var }},
        votes_var = {{ votes_var }},
        elected_var = {{ elected_var }},
        independent_label = independent_label
    )
    district_status <- summarise_district_status(candidate_data)

    validate_vote_missingness(district_status)

    candidate_status <- add_candidate_status(
        candidate_data = candidate_data,
        district_status = district_status
    )
    contested_candidate_data <- filter_contested_candidates(candidate_status)
    analysis_data <- summarise_analysis_data(contested_candidate_data)

    if (nrow(analysis_data) == 0) {
        rlang::abort(
            "No contested districts remain after excluding districts with `votes_var = NA` (treated as no-contest districts)."
        )
    }

    list(
        candidate_data = candidate_data,
        candidate_status = candidate_status,
        contested_candidate_data = contested_candidate_data,
        analysis_data = analysis_data,
        district_status = district_status,
        total_votes = sum(analysis_data$votes, na.rm = TRUE),
        total_seats = sum(analysis_data$seats, na.rm = TRUE)
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
        group_col = "party_key",
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
                                    group_decomposition) {
    if (group_decomposition == "party_vs_independent") {
        party_group_lookup <- analysis_data |>
            dplyr::distinct(.data$party_key, .data$is_independent) |>
            dplyr::transmute(
                party_key = .data$party_key,
                group_label = dplyr::if_else(
                    .data$is_independent,
                    "independent",
                    "party"
                )
            )
    } else {
        party_group_lookup <- analysis_data |>
            dplyr::summarise(
                total_candidate_count = sum(.data$candidate_count, na.rm = TRUE),
                .by = dplyr::all_of("party_key")
            ) |>
            dplyr::transmute(
                party_key = .data$party_key,
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
            by = "party_key"
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

summarise_overall_candidate_competition <- function(election_context) {
    votes <- election_context$contested_candidate_data$votes

    tibble::tibble(
        overall_effective_candidates_lt = hill_number(votes, p = 2),
        overall_effective_candidates_molinar = molinar_index(votes)
    )
}

summarise_district_candidate_competition <- function(election_context) {
    election_context$contested_candidate_data |>
        dplyr::summarise(
            effective_candidates_lt = hill_number(.data$votes, p = 2),
            effective_candidates_molinar = molinar_index(.data$votes),
            .by = dplyr::all_of("district")
        )
}

summarise_overall_party_competition <- function(election_context) {
    party_votes <- election_context$analysis_data |>
        dplyr::summarise(
            votes = sum(.data$votes, na.rm = TRUE),
            .by = dplyr::all_of("party_key")
        )

    tibble::tibble(
        overall_effective_parties_lt = hill_number(party_votes$votes, p = 2),
        overall_effective_parties_molinar = molinar_index(party_votes$votes)
    )
}

summarise_district_party_competition <- function(election_context) {
    election_context$analysis_data |>
        dplyr::summarise(
            effective_parties_lt = hill_number(.data$votes, p = 2),
            effective_parties_molinar = molinar_index(.data$votes),
            .by = dplyr::all_of("district")
        )
}

mean_or_na <- function(x) {
    if (all(is.na(x))) {
        return(NA_real_)
    }

    mean(x, na.rm = TRUE)
}

summarise_election_info <- function(election_context) {
    candidate_status <- election_context$candidate_status
    analysis_data <- election_context$analysis_data
    district_status <- election_context$district_status
    total_votes <- election_context$total_votes
    total_seats <- election_context$total_seats

    district_count <- dplyr::n_distinct(analysis_data$district)
    party_count <- dplyr::n_distinct(analysis_data$party_key)
    districts_no_contest <- sum(district_status$district_no_contest, na.rm = TRUE)
    total_seats_no_contest <- district_status |>
        dplyr::filter(.data$district_no_contest) |>
        dplyr::summarise(total = sum(.data$seats_in_district, na.rm = TRUE)) |>
        dplyr::pull("total")
    candidates_with_votes <- candidate_status |>
        dplyr::filter(!is.na(.data$votes))
    overall_candidate_competition <- summarise_overall_candidate_competition(election_context)
    district_candidate_competition <- summarise_district_candidate_competition(election_context)
    overall_party_competition <- summarise_overall_party_competition(election_context)
    district_party_competition <- summarise_district_party_competition(election_context)

    tibble::tibble(
        districts_w_contest = district_count,
        districts_no_contest = districts_no_contest,
        party_count = party_count,
        overall_effective_parties_lt =
            overall_party_competition$overall_effective_parties_lt,
        overall_effective_parties_molinar =
            overall_party_competition$overall_effective_parties_molinar,
        mean_effective_parties_lt = mean_or_na(
            district_party_competition$effective_parties_lt
        ),
        mean_effective_parties_molinar = mean_or_na(
            district_party_competition$effective_parties_molinar
        ),
        candidates_with_votes_total = nrow(candidates_with_votes),
        candidates_with_votes_w_contest = sum(
            !candidates_with_votes$district_no_contest,
            na.rm = TRUE
        ),
        overall_effective_candidates_lt =
            overall_candidate_competition$overall_effective_candidates_lt,
        overall_effective_candidates_molinar =
            overall_candidate_competition$overall_effective_candidates_molinar,
        mean_effective_candidates_lt = mean_or_na(
            district_candidate_competition$effective_candidates_lt
        ),
        mean_effective_candidates_molinar = mean_or_na(
            district_candidate_competition$effective_candidates_molinar
        ),
        total_valid_votes = total_votes,
        total_seats_w_contest = total_seats,
        total_seats_no_contest = total_seats_no_contest,
        seats_per_district = total_seats / district_count,
        votes_per_seat = total_votes / total_seats,
        votes_per_district = total_votes / district_count
    )
}

summarise_group_election_info <- function(party_group_lookup, group_decomposition) {
    if (is.null(party_group_lookup)) {
        return(tibble::tibble())
    }

    count_group <- function(group_label) {
        sum(party_group_lookup$group_label == group_label, na.rm = TRUE)
    }

    if (group_decomposition == "party_vs_independent") {
        return(tibble::tibble(
            official_party_count = count_group("party"),
            independent_count = count_group("independent")
        ))
    }

    if (group_decomposition == "multicandidate_vs_singlecandidate") {
        return(tibble::tibble(
            multicandidate_party_count = count_group("multicandidate"),
            singlecandidate_party_count = count_group("singlecandidate")
        ))
    }

    rlang::abort("Unsupported `group_decomposition` for grouped election info.")
}
