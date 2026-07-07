positive_vote_shares <- function(votes) {
    positive_votes <- votes[!is.na(votes) & votes > 0]
    total_votes <- sum(positive_votes, na.rm = TRUE)

    if (total_votes <= 0) {
        return(numeric())
    }

    positive_votes / total_votes
}

hill_number_from_shares <- function(shares, p) {
    if (p == 0) {
        return(as.numeric(length(shares)))
    }

    if (p == 1) {
        return(exp(-sum(shares * log(shares))))
    }

    if (p == 2) {
        return(1 / sum(shares^2))
    }

    (sum(shares^p))^(1 / (1 - p))
}

hill_number <- function(votes, p = 2) {
    if (!is.numeric(p) || length(p) != 1L || is.na(p) || !is.finite(p) || p < 0) {
        rlang::abort("`p` must be a single non-negative finite numeric value.")
    }

    shares <- positive_vote_shares(votes)

    if (length(shares) == 0) {
        return(NA_real_)
    }

    hill_number_from_shares(shares = shares, p = p)
}

molinar_index <- function(votes) {
    shares <- positive_vote_shares(votes)

    if (length(shares) == 0) {
        return(NA_real_)
    }

    lt <- hill_number_from_shares(shares = shares, p = 2)

    1 + lt^2 * (sum(shares^2) - max(shares)^2)
}
