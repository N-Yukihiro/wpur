hill_number <- function(votes, p = 2) {
    if (!is.numeric(p) || length(p) != 1L || is.na(p) || !is.finite(p) || p < 0) {
        rlang::abort("`p` must be a single non-negative finite numeric value.")
    }

    positive_votes <- votes[!is.na(votes) & votes > 0]
    total_votes <- sum(positive_votes, na.rm = TRUE)

    if (total_votes <= 0) {
        return(NA_real_)
    }

    shares <- positive_votes / total_votes

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

molinar_index <- function(votes) {
    positive_votes <- votes[!is.na(votes) & votes > 0]
    total_votes <- sum(positive_votes, na.rm = TRUE)

    if (total_votes <= 0) {
        return(NA_real_)
    }

    shares <- positive_votes / total_votes
    lt <- hill_number(positive_votes, p = 2)

    1 + lt^2 * (sum(shares^2) - max(shares)^2)
}
