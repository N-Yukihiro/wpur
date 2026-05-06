#' Compute alpha-divergence between seat and vote shares
#'
#' Internal divergence function used by `decompose_unequal_representation()`.
#'
#' @keywords internal
#' @noRd
alpha_divergence <- function(seat_share, vote_share, alpha) {
    ratio <- seat_share / vote_share

    if (alpha == 0) {
        term <- vote_share * (-log(ratio))
    } else if (alpha == 1) {
        term <- dplyr::if_else(
            seat_share == 0,
            0,
            vote_share * (ratio * log(ratio))
        )
    } else {
        term <- (1 / (alpha * (alpha - 1))) *
            vote_share * (ratio^alpha - 1)
    }

    sum(term, na.rm = TRUE)
}

#' Compute the alpha-weight for seat and vote shares
#'
#' Internal divergence function used by `decompose_unequal_representation()`.
#'
#' @keywords internal
#' @noRd
alpha_weight <- function(seat_share, vote_share, alpha) {
    vote_share * (seat_share / vote_share)^alpha
}

#' Compute alpha-divergence on conditional seat and vote distributions
#'
#' Internal divergence function used by `decompose_unequal_representation()`.
#'
#' @keywords internal
#' @noRd
conditional_alpha_divergence <- function(seat, vote, alpha) {
    total_seat <- sum(seat, na.rm = TRUE)
    total_vote <- sum(vote, na.rm = TRUE)

    if (total_vote == 0) {
        return(NA_real_)
    }

    if (total_seat == 0) {
        return(0)
    }

    alpha_divergence(
        seat_share = seat / total_seat,
        vote_share = vote / total_vote,
        alpha = alpha
    )
}
