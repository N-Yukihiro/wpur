# Decompose unequal representation

`decompose_unequal_representation()` aggregates candidate-level election
results to party-by-district totals and decomposes alpha-divergence
between seat shares and vote shares into disproportionality,
malapportionment, wasted votes, and intra-party unequal representation.

## Usage

``` r
decompose_unequal_representation(
  data,
  party_var,
  district_var,
  votes_var,
  elected_var,
  alpha = 2,
  group_decomposition = "none",
  independent_label = "Independent",
  election_info = FALSE
)
```

## Arguments

- data:

  A data frame with one row per candidate.

- party_var, district_var, votes_var, elected_var:

  Unquoted column names identifying the party, district, votes, and
  election outcome variables.

- alpha:

  Numeric scalar controlling the alpha-divergence. `alpha = 0` and
  `alpha = 1` use their limiting forms.

- group_decomposition:

  Character string selecting optional decomposition of party-level
  disproportionality. One of `"none"`, `"party_vs_independent"`, or
  `"multicandidate_vs_others"`.

- independent_label:

  Character scalar used to identify independents in `party_var`. When
  `group_decomposition = "party_vs_independent"`, matching candidates
  are renumbered internally so each independent is treated as a separate
  party.

- election_info:

  Logical scalar indicating whether election-level metadata should be
  added to the returned tibble.

## Value

A one-row tibble. It always includes `group_decomposition`,
`whole_picture_of_unequal_representation`, `disproportionality`,
`intra_party_unequal_representation`, `malapportionment`, and
`wasted_votes`. Additional columns are added when grouped decomposition
and/or `election_info = TRUE` are requested.

## Details

Districts where `votes_var` is entirely missing are treated as
no-contest districts and excluded from the decomposition. Within a
district, `votes_var` must be either fully observed or fully missing.

## Examples

``` r
election <- tibble::tribble(
    ~district, ~party, ~votes, ~elected,
    "D1", "Party A", 60, 1,
    "D1", "Party B", 40, 1,
    "D2", "Party A", 40, 1,
    "D2", "Party B", 60, 1
)

decompose_unequal_representation(
    data = election,
    party_var = party,
    district_var = district,
    votes_var = votes,
    elected_var = elected,
    alpha = 2,
    election_info = TRUE
)
#> # A tibble: 1 × 15
#>   districts_w_contest districts_no_contest party_count total_valid_votes
#>                 <int>                <int>       <int>             <dbl>
#> 1                   2                    0           2               200
#> # ℹ 11 more variables: total_seats_w_contest <dbl>,
#> #   total_seats_no_contest <dbl>, seats_per_district <dbl>,
#> #   votes_per_seat <dbl>, votes_per_district <dbl>, group_decomposition <chr>,
#> #   whole_picture_of_unequal_representation <dbl>, disproportionality <dbl>,
#> #   intra_party_unequal_representation <dbl>, malapportionment <dbl>,
#> #   wasted_votes <dbl>
```
