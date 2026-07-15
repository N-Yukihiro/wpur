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
  `"multicandidate_vs_singlecandidate"`.

- independent_label:

  Character scalar used to identify independents in `party_var`.
  Matching candidates are always treated as separate internal parties.
  When `group_decomposition = "party_vs_independent"`, the result also
  includes a party-vs-independent split of party-level
  disproportionality.

- election_info:

  Logical scalar indicating whether election-level summary columns,
  including election counts and competition indicators, should be added
  to the returned tibble.

## Value

A one-row tibble. It always includes `group_decomposition`,
`whole_picture_of_unequal_representation`, `disproportionality`,
`intra_party_unequal_representation`, `malapportionment`, and
`wasted_votes`. Additional columns are added when grouped decomposition
and/or `election_info = TRUE` are requested.

When `election_info = TRUE`, the returned tibble also includes
election-level summary columns:

- `districts_w_contest`, `districts_no_contest`, `party_count`,
  `candidates_with_votes_total`, and `candidates_with_votes_w_contest`
  summarize the numbers of districts, parties, and candidates with
  votes.

- `overall_effective_parties_lt`, `overall_effective_parties_molinar`,
  `mean_effective_parties_lt`, `mean_effective_parties_molinar`,
  `overall_effective_candidates_lt`,
  `overall_effective_candidates_molinar`,
  `mean_effective_candidates_lt`, and
  `mean_effective_candidates_molinar` summarize party- and
  candidate-level competition. The `overall_` columns are computed over
  all contested districts, while the `mean_` columns are unweighted
  means of contested-district values. The `_lt` columns are
  Laakso-Taagepera effective counts, and the `_molinar` columns are
  Molinar indices.

- `total_valid_votes`, `total_seats_w_contest`,
  `total_seats_no_contest`, `seats_per_district`, `votes_per_seat`, and
  `votes_per_district` summarize votes and seats.

When `group_decomposition = "party_vs_independent"` and
`election_info = TRUE`, `official_party_count` and `independent_count`
are also added. When
`group_decomposition = "multicandidate_vs_singlecandidate"` and
`election_info = TRUE`, `multicandidate_party_count` and
`singlecandidate_party_count` are also added.

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
#> # A tibble: 1 × 25
#>   districts_w_contest districts_no_contest party_count overall_effective_parti…¹
#>                 <int>                <int>       <int>                     <dbl>
#> 1                   2                    0           2                         2
#> # ℹ abbreviated name: ¹​overall_effective_parties_lt
#> # ℹ 21 more variables: overall_effective_parties_molinar <dbl>,
#> #   mean_effective_parties_lt <dbl>, mean_effective_parties_molinar <dbl>,
#> #   candidates_with_votes_total <int>, candidates_with_votes_w_contest <int>,
#> #   overall_effective_candidates_lt <dbl>,
#> #   overall_effective_candidates_molinar <dbl>,
#> #   mean_effective_candidates_lt <dbl>, …
```
