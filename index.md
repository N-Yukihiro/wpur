# wpur

`wpur` implements the unified alpha-divergence framework proposed by
Wada and Kamahara (2024) for measuring whole picture of unequal
representation.

## Installation

You can install the development version of wpur from
[GitHub](https://github.com/) with:

``` r

# install.packages("pacman")
# pacman::p_load_gh("N-Yukihiro/wpur")
```

## Example

``` r

library(wpur)
```

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
