# wpur

`wpur` implements the unified alpha-divergence framework proposed by
Wada and Kamahara (2024) for measuring whole picture of unequal
representation.

## Installation

You can install the development version of wpur from
[GitHub](https://github.com/) with:

``` r

# install.packages("pacman")
pacman::p_load_gh("N-Yukihiro/wpur")
```

## Example

[`decompose_unequal_representation()`](https://n-yukihiro.github.io/wpur/reference/decompose_unequal_representation.md)
can be used with either candidate-level results or data that has already
been aggregated to party-by-district totals. When candidate-level
results are supplied, the function internally sums votes and seats by
party within each district.

``` r

library(wpur)
```

Candidate-level results can be supplied with one row per candidate.

``` r

candidate_results <- tibble::tribble(
  ~district, ~party, ~candidate, ~votes, ~elected,
  "D1", "Party A", "candidate a", 60, 1,
  "D1", "Party A", "candidate b", 60, 1,
  "D1", "Party B", "candidate c", 40, 0,
  "D2", "Party A", "candidate d", 40, 0,
  "D2", "Party B", "candidate e", 60, 1
)

decompose_unequal_representation(
  data = candidate_results,
  party_var = party,
  district_var = district,
  votes_var = votes,
  elected_var = elected,
  alpha = 2,
  group_decomposition = "none",
  election_info = FALSE
)
#> # A tibble: 1 × 6
#>   group_decomposition whole_picture_of_unequal_representation disproportionality
#>   <chr>                                                 <dbl>              <dbl>
#> 1 none                                                  0.222            0.00556
#> # ℹ 3 more variables: intra_party_unequal_representation <dbl>,
#> #   malapportionment <dbl>, wasted_votes <dbl>
```

The same election can also be supplied after aggregating votes and seats
to one row per party within each district.

``` r

party_results <- tibble::tribble(
  ~district, ~party, ~votes, ~elected,
  "D1", "Party A", 120, 2,
  "D1", "Party B", 40, 0,
  "D2", "Party A", 40, 0,
  "D2", "Party B", 60, 1
)

decompose_unequal_representation(
  data = party_results,
  party_var = party,
  district_var = district,
  votes_var = votes,
  elected_var = elected,
  alpha = 2,
  group_decomposition = "none",
  election_info = FALSE
)
#> # A tibble: 1 × 6
#>   group_decomposition whole_picture_of_unequal_representation disproportionality
#>   <chr>                                                 <dbl>              <dbl>
#> 1 none                                                  0.222            0.00556
#> # ℹ 3 more variables: intra_party_unequal_representation <dbl>,
#> #   malapportionment <dbl>, wasted_votes <dbl>
```

When the data contain multiple elections, such as multiple years, nest
the data by election and apply
[`decompose_unequal_representation()`](https://n-yukihiro.github.io/wpur/reference/decompose_unequal_representation.md)
to each nested data frame.

``` r

multi_year_results <- tibble::tribble(
  ~year, ~district, ~party, ~votes, ~elected,
  2020, "D1", "Party A", 120, 2,
  2020, "D1", "Party B", 40, 0,
  2020, "D2", "Party A", 40, 0,
  2020, "D2", "Party B", 60, 1,
  2024, "D1", "Party A", 80, 1,
  2024, "D1", "Party B", 80, 1,
  2024, "D2", "Party A", 30, 0,
  2024, "D2", "Party B", 70, 1
)

multi_year_results |>
  tidyr::nest(data = !year) |>
  dplyr::mutate(
    result = purrr::map(
      data,
      \(data_by_year) {
        decompose_unequal_representation(
          data = data_by_year,
          party_var = party,
          district_var = district,
          votes_var = votes,
          elected_var = elected,
          alpha = 2,
          group_decomposition = "none",
          election_info = FALSE
        )
      }
    )
  ) |>
  dplyr::select(!data) |>
  tidyr::unnest(result)
#> # A tibble: 2 × 7
#>    year group_decomposition whole_picture_of_unequal_repres…¹ disproportionality
#>   <dbl> <chr>                                           <dbl>              <dbl>
#> 1  2020 none                                           0.222             0.00556
#> 2  2024 none                                           0.0675            0.0165 
#> # ℹ abbreviated name: ¹​whole_picture_of_unequal_representation
#> # ℹ 3 more variables: intra_party_unequal_representation <dbl>,
#> #   malapportionment <dbl>, wasted_votes <dbl>
```
