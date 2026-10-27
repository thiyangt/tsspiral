# Sri Lanka Tourism Time Series

Monthly tourism-related observations for Sri Lanka, consisting of a date
variable and the corresponding observed value.

## Usage

``` r
data(tourism.srilanka)
```

## Format

A data frame with 2 columns:

- Date:

  Date of the observation.

- Value:

  Observed tourism value for the corresponding date.

## Source

Data compiled from publicly available Sri Lankan tourism statistics.

## Examples

``` r
data(tourism.srilanka)
head(tourism.srilanka)
#> # A tibble: 6 × 2
#>   Date       Value
#>   <chr>      <dbl>
#> 1 01.01.2023  2246
#> 2 02.01.2023  3633
#> 3 03.01.2023  2982
#> 4 04.01.2023  2694
#> 5 05.01.2023  3512
#> 6 06.01.2023  3035
```
