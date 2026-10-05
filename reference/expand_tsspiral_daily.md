# Expand Time Series to Daily Resolution

Internal function that converts weekly, monthly, quarterly, and annual
observations into daily observations.

## Usage

``` r
expand_tsspiral_daily(data, frequency)
```

## Arguments

- data:

  Data frame containing `x`.

- frequency:

  Detected frequency.

## Value

A data frame with daily observations.
