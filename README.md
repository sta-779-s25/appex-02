# Appex 02


``` r
library(tidyverse)
```

## Exercise 1

``` r
set.seed(1)
n <- 10000 
sim <- tibble(
  # generate the confounder from a binomial distribution
  # with a probability 0.5 for being in either group 
  confounder = rbinom(n, 1, 0.5),
  # make the probability of exposure dependent on the 
  # confounder value
  p_exposure = case_when(
    confounder == 1 ~ 0.75,
    confounder == 0 ~ 0.25
  ),
  # generate the exposure from a binomial distribution
  # with the probability of exposure dependent on the confounder
  exposure = rbinom(n, 1, p_exposure),
  # generate the "true" average treatment effect of 0 
  # to do this, we are going to generate the potential outcomes, first 
  # the potential outcome if exposure = 0
  # (notice exposure is not in the equation below, only the confounder)
  # we use rnorm(n) to add the random error term that is normally
  # distributed with a mean of 0 and a standard deviation of 1
  y0 = confounder + rnorm(n),
  # because the true effect is 0, the potential outcome if exposure = 1
  # is identical
  y1 = y0,
  # now, in practice we will only see one of these, outcome is what is 
  # observed
  outcome = (1 - exposure) * y0 + exposure * y1,
  observed_potential_outcome = case_when(
    exposure == 0 ~ "y0",
    exposure == 1 ~ "y1"
  )
)
```

### What is the probability that exposure = 1 given confounder = 1?

``` r
sim |>
  count(exposure, confounder) |>
  filter(confounder == 1) |>
  mutate(prob_1_1 = n / sum(n))
```

    # A tibble: 2 × 4
      exposure confounder     n prob_1_1
         <int>      <int> <int>    <dbl>
    1        0          1  1220    0.246
    2        1          1  3733    0.754

### What is the probability that exposure = 0 given confounder = 1

``` r
sim |>
  count(exposure, confounder) |>
  filter(confounder == 1) |>
  mutate(prob_1_1 = n / sum(n))
```

    # A tibble: 2 × 4
      exposure confounder     n prob_1_1
         <int>      <int> <int>    <dbl>
    1        0          1  1220    0.246
    2        1          1  3733    0.754

### What is the difference in the average outcome between exposure groups?

``` r
sim |>
  group_by(exposure) |>
  summarise(avg_outcome = mean(outcome)) |>
  pivot_wider(names_from = exposure, values_from = avg_outcome) |>
  mutate(diff = `1` - `0`)
```

    # A tibble: 1 × 3
        `0`   `1`  diff
      <dbl> <dbl> <dbl>
    1 0.228 0.756 0.528

``` r
mean(sim[sim$exposure == 1, "outcome", drop = TRUE]) - 
  mean(sim[sim$exposure == 0, "outcome", drop = TRUE])
```

    [1] 0.5282931

## Exercise 2

### Stratify by confounder and calculate the average difference between exposure group within each stratum

``` r
sim |>
  group_by(confounder, exposure) |>
  summarise(avg_outcome = mean(outcome), n = n()) |>
  pivot_wider(names_from = exposure, values_from = avg_outcome:n) |>
  mutate(diff = avg_outcome_1 - avg_outcome_0,
         n = n_1 + n_0,
         diff_n = diff * n) |>
  select(confounder, diff_n) |>
  pivot_wider(names_from = confounder, values_from = diff_n) |>
  mutate(diff = (`0` + `1`) / 10000)
```

    `summarise()` has grouped output by 'confounder'. You can override using the
    `.groups` argument.

    # A tibble: 1 × 3
        `0`   `1`   diff
      <dbl> <dbl>  <dbl>
    1  141.  60.4 0.0202

How does the above compare to the “Truth” from the simulation?
