# Calculate Active/Passive IFR/SIR From True IFR/SIR And Detection Rates

Calculate Active/Passive IFR/SIR From True IFR/SIR And Detection Rates

## Usage

``` r
calculate_observed_ifr_sir(
  strata,
  passive_asymptomatic_detection,
  passive_symptomatic_detection
)
```

## Arguments

- strata:

  A data.frame like object with the columns 'population', 'sir', and
  'ifr'. Any additional columns are treated as descriptions for the
  strata. If `strata` only contains the columns 'population', 'sir',
  'ifr' it's assumed that the sample data set contains no strata.

- passive_asymptomatic_detection:

  The probability of detecting a case presenting asymptomatically
  through passive surveillance.

- passive_symptomatic_detection:

  The probability of detecting a case presenting symptomatically through
  passive surveillance.

## Value

The given `strata` data.frame with the additional columns 'theta',
'active_sir', 'passive_sir', 'active_ifr', and 'passive_ifr'.

## Examples

``` r
strata <- data.frame(
  population = 1000L,
  sir = 0.15,
  ifr = 0.02
)
calculate_observed_ifr_sir(
  strata,
  passive_asymptomatic_detection = 0.2,
  passive_symptomatic_detection = 0.6
)
#>   population  sir  ifr theta active_sir passive_sir active_ifr passive_ifr
#> 1       1000 0.15 0.02  0.26       0.15   0.3937409       0.02   0.0727802
```
