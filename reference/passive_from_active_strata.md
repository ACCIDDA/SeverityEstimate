# Calculate The Passively Observed SIR, IFR From Actively Observed SIR, IFR

Calculate The Passively Observed SIR, IFR From Actively Observed SIR,
IFR

## Usage

``` r
passive_from_active_strata(
  strata,
  active_detection,
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

- active_detection:

  The probability of detecting a case through active detection.

- passive_asymptomatic_detection:

  The probability of detecting a case presenting asymptomatically
  through passive surveillance.

- passive_symptomatic_detection:

  The probability of detecting a case presenting symptomatically through
  passive surveillance.

## Value

A data.frame of the same structure as `strata` but with the 'sir' and
'ifr' columns adjusted for being passively observed.
