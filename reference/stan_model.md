# Run A Stan Model From A Template

Run A Stan Model From A Template

## Usage

``` r
stan_model(template, template_data = list(), fun = rstan::stan, ...)
```

## Arguments

- template:

  The name of the template to render, should match a file under the
  `inst/` directory.

- template_data:

  The data to use when rendering the template.

- fun:

  The function to call on the rendered template, typically one of
  [`rstan::stan()`](https://mc-stan.org/rstan/reference/stan.html) for
  live use and
  [`rstan::stanc()`](https://mc-stan.org/rstan/reference/stanc.html) for
  unit testing.

- ...:

  Further arguments passed to the `fun` function.

## Value

The output of `fun` called with the given further arguments.
