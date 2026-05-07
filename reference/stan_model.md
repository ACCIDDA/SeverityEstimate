# Run A Precompiled Stan Model

Run A Precompiled Stan Model

## Usage

``` r
stan_model(model_name, ...)
```

## Arguments

- model_name:

  The name of a model in `stanmodels`.

- ...:

  Further arguments passed to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

The output of
[`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
called with the given further arguments.
