normal_parameters <- parameters(
  parameters = list(
    mu = parameter(label="mu", value=0, support=real(), fixed=FALSE),
    sigma = parameter(label="sigma", value=5, support=real(0), fixed=FALSE)
  ),
  transformed_parameters = list(
    sigma2 = transformed_parameter(
      label="sigma2", transform = expression(sigma^2)
    )
  )
)

normal_properties <- as.property(normal_parameters)
normal_properties[['parameters']] <- parameters

normal <- S7::new_class(
  "normal",
  parent = distribution_continuous,
  properties = normal_properties,
  constructor = function(mu=0, sigma=1) {
    pars <- normal_parameters
    pars@parameters[["mu"]]@value <- mu
    pars@parameters[["sigma"]]@value <- sigma
    S7::new_object(
      S7::S7_object(),
      name = "Normal",
      support = real(),
      parameters = pars,
      mu=mu, sigma=sigma
      )
  })

n <- normal(sigma=5)

S7::method(pdf, normal) <- function(distribution, x, log = FALSE) {
  stats::dnorm(x=x, mean=distribution@mu, sd=distribution@sigma, log=log)
}

# S7::method(mle, normal) <- function(distribution, x) {
#   x <- na.omit(x)
#   x_bar <- mean(x)
#   sd <- sqrt(mean((x-x_bar)^2))
#   result <- normal(mu = x_bar, sigma = sd)
#
#   return(result)
# }
