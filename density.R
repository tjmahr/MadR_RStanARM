library("ggplot2")
library("dplyr")

# Some toy data
davis <- car::Davis %>%
  filter(100 < height)

# Fit a model and estimate mean at five points
m <- lm(weight ~ height, davis)
newdata <- data_frame(height = seq(150, 190, length.out = 20))
newdata$fit <- predict(m, newdata)

# get density of random normal values
get_density_df <- function(mean, sd, steps) {
  ends <- qnorm(c(.001, .999), mean, sd)
  steps <- seq(ends[1], ends[2], length.out = steps)

  df <- data_frame(
    value = steps,
    density = dnorm(steps, mean, sd))
  df
}

# Get a distribution at each mean
simulated <- newdata %>%
  group_by(height) %>%
  do(get_density_df(.$fit, sigma(m), 10000)) %>%
  ungroup


ggplot(simulated) +
  # Plot at each mean, adding some scaled value of density to the mean.
  aes(x = height - (100 * density), y = value, group = height) +
  geom_polygon(fill = "grey50") +
  # raw data
  geom_point(aes(height, weight), data = davis)

ggplot(simulated) +
  aes(x = height + (150 * density), y = value, group = height) +
  geom_polygon(fill = "grey80", color = "skyblue") +
  geom_point(aes(height, weight), data = davis)


ggplot(simulated) +
  aes(x = height + (150 * density), y = value, group = height) +
  geom_path(color = "skyblue") +
  geom_point(aes(height, weight), data = davis)


newdata <- data_frame(height = c(155, 165, 175, 185))
newdata$fit <- predict(m, newdata)
# Get a distribution at each mean
simulated <- newdata %>%
  group_by(height) %>%
  do(get_density_df(.$fit, sigma(m), 10000)) %>%
  ungroup

ggplot(simulated) +
  aes(x = height + (150 * density), y = value, group = height) +
  geom_polygon(fill = "grey80", color = "skyblue") +
  geom_point(aes(height, weight), data = davis) +
  xlab("Height") +
  ylab("Weight")

ggplot(davis) +
  aes(height, weight) +
  geom_polygon(aes(x = height + (150 * density), y = value, group = height),
               data = simulated,
               fill = "grey80", color = "skyblue") +
  stat_smooth(aes(x = height + (150 * density), y = value, group = height),
              data = simulated, method = "lm", se = FALSE) +
  stat_smooth(method = "lm")


  geom_point(, data = davis) +



  xlab("Height") +
  ylab("Weight")
