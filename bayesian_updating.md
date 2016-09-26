Bayesian updating demo
================
Tristan Mahr
September 20, 2016

``` r
library("dplyr")
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library("tidyr")
library("ggplot2")

# We have some IQ scores
iqs <- car::Burt$IQbio
iqs
#>  [1]  82  80  88 108 116 117 132  71  75  93  95  88 111  63  77  86  83
#> [18]  93  97  87  94  96 112 113 106 107  98

# Want to use Bayesian updating to estimate the mean of the data and the sd of
# the data.

# Use a uniform prior across a range of means and SDs
each_mean <- seq(from = 70, to = 130, by = 1)
each_sd <- 10:20
# equal probability
p_mean <- 1 / length(each_mean)
p_sd <- 1 / length(each_sd)

# We are using a grid approximation. Create every combination of mean and sd.
# Joint probability as product of each one's prior probability.
df <- data_frame(each_mean, p_mean) %>%
  expand(nesting(each_mean, p_mean), nesting(each_sd, p_sd)) %>%
  mutate(prior = p_sd * p_mean,
         posterior = prior,
         data_seen = 0)
df
#> # A tibble: 671 × 7
#>    each_mean     p_mean each_sd       p_sd       prior   posterior
#>        <dbl>      <dbl>   <int>      <dbl>       <dbl>       <dbl>
#> 1         70 0.01639344      10 0.09090909 0.001490313 0.001490313
#> 2         70 0.01639344      11 0.09090909 0.001490313 0.001490313
#> 3         70 0.01639344      12 0.09090909 0.001490313 0.001490313
#> 4         70 0.01639344      13 0.09090909 0.001490313 0.001490313
#> 5         70 0.01639344      14 0.09090909 0.001490313 0.001490313
#> 6         70 0.01639344      15 0.09090909 0.001490313 0.001490313
#> 7         70 0.01639344      16 0.09090909 0.001490313 0.001490313
#> 8         70 0.01639344      17 0.09090909 0.001490313 0.001490313
#> 9         70 0.01639344      18 0.09090909 0.001490313 0.001490313
#> 10        70 0.01639344      19 0.09090909 0.001490313 0.001490313
#> # ... with 661 more rows, and 1 more variables: data_seen <dbl>

# Update the posterior given some data
update_distribution <- function(df, data) {
  data_seen <- unique(df$data_seen) + length(data)
  
  df %>%
    # Last posterior is current prior
    select(each_mean, each_sd, prior = posterior) %>%
    # Attach data to each mean-sd pair
    expand(nesting(each_mean, each_sd, prior), points = data) %>%
    # Measure likelihood of data in each mean-sd pair
    group_by(each_mean, each_sd, prior) %>%
    summarise(likelihood = prod(dnorm(points, each_mean, each_sd))) %>%
    ungroup %>%
    # bayes theorem
    mutate(posterior = likelihood * prior / (sum(likelihood * prior)),
           data_seen = data_seen)
}

# Create each sequence of data samples
data_sequence <- Map(seq, 1, seq_along(iqs)) %>%
  lapply(function(xs) iqs[xs])

iq_sequence <- data_sequence %>%
  purrr::map(~ update_distribution(df, .x))

# include 0 data seen
iq_sequence <- c(list(df), iq_sequence)

iq_sequence %>% 
  purrr::map_dbl(~ sum(.x$posterior))
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1

df_fit <- iq_sequence %>%
  purrr::accumulate(bind_rows) %>%
  # Create animation steps
  purrr::map(. %>% mutate(step = max(data_seen)) %>%
               # Keep last five data_seen values.
               filter(step - data_seen <= 5) %>%
               # Create a column to control transparency using recency
               mutate(alpha = 1 - ((step - data_seen) / 5))) %>%
  bind_rows
df_fit
#> # A tibble: 102,663 × 10
#>    each_mean     p_mean each_sd       p_sd       prior   posterior
#>        <dbl>      <dbl>   <int>      <dbl>       <dbl>       <dbl>
#> 1         70 0.01639344      10 0.09090909 0.001490313 0.001490313
#> 2         70 0.01639344      11 0.09090909 0.001490313 0.001490313
#> 3         70 0.01639344      12 0.09090909 0.001490313 0.001490313
#> 4         70 0.01639344      13 0.09090909 0.001490313 0.001490313
#> 5         70 0.01639344      14 0.09090909 0.001490313 0.001490313
#> 6         70 0.01639344      15 0.09090909 0.001490313 0.001490313
#> 7         70 0.01639344      16 0.09090909 0.001490313 0.001490313
#> 8         70 0.01639344      17 0.09090909 0.001490313 0.001490313
#> 9         70 0.01639344      18 0.09090909 0.001490313 0.001490313
#> 10        70 0.01639344      19 0.09090909 0.001490313 0.001490313
#> # ... with 102,653 more rows, and 4 more variables: data_seen <dbl>,
#> #   step <dbl>, alpha <dbl>, likelihood <dbl>

# Also want a data-frame of the cumulative data-set as each new point is added.
# This lets us plot a rug of raw data.
data_sequence <- data_sequence %>% 
  purrr::map_df(~ data_frame(iq = .x, data_seen = length(iq))) %>% 
  group_by(data_seen) %>% 
  mutate(step = max(data_seen), sequence = seq_along(data_seen), current = sequence == max(sequence)) %>% 
  select(-sequence) %>% 
  ungroup
data_sequence
#> # A tibble: 378 × 4
#>       iq data_seen  step current
#>    <int>     <int> <int>   <lgl>
#> 1     82         1     1    TRUE
#> 2     82         2     2   FALSE
#> 3     80         2     2    TRUE
#> 4     82         3     3   FALSE
#> 5     80         3     3   FALSE
#> 6     88         3     3    TRUE
#> 7     82         4     4   FALSE
#> 8     80         4     4   FALSE
#> 9     88         4     4   FALSE
#> 10   108         4     4    TRUE
#> # ... with 368 more rows

library("gganimate")

# plot posterior of means, one line per sd level
p <- ggplot(df_fit) +
  aes(x = each_mean, y = posterior, frame = step) +
  geom_line(aes(group = interaction(each_sd, data_seen), alpha = alpha)) +
  ggtitle("Data observed:") +
  xlab("possible mean") +
  ylab("posterior probability") +
  guides(alpha = FALSE) +
  theme_grey(base_size = 14) + 
  geom_rug(aes(x = iq, y = 0), data_sequence, sides = "b") + 
  geom_rug(aes(x = iq, y = 0), filter(data_sequence, current), size = 1.5, 
           sides = "b")

animation::ani.options(interval = 1/10)
g <- gg_animate(p,  ani.height = 400, ani.width = 640)
# g
gg_animate_save(g, filename = "./updating.gif", saver = "gif",
                ani.height = 400, ani.width = 640)
#> Executing: 
#> ""convert" -loop 0 -delay 10 Rplot1.png Rplot2.png Rplot3.png
#>     Rplot4.png Rplot5.png Rplot6.png Rplot7.png Rplot8.png
#>     Rplot9.png Rplot10.png Rplot11.png Rplot12.png Rplot13.png
#>     Rplot14.png Rplot15.png Rplot16.png Rplot17.png Rplot18.png
#>     Rplot19.png Rplot20.png Rplot21.png Rplot22.png Rplot23.png
#>     Rplot24.png Rplot25.png Rplot26.png Rplot27.png Rplot28.png
#>     "updating.gif""
#> Output at: updating.gif


# plot posterior of sds, one line per mean level
p <- ggplot(df_fit) +
  aes(x = each_sd, y = posterior, frame = step) +
  geom_line(aes(group = interaction(each_mean, data_seen), alpha = alpha)) +
  ggtitle("Data observed:") +
  xlab("possible sd") +
  ylab("posterior probability") +
  guides(alpha = FALSE) +
  theme_grey(base_size = 14)

g2 <- gg_animate(p,  ani.height = 400, ani.width = 640)

gg_animate_save(g2, filename = "./updating_sds.gif", saver = "gif",
                ani.height = 400, ani.width = 640)
#> Executing: 
#> ""convert" -loop 0 -delay 10 Rplot1.png Rplot2.png Rplot3.png
#>     Rplot4.png Rplot5.png Rplot6.png Rplot7.png Rplot8.png
#>     Rplot9.png Rplot10.png Rplot11.png Rplot12.png Rplot13.png
#>     Rplot14.png Rplot15.png Rplot16.png Rplot17.png Rplot18.png
#>     Rplot19.png Rplot20.png Rplot21.png Rplot22.png Rplot23.png
#>     Rplot24.png Rplot25.png Rplot26.png Rplot27.png Rplot28.png
#>     "updating_sds.gif""
#> Output at: updating_sds.gif

p <- iq_sequence %>%
  bind_rows %>%
  ggplot(.) +
  aes(x = each_mean, y = each_sd, z = posterior, group = data_seen, frame = data_seen) +
  geom_contour() +
  ggtitle("Data observed:") +
  xlab("possible mean") +
  ylab("possible sd")
g3 <- gg_animate(p, ani.height = 400, ani.width = 640)
#> Warning in grDevices::contourLines(x = sort(unique(data$x)), y =
#> sort(unique(data$y)), : all z values are equal
#> Warning: Not possible to generate contour data

gg_animate_save(g3, filename = "./updating_contour.gif", saver = "gif",
                ani.height = 400, ani.width = 640)
#> Executing: 
#> ""convert" -loop 0 -delay 10 Rplot1.png Rplot2.png Rplot3.png
#>     Rplot4.png Rplot5.png Rplot6.png Rplot7.png Rplot8.png
#>     Rplot9.png Rplot10.png Rplot11.png Rplot12.png Rplot13.png
#>     Rplot14.png Rplot15.png Rplot16.png Rplot17.png Rplot18.png
#>     Rplot19.png Rplot20.png Rplot21.png Rplot22.png Rplot23.png
#>     Rplot24.png Rplot25.png Rplot26.png Rplot27.png
#>     "updating_contour.gif""
#> Output at: updating_contour.gif
```

![](./updating.gif) ![](./updating_sds.gif) ![](./updating_contour.gif)
