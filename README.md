# MadR_RStanARM

Slides and materials for my talk to the Madison R Users Group (September 21st, 2016).


## Things to talk about

Why I got into Bayesian stats

- Psychology's soul-searching about replication crisis made me want to level up my stats.
- Most NHST practitioners cannot correctly interpret confidence intervals or p-values.
- Started reading ARM book. It emphasized uncertainty and simulation.
  - Similar perspective: https://speakerdeck.com/jakevdp/statistics-for-hackers
  - arm package's opinionated interface. No p value.
  - Lazy posteriors: Use point estimate of model parameter and its standard error to seed a distribution. Draw from that distribution. Now you have an interval of plausible model parameters. 
  
Bayesian stats

- A frequentist model provides one model of many plausible models of the data. This model has certain properties and optimizes a certain penalty.
- A Bayesian model is a model of models. We get a distribution of plausible models of the data.
- We can quantify our uncertainty about the role of predictor by asking questions about the distribution of that predictor's parameter values.
- Priors...? They get conditioned/trained/updated by the data. We'll get back to them.

What is Stan

- A programming language for probablistic stats.
- Write out program using a very math-like syntax.
- The model is _compiled_ into an executable that does the sampling.
- Pystan and RStan are interfaces that you send/receive data from the program.
- Simple example with RStan
- But ugh, I don't want to learn a whole new programming language right now.

What is RStanArm?

- Precompiled versions of the classic regression models.
- glm -> stan_glm, glmer -> stan_glmer.
- Write your regular code. Add stan_ to the front, and add a prior.
- examples

Things to do with the package

- Fit a model
- launch_shinystan -- killer feature.
- posterior_vs_prior
- How to plot uncertainty

Other things you can do with this framework

- posterior_predict
- looic, waic

Learn more:

- Read the vignettes. They are great.
- Statistical Rethinking.
- brms, which turns R code into Stan programs that are compiled. Much more
  flexible. Compile-time penalty.
