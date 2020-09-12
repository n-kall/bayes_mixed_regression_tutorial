## ----setup, include=FALSE, echo = FALSE, message = FALSE----------------------
knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE,
                      collapse = TRUE,
                      cache = TRUE,
                      dev.args = list(bg = "transparent"),
                      fig.align = "center",
                      fig.height = 3,
                      fig.widht = 4)
library(tidyverse)
theme_set(theme_bw() + theme(plot.background=element_blank()) )

## ---- eval = F----------------------------------------------------------------
#  devtools::install_github(
#    "n-kall/faintr",
#    build_vignettes = TRUE
#  )
#  library(faintr)

## ---- echo = F----------------------------------------------------------------
library(faintr)

## ---- error=FALSE, warning=FALSE, message=FALSE-------------------------------
library(tidyverse)
data(politeness)

## -----------------------------------------------------------------------------
politeness %>%
    group_by(gender, context) %>%
    summarize(mean_pitch = mean(pitch))

## ---- error=FALSE, warning=FALSE, message=FALSE, results="hide"---------------
library(brms)
m_dummy <- brm(pitch ~ gender * context + (1 | subject + sentence), politeness)

## -----------------------------------------------------------------------------
fixef(m_dummy)

## -----------------------------------------------------------------------------
female_polite <- faintr::filter_draws(m_dummy, gender == "F", context == "pol")
male_informal <- faintr::filter_draws(m_dummy, gender == "M", context == "inf")


## -----------------------------------------------------------------------------
library(ggplot2)

diff <- pull(female_polite) - pull(male_informal)

mean(diff)

qplot(diff, geom = "density")


## -----------------------------------------------------------------------------

library(ggplot2)

bind_cols(female_polite, male_informal) %>%
  gather(key = "group") %>%
  ggplot(aes(x = value, color = group, fill = group)) +
  geom_density(alpha = 0.5)


## -----------------------------------------------------------------------------
male <- faintr::filter_draws(m_dummy, gender == "M")
female <- faintr::filter_draws(m_dummy, gender == "F")

bind_cols(female, male) %>%
  gather(key = "group") %>%
  ggplot(aes(x = value, color = group, fill = group)) +
  geom_density(alpha = 0.5)


## ---- error=FALSE, warning=FALSE, message=FALSE, results="hide"---------------
m_trt <- brm(pitch ~ 0 + gender * context + (1 | subject + sentence), politeness)

## -----------------------------------------------------------------------------
fixef(m_trt)

## -----------------------------------------------------------------------------
female_polite <- faintr::filter_draws(m_trt, gender == "F", context == "pol")
male_informal <- faintr::filter_draws(m_trt, gender == "M", context == "inf")

bind_cols(female_polite, male_informal) %>%
  gather(key = "group") %>%
  ggplot(aes(x = value, color = group, fill = group)) +
  geom_density(alpha = 0.5)


