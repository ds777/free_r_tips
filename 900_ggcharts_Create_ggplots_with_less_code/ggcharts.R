
# Install packages

#install.packages("ggcharts")  # Better install dev version https://thomas-neitmann.github.io/ggcharts/index.html


# Load libraries

library(dplyr)
library(ggplot2)
library(ggcharts)
library(tidytext)
data("biomedicalrevenue")


# Set working directory
setwd("~/Github/00-DS/free_r_tips/900_ggcharts_Create_ggplots_with_less_code")

# Comparative Ranking in 3 years: Long coding

biomedicalrevenue %>%
  filter(year %in% c(2012, 2015, 2018)) %>%
  group_by(year) %>%
  top_n(10, revenue) %>%
  ungroup() %>%
  mutate(company = tidytext::reorder_within(company, revenue, year)) %>%
  ggplot(aes(company, revenue)) +
  geom_col() +
  coord_flip() +
  tidytext::scale_x_reordered() +
  facet_wrap(vars(year), scales = "free_y")



# Comparative Ranking in 3 years: Short code using ggcharts


biomedicalrevenue %>%
  filter(year %in% c(2012, 2015, 2018)) %>%
  bar_chart(x = company, y = revenue, facet = year, top_n = 10)


# Comparative ranking: before, after, and change

data(popeurope)
dumbbell_chart(popeurope, country, pop1952, pop2007)



data("popeurope")
dumbbell_chart(
  data = popeurope,
  x = country,
  y1 = pop1952,
  y2 = pop2007,
  top_n = 10,
  point_colors = c("lightgray", "#494F5C")
) +
  labs(
    x = NULL,
    y = "Population",
    title = "Europe's Largest Countries by Population in 2007"
  ) +
  scale_y_continuous(
    limits = c(0, NA),
    labels = function(x) paste(x, "Mn.")
  )

# Time series
source("orginalFunctions/line_chart.R")
source("orginalFunctions/ggcharts_global.R")
source("orginalFunctions/matplotlib_colors.R")
source("orginalFunctions/utils.R")
source("orginalFunctions/theme.R")
source("orginalFunctions/pre_process_data.R")
source("orginalFunctions/post_process_plot.R")
source("orginalFunctions/zzz.R")
source("orginalFunctions/reorder_within.R")
source("orginalFunctions/scale.R")

biomedicalrevenue %>%
    filter(company %in% c("Roche", "Novartis", "Bayer")) %>%
    line_chart(year, revenue, group = company)

# Modern bar lines

biomedicalrevenue %>%
    filter(year == 2018) %>%
    lollipop_chart(x = company, y = revenue, threshold = 30) +
    labs(
        x = NULL,
        y = "Revenue",
        title = "Biomedical Companies with Revenue > $30Bn."
    ) +
    scale_y_continuous(
        labels = function(x) paste0("$", x, "Bn."),
        expand = expansion(mult = c(0, .05))
    )

# Diverging bars
data(mtcars)
mtcars_z <- dplyr::transmute(
    .data = mtcars,
    model = row.names(mtcars),
    hpz = scale(hp)
)

diverging_bar_chart(data = mtcars_z, x = model, y = hpz)


# Diverging lollipop

diverging_lollipop_chart(
    data = mtcars_z,
    x = model,
    y = hpz,
    lollipop_colors = c("#006400", "#b32134"),
    text_color = c("#006400", "#b32134")
)

# Pyramid

data("popch")
pyramid_chart(data = popch, x = age, y = pop, group = sex)
