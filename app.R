# Points of Significance - Uncertainty and the modelling and management of infectious epidemics
# Katriona Shea (1), Ottar Bjørnstad (1,2), Martin Krzywinski (3*), Naomi Altman (4)
#
# 1. Department of Biology, The Pennsylvania State University, State College, PA, USA.
# 2. Department of Entomology, The Pennsylvania State University, State College, PA, USA.
# 3. Canada’s Michael Smith Genome Sciences Centre, Vancouver, British Columbia, Canada.
# 4. Department of Statistics, The Pennsylvania State University, State College, PA, USA.
# * martink@bcgsc.ca
#
# CITATION
# Shea, K., Bjørnstad, O., Krzywinski, M. & Altman, N. 
# Points of Significance: Uncertainty and the modelling and management of infectious epidemics (2020) Nature Methods 17 (in press).
#
# DOWNLOAD
# https://martinkrz.github.io/posepi3
#
# REMOTE ACCESS
#
# http://shiny.bcgsc.ca/posepi3
#
# RUN LOCALLY
#
# 1. Requirements
#
library(shiny)
library(deSolve)
library(ggplot2)
#library(grid)
library(stringr)
library(tidyverse)
library(shinyjs)
library(shinyWidgets)
#
# 2. Getting started
#
# Load this file in R Studio and click "Run App" in top right of this pane.
#
# CHANGELOG
#
# 22-07-2020  spawned from posepi3

# CUSTOM SETTINGS
# The colors of the SEIR groups and some greys (Cn)
palette              = c(S="#333333",
                         E="#a0d848",
                         I="#f15a24",
                         R="#29abe2",
                         G="#8cc63f",
                         M="#ed1e79",
                         MD="#720f3f",
                         C="#6eccdc",
                         CD="#3f747a",
                         C1="#333333",
                         C2="#cccccc",
                         C3="#cccccc",
                         M1="#c7b299",
                         M2="#ffff00",
                         p1="#b3b3b3",
                         p2="#8cc63f",
                         p3="#006837")
debug = TRUE
# Plot height and line width for trajectories
plot_line_width      = 1.5
plot_text_size       = 12
# Initial value for I(0). S(0) = 1 - sir_init_i - vaccination_fraction and R(0) = vaccination_fraction
sir_init_i           = 0.001
sir_system_steps     = 1000
sir_system_time_step = 0.01 # last resort default, shouldn't be used
# Infectious period max and default. Slider step is 1.
ip_max               = 28
ip_default           = 7
ip_step              = 1
# R0 max and slider step
R0_default           = 4
R0M_default          = 1.5
R0_min               = 1.1
R0_max               = 5
R0_step              = 0.1
R0_A1   = 1.5
R0_A2   = 2.5
R0_Amax = 4
R0_step2 = 0.2
# Latent period max and default. Slider step is 1.
lp_max               = 28
lp_default           = 7
lp_min               = 1
lp_step              = 1
# Immunity duration max and default. Slider step is 0.1. Units are years.
id_min               = 1
id_max               = 5
idshort_default      = 1
idlong_default       = 2
id_step              = 0.1
# Life expectancy max and default. Slider step is 1. Units are years.
le_min               = 10
le_max               = 100
le_default           = 76
le_step              = 1
# Alpha
al_min               = 0
al_max               = 1000
al_default           = 0
al_step              = 1
#
p_min  = 0
p_max  = 99
p_step = 1
p_A1   = 30
p_A2   = 45
# max time in plots
tmax_min             = 1
tmax_max             = 10
tmax_default         = 4
tmax_step            = 0.1
# flu 
tflu_start    = 304
tflu_duration = 122
# vaccination plot
p3_min = 0
p3_max = 0.9
p3_step = 0.1
# do timings
do_timing            = FALSE

interpretive_default = FALSE
captions_default     = FALSE

if(debug) {
  sir_system_steps     = 200
  R0_step2             = 0.5
  interpretive_default = TRUE
  captions_default     = TRUE
  do_timing = TRUE
}
source("R/format.helpers.R",local=TRUE)
source("R/ggplot.helpers.R",local=TRUE)
source("R/ui.R",local=TRUE)
source("R/server.R",local=TRUE)

shinyApp(ui,server)
