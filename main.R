library("tidyverse")
library("mosaic")
library("mosaicData")
library("mosaicCalc")
library("ggformula")
library("ggplot2")

x <- 10
y <- 5


slice_plot(3 * x - 2 ~ x, domain(x = range(0, 10)))
