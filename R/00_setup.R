# R/00_setup.R — carga paquetes y utilidades
pkgs <- c("readxl","dplyr","tidyr","forcats","ggplot2","scales",
          "naniar","corrplot","Hmisc","GGally","plotly","leaflet",
          "DT","reactable","crosstalk","patchwork")

invisible(lapply(pkgs, function(p) {
  if (!requireNamespace(p, quietly = TRUE))
    install.packages(p)
  suppressPackageStartupMessages(library(p, character.only = TRUE))
}))

source("R/utils.R")