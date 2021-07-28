library(dplyr)
library(ggplot2)

perfil_incidencia_11 <- read.csv("analisis_imagenes/data/perfil_incidente_11.csv")
perfil_incidencia_31 <- read.csv("analisis_imagenes/data/perfil_incidente_31.csv")
perfil_incidencia_61 <- read.csv("analisis_imagenes/data/perfil_incidente_61.csv")
perfil_reflejado_11 <- read.csv("analisis_imagenes/data/perfil_reflejado_11.csv")
perfil_reflejado_31 <- read.csv("analisis_imagenes/data/perfil_reflejado_31.csv")
perfil_reflejado_61 <- read.csv("analisis_imagenes/data/perfil_reflejado_61.csv")


#' Esta función permite obtener por medio de bootstrap, un valor certero para 
#' el valor de la intensidad del laser, además entrega la grafica del histograma
#' y boxplot de dicho procedimiento estadístico.
bootstrap_analisis <- function(perfil){
  bootstrap <- replicate(n=10000, sample(perfil$luma, replace = TRUE))
  maximos <- apply(bootstrap, MARGIN = 2, FUN = max)
  hist_bootstrap <- ggplot() + 
    geom_histogram(aes(x = maximos), color="black", binwidth = 0.5)+
    ylab("Frecuencia") +
    xlab("Máximos")+
    ggtitle("Histograma de los Máximos")
  boxplot_bootstrap <- ggplot() +
    geom_boxplot(
      aes(x=maximos),
      # custom boxes
      color="blue",
      fill="blue",
      alpha=0.2,
      
      # Notch?
      notch=TRUE,
      notchwidth = 0.8,
      
      # custom outliers
      outlier.colour="red",
      outlier.fill="red",
      outlier.size=3
      
    ) +
    xlab("Máximos") +
    ggtitle("Boxplot de los Máximos")
  print(hist_bootstrap)
  print(boxplot_bootstrap)
  print(summary(maximos))
  print(t.test(maximos))
}

bootstrap_analisis(perfil_incidencia_11)
bootstrap_analisis(perfil_incidencia_31)
bootstrap_analisis(perfil_incidencia_61)
bootstrap_analisis(perfil_reflejado_11)
bootstrap_analisis(perfil_reflejado_31)
bootstrap_analisis(perfil_reflejado_61)

