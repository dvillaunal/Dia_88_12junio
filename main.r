## ---- eval=FALSE, include=TRUE-------------------------------------------------------
## "Protocolo:
## 
##  1. Daniel Felipe Villa Rengifo
## 
##  2. Lenguaje: R
## 
##  3. Tema: ANÁLISIS GRÁFICO DE BLAND-ALTMAN EN R
## 
##  4. Fuentes:
##     https://cran.r-project.org/web/packages/BlandAltmanLeh/vignettes/Intro.html"


## ------------------------------------------------------------------------------------
## Creamos el archivo de los outputs
sink(file = "OUTPUTS.txt")

# Creamos una base dedatos hipotetica, tomando como ejemplo la base de la fuente dada:

## Creamos dos vectores con 30 datos cada uno
metodo_A <- c (1, 5, 10, 20, 50, 40, 50, 60, 70, 80, 90, 100, 150, 200, 250, 
               300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800, 850, 900,
               950, 1000)

metodo_B <- c (8, 16, 30, 24, 39, 54, 40, 68, 72, 62, 122, 80, 181, 259, 275, 
               380, 320, 434, 479, 587, 626, 648, 738, 766, 793, 851, 871, 957, 
               1001, 960)

## Creamos un vector de la difernccia de las obseraciones
diferencia <- metodo_A - metodo_B

## Creamos un vector de la media de las obseraciones
media <- (metodo_A + metodo_B) / 2

## Creamos un vector de la proporcion en porcentaje de las obseraciones
porcentaje <- ((diferencia / media) * 100)

## Creamos un data.frame con los vectores anteriores
datos <- data.frame(metodo_A, metodo_B, diferencia, media, porcentaje)

## Creamos unos estadisticos de la base de datos
summary(datos)


## ------------------------------------------------------------------------------------
#Como primer paso, inspeccionamos nuestros datos mediante la representación de un diagrama de dispersión:
library(ggplot2)

# Diagrama de dispersión con bisectriz y modelo lineal de los datos

png(filename = "DiagramaDeDispercion.png")

diagdis <- ggplot(data = datos, mapping = aes(x = metodo_A, y = metodo_B)) +
  geom_point(color = "black", size = 1) +
  labs(title = "Diagrama de dispersión", x = "método A", y = "método B") +
  geom_smooth(method = "lm", se = TRUE, color = "red", lwd = 0.5) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "blue") +
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 

diagdis

dev.off()


# Coeficiente de correlación 
cor.test(datos$metodo_A, datos$metodo_B, method = "pearson")

#El coeficiente de correlación de Pearson muestra una fuerte asociación positiva entre los valores obtenidos por el método A y B (r = 0.996, 95% CI = 0.991-0.998, p-value < 0.001). Sin embargo, una alta correlación no es necesariamente sinónimo de concordancia entre métodos, puesto que evalúa la relación y no la diferencia.


# Modelo lineal de los datos
modelo_lineal <- lm(formula = metodo_B ~ metodo_A, data = datos)
summary(modelo_lineal)


# Intervalos de confianza (CI) de los parámetros del modelo lineal
confint(modelo_lineal, level = 0.95)

# la recta de regresión y=1.04x+12.02 tiene una pendiente de 1.04, lo cual podría evaluarse como una buena concordancia entre métodos. En este caso la pendiente indica que el método B sobreestima al A. Además, los datos muestran una dispersión muy cercana a la bisectriz (pero no sobre ella), lo cual también sugiere la existencia de un pequeño nivel de discrepancia entre los métodos.




## ------------------------------------------------------------------------------------
# Obtención del gráfico Bland-Altman.
"Para obtener el dato, podemos hacerlo de dos maneras:
1. Manual
2. Automatico"

## Metodo Manual
png(filename = "BA_plot.png")

BA_plot <- ggplot(data = datos, aes(x = media, y = diferencia)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Bland-Altman plot", x = "media método A y B",
       y = "método A - método B") +
  ylim(mean(datos$diferencia) - 4 * sd(datos$diferencia),
       mean(datos$diferencia) + 4 * sd(diferencia)) +
  # Línea de bias
  geom_hline(yintercept = mean(datos$diferencia), lwd = 1) +
  # Línea en y=0
  geom_hline(yintercept = 0, lty = 3, col = "grey30") +
  # Limites de "Agreement"
  geom_hline(yintercept = mean(datos$diferencia) +
               1.96 * sd(datos$diferencia), 
               lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(datos$diferencia) -
               1.96 * sd(datos$diferencia), 
             lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(label = "Bias", x = 991, y = -18, size = 3, 
            colour = "black") +
  geom_text(label = "+1.96SD", x = 960, y = 50, size = 3, 
            colour = "firebrick") +
  geom_text(label = "-1.96SD", x = 960, y = -103, size = 3, 
            colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

BA_plot

dev.off()

# Explicación del Resultado:

"En este ejemplo, el bias entre métodos es de -27.17 unidades, lo que significa que en promedio el método B mide 27.17 unidades más que el método A (24/30 = 80% de los datos se sitúan por debajo de 0). Además, este sesgo negativo parece deberse a medidas por encima de las 200 unidades, puesto que, para unidades medias menores a 200, los datos parecen encontrarse más próximos entre sí y más próximos a una diferencia media de cero."



png(filename = "BA_plot2.png")

BA_plot +
geom_smooth(method = "lm", se = TRUE, fill = "lightgrey", lwd = 0.1, lty = 5)

dev.off()

#La línea de regresión calculada para las diferencias denota un bias sistemático proporcional (no constante), con una tendencia negativa de las diferencias conforme aumenta la magnitud de la variable medida.


# En este caso podemos calcular el % de error (proporción entre la magnitud y el error de la medida):

sd <- sd(datos$diferencia)
bias <- mean(datos$diferencia)
LoA_superior <- bias + 1.96 * sd
LoA_inferior <- bias - 1.96 * sd

# Porcentaje de error tomando como referencia el método A

error <- (LoA_superior - LoA_inferior)/mean(metodo_A) * 100
print(error)


## ------------------------------------------------------------------------------------
#También sería útil en este caso representar un gráfico utilizando el porcentaje de las diferencias:

png(filename = "PorcentajeDiferencia.png")

ggplot(data = datos, aes(x = media, y = porcentaje)) +
  geom_point(pch = 1, size = 1.5, col = "black") +
  labs(title = "Bland-Altman plot", x = "media método A y B", 
       y = "método A - método B/ media (%)") +
  ylim(mean(datos$diferencia) - 4 * sd(datos$diferencia), 
       mean(datos$diferencia) + 4 * sd(diferencia)) +
  # Línea de bias
  geom_hline(yintercept = mean(datos$porcentaje), lwd = 1) +
  # Línea en y=0
  geom_hline(yintercept = 0, lty = 3, col = "grey30") +
  # Limites de "Agreement"
  geom_hline(yintercept = mean(datos$diferencia) + 1.96 * sd(datos$diferencia), 
             lty = 2, col = "firebrick") +
  geom_hline(yintercept = mean(datos$diferencia) - 1.96 * sd(datos$diferencia), 
             lty = 2, col = "firebrick") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  geom_text(label = "Bias", x = 991, y = -10, size = 3, 
            colour = "black") +
  geom_text(label = "+1.96SD", x = 960, y = 50, size = 3, 
            colour = "firebrick") +
  geom_text(label = "-1.96SD", x = 960, y = -103, size = 3, 
            colour = "firebrick") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

dev.off()

#  El bias es de un -17.4%, casi constante para todo el rango de la variable, con la excepción de valores menores a 100 unidades. Los limites de "agreement" se sitúan del 41.05% al -95.39%.
mean(datos$porcentaje)

# Guardamos los OUTPUTS:
sink()
