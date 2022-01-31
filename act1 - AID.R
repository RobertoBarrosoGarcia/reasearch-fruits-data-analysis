setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Librerias
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(orca)

# FIGURA 1 ---------------------------------------------------------------------

# Importar CSV
Tendencias_de_precio_y_consumo <-
  read_csv("Tendencias-de-precio-y-consumo.csv")

# Datos necesarios por columna
Tiempo <- Tendencias_de_precio_y_consumo$'Tiempo'
Precio <- Tendencias_de_precio_y_consumo$'Millones de Euros'
Consumo <- Tendencias_de_precio_y_consumo$'Millones de kilos'

# Modelo de tendencia exponencial
# Precio
exp.model_pr <-
  lm(log(Precio) ~ Tiempo, data = Tendencias_de_precio_y_consumo)
exp_pr <- exp(fitted(exp.model_pr))

# Consumo
exp.model_con <-
  lm(log(Consumo) ~ Tiempo, data = Tendencias_de_precio_y_consumo)
exp_con <- exp(fitted(exp.model_con))

# Pintar grafica consumo vs gasto
data <- data.frame(Tiempo, Precio, Consumo, exp_pr, exp_con)

fig <-
  plot_ly(
    data,
    x = ~ Tiempo,
    y = ~ Precio,
    name = 'Precio (kg)',
    type = 'scatter',
    mode = 'lines+markers'
  ) %>%
  layout(
    title = "Relacion entre el consumo y el precio",
    xaxis = list(title = "Tiempo transcurrido"),
    yaxis = list(title = "Cantidad (en millones)")
  )
fig <- fig %>% add_trace(y = ~ Consumo, name = 'Consumo (kg)')
fig <-
  fig %>% add_trace(
    y = ~ exp_pr,
    name = 't.expPrecio',
    mode = 'lines',
    line = list(dash = 'dot')
  )
fig <-
  fig %>% add_trace(
    y = ~ exp_con,
    name = 't.exp consumo',
    mode = 'lines',
    line = list(dash = 'dot')
  )

fig
orca(fig, "Figura1.png")

# Tasa de incremento/decremento
pr = (1 - Precio[1] / Precio[length(Precio)]) * 100
con = (1 - Consumo[1] / Consumo[length(Consumo)]) * 100

# FIGURA 2 ----------------------------------------------------------------------

# Importar CSV
Comparativa_precio_consumo_cada_item <-
  read_delim(
    "Comparativa precio consumo cada item.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

# Filtro de alimentos de estudio
Comparativa_precio_consumo_cada_item = filter(
  Comparativa_precio_consumo_cada_item,
  Alimento %in% c("FRESA", "NARANJAS", "PATATAS FRESCAS", "TOMATES")
)

# Datos necesarios por columna
Alimento <- Comparativa_precio_consumo_cada_item$'Alimento'
Precio <-
  Comparativa_precio_consumo_cada_item$'Suma de Millones de Euros'
Cantidad <-
  Comparativa_precio_consumo_cada_item$'Suma de Millones de kilos'


# Pintar grafica Consumo vs Gasto separado en alimentos
data <- data.frame(Alimento, Precio, Cantidad)

fig2 <-
  plot_ly(
    data,
    x = ~ Alimento,
    y = ~ Precio,
    name = 'Precio(Euros/kg)',
    type = 'bar'
  ) %>%
  layout(title = "Relacion entre lo que consume la poblacion y su precio",
         yaxis = list(title = "Cantidad (en millones)"))
fig2 <- fig2 %>% add_trace(y = ~ Cantidad, name = 'Consumido (kg)')

fig2
orca(fig2, "Figura2.png")

# ------------------------------------------------------------------------------

# Importar CSV
Encuesta_fruta_mas_consumida <-
  read_delim(
    "Encuesta-fruta-mas-consumida.csv",
    ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

# Filtro de alimentos de estudio
Encuesta_fruta_mas_consumida = filter(
  Encuesta_fruta_mas_consumida,
  Alimento %in% c("FRESA", "NARANJAS", "PATATAS FRESCAS", "TOMATES")
)

# Datos necesarios por columna
Id <- Encuesta_fruta_mas_consumida$'ID Encuestado'
Alimento <- Encuesta_fruta_mas_consumida$'Alimento'
Puntuacion <- Encuesta_fruta_mas_consumida$'Puntuacion'

# Dataframe de la tabla
n_app = as.data.frame(table(Alimento))
Alimento = n_app$Var1
Votaciones = n_app$Freq

# Proporcion consumido vs votado
Cantidad_100 = Cantidad / sum(Cantidad)
Votaciones_100 = Votaciones / sum(Votaciones)

# Pintar grafica consumido vs votado
fig3 <-
  plot_ly(
    n_app,
    x = ~ Alimento,
    y = ~ Votaciones_100,
    name = 'Votaciones',
    type = 'bar'
  ) %>%
  layout(title = "Relacion entre lo que desea la poblacion y lo que consume",
         yaxis = list(title = "Proporcion del total"))
fig3 <- fig3 %>% add_trace(y = ~ Cantidad_100, name = 'Consumido')

fig3
orca(fig3, "Figura3.png")

# Puntuaciones -----------------------------------------------------------------

nota_Fresas = select(filter(Encuesta_fruta_mas_consumida,
                            Alimento %in% c("FRESA")),
                     'Puntuacion')
nota_Naranjas = select(filter(Encuesta_fruta_mas_consumida,
                              Alimento %in% c("NARANJAS")),
                       'Puntuacion')
nota_Patatas = select(filter(
  Encuesta_fruta_mas_consumida,
  Alimento %in% c("PATATAS FRESCAS")
),
'Puntuacion')
nota_Tomates = select(filter(Encuesta_fruta_mas_consumida,
                             Alimento %in% c("TOMATES")),
                      'Puntuacion')

fig4 <- plot_ly(
  Encuesta_fruta_mas_consumida,
  x = Encuesta_fruta_mas_consumida$Alimento,
  y = Encuesta_fruta_mas_consumida$Puntuacion,
  color = Encuesta_fruta_mas_consumida$Alimento ,
  type = "box"
) %>%
  layout(xaxis = list(title = ''),
         yaxis = list(title = 'Puntuacion'))
fig4
orca(fig4, "Figura4.png")

# Datos por pantalla
summary(nota_Fresas)
summary(nota_Naranjas)
summary(nota_Patatas)
summary(nota_Tomates)