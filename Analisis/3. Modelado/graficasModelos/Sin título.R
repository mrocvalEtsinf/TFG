# Cargar librerías necesaria

library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

# Leer el Excel
df <- openxlsx::read.xlsx("vloresTabla.xlsx")


# Convertir a formato largo
df_long <- df %>%
  pivot_longer(cols = c(Accuracy, Kappa, B.ACC, Sensibilidad, Especificidad),
               names_to = "Métrica",
               values_to = "Valor")

# Paleta personalizada por métrica
colores_metricas <- c(
  "Accuracy" = "#F96B04",
  "Kappa" = "#EF9F01",
  "B.ACC" = "#E4B815",
  "Sensibilidad" = "#80B295",
  "Especificidad" = "#37B0B4"
)

# ============================
# GRAFICOS POR CLASE – X1EVAL
# ============================
df_eval <- df_long %>% filter(Variable.respuesta == "x1eval")

ggplot(df_eval, aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_col(position = "stack") +
  geom_text(aes(label = round(Valor, 2)), 
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  facet_wrap(~Respuesta, ncol = 2) +
  scale_fill_manual(values = colores_metricas) +
  labs(title = "Evaluación inicial",
       x = "Modelo", y = "Valor de la métrica") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

# ============================
# GRAFICOS POR CLASE – MEJORRESP
# ============================
df_mejor <- df_long %>% filter(Variable.respuesta == "MejorRes")

ggplot(df_mejor, aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_col(position = "stack") +
  geom_text(aes(label = round(Valor, 2)), 
            position = position_stack(vjust = 0.5), size = 3, color = "white") +
  facet_wrap(~Respuesta, ncol = 2) +
  scale_fill_manual(values = colores_metricas) +
  labs(title = "Mejor respuesta",
       x = "Modelo", y = "Valor de la métrica") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold"))

