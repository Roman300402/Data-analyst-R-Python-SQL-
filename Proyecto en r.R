# Autores: Roman Dominguez Solis, Edel Araiza
library(summarytools)
library(haven)
library(psych)
library(tidyverse)
# library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(gridExtra)

library(tseries)
library(car)
library(lmtest)
library(sandwich)
library(nortest)
library(performance)
library(writexl)
library(openxlsx)
library(MASS)

library(survey)

library(corrplot)
library(rstatix)

library(performance)
library(visdat)

library(scales)

rm(list= ls())

ESRU_EMOVI <- read_dta("C:/Users/roman/OneDrive - CIDE/Documentos/Estadística/Base de datos/entrevistado_2023.dta") %>%
  select(p112, sexo, region, factor,
         educ, educm, educp)

vis_miss(ESRU_EMOVI)

sum(is.na(ESRU_EMOVI))
sum(is.na(ESRU_EMOVI$educp)) # para una columna especifica

ESRU_clean<- ESRU_EMOVI %>%
  filter(! is.na(ESRU_EMOVI$educp),
         ! is.na(ESRU_EMOVI$educ),
         ! is.na(ESRU_EMOVI$educm))

vis_miss(ESRU_clean)
sum(is.na(ESRU_clean))

ESRU_clean <- ESRU_clean %>%
  mutate(
    hombre = ifelse(sexo == 1, 1, 0),
    mujer = ifelse(sexo == 2, 1, 0)
  )

ESRU_clean <- ESRU_clean %>%
  mutate(
    region1_dummy = ifelse(region == 1, 1, 0),
    region2_dummy = ifelse(region == 2, 1, 0),
    region3_dummy = ifelse(region == 3, 1, 0),
    region4_dummy = ifelse(region == 4, 1, 0),
    region5_dummy = ifelse(region == 5, 1, 0)
  )

ESRU_clean <- ESRU_clean %>%
  mutate(
    color_cat = case_when(
      p112 %in% c("A", "B", "C", "D") ~ "Oscuro",
      p112 %in% c("E", "F", "G") ~ "Medio", 
      p112 %in% c("H", "I", "J", "K") ~ "Claro"
    ),
    color_cat = factor(color_cat, levels = c("Oscuro", "Medio", "Claro"))
  )

ESRU_clean <- ESRU_clean %>%
  mutate(
    color_oscuro = ifelse(color_cat == "Oscuro", 1, 0),
    color_medio = ifelse(color_cat == "Medio", 1, 0),
    color_claro = ifelse(color_cat == "Claro", 1, 0)
  )

ESRU_clean <- ESRU_clean %>%
  mutate(
    educ_factor = factor(educ,
                         levels = c(1, 2, 3, 4),
                         labels = c("Primaria o menos", "Secundaria", "Preparatoria", "Profesional"))
  )

cat("=== VERIFICACIÓN DUMMIES COLOR DE PIEL ===\n")
ESRU_clean %>%
  group_by(color_cat) %>%
  summarise(
    n = n(),
    oscuro_dummy = sum(color_oscuro),
    medio_dummy = sum(color_medio),
    claro_dummy = sum(color_claro),
    .groups = 'drop'
  )

save(ESRU_clean, file = "C:/Users/roman/OneDrive - CIDE/Documentos/Estadística/Base de datos/ESRU_clean.RData")

summary(ESRU_clean)

ESRU_clean %>%
  group_by(sexo) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100 * total / sum(total), 1)) %>%
  arrange(sexo, desc(prop))

ESRU_clean %>%
  group_by(region) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100 * total / sum(total), 1)) %>%
  arrange(region, desc(prop))

ESRU_clean %>%
  summarise(
    oscuro_total = sum(color_oscuro),
    medio_total = sum(color_medio),
    claro_total = sum(color_claro),
    oscuro_prop = round(100 * mean(color_oscuro), 1),
    medio_prop = round(100 * mean(color_medio), 1),
    claro_prop = round(100 * mean(color_claro), 1)
  )

ESRU_clean %>%
  group_by(sexo, region) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100 * total / sum(total), 1)) %>%
  arrange(sexo, desc(prop))

ESRU_clean %>%
  group_by(sexo, educ) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100 * total / sum(total), 1)) %>%
  arrange(educ, desc(prop))

region_total <- ESRU_clean %>%
  group_by(region) %>%
  summarise(total = n()) %>%
  mutate(prop = round(100 * total / sum(total), 1),
         region_label = case_when(
           region == 1 ~ "Región 1",
           region == 2 ~ "Región 2", 
           region == 3 ~ "Región 3",
           region == 4 ~ "Región 4",
           region == 5 ~ "Región 5"
         ))

p1 <- ggplot(region_total, aes(x = region_label, y = total, fill = region_label)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(total, "\n(", prop, "%)")), 
            vjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribución total de personas por región",
       x = "Región", 
       y = "Número de personas") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave("distribucion_total_region.png", plot = p1, width = 10, height = 6, dpi = 300)

region_sexo <- ESRU_clean %>%
  group_by(region) %>%
  summarise(
    total = n(),
    hombres = sum(hombre),
    mujeres = sum(mujer),
    .groups = 'drop'
  ) %>%
  mutate(
    region_label = case_when(
      region == 1 ~ "Región 1",
      region == 2 ~ "Región 2", 
      region == 3 ~ "Región 3",
      region == 4 ~ "Región 4",
      region == 5 ~ "Región 5"
    )
  ) %>%
  pivot_longer(cols = c(hombres, mujeres), 
               names_to = "sexo", 
               values_to = "conteo")

p2 <- ggplot(region_sexo, aes(x = region_label, y = conteo, fill = sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = conteo), 
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("hombres" = "steelblue", "mujeres" = "lightpink"),
                    labels = c("Hombres", "Mujeres")) +
  labs(title = "Distribución por región y sexo",
       x = "Región", 
       y = "Número de personas",
       fill = "Sexo") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave("distribucion_region_sexo.png", plot = p2, width = 10, height = 6, dpi = 300)

region_sexo_prop <- ESRU_clean %>%
  group_by(region) %>%
  summarise(
    hombres = sum(hombre),
    mujeres = sum(mujer),
    total = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    prop_hombres = round(100 * hombres / total, 1),
    prop_mujeres = round(100 * mujeres / total, 1),
    region_label = case_when(
      region == 1 ~ "Región 1",
      region == 2 ~ "Región 2", 
      region == 3 ~ "Región 3",
      region == 4 ~ "Región 4",
      region == 5 ~ "Región 5"
    )
  ) %>%
  pivot_longer(cols = c(prop_hombres, prop_mujeres), 
               names_to = "sexo", 
               values_to = "proporcion") %>%
  mutate(sexo = ifelse(sexo == "prop_hombres", "Hombres", "Mujeres"))

p3 <- ggplot(region_sexo_prop, aes(x = region_label, y = proporcion, fill = sexo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(proporcion, "%")),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold") +
  scale_fill_manual(values = c("Hombres" = "steelblue", "Mujeres" = "lightpink")) +
  labs(title = "Proporción por región y sexo",
       x = "Región", 
       y = "Proporción (%)",
       fill = "Sexo") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

ggsave("proporcion_region_sexo.png", plot = p3, width = 10, height = 6, dpi = 300)

p4 <- ESRU_clean %>%
  mutate(
    sexo = factor(sexo, labels = c("Hombre", "Mujer")),
    educ = factor(
      educ,
      labels = c("Primaria o menos", "Secundaria", "Media superior", "Profesional"))
  ) %>%
  group_by(sexo, educ) %>%
  summarise(total = n(), .groups = "drop") %>%
  mutate(prop = round(100 * total / sum(total), 1)) %>%
  ggplot(aes(x = educ, y = prop, fill = educ)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~sexo, ncol = 1) +
  scale_fill_manual(values = c(
    "Primaria o menos" = "cadetblue",
    "Secundaria" = "skyblue3",
    "Media superior" = "steelblue",
    "Profesional" = "navy"
  )) +
  labs(
    x = "",
    y = "Proporción (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, face = "plain"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15),
    strip.text = element_text(size = 15, face = "bold") # título de las facetas (Hombre/Mujer) # face "bold" en negritS
  ) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  geom_text(aes(label = paste0(prop, "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2, size = 3.5, fontface = "bold")

ggsave("imagen1.png", plot = p4, width = 12, height = 8, dpi = 300)

colores_paleta_cat <- c(
  "Oscuro" = "#373028",
  "Medio" = "#9D7A54", 
  "Claro" = "#F3DAD6"
)

hombres_data <- ESRU_clean %>%
  filter(sexo == 1) %>%
  count(color_cat) %>%
  mutate(prop = round(100 * n / sum(n), 1)) %>%
  arrange(color_cat)

p5 <- ggplot(hombres_data, aes(x = color_cat, y = prop, fill = color_cat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_paleta_cat) +
  labs(x = "Categoría de color de piel",
       y = "Porcentaje (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, face = "plain"),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  ) +
  geom_text(aes(label = paste0(prop, "%")),
            vjust = -0.5,
            size = 4, fontface = "bold") +
  ylim(0, max(hombres_data$prop) * 1.15)

ggsave("imagen2.png", plot = p5, width = 8, height = 6, dpi = 300)

mujeres_data <- ESRU_clean %>%
  filter(sexo == 2) %>%
  count(color_cat) %>%
  mutate(prop = round(100 * n / sum(n), 1)) %>%
  arrange(color_cat)

p6 <- ggplot(mujeres_data, aes(x = color_cat, y = prop, fill = color_cat)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colores_paleta_cat) +
  labs(x = "Categoría de color de piel",
       y = "Porcentaje (%)") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12, face = "plain"),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  ) +
  geom_text(aes(label = paste0(prop, "%")),
            vjust = -0.5,
            size = 4, fontface = "bold") +
  ylim(0, max(mujeres_data$prop) * 1.15)

ggsave("imagen3.png", plot = p6, width = 8, height = 6, dpi = 300)

datos_proph <- ESRU_clean %>%
  filter(sexo == 1) %>%
  group_by(educ_factor, color_cat) %>%
  summarise(total = n(), .groups = 'drop') %>%
  group_by(educ_factor) %>%
  mutate(prop = total / sum(total)) %>%
  arrange(educ_factor, color_cat)

p7 <- ggplot(datos_proph, aes(x = educ_factor, y = prop, fill = color_cat)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = ifelse(prop > 0.019, paste0(round(prop * 100, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3.8, fontface = "bold", color = "black"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colores_paleta_cat) +
  labs(
    x = "", y = "Proporción",
    fill = "Color de piel"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12, face = "plain"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

ggsave("imagen4.png", plot = p7, width = 10, height = 6, dpi = 300)

datos_propm <- ESRU_clean %>%
  filter(sexo == 2) %>%
  group_by(educ_factor, color_cat) %>%
  summarise(total = n(), .groups = 'drop') %>%
  group_by(educ_factor) %>%
  mutate(prop = total / sum(total)) %>%
  arrange(educ_factor, color_cat)

p8 <- ggplot(datos_propm, aes(x = educ_factor, y = prop, fill = color_cat)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = ifelse(prop > 0.019, paste0(round(prop * 100, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 3.8, fontface = "bold", color = "black"
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = colores_paleta_cat) +
  labs(
    x = "", y = "Proporción",
    fill = "Color de piel"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "right",
    axis.text = element_text(size = 12, face = "plain"),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5)
  )

ggsave("imagen5.png", plot = p8, width = 10, height = 6, dpi = 300)

cat("=== RESUMEN POR REGIÓN ===\n")
for(i in 1:4) {
  datos_region <- ESRU_clean %>% filter(region == i)
  cat("Región", i, ":\n")
  cat("  Total:", nrow(datos_region), "personas\n")
  cat("  Hombres:", sum(datos_region$hombre), 
      "(", round(100 * mean(datos_region$hombre), 1), "%)\n")
  cat("  Mujeres:", sum(datos_region$mujer), 
      "(", round(100 * mean(datos_region$mujer), 1), "%)\n")
  cat("  Distribución por color de piel:\n")
  color_dist <- datos_region %>% 
    count(color_cat) %>% 
    mutate(prop = round(100 * n / sum(n), 1))
  for(j in 1:nrow(color_dist)) {
    cat("    ", as.character(color_dist$color_cat[j]), ":", 
        color_dist$n[j], "(", color_dist$prop[j], "%)\n")
  }
  cat("\n")
}

cat("=== RESUMEN USANDO DUMMIES DE COLOR DE PIEL ===\n")
ESRU_clean %>%
  summarise(
    Total = n(),
    Color_Oscuro = paste0(sum(color_oscuro), " (", round(100 * mean(color_oscuro), 1), "%)"),
    Color_Medio = paste0(sum(color_medio), " (", round(100 * mean(color_medio), 1), "%)"),
    Color_Claro = paste0(sum(color_claro), " (", round(100 * mean(color_claro), 1), "%)")
  ) %>%
  print()

cat("=== COLOR DE PIEL POR REGIÓN (USANDO DUMMIES) ===\n")
ESRU_clean %>%
  group_by(region) %>%
  summarise(
    Total = n(),
    Oscuro = paste0(sum(color_oscuro), " (", round(100 * mean(color_oscuro), 1), "%)"),
    Medio = paste0(sum(color_medio), " (", round(100 * mean(color_medio), 1), "%)"),
    Claro = paste0(sum(color_claro), " (", round(100 * mean(color_claro), 1), "%)"),
    .groups = 'drop'
  ) %>%
  print()

movilidad_extrema <- ESRU_clean %>%
  mutate(
    educ_max_padres = pmax(educm, educp, na.rm = TRUE),
    grupo_parental = case_when(
      educ_max_padres == 1 ~ "Padres: Primaria o menos",
      educ_max_padres == 4 ~ "Padres: Estudios profesionales"
    ),
    educ_hijos = case_when(
      educ == 1 ~ "Primaria o menos",
      educ == 2 ~ "Secundaria", 
      educ == 3 ~ "Media superior",
      educ == 4 ~ "Profesional"
    ),
    tono_piel = case_when(
      color_oscuro == 1 ~ "Oscuro",
      color_medio == 1 ~ "Medio", 
      color_claro == 1 ~ "Claro"
    )
  ) %>%
  filter(!is.na(grupo_parental), !is.na(educ_hijos), !is.na(tono_piel)) %>%
  mutate(
    educ_hijos = factor(educ_hijos, levels = c("Primaria o menos", "Secundaria", 
                                              "Media superior", "Profesional"))
  )

datos_grafica <- movilidad_extrema %>%
  group_by(grupo_parental, educ_hijos, tono_piel) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(grupo_parental, educ_hijos) %>%
  mutate(
    porcentaje = round(100 * n / sum(n), 1),
    etiqueta = ifelse(porcentaje >= 5, paste0(porcentaje, "%"), "")
  ) %>%
  ungroup()

datos_grafica <- datos_grafica %>%
  mutate(grupo_parental = factor(grupo_parental, 
                                 levels = c("Padres: Primaria o menos", 
                                            "Padres: Estudios profesionales")))

p9 <- ggplot(datos_grafica, aes(x = educ_hijos, y = porcentaje, fill = tono_piel)) +
  geom_col(alpha = 0.9, width = 0.7) +
  geom_text(aes(label = etiqueta),
            position = position_stack(vjust = 0.5),
            size = 2.5, color = "white", fontface = "bold") +
  facet_wrap(~grupo_parental, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Oscuro" = "#373028",
      "Medio" = "#9D7A54", 
      "Claro" = "#F3DAD6"
    ),
    name = "Tono de piel"
  ) +
  labs(
    x = "Nivel de escolaridad alcanzado",
    y = "Porcentaje de Personas (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines"),
    panel.grid.major.x = element_blank()
  )

ggsave("imagen6.png", plot = p9, width = 12, height = 8, dpi = 300)

movilidad_profesional <- movilidad_extrema %>%
  group_by(grupo_parental, tono_piel) %>%
  summarise(
    n_total = n(),
    n_profesional = sum(educ == 4),
    porcentaje_profesional = round(100 * n_profesional / n_total, 1),
    etiqueta = paste0(porcentaje_profesional, "%"),
    .groups = 'drop'
  )

movilidad_profesional <- movilidad_profesional %>%
  mutate(grupo_parental = factor(grupo_parental, 
                                 levels = c("Padres: Primaria o menos", 
                                            "Padres: Estudios profesionales")))

p10 <- ggplot(movilidad_profesional, aes(x = tono_piel, y = porcentaje_profesional, fill = tono_piel)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = etiqueta), 
            vjust = -0.3, size = 4.5, fontface = "bold", lineheight = 0.8) +
  facet_wrap(~grupo_parental, nrow = 1) +
  scale_fill_manual(values = c("Oscuro" = "#373028", 
                               "Medio" = "#9D7A54", 
                               "Claro" = "#F3DAD6")) +
  labs(
    x = "Tono de Piel",
    y = "Porcentaje con Educación Profesional (%)",
    fill = "Tono de Piel"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12),
    legend.position = "none",
    strip.text = element_text(size = 11, face = "bold")
  ) +
  ylim(0, max(movilidad_profesional$porcentaje_profesional) * 1.15)

ggsave("Imagen7.png", plot = p10, width = 10, height = 6, dpi = 300)

datos_hombres <- ESRU_clean %>%
  filter(sexo == 1) %>%  # Hombres
  mutate(
    educ_individuo = case_when(
      educ == 1 ~ "Primaria o menos",
      educ == 2 ~ "Secundaria",
      educ == 3 ~ "Media superior", 
      educ == 4 ~ "Profesional"
    ),
    educ_padre = case_when(
      educp == 1 ~ "Primaria o menos",
      educp == 2 ~ "Secundaria",
      educp == 3 ~ "Media superior",
      educp == 4 ~ "Profesional"
    ),
    tono_piel = case_when(
      color_oscuro == 1 ~ "Oscuro",
      color_medio == 1 ~ "Medio", 
      color_claro == 1 ~ "Claro"
    ),
    comparacion = "Hombres vs Padres"
  ) %>%
  filter(!is.na(educ_individuo), !is.na(educ_padre), !is.na(tono_piel))

datos_mujeres <- ESRU_clean %>%
  filter(sexo == 2) %>%  # Mujeres
  mutate(
    educ_individuo = case_when(
      educ == 1 ~ "Primaria o menos",
      educ == 2 ~ "Secundaria",
      educ == 3 ~ "Media superior", 
      educ == 4 ~ "Profesional"
    ),
    educ_madre = case_when(
      educm == 1 ~ "Primaria o menos",
      educm == 2 ~ "Secundaria", 
      educm == 3 ~ "Media superior",
      educm == 4 ~ "Profesional"
    ),
    tono_piel = case_when(
      color_oscuro == 1 ~ "Oscuro",
      color_medio == 1 ~ "Medio", 
      color_claro == 1 ~ "Claro"
    ),
    comparacion = "Mujeres vs Madres"
  ) %>%
  filter(!is.na(educ_individuo), !is.na(educ_madre), !is.na(tono_piel))

hombres_grafica <- datos_hombres %>%
  group_by(educ_padre, educ_individuo, tono_piel) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(educ_padre, educ_individuo) %>%
  mutate(
    porcentaje = round(100 * n / sum(n), 1),
    etiqueta = ifelse(porcentaje >= 5, paste0(porcentaje, "%"), ""),
    comparacion = "Hombres vs Padres"
  ) %>%
  rename(educ_parental = educ_padre)

mujeres_grafica <- datos_mujeres %>%
  group_by(educ_madre, educ_individuo, tono_piel) %>%
  summarise(n = n(), .groups = 'drop') %>%
  group_by(educ_madre, educ_individuo) %>%
  mutate(
    porcentaje = round(100 * n / sum(n), 1),
    etiqueta = ifelse(porcentaje >= 5, paste0(porcentaje, "%"), ""),
    comparacion = "Mujeres vs Madres"
  ) %>%
  rename(educ_parental = educ_madre)

datos_comparacion <- bind_rows(hombres_grafica, mujeres_grafica) %>%
  mutate(
    educ_parental = factor(educ_parental, 
                           levels = c("Primaria o menos", "Secundaria", 
                                      "Media superior", "Profesional")),
    educ_individuo = factor(educ_individuo,
                            levels = c("Primaria o menos", "Secundaria",
                                       "Media superior", "Profesional")),
    comparacion = factor(comparacion, 
                         levels = c("Hombres vs Padres", "Mujeres vs Madres")),
    tono_piel = factor(tono_piel, levels = c("Oscuro", "Medio", "Claro"))
  )

movilidad_neta <- bind_rows(
  datos_hombres %>% 
    mutate(
      movilidad = case_when(
        educ > educp ~ "Ascendente",
        educ == educp ~ "Estática", 
        educ < educp ~ "Descendente"
      )
    ) %>%
    group_by(tono_piel, movilidad) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(tono_piel) %>%
    mutate(porcentaje = round(100 * n / sum(n), 1)) %>%
    mutate(comparacion = "Hombres y Padres"),
  
  datos_mujeres %>% 
    mutate(
      movilidad = case_when(
        educ > educm ~ "Ascendente",
        educ == educm ~ "Estática",
        educ < educm ~ "Descendente"
      )
    ) %>%
    group_by(tono_piel, movilidad) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(tono_piel) %>%
    mutate(porcentaje = round(100 * n / sum(n), 1)) %>%
    mutate(comparacion = "Mujeres y Madres")
) %>%
  mutate(
    movilidad = factor(movilidad, levels = c("Ascendente", "Estática", "Descendente")),
    tono_piel = factor(tono_piel, levels = c("Oscuro", "Medio", "Claro"))
  )

p11 <- ggplot(movilidad_neta, aes(x = movilidad, y = porcentaje, fill = tono_piel)) +
  geom_col(alpha = 0.9, width = 0.7, position = "dodge") +
  geom_text(aes(label = paste0(porcentaje, "%")),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 4, fontface = "bold") +
  facet_wrap(~comparacion, nrow = 1) +
  scale_fill_manual(
    values = c(
      "Oscuro" = "#373028",
      "Medio" = "#9D7A54", 
      "Claro" = "#F3DAD6"
    ),
    name = "Tono de piel"
  ) +
  labs(
    x = "Tipo de Movilidad Educativa",
    y = "Porcentaje de individuos (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  ylim(0, max(movilidad_neta$porcentaje) * 1.2)

ggsave("imagen8.png", plot = p11, width = 13, height = 8, dpi = 300)

# PRUEBA DE HIPOSTESIS

# CONSTRUCCIÓN DE LA TABLA DE CONTINGENCIA 

tabla_contingencia <- table(ESRU_clean$color_cat, ESRU_clean$educ_factor)

cat("=== 1. TABLA DE FRECUENCIAS OBSERVADAS (n_ij) ===\n")
print(tabla_contingencia)



# PRUEBA DE INDEPENDENCIA CHI-CUADRADA 

prueba_global <- chisq.test(tabla_contingencia) #comando que hace todo

cat("\n=== 2. RESULTADOS DEL ESTADÍSTICO DE PEARSON (X^2) ===\n")
cat("Estadístico X^2 calculada:", round(prueba_global$statistic, 4), "\n")
cat("Grados de Libertad (df):", prueba_global$parameter, "\n") # df = (I-1)(J-1)
cat("Valor P (Probabilidad de cola derecha):", format.pval(prueba_global$p.value, digits=4), "\n")


if(prueba_global$p.value < 0.05) {
  cat("CONCLUSIÓN: Se rechaza H0 con un nivel de confianza del 95%.\n")
  cat("Existe evidencia fuerte de dependencia entre el tono de piel y el logro educativo.\n")
} else {
  cat("CONCLUSIÓN: No existe evidencia suficiente para rechazar la independencia.\n")
}


min_esperado <- min(prueba_global$expected)
if(min_esperado < 5) {
  cat("ADVERTENCIA: Alguna frecuencia esperada es < 5. La aproximación chi-cuadrada podría ser inexacta.\n")
}



residuales_std <- prueba_global$stdres

cat("\n=== 3. RESIDUALES ESTANDARIZADOS (DETECCIÓN DE PATRONES) ===\n")
cat("Regla de decisión (Agresti p. 39): Valores > +2 o < -2 indican falta de ajuste significativa.\n")
cat("- Valores Positivos (> 2): Sobrerrepresentación (Más gente de la esperada).\n")
cat("- Valores Negativos (< -2): Subrepresentación (Menos gente de la esperada / Barrera).\n\n")

print(round(residuales_std, 2))



library(corrplot)


cat("Tu imagen se guardará en esta carpeta:\n")
print(getwd())

png(filename = "grafica_9.png", width = 10, height = 8, units = "in", res = 300)

corrplot(residuales_std, 
         is.cor = FALSE, 
         method = "color",       
         col = COL2('RdBu', 10), 
         addCoef.col = "black",  
         tl.col = "black",       
         tl.srt = 45,            
         mar = c(0,0,2,0),       
         cl.pos = "r")           

dev.off() 
cat("¡Listo! Archivo 'grafica_9.png' creado.\n")


corrplot(residuales_std, 
         is.cor = FALSE, 
         method = "color",       
         col = COL2('RdBu', 10), 
         addCoef.col = "black",  
         tl.col = "black",       
         tl.srt = 45,            
         mar = c(0,0,2,0),       
         cl.pos = "r")

 prueba_global$p.value