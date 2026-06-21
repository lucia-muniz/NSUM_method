#nuevos gráficos



## Edad ------------------------------------------------------------------------
#frecuencia absoluta
hist_freq_abs_age <- function(df) {
  ggplot(df, aes(x = Age, fill = factor(Gender))) +
                   geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
                     labs(x = "Age",
                          fill = "Gender") +
                     scale_fill_manual(values = c("Masculino" = "#7FD8D8", "Femenino" = "#F28C8C"))

}


hist_freq_rel_age <- function(df) {
  ggplot(df, aes(x = Age, y = after_stat(count/sum(count)), fill = factor(Gender))) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
    labs(
      x = "Age",
      y = "Frecuencia relativa",
      fill = "Gender"
    ) +
    scale_fill_manual(values = c("Masculino" = "#7FD8D8", "Femenino" = "#F28C8C"))
}



hist_freq_rel_age(cuidansum_data_netq_anonymized_filtered)
hist_freq_abs_age(cuidansum_data_netq_anonymized_filtered)





#nacionalidad-------------------------------------------------------------------
nacionalidad_bp_genero <- function(df) {
  df <- df %>%
    mutate(Nacionalidad = as.character(Nacionalidad),
           Nacionalidad2 = as.character(Nacionalidad2),
           Nacionalidad_combinada = ifelse(is.na(Nacionalidad2),
                                           Nacionalidad,
                                           paste(Nacionalidad, Nacionalidad2, sep = " + ")))

  df$Nacionalidad_combinada <- as.factor(df$Nacionalidad_combinada)

  # Tabla de frecuencias por género
  df_nacionalidades <- df %>%
    count(Gender, Nacionalidad_combinada) %>%
    group_by(Gender) %>%
    mutate(Percentage = n / sum(n))

  # Paleta
  colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")

  ggplot(df_nacionalidades, aes(x = Nacionalidad_combinada, y = Percentage, fill = factor(Gender))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colores_genero) +
    labs(title = "Nacionalidad por Género", x = "", y = "Frecuencia relativa", fill = "Género") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


nacionalidad_bp_genero(cuidansum_data_netq_anonymized_filtered)
#ggsave("plots/nacionalidades_genero.jpeg", width = 8, height = 5, dpi = 300, bg = "white")





nacionalidad_bp_genero_noesp <- function(df) {
  df <- df %>%
    mutate(Nacionalidad = as.character(Nacionalidad),
           Nacionalidad2 = as.character(Nacionalidad2),
           Nacionalidad_combinada = ifelse(is.na(Nacionalidad2),
                                           Nacionalidad,
                                           paste(Nacionalidad, Nacionalidad2, sep = " + "))) %>%
    filter(!(Nacionalidad_combinada == "España"))

  df_nacionalidades <- df %>%
    count(Gender, Nacionalidad_combinada) %>%
    group_by(Gender) %>%
    mutate(Percentage = n / sum(n))

  colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")

  ggplot(df_nacionalidades, aes(x = Nacionalidad_combinada, y = Percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colores_genero) +
    labs(title = "Nacionalidades sin España (por Género)", x = "", y = "Frecuencia relativa", fill = "Género") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

nacionalidad_bp_genero_noesp(cuidansum_data_netq_anonymized_filtered)
#ggsave("plots/nacionalidades_sin_espana_genero.jpeg", width = 8, height = 5, dpi = 300, bg = "white")







#habitantes (Q5)---------------------------------------------------------------------
habitantes_bp_genero <- function(df) {
  q5_counts <- df %>%
    count(Gender, Q5) %>%
    group_by(Gender) %>%
    mutate(Percentage = n / sum(n))

  colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")

  ggplot(q5_counts, aes(x = Q5, y = Percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = colores_genero) +
    labs(title = "Número de habitantes por género", x = "Habitantes", y = "Frecuencia relativa", fill = "Género") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size= 7))
}



#ggsave("plots/habitantes_genero.jpeg", width = 6, height = 5, dpi = 300)
habitantes_bp_genero(cuidansum_data_netq_anonymized_filtered)





#Q7,Q8--------------------------------------------------------------------------
#heatmap por genero frecuencia absoluta
### Heatmap Q6 vs Q7 por género -----------------------------------------------

# Tabla de frecuencias cruzada
heatmap_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q6), !is.na(Q7), !is.na(Gender)) %>%
  count(Gender, Q6, Q7) %>%
  group_by(Gender) %>%
  mutate(RelFreq = n / sum(n)) %>%
  ungroup()

# Heatmap frecuencia absoluta con facetas por género
ggplot(heatmap_gender, aes(x = Q7, y = Q6, fill = n)) +
  geom_tile() +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Nivel educativo: Persona entrevistada vs Pareja",
    subtitle = "Frecuencia absoluta por género",
    x = "Pareja", y = "Persona entrevistada", fill = "Frecuencia"
  ) +
  facet_wrap(~ Gender) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#heatmap por género frecuencia relativa
# Heatmap frecuencia relativa con facetas por género
ggplot(heatmap_gender, aes(x = Q7, y = Q6, fill = RelFreq)) +
  geom_tile() +
  geom_text(aes(label = round(RelFreq, 3)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Nivel educativo: Persona entrevistada vs Pareja",
    subtitle = "Frecuencia relativa por género",
    x = "Pareja", y = "Persona entrevistada", fill = "Frecuencia relativa"
  ) +
  facet_wrap(~ Gender) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/heatmap_genero.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



#grafico de barras Q6:-----------------------------------------------------------
q6_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q6), !is.na(Gender)) %>%
  group_by(Q6, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

ggplot(q6_gender, aes(x = Q6, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Persona entrevistada por género",
       x = "", y = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("plots/netquest/estudios_respondent_por_genero_netquest.jpeg", width = 6, height = 5, dpi = 300)






#grafico de barras Q7:-----------------------------------------------------------
q7_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q7), !is.na(Gender)) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  group_by(Q7, Gender_pareja) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

ggplot(q7_gender, aes(x = Q7, y = Percentage, fill = Gender_pareja)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Pareja por género",
       x = "", y = "Frecuencia relativa", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("plots/netquest/estudios_pareja_por_genero_netquest.jpeg",width = 6, height = 5, dpi = 300)







#gráfico de barras combinado Q6 y Q7:-------------------------------------------
library(tidyr)
q6_q7_gender <- cuidansum_data_netq_anonymized_filtered %>%
  dplyr::select(Gender, Q6, Q7) %>%
  pivot_longer(cols = c(Q6, Q7), names_to = "Persona", values_to = "Estudios") %>%
  mutate(Género = if_else(Persona == "Q6", Gender, if_else(Gender == "Femenino", "Masculino", "Femenino"))) %>%
  filter(!is.na(Estudios), !is.na(Género)) %>%
  group_by(Estudios, Género) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

ggplot(q6_q7_gender, aes(x = Estudios, y = Percentage, fill = Género)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Total (Entrevistado + Pareja) por género",
       x = "", y = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#ggsave("plots/netquest/estudios_ambos_por_genero_netquest.jpeg", width = 6, height = 5, dpi = 300)











### Q8 y Q9 combinados por género---------------------------------------------
### Colores de género
colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")


#Q8 grafico de barras:
summary(cuidansum_data_netq_anonymized_filtered$Q8)
# Crear tabla de frecuencias con Gender
q8_gender_counts <- cuidansum_data_netq_anonymized_filtered%>%
  filter(!is.na(Q8), !is.na(Gender)) %>%
  group_by(Q8, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

# Gráfico de barras por género
ggplot(q8_gender_counts, aes(x = Q8, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel de ingresos - Persona entrevistada por género",
       x = "Ingresos", y = "Frecuencia relativa", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))

# Guardar plot
#ggsave("plots/netquest/ingresos_respondent_por_genero_netquest.jpeg", width = 6, height = 5, dpi = 300, bg = "white")





#Q9 gráfico de barras:
summary(cuidansum_data_netq_anonymized_filtered$Q9)
# tabla de frecuencias con Gender
q9_gender_counts <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q9), !is.na(Gender)) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  group_by(Q9, Gender_pareja) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

# Gráfico de barras por género
ggplot(q9_gender_counts, aes(x = Q9, y = Percentage, fill = Gender_pareja)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel de ingresos - Pareja por género",
       x = "Ingresos", y = "Frecuencia relativa", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))

# Guardar plot
#ggsave("plots/netquest/ingresos_pareja_por_genero_netquest.jpeg", width = 6, height = 5, dpi = 300, bg = "white")






# Grafico de barras combinado (Q8+Q9)
q8_q9_gender <- cuidansum_data_netq_anonymized_filtered %>%
  dplyr::select(Gender, Q8, Q9) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  pivot_longer(cols = c(Q8, Q9), names_to = "Persona", values_to = "Ingresos") %>%
  mutate(Género = if_else(Persona == "Q8", Gender, Gender_pareja)) %>%
  filter(!is.na(Ingresos), !is.na(Género)) %>%
  group_by(Ingresos, Género) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Frecuencia_relativa = Count / sum(Count))


ggplot(q8_q9_gender, aes(x = Ingresos, y = Frecuencia_relativa, fill = Género)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = colores_genero, name = "Género") +
  labs(title = "Nivel de ingresos - Género",
       x = "Nivel de ingresos", y = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))
#ggsave("plots/netquest/ingresos_ambos_por_genero_netquest.jpeg", width = 6, height = 5, dpi = 300, bg = "white")





### Heatmap ingresos vs educación diferenciando por género
educ_ingresos_gender <- cuidansum_data_netq_anonymized_filtered %>%
  dplyr::select(Gender, Q6, Q7, Q8, Q9) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  pivot_longer(cols = c(Q6, Q7), names_to = "PersonaEdu", values_to = "Educacion") %>%
  pivot_longer(cols = c(Q8, Q9), names_to = "PersonaIng", values_to = "Ingresos") %>%
  mutate(Género = case_when(
    PersonaEdu == "Q6" ~ Gender,
    PersonaEdu == "Q7" ~ Gender_pareja
  )) %>%
  filter(!is.na(Educacion), !is.na(Ingresos), !is.na(Género)) %>%
  group_by(Educacion, Ingresos, Género) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  complete(Educacion, Ingresos, Género, fill = list(Freq = 0)) %>%
  mutate(RelFreq = Freq / sum(Freq))

# Heatmap frecuencia absoluta por género
ggplot(educ_ingresos_gender, aes(x = Ingresos, y = Educacion, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  facet_wrap(~Género) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ingresos vs Nivel educativo - Frecuencia absoluta por género",
       x = "Ingresos", y = "Nivel educativo", fill = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Heatmap frecuencia relativa por género
ggplot(educ_ingresos_gender, aes(x = Ingresos, y = Educacion, fill = RelFreq)) +
  geom_tile() +
  geom_text(aes(label = round(RelFreq, 3)), color = "white", size=2.5) +
  facet_wrap(~Género) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ingresos vs Nivel educativo - Frecuencia relativa por género",
       x = "Ingresos", y = "Nivel educativo", fill = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





### Heatmap ingresos vs ingresos diferenciando por género:
#
#FALTA
#






--------------------------------------------------------------------------------
#2 GRAFICAS SEPARADAS-----------------------------------------------------------
#--------------------------------------------------------------------------------

#edad por género, 2 plots-------------------------------------------------------
hist_freq_rel_age2 <- function(df) {
ggplot(df, aes(x = Age, fill = Gender)) +
  geom_histogram(aes(y = after_stat(count / sum(count))),
                 binwidth = 5, color = "black", alpha = 0.8) +
  facet_wrap(~ Gender) +
  labs(
    title = "Age distribution by gender",
    x = "Age",
    y = "Relative frequency"
  ) +
  scale_fill_manual(
    values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )
}
hist_freq_rel_age2(cuidansum_data_netq_anonymized_filtered)

ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")





#nacionalidad--------------------------------------------------------------------
nacionalidad_bp_genero2 <- function(df) {
  df <- df %>%
    mutate(Nacionalidad = as.character(Nacionalidad),
           Nacionalidad2 = as.character(Nacionalidad2),
           Nacionalidad_combinada = ifelse(is.na(Nacionalidad2),
                                           Nacionalidad,
                                           paste(Nacionalidad, Nacionalidad2, sep = " + ")))

  df$Nacionalidad_combinada <- as.factor(df$Nacionalidad_combinada)

  # Tabla de frecuencias por género
  df_nacionalidades <- df %>%
    count(Gender, Nacionalidad_combinada) %>%
    group_by(Gender) %>%
    mutate(Percentage = n / sum(n))

  # Paleta
  colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")

  ggplot(df_nacionalidades, aes(x = Nacionalidad_combinada, y = Percentage, fill = factor(Gender))) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Gender) +
    scale_fill_manual(values = colores_genero) +
    labs(title = "Nacionalidad por Género", x = "", y = "Frecuencia relativa", fill = "Género") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

nacionalidad_bp_genero2(cuidansum_data_netq_anonymized_filtered)
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_nacionalidad_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")




nacionalidad_bp_genero_noesp2 <- function(df) {
  df <- df %>%
    mutate(Nacionalidad = as.character(Nacionalidad),
           Nacionalidad2 = as.character(Nacionalidad2),
           Nacionalidad_combinada = ifelse(is.na(Nacionalidad2),
                                           Nacionalidad,
                                           paste(Nacionalidad, Nacionalidad2, sep = " + "))) %>%
    filter(!(Nacionalidad_combinada == "España"))

  df_nacionalidades <- df %>%
    count(Gender, Nacionalidad_combinada) %>%
    group_by(Gender) %>%
    mutate(Percentage = n / sum(n))

  colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")

  ggplot(df_nacionalidades, aes(x = Nacionalidad_combinada, y = Percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Gender) +
    scale_fill_manual(values = colores_genero) +
    labs(title = "Nacionalidades sin España (por Género)", x = "", y = "Frecuencia relativa", fill = "Género") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

nacionalidad_bp_genero_noesp2(cuidansum_data_netq_anonymized_filtered)
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_gender_noesp.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")






#habitantes (Q5)---------------------------------------------------------------------
habitantes_bp_genero2 <- function(df) {
  q5_counts <- df %>%
    count(Gender, Q5) %>%
    group_by(Gender) %>%
    mutate(Percentage = n / sum(n))

  colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")

  ggplot(q5_counts, aes(x = Q5, y = Percentage, fill = Gender)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Gender) +
    scale_fill_manual(values = colores_genero) +
    labs(title = "Número de habitantes por género", x = "Habitantes", y = "Frecuencia relativa", fill = "Género") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size= 7))
}

habitantes_bp_genero2(cuidansum_data_netq_anonymized_filtered)

ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_habitantes.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")






#grafico de barras Q6:-----------------------------------------------------------
q6_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q6), !is.na(Gender)) %>%
  group_by(Q6, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

ggplot(q6_gender, aes(x = Q6, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Persona entrevistada por género",
       x = "", y = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_nivel_educativo.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



#Q7,Q8--------------------------------------------------------------------------
#heatmap por genero frecuencia absoluta
### Heatmap Q6 vs Q7 por género -----------------------------------------------

# Tabla de frecuencias cruzada
heatmap_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q6), !is.na(Q7), !is.na(Gender)) %>%
  count(Gender, Q6, Q7) %>%
  group_by(Gender) %>%
  mutate(RelFreq = n / sum(n)) %>%
  ungroup()

# Heatmap frecuencia absoluta con facetas por género
ggplot(heatmap_gender, aes(x = Q7, y = Q6, fill = n)) +
  geom_tile() +
  facet_wrap(~ Gender) +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Nivel educativo: Persona entrevistada vs Pareja",
    subtitle = "Frecuencia absoluta por género",
    x = "Pareja", y = "Persona entrevistada", fill = "Frecuencia"
  ) +
  facet_wrap(~ Gender) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/nivel_educativo_gender(heatmap_f_absolutas).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



#heatmap por género frecuencia relativa
# Heatmap frecuencia relativa con facetas por género
ggplot(heatmap_gender, aes(x = Q7, y = Q6, fill = RelFreq)) +
  geom_tile() +
  facet_wrap(~ Gender) +
  geom_text(aes(label = round(RelFreq, 3)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Nivel educativo: Persona entrevistada vs Pareja",
    subtitle = "Frecuencia relativa por género",
    x = "Pareja", y = "Persona entrevistada", fill = "Frecuencia relativa"
  ) +
  facet_wrap(~ Gender) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/nivel_educativo_gender(heatmap).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")


#grafico de barras Q6:-----------------------------------------------------------
q6_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q6), !is.na(Gender)) %>%
  group_by(Q6, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

ggplot(q6_gender, aes(x = Q6, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Persona entrevistada por género",
       x = "", y = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/nivel_educativo_gender(q6).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")





#grafico de barras Q7:-----------------------------------------------------------
q7_gender <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q7), !is.na(Gender)) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  group_by(Q7, Gender_pareja) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))






ggplot(q7_gender, aes(x = Q7, y = Percentage, fill = Gender_pareja)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Pareja por género",
       x = "", y = "Frecuencia relativa", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~ Gender_pareja)
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/nivel_educativo_gender(q7).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")


#gráfico de barras combinado Q6 y Q7:-------------------------------------------
library(tidyr)
q6_q7_gender <- cuidansum_data_netq_anonymized_filtered %>%
  dplyr::select(Gender, Q6, Q7) %>%
  pivot_longer(cols = c(Q6, Q7), names_to = "Persona", values_to = "Estudios") %>%
  mutate(Género = if_else(Persona == "Q6", Gender, if_else(Gender == "Femenino", "Masculino", "Femenino"))) %>%
  filter(!is.na(Estudios), !is.na(Género)) %>%
  group_by(Estudios, Género) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

ggplot(q6_q7_gender, aes(x = Estudios, y = Percentage, fill = Género)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Género) +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel educativo - Total (Entrevistado + Pareja) por género",
       x = "", y = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/nivel_educativo_gender(q6,q7).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")







### Q8 y Q9 combinados por género---------------------------------------------
### Colores de género
colores_genero <- c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")


#Q8 grafico de barras:
summary(cuidansum_data_netq_anonymized_filtered$Q8)
# Crear tabla de frecuencias con Gender
q8_gender_counts <- cuidansum_data_netq_anonymized_filtered%>%
  filter(!is.na(Q8), !is.na(Gender)) %>%
  group_by(Q8, Gender) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

# Gráfico de barras por género
ggplot(q8_gender_counts, aes(x = Q8, y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender) +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel de ingresos - Persona entrevistada por género",
       x = "Ingresos", y = "Frecuencia relativa", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))

#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/ingresos_gender(q8).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")




#Q9 gráfico de barras:
summary(cuidansum_data_netq_anonymized_filtered$Q9)
# tabla de frecuencias con Gender
q9_gender_counts <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q9), !is.na(Gender)) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  group_by(Q9, Gender_pareja) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count))

# Gráfico de barras por género
ggplot(q9_gender_counts, aes(x = Q9, y = Percentage, fill = Gender_pareja)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Gender_pareja) +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  labs(title = "Nivel de ingresos - Pareja por género",
       x = "Ingresos", y = "Frecuencia relativa", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/ingresos_gender(q9).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")








# Grafico de barras combinado (Q8+Q9)
q8_q9_gender <- cuidansum_data_netq_anonymized_filtered %>%
  dplyr::select(Gender, Q8, Q9) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  pivot_longer(cols = c(Q8, Q9), names_to = "Persona", values_to = "Ingresos") %>%
  mutate(Género = if_else(Persona == "Q8", Gender, Gender_pareja)) %>%
  filter(!is.na(Ingresos), !is.na(Género)) %>%
  group_by(Ingresos, Género) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Frecuencia_relativa = Count / sum(Count))


  ggplot(q8_q9_gender, aes(x = Ingresos, y = Frecuencia_relativa, fill = Género)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Género) +
    scale_fill_manual(values = colores_genero, name = "Género") +
    labs(title = "Nivel de ingresos - Género",
         x = "Nivel de ingresos", y = "Frecuencia relativa") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  #ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/ingresos_gender(q8,q9).png",
         width = 12, height = 4.5, dpi = 300, bg = "white")



### Heatmap ingresos vs educación diferenciando por género
educ_ingresos_gender <- cuidansum_data_netq_anonymized_filtered %>%
  dplyr::select(Gender, Q6, Q7, Q8, Q9) %>%
  mutate(Gender_pareja = if_else(Gender == "Femenino", "Masculino", "Femenino")) %>%
  pivot_longer(cols = c(Q6, Q7), names_to = "PersonaEdu", values_to = "Educacion") %>%
  pivot_longer(cols = c(Q8, Q9), names_to = "PersonaIng", values_to = "Ingresos") %>%
  mutate(Género = case_when(
    PersonaEdu == "Q6" ~ Gender,
    PersonaEdu == "Q7" ~ Gender_pareja
  )) %>%
  filter(!is.na(Educacion), !is.na(Ingresos), !is.na(Género)) %>%
  group_by(Educacion, Ingresos, Género) %>%
  summarise(Freq = n(), .groups = "drop") %>%
  complete(Educacion, Ingresos, Género, fill = list(Freq = 0)) %>%
  mutate(RelFreq = Freq / sum(Freq))

# Heatmap frecuencia absoluta por género
ggplot(educ_ingresos_gender, aes(x = Ingresos, y = Educacion, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white") +
  facet_wrap(~Género) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ingresos vs Nivel educativo - Frecuencia absoluta por género",
       x = "Ingresos", y = "Nivel educativo", fill = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/ingresos_vs_nivel_educativo_abs_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")

# Heatmap frecuencia relativa por género
ggplot(educ_ingresos_gender, aes(x = Ingresos, y = Educacion, fill = RelFreq)) +
  geom_tile() +
  geom_text(aes(label = round(RelFreq, 3)), color = "white", size=2.5) +
  facet_wrap(~Género) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Ingresos vs Nivel educativo - Frecuencia relativa por género",
       x = "Ingresos", y = "Nivel educativo", fill = "Frecuencia relativa") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/ingresos_vs_nivel_educativo_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



## Aportación económica unidad familiar (Q10) ----------------------------------

ggplot(cuidansum_data_netq_anonymized_filtered, aes(x = Q10, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Aportación económica a la unidad familiar",
       x = "",
       y = "Frecuencia absoluta") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
##ggsave("plots/netquest/ingresos_ambos_por_genero_netquest.jpeg", width = 6, height = 5, dpi = 300, bg = "white")


q10_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q10) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q10_gender_counts_netq, aes(x = Q10, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Aportación económica a la unidad familiar",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_aportacion_economica_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



#Q11 Número de hijos-------------------------------------------------------------------
q11_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q11) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender_counts_netq, aes(x = Q11, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de hijos",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_n_hijos_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



#Q11 Género de tu hijo---------------------------------------------------------------
q11_gender1_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_GENERO_1) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender1_counts_netq, aes(x = P11_GENERO_1, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Género hijo 1",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_genero_hijo1.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")


#-------------------
q11_gender2_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_GENERO_2) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender2_counts_netq, aes(x = P11_GENERO_2, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Género hijo 2",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_genero_hijo2.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")

#-----------------------------

q11_gender3_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_GENERO_3) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender3_counts_netq, aes(x = P11_GENERO_3, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Género hijo 3",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_genero_hijo3.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")

#-------------------------
q11_gender4_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_GENERO_4) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender4_counts_netq, aes(x = P11_GENERO_4, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Género hijo 4",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_genero_hijo4.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")

#--------------------------
q11_gender5_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_GENERO_5) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender5_counts_netq, aes(x = P11_GENERO_5, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Género hijo 5",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_genero_hijo5.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
#----------------------------

q11_gender6_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_GENERO_6) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_gender6_counts_netq, aes(x = P11_GENERO_6, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Género hijo 6",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_genero_hijo6.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")



#Q12 Edad hijo--------------------------------------------------------------------------

q11_edad1_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_EDAD_1) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edad1_counts_netq, aes(x = P11_EDAD_1, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo 1",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo1_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
#----------------------

q11_edad2_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_EDAD_2) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edad2_counts_netq, aes(x = P11_EDAD_2, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo 2",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo2_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
#-----------------------

q11_edad3_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_EDAD_3) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edad3_counts_netq, aes(x = P11_EDAD_3, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo 3",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo3_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
#-------------------------------

q11_edad4_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_EDAD_4) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edad4_counts_netq, aes(x = P11_EDAD_4, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo 1",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo4_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")

#-----------------------
q11_edad5_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, P11_EDAD_5) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edad5_counts_netq, aes(x = P11_EDAD_5, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo 5",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo5_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")


#Q13--------------------edad menor----------------------------------------------------
q11_edadmen_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, edad_menor) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edadmen_counts_netq, aes(x = edad_menor, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo menor",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo_menor_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")


#Q14-------------------edad mayor--------------------------------------------------------
q11_edadmay_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, edad_mayor) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q11_edadmay_counts_netq, aes(x = edad_mayor, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Edad hijo mayor",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_edad_hijo_mayor_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")



#Q15 Tamaño red de contactos-------------------------------------------------------------
q15_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q15) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q15_counts_netq, aes(x = Q15, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Tamaño red de contactos",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_tamaño_red_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")




#####PREGUNTAS INDIRECTAS---------------------------------------------------------------
#---------------------------------------------------------------------------------------

#q17----------------------------------------
q17_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q17) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q17_counts_netq, aes(x = Q17, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de mujeres de la red que han reducido su jornada laboral, han pedido una excedencia y/o han dejado de trabajar para asumir tareas domésticas o de cuidado de hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_reduccion_jornada_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q17 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q17)
chisq.test(tab_q17)



#q18---------------------------------------------
q18_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q18) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q18_counts_netq, aes(x = Q18, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de hombres de la red que han reducido su jornada laboral, han pedido una excedencia y/o han dejado de trabajar para asumir tareas domésticas o de cuidado de hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_reduccion_jornada_gender_hombres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q18 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q18)
chisq.test(tab_q18)



#17 vs 18
q17_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q17 = as.numeric(Q17)) %>%
  group_by(Q17) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q18_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q18 = as.numeric(Q18)) %>%
  group_by(Q18) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–20)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3
) * 1.05

p_q17 <- ggplot(q17_counts, aes(x = Q17, y = freq_rel)) +
  geom_col(fill = "#F28C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q17. Reducción de jornada por tareas de cuidado, mujeres",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 9, hjust = 0.5)
  )

p_q18 <- ggplot(q18_counts, aes(x = Q18, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q18. Reducción de jornada por tareas de cuidado, hombres",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 9, hjust = 0.5)
  )

p_combined <- p_q17 | p_q18
#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_reduccion_jornada_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")

# Test Chi-cuadrado 17 vs 18
tab_q17vs18 <- table(cuidansum_data_netq_anonymized_filtered$Q17, cuidansum_data_netq_anonymized_filtered$Q18)
t1718=chisq.test(tab_q17vs18)

#---------------------------------------------------------------------------------------------
#q19----------------------------------

q19_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q19) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q19_counts_netq, aes(x = Q19, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de mujeres de la red que han teletrabajado para asumir tareas domésticas o de cuidado de hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_teletrabajo_gender_mujeres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q19 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q19)
chisq.test(tab_q19)




#q20----------------------------------
q20_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q20) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q20_counts_netq, aes(x = Q20, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de hombres de la red que han teletrabajado para asumir tareas domésticas o de cuidado de hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_teletrabajo_gender_hombres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q20 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q20)
chisq.test(tab_q20)


#19 vs 20
q19_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q19 = as.numeric(Q17)) %>%
  group_by(Q19) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q20_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q20 = as.numeric(Q20)) %>%
  group_by(Q20) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–20)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3
) * 1.05

p_q19 <- ggplot(q19_counts, aes(x = Q19, y = freq_rel)) +
  geom_col(fill = "#F28C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q19. Teletrabajo para asumir tareas domésticas o de cuidado de
hijos/as
, mujeres",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 9, hjust = 0.5)
  )

p_q20 <- ggplot(q20_counts, aes(x = Q20, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q20. Teletrabajo para asumir tareas domésticas o de cuidado de
hijos/as, hombres",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 9, hjust = 0.5)
  )

p_combined <- p_q19 | p_q20
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_teletrabajo_q19y20_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q19vs20 <- table(cuidansum_data_netq_anonymized_filtered$Q19, cuidansum_data_netq_anonymized_filtered$Q20)
t1920=chisq.test(tab_q19vs20)

#------------------------------------------------------------------------------------------------
#q21---------------------------------------------
q21_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q21) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q21_counts_netq, aes(x = Q21, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de mujeres de la red que han sufrido problemas en el trabajo por tener que responder a demandas puntuales de atención a sus hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problem_trabajo_gender_mujer.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q21 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q21)
chisq.test(tab_q21)

#q22---------------------------------------------
q22_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q22) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q22_counts_netq, aes(x = Q22, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de hombres de la red que han sufrido problemas en el trabajo por tener que responder a demandas puntuales de atención a sus hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problem_trabajo_gender_hombre.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q22 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q22)
chisq.test(tab_q22)



#21 vs 22
q21_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q21 = as.numeric(Q21)) %>%
  group_by(Q21) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q22_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q22 = as.numeric(Q22)) %>%
  group_by(Q22) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–20)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3
) * 1.05

p_q21 <- ggplot(q21_counts, aes(x = Q21, y = freq_rel)) +
  geom_col(fill = "#F28C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q21. Problemas en tu empleo por tener que ausentarte puntualmente para atender a tus hijos/as, mujeres",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 9, hjust = 0.5)
  )

p_q22 <- ggplot(q22_counts, aes(x = Q22, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q22. Problemas en tu empleo por tener que ausentarte puntualmente para atender a tus hijos/as
, hombres",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 9, hjust = 0.5)
  )

p_combined <- p_q21 | p_q22
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_q21y22_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q21vs22 <- table(cuidansum_data_netq_anonymized_filtered$Q21, cuidansum_data_netq_anonymized_filtered$Q22)
t2122=chisq.test(tab_q21vs22)


#-------------------------------------------------------------------------------------
#q24---------------------------------------------
q24_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q24) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q24_counts_netq, aes(x = Q24, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q24 Número de parejas de la red con apoyo externo contratado para el cuidado de los hijos/as y/o las tareas domésticas",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender) +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 0.3)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q24hist_ayuda_externa_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q24 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q24)
chisq.test(tab_q24)

#-------------------------------------------------------------------------------------
#q25---------------------------------------------
q25_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q25) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q25_counts_netq, aes(x = Q25, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q25 Número de parejas de la red con apoyo familiar contratado para el cuidado de los hijos/as y/o las tareas domésticas",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 0.3)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/Q25hist_ayuda_familiar_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q25 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q25)
chisq.test(tab_q25)


#-------------------------------------------------------------------------------------
#q27---------------------------------------------
q27_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q27) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q27_counts_netq, aes(x = Q27, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q27 Número de parejas de la red en las que el reparto de las tareas de cuidados de los hijos/as es equitativo",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q27_reparto_equitativo_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q27 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q27)
chisq.test(tab_q27)




#-------------------------------------------------------------------------------------
#q28---------------------------------------------
q28_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q28) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q28_counts_netq, aes(x = Q28, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q28 Número de parejas de la red en las que el reparto de las tareas domésticas es equitativo",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q28_reparto_equitativo_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q28 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q28)
chisq.test(tab_q28)

#-------------------------------------------------------------------------------------
#q29---------------------------------------------
q29_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q29) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q29_counts_netq, aes(x = Q29, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q29 Número de parejas de la red en las que el reparto de las tareas domésticas es causa de discusiones",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F29C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q29_reparto_equitativo_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q29 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q29)
chisq.test(tab_q29)
#-------------------------------------------------------------------------------------
#q31---------------------------------------------
q31_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q31) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q31_counts_netq, aes(x = Q31, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q31 Número de mujeres de la red que crean que la desigualdad de género ya no es un problema en la actualidad",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q31_reparto_discusiones_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q31 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q31)
chisq.test(tab_q31)


#q32---------------------------------------------
q32_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q32) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q32_counts_netq, aes(x = Q32, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q32 Número de hombres de la red que crean que la desigualdad de género ya no es un problema en la actualidad",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q32_desigualdad_hombre_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q32 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q32)
chisq.test(tab_q32)


#----------------------------------------
#31 vs 32
q31_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q31 = as.numeric(Q31)) %>%
  group_by(Q31) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q32_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  group_by(Q32) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–30)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3) * 1.05

p_q31 <- ggplot(q31_counts, aes(x = Q31, y = freq_rel)) +
  geom_col(fill = "#F38C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q31.  Número de mujeres de la red que crean que la desigualdad de género ya no es un problema en la actualidad",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )

p_q32 <- ggplot(q32_counts, aes(x = Q32, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q32.  Número de hombres de la red que crean que la desigualdad de género ya no es un problema en la actualidad",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )
library(patchwork)
p_combined <- p_q31 | p_q32
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_q31y32_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q31vs32 <- table(cuidansum_data_netq_anonymized_filtered$Q31, cuidansum_data_netq_anonymized_filtered$Q32)
q3132=chisq.test(tab_q31vs32)

#-------------------------------------------------------------------------------------
#q33---------------------------------------------
q33_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q33) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q33_counts_netq, aes(x = Q33, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q33 Número de mujeres de la red que estén de acuerdo con desarrollar ellas la mayoría de las tareas del hogar",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q33_reparto_discusiones_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q33 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q33)
chisq.test(tab_q33)


#q34---------------------------------------------
q34_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q34) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q34_counts_netq, aes(x = Q34, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q34 Número de hombres de la red que estén de acuerdo con desarrollar ellos la mayoría de las tareas del hogar",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q34_reparto_mayoria_tareas_hombres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q34 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q34)
chisq.test(tab_q34)


#----------------------------------------
#33 vs 34
q33_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  group_by(Q33) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q34_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  group_by(Q34) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–30)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3) * 1.05

p_q33 <- ggplot(q33_counts, aes(x = Q33, y = freq_rel)) +
  geom_col(fill = "#F38C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q33.  Número de mujeres de la red que estén de acuerdo con desarrollar ellos la mayoría de las tareas del hogar",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )

p_q34 <- ggplot(q34_counts, aes(x = Q34, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q34. Número de hombres de la red que estén de acuerdo con desarrollar ellos la mayoría de las tareas del hogar",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )
library(patchwork)
p_combined <- p_q33 | p_q34
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_q33y34_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q33vs34 <- table(cuidansum_data_netq_anonymized_filtered$Q33, cuidansum_data_netq_anonymized_filtered$Q34)
q3334= chisq.test(tab_q33vs34)


#-------------------------------------------------------------------------------------
#q35---------------------------------------------
q35_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q35) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q35_counts_netq, aes(x = Q35, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q35 Número de mujeres de la red que estén de acuerdo con desarrollar ellas la mayoría del cuidado de los hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q35_mayoria_cuidado_mujeres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q35 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q35)
chisq.test(tab_q35)


#q36---------------------------------------------
q36_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q36) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q36_counts_netq, aes(x = Q36, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Q36 Número de hombres de la red que estén de acuerdo con que las mujeres desarrollen la mayoría del cuidado de los hijos/as",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q36_mayoria_cuidado_hombres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q36 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q36)
chisq.test(tab_q36)


#----------------------------------------
#35 vs 36
q35_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q35 = as.numeric(Q35)) %>%
  group_by(Q35) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q36_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  group_by(Q36) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–30)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3) * 1.05

p_q35 <- ggplot(q35_counts, aes(x = Q35, y = freq_rel)) +
  geom_col(fill = "#F38C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q35.  Número de mujeres de la red que estén de acuerdo con desarrollar ellos la mayoría de las tareas del cuidado de los hijos/as",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )

p_q36 <- ggplot(q36_counts, aes(x = Q36, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q36. Número de hombres de la red que estén de acuerdo con desarrollar ellos la mayoría de las tareas del cuidado de los hijos/as",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )
library(patchwork)
p_combined <- p_q35 | p_q36
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_q35y36_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q35vs36 <- table(cuidansum_data_netq_anonymized_filtered$Q35, cuidansum_data_netq_anonymized_filtered$Q36)
q3536=chisq.test(tab_q35vs36)


#------------------------------------------------------------------------------------------
#q37---------------------------------------------
q37_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q37) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q37_counts_netq, aes(x = Q37, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de mujeres de la red que hablan de las dificultades para conciliar vida familiar y laboral",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q37_dificultades_mujeres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q37 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q37)
chisq.test(tab_q37)



#q38---------------------------------------------
q38_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q38) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q38_counts_netq, aes(x = Q38, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de hombres de la red que hablan de las dificultades para conciliar vida familiar y laboral",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q38_dificultades_hombres.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q38 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q38)
chisq.test(tab_q38)

#----------------------------------------
#37 vs 38
q37_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q37 = as.numeric(Q37)) %>%
  group_by(Q37) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q38_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  group_by(Q38) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–30)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3) * 1.05

p_q37 <- ggplot(q37_counts, aes(x = Q37, y = freq_rel)) +
  geom_col(fill = "#F38C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q37.  Número de mujeres de la red que hablan de las dificultades para conciliar vida familiar y laboral",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )

p_q38<- ggplot(q38_counts, aes(x = Q38, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q38. Número de hombres de la red que hablan de las dificultades para conciliar vida familiar y laboral",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 7, hjust = 0.5)
  )
library(patchwork)
p_combined <- p_q37 | p_q38
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_q37y38_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q37vs38 <- table(cuidansum_data_netq_anonymized_filtered$Q37, cuidansum_data_netq_anonymized_filtered$Q38)
q3738=chisq.test(tab_q37vs38)

#--------------------------------------------------------------------------------------
#q40---------------------------------------------
q40_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q40) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q41_counts_netq, aes(x = Q40, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de mujeres de la red que hayan tomado medicación contra la depresión, ansiedad o insomnio, desde que han tenido hijos/as",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q40_medicacion_mujeres_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q40 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q40)
chisq.test(tab_q40)

#q41---------------------------------------------
q41_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q41) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q41_counts_netq, aes(x = Q41, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de hombres de la red que hayan tomado medicación contra la depresión, ansiedad o insomnio, desde que han tenido hijos/as",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q41_medicacion_mujeres_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q41 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q41)
chisq.test(tab_q41)

#---------------------------------
#40 vs 41
q40_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q40 = as.numeric(Q40)) %>%
  group_by(Q40) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

q41_counts <- cuidansum_data_netq_anonymized_filtered %>%
  mutate(Q33 = as.numeric(Q33)) %>%
  group_by(Q41) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(freq_rel = n / sum(n))

# Eje X común (escala 0–30)
x_limits <- c(0, 20)

# Eje Y común
y_max <- max(0.3) * 1.05

p_q40 <- ggplot(q40_counts, aes(x = Q40, y = freq_rel)) +
  geom_col(fill = "#F28C8C", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q40.  Número de mujeres de la red que hayan tomado medicación contra la depresión, ansiedad o insomnio, desde que han tenido hijos/as",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 6, hjust = 0.5)
  )

p_q41<- ggplot(q41_counts, aes(x = Q41, y = freq_rel)) +
  geom_col(fill = "#7FD8D8", width = 0.8) +
  scale_x_continuous(
    limits = x_limits,
    breaks = seq(0, 20, by = 2)
  ) +
  scale_y_continuous(limits = c(0, y_max)) +
  labs(
    title = "Q41. Número de hombres de la red que hayan tomado medicación contra la depresión, ansiedad o insomnio, desde que han tenido hijos/as",
    x = "",
    y = "Frecuencia relativa"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6),
    plot.title = element_text(size = 6, hjust = 0.5)
  )
library(patchwork)
p_combined <- p_q40 | p_q41
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_q40y41_gender_hym.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado 17 vs 18
tab_q40vs41 <- table(cuidansum_data_netq_anonymized_filtered$Q40, cuidansum_data_netq_anonymized_filtered$Q41)
q4041=chisq.test(tab_q40vs41)

#-----------------------------------------------------------------------------
#q42---------------------------------------------
q42_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q42) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q42_counts_netq, aes(x = Q42, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Número de parejas de la red donde la mujer dedica más tiempo que el hombre al ocio",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        plot.title = element_text(size = 10) )
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_q42_ocio_mujer_gender.png",width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q42 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q42)
chisq.test(tab_q42)




#-----------------------------------------------------------------------------------------
#---------------Siguientes preguntas directas--------------------------------------------


#q43 vs q44:situación laboral---------------------------------------------------------
# Tabla de frecuencias cruzada
heatmap_gender_q43_q44 <- cuidansum_data_netq_anonymized_filtered %>%
  filter(!is.na(Q43), !is.na(Q44), !is.na(Gender)) %>%
  count(Gender, Q43, Q44) %>%
  group_by(Gender) %>%
  mutate(RelFreq = n / sum(n)) %>%
  ungroup()

# Heatmap frecuencia absoluta con facetas por género
ggplot(heatmap_gender_q43_q44, aes(x = Q43, y = Q44, fill = n)) +
  geom_tile() +
  facet_wrap(~ Gender) +
  geom_text(aes(label = n), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Situación laboral: Persona entrevistada vs Pareja",
    subtitle = "Frecuencia absoluta por género",
    x = "Pareja", y = "Persona entrevistada", fill = "Frecuencia"
  ) +
  facet_wrap(~ Gender) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/situación_labora_gender(heatmap_f_absolutas).png",
       width = 12, height = 4.5, dpi = 300, bg = "white")


# Heatmap frecuencia relativa con facetas por género
ggplot(heatmap_gender_q43_q44, aes(x = Q43, y = Q44, fill = RelFreq)) +
  geom_tile() +
  facet_wrap(~ Gender) +
  geom_text(aes(label = round(RelFreq, 3)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Situación laboral: Persona entrevistada vs Pareja",
    subtitle = "Frecuencia relativa por género",
    x = "Pareja", y = "Persona entrevistada", fill = "Frecuencia relativa"
  ) +
  facet_wrap(~ Gender) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label = round(RelFreq, 3)), color = "white", size = 0.05)

#ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/situación_labora_gender(heatmap_f_relativas).png",width = 12, height = 4.5, dpi = 300, bg = "white")


#grafico de barras q43
ggplot(heatmap_gender_q43_q44, aes(x = Q43, y = RelFreq, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Situación laboral, persona entrevistada ",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_situacion_laboral_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")

# Test Chi-cuadrado
tab_q43 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q43)
chisq.test(tab_q43)


#grafico de barras q44
ggplot(heatmap_gender_q43_q44, aes(x = Q44, y = RelFreq, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Situación laboral, pareja ",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_situacion_laboral_pareja_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")

# Test Chi-cuadrado
tab_q44 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q44)
chisq.test(tab_q44)



#q45: has reducido tu jornada laboral?------------------------------------------
q45_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q45) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q45_gender_counts_netq, aes(x = Q45, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Reducción de jornada laboral ",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_reduccion_jornada_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q45 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q45)
chisq.test(tab_q45)




#q46:¿Has dejado de trabajar en algún momento por tener que atender el cuidado de tus hijos/as y las tareas del hogar?---------------------------------------------
q46_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q46) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q46_gender_counts_netq, aes(x = Q46, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Has dejado de trabajar en algún momento por tener que atender el cuidado de tus hijos?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_abandono_trabajo_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q46 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q46)
chisq.test(tab_q46)


#q47: teletrabajo en los dos ultimos años?-----------------------------------
#tabla con q47 y gender
cols_q47 <- c("Q47_si.empresa", "Q47_si.conciliar", "Q47_si.tiempo.pers", "Q47_no")

# Crear el nuevo data frame con Q47 y Gender
dataq47 <- data.frame(
  Gender = cuidansum_data_netq_anonymized_filtered$Gender,
  Q47 = apply(
    cuidansum_data_netq_anonymized_filtered[cols_q47], 1,
    function(row) {
      paste(
        substr(names(row)[which(row == 1)], 5, nchar(names(row)[which(row == 1)])),
        collapse = "_"
      )
    }
  )
)


q47_gender_counts_netq <- dataq47 %>%
  group_by(Gender, Q47) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q47_gender_counts_netq, aes(x = Q47, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Teletrabajo en los últimos dos años",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_teletrabajo_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q47 <- table(dataq47$Gender, dataq47$Q47)
chisq.test(tab_q47)



#q48:¿Has tenido problemas en tu empleo por tener que ausentarte puntualmente para atender a tus hijos/as
q48_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q48) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q48_gender_counts_netq, aes(x = Q48, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Has tenido problemas en tu empleo por tener que ausentarte puntualmente para atender a tus hijos/as?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_problemas_trabajo_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q48 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q48)
chisq.test(tab_q48)











#q51:En ocasiones los niños/as se enferman, entonces ¿quién se suele quedar en casa?
q51_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q51) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q51_gender_counts_netq, aes(x = Q51, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Quién se queda en casa cuando los hijos/as enferman?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/hist_enferman_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q51 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q51)
chisq.test(tab_q51)




#q52--------------------------------------------------------
q52_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q52) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q52_gender_counts_netq, aes(x = Q52, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cuentas con poyo externo contratado para el cuidado de los hijos/as y/o las tareas domésticas
",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q52_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q52 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q52)
chisq.test(tab_q52)



#q53--------------------------------------------------------
q53_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q53) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q53_gender_counts_netq, aes(x = Q53, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Cuentas con apoyo familiar para el cuidado de los hijos/as y/o las tareas domésticas
",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q53_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q53 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q53)
chisq.test(tab_q53)




#q54:¿cómo percibes el reparto de las tareas domésticas en tu hogar?
q54_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q54) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q54_gender_counts_netq, aes(x = Q54, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percepción del reparto de tareas domésticas",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q54_hist_reparto_tareas_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q54 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q54)
chisq.test(tab_q54)




#q55:¿Y de las tareas de cuidados de tus hijos/as?------------------------------
q55_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q55) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q55_gender_counts_netq, aes(x = Q55, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Percepción del reparto de tareas de cuidados",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q55_hist_reparto_cuidados_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q54 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q54)
chisq.test(tab_q54)





#q56: ¿Te sientes cómodo/a con la forma en que se reparten las tareas domésticas y de cuidados en tu hogar?
q56_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q56) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q56_gender_counts_netq, aes(x = Q56, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Te sientes cómodo/a con la forma en que se reparten las tareas domésticas y de cuidados en tu hogar?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q56_hist_reparto_conformidad_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q56 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q56)
chisq.test(tab_q56)



#q57: ¿Cuál crees que es la principal razón para este reparto detareas?
q57_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q57) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q57_gender_counts_netq, aes(x = Q57, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Cuál crees que es la principal razón para este reparto de tareas?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q57_hist_reparto_razón_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q56 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q56)
chisq.test(tab_q56)




#q60:¿Cómo consideras que debería organizarse el reparto de las tareas domésticas en una pareja en la que ambos trabajan fuera de casa?
q60_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q60) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q60_gender_counts_netq, aes(x = Q60, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Cómo consideras que debería organizarse el reparto de las tareas domésticas en una pareja en la que ambos trabajan fuera de casa?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q60_hist_reparto_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q60 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q60)
chisq.test(tab_q60)





#q61:¿Cómo consideras que debería organizarse el reparto de las tareas de cuidados de hijos/as en una pareja en que ambos trabajan fuera de casa?
q61_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q61) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q61_gender_counts_netq, aes(x = Q61, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Cómo consideras que debería organizarse el reparto de las tareas de cuidados de hijos/as en una pareja en que ambos trabajan fuera de casa?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q61_hist_reparto_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q60 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q60)
chisq.test(tab_q60)





#q62:En tu opinión, la desigualdad de género en el reparto de tareas domésticas y de cuidados es:
q62_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q62) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q62_gender_counts_netq, aes(x = Q62, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "En tu opinión, la desigualdad de género en el reparto de tareas domésticas y de cuidados es:",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q62_hist_reparto_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q60 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q60)
chisq.test(tab_q60)




#Q65 Desde que has tenido tus hijos/as, ¿tomas o has tomado medicación contra la depresión, ansiedad o insomnio?
q65_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q65) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q65_gender_counts_netq, aes(x = Q65, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Desde que has tenido tus hijos/as, ¿tomas o has tomado medicación contra la depresión, ansiedad o insomnio?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q62_hist_medicacion_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q65 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q65)
chisq.test(tab_q65)



#Q66 En una semana típica, ¿con qué frecuencia realizas actividades de ocio personal
q66_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q66) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q66_gender_counts_netq, aes(x = Q66, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "¿Con qué frecuencia realizas actividades de ocio personal?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q62_hist_ocio_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q66 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q66)
chisq.test(tab_q66)


#Q67 En comparación con tu pareja, ¿cómo crees que es tu tiempo de ocio personal
q67_gender_counts_netq <- cuidansum_data_netq_anonymized_filtered %>%
  group_by(Gender, Q67) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q67_gender_counts_netq, aes(x = Q67, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "En comparación con tu pareja, ¿cómo crees que es tu tiempo de ocio personal?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q62_hist_ocio_comparacion_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q67 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q67)
chisq.test(tab_q67)



#Q68 El tiempo que dedicas al ocio personal se explica porque:
cols_q47 <- c("Q47_si.empresa", "Q47_si.conciliar", "Q47_si.tiempo.pers", "Q47_no")

# Crear el nuevo data frame con Q47 y Gender
dataq47 <- data.frame(
  Gender = cuidansum_data_netq_anonymized_filtered$Gender,
  Q47 = apply(
    cuidansum_data_netq_anonymized_filtered[cols_q47], 1,
    function(row) {
      paste(
        substr(names(row)[which(row == 1)], 5, nchar(names(row)[which(row == 1)])),
        collapse = "_"
      )
    }
  )
)


q47_gender_counts_netq <- dataq47 %>%
  group_by(Gender, Q47) %>%
  summarise(n = n()) %>%
  mutate(freq_rel = n / sum(n))

ggplot(q68_gender_counts_netq, aes(x = Q68, y = freq_rel, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "En comparación con tu pareja, ¿cómo crees que es tu tiempo de ocio personal?",
       x = "",
       y = "Frecuencia relativa") +
  scale_fill_manual(values = c("Femenino" = "#F28C8C", "Masculino" = "#7FD8D8")) +
  facet_wrap(~Gender)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
ggsave("C:/Users/Lucia/Documents/Proj_CuidaNSUM/Proj_CuidaNSUM/plots2/q62_hist_ocio_explicacion_gender.png",
       width = 12, height = 4.5, dpi = 300, bg = "white")
# Test Chi-cuadrado
tab_q68 <- table(cuidansum_data_netq_anonymized_filtered$Gender, cuidansum_data_netq_anonymized_filtered$Q68)
chisq.test(tab_q68)


library(openxlsx)

# Crear workbook
wb <- createWorkbook()
addWorksheet(wb, "Resultados")

row_start <- 1

# Función general (sirve para htest y otros objetos)
to_df <- function(obj){

  if(inherits(obj, "htest")){
    return(data.frame(
      statistic = as.numeric(obj$statistic),
      p_value = obj$p.value,
      method = obj$method
    ))
  } else {
    return(as.data.frame(obj))
  }
}

# Lista de objetos
results_list <- list(
  "Q17-18" = t1718,
  "Q19-20" = t1920,
  "Q21-22" = t2122,
  "Q31-32" = q3132,
  "Q33-34" = q3334,
  "Q35-36" = q3536,
  "Q37-38" = q3738,
  "Q40-41" = q4041
)

# Loop automático
for(name in names(results_list)){

  df <- to_df(results_list[[name]])

  # título
  writeData(wb, "Resultados", name, startRow = row_start)

  # tabla
  writeData(wb, "Resultados", df, startRow = row_start + 1)

  # actualizar fila
  row_start <- row_start + nrow(df) + 4
}

# Ajustar columnas
setColWidths(wb, "Resultados", cols = 1:10, widths = "auto")

# Guardar
saveWorkbook(wb, "resultados.xlsx", overwrite = TRUE)
