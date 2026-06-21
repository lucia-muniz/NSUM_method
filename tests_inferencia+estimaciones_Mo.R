#ESTIMACIONES CHI CUADRADO, PREGUNTAS 66:89, 132:136--------------------------------------------------------------------
library(dplyr)
library(openxlsx)

#Chisq test-----------------------------------------------------------------------
# Definir rangos de columnas a analizar
cols_ranges <- c(47:65, 90:131, 137:147)


# Inicializar lista para guardar resultados
results <- list()

# Bucle sobre columnas
for (col_index in cols_ranges) {
  # Nombre de la columna
  var_name <- colnames(cuidansum_data_netq_anonymized_filtered)[col_index]

  # Crear tabla de contingencia con Gender
  tab <- table(cuidansum_data_netq_anonymized_filtered$Gender,
               cuidansum_data_netq_anonymized_filtered[[col_index]])

  # Hacer test Chi-cuadrado
  chi <- chisq.test(tab)

  # Guardar resultados
  results[[length(results) + 1]] <- data.frame(
    Variable = var_name,
    P_value = chi$p.value
  )
}

# Combinar todos los resultados en un solo data.frame
results_df <- bind_rows(results)

# Exportar a Excel
download_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "chi_squared_results.xlsx")
write.xlsx(results_df, download_path, rowNames = FALSE)



#T-test-------------------------------------------------------------------------------
#--------------------------------------------------------------
# Columnas numéricas a analizar
num_cols <- c(66:89, 132:136)

# Lista para resultados
t_results <- list()

for (col_index in num_cols) {

  var_name <- colnames(cuidansum_data_netq_anonymized_filtered)[col_index]

  # Subconjunto limpio: Gender + variable actual
  df <- cuidansum_data_netq_anonymized_filtered %>%
    select(Gender, all_of(var_name)) %>%
    filter(!is.na(Gender))  # filtramos solo Gender por ahora

  # Convertir variable a numérica
  df[[var_name]] <- as.numeric(df[[var_name]])

  # Filtrar filas donde la variable sea NA
  df <- df %>% filter(!is.na(.data[[var_name]]))

  # Hacer t-test solo si hay datos en ambos grupos
  if(length(unique(df$Gender)) > 1 && all(table(df$Gender) > 0)) {
    t_res <- t.test(df[[var_name]] ~ df$Gender)
    p_val <- t_res$p.value
  } else {
    p_val <- NA  # no se puede hacer t-test
  }

  # Guardar resultados
  t_results[[length(t_results) + 1]] <- data.frame(
    Variable = var_name,
    P_value = p_val
  )
}

# Combinar resultados
t_results_df <- bind_rows(t_results)

# Exportar a Excel en la carpeta Descargas
download_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "t_test_results.xlsx")
write.xlsx(t_results_df, download_path, rowNames = FALSE)

# Mostrar resultados
t_results_df


#ESTIMACIONES MoR--------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
MoR = function(y,degrees){
  mean(y/degrees)
}

MoR_estimaciones= cuidansum_data_netq_anonymized_filtered %>%
  summarise(across(
    c(Q17:Q22, Q24, Q25, Q27:Q29, Q31:Q38, Q40:Q42),
    ~ MoR(.x, degrees = cuidansum_data_netq_anonymized_filtered$Q15)
  ))

MoR_estimaciones
library(openxlsx)

# Crear Excel
wb <- createWorkbook()
addWorksheet(wb, "Datos")

# Escribir tabla
writeData(wb, "Datos", MoR_estimaciones)

# Ajustar columnas (opcional pero recomendable)
setColWidths(wb, "Datos", cols = 1:ncol(MoR_estimaciones), widths = "auto")

# Guardar archivo
saveWorkbook(wb, "estimacionesMoR.xlsx", overwrite = TRUE)



#Estimaciones preguntas directas-------------------------------------------------
#---------------------------------------------------------------------------
#Q45
Q45_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
  filter(Gender == "Masculino", !is.na(Q45)) %>%
  summarise(
    p = mean(Q45 != "No, nunca las he pedido")
  )

Q45_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
  filter(Gender == "Femenino", !is.na(Q45)) %>%
  summarise(
    p = mean(Q45 != "No, nunca las he pedido")
  )


#Q47---------------------------------------------------------------
Q47_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
  filter(Gender == "Masculino", !is.na(Q47_si.conciliar)) %>%
  summarise(
    p = mean(Q47_si.conciliar == 1)
  )

Q47_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
  filter(Gender == "Femenino", !is.na(Q47_si.conciliar)) %>%
  summarise(
    p = mean(Q47_si.conciliar == 1)
  )


#-------------------------------------------------------------------------
#Q48
Q48_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Masculino", !is.na(Q48)) %>%
    summarise(
      p = mean(Q48 != "Nunca")
    )

  Q48_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Femenino", !is.na(Q48)) %>%
    summarise(
      p = mean(Q48 != "Nunca")
    )


  #----------------------------------------------------------------------------
  #Q52
  Q52_estimacion <- cuidansum_data_netq_anonymized_filtered %>%
    summarise(
      p = mean(Q52 != "No")
    )


  Q52_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Masculino", !is.na(Q52)) %>%
    summarise(
      p = mean(Q52 != "No")
    )

  Q52_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Femenino", !is.na(Q52)) %>%
    summarise(
      p = mean(Q52 != "No")
    )

  #----------------------------------------------------------------------------
  #Q53
  Q53_estimacion <- cuidansum_data_netq_anonymized_filtered %>%
    summarise(
      p = mean(Q53 != "No")
    )



  Q53_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Masculino", !is.na(Q53)) %>%
    summarise(
      p = mean(Q52 != "No")
    )

  Q53_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Femenino", !is.na(Q53)) %>%
    summarise(
      p = mean(Q53 != "No")
    )


  #----------------------------------------------------------------------------
  #Q54
  Q54_estimacion <- cuidansum_data_netq_anonymized_filtered %>%
    summarise(
      p = mean(Q54 == "Equitativo")
    )



  Q54_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Masculino", !is.na(Q54)) %>%
    summarise(
      p = mean(Q54 != "Equitativo")
    )

  Q54_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Femenino", !is.na(Q54)) %>%
    summarise(
      p = mean(Q54 != "Equitativo")
    )



  #---------------------------------------------------------------------------
  #Q59
  Q59_estimacion <- cuidansum_data_netq_anonymized_filtered %>%
    summarise(
      p = mean(Q59 == "Habitualmente, es un motivo de conflicto frecuente en casa")
    )

  Q59_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Masculino", !is.na(Q59)) %>%
    summarise(
      p = mean(Q59 != "Habitualmente, es un motivo de conflicto frecuente en casa")
    )

  Q59_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Femenino", !is.na(Q59)) %>%
    summarise(
      p = mean(Q59 != "Habitualmente, es un motivo de conflicto frecuente en casa")
    )


  #---------------------------------------------------------------------------
  #Q60
  Q60_estimacion_masc <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Masculino", !is.na(Q60)) %>%
    summarise(
      p = mean(Q60 == "La mujer debería encargarse de la mayoría de las tareas")
    )

  Q60_estimacion_fem <- cuidansum_data_netq_anonymized_filtered %>%
    filter(Gender == "Femenino", !is.na(Q60)) %>%
    summarise(
      p = mean(Q60 == "La mujer debería encargarse de la mayoría de las tareas")
    )






#DATAFRAME DIRECTAS VS INDIRECTAS---------------------------------------------
  library(tibble)

  df_directas_vs_indirectas <- tibble(
    nombre = character(),
    directas = numeric(),
    indirectas = numeric()
  )

nombres= c("Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q24", "Q25", "Q27", "Q28", "Q29", "Q33", "Q34")

directas= c(Q45_estimacion_masc[1,1], Q45_estimacion_fem[1,1], Q47_estimacion_masc[1,1] , Q47_estimacion_fem[1,1],
            Q48_estimacion_masc[1,1], Q48_estimacion_fem[1,1], Q52_estimacion[1,1],  Q53_estimacion[1,1],  Q54_estimacion[1,1],
            Q59_estimacion[1,1],  Q52_estimacion[1,1], Q60_estimacion_masc[1,1], Q60_estimacion_fem[1,1])
MoR_reducido= MoR_estimaciones[,-c(12, 13) ]


  df_directas_vs_indirectas <- data.frame(
    nombre = as.character(nombres),
    indirectas = as.numeric(MoR_reducido[1, 1:13]),
    directas= as.numeric(directas)
  )


  # Crear Excel
  wb <- createWorkbook()
  addWorksheet(wb, "Datos")

  # Escribir tabla
  writeData(wb, "Datos", df_directas_vs_indirectas)

  # Ajustar columnas (opcional pero recomendable)
  setColWidths(wb, "Datos", cols = 1:ncol(df_directas_vs_indirectas), widths = "auto")

  # Guardar archivo
  saveWorkbook(wb, "directas_vs_indirectas.xlsx", overwrite = TRUE)







#GRÁFICOS DIRECTAS VS INDIRECTAS-------------------------------------
library(ggplot2)
library(corrplot)

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

pdf("C:/Users/Lucia/Downloads/grafico_cor2.pdf", width = 6, height = 6)

ggplot(df_directas_vs_indirectas,
       aes(x = directas,
           y = indirectas)) +

  geom_point(size = 2.5) +

  geom_smooth(method = "lm",
              se = TRUE,
              color = "blue",
              linewidth = 1) +

  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +

  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +

  labs(
    subtitle = paste0("Correlación de Pearson: r = ", round(cor_val, 3)),
    x = "Respuestas directas",
    y = "Respuestas indirectas"
  ) +

  base_theme

dev.off()

pdf("C:/Users/Lucia/Downloads/grafico_rel.pdf", width = 6, height = 6)

ggplot(df_directas_vs_indirectas,
       aes(x = directas,
           y = indirectas,
           label = nombre)) +

  geom_point(size = 2.5) +

  geom_text(vjust = -0.5, size = 2.5, check_overlap = TRUE) +

  geom_abline(slope = 1,
              intercept = 0,
              linetype = "dashed",
              color = "red",
              linewidth = 0.8) +

  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +

  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +

  labs(

    subtitle = "Línea roja: igualdad perfecta (y = x)",
    x = "Respuestas directas",
    y = "Respuestas indirectas"
  ) +

  base_theme

dev.off()
