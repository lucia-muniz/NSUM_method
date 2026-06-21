# Histogramas para preguntas indirectas

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)


variables <- c("Q15", "Q17", "Q18", "Q19", "Q20",
               "Q21", "Q22", "Q24", "Q25", "Q27", "Q28", "Q29", "Q31", "Q32",
               "Q33", "Q34", "Q35", "Q36", "Q37", "Q38", "Q40", "Q41", "Q42")

if (!dir.exists("histograms")) {
  dir.create("histograms")
}

for (var in variables) {
  # Personalizar título y eje X para Q15
  x_label <- ifelse(var == "Q15", "network size", var)
  plot_title <- ifelse(var == "Q15", "Network Size", paste0("Histograma de ", var))

  p <- ggplot(data1, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white", binwidth = 1) +
    xlab(x_label) +
    theme(axis.text.x = element_text(vjust = 0.5, hjust = 1, size = 36),
          axis.text.y = element_text(vjust = 0.5, hjust = 1, size = 36),
          axis.title = element_text(size = 40),
          legend.text = element_text(size = 26), legend.title = element_blank(),
          plot.title = element_text(face = "bold", size = 40),
          legend.position = "bottom", legend.box = "horizontal") +
    ggtitle(plot_title)

  ggsave(filename = paste0("histograms/hist_", var, ".png"), plot = p)
}
