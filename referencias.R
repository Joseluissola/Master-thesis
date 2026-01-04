
#if (!require("readxl")) install.packages("readxl")
library(readxl)
library(vcd)

#install.packages("vcd", repos = "http://cran.r-project.org", type = "win.binary")
# Leer la hoja de Excel
mis_refs <- read_excel("referencias.xlsx")
tabla_contingencia <- xtabs(~ Area  + Methodology, data = mis_refs)
tabla_contingencia <- as.matrix(tabla_contingencia)
par(mar = c(5, 15, 5, 4)) # Ajuste de márgenes para que no se corten los nombres
mosaic(
  tabla_contingencia, 
  shade = TRUE, 
  legend = TRUE,
  direction = c("v", "h"), 
  main = "Mosaic plot of Fields and Methodology",
  main_gp = gpar(fontsize = 14, fontface = "bold"),
  sub = "Analysis of Bibliographic References",
  labeling_args = list(
    set_varnames = c(Area = "Research Fields", Methodology = "Methodology"),
    rot_labels = c(left = 0, top = 0),
    just_labels = c(left = "right"),
    offset_labels = c(left = 0.5, top = 0.5),
    gp_labels = gpar(cex = 0.76),
    # AQUÍ CAMBIAS EL TAMAÑO: cex = 0.7 hace la letra un 30% más pequeña
    offset_varnames = c(left = 12, top = 1),
    gp_text = gpar(cex = 0.7)
  ),
  # Añadimos un poco de espacio entre bloques para que no se vea saturado
  spacing = spacing_equal(unit(0.8, "lines")) 
)

# Esto abrirá una ventana grande y guardará el gráfico con alta resolución
dev.copy(png, filename="mosaico_final.png", width=1200, height=900, res=120)



#Exportar tabla de contingencia

library(writexl)
# 1. Crear la tabla base
tabla_base <- xtabs(~ Methodology + Area, data = mis_refs)

# 2. Añadir los sumatorios (Totales) a las filas y columnas
tabla_con_sumas <- addmargins(tabla_base)

# 3. Visualizar el resultado en la consola
print(tabla_con_sumas)
df_excel <- as.data.frame.matrix(tabla_con_sumas)
# 4. (Opcional) Los nombres de las metodologías están en los nombres de fila. 
# Para que aparezcan en una columna de Excel, hacemos lo siguiente:
df_excel <- cbind(Methodology = rownames(df_excel), df_excel)

# 5. Exportar el archivo
write_xlsx(df_excel, "Tabla_Contingencia_Final.xlsx")

#Articulo mas antiguo y reciente:

# Asegúrate de que la columna 'Year' sea numérica
mis_refs$Year <- as.numeric(mis_refs$Year)

# Identificar el año mínimo y máximo
oldest_year <- min(mis_refs$Year, na.rm = TRUE)
newest_year <- max(mis_refs$Year, na.rm = TRUE)

# Ver los detalles de esos artículos
oldest_paper <- mis_refs[mis_refs$Year == oldest_year, ]
newest_paper <- mis_refs[mis_refs$Year == newest_year, ]

cat("El artículo más antiguo es de:", oldest_year, "\n")
cat("El artículo más reciente es de:", newest_year, "\n")


# Crear tabla de frecuencias por año
publicaciones_por_año <- as.data.frame(table(mis_refs$Year))
colnames(publicaciones_por_año) <- c("Year", "Count")
publicaciones_por_año$Year <- as.numeric(as.character(publicaciones_por_año$Year))

# Graficar la evolución temporal
library(ggplot2)

ggplot(publicaciones_por_año, aes(x = Year, y = Count)) +
  geom_line(color = "#2c3e50", size = 1) +
  geom_point(color = "#e74c3c", size = 2) +
  theme_minimal() +
  labs(title = "Evolution of Bibliographic Publications",
       x = "Year of Publication",
       y = "Number of Articles") +
  scale_x_continuous(breaks = seq(oldest_year, newest_year, by = 2))

