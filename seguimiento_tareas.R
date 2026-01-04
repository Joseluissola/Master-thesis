

# Loading the necessary packages
library(tidyverse)
library(plan)

#install.packages("plan", repos = "http://cran.rstudio.com")


# 1. Lee el archivo con R normal 
temp_data <- read.csv("tasks.csv", sep = ";", stringsAsFactors = FALSE)

# 2. Conviértelo manualmente al formato que entiende el paquete 'plan'
gt_object <- as.gantt(key = temp_data$Key, 
                      description = temp_data$Description, 
                      start = as.Date(temp_data$Start, format="%d/%m/%Y"), 
                      end = as.Date(temp_data$End, format="%d/%m/%Y"), 
                      done = temp_data$Done, 
                      neededBy = temp_data$NeededBy)

# 3. Grafica
plot(gt_object, main="TFM Plan Gantt Chart")


plot(gt_object,event.label='Report Date',event.time=as.Date('2025/11/01'),
     col.event=c("black"),
     col.done=c("lightblue"),
     col.notdone=c("pink"),
     lty.eventLine=2:1,
     main="TFM Plan Gantt Chart"
)

legend("topright", 
       pch = 22, 
       pt.cex = 1.4, 
       cex = 1.1, 
       pt.bg = c("lightblue", "pink"),
       border = "black", 
       inset = c(0.02, 0.02), # Mueve la leyenda un 2% hacia adentro desde los bordes
       text.width = 0.1,
       legend = c("Completed", "Not Yet Done"), 
       bg = "white")
