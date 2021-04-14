library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw())


tr <- read.delim("C:\\hS\\LearnR\\Frac\\INT100.txt",
                 sep = " ")


x <- dim(tr)[1]
y <- dim(tr)[2]
x
y


tr <- tr[2:(x-2),]
str(tr)


tr[,2:y] <- lapply(tr[,2:y], as.numeric)
tr$JobTime <- dmy_hms(tr$JobTime)

head(tr)
str(tr)

slur_multiplier <- 10

prc_plot <- ggplot(tr, aes(x = JobTime)) + 
  geom_line(aes(y = TR_PRESS), color = "red") + 
  geom_line(aes(y = SLUR_RATE*slur_multiplier), color = "blue") + 
  geom_line(aes(y = PROP_CON/10), color = "seagreen4") + 
  geom_line(aes(y = BH_PROP_CON/10), color = "orange") + 
  labs(title = "PRC Plot", x = "Time", y = "Pressure (MPa) / Rate (m3/min)x10") + 
  scale_y_continuous(
    limits = c(0,80),
    breaks = seq(0,100,10), 
    sec.axis = sec_axis(~.*10, name = "Prop Con(kgPA)")
  )
prc_plot

add_plot <- ggplot(tr, aes(x = JobTime)) + 
  geom_line(aes(y = B596_CONC), color = "red") + 
  geom_line(aes(y = B545C_CONC), color = "blue") + 
  geom_line(aes(y = CFLD_RATE*1/8), color = "seagreen4") + 
  labs(title = "Additives Plot Plot", x = "Time", y = "Concentration (L/m3)", ) + 
  scale_y_continuous(
    limits = c(0,1),
    breaks = seq(0,8,0.5), 
    sec.axis = sec_axis(~.*8, name = "Concentration (kg/m3) & Clean Fluid Rate (m3/min)")
  )
add_plot
