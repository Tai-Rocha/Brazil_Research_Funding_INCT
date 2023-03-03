#######################################################
## Code to build ggplot2 graphics 
## R version 4.2.2
## Auhtor code: Tainá Rocha 
#######################################################


## Library package
library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(tidyverse)
library(sf)
library(tmap)


#1 Read the csv file
data = read.csv("data/table_all.csv")

#1 test  Create the plot
ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5))

#2 test  Create the plot


ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5))


#3 test  Create the plot- Ok

ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme(plot.title = element_text(hjust = 0.5))

# 4 test Create the plot remove grey background

ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 5 test Create de plot remove the box and flatten the bars

ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack", color="white", width=0.5) +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# 6 test Create the plot remove grey lines and make bars more thin

ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack", color="white", width=0.8) +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 7 

ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack", color=NA, width=0.8) +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of N Papers per State by Gender",
       x="State", y="Number of N Papers",
       fill="Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# 8 Works ok

ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack", color=NA, width=0.8) +
  scale_fill_manual(values=c("cornflowerblue", "hotpink")) +
  labs(title="Number of Publications per State by Gender",
       x="Region", y="Number of Publications",
       fill="Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line())


################################### MAp ################################

brazil_map = st_read("data-raw/Shapefile/BR_UF_2021.shp")


# Load your data with the number of approved results per state
approved_data = data.frame(State = c("AM", "BA", "CE", "DF", "MG", "PA", "PB", "PE", "PI", "PR", "RJ", "RS", "SC", "SP"), Result = c(1, 1, 4, 1, 9, 2, 1, 5, 1, 4, 7, 6, 2, 13))

# Join the data with the map
brasil_map_data = left_join(brazil_map, approved_data, by = c("SIGLA" = "State"))

#brasil_map_data$circle_size = sqrt(brasil_map_data$Result)


# create map
tm_shape(brasil_map_data) +
  tm_polygons("Result", palette = "Blues", style = "quantile", border.col = "black") +
  tm_text("NM_UF", size = 0.5) +
  tm_layout(frame = FALSE)


tm_shape(brasil_map_data) + 
  tm_polygons() +
  tm_shape(brasil_map_data) +
  tm_dots(size = "Result", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = "Results by State", legend.title.size = 1.5, legend.text.size = 1.2)

##### Test random

tm_shape(brasil_map_data) +
  tm_polygons("Result", palette = "Blues", style = "quantile", border.col = "black") +
  tm_dots(size = "Result", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = "Results by State", legend.title.size = 1.5, legend.text.size = 1.2)


