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

data_geo = read.csv("data-raw/lat_long.csv")

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

png(file="SEX.png",
    width=12, height=10, units="in", res=300)
ggplot(data, aes(x=Region, y=`N_papers`, fill=Gender)) +
  geom_bar(stat="identity", position="stack", color=NA, width=0.5) +
  scale_fill_manual(values=c("#39578c", "orange")) +
  labs(title=" ",
       x=" ", y="Number of Publications",
       fill="SEX") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(size = 14))
dev.off()

################################### MAp ################################

brazil_map = st_read("data-raw/Shapefile/BR_UF_2021.shp")

# Load your data with the number of approved results per state
approved_data = data.frame(State = c("AM", "BA", "CE", "DF", "MG", "PA", "PB", "PE", "PI", "PR", "RJ", "RS", "SC", "SP"), Result = c(1, 1, 4, 1, 9, 2, 1, 5, 1, 4, 7, 6, 2, 13))

# Join the data with the map
brasil_map_data = left_join(brazil_map, approved_data, by = c("SIGLA" = "State")) |> 
  dplyr::rename("Proportion of accepted proposals" = Result)

## Write as shapefile
sf::st_write(brasil_map_data, "data/shp/brasil_map_data.shp")


# create map 1
tm_shape(brasil_map_data) +
  tm_polygons("INCTs approved", palette = "Blues", style = "pretty", border.col = "black") +
  tm_text("NM_UF", size = 0.5) +
  tm_layout(frame = FALSE)


##### Test bolinhas

tm_shape(brasil_map_data) +
  tm_polygons("INCTs approved", palette = "Blues", style = "quantile", border.col = "black") +
  tm_dots(size = "INCTs approved", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = "Results by State", legend.title.size = 1.5, legend.text.size = 1.2)


tm_shape(brasil_map_data) +
  tm_polygons() +
  tm_dots(size = "Proportional number of accepted proposals", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = " ", legend.title.size = 1.5, legend.text.size = 1.2)



# Create the map
png(file="Map_v3.png",
    width=12, height=10, units="in", res=300)
tm_shape(brasil_map_data) +
  tm_polygons(border.col = "black", lwd = 2.0, lty = "solid") +
    tm_dots(size = "Proportion of accepted proposals", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  #tm_text(" ", size = 0.8, col = "black", root = TRUE, case = "upper", just = "top", xmod= 0.7, ymod = 0) +
  tm_layout(title = " ", legend.title.size = 1.5, legend.text.size = 1.2)
dev.off()

tm_shape(brasil_map_data) +
  tm_polygons() +
  tm_dots(size = "Proportional number of accepted proposals", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_labels(text = "SIGLA", size = 0.8, col = "black", bg.color = "white", bg.alpha = 0.8, align = c("left", "bottom")) +
  tm_layout(title = " ", legend.title.size = 1.5, legend.text.size = 1.2)


############# ALL
all_data = data.frame(State = c("AM", "BA", "CE", "DF", "MG", "PA", "PB", "PE", "PI", "PR", "RJ", "RS", "SC", "SP", "AL", "ES", "GO", "MS", "MT", "RN", "RO", "TO", "AC", "RR", "AP", "MA", "SE"), Result = c(1, 1, 4, 1, 9, 2, 1, 5, 1, 4, 7, 6, 2, 13, 0, 0, 0, 0, 0, 0, 0, 0, NA, NA, NA, NA, NA))

## Join all 

brasil_map_data_2 = left_join(brazil_map, all_data, by = c("SIGLA" = "State")) |> 
  dplyr::rename("INCTs approved" = Result)

## Write as shapefile
#sf::st_write(brasil_map_data_2, "data/shp/brasil_map_data_all.shp")

#brasil_map_data$circle_size = sqrt(brasil_map_data$Result)


### Circles

tm_shape(brasil_map_data_2) +
  tm_polygons() +
  tm_dots(size = "INCTs approved", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = "Results by State", legend.title.size = 1.5, legend.text.size = 1.2)

# create map 2
tm_shape(brasil_map_data_2) +
  tm_polygons("INCTs approved", palette = "Blues", style = "pretty", border.col = "black") +
  tm_text("NM_UF", size = 0.5) +
  tm_layout(frame = FALSE)

##### Test random 2

tm_shape(brasil_map_data_2) +
  #tm_polygons("INCTs approved", palette = "Blues", style = "quantile", border.col = "black") +
  tm_dots(size = "INCTs approved", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = "Results by State", legend.title.size = 1.5, legend.text.size = 1.2)




################# 
tm_shape(brasil_map_data) + 
  tm_polygons() +
  tm_shape(brasil_map_data) +
  tm_dots(size = "INCTs approved", col = "red", border.col = NA, alpha = 0.5, shape = 21) +
  tm_layout(title = "Results by State", legend.title.size = 1.5, legend.text.size = 1.2)




