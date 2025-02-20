#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readr)
#install.packages("ggpubr")
library(ggpubr)
install.packages("ggpmisc")
library(ggpmisc)
# install.packages("pracma")
# library(pracma)
#install.packages("quantmod")
library(quantmod)


setwd("~/student_documents/UBC/Research/Writing, talks, notes\\edulis Science\\contour stuff")

curvature.files <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) # leave out folder path
head(curvature.files, n = 40)

# make a data frame of all the csvs 
# and a column for species using file name
curvatures <- read_delim(curvature.files, 
                      delim = ",", 
                      id = "species") %>%
  rename(perim_start = " ") %>%
  mutate(species = substr(species, 1, nchar(species)-6)) %>% #drop file extension
  mutate(abs.curv = abs(Curvature)) %>%
  group_by(species) %>%
  print()

# get a table with x-axis peak locations
peak.filtered.test.plot <- test.plot %>%
  mutate(peaks = find_peaks(Curvature, ignore_threshold = 0.8, span = 10, strict = FALSE)) %>% # column indicating peaks
  filter(peaks == "TRUE") %>% # make peaks findable
  print


# figuring out manipulation on a small scale
test.plot <- read_csv("~/student_documents/UBC/Research/Writing, talks, notes\\edulis Science\\contour stuff/americanus 1.csv") %>%
  rename(perim_start = ...1) %>%
  mutate(abs.curv = abs(Curvature)) %>%
  mutate(peak.start.perim = perim_start - peak.filtered.test.plot$perim_start[1]) %>%
  mutate(pos.peak.start = c(peak.start.perim[peak.start.perim >= 0], 
                            (abs(peak.start.perim[peak.start.perim < 0]))+ max(peak.start.perim))) %>%
  mutate(curv.shift = c(Curvature[perim_start >= peak.filtered.test.plot$perim_start[1]], 
                        rev(Curvature[perim_start < peak.filtered.test.plot$perim_start[1]]))) %>%
  print()



# confirm
ggplot(data = test.plot, aes(x = pos.peak.start, y = curv.shift)) +
  geom_point() #+
  #xlab("perimeter position") #+
  # geom_vline(xintercept = 2894)+
  # geom_vline(xintercept = 2063)


