#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(readr)
library(ggpubr)
#install.packages("ggpmisc")
library(ggpmisc)



setwd("~/student_documents/UBC/Research/Writing, talks, notes\\edulis Science\\contour stuff")

curvature.files <- # list of file names
  list.files(pattern = "\\.csv$",
             full.names = F) # leave out folder path
print(curvature.files)

####    first read in of df    ####
# make a data frame of all the csvs 
# and a column for species using file name
curvatures <- read_delim(curvature.files, 
                      delim = ",", 
                      id = "species") %>%
  rename(perim_start = " ") %>%
  mutate(indvd = substr(species, 1, nchar(species)-4), .after = species) %>% #drop file extension
  mutate(species = substr(species, 1, nchar(species)-6)) %>% #drop file extension and individual number
  #mutate(abs.curv = abs(Curvature)) %>%
  #filter(indvd == "americanus 5") %>%
  select(species, indvd, perim_start, Curvature) %>%
  print()

####    find peaks as a separate df    ####
peak.filt.curv <- curvatures %>%
  group_by(indvd) %>%
  mutate(peaks = find_peaks(Curvature, ignore_threshold = 0.6, span = 3, strict = FALSE)) %>% # column indicating peaks
  filter(peaks == "TRUE") %>% # make peaks findable
  filter(
    !(species == "americanus" & Curvature < 0.0056) &
      !(species == "anomalus" & Curvature < 0.03) & ###   peak finding issue for 6, 7, 8??
      !(species == "pallidipes" & Curvature < 0.0275) &
      !(species == "edulis" & Curvature < 0.02) &
      !(species == "trivittatus" & Curvature < 0.02)
    ) %>%
  filter(perim_start == perim_start[1]) %>%
  rename(perim_subtract = perim_start) %>%
  select(indvd, perim_subtract) %>%
  print()

curvatures.aligned <- curvatures %>% 
  left_join(peak.filt.curv, by = "indvd") %>%
  group_by(indvd) %>%
  mutate(peak.start.perim = perim_start - perim_subtract) %>%
  mutate(pos.peak.start = c(peak.start.perim[peak.start.perim >= 1], 
                            rev(abs(peak.start.perim[peak.start.perim < 1]) + max(peak.start.perim)))) %>%
  mutate(curv.shift = c(Curvature[perim_start >= perim_subtract],
                        Curvature[perim_start < perim_subtract])) %>% # remake curvature column so that it aligns with the new x-axis (formerly negative region needs to be reversed)
  #filter(species == "anomalus") %>%
  print()

str(curvatures.aligned)


# test some plots to make sure
ggplot(data = filter(curvatures.aligned, indvd == "trivittatus 7"), 
       #aes(x = perim_start, y = Curvature)
       #aes(x = peak.start.perim, y = Curvature)
       #aes(x = pos.peak.start, y = Curvature)
       aes(x = pos.peak.start, y = curv.shift)
       
       ) +
  geom_point() #+
#xlab("perimeter position") #+
#geom_vline(xintercept = 1298)



####    find peaks as a separate df    ####  subsequent weird shifting
# peak.filt.curv <- curvatures %>%
#   mutate(peaks = find_peaks(Curvature, ignore_threshold = 0.6, span = 3, strict = FALSE)) %>% # column indicating peaks
#   filter(peaks == "TRUE") %>% # make peaks findable
#   filter(!(species == "americanus" & Curvature < 0.0056)) %>%
#   # filter(!(species == "anomalus" & Curvature < 0.03)) %>%
#   # filter(!(species == "pallidipes" & Curvature < 0.0275)) %>%
#   # filter(!(species == "edulis" & Curvature < 0.02)) %>%
#   # filter(!(species == "trivittatus" & Curvature < 0.02)) %>%
#   print()

# slide the data so it always starts at the first peak (right hand sac tip in images)
curvatures.aligned <- curvatures %>%
  group_by(indvd) %>%
  mutate(peak.start.perim = perim_start - peak.filt.curv$perim_start[1]) %>% # slide the x-axis zero to be at the first peak (now has + and -)
  mutate(pos.peak.start = c(peak.start.perim[peak.start.perim >= 0],
                            (abs(peak.start.perim[peak.start.perim < 0])) + max(peak.start.perim))) %>% # remake x-axis to start from zero, and add negative values to the end as positive
  mutate(curv.shift = c(Curvature[perim_start >= peak.filt.curv$perim_start[1]],
                        rev(Curvature[perim_start < peak.filt.curv$perim_start[1]]))) %>% # remake curvature column so that it aligns with the new x-axis (formerly negative region needs to be reversed)
  print()




#####   single indvd wrangle

# figuring out manipulation on a small scale
test.plot <- read_csv("~/student_documents/UBC/Research/Writing, talks, notes\\edulis Science\\contour stuff/americanus 2.csv") %>%
  rename(perim_start = ...1) %>%
  mutate(abs.curv = abs(Curvature)) %>%
  print()

# get a table with x-axis peak locations
peak.filtered.test.plot <- test.plot %>%
  mutate(peaks = find_peaks(Curvature, ignore_threshold = 0.6, span = 3, strict = F)) %>% # column indicating peaks
  filter(peaks == "TRUE") %>% # make peaks findable
  filter(Curvature > 0.0056) %>%
  print()

test.plot <- test.plot %>%
  mutate(peak.start.perim = perim_start - peak.filtered.test.plot$perim_start[1]) %>%
  mutate(pos.peak.start = c(peak.start.perim[peak.start.perim >= 0],
                            (abs(peak.start.perim[peak.start.perim < 0]))+ max(peak.start.perim))) %>%
  mutate(curv.shift = c(Curvature[perim_start >= peak.filtered.test.plot$perim_start[1]],
                        rev(Curvature[perim_start < peak.filtered.test.plot$perim_start[1]]))) %>%
  print()


# confirm
ggplot(data = test.plot, #aes(x = perim_start, y = Curvature)
       aes(x = pos.peak.start, y = curv.shift)
       ) +
  geom_point() +
  #xlab("perimeter position") #+
  geom_vline(xintercept = 3507)
  # geom_vline(xintercept = 2063)


##############################################################################################################################

curvatures.aligned <- curvatures %>% 
  group_by(indvd) %>%
  mutate(peak.start.perim = perim_start - peak.filt.curv$perim_start[1])


curvatures.aligned <- curvatures %>%
  left_join(peak.filt.curv, by = "indvd", suffix = c("", ".peak.filt.curv")) %>%  # Join df2 to bring in perim_start values
  mutate(peak.start.perim = perim_start - perim_start.peak.filt.curv) %>%  # Perform the correct subtraction
  select(-perim_start.peak.filt.curv)  # Remove the extra column from df2 if not needed
