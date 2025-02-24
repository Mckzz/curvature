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
  mutate(peak.start.perim = perim_start - perim_subtract) %>% # centers zero on the first peak, axis now has + and -
  mutate(pos.peak.start = c(peak.start.perim[peak.start.perim >= 0], 
                            rev(abs(peak.start.perim[peak.start.perim < 0]) + max(peak.start.perim)))) %>% # same as perim_start?
  mutate(curv.shift = c(Curvature[perim_start >= perim_subtract],
                        Curvature[perim_start < perim_subtract])) %>% # remake curvature column so that it aligns with the new x-axis
  mutate(norm_pos.peak.start = pos.peak.start / max(pos.peak.start)) %>%
  #mutate(norm2_pos.peak.start = (pos.peak.start - min(pos.peak.start)) / max(pos.peak.start)) %>%
  #filter(species == "anomalus") %>%
  print()

str(curvatures.aligned)

curvatures.aligned_normalized <- curvatures.aligned %>%
  group_by(indvd) %>%
  mutate(norm_pos.peak.start = (pos.peak.start - min(pos.peak.start)) / (max(pos.peak.start) - min(pos.peak.start))) %>%
  ungroup() %>%
  print()

# Create a common normalized x-axis
common_pos_peak_start <- seq(0, 1, length.out = 1000)

# Interpolate curv.shift for each individual
curvatures.aligned_interpolated <- curvatures.aligned_normalized %>%
  group_split(indvd) %>%  # Split by individual
  map_dfr(~ {
    tibble(
      indvd = unique(.x$indvd),
      norm_pos.peak.start = common_pos_peak_start,  # Use the same common x-axis
      curv.shift = approx(
        x = .x$norm_pos.peak.start, 
        y = .x$curv.shift, 
        xout = common_pos_peak_start, 
        rule = 2
      )$y
    )
  }) %>%
  mutate(species = substr(indvd, 1, nchar(indvd)-2)) %>% #drop ndividual number
  ungroup() %>%
  group_by(species, norm_pos.peak.start) %>%
  mutate(mean_curv.shift = mean(curv.shift))

check.row <- curvatures.aligned_interpolated %>%
  group_by(indvd) %>%
  summarise(n_norm_pos.peak.start = length(norm_pos.peak.start), n_curv.shift = length(curv.shift))


# test some plots to make sure
ggplot(data = filter(curvatures.aligned_interpolated, indvd == "trivittatus 7"), 
       #aes(x = perim_start, y = Curvature)
       #aes(x = peak.start.perim, y = Curvature)
       #aes(x = pos.peak.start, y = Curvature)
       aes(x = norm_pos.peak.start, y = curv.shift)
       ) +
  geom_point() #+
#xlab("perimeter position") #+
#geom_vline(xintercept = 1298)

ggplot(data = curvatures.aligned_interpolated,
       aes(x = norm_pos.peak.start, y = mean_curv.shift,
           group = species,
           colour = species)
       ) +
  geom_point()




#####   single indvd wrangle

# figuring out manipulation on a small scale
test.plot <- read_csv("~/student_documents/UBC/Research/Writing, talks, notes\\edulis Science\\contour stuff/anomalus 8.csv") %>%
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
  geom_vline(xintercept = 836)
  # geom_vline(xintercept = 2063)


##############################################################################################################################

curvatures.aligned <- curvatures %>% 
  group_by(indvd) %>%
  mutate(peak.start.perim = perim_start - peak.filt.curv$perim_start[1])


curvatures.aligned <- curvatures %>%
  left_join(peak.filt.curv, by = "indvd", suffix = c("", ".peak.filt.curv")) %>%  # Join df2 to bring in perim_start values
  mutate(peak.start.perim = perim_start - perim_start.peak.filt.curv) %>%  # Perform the correct subtraction
  select(-perim_start.peak.filt.curv)  # Remove the extra column from df2 if not needed
