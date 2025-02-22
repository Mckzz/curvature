


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
  print()


####    find peaks as a separate df    ####
am5.peak.filt.curv <- filter(curvatures, indvd == "americanus 5") %>%
  mutate(peaks = find_peaks(Curvature, ignore_threshold = 0.6, span = 3, strict = FALSE)) %>% # column indicating peaks
  filter(peaks == "TRUE") %>% # make peaks findable
  filter(!(Curvature < 0.0056)) %>%
  print()

am5.curvature.aligned <- filter(curvatures, indvd == "americanus 5") %>% 
  group_by(indvd) %>%
  mutate(peak.start.perim = perim_start - 
           am5.peak.filt.curv$perim_start[1]) %>% # slide the x-axis zero to be at the first peak (now has + and -)
  mutate(pos.peak.start = c(peak.start.perim[peak.start.perim >= 0],
                            (abs(peak.start.perim[peak.start.perim < 0])) + max(peak.start.perim))) %>% # remake x-axis to start from zero, and add negative values to the end as positive
  mutate(curv.shift = c(Curvature[perim_start >= 
                                    am5.peak.filt.curv$perim_start[1]],
                        rev(Curvature[perim_start < 
                                        am5.peak.filt.curv$perim_start[1]]))) %>% # remake curvature column so that it aligns with the new x-axis (formerly negative region needs to be reversed)
  print()

# test some plots to make sure
ggplot(data = am5.curvature.aligned, 
       #aes(x = perim_start, y = Curvature)
       #aes(x = peak.start.perim, y = Curvature)
       aes(x = pos.peak.start, y = curv.shift)
       ) +
  geom_point() +
  #xlab("perimeter position") #+
  geom_vline(xintercept = 1298)




