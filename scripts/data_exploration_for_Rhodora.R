# Species found in the past in Concord, but now thought extirpated.
# Rare plant species known to exist in one location only in Concord.
# Non-native species known from in one location in Concord. 
# For each group of species, I am checking iNaturalist to see if there are any 
# photos from Concord, how many photos, and how many distinct locations.  
# I am also checking the photos to see if they are correctly identified, and also
# checking to see if the photos are apparently taken in the wild or from a garden.  
# I have seen that a certain number of the photos are incorrectly identified, and 
# some of the photos are taken in public and private wildflower gardens; 
# these photos need to be noted and not considered as wild occurrences of the target species.


#------------------------------------------#
####           Call packages            ####
#------------------------------------------#

library(tidyverse)



#------------------------------------------#
####            Import data             ####
#------------------------------------------#

## Read in iNaturalist data from Acadia
inat <- read.csv("data/inaturalist_acad_obs_20250822.csv") %>% 
  filter(iconic_taxon_name == "Plantae",
         captive_cultivated == "false") %>% 
  as_tibble()

## Read in Caitlin's Acadia plant database
acad.plants <- readxl::read_excel("data/MDI Flora Abundance Comparison Draft.xlsx", sheet = "Database") %>%
  select_all(~gsub("\\s+|\\.", ".", .)) %>% 
  rename_with(tolower)




#-----------------------------------------------------------------------#
#### Species found in the past in Acadia, but now thought extirpated ####
#-----------------------------------------------------------------------#

## Create list of extirpated plants from MDI
ext <- acad.plants %>% 
  filter(change.in.mdi.abundance == "extirpated") %>% 
  select(species, change.in.mdi.abundance, native, invasive)

## Filter iNaturalist data to only those extirpated species
## This currently includes non-native species that are extirpated
inat.ext <- inat %>% 
  filter(scientific_name %in% ext$species)

## Create a list of distinct species
ext.list <- inat.ext %>% 
  select(scientific_name, common_name) %>% 
  distinct() %>% 
  arrange(scientific_name) %>% 
  print(n=length(.$scientific_name))

## Export list for creation of excel database of records referencing iNaturalist
write.csv(ext.list, "outputs/extirpated_list.csv", row.names = F)



#------------------------------------------------------------------------#
#### Rare plant species known to exist in one location only in Acadia ####
#------------------------------------------------------------------------#

## Create list of non-native plants from MDI
rare <- acad.plants %>% 
  filter(`2010.mdi.abundance` == "R") %>% 
  #filter(is.na(native)) %>% 
  select(species, change.in.mdi.abundance, native, invasive)

## Filter iNaturalist data to only those non-native species
inat.rare <- inat %>% 
  filter(scientific_name %in% rare$species)

## Create a list of distinct species
rare.list <- inat.rare %>% 
  select(scientific_name, common_name) %>% 
  distinct(scientific_name, .keep_all = T) %>% 
  arrange(scientific_name) %>% 
  print(n=length(.$scientific_name))

## Export list for creation of excel database of records referencing iNaturalist
write.csv(rare.list, "outputs/rare_list.csv", row.names = F)



#---------------------------------------------------------------#
#### Non-native species known from in one location in Acadia ####
#---------------------------------------------------------------#

## Create list of non-native plants from MDI
nn <- acad.plants %>% 
  filter(native == "no") %>% 
  select(species, change.in.mdi.abundance, native, invasive)

## Filter iNaturalist data to only those non-native species
inat.nn <- inat %>% 
  filter(scientific_name %in% nn$species)

## Create a list of distinct species
nn.list <- inat.nn %>% 
  select(scientific_name, common_name) %>% 
  distinct(scientific_name, .keep_all = T) %>% 
  arrange(scientific_name) %>% 
  print(n=length(.$scientific_name))

## Export list for creation of excel database of records referencing iNaturalist
write.csv(nn.list, "outputs/nonnative_list.csv", row.names = F)





