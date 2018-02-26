library(tidyverse)
library(data.table)
future::availableCores()

# set up data path and import tsvs into single df -------------------------


path <- "/datatwo/data/tatk/pubmed/"

# DONE: data ingest and unnest is embarassingly parallel,
# DONE: try use future / promises to run on multiple threads (furrr?)
# read directoy of directories
pubmed_sample <- data_frame(folders = dir(path, full.names = TRUE)) %>% 
  # extract all the files in the subdirectories as a list
  mutate(csvs = map(folders, dir, full.names = TRUE)) %>% 
  # expand each list of files in the subdirectory to a new row
  unnest(csvs) %>% 
  # retain only the csvs
  filter(str_detect(csvs, ".csv")) %>% 
  # this ends up with 890 csv files, so sample if you want to limit size and time
  sample_n(10) %>%
  # read all of these tab separated values masquerading around like commas
  mutate(dfs = map(csvs, read_tsv))


# parallelize -------------------------------------------------------------


# initial attempt at furrry analysis:
# set up ~~multisession~~ multiprocess plan for future and run map with future
# hmm, can we select a batch size or is this doing online?
library(future)
# plan(multisession)
# plan(multicore)
plan(multiprocess)
pubmed_dfs <- data_frame(folders = dir(path, full.names = TRUE)) %>% 
  # extract all the files in the subdirectories as a list
  mutate(csvs = map(folders, dir, full.names = TRUE)) %>% 
  # expand each list of files in the subdirectory to a new row
  unnest(csvs) %>% 
  # retain only the csvs
  filter(str_detect(csvs, ".csv")) %>% 
  # this ends up with 890 csv files, so sample if you want to limit size and time
  # sample_n(10) %>%
  # read all of these tab separated values masquerading around like commas
  mutate(dfs = map(csvs, ~future(read_tsv(.x))) %>% values)

# sanity checks before rbind ----------------------------------------------


# verify they have same number of columns
pubmed_dfs %>% 
  select(dfs) %>% 
  mutate(num_col = map_int(dfs, ncol)) %>% 
  group_by(num_col) %>% tally

## hmm, one element has zero columns, somethin ain't right
pubmed_dfs %>% 
  mutate(num_col = map_int(dfs, ncol)) %>% 
  filter(num_col < 4)
# /datatwo/data/tatk/pubmed//pubmed18n0654.tsv/part-00000-28084333-87be-4df6-8ee6-9666e84f2404-c000.csv

## remove bad kid and unnest all dfs
## leads to 16 million rows
pubmed_dfs <- pubmed_dfs %>% 
  filter(csvs != "/datatwo/data/tatk/pubmed//pubmed18n0654.tsv/part-00000-28084333-87be-4df6-8ee6-9666e84f2404-c000.csv") %>% 
  unnest(dfs)


# create multi-label for mesh terms ---------------------------------------

# extract and tabulate mesh terms, could also parallelize with future
pubmed_dfs <- pubmed_dfs %>%
  mutate(mesh_split = map(mesh_terms, ~future(str_split(string = .x, pattern = "; "))) %>% values) %>%
  mutate(mesh_split = map(mesh_split, unlist),
         mesh_len = map_int(mesh_split, ~future(length(.x))) %>% values)
         

# tweedie?
pubmed_dfs %>%  
  group_by(mesh_len) %>% tally %>% 
  ggplot(aes(x = mesh_len, y = n)) + geom_bar(stat = 'identity')

# pubmed_dfs %>% ggplot(aes(x = mesh_len, y = ..count..)) + geom_bar()


# save final df for later -------------------------------------------------


# saveRDS(pubmed_dfs, file = "pubmed_df.rds")
