library(tidyverse)
library(data.table)

path <- "/datatwo/data/tatk/pubmed/"

# read directoy of directories
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
  mutate(dfs = map(csvs, read_tsv))

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

## unnest all dfs
pubmed_dfs <- pubmed_dfs %>% 
  unnest(dfs)

# extract and tabulate mesh terms
pubmed_dfs <- pubmed_dfs %>%
  filter(csvs != "/datatwo/data/tatk/pubmed//pubmed18n0654.tsv/part-00000-28084333-87be-4df6-8ee6-9666e84f2404-c000.csv") %>% 
  mutate(mesh_split = map(mesh_terms, str_split, pattern = "; ")) %>% 
  mutate(mesh_split = map(mesh_split, unlist),
         mesh_len = map_int(mesh_split, length))


pubmed_dfs %>%  
  group_by(mesh_len) %>% tally

# tweedie?
pubmed_dfs %>% ggplot(aes(x = mesh_len, y = ..count..)) + geom_bar()

# saveRDS(pubmed_dfs, file = "pubmed_df.rds")
