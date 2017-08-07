rm(list = ls())
Sys.setlocale('LC_ALL','C')

library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(uuid)

cn <- c("id", "artist", "size", "grouping", "no")

tracks <- read_csv("~/Music/big.csv", col_names = cn) %>%
  mutate(no = ifelse(is.na(grouping), NA, no))

ungrouped_tracks <- filter(tracks, is.na(grouping)) %>% group_by(id) %>%
  mutate(uuid = UUIDgenerate()) %>% ungroup()
grouped_tracks <- filter(tracks, !is.na(grouping)) %>% group_by(grouping) %>%
  mutate(uuid = UUIDgenerate()) %>% ungroup()

com_tracks <- bind_rows(ungrouped_tracks, grouped_tracks)

artist_counts <- com_tracks %>% group_by(uuid, artist) %>% count() %>%
  ungroup() %>% select(-n) %>% count(artist)

com_tracks <- com_tracks %>% semi_join(artist_counts %>% filter(n >= 2), by = "artist")

sizes <- com_tracks %>%
  group_by(uuid) %>% summarize(size = sum(size)) %>%
  arrange(-size)

bigs <- left_join(com_tracks, sizes[1:20,], by = "uuid") %>% arrange(-size.y)

write(paste0(c(bigs$id), collapse = ","), file = "~/Music/big_ids.txt")
