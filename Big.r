rm(list = ls())
Sys.setlocale('LC_ALL','C')

library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(uuid)

cn <- c("id", "artist", "size", "album", "rating", "no")

tracks <- read_csv("~/Music/big.csv", col_names = cn)

ungrouped_tracks <- filter(tracks, is.na(album)) %>% group_by(id) %>%
  mutate(uuid = UUIDgenerate()) %>% ungroup()
grouped_tracks <- filter(tracks, !is.na(album)) %>% group_by(album) %>%
  mutate(uuid = UUIDgenerate()) %>% ungroup()

com_tracks <- bind_rows(ungrouped_tracks, grouped_tracks)

zero_tracks <- com_tracks %>% group_by(uuid) %>% mutate(total_rating = sum(rating)) %>% ungroup() %>%
  filter(total_rating == 0)

artist_counts <- com_tracks %>% group_by(uuid, artist) %>% count() %>%
  ungroup() %>% select(-n) %>% count(artist)

zero_tracks <- zero_tracks %>% semi_join(artist_counts %>% filter(n >= 1), by = "artist")

sizes <- zero_tracks %>%
  group_by(uuid) %>% summarize(size = sum(size)) %>%
  arrange(-size)

bigs <- inner_join(zero_tracks, sizes[1:25,], by = "uuid") %>% arrange(-size.y)

write(paste0(c(bigs$id), collapse = ","), file = "~/Music/big_ids.txt")
