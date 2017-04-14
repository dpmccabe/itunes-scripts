rm(list = ls())
Sys.setlocale('LC_ALL','C')
set.seed(1)

library(plyr)
library(dplyr)
library(readr)
library(tidyr)
library(uuid)
# library(ggplot2)
# library(gridExtra)

assign_genre_cat <- Vectorize(function(genre) {
  if (grepl("Celtic|World|French|Galician", genre)) {
    return("Celtic")
  } else if (grepl("Classical", genre)) {
    return("Classical")
  } else {
    return("Other")
  }
}, USE.NAMES = F)

hours <- as.integer(commandArgs(trailingOnly = T))
seconds <- hours * 60 * 60

cn <- c("id", "duration", "rating", "plays", "last_played", "genre", "grouping", "no")

tracks <- read_csv("~/Music/starred.csv", col_names = cn) %>% filter(last_played > 2) %>%
  mutate(genre_cat = assign_genre_cat(genre), no = ifelse(is.na(grouping), NA, no)) %>%
  group_by(grouping) %>% mutate(ref = ifelse(is.na(no) | no == 1, T, F)) %>% ungroup()

N <- nrow(tracks)

gr_props <- tracks %>% group_by(genre_cat, rating) %>% summarize(prop = n() / N) %>% ungroup()

gr_marg_props <- gr_props %>% group_by(genre_cat) %>%
  mutate(marg_prop = prop / sum(prop)) %>% ungroup() %>% select(genre_cat, rating, marg_prop) %>%
  spread(key = rating, value = marg_prop) %>%
  mutate(`5` = ifelse(is.na(`5`), 0, `5`)) %>%
  mutate(sum45 = `4` + `5`, marg4 = `4` / sum45, marg5 = `5` / sum45) %>%
  mutate(sum45 = (`4` + `5`) * 1.5) %>%
  mutate(marg5 = marg5 * 1.5, marg4 = 1 - marg5) %>%
  mutate(`4` = marg4 * sum45, `5` = marg5 * sum45) %>%
  select(-marg5, -marg4) %>%
  mutate(`3` = 1 - sum45) %>% select(-sum45) %>%
  gather(`3`:`5`, key = "rating", value = "final_prop", convert = T) %>%
  arrange(genre_cat, rating)

props <- gr_marg_props %>% filter(final_prop > 0) %>%
  mutate(final_prop = ifelse(genre_cat == "Classical", final_prop * 0.25, final_prop * 0.375)) %>%
  mutate(min_prop = final_prop * 0.85, max_prop = final_prop * 1.15) %>%
  mutate(min_seconds = seconds * min_prop, max_seconds = seconds * max_prop)

gtracks <- tracks

gtracks_nonref <- filter(gtracks, !ref)
gtracks <- filter(gtracks, ref)

gtracks <- gtracks %>% group_by(rating) %>%
  mutate(plays_q = cut(plays, labels = F, include.lowest = T,
                       breaks = unique(quantile(plays, probs = seq(0, 1, 0.01))))) %>%
  mutate(plays_q = plays_q - min(plays_q), plays_q = 1 - plays_q / max(plays_q)) %>%
  mutate(lp_q = cut(last_played, labels = F, include.lowest = T,
                    breaks = unique(quantile(last_played, probs = seq(0, 1, 0.01))))) %>%
  mutate(lp_q = lp_q - min(lp_q), lp_q = lp_q / max(lp_q)) %>%
  mutate(sprob = plays_q^(1 / 2) * lp_q^(1 / 2.2)) %>%
  ungroup() %>%
  mutate(sprob = sprob + rnorm(nrow(gtracks), 0, 0.1)) %>%
  mutate(sprob = sprob - min(sprob), sprob = sprob / max(sprob))

gtracks_nonref <- gtracks_nonref %>% left_join(gtracks %>% select(grouping, sprob), by = "grouping")

etracks <- bind_rows(gtracks, gtracks_nonref) %>% select(-plays_q, -lp_q)
ptracks <- data_frame()

while(T) {
  etracks <- arrange(etracks, -sprob)
  next_track <- etracks[1,]

  if (is.na(next_track$grouping)) {
    next_tracks <- next_track
    ptracks <- bind_rows(ptracks, next_tracks)
    etracks <- etracks[-1,]
  } else {
    next_tracks <- semi_join(etracks, next_track, by = "grouping")
    ptracks <- bind_rows(ptracks, next_tracks)
    etracks <- anti_join(etracks, next_track, by = "grouping")
  }

  current_props <- ptracks %>% group_by(genre_cat, rating) %>%
    summarize(gc_seconds = sum(duration)) %>% ungroup()
  prop_status <- left_join(props, current_props, by = c("genre_cat", "rating")) %>%
    mutate(enough = gc_seconds >= min_seconds, done = gc_seconds >= max_seconds)

  done_gc <- filter(prop_status, done)
  etracks <- anti_join(etracks, done_gc, by = c("genre_cat", "rating"))

  if (all(!is.na(prop_status) & prop_status$enough == T)) break()
}

ungrouped_ptracks <- filter(ptracks, is.na(grouping)) %>% group_by(id) %>%
  mutate(uuid = UUIDgenerate()) %>% ungroup()
grouped_ptracks <- filter(ptracks, !is.na(grouping)) %>% group_by(grouping) %>%
  mutate(uuid = UUIDgenerate()) %>% ungroup()

com_ptracks <- bind_rows(ungrouped_ptracks, grouped_ptracks)

oc_ptracks <- filter(com_ptracks, genre_cat != "Classical") %>%
  mutate(i = as.integer(factor(rank(uuid, ties.method = "min")))) %>% select(-uuid)
classical_ptracks <- filter(com_ptracks, genre_cat == "Classical") %>%
  mutate(i = as.integer(factor(rank(uuid, ties.method = "min"))) - 1) %>% select(-uuid) %>%
  mutate(i = round(5 + i * (max(oc_ptracks$i) - 10) / max(i)))

fudges <- select(classical_ptracks, i) %>% distinct() %>%
  mutate(fudge = sample(-2:2, size = n(), replace = T) + 0.1)
classical_ptracks <- left_join(classical_ptracks, fudges, by = "i") %>%
  mutate(i = i + fudge) %>% select(-fudge)

ftracks <- bind_rows(classical_ptracks, oc_ptracks) %>% arrange(i, no)

write(paste0(c(ftracks$id), collapse = ","), file = "~/Music/phone_ids.txt")
