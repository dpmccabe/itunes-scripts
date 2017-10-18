rm(list = ls())
Sys.setlocale('LC_ALL','C')
set.seed(1)

library(dplyr)
library(readr)
library(tidyr)
library(uuid)
library(stringr)
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
if (length(hours) == 0) hours <- 24
seconds <- hours * 60 * 60

cn <- c("id", "duration", "rating", "plays", "last_played", "genre", "grouping", "no")

starred <- read_lines("~/Music/starred.txt")

kvs <- starred %>% str_split("\\t")
n_kvs <- sapply(kvs, length)

now <- as.numeric(Sys.time())

tracks <- kvs %>% unlist() %>% str_split(":=", simplify = T) %>% as_data_frame() %>%
  rename(k = V1, v = V2) %>%
  mutate(ix = rep(1:length(kvs), times = n_kvs)) %>%
  spread(key = k, value = v) %>%
  select(id = `Track ID`, genre = Genre, duration = `Total Time`, rating = Rating, plays = `Play Count`,
         last_played = `Play Date UTC`, grouping = Grouping, no = `Track Number`) %>%
  mutate(last_played = (now - as.numeric(strptime(last_played, format = "%FT%TZ", tz = "GMT"))) / 60 / 60 / 24) %>%
  filter(last_played > 0) %>%
  type_convert %>%
  mutate(genre_cat = assign_genre_cat(genre),
         duration = duration / 1000,
         no = ifelse(is.na(grouping), NA, no),
         rating = as.integer(rating / 20)) %>%
  group_by(grouping) %>% mutate(ref = ifelse(is.na(no) | no == 1, T, F)) %>% ungroup()

N <- nrow(tracks)

gr_props <- tracks %>% filter(ref) %>%
  group_by(genre_cat, rating) %>% summarize(prop = n() / N) %>% ungroup()

gr_fixed_props <- gr_props %>% group_by(genre_cat) %>%
  mutate(marg_prop = prop / sum(prop)) %>% ungroup() %>% select(genre_cat, rating, marg_prop) %>%
  spread(key = rating, value = marg_prop) %>%
  mutate(sum45 = `4` + `5`, marg4 = `4` / sum45, marg5 = `5` / sum45) %>%
  mutate(sum45 = (sum45 * 2 + 0.5 * 4) / 6, `3` = 1 - sum45) %>%
  mutate(marg5 = (marg5 * 2 + 0.5 * 3) / 5, marg4 = 1 - marg5) %>%
  mutate(`4` = marg4 * sum45, `5` = marg5 * sum45) %>%
  select(-marg5, -marg4, -sum45) %>%
  gather(`3`:`5`, key = "rating", value = "final_prop", convert = T) %>%
  arrange(genre_cat, rating)

props <- gr_fixed_props %>% filter(final_prop > 0) %>%
  mutate(final_prop = ifelse(genre_cat == "Classical", final_prop * 0.25, final_prop * 0.375)) %>%
  mutate(min_prop = final_prop * 0.9, max_prop = final_prop / 0.9) %>%
  mutate(min_seconds = seconds * min_prop, max_seconds = seconds * max_prop)

gtracks <- tracks

gtracks_nonref <- filter(gtracks, !ref)
gtracks <- filter(gtracks, ref)

gtracks <- gtracks %>% group_by(rating, genre_cat) %>%
  mutate(plays_q = cut(plays, labels = F, include.lowest = T,
                       breaks = unique(quantile(plays, probs = seq(0, 1, 0.01))))) %>%
  mutate(plays_q = plays_q - min(plays_q), plays_q = 1 - plays_q / max(plays_q)) %>%
  mutate(lp_q = cut(last_played, labels = F, include.lowest = T,
                    breaks = unique(quantile(last_played, probs = seq(0, 1, 0.01))))) %>%
  mutate(lp_q = lp_q - min(lp_q), lp_q = lp_q / max(lp_q)) %>%
  mutate(sprob = plays_q^5 + lp_q^2) %>%
  mutate(sprob = sprob - min(sprob), sprob = sprob / max(sprob)) %>%
  mutate(sprob_q = cut(sprob, labels = F, include.lowest = T,
                       breaks = unique(quantile(sprob, probs = seq(0, 1, 0.1))))) %>%
  ungroup() %>%
  mutate(sprob = sprob + rnorm(nrow(.), 0, 0.05))

gtracks_nonref <- gtracks_nonref %>% left_join(gtracks %>% select(grouping, sprob), by = "grouping")

etracks <- bind_rows(gtracks, gtracks_nonref) %>% select(-plays_q, -lp_q) %>%
  mutate(chosen = F, available = T) %>% arrange(-sprob)

while(T) {
  current_props <- etracks %>% filter(chosen) %>% group_by(genre_cat, rating) %>%
    summarize(gc_seconds = sum(duration)) %>% ungroup()
  prop_status <- left_join(props, current_props, by = c("genre_cat", "rating")) %>%
    mutate(enough = gc_seconds >= min_seconds, done = gc_seconds >= max_seconds) %>%
    replace_na(list(gc_seconds = 0, enough = F, done = F))

  if (all(prop_status$enough)) break()

  etracks <- etracks %>%
    left_join(prop_status %>% select(genre_cat, rating, done), by = c("genre_cat", "rating")) %>%
    mutate(available = ifelse(is.na(done) | !done, T, F)) %>% select(-done) %>%
    arrange(-sprob)

  next_track <- etracks %>% filter(!chosen, available) %>% .[1,]

  if (is.na(next_track$grouping)) {
    etracks <- etracks %>% mutate(chosen = ifelse(id == next_track$id, T, chosen))
  } else {
    etracks <- etracks %>% mutate(chosen = ifelse(!is.na(grouping) & grouping == next_track$grouping, T, chosen))
  }
}

# make_plots <- function(i) {
#   p <- ggplot(etracks %>% filter(rating == prop_status$rating[i], genre_cat == prop_status$genre_cat[i]),
#               aes(last_played, plays, color = chosen, alpha = chosen)) + geom_point(show.legend = F) +
#     ggtitle(paste(prop_status$rating[i], prop_status$genre_cat[i]))
#   return(p)
# }
# plots <- purrr::map(1:nrow(prop_status), make_plots)
# do.call(grid.arrange, c(plots, ncol = 3))

ptracks <- filter(etracks, chosen)

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
