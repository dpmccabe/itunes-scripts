rm(list = ls())
Sys.setlocale('LC_ALL','C')
set.seed(1)

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tibble)

debug <- F
# debug <- T
if (debug) {
  library(ggplot2)
  library(gridExtra)
}

assign_genre_cat <- Vectorize(function(genre) {
  if (grepl("Celtic|World|French|Galician", genre)) {
    return("Celtic")
  } else if (grepl("Classical", genre)) {
    return("Classical")
  } else {
    return("Other")
  }
}, USE.NAMES = F)

make_q <- function(d, comp = F) {
  quants <- quantile(d, probs = seq(0, 1, length.out = 21))
  q <- cut(d, labels = F, include.lowest = T, breaks = unique(quants))
  q <- q / max(q)
  if (comp) q <- 1 - (q - min(q))
  return(q)
}

hours <- as.integer(commandArgs(trailingOnly = T))
if (length(hours) == 0) hours <- 12
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
  filter(last_played > 1) %>%
  type_convert %>%
  mutate(genre_cat = assign_genre_cat(genre),
         duration = duration / 1000,
         no = ifelse(is.na(grouping), NA, no),
         rating = as.integer(rating / 20))

tracks_ungrouped <- tracks %>%
  filter(is.na(grouping)) %>%
  mutate(gid = id) %>%
  select(gid, genre_cat, rating, duration, plays, last_played)

tracks_grouped <- tracks %>%
  filter(!is.na(grouping)) %>%
  group_by(grouping) %>%
  arrange(grouping, no) %>%
  mutate(gid = id[1]) %>%
  ungroup()

tracks_grouped_comb <- tracks_grouped %>%
  group_by(gid) %>%
  summarize(genre_cat = genre_cat[1], rating = rating[1], duration = sum(duration), plays = plays[1], last_played = last_played[1])

tracks_simp <- bind_rows(tracks_grouped_comb, tracks_ungrouped)

emp_props <- tracks_simp %>%
  group_by(genre_cat, rating) %>%
  summarize(prop = sum(duration) / sum(tracks_simp$duration)) %>%
  ungroup() %>%
  spread(rating, prop)

emp_props_mat <- emp_props %>%
  remove_rownames() %>%
  column_to_rownames("genre_cat") %>%
  as.matrix()

rating_weights_df <- tribble(
  ~rating, ~rating_w,
  3, 1,
  4, 3,
  5, 5
)

ideal_props1 <- sweep(emp_props_mat, MARGIN = 1, FUN = `/`, rowSums(emp_props_mat))
ideal_props2 <- sweep(ideal_props1, MARGIN = 2, FUN = `*`, rating_weights_df$rating_w)
ideal_props3 <- sweep(ideal_props2, MARGIN = 1, FUN = `/`, rowSums(ideal_props2))
ideal_props4 <- ideal_props3 / sum(ideal_props3)
ideal_props5 <- sweep(ideal_props4, MARGIN = 1, FUN = `*`, c(0.375, 0.25, 0.375))
ideal_props <- ideal_props5 / sum(ideal_props5)

gc_weights <- c(0.375, 0.25, 0.375) / rowSums(emp_props_mat)
gc_weights_df <- tibble(genre_cat = names(gc_weights), gc_w = gc_weights)

ideal_seconds <- as.data.frame(ideal_props * seconds) %>%
  rownames_to_column(var = "genre_cat") %>%
  gather(`3`:`5`, key = "rating", value = "max_dur") %>%
  as_tibble() %>%
  type_convert()

ideal_seconds_gc <- ideal_seconds %>%
  group_by(genre_cat) %>%
  summarize(max_dur = sum(max_dur))

ideal_seconds_rating <- ideal_seconds %>%
  group_by(rating) %>%
  summarize(max_dur = sum(max_dur))

dim_weights <- c("plays" = 1/2, "lp" = 1/4)
dim_weights <- dim_weights / sum(dim_weights)

gtracks_within <- tracks_simp %>%
  group_by(genre_cat, rating) %>%
  mutate(plays_q = make_q(plays, comp = T)) %>%
  mutate(lp_q = make_q(last_played)) %>%
  mutate(dfc = 1 - plays_q^dim_weights["plays"] * lp_q^dim_weights["lp"]) %>%
  arrange(genre_cat, rating, dfc) %>%
  mutate(cs = cumsum(duration)) %>%
  mutate(cs = lag(cs, default = 0)) %>%
  left_join(ideal_seconds, by = c("genre_cat", "rating")) %>%
  mutate(chosen = cs <= max_dur) %>%
  ungroup()

gtracks_within_gc <- tracks_simp %>%
  group_by(genre_cat) %>%
  mutate(plays_q = make_q(plays, comp = T)) %>%
  mutate(lp_q = make_q(last_played)) %>%
  mutate(dfc = 1 - plays_q^dim_weights["plays"] * lp_q^dim_weights["lp"]) %>%
  left_join(rating_weights_df, by = "rating") %>%
  mutate(dfc_w = dfc / rating_w) %>%
  arrange(genre_cat, dfc_w) %>%
  mutate(cs = cumsum(duration)) %>%
  mutate(cs = lag(cs, default = 0)) %>%
  left_join(ideal_seconds_gc, by = "genre_cat") %>%
  mutate(chosen = cs <= max_dur) %>%
  ungroup()

gtracks_within_rating <- tracks_simp %>%
  group_by(rating) %>%
  mutate(plays_q = make_q(plays, comp = T)) %>%
  mutate(lp_q = make_q(last_played)) %>%
  mutate(dfc = 1 - plays_q^dim_weights["plays"] * lp_q^dim_weights["lp"]) %>%
  left_join(gc_weights_df, by = "genre_cat") %>%
  mutate(dfc_w = dfc / gc_w) %>%
  arrange(rating, dfc_w) %>%
  mutate(cs = cumsum(duration)) %>%
  mutate(cs = lag(cs, default = 0)) %>%
  left_join(ideal_seconds_rating, by = "rating") %>%
  mutate(chosen = cs <= max_dur) %>%
  ungroup()

chosen_tracks <- tracks_simp %>%
  mutate(chosen_within = gid %in% (gtracks_within %>% filter(chosen) %>% pull(gid)),
         chosen_within_gc = gid %in% (gtracks_within_gc %>% filter(chosen) %>% pull(gid)),
         chosen_within_rating = gid %in% (gtracks_within_rating %>% filter(chosen) %>% pull(gid)),
         chosen = chosen_within | (chosen_within_rating & chosen_within_gc))

if (debug) {
  chosen_tracks %>% group_by(chosen_within, chosen_within_gc, chosen_within_rating) %>% count()
  chosen_tracks %>% filter(genre_cat=="Classical",rating==5)
  chosen_tracks %>% filter(!chosen_within, chosen_within_gc, chosen_within_rating)
  chosen_tracks %>% filter(chosen_within, !chosen_within_gc, !chosen_within_rating)
  sum((chosen_tracks %>% filter(chosen))$duration) / seconds

  chosen_prop_mat <- chosen_tracks %>%
    mutate(dur_for_sum = duration * chosen) %>%
    group_by(genre_cat, rating) %>%
    summarize(dur = sum(dur_for_sum)) %>%
    ungroup() %>%
    mutate(dur = dur / sum(dur)) %>%
    spread(rating, dur) %>%
    remove_rownames() %>%
    column_to_rownames("genre_cat") %>%
    as.matrix()

  print(chosen_prop_mat)
  print(chosen_prop_mat / ideal_props)
  print(chosen_prop_mat - ideal_props)

  grid_space <- chosen_tracks %>% distinct(genre_cat, rating) %>% arrange(genre_cat, rating)

  max_lp <- max(chosen_tracks %>% pull(last_played))
  max_plays <- max(chosen_tracks %>% pull(plays))

  make_plots <- function(i) {
    d <- chosen_tracks %>%
      filter(rating == grid_space$rating[i], genre_cat == grid_space$genre_cat[i]) %>%
      arrange(desc(chosen))

    p <- ggplot(d, aes(last_played, plays, color = chosen, alpha = chosen)) +
      geom_jitter(show.legend = F, size = 0.5) +
      scale_color_manual(values = c("#88dddd", "#550000")) +
      scale_alpha_manual(values = c(0.5, 1)) +
      ggtitle(paste(grid_space$rating[i], grid_space$genre_cat[i])) +
      xlim(c(0, max_lp)) + ylim(c(0, max_plays)) +
      ggthemes::theme_few()
    return(p)
  }

  plots <- purrr::map(1:nrow(grid_space), make_plots)
  do.call(grid.arrange, c(plots, ncol = 3))
}

ptracks <- filter(chosen_tracks, chosen) %>%
  select(-matches("chosen")) %>%
  group_by(gid) %>%
  mutate(uuid = stringi::stri_rand_strings(n(), 10)) %>%
  ungroup()

exploded_tracks <- bind_rows(tracks_ungrouped, tracks_grouped) %>%
  select(gid, id, no) %>%
  inner_join(ptracks, by = "gid") %>%
  mutate(id = ifelse(is.na(id), gid, id))

oc_ptracks <- exploded_tracks %>%
  filter(genre_cat != "Classical") %>%
  mutate(i = as.double(factor(rank(uuid, ties.method = "min")))) %>%
  select(-uuid)
classical_ptracks <- exploded_tracks %>%
  filter(genre_cat == "Classical") %>%
  mutate(i = as.integer(factor(rank(uuid, ties.method = "min"))) - 1) %>%
  select(-uuid) %>%
  mutate(i = round(5 + i * (max(oc_ptracks$i) - 5) / max(i)))

fudges <- classical_ptracks %>%
  distinct(gid) %>%
  mutate(fudge = sample(-2:2, size = n(), replace = T) + 0.1)
classical_ptracks_fudged <- classical_ptracks %>%
  left_join(fudges, by = "gid") %>%
  mutate(i = i + fudge) %>%
  select(-fudge)

ftracks <- bind_rows(classical_ptracks_fudged, oc_ptracks) %>%
  arrange(i, no)

write(paste0(ftracks$id, collapse = ","), file = "~/Music/phone_ids.txt")
