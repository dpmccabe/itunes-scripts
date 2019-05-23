if (!("packrat" %in% loadedNamespaces())) source("packrat/init.R")
rm(list = ls())
Sys.setlocale('LC_ALL','C')
# set.seed(1)

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)

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

if (length(hours) == 0) {
  hours <- 8

  debug <- T
  library(ggplot2)
  library(gridExtra)
  library(ggthemes)
} else {
  debug <- F
}

seconds <- 24 * 60 * 60
seconds_to_export <- hours * 60 * 60

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
  type_convert() %>%
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

tracks_simp <- bind_rows(tracks_grouped_comb, tracks_ungrouped) %>%
  filter((plays >= 5 & last_played > 5) | (plays < 5 & last_played > plays))

emp_props <- tracks_simp %>%
  group_by(genre_cat, rating) %>%
  summarize(prop = sum(duration) / sum(tracks_simp$duration)) %>%
  ungroup() %>%
  spread(rating, prop)

emp_props_mat <- emp_props %>%
  remove_rownames() %>%
  column_to_rownames("genre_cat") %>%
  as.matrix()

rating_mults_df <- tribble(
  ~rating, ~mult,
  3, 1,
  4, 3,
  5, 5
)

gc_mults_df <- tibble(
  genre_cat = rownames(emp_props_mat),
  mult = c(0.375, 0.25, 0.375) / rowSums(emp_props_mat)
) %>% mutate(mult = mult / min(mult))

mult_mat <- t(t(gc_mults_df$mult)) %*% t(rating_mults_df$mult)

ideal_props <- emp_props_mat * mult_mat
ideal_props <- ideal_props / sum(ideal_props)

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

make_q <- function(x, cuts) {
  uq <- quantile(x, cuts)
  q <- as.integer(cut(x, uq, include.lowest = T))

  q <- q - min(q)
  if (max(q) > 0) q <- q / max(q)

  q <- 1 - q
  q
}

exponent <- 3

compute_qrg <- function(d) {
  max_dur <- d$max_dur[1]
  nbins <- 1

  while (T) {
    even_cuts <- seq(0, 1, length.out = nbins + 1)
    decaying_cuts <- even_cuts^(exponent)

    d_chosen <- d %>%
      mutate(plays_q = make_q(plays + runif(n(), 0, 0.1), decaying_cuts)) %>%
      group_by(plays_q, add = T) %>%
      mutate(chosen = rank(-last_played, ties.method = "random") == 1)

    d_time <- sum(d_chosen[d_chosen$chosen, "duration"])

    if (d_time >= max_dur) {
      break()
    } else {
      nbins <- nbins + 1
    }
  }

  d_chosen
}

gtracks_within <- tracks_simp %>%
  left_join(ideal_seconds, by = c("genre_cat", "rating")) %>%
  split(group_indices(., genre_cat, rating)) %>%
  map_dfr(compute_qrg)

if (debug) {
  g <- gtracks_within %>% filter(genre_cat == "Celtic", rating == 3)
  ggplot(g, aes(last_played, -plays, color = chosen, alpha = chosen)) +
    geom_jitter(show.legend = F, size = 1) +
    scale_color_manual(values = c("#88dddd", "#550000")) +
    scale_alpha_manual(values = c(0.5, 1)) +
    ggthemes::theme_few()
}

# gtracks_within_rating <- tracks_simp %>%
#   left_join(ideal_seconds_rating, by = "rating") %>%
#   split(group_indices(., rating)) %>%
#   map_dfr(compute_qrg)
#
# gtracks_within_gc <- tracks_simp %>%
#   left_join(ideal_seconds_gc, by = "genre_cat") %>%
#   split(group_indices(., genre_cat)) %>%
#   map_dfr(compute_qrg)

chosen_tracks <- tracks_simp %>%
  mutate(chosen_within = gid %in% (gtracks_within %>% filter(chosen) %>% pull(gid)),
         # chosen_within_gc = gid %in% (gtracks_within_gc %>% filter(chosen) %>% pull(gid)),
         # chosen_within_rating = gid %in% (gtracks_within_rating %>% filter(chosen) %>% pull(gid)),
         chosen = chosen_within)

if (debug) {
  chosen_tracks %>% group_by(chosen_within, chosen_within_gc, chosen_within_rating) %>% count()
  chosen_tracks %>% filter(!chosen_within, chosen_within_gc, chosen_within_rating) %>% count(genre_cat, rating)
  chosen_tracks %>% filter(chosen_within, !chosen_within_gc, !chosen_within_rating) %>% count(genre_cat, rating)
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

    p <- ggplot(d, aes(last_played, -plays, color = chosen, alpha = chosen)) +
      geom_jitter(show.legend = F, size = 0.5) +
      scale_color_manual(values = c("#88dddd", "#550000")) +
      scale_alpha_manual(values = c(0.5, 1)) +
      ggtitle(paste(grid_space$rating[i], grid_space$genre_cat[i])) +
      # xlim(c(0, max_lp)) + ylim(c(-max_plays, 0)) +
      ggthemes::theme_few()
    return(p)
  }

  plots <- purrr::map(1:nrow(grid_space), make_plots)
  do.call(grid.arrange, c(plots, ncol = 3))
}

ptracks <- filter(chosen_tracks, chosen) %>%
  select(-matches("chosen")) %>%
  mutate(uuid = stringi::stri_rand_strings(n(), 10)) %>%
  arrange(uuid) %>%
  inner_join(
    tracks %>% select(id, genre),
    by = c("gid" = "id")
  ) %>%
  mutate(gc_rating = paste(genre_cat, rating, sep = "-"))

ptracks_gcr <- split(ptracks, ptracks$gc_rating)

interleave <- function(d, arrange_col) {
  d %>%
    arrange_(arrange_col) %>%
    mutate(j = 1:n() / n()) %>%
    mutate(
      b1 = lag(j, default = 0),
      b2 = j
    ) %>%
    mutate(j =  (b1 + b2) / 2) %>%
    select(-b1, -b2)
}

ptracks_gcr_j <- lapply(ptracks_gcr, interleave, arrange_col = "uuid")

ptracks_joined <- bind_rows(ptracks_gcr_j) %>%
  group_by(genre_cat) %>%
  arrange(genre_cat, j) %>%
  mutate(l = 1:n()) %>%
  ungroup()

ptracks_gc <- split(ptracks_joined, ptracks_joined$genre_cat)

ptracks_gc_j <- lapply(ptracks_gc, interleave, arrange_col = "l")

ptracks_joined <- bind_rows(ptracks_gc_j)

if (debug) {
  ggplot(ptracks_joined, aes(x = rank(j), y = rating, color = genre_cat)) +
    geom_point() +
    theme_few()
}

ptracks_joined <- arrange(ptracks_joined, j)
ptracks_export <- ptracks_joined[seq_len(1 + sum(!(cumsum(ptracks_joined$duration) >= seconds_to_export))),]

exploded_tracks <- bind_rows(tracks_ungrouped, tracks_grouped) %>%
  select(gid, id, no) %>%
  inner_join(ptracks_export, by = "gid") %>%
  mutate(id = ifelse(is.na(id), gid, id)) %>%
  arrange(j, no)

write(paste0(exploded_tracks$id, collapse = ","), file = "~/Music/phone_ids.txt")
