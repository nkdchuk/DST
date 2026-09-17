# =============================================================================
#  Most popular song of all time?  --  Billboard charts, 1976-2026
#  Data: billboardcharts.csv  (weekly TOP 50: week, rank, song, artist)
#  Note: this is Billboard only. No Spotify, no YouTube. "Popular" = "charted well".
# =============================================================================

# --- 0. Setup ----------------------------------------------------------------
# tidyverse already bundles dplyr and ggplot2; the next two lines are just to
# make explicit what we lean on most.
library("tidyverse")
library("dplyr")       # filter, group_by, summarise, left_join, ...
library("ggplot2")     # plots

setwd(" ") #set your own working directory

# source: https://musicchartsarchive.com/

# --- 1. Load + look ----------------------------------------------------------

charts <- read.csv("billboardcharts_full_1976-2026.csv", stringsAsFactors = FALSE)

# sanity checks
glimpse(charts)         # what columns, what types?
nrow(charts)            # should be: 129250
max(charts$week)  # "2026-05-302
str(charts$rank)  # int [...]
summary(charts$rank)    # rank really runs 1..50? anything weird?

# --- 2. Build the keys we'll group on ----------------------------------------
# The pipe %>% feeds its left side into the next function as the first argument:
# x %>% f() is f(x), and x %>% f() %>% g() is g(f(x)).

# Two songs can share a title, so title alone isn't a safe ID. Glue on the artist
# to make a unique key. Run this BEFORE section 3 -- the scoring groups by
# song_artist, so the column must exist first.
charts <- charts %>% mutate(song_artist = paste0(song, " by ", artist))

# "week" is text -> make it a real Date so sorting and min/max behave.
charts <- charts %>% mutate(date = as.Date(week))


# --- 3. Score each song ------------------------------------------------------
# Idea: league points. Weeks in the TOP 5 earn points, summed across those weeks.
# Rewards peaking high AND lasting long near the top. The weights are a CHOICE, not a fact.

# A tiny lookup table: rank 1 is worth 10 points, rank 2 worth 6, ... rank 5 worth 0.
points <- data.frame(rank = c(1, 2, 3, 4, 5),
                     points = c(10, 6, 3, 1, 0))

top5 <- charts %>%
  filter(rank <= 5) %>%            # throw away rows ranked 6+ (our scheme ignores them)
  left_join(points, by = "rank")   # attach the point value for each row's rank

song_ranking <- top5 %>%
  group_by(song_artist) %>%        # one group per song (so each song's rows collapse together)
  summarise(weeks   = n(),         # how many weeks it spent in the top 5
            points  = sum(points), # its total score = our popularity metric
            peak    = min(rank),   # best position it ever reached (min, since rank 1 is best)
            .groups = "drop") %>%  # forget the grouping afterwards (tidy housekeeping)
  arrange(desc(points))            # sort so the highest score is first

head(song_ranking, 15)   # the leaderboard

# Same leaderboard as a picture: the top 10 songs by total points.
# reorder() sorts the bars; coord_flip() lays them horizontally so titles fit.
song_ranking %>%
  head(10) %>%
  ggplot(aes(x = reorder(song_artist, points), y = points)) +
  geom_col(fill = "#17becf") +
  geom_text(aes(label = points), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(title = "Most popular songs (top-5 points model)",
       x = NULL, y = "total points") +
  theme_minimal()


# --- 4. Plot one song's life -------------------------------------------------
# Pick a song, lay it on the full timeline so weeks-off-chart show as gaps.
# y = 51 - rank so that higher on the page = higher on the chart.
all_weeks <- data.frame(date = seq(as.Date("1976-11-20"),
                                   as.Date("2026-05-30"), by = "7 days"))

# Pick ONE song here. Uncomment a different line to compare shapes live:
# chosen <- "All I Want For Christmas Is You [seasonal] by Mariah Carey"
chosen <- "As It Was by Harry Styles"               # one tall, narrow spike
# chosen <- "Blinding Lights by The Weeknd"         # huge, very long-lasting hit
# chosen <- "The Sign by Ace Of Base"               # classic 90s slow rise and fall
# chosen <- "Uptown Funk! by Mark Ronson ft. Bruno Mars"  # long plateau at the top

one_song <- charts %>% filter(song_artist == chosen)

journey <- all_weeks %>%
  left_join(one_song, by = "date") %>%
  filter(date >= min(one_song$date), date <= max(one_song$date))

ggplot(journey, aes(x = date, y = rank)) +
  geom_step() + geom_point() +
  scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50), limits = c(50, 1)) +
  labs(title = chosen, x = NULL, y = "Chart position") +
  theme_minimal()

# --- 5. Turn the knobs -------------------------------------------------------
# The points scheme above is one reasonable way to weight the ranks. Other
# reasonable choices exist too. A good habit is to check whether the result holds
# up if we weight things differently. We wrap the pipeline in a function so we can
# re-run it with different settings in one line.

rank_songs <- function(data, depth, weights) {
  pts <- data.frame(rank = 1:depth, points = weights)
  data %>%
    filter(rank <= depth) %>%
    left_join(pts, by = "rank") %>%
    group_by(song_artist) %>%
    summarise(points = sum(points), .groups = "drop") %>%
    arrange(desc(points))
}

rank_songs(charts, depth = 5,  weights = c(10, 6, 3, 1, 0)) %>% head(5)  # our original scheme
rank_songs(charts, depth = 10, weights = 10:1)              %>% head(15)  # top 10, flatter weights: rewards longevity more
rank_songs(charts, depth = 1,  weights = c(1))             %>% head(15)  # only #1 counts -- ranks by weeks spent at number one
# If the top song changes when the rules change, the rules were the answer.


# --- 6. Artist dominance -----------------------------------------------------
# Different question: in one week, how many songs in the top 50 did a single artist hold at once?
# Count songs per (week, artist)...
weekly_artist_counts <- charts %>%
  group_by(date, artist) %>%
  summarise(songs = n(), .groups = "drop")

# ...then for each week keep only the most-represented artist.
weekly_top <- weekly_artist_counts %>%
  group_by(date) %>%
  slice_max(songs, n = 1, with_ties = FALSE) %>%
  ungroup()

# A console table naming the biggest weeks: one row per artist, their best week.
peak_table <- weekly_artist_counts %>%   # not weekly_top
  group_by(artist) %>%
  slice_max(songs, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(desc(songs)) %>%
  select(artist, date, songs)

head(peak_table, 10)   # the record-holders, by name, in the console

# The summary chart
ggplot(weekly_top, aes(x = date, y = songs)) +
  geom_col(fill = "#17becf") +
  labs(title = "Artist dominance: weekly top-50 entries (1976-2026)",
       x = NULL, y = "Songs by dominant artist") +
  theme_minimal()


# --- 6b. Trajectories for any artist -----------------------------------------
# Shows an artist's biggest songs over a year range, each as its own line
# (1 = top of the chart). We colour only the top few songs (most chart points)
# and grey out the rest, otherwise a prolific artist becomes an unreadable soup.
# The grey lines are the artist's other charting songs, shown as faint context.
#
# One subtlety: if a song charts, drops off for months, then returns, we do NOT
# want a straight line bridging that gap. So we insert a blank (NA) row whenever
# there's a break, which "lifts the pen" instead of drawing across the void.

plot_artist <- function(data, who, year_from, year_to, top_n = 6) {
  d <- data %>%
    filter(artist == who,
           date >= as.Date(paste0(year_from, "-01-01")),
           date <= as.Date(paste0(year_to,   "-12-31")))
  
  # which songs get a colour? the top_n by points (same scoring as section 3).
  big <- d %>%
    left_join(points, by = "rank") %>%
    mutate(points = ifelse(is.na(points), 0, points)) %>%
    group_by(song) %>% summarise(p = sum(points), .groups = "drop") %>%
    slice_max(p, n = top_n) %>% pull(song)
  
  # break a line wherever a song is absent for more than ~3 weeks: sort by date,
  # then set rank to NA on the first row after any gap so geom_line lifts the pen.
  d <- d %>%
    arrange(song, date) %>%
    group_by(song) %>%
    mutate(gap = as.numeric(date - lag(date)) > 21,
           rank = ifelse(!is.na(gap) & gap, NA, rank)) %>%
    ungroup()
  
  coloured <- d %>% filter(song %in% big)
  greyed   <- d %>% filter(!song %in% big)   # the artist's other songs, as context
  
  ggplot(mapping = aes(x = date, y = rank, group = song)) +
    geom_line(data = greyed, colour = "grey85", linewidth = 0.4) +
    geom_line(data = coloured, aes(colour = song), linewidth = 1) +
    geom_point(data = coloured, aes(colour = song), size = 1.2) +
    scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50), limits = c(50, 1)) +
    labs(title = paste0(who, " - top ", top_n, " songs (", year_from, "-", year_to, ")"),
         x = NULL, y = "Chart position (1 = best)", colour = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 7)) +
    guides(colour = guide_legend(nrow = 2))       # small, tucked under the chart
}

# Just change the arguments to show anyone:

# plot_artist(charts, "Taylor Swift", 2019, 2022)
# plot_artist(charts, "Drake",        2015, 2020)
# plot_artist(charts, "The Weeknd",   2019, 2023)
plot_artist(charts, "Madonna",        1984, 1990)


# --- 7. Discussion: coding in the age of LLMs --------------------------------
# Open question, no code. An LLM will write most of this faster than any of us.
# So what's actually worth being good at?
#
# My personal take: the syntax is the cheap part now. The value moves up a level, to
# prioritisation and to defining the problem. What do we ACTUALLY need to know?
# If a CEO is looking at this chart, what's the one thing that matters to them,
# and what's just noise? Picking the right question, and knowing when an answer
# is good enough, is the part the model can't do for you.
#
# But that's just my view. What do you think is worth learning? I'll fold your
# answers into how we set up the next project.

