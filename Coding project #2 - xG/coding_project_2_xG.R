# =====================================================================
# Data Science Toolbox - Coding Project #2
# Expected Goals (xG): logistic regression on real shot data
# =====================================================================
# Idea: every shot is a 0/1 outcome (goal or not). The model predicts the
# PROBABILITY of a goal from where the shot was taken. That probability is
# "expected goals" (xG) - the same logistic regression from the course,
# applied to ~3,700 real shots across 400 games from StatsBomb open data.
# ---------------------------------------------------------------------

# install.packages("tidyverse")
library(dplyr)
library(ggplot2)

# Set the working directory to where xgdata.csv sits (adjust the path!)
# Session -> Set Working Directory -> To Source File Location
setwd("C:/Users/Daniel/Desktop/coding_project_2_xG")


# Real world anecdote: https://theanalyst.com/articles/germany-vs-paraguay-stats-world-cup-2026


# =====================================================================
# 0) GET THE DATA
# =====================================================================
# xgdata.csv is provided ready to use - just read it in.
shots <- read.csv("xgdata.csv")

# Each row is one shot. Columns:
#   xpos, ypos          -> shot location on a 120 x 80 pitch (goal at x=120)
#   shot_outcome_name   -> "Goal", "Saved", "Blocked", ...
#   shot_statsbomb_xg   -> StatsBomb's own xG (their full model) - for comparison
#   match_file          -> which match the shot came from
head(shots)
nrow(shots)

# Outcome variable: 1 if goal, else 0
shots <- shots %>% mutate(isGoal = ifelse(shot_outcome_name == "Goal", 1, 0))
mean(shots$isGoal)   # overall conversion rate (~11%)

# ---------------------------------------------------------------------
# How the data was pulled:
# StatsBomb publishes one JSON of events per match. Loop over the matches,
# keep type == "Shot" & play_pattern == "Regular Play" (open play only, so
# penalties and set pieces don't distort it), and save location + outcome +
# their xg. The full open dataset is ~1000+ matches.
#
# Source: https://github.com/statsbomb/open-data
# ---------------------------------------------------------------------

# library(httr2); library(jsonlite); library(purrr)
# api_url <- "https://api.github.com/repos/statsbomb/open-data/contents/data/events"
# files <- request(api_url) |> req_perform() |> resp_body_json()
# xgdataset <- tibble()
# for (f in files[1:1000]) {
#   events <- fromJSON(f$download_url, flatten = TRUE) |> as_tibble()
#   s <- events |>
#     filter(type.name == "Shot", play_pattern.name == "Regular Play") |>
#     select(location, shot.outcome.name, shot.statsbomb_xg)
#   xgdataset <- bind_rows(xgdataset, s)
# }
# ---------------------------------------------------------------------


# =====================================================================
# 1) LOOK AT THE RAW SHOTS
# =====================================================================
# Plot every shot, colouring goals vs non-goals. Before any model: where do
# goals come from?
ggplot() +
  # --- pitch markings (soft grey, so the data dominates) ---
  annotate("rect", xmin = 18, xmax = 62, ymin = 102, ymax = 120,
           fill = NA, color = "grey55", linewidth = 0.5) +   # penalty area
  annotate("rect", xmin = 30, xmax = 50, ymin = 114, ymax = 120,
           fill = NA, color = "grey55", linewidth = 0.5) +   # 6-yard box
  annotate("rect", xmin = 36, xmax = 44, ymin = 120, ymax = 121,
           fill = NA, color = "grey55", linewidth = 0.5) +   # goal
  annotate("point", x = 40, y = 108, color = "grey40", size = 1) + # penalty spot

  # --- two layers: non-goals underneath, goals on top ---
  geom_point(data = filter(shots, isGoal == 0),
             aes(x = ypos, y = xpos, color = "No goal"),
             alpha = 0.15, size = 1.3) +
  geom_point(data = filter(shots, isGoal == 1),
             aes(x = ypos, y = xpos, color = "Goal"),
             alpha = 0.9, size = 1.8) +

  scale_color_manual(values = c("No goal" = "#d1495b", "Goal" = "#2e86ab"),
                     name = "", breaks = c("No goal", "Goal")) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +

  coord_fixed(ylim = c(80, 122), xlim = c(10, 70)) +   # real pitch proportions
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(face = "bold"))


# =====================================================================
# 2) BUILD THE FEATURES: distance and angle
# =====================================================================
# The goal centre is at (x=120, y=40). Two intuitive predictors:
#   distance -> how far the shot is from the goal centre
#   angle    -> the (small) angle to the goal LINE. 90 deg = straight on,
#               small angle = shooting from near the byline.
#
# NOTE: this is the angle to the goal LINE, not the player's opening angle
# onto the goal mouth. This keeps distance and angle as independent as
# possible - the opening angle would already contain distance inside it
# (a shot far out sees a narrow goal mouth purely because it is far).

shots <- shots %>%                                    # take the shots data, pipe it forward
  mutate(                                             # add new columns
    distance  = sqrt((ypos - 40)^2 + (xpos - 120)^2), # straight-line distance from shot to goal centre (120,40)
    angle     = atan2(120 - xpos, abs(40 - ypos)) * 180 / pi, # angle to goal line in degrees (90 = straight on)
    distance2 = distance^2,                           # squared terms, available if you want to
    angle2    = angle^2                               #   experiment with curved effects (not used below)
  )                                                     # overwrite shots with the result

# Sanity check: the penalty spot is (40, 108) -> distance 12, angle 90
# (perfectly central, 12 yards out). Confirm the formulas behave:
subset(transform(data.frame(xpos = 108, ypos = 40),
                 distance = sqrt((ypos-40)^2 + (xpos-120)^2),
                 angle    = atan2(120-xpos, abs(40-ypos))*180/pi))


# =====================================================================
# 3) FIT THE LOGISTIC MODEL
# =====================================================================
# glm with family = binomial -> logistic regression. Build it up.
reg1 <- glm(isGoal ~ distance, data = shots, family = binomial)
reg2 <- glm(isGoal ~ distance + angle, data = shots, family = binomial)

summary(reg1)   # distance alone: strong negative effect, far = worse
summary(reg2)   # add angle: positive effect (small but significant)

# --- how to read summary(reg1): ---
#   (Intercept)  -> log-odds of a goal when distance = 0 (on the goal line).
#                   plogis(0.031) ~ 0.51. It anchors the curve; distance = 0
#                   is outside the data, so don't over-read it.
#   distance     -> the SIGN is the story: NEGATIVE = farther means less likely.
#                   On the odds scale: exp(-0.125) ~ 0.88, i.e. ~12% lower odds
#                   of scoring per extra yard.
#   Std. Error   -> uncertainty on the estimate (smaller = more precise).
#   z value      -> Estimate / Std. Error: how many SEs from zero. |z| > ~2
#                   is the usual "real effect" threshold; here z = -14.9, huge.
#   Pr(>|z|)     -> p-value. <2e-16 = essentially zero chance it's noise.
#                   Stars: *** < 0.001, ** < 0.01, * < 0.05, . < 0.1.
#   Null vs Residual deviance -> "no-predictor fit" vs "this model's fit".
#                   The drop (2585.6 -> 2302.6) is what distance explained;
#                   it plays the role of R^2 in logistic regression.

AIC(reg1, reg2)   # lower AIC = better fit. Note how SMALL the gap is:
# reg1 = 2306.6, reg2 = 2304.6 -> a drop of only ~2. Rule of thumb: a
# difference under ~2 is negligible, 4-10 moderate, >10 strong. So angle
# helps, but only a little - distance is most of the story - at least for this dataset.

reg <- reg2   # final model: distance + angle


# =====================================================================
# 3.5) HOW THE FIT IS CHOSEN: maximum likelihood
# =====================================================================
# glm does not guess the curve - it picks the one that makes the observed
# goals/misses most probable (maximum likelihood). This plot shows it on a
# single feature (distance) so there is one clean curve. Each shot sits at
# 0 or 1; the vertical line POINTS to the curve giving the probability the
# model assigned to what ACTUALLY happened (goal -> P(goal); miss -> P(no goal)).
# Those probabilities should be HIGH, so the fit MAXIMISES their product = the
# likelihood (in logs: the log-likelihood; and deviance = -2 * log-likelihood).
#
# Colour = was the model's bet on THIS shot right or wrong (at a 0.5 line)?
#   blue = model gave >50% to what actually happened (bet right)
#   red  = model gave <50% to what actually happened (bet wrong) - i.e. a goal
#          from far out, OR a sitter that was missed.
# NOTE: only a SMALL SAMPLE of shots is drawn (9 goals + 25 misses) so the
# lines are legible - the curve itself is fit on ALL ~3,700 shots below.

m1 <- glm(isGoal ~ distance, data = shots, family = binomial)
bb <- coef(m1)
pg <- function(d) plogis(bb[1] + bb["distance"]*d)

set.seed(3)
samp <- rbind(
  shots[shots$isGoal == 1, ][sample(sum(shots$isGoal == 1), 9), ],
  shots[shots$isGoal == 0, ][sample(sum(shots$isGoal == 0), 25), ]
)

dd <- seq(0, 40, length.out = 300)
plot(dd, pg(dd), type = "l", lwd = 2, ylim = c(0, 1),
     xlab = "Distance to goal (yards)", ylab = "Outcome (0 = miss, 1 = goal)",
     main = "Maximum likelihood: fit the curve to the outcomes")
lines(dd, 1 - pg(dd), lwd = 2)          # mirror curve: P(no goal)

for (i in seq_len(nrow(samp))) {
  d <- samp$distance[i]; g <- samp$isGoal[i]
  ytop <- ifelse(g == 1, 1, 0)
  # the line ends at the probability the model gave to WHAT ACTUALLY HAPPENED:
  #   goal -> P(goal) = pg(d)   ;   miss -> P(no goal) = 1 - pg(d)
  yend <- ifelse(g == 1, pg(d), 1 - pg(d))
  p_actual <- yend                       # probability assigned to the real outcome
  col  <- ifelse(p_actual < 0.5, "red", "blue")  # red = model was confidently wrong
  segments(d, ytop, d, yend, col = col)
  points(d, ytop, pch = 16)
}

# --- label the two curves directly + a colour legend for the lines ---
text(40, pg(40) + 0.06,     "P(goal)",                 col = "black", cex = 0.8, pos = 2, font = 2)
text(34, 1 - pg(34) - 0.04, "1 - P(goal) = P(no goal)", col = "black", cex = 0.8, pos = 1, font = 2)
legend("right",
       legend = c("model bet right (>0.5)", "model bet wrong (<0.5)"),
       lwd = 1, col = c("blue", "red"), bty = "n", cex = 0.8)

# =====================================================================
# 4) READ OFF GOAL PROBABILITY (the actual "xG")
# =====================================================================
# predict(type = "response") turns the model into a probability per shot.
shots$my_xg <- predict(reg, type = "response")

# How does this 2-feature model compare to StatsBomb's full model?
cor(shots$my_xg, shots$shot_statsbomb_xg)        # ~0.75, not bad for 2 inputs
sum(shots$my_xg)                                 # total xG from the model
sum(shots$isGoal)                                # actual goals - these should match!
# A logistic model's fitted probabilities sum to the observed count, so the
# total xG "adds up" to the real number of goals. This is a CALIBRATION
# property (the total is right), NOT proof the model is accurate shot-by-shot.


# =====================================================================
# 5) WHAT THE MODEL LEARNED: marginal effects
# =====================================================================
b <- coef(reg)

# Hold angle straight-on (90), vary distance:
d_seq <- 0:45
p_dist <- plogis(b[1] + b["distance"]*d_seq + b["angle"]*90)
plot(d_seq, p_dist, type = "l", lwd = 2,
     xlab = "Distance to goal (yards)", ylab = "Goal probability",
     main = "Effect of distance (angle = 90)")

# Hold distance at 15, vary angle:
a_seq <- 0:90
p_ang <- plogis(b[1] + b["distance"]*15 + b["angle"]*a_seq)
plot(a_seq, p_ang, type = "l", lwd = 2,
     xlab = "Angle to goal line (90 = straight on)", ylab = "Goal probability",
     main = "Effect of angle (distance = 15)")

# Both curves move the way intuition says: closer is better, straighter on is
# better. Compare the swings - distance covers a much wider range than angle,
# so distance is the stronger predictor (matching the coefficients and AIC).


# =====================================================================
# 6) THE xG MAP: pixelated heatmap
# =====================================================================
# Predict over a grid and shade each cell by goal probability - the classic
# xG map. Bright = high chance, dark = low.

pix <- 1.5   # pixel size: smaller = finer, larger = chunkier
grid <- expand.grid(xpos = seq(80, 120, by = pix),
                    ypos = seq(0, 80, by = pix))
grid$distance <- sqrt((120 - grid$xpos)^2 + (40 - grid$ypos)^2)
grid$angle    <- atan2(120 - grid$xpos, abs(40 - grid$ypos)) * 180 / pi
grid$prob     <- predict(reg, newdata = grid, type = "response")

ggplot() +
  # pixelated heatmap fill
  geom_tile(data = grid, aes(x = ypos, y = xpos, fill = prob)) +
  # sqrt transform stretches the low end, so the large low-xG area shows a
  # gradient instead of one flat dark-blue slab
  scale_fill_viridis_c(option = "plasma", name = "Goal prob. (xG)",
                       trans = "sqrt",
                       breaks = c(0.01, 0.05, 0.10, 0.20, 0.40),
                       labels = scales::percent) +
  # pitch markings in white
  geom_rect(aes(xmin = 18, xmax = 62, ymin = 102, ymax = 120),
            inherit.aes = FALSE, fill = NA, color = "white", linewidth = 0.5) +
  geom_rect(aes(xmin = 30, xmax = 50, ymin = 114, ymax = 120),
            inherit.aes = FALSE, fill = NA, color = "white", linewidth = 0.5) +
  geom_rect(aes(xmin = 36, xmax = 44, ymin = 120, ymax = 121),
            inherit.aes = FALSE, fill = NA, color = "white", linewidth = 0.5) +
  coord_fixed(ylim = c(80, 121), xlim = c(8, 72)) +
  scale_y_reverse() +
  labs(title = "Expected goals (xG) heatmap") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12,
                                  margin = margin(b = 8)),
        legend.position = "right")

# Tip: change `pix` (e.g. 1, 2, 4) to make the pixels finer or chunkier.

# Done. Two features, one logistic regression -> a usable xG model.
