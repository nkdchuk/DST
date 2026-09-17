#### Data Science Toolbox - Tutorial 7: Data Visualization ####

# first install packages in case they are not already installed on your device
# install.packages("tidyverse")
# install.packages("ggridges")

library("tidyverse")   # loads dplyr, ggplot2, forcats, ... in one go
library("ggridges")    # adds the ridgeline geom we use in Exercise 3

#### Exercise 3: explore the AI-literacy dataset ####
# dst.csv columns: group, ai_lit (literacy), ai_sat (satisfaction with AI)
# groups: hl = professors, wm = research staff,
#         stud = students, mtsv = technical/service staff

data = read.csv("dst.csv")


## Plot 1: group means with 95% confidence intervals ------------------------
# for each group we need the mean, the standard error (= sd / sqrt(n)) and n.
# the standard error tells us how precise the mean is - it shrinks as n grows,
# so the confidence-interval bars are narrower for bigger groups
temp = data %>%
  group_by(group) %>%
  summarize(mean_lit = mean(ai_lit),
            sd_lit   = sd(ai_lit),
            se_lit   = sd_lit / sqrt(n()),
            n        = n(),
            sqrtn    = sqrt(n)) %>%      
  ungroup()

# the dot is the group mean; the bar is mean +/- 1.96*SE (the 95% confidence interval).
# note: coord_cartesian(ylim=c(3,4)) zooms the axis so small differences are visible -
# be aware this exaggerates the gaps (see the discussion in Exercise 2!)
ggplot(data=temp, aes(x=group, color=group, y=mean_lit, size=sqrtn))+
  geom_pointrange(aes(ymin=mean_lit-1.96*se_lit, ymax=mean_lit+1.96*se_lit))+
  coord_cartesian(ylim=c(3,4))+
  scale_color_viridis_d()+
  scale_size_continuous(range=c(0.3, 1.5), name="√n")+
  theme_bw()


## Plot 2: full distributions per group (ridgeline) -------------------------
# means hide the shape of the data. a ridgeline plot draws a smoothed density
# for each group so we can see the whole distribution, not just its centre.
# scale controls how much the ridges overlap; alpha keeps overlaps readable
ggplot(data, aes(x=ai_lit, y=group, fill=group)) +
  geom_density_ridges(alpha=.8, scale=4) +
  scale_fill_viridis_d(option = "D") +
  theme_ridges()+
  labs(x="AI literacy", y="group")


## Plot 3: relationship between literacy and satisfaction -------------------
# both variables sit on coarse scales, so many people share the exact same point.
# geom_jitter adds a tiny random nudge (+/-0.05) so overlapping points fan out;
# alpha shows density (darker = more points stacked).
# geom_smooth(method="lm") adds the linear trend line;
# geom_density_2d draws contour "hills" over the densest region
ggplot(data, aes(x=ai_lit, y=ai_sat))+
  geom_jitter(width=0.05, height=0.05, alpha=0.5, shape=16, size=3)+
  geom_smooth(method="lm", alpha=0.1)+
  geom_density_2d()+
  theme_bw()+
  labs(x="AI literacy", y="AI satisfaction")