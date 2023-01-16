library(fabricatr)
library(GGally)
library(ggplot2)

# Based on ARC-HBR validation paper (Cao et al.)

# Define the proportions of HBR patients to be in each HBR score category - central illustration figure
arc_hbr_prop = c("0"=0, "1"=0.616, "2"=0.291, "3"=0.079, "4"=0.014, "5"=0, "6"=0, "7"=0, "8"=0, "9"=0)

# Create the data
hbr_data <- fabricate(
  N       = 1e4,
  sex     = factor(draw_binary(0.751, N=N), levels=c(0,1), labels=c("female", "male")),

  # MAJOR criteria - from figure 1
  major_oac     = as.logical(draw_binary(0.185, N=N)),
  major_sev_ckd = as.logical(draw_binary(0.137, N=N)),
  major_anaemia = as.logical(draw_binary(0.332, N=N)),
  major_bleed   = as.logical(draw_binary(0.037, N=N)),
  major_thrmcyt = as.logical(draw_binary(0.043, N=N)),
  major_surgery = as.logical(draw_binary(0.082, N=N)),
  major_cva     = as.logical(draw_binary(0.207, N=N)),
  major_malig   = as.logical(draw_binary(0.168, N=N)),

  # MINOR criteria - from figure 1
  age     = rnorm(N, mean=71.7, sd=11.0),
  minor_age_gt75= age>=75,
  minor_mild_an = as.logical(draw_binary(0.369, N=N)),
  minor_mod_ckd = as.logical(draw_binary(0.396, N=N)),
) %>%
  # Calculate the number of times the HBR category has been satisfied
  rowwise() %>%
  mutate(maj_score = sum(c_across(starts_with("major"))),
         min_score = sum(c_across(starts_with("minor"))) %/% 2,
         arc_hbr_score = sum(maj_score, min_score)) %>%
  ungroup() %>%
  # Ditch the scores we don't care about
  filter(between(arc_hbr_score, 1, 4)) %>%
  # Apply the wanted proportions of each HBR score group
  group_by(arc_hbr_score) %>%
  group_modify(~slice_head(.x, n=1000*arc_hbr_prop[[as.character(.y)]])) %>%
  ungroup()

# Check how well we did
data %>%
  mutate(n=n()) %>%
  group_by(arc_hbr_score) %>%
  summarise(prop = n()/first(n),
            across(matches("major|minor"), ~ sum(.x)/first(n))) %>%
  bind_rows(
    data %>%
      summarise(arc_hbr_sc = NA_real_,
                prop = n()/n(),
                across(matches("major|minor"), ~ sum(.x)/n()))
  ) %>%
  View()

# Pairs plot
# ggpairs(data    = data %>% select(-ID))
#         # mapping = ggplot2::aes(colour=arc_hbr))
