library(colorspace)
library(ragg)
library(cowplot)
library(ggtext)
library(pdftools)
library(showtext)

font_add("Neutraface Slab Display TT Bold", 
         regular = here::here("fonts", "NeutrafaceSlabDisplayTT-Bold.ttf"))
font_add("Neutraface Slab Display TT Titl", 
         regular = here::here("fonts", "NeutrafaceSlabDisplayTT-Titling.ttf"))
font_add("Neutraface Display Medium", 
         regular = here::here("fonts", "NeutrafaceSlabDisplayTT-Medium.ttf"))
font_add("Neutraface Text Book Italic", 
         regular = here::here("fonts", "NeutrafaceSlabTextTT-BookItalic.ttf"))
showtext_auto()

theme_set(theme_minimal(base_size = 15, base_family = "Neutraface Slab Display TT Bold"))

theme_update(
  panel.grid.major = element_line(color = "grey92", size = .4),
  panel.grid.minor = element_blank(),
  axis.title.x = element_text(color = "grey30", margin = margin(t = 7)),
  axis.title.y = element_text(color = "grey30", margin = margin(r = 7)),
  axis.text = element_text(color = "grey50"),
  axis.ticks =  element_line(color = "grey92", size = .4),
  axis.ticks.length = unit(.6, "lines"),
  legend.position = "top",
  plot.title = element_text(hjust = 0, color = "black", 
                            family = "Neutraface 2 Display Titling",
                            size = 21, margin = margin(t = 10, b = 35)),
  plot.subtitle = element_text(hjust = 0, face = "bold", color = "grey30",
                               family = "Neutraface Text Book Italic", 
                               size = 14, margin = margin(0, 0, 25, 0)),
  plot.title.position = "plot",
  plot.caption = element_text(color = "grey50", size = 10, hjust = 1,
                              family = "Neutraface Display Medium", 
                              lineheight = 1.05, margin = margin(30, 0, 0, 0)),
  plot.caption.position = "plot", 
  plot.margin = margin(rep(20, 4))
)

pal <- c("#FF8C00", "#A034F0", "#159090")

df_penguins <- palmerpenguins::penguins %>% 
  mutate(species = as.character(species)) %>% 
  mutate(species = if_else(species == "Adelie", "Adélie", species))

df_rect <-
  tibble(
    xmin = c(-Inf, 2.46, 3.27),
    xmax = c(Inf, Inf, Inf),
    ymin = c(3, 2, 1),
    ymax = c(Inf, Inf, Inf)
  )

df_peng_iqr <- 
  df_penguins %>% 
  mutate(bill_ratio = bill_length_mm / bill_depth_mm) %>% 
  filter(!is.na(bill_ratio)) %>% 
  group_by(species) %>% 
  mutate(
    median = median(bill_ratio),
    q25 = quantile(bill_ratio, probs = .25),
    q75 = quantile(bill_ratio, probs = .75),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(species_num = as.numeric(fct_rev(species))) 

img_file <- here::here("img", "lter_penguins.png")
img <- png::readPNG(img_file)
i2 <- grid::rasterGrob(img, interpolate = T)

rain <- 
  ggplot(df_peng_iqr, aes(bill_ratio, species_num - .2)) +
  geom_rect(
    data = df_rect,
    aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax
    ),
    inherit.aes = F,
    fill = "white"
  ) +
  geom_linerange(
    data = df_peng_iqr %>% 
      group_by(species, species_num) %>% 
      summarize(m = unique(median)),
    aes(
      xmin = -Inf, 
      xmax = m, 
      y = species_num,
      color = species
    ),
    inherit.aes = F,
    linetype = "dotted",
    size = .7
  ) +
  geom_boxplot(
    aes(
      color = species,
      color = after_scale(darken(color, .1, space = "HLS"))
    ),
    width = 0,
    size = .9
  ) +
  geom_rect(
    aes(
      xmin = q25,
      xmax = median,
      ymin = species_num - .05,
      ymax = species_num - .35
    ),
    fill = "grey89"
  ) +
  geom_rect(
    aes(
      xmin = q75,
      xmax = median,
      ymin = species_num - .05,
      ymax = species_num - .35
    ),
    fill = "grey79"
  ) +
  geom_segment(
    aes(
      x = q25, 
      xend = q25,
      y = species_num - .05,
      yend = species_num - .35,
      color = species,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    linewidth = .25
  ) +
  geom_segment(
    aes(
      x = q75, 
      xend = q75,
      y = species_num - .05,
      yend = species_num - .35,
      color = species,
      color = after_scale(darken(color, .05, space = "HLS"))
    ),
    linewidth = .25
  ) +
  geom_point(
    aes(
      color = species
    ), 
    shape = "|",
    size = 5,
    alpha = .33
  ) +
  ggdist::stat_halfeye(
    aes(
      y = species_num,
      color = species,
      fill = after_scale(lighten(color, .5))
    ),
    shape = 18,
    point_size = 3,
    interval_size = 1.8,
    adjust = .5,
    .width = c(0, 1)
  ) +
  geom_text(
    data = df_peng_iqr %>% 
      group_by(species, species_num) %>% 
      summarize(m = unique(median)),
    aes(
      x = m, 
      y = species_num + .12,
      label = format(round(m, 2), nsmall = 2)
    ),
    inherit.aes = F,
    color = "white",
    family = "Neutraface Slab Display TT Titl",
    size = 3.5
  ) +
  geom_text(
    data = df_peng_iqr %>% 
      group_by(species, species_num) %>% 
      summarize(n = unique(n), max = max(bill_ratio, na.rm = T)),
    aes(
      x = max + .01, 
      y = species_num + .02,
      label = glue::glue("n = {n}"),
      color = species
    ),
    inherit.aes = F,
    family = "Neutraface Slab Display TT Bold",
    size = 3.5,
    hjust = 0
  ) +
  annotation_custom(i2, ymin = 2.5, ymax = 3.6, xmin = 3, xmax = 3.7) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(
    limits = c(1.57, 3.7),
    breaks = seq(1.6, 3.6, by = .2),
    expand = c(.001, .001)
  ) +
  scale_y_continuous(
    limits = c(.55, NA),
    breaks = 1:3,
    labels = c("Gentoo", "Chinstrap", "Adélie"),
    expand = c(0, 0)
  ) +
  scale_color_manual(
    values = pal,
    guide = "none"
  ) +
  scale_fill_manual(
    values = pal,
    guide = "none"
  ) +
  labs(
    x = "Bill ratio",
    y = NULL,
    subtitle = "Distribution of the bill ratio, estimated as bill length divided by bill depth",
    caption = 'Visualization: Cédric Scherer  •  Data: Gorman, Williams & Fraser (2014) •  Illustrations: Allison Horst'
  ) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(family = "Neutraface Slab Display TT Titl", 
                               color = rev(pal), size = 14, lineheight = .9),
    axis.ticks.length = unit(0, "lines"),
    plot.subtitle = element_text(margin = margin(0, 0, -10, 0))
  )
