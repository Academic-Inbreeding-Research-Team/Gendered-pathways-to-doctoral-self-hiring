
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

DATA_ROOT <- "."
if (!dir.exists(file.path(DATA_ROOT, "fig1&edfig1"))) DATA_ROOT <- ".."
DATA_DIR <- file.path(DATA_ROOT, "fig1&edfig1")
if (!dir.exists(DATA_DIR)) stop("Set DATA_ROOT to the directory containing the source-data folders.")
read_source <- function(file, sheet = 1) {
  data <- read_excel(file.path(DATA_DIR, file), sheet = sheet)
  # Excel labels may store a literal backslash-n instead of a line break.
  data %>% mutate(across(where(is.character), ~ gsub("\\n", "\n", .x, fixed=TRUE)))
}

library(sf)
library(ggrepel)
library(gridExtra)

COL_F <- "#d62728"

COL_M <- "#1f77b4"

COHORT_SHORT <- c("1990–94","1995–99","2000–04","2005–09","2010–14","2015–19","2020–24")

theme_fig <- theme_classic(base_size = 9, base_family = "Arial") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 13, hjust = 0),
    axis.title = element_text(size = 9, colour = "black"),
    axis.text = element_text(size = 8.5, colour = "black"),
    axis.line = element_line(linewidth = 0.55, colour = "black"),
    axis.ticks = element_line(linewidth = 0.55, colour = "black"),
    panel.grid = element_blank(),
    plot.margin = margin(6, 8, 6, 6)
  )

# a. Aggregate and standardized differences
 a_segments <- read_source("1a.xlsx")
a_points <- pivot_longer(a_segments, c(Male, Female),
                         names_to="gender_final", values_to="estimate") %>%
  mutate(gender_final=factor(gender_final, levels=c("Male","Female")))
P_1A <- ggplot() +
  geom_segment(
    data=a_segments,
    aes(x=Male,xend=Female,y=y,yend=y),
    colour="grey50",
    linewidth=0.75
  ) +
  geom_point(
    data=a_points,
    aes(x=estimate,y=y,colour=gender_final),
    size=2.8
  ) +
  geom_text(
    data=a_points,
    aes(
      x=estimate,
      y=y+0.17,
      colour=gender_final,
      label=sprintf("%.2f%%",estimate)
    ),
    size=3,
    fontface="bold",
    show.legend=FALSE
  ) +
  geom_text(
    data=a_segments,
    aes(
      x=midpoint,
      y=y-0.25,
      label=difference_label
    ),
    size=2.85,
    lineheight=1.15,
    fontface="bold",
    colour="black"
  ) +
  scale_colour_manual(
    values=c("Female"=COL_F,"Male"=COL_M),
    breaks=c("Female","Male"),
    labels=c("Women","Men"),
    name=NULL,
    guide=guide_legend(
      override.aes=list(size=3),
      nrow=1
    )
  ) +
  scale_x_continuous(
    breaks=seq(18,21.5,0.5),
    expand=expansion(mult=c(0,0))
  ) +
  scale_y_continuous(
    limits=c(0.48,3.40),
    breaks=c(1,2,3),
    labels=c(
      "Adjusted\n(system + domain)",
      "Unadjusted\n(System- and\ndomain-eligible sample)",
      "Unadjusted\n(Full eligible sample)"
    ),
    expand=expansion(mult=c(0,0))
  ) +
  labs(
    title="a  Aggregate and standardized differences",
    x="Self-hiring (%)",
    y=NULL
  ) +
  coord_cartesian(
    xlim=c(18,21.5),
    clip="off"
  ) +
  theme_fig +
  theme(
    text=element_text(family="sans"),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(
      size=9.5,
      face="bold",
      margin=margin(r=6)
    ),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.justification="center",
    legend.text=element_text(size=9.5),
    legend.key.size=unit(0.35,"cm"),
    legend.spacing.x=unit(0.4,"cm"),
    legend.margin=margin(t=3,b=0),
    plot.margin=margin(t=6,r=12,b=6,l=6)
  )

# b. Disciplinary differences (stored in 1b.xlsx).
domain_rd <- read_source("1b.xlsx") %>%
  rename(current_domain=first_faculty_domain)
P_1C <- ggplot(domain_rd) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "grey65"
  ) +
  geom_errorbarh(
    aes(
      y = y,
      xmin = conf.low_pp,
      xmax = conf.high_pp
    ),
    height = 0.13,
    linewidth = 0.5,
    colour = "#4C9A9A"
  ) +
  geom_point(
    aes(
      x = estimate_pp,
      y = y
    ),
    size = 2.5,
    colour = "#4C9A9A"
  ) +
  geom_text(
    aes(
      x = estimate_pp + if_else(estimate_pp >= 0, 0.35, -0.35),
      y = y + 0.22,
      label = sprintf("%+.2f", estimate_pp),
      hjust = if_else(estimate_pp >= 0, 0, 1)
    ),
    size = 2.9,
    colour = "#4C9A9A",
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = -0.35,
    xend = -3.3,
    y = 6.62,
    yend = 6.62,
    colour = "black",
    linewidth = 0.65,
    arrow = arrow(length = unit(0.09, "inches"))
  ) +
  annotate(
    "segment",
    x = 0.35,
    xend = 3.3,
    y = 6.62,
    yend = 6.62,
    colour = "black",
    linewidth = 0.65,
    arrow = arrow(length = unit(0.09, "inches"))
  ) +
  annotate(
    "text",
    x = -3,
    y = 7,
    label = "Men higher",
    colour = "black",
    fontface = "bold",
    size = 3.0
  ) +
  annotate(
    "text",
    x = 4,
    y = 7,
    label = "Women higher",
    colour = "black",
    fontface = "bold",
    size = 3.0
  ) +
  scale_x_continuous(
    limits = c(-6, 6.3),
    breaks = seq(-6, 6, 2),
    labels = function(x) ifelse(
      x > 0,
      paste0("+", x),
      as.character(x)
    ),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    limits = c(0.5, 7.0),
    breaks = domain_rd$y,
    labels = domain_rd$current_domain
  ) +
  labs(
    title = "b  Disciplinary differences",
    x = "Women − men difference in\nself-hiring (pp)",
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_fig +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_text(
      size = 8.5,
      margin = margin(r = 5)
    )
  )

# c. Faculty-entry cohorts (stored in 1c.xlsx).
cohort_rd <- read_source("1c.xlsx")
P_1D <- ggplot(
  cohort_rd,
  aes(x = x, y = estimate_pp)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    colour = "grey65"
  ) +
  geom_errorbar(
    aes(
      ymin = conf.low_pp,
      ymax = conf.high_pp
    ),
    width = 0.12,
    linewidth = 0.55,
    colour = "#4C9A9A"
  ) +
  geom_line(
    colour = "#4C9A9A",
    linewidth = 0.85
  ) +
  geom_point(
    colour = "#4C9A9A",
    size = 2.5
  ) +
  geom_text(
    aes(label = sprintf("%.2f", estimate_pp)),
    nudge_x = 0.3,
    nudge_y = -0.3,
    colour = "#4C9A9A",
    size = 2.9,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = 1:7,
    labels = COHORT_SHORT,
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(
    breaks = seq(0, 5, 1),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  labs(
    title = "c  Faculty-entry cohorts",
    x = "Faculty-entry cohort",
    y = "Women − men difference in\nself-hiring (pp)"
  ) +
  coord_cartesian(
    ylim = c(-0.25, 4.85),
    clip = "off"
  ) +
  theme_fig +
  theme(
    axis.text.x = element_text(
      angle = 30,
      hjust = 1,
      vjust = 1
    )
  )

# d. Academic-system map (stored in 1d.xlsx).
# Medium-resolution basemap is supplied by rnaturalearth/rnaturalearthdata.
country_rd <- read_source("1d.xlsx", "Fig1d") %>%
  transmute(first_faculty_country=`First-faculty academic system`,
            iso3=`ISO 3166-1 alpha-3 code`,
            estimate_pp=gender_difference_pp,
            conf.low_pp=ci_lower_pp, conf.high_pp=ci_upper_pp,
            ci_excludes_zero=as.logical(ci_excludes_zero))
world <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) %>%
  mutate(
    iso3 = if_else(iso_a3 == "-99", adm0_a3, iso_a3)
  )

map_fig1b <- world %>%
  left_join(country_rd, by = "iso3")

n_supported <- nrow(country_rd)
n_positive <- sum(country_rd$estimate_pp > 0)
median_rd <- median(country_rd$estimate_pp)


MAP_NEG_DARK  <- "#4A2C7A"
MAP_NEG_MID   <- "#8C6BB1"
MAP_NEG_LIGHT <- "#D6CDE5"

MAP_ZERO <- "#FFFFFF"

MAP_POS_LIGHT <- "#C7E3D6"
MAP_POS_MID   <- "#4F9E7C"
MAP_POS_DARK  <- "#005C42"

MAP_NA <- "#AFAFAF"
P_1B_map <- ggplot(map_fig1b) +
  geom_sf(
    aes(fill = estimate_pp),
    colour = "white",
    linewidth = 0.12
  ) +
  scale_fill_gradientn(
    colours = c(
      MAP_NEG_DARK,
      MAP_NEG_MID,
      MAP_NEG_LIGHT,
      MAP_ZERO,
      MAP_POS_LIGHT,
      MAP_POS_MID,
      MAP_POS_DARK
    ),
    values = scales::rescale(
      c(-15, -5, -1.5, 0, 1.5, 5, 15),
      from = c(-15, 15)
    ),
    limits = c(-15, 15),
    breaks = c(-15, -10, -5, 0, 5, 10, 15),
    labels = c("≤−15", "−10", "−5", "0", "+5", "+10", "≥+15"),
    oob = scales::squish,
    na.value = MAP_NA,
    name = "Women − men\ndifference in\nself-hiring (pp)"
  ) +
  coord_sf(
    crs = "+proj=robin",
    default_crs = st_crs(4326),
    expand = FALSE
  ) +
  labs(title = "d  Geographic variation") +
  theme_void(base_size = 9) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "right",
    legend.title = element_text(
      size = 8.5,
      margin = margin(b = 12)
    ),
    legend.text = element_text(size = 8),
    legend.key.height = unit(18, "mm"),
    legend.key.width = unit(3.2, "mm"),
    plot.margin = margin(6, 6, 0, 6)
  )
set.seed(1)
country_rd_plot <- country_rd %>%
  mutate(plot_y = runif(n(), -0.055, 0.055))

focus_countries <- c("China", "United States", "Japan", "United Kingdom")

label_df <- country_rd_plot %>%
  filter(first_faculty_country %in% focus_countries | estimate_pp > 30 | estimate_pp < -5) %>%
  distinct(first_faculty_country, .keep_all = TRUE) %>%
  mutate(
    label_name = case_when(
      TRUE ~ first_faculty_country
    ),
    label_nudge = if_else(estimate_pp >= 0, 0.11, -0.11)
  )
P_1B_inset <- ggplot(country_rd_plot, aes(x = estimate_pp, y = plot_y)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.45, colour = "grey55") +
  geom_point(
    data = country_rd_plot %>% filter(ci_excludes_zero),
    aes(colour = direction),
    size = 1.8
  ) +
  geom_point(
    data = country_rd_plot %>% filter(!ci_excludes_zero),
    aes(colour = direction),
    shape = 21,
    fill = "white",
    stroke = 0.7,
    size = 1.8
  ) +
  geom_text_repel(
    data = label_df,
    aes(label = label_name, colour = direction),
    nudge_y = label_df$label_nudge,
    size = 2.7,
    fontface = "bold",
    box.padding = 0.40,
    point.padding = 0.25,
    force = 1.8,
    force_pull = 0.1,
    min.segment.length = 0,
    segment.color = "grey40",
    segment.size = 0.35,
    seed = 1,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  scale_colour_manual(
    values = c("Men higher" = MAP_NEG_MID, "Women higher" = MAP_POS_MID),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = c(-15, 35),
    breaks = c(-10, 0, 10, 20, 30),
    labels = function(x) ifelse(x > 0, paste0("+", x), as.character(x)),
    expand = expansion(mult = c(0, 0.01))
  ) +
  scale_y_continuous(limits = c(-0.18, 0.18), expand = c(0, 0)) +
  labs(x = "Women − men difference in self-hiring (pp)", y = NULL) +
  theme_classic(base_size = 8.5) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    axis.text.x = element_text(size = 7.8, margin = margin(t = 2)),
    axis.title.x = element_text(size = 8.3, margin = margin(t = 3)),
    axis.line.x = element_line(linewidth = 0.5),
    axis.ticks.x = element_line(linewidth = 0.45),
    plot.margin = margin(t = 2, r = 8, b = 2, l = 4)
  )
P_1B_summary <- textGrob(
  sprintf(
    "Positive point estimates: %d/%d systems\nMedian difference: %+.2f pp\nSolid/open: 95%% CI excludes/includes zero",
    n_positive, n_supported, median_rd
  ),
  x = unit(0.05, "npc"),
  y = unit(0.55, "npc"),
  hjust = 0,
  vjust = 0.5,
  gp = gpar(fontsize = 8.5, col = "black", lineheight = 1.25)
)

legend_b <- cowplot::get_legend(
  P_1B_map +
    theme(
      legend.position = "right",
      legend.box.margin = margin(0, 0, 0, 0)
    )
)

P_1B_map_noleg <- P_1B_map +
  theme(legend.position = "none")

# The continuous colour bar cannot by itself represent missing countries.
NA_KEY <- gridExtra::arrangeGrob(
  grid::rectGrob(gp=grid::gpar(fill=MAP_NA,col=NA),
                 width=grid::unit(3.5,"mm"),height=grid::unit(3.5,"mm")),
  grid::textGrob("No estimate",x=grid::unit(0,"npc"),hjust=0,
                 gp=grid::gpar(fontsize=7.5,col="grey25")),
  ncol=2,widths=grid::unit.c(grid::unit(0.55,"cm"),grid::unit(1.75,"cm"))
)

# Combine colour bar and No estimate
legend_b_group <- gridExtra::arrangeGrob(
  legend_b,
  NA_KEY,
  ncol=1,
  heights=grid::unit.c(
    grid::grobHeight(legend_b),
    grid::unit(0.5,"cm")
  )
)

# Move the entire legend group downward
legend_b_full <- gridExtra::arrangeGrob(
  grid::nullGrob(),
  legend_b_group,
  grid::nullGrob(),
  ncol=1,
  heights=grid::unit.c(
    grid::unit(0.8,"cm"),
    grid::grobHeight(legend_b_group),
    grid::unit(-1.2,"null")
  )
)


P_1B_bottom <- arrangeGrob(
  nullGrob(),
  P_1B_summary,
  P_1B_inset,
  ncol = 3,
  widths = c(0.30, 0.70, 2.00)
)

P_1B_all <- arrangeGrob(
  P_1B_map_noleg,
  legend_b_full,
  P_1B_bottom,
  nullGrob(),
  layout_matrix = rbind(
    c(1, 2),
    c(3, 4)
  ),
  widths = c(1, 0.12),
  heights = c(4, 1.3)
)

FIG1_top <- arrangeGrob(
  nullGrob(),
  P_1A,
  P_1C,
  P_1D,
  ncol=4,
  widths=unit.c(
    unit(0.35,"cm"),
    unit(1.1,"null"),
    unit(1.1,"null"),
    unit(1,"null")
  )
)

FIG1 <- arrangeGrob(
  FIG1_top,
  P_1B_all,
  ncol=1,
  heights=c(.45,.8)
)

grid.newpage()
grid.draw(FIG1)
