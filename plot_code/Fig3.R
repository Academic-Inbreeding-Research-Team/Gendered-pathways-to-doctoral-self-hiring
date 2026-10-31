# Figure 3: plotting from published source data only.
# Run from the submission root, or from this script's directory.
# Keep the four source-data folders beside submission_plot_code.
# No model fitting, input-file changes, or automatic file export.
# Packages: readxl, dplyr, tidyr, ggplot2, patchwork, scales.
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

DATA_ROOT <- "."
if (!dir.exists(file.path(DATA_ROOT, "fig3"))) DATA_ROOT <- ".."
DATA_DIR <- file.path(DATA_ROOT, "fig3")
if (!dir.exists(DATA_DIR)) stop("Set DATA_ROOT to the directory containing the source-data folders.")
read_source <- function(file, sheet = 1) {
  data <- read_excel(file.path(DATA_DIR, file), sheet = sheet)
  # Excel labels may store a literal backslash-n instead of a line break.
  data %>% mutate(across(where(is.character), ~ gsub("\\n", "\n", .x, fixed=TRUE)))
}

PATHS <- c("ABA","ABB","ABC")

PATH_LABELS <- c(
  "ABA"="External return/self-hire",
  "ABB"="Prior external-institution entry",
  "ABC"="Other-institution entry"
)

COL_GENDER <- c("Female"="#D62728","Male"="#1F77B4")

PATH_COL <- c(
  ABA="#9B77D5",
  ABB="#B66E44",
  ABC="#E5D5C5"
)

path_fill <- scale_fill_manual(
  values=PATH_COL,
  breaks=PATHS,
  labels=unname(PATH_LABELS[PATHS]),
  name=NULL,
  drop=FALSE,
  guide=guide_legend(nrow=1,byrow=TRUE)
)

COL_DIFF <- "#4C9A9A"

COL_FE <- "#8B73B5"

EFFECTS <- c("ABA","ABB","ABC","ABA - ABB")

SPECS <- c("Unadjusted","+ Country","+ Country + domain",
           "Full adjustment","Doctoral-institution FE")

EFFECT_LABELS <- c(
  "ABA"="External return/self-hire (ABA)",
  "ABB"="Prior external-institution entry (ABB)",
  "ABC"="Other-institution entry (ABC)",
  "ABA - ABB"="Difference in gender gaps (ABA − ABB)"
)

SPEC_LABELS <- c(
  "Unadjusted"="Unadjusted",
  "+ Country"="+ Country",
  "+ Country + domain"="+ Country\n+ domain",
  "Full adjustment"="Full\nadjustment",
  "Doctoral-institution FE"="Doctoral-institution\nfixed effects"
)

RANK_LEVELS <- c(
  "Top 100",
  "101–500",
  "501–1,500",
  "1,501–2,500",
  "2,501–3,500",
  ">3,500"
)

# a. Observed composition and adjusted probabilities.
fig3a_overall <- read_source("3a.xlsx","top") %>%
  mutate(pathway=factor(pathway,levels=PATHS))
fig3a_prob_data <- read_source("3a.xlsx","bottom")
P3a_observed <- ggplot(fig3a_overall) +
  geom_rect(
    aes(xmin=xmin,xmax=xmax,ymin=0.72,ymax=1.28,fill=pathway),
    colour="white",linewidth=0.5
  ) +
  geom_text(
    aes(x=xmid,y=1,label=label,colour=text_col),
    size=3.1,fontface="bold",show.legend=FALSE
  ) +
  path_fill +
  scale_colour_identity(guide="none") +
  scale_x_continuous(
    breaks=seq(0,100,20),
    labels=function(x)sprintf("%g%%",x),
    expand=expansion(mult=0)
  ) +
  scale_y_continuous(
    breaks=NULL,
    expand=expansion(mult=c(0.2,0.2))
  ) +
  coord_cartesian(xlim=c(0,100),clip="off") +
  labs(
    title="Observed sample composition",
    x="Share of analytical sample (%)",
    y=NULL
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(face="bold",size=11.5,colour="#202124"),
    plot.title.position="plot",
    axis.text.x=element_text(size=9,colour="black"),
    axis.title.x=element_text(size=9.5,margin=margin(t=6)),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.y=element_blank(),
    axis.line.x=element_line(colour="black",linewidth=0.35),
    axis.ticks.x=element_line(colour="black",linewidth=0.3),
    legend.position="bottom",
    legend.text=element_text(size=8.5),
    legend.key.width=grid::unit(1,"lines"),
    legend.spacing.x=grid::unit(0.3,"cm"),
    plot.margin=margin(5,8,5,5)
  )
plot_dat <- fig3a_prob_data %>%
  mutate(
    pathway=factor(pathway,levels=PATHS),
    gender=factor(gender,levels=c("Male","Female")),
    y=ifelse(gender=="Female",2,1)
  ) %>%
  arrange(gender,pathway) %>%
  group_by(gender) %>%
  mutate(
    xmax=cumsum(probability),
    xmin=xmax-probability,
    xmid=(xmin+xmax)/2,
    label=sprintf("%.1f%%",probability),
    text_col=if_else(as.character(pathway)=="ABC","#30343B","#FFFFFF")
  ) %>%
  ungroup()

P3a_adjusted <- ggplot(plot_dat) +
  geom_rect(
    aes(xmin=xmin,xmax=xmax,ymin=y-0.25,ymax=y+0.25,fill=pathway),
    colour="white",linewidth=0.5
  ) +
  geom_text(
    aes(x=xmid,y=y,label=label,colour=text_col),
    size=3.1,fontface="bold",show.legend=FALSE
  ) +
  path_fill +
  scale_colour_identity(guide="none") +
  scale_x_continuous(
    breaks=seq(0,100,20),
    labels=function(x)sprintf("%g%%",x),
    expand=expansion(mult=0)
  ) +
  scale_y_continuous(
    breaks=c(1,2),
    labels=c("Men","Women"),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(0,100),
    ylim=c(0.55,2.45),
    expand=FALSE
  ) +
  labs(
    title="Adjusted destination probabilities",
    x="Adjusted probability (%)",
    y=NULL
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(face="bold",size=11.5,colour="#202124"),
    plot.title.position="plot",
    axis.title.x=element_text(size=9.5,margin=margin(t=6)),
    axis.text.x=element_text(size=9,colour="black"),
    axis.text.y=element_text(size=9,colour="#30343B"),
    axis.ticks.y=element_blank(),
    axis.line.y=element_blank(),
    axis.line.x=element_line(colour="black",linewidth=0.35),
    axis.ticks.x=element_line(colour="black",linewidth=0.3),
    panel.grid=element_blank(),
    legend.position="bottom",
    legend.text=element_text(size=8.5),
    legend.key.width=grid::unit(1,"lines"),
    legend.spacing.x=grid::unit(0.3,"cm"),
    plot.margin=margin(5,8,5,5)
  )

# Combined Fig. 3a
P3a <- (P3a_observed / P3a_adjusted) +
  plot_layout(
    heights=c(1,1.3),
    guides="collect"
  ) +
  plot_annotation(
    title="a  Observed and adjusted destinations\n    among faculty with external experience",
    theme=theme(
      plot.title=element_text(
        face="bold",size=13,colour="#202124",
        hjust=0,margin=margin(b=8)
      ),
      plot.title.position="plot"
    )
  ) &
  theme(
    legend.position="bottom",
    legend.justification="center",
    legend.direction="horizontal"
  )

# b. Destination-specific gaps.
fig3b_plot <- read_source("3b.xlsx")
x_range <- diff(range(
  c(fig3b_plot$conf_low_pp,fig3b_plot$conf_high_pp,0),
  na.rm=TRUE
))
label_offset <- max(x_range*0.035,0.15)

fig3b_plot <- fig3b_plot %>%
  mutate(label_x=conf_high_pp+label_offset)

x_min <- min(c(0,fig3b_plot$conf_low_pp),na.rm=TRUE)-label_offset*2
x_max <- max(fig3b_plot$label_x,na.rm=TRUE)+x_range*0.32
P3b <- ggplot(fig3b_plot,aes(x=difference_pp,y=y)) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    linewidth=0.5,
    colour="#999999"
  ) +
  geom_segment(
    aes(x=conf_low_pp,xend=conf_high_pp,yend=y),
    linewidth=0.85,
    colour="#4C9A9A",
    lineend="round"
  ) +
  geom_point(
    size=3.0,
    shape=21,
    fill="#4C9A9A",
    colour="white",
    stroke=0.5
  ) +
  geom_text(
    aes(x=label_x,label=difference_label,fontface=fontface),
    hjust=0,
    size=3.0,
    colour="#333333"
  ) +
  scale_y_continuous(
    breaks=c(1,2,3,4),
    labels=c(
      "Difference in gender gaps (ABA − ABB)",
      "Other-institution entry (ABC)",
      "Prior external-institution entry (ABB)",
      "External return/self-hire (ABA)"
    ),
    expand=expansion(add=0.55)
  ) +
  scale_x_continuous(
    breaks=scales::breaks_pretty(n=6),
    # labels=function(x)sprintf("%+.1f",x),
    expand=expansion(mult=c(0,0))
  ) +
  coord_cartesian(
    xlim=c(x_min,x_max),
    clip="off"
  ) +
  labs(
    title="b  Destination-specific gender gaps\n    among faculty with external experience",
    x="Women − men difference (pp)",
    y=NULL
  ) +
  theme_classic(base_size=11) +
  theme(
    plot.title=element_text(
      face="bold",
      size=13,
      hjust=0,
      colour="black",
      margin=margin(b=12)
    ),
    plot.title.position="plot",
    axis.text.x=element_text(size=10,colour="black"),
    axis.text.y=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10.5,margin=margin(t=10)),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major.y=element_line(
      colour="#EEEEEE",
      linewidth=0.35
    ),
    panel.grid.minor=element_blank(),
    plot.margin=margin(10,35,10,8)
  )

# c. Sequential specifications.
fig3c_data <- read_source("3c.xlsx") %>%
  mutate(effect=factor(effect,levels=EFFECTS), is_FE=as.logical(is_FE))
P_3C <- ggplot(fig3c_data,aes(x=xpos,y=estimate)) +
  geom_hline(
    yintercept=0,colour="#999999",
    linetype="dashed",linewidth=0.45
  ) +
  geom_vline(
    xintercept=4.5,colour="#B8B8B8",
    linetype="dotted",linewidth=0.65
  ) +
  geom_line(
    data=filter(fig3c_data,!is_FE),
    aes(group=effect),
    colour=COL_DIFF,linewidth=0.7
  ) +
  geom_errorbar(
    aes(ymin=conf_low,ymax=conf_high,colour=is_FE),
    width=0.10,linewidth=0.65
  ) +
  geom_point(
    aes(colour=is_FE,shape=is_FE),
    size=2.6
  ) +
  scale_colour_manual(
    values=c(`FALSE`=COL_DIFF,`TRUE`=COL_FE),
    breaks=c(FALSE,TRUE),
    labels=c("Sequential adjustment","Doctoral-institution FE"),
    name=NULL
  ) +
  scale_shape_manual(
    values=c(`FALSE`=16,`TRUE`=18),
    guide="none"
  ) +
  scale_x_continuous(
    breaks=1:5,
    labels=unname(SPEC_LABELS[SPECS]),
    limits=c(0.65,5.35),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    expand=expansion(mult=c(0.13,0.18))
  ) +
  facet_wrap(
    ~effect,ncol=1,scales="free_y",
    labeller=as_labeller(EFFECT_LABELS)
  ) +
  labs(
    title="c  Gender gaps across model specifications\n    among faculty with external experience",
    # subtitle=paste(
    #   "Four sequential multinomial specifications;",
    #   "doctoral-institution fixed effects shown separately."
    # ),
    x=NULL,
    y="Women − men difference (pp)"
  ) +
  guides(colour=guide_legend(nrow=1,byrow=TRUE)) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(
      face="bold",size=12,hjust=0,
      margin=margin(b=5)
    ),
    plot.title.position="plot",
    strip.background=element_blank(),
    strip.text=element_text(
      face="bold",size=9,hjust=0,colour="black"
    ),
    strip.placement="outside",
    panel.spacing=grid::unit(0.5,"lines"),
    axis.title.y=element_text(size=9.5,margin=margin(r=7)),
    axis.text.x=element_text(size=9,colour="black",lineheight=0.95),
    axis.text.y=element_text(size=9,colour="black"),
    axis.line=element_line(colour="black",linewidth=0.35),
    axis.ticks=element_line(colour="black",linewidth=0.3),
    axis.ticks.length=grid::unit(2,"pt"),
    panel.grid=element_blank(),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.text=element_text(size=9),
    legend.key.width=grid::unit(1.1,"lines"),
    legend.margin=margin(t=4),
    plot.margin=margin(7,9,7,7)
  )

# d. Doctoral-rank-specific gaps.
# The overall interaction P value is absent from 3d.xlsx.
# This rounded annotation is transcribed from the supplied fig3.pdf;
# it is not recalculated from the subgroup P values.
PLOT_ORDER <- c("Overall",RANK_LEVELS)
COL_D <- "#C47F62"
fig3d_data <- read_source("3d.xlsx") %>%
  mutate(group=factor(group,levels=rev(PLOT_ORDER)),
         is_overall=as.logical(is_overall))
x_min <- min(0,fig3d_data$conf_low,na.rm=TRUE)
x_max <- max(0,fig3d_data$conf_high,na.rm=TRUE)
x_span <- x_max-x_min

x_breaks <- pretty(c(x_min,x_max),n=5)
x_breaks <- x_breaks[x_breaks>=x_min & x_breaks<=x_max]

label_x <- x_max+x_span*0.12
x_left <- x_min-x_span*0.08
x_right <- label_x+x_span*0.65

fig3d_data <- fig3d_data %>%
  mutate(label_x=label_x)
P3d <- ggplot(fig3d_data,aes(x=estimate,y=group)) +
  geom_vline(
    xintercept=0,colour="#A6A6A6",
    linetype="dashed",linewidth=0.4
  ) +
  geom_hline(
    yintercept=length(PLOT_ORDER)-0.5,
    colour="#C9C9C9",
    linetype="dashed",linewidth=0.45
  ) +
  geom_errorbar(
    aes(xmin=conf_low,xmax=conf_high),
    orientation="y",width=0.12,
    linewidth=0.7,colour=COL_D
  ) +
  geom_point(
    aes(shape=is_overall),
    colour=COL_D,size=3
  ) +
  geom_text(
    aes(x=label_x,label=label),
    hjust=0,size=2.8,colour="#30343B"
  ) +
  scale_shape_manual(
    values=c(`TRUE`=18,`FALSE`=16),
    guide="none"
  ) +
  scale_y_discrete(
    limits=rev(PLOT_ORDER),
    expand=expansion(add=0.55)
  ) +
  scale_x_continuous(
    breaks=x_breaks,
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(x_left,x_right),
    clip="on"
  ) +
  labs(
    title="d  External return/self-hire by doctoral rank\n    among faculty with external experience",
    subtitle="     Gender × rank interaction: Wald P = 0.618",
    x="Women − men difference (pp)",
    y=NULL
  ) +
  theme_classic(base_size=10.5) +
  theme(
    plot.title=element_text(
      face="bold",size=11.5,hjust=0,
      colour="#202124",lineheight=1.15,
      margin=margin(b=5)
    ),
    plot.subtitle=element_text(
      size=8.8,colour="#555555",
      margin=margin(b=8)
    ),
    plot.title.position="plot",
    axis.text.y=element_text(
      size=9.2,colour="#202124"
    ),
    axis.text.x=element_text(
      size=8.8,colour="#30343B",
      margin=margin(t=5)
    ),
    axis.title.x=element_text(
      size=9.2,margin=margin(t=10)
    ),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.x=element_line(
      colour="#777777",linewidth=0.35
    ),
    axis.ticks.x=element_line(
      colour="#777777",linewidth=0.3
    ),
    panel.grid.major.x=element_line(
      colour="#F0F0F0",linewidth=0.3
    ),
    panel.grid.minor=element_blank(),
    plot.margin=margin(7,12,7,7)
  )
P_FIG3 <- (
  patchwork::wrap_elements(full=P3a) |
    patchwork::wrap_elements(full=P3b)
) / (
  patchwork::wrap_elements(full=P_3C) |
    patchwork::wrap_elements(full=P3d)
) +
  plot_layout(
    widths=c(1,1.2),
    heights=c(1,1)
  )

print(P_FIG3)
