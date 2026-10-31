
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

DATA_ROOT <- "."
if (!dir.exists(file.path(DATA_ROOT, "fig2&edfig2"))) DATA_ROOT <- ".."
DATA_DIR <- file.path(DATA_ROOT, "fig2&edfig2")
if (!dir.exists(DATA_DIR)) stop("Set DATA_ROOT to the directory containing the source-data folders.")
read_source <- function(file, sheet = 1) {
  data <- read_excel(file.path(DATA_DIR, file), sheet = sheet)
  # Excel labels may store a literal backslash-n instead of a line break.
  data %>% mutate(across(where(is.character), ~ gsub("\\n", "\n", .x, fixed=TRUE)))
}

COL <- c("Women"="#D62728","Men"="#1F77B4")
GROUP_COL <- c(COL,"Overall"="#777777")
PATHWAY_LEVELS <- c("Direct self-hiring","Internal pre-entry pathway",
                    "External pre-entry pathway","Institution-unresolved pre-entry pathway","Total self-hiring")
PATHS <- PATHWAY_LEVELS[1:3]
PATH3 <- PATHS
PATH_LABELS <- c(
  "Direct self-hiring"="Direct self-hire",
  "Internal pre-entry pathway"="Internal retention",
  "External pre-entry pathway"="External return",
  "Institution-unresolved pre-entry pathway"="Institution-unresolved pathway",
  "Total self-hiring"="Total self-hiring"
)
PATH_COL <- c("Direct self-hiring"="#45B5AA","Internal pre-entry pathway"="#F2A65A",
              "External pre-entry pathway"="#9B77D5","Institution-unresolved pre-entry pathway"="#999999",
              "Total self-hiring"="#222222")
SPEC_LEVELS <- c("Full-sample raw","Primary-sample raw","Primary-sample standardized")

# Short legend text avoids crowding in the two-column layout.
# Adjusted = primary eligible sample standardized for country and domain.
SPEC_LABELS <- c("Full-sample raw"="Full raw",
                 "Primary-sample raw"="Eligible raw",
                 "Primary-sample standardized"="Adjusted")
SPEC_COL <- c("Full-sample raw"="#7F7F7F","Primary-sample raw"="#4C78A8",
              "Primary-sample standardized"="#D62728")
SPEC_SHAPE <- c("Full-sample raw"=16,"Primary-sample raw"=17,
                "Primary-sample standardized"=15)
CUTS <- c(100,500,1500,2500,3500)

base_theme <- theme_classic(base_size=9) + theme(
  axis.title=element_text(size=9),axis.text=element_text(size=8,colour="#222222"),
  plot.title=element_text(size=11,face="bold",hjust=0),plot.title.position="plot",
  legend.position="bottom",legend.title=element_blank(),legend.text=element_text(size=8),
  plot.margin=margin(5,7,5,6))

COL_2E <- c("Women"="#D62728","Men"="#1F77B4")

COL_DIFF_2E <- "#4C9A9A"

# a. Pathway gaps across specifications.
ed2a <- read_source("ed2a.xlsx","ed2a") %>%
  mutate(specification=factor(specification,levels=SPEC_LEVELS))
rr <- range(c(0,ed2a$low,ed2a$high),na.rm=TRUE)
span <- max(diff(rr),.5)
label_x <- rr[2]+.40*span
x_lim <- c(rr[1]-.07*span,label_x+.07*span)
x_breaks <- pretty(rr,n=4)
x_breaks <- x_breaks[x_breaks>=rr[1] & x_breaks<=rr[2]]
P_ED2A <- ggplot(ed2a,aes(x=estimate,y=y,colour=specification)) +
  annotate("rect",xmin=-Inf,xmax=Inf,ymin=.60,ymax=1.40,
           fill="#F7F7F7",colour=NA) +
  geom_hline(yintercept=1.5,colour="grey85",linewidth=.35) +
  geom_vline(xintercept=0,linetype="dashed",colour="grey65",linewidth=.45) +
  geom_segment(aes(x=low,xend=high,yend=y),linewidth=.75,lineend="round") +
  geom_point(aes(shape=specification),size=2.7) +
  geom_text(aes(x=label_x,label=label),hjust=1,size=3,show.legend=FALSE) +
  scale_colour_manual(values=SPEC_COL,breaks=SPEC_LEVELS,
                      labels=SPEC_LABELS,name=NULL) +
  scale_shape_manual(values=SPEC_SHAPE,breaks=SPEC_LEVELS,
                     labels=SPEC_LABELS,name=NULL) +
  scale_y_continuous(
    breaks=5:1,
    labels=c("Direct self-hire","Internal retention",
             "External return","Unresolved","Total self-hiring"),
    limits=c(.5,5.5),expand=expansion(mult=0)
  ) +
  scale_x_continuous(
    limits=x_lim,breaks=x_breaks,expand=expansion(mult=0)
  ) +
  labs(
    title="a  Women–men differences in self-hiring\n    by pathway and specification",
    x="Women − men difference in self-hiring probability (pp)",y=NULL
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(size=12,face="bold",hjust=0),
    plot.title.position="plot",
    axis.text=element_text(size=10,colour="black"),
    axis.ticks.y=element_blank(),
    axis.line.y=element_blank(),
    legend.position="top",
    legend.justification="left",
    legend.text=element_text(size=10),
    plot.margin=margin(6,8,5,6)
  )

# b. Pathway composition by gender.
b_tab <- read_source("ed2b.xlsx","b_tab")
b_xmax <- max(60,ceiling((max(b_tab$high)+5)/10)*10)
P_ED2B <- ggplot(b_tab) +
  geom_segment(aes(x=low,xend=high,y=y,yend=y,colour=group),
               linewidth=.75) +
  geom_point(aes(x=pct,y=y,colour=group),size=2.7) +
  geom_text(aes(x=high+1,y=y,label=label,colour=group),
            hjust=0,size=3,show.legend=FALSE) +
  scale_colour_manual(values=GROUP_COL,
                      breaks=c("Women","Men","Overall"),name=NULL) +
  scale_y_continuous(
    breaks=3:1,
    labels=c("Direct self-hire","Internal retention","External return"),
    limits=c(.55,3.45),expand=expansion(mult=0)
  ) +
  scale_x_continuous(
    limits=c(0,b_xmax),
    breaks=seq(0,b_xmax,10),
    expand=expansion(mult=c(0,.02))
  ) +
  labs(
    title="b  Self-hiring pathway composition by gender",
    x="Share among pathway-identifiable self-hires (%)",y=NULL
  ) +
  guides(colour=guide_legend(nrow=1)) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(size=12,face="bold",hjust=0,margin=margin(b=4)),
    plot.title.position="plot",
    legend.position="top",
    legend.justification="center",
    legend.box.just="center",
    legend.text=element_text(size=10),
    legend.key.width=grid::unit(8,"mm"),
    legend.box.spacing=grid::unit(2,"pt"),
    legend.margin=margin(0,0,0,0),
    axis.title.x=element_text(size=10,margin=margin(t=6)),
    axis.text=element_text(size=10,colour="black"),
    axis.ticks.y=element_blank(),
    axis.line.y=element_blank(),
    axis.line.x=element_line(colour="grey75",linewidth=.35),
    plot.margin=margin(6,8,5,6)
  )

# c-e. Read the already estimated curves.
b_curve_plot <- read_source("ed2c.xlsx","top")
b_gap <- read_source("ed2c.xlsx","bottom")
c_curves <- read_source("ed2d.xlsx","curves") %>% mutate(pathway=factor(pathway,levels=PATHS))
e_gender_curves <- read_source("ed2e.xlsx","left")
e_curve <- read_source("ed2e.xlsx","right")
# Preserve the original c/d support; e has a narrower observed support.
RANK_MIN <- max(
  min(b_curve_plot$rank,na.rm=TRUE),
  min(c_curves$rank,na.rm=TRUE)
)

RANK_MAX <- min(
  max(b_curve_plot$rank,na.rm=TRUE),
  max(c_curves$rank,na.rm=TRUE)
)

if(!is.finite(RANK_MIN) || !is.finite(RANK_MAX) ||
   RANK_MIN<=0 || RANK_MIN>=RANK_MAX)
  stop("ED Fig.2c 和 2d 没有有效的共同排名区间")

RANK_BREAKS <- c(50,100,500,1000,4000)
RANK_BREAKS <- RANK_BREAKS[
  RANK_BREAKS>=RANK_MIN & RANK_BREAKS<=RANK_MAX
]

rank_scale_common <- scale_x_log10(
  breaks=RANK_BREAKS,
  labels=scales::label_comma(),
  expand=expansion(mult=c(0,0))
)

LINE_LEVELS <- c("Women","Men","Gap")
LINE_COL <- c(COL,"Gap"="#4C9A9A")
TOP_MAX <- max(20,ceiling(max(b_curve_plot$high,na.rm=TRUE)/20)*20)
GAP_LIM <- c(
  floor(min(c(0,b_gap$low),na.rm=TRUE)/2)*2,
  ceiling(max(c(0,b_gap$high),na.rm=TRUE)/2)*2
)

ED2C_LEGEND <- tibble(
  rank=c(RANK_MIN,RANK_MAX),
  estimate=c(-1,-1),
  gender=factor("Gap",levels=LINE_LEVELS)
)
P_ED2C_top <- ggplot(
  b_curve_plot,
  aes(x=rank,y=estimate,colour=gender,fill=gender)
) +
  geom_vline(
    xintercept=CUTS[CUTS>=RANK_MIN & CUTS<=RANK_MAX],
    linetype="dotted",colour="grey85",linewidth=0.3
  ) +
  geom_ribbon(
    aes(ymin=low,ymax=high),
    alpha=0.12,colour=NA,show.legend=FALSE
  ) +
  geom_line(linewidth=0.9) +
  geom_line(
    data=ED2C_LEGEND,
    aes(x=rank,y=estimate,colour=gender),
    inherit.aes=FALSE,
    linewidth=1.1,
    show.legend=TRUE
  ) +
  rank_scale_common +
  scale_colour_manual(
    values=LINE_COL,
    limits=LINE_LEVELS,
    breaks=LINE_LEVELS,
    labels=c("Women","Men","Women − men difference"),
    drop=FALSE,
    name=NULL
  ) +
  scale_fill_manual(
    values=LINE_COL[c("Women","Men")],
    guide="none"
  ) +
  scale_y_continuous(
    breaks=seq(0,TOP_MAX,by=20),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(RANK_MIN,RANK_MAX),
    ylim=c(0,TOP_MAX),
    expand=FALSE
  ) +
  labs(x=NULL,y="Adjusted self-hiring\nprobability (%)") +
  guides(
    colour=guide_legend(
      nrow=1,
      byrow=TRUE,
      override.aes=list(linewidth=c(0.9,0.9,1.1))
    )
  ) +
  base_theme +
  theme(
    legend.position="top",
    legend.justification="center",
    legend.direction="horizontal",
    legend.text=element_text(size=10),
    legend.key.width=grid::unit(1,"lines"),
    legend.spacing.x=grid::unit(2,"mm"),
    legend.box.spacing=grid::unit(2,"pt"),
    legend.margin=margin(0,0,0,0),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_text(size=10),
    plot.margin=margin(2,7,2,6)
  )
P_ED2C_bottom <- ggplot(b_gap,aes(x=rank,y=estimate)) +
  geom_hline(
    yintercept=0,linetype="dashed",
    colour="grey60",linewidth=0.45
  ) +
  geom_vline(
    xintercept=CUTS[CUTS>=RANK_MIN & CUTS<=RANK_MAX],
    linetype="dotted",colour="grey85",linewidth=0.3
  ) +
  geom_ribbon(
    aes(ymin=low,ymax=high),
    fill=LINE_COL["Gap"],
    alpha=0.18,colour=NA
  ) +
  geom_line(
    colour=LINE_COL["Gap"],
    linewidth=1.1
  ) +
  rank_scale_common +
  scale_y_continuous(
    breaks=seq(GAP_LIM[1],GAP_LIM[2],by=2),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(RANK_MIN,RANK_MAX),
    ylim=GAP_LIM,
    expand=FALSE
  ) +
  labs(
    x="Doctoral institution rank",
    y="Adjusted\nwomen − men\ndifference (pp)"
  ) +
  base_theme +
  theme(
    legend.position="none",
    axis.title.y=element_text(size=10),
    axis.title.x=element_text(size=10,margin=margin(t=6)),
    plot.margin=margin(5,7,5,6)
  )
P_ED2C <- (P_ED2C_top / P_ED2C_bottom) +
  plot_layout(heights=c(1.3,1),guides="keep") +
  plot_annotation(
    title="c  Self-hiring probability across doctoral\n    institution ranks, by gender",
    theme=theme(
      plot.title=element_text(size=12,face="bold",hjust=0),
      plot.title.position="plot"
    )
  )
P_ED2D <- ggplot(
  c_curves,
  aes(
    x=rank,
    y=estimate,
    colour=pathway,
    fill=pathway
  )
) +
  geom_ribbon(
    aes(ymin=low,ymax=high),
    alpha=0.10,
    colour=NA,
    show.legend=FALSE
  ) +
  geom_line(
    linewidth=0.9
  ) +
  rank_scale_common +
  scale_y_continuous(
    breaks=seq(0,80,10),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(RANK_MIN,RANK_MAX),
    ylim=c(0,80),
    expand=FALSE
  ) +
  scale_colour_manual(
    values=PATH_COL,
    breaks=PATHS,
    labels=unname(PATH_LABELS[PATHS]),
    name=NULL
  ) +
  scale_fill_manual(
    values=PATH_COL,
    guide="none"
  ) +
  labs(
    title="d  Self-hiring pathway composition across doctoral\n    institution ranks",
    x="Doctoral institution rank",
    y="Adjusted share\namong identifiable self-hires (%)"
  ) +
  guides(
    colour=guide_legend(nrow=1)
  ) +
  base_theme +
  theme(
    plot.title=element_text(
      size=12,
      face="bold",
      hjust=0
    ),
    plot.title.position="plot",
    legend.position="bottom",
    legend.justification="center",
    legend.box.just="center",
    legend.text=element_text(size=9),
    legend.key.width=grid::unit(9,"mm"),
    panel.spacing=grid::unit(0.8,"lines"),
    plot.margin=margin(5,7,5,6)
  )

# Panel e uses its own exported rank support, as in the original script.
RANK_MIN <- min(e_gender_curves$rank)
RANK_MAX <- max(e_gender_curves$rank)
RANK_BREAKS <- c(50,100,500,1000,4000)
RANK_BREAKS <- RANK_BREAKS[RANK_BREAKS>=RANK_MIN & RANK_BREAKS<=RANK_MAX]
rank_scale_common <- scale_x_log10(breaks=RANK_BREAKS,
  labels=scales::label_comma(),expand=expansion(mult=0))
e_equal <- distinct(e_gender_curves,rank) %>% mutate(external_log_rank=log10(rank))
P_ED2E1 <- ggplot(
  e_gender_curves,
  aes(x=rank,y=estimate,colour=gender,fill=gender)
) +
  geom_line(
    data=e_equal,
    aes(x=rank,y=external_log_rank),
    inherit.aes=FALSE,
    linetype="dashed",
    colour="grey65",
    linewidth=0.4
  ) +
  geom_ribbon(
    aes(ymin=low,ymax=high),
    alpha=0.12,
    colour=NA,
    show.legend=FALSE
  ) +
  geom_line(linewidth=0.85) +
  rank_scale_common +
  scale_y_continuous(
    labels=function(x)scales::label_comma()(10^x)
  ) +
  scale_colour_manual(
    values=COL_2E,
    breaks=c("Women","Men"),
    name=NULL
  ) +
  scale_fill_manual(
    values=COL_2E,
    guide="none"
  ) +
  coord_cartesian(
    xlim=c(RANK_MIN,RANK_MAX),
    expand=FALSE
  ) +
  labs(
    title="Adjusted external institution rank by gender",
    x="Doctoral institution rank",
    y="Adjusted external institution rank"
  ) +
  base_theme +
  theme(
    plot.title=element_text(
      size=10,
      face="bold",
      hjust=0.5
    ),
    axis.title=element_text(size=10),
    axis.text=element_text(size=10,colour="black"),
    legend.position="bottom",
    legend.direction="horizontal",
    legend.text=element_text(size=10),
    legend.key.width=grid::unit(1.2,"lines"),
    plot.margin=margin(5,7,5,6)
  )
P_ED2E2 <- ggplot(
  e_curve,
  aes(x=rank,y=estimate)
) +
  geom_hline(
    yintercept=0,
    linetype="dashed",
    colour="grey65",
    linewidth=0.4
  ) +
  geom_ribbon(
    aes(ymin=low,ymax=high),
    fill=COL_DIFF_2E,
    alpha=0.16
  ) +
  geom_line(
    colour=COL_DIFF_2E,
    linewidth=0.9
  ) +
  rank_scale_common +
  coord_cartesian(
    xlim=c(RANK_MIN,RANK_MAX),
    expand=FALSE
  ) +
  labs(
    title="Women–men differences in relative external rank",
    x="Doctoral institution rank",
    y="Women − men difference in\ndoctoral-to-external log rank ratio"
  ) +
  base_theme +
  theme(
    plot.title=element_text(
      size=10,
      face="bold",
      hjust=0.5
    ),
    axis.title=element_text(size=10),
    axis.text=element_text(size=10,colour="black"),
    plot.margin=margin(5,7,5,6)
  )
P_ED2E <- (P_ED2E1 | P_ED2E2) +
  plot_layout(widths=c(1.1,1)) +
  plot_annotation(
    title="e  Gender differences in external institution ranks across doctoral institution ranks\n    among external-return self-hires",
    theme=theme(
      plot.title=element_text(
        size=12,
        face="bold",
        hjust=0
      ),
      plot.title.position="plot"
    )
  )
P_EDFIG2 <- wrap_plots(
  A=wrap_elements(full=P_ED2A),
  B=wrap_elements(full=P_ED2B),
  C=wrap_elements(full=P_ED2C),
  D=wrap_elements(full=P_ED2D),
  E=wrap_elements(full=P_ED2E),
  design="AB\nCD\nEE",
  heights=c(1,1,1),
  widths=c(1.1,1.1)
)

print(P_EDFIG2)
