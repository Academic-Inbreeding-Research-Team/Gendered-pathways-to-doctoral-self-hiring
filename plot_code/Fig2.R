# Figure 2: plotting from published source data only.
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
if (!dir.exists(file.path(DATA_ROOT, "fig2&edfig2"))) DATA_ROOT <- ".."
DATA_DIR <- file.path(DATA_ROOT, "fig2&edfig2")
if (!dir.exists(DATA_DIR)) stop("Set DATA_ROOT to the directory containing the source-data folders.")
read_source <- function(file, sheet = 1) {
  data <- read_excel(file.path(DATA_DIR, file), sheet = sheet)
  # Excel labels may store a literal backslash-n instead of a line break.
  data %>% mutate(across(where(is.character), ~ gsub("\\n", "\n", .x, fixed=TRUE)))
}

PATHS <- c("Direct self-hire","Internal retention","External return","Institution-unresolved pathway")
ALL_PATHS <- c(PATHS,"Total self-hiring")
COL_PATH <- c("Direct self-hire"="#45B5AA","Internal retention"="#F2A65A",
              "External return"="#9B77D5","Institution-unresolved pathway"="#999999",
              "Total self-hiring"="#222222")
COL_LABELS <- c("Direct self-hire"="Direct","Internal retention"="Internal",
                "External return"="External return","Institution-unresolved pathway"="Unresolved",
                "Total self-hiring"="Total self-hiring")

# Harmonize source-data pathway names with the original panel-a labels.
pathway_a <- function(x) recode(as.character(x),
  "Direct self-hiring"="Direct self-hire",
  "Internal pre-entry pathway"="Internal retention",
  "External pre-entry pathway"="External return",
  "Institution-unresolved pre-entry pathway"="Institution-unresolved pathway")
comp_plot <- read_source("2a.xlsx","top") %>%
  mutate(pathway=pathway_a(pathway),inside=as.logical(inside))
adjusted <- read_source("2a.xlsx","bottom") %>% mutate(pathway=pathway_a(pathway))
P_2A_top <- ggplot(comp_plot) +
  geom_rect(aes(xmin=xmin,xmax=xmax,ymin=.23,ymax=.69,fill=pathway),
            colour="white",linewidth=.35) +
  geom_segment(data=filter(comp_plot,!inside),
               aes(x=xmid,xend=xmid,y=.70,yend=.78),colour="grey55",linewidth=.35) +
  geom_text(aes(x=xmid,y=label_y,label=label,colour=label_col,hjust=label_hjust),
            fontface="bold",size=3.2,show.legend=FALSE) +
  scale_colour_identity(guide="none") +
  scale_fill_manual(values=COL_PATH[PATHS],breaks=PATHS,
                    labels=unname(COL_LABELS[PATHS]),name=NULL,drop=FALSE) +
  coord_cartesian(xlim=c(0,100),ylim=c(.18,.98),expand=FALSE) +
  labs(title="Self-hiring pathway composition") +
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  theme_void(base_size=9) +
  theme(plot.title=element_text(size=10,face="bold",hjust=.5,margin=margin(b=5)),
        plot.title.position="plot",legend.position="bottom",legend.justification="center",
        legend.box.spacing=grid::unit(0,"pt"),legend.margin=margin(0,0,0,0),
        legend.text=element_text(size=8),legend.key.size=grid::unit(3.2,"mm"),
        legend.spacing.x=grid::unit(1.5,"mm"),plot.margin=margin(3,3,8,3))
gap_span <- diff(range(c(0,adjusted$low,adjusted$high)))
gap_plot <- adjusted %>% mutate(pathway=factor(pathway,levels=rev(ALL_PATHS)),
                                col=unname(COL_PATH[as.character(pathway)]),
                                label=sprintf("%+.2f [%.2f, %.2f]",estimate,low,high),
                                label_x=high+.05*gap_span)


x_right <- max(gap_plot$high)+max(1.05,.60*gap_span)
x_left <- min(0,gap_plot$low)-.06*gap_span
x_breaks <- seq(floor(min(0,gap_plot$low)*2)/2,
                ceiling(max(gap_plot$high)*2)/2,by=.5)
P_2A_bottom <- ggplot(gap_plot,aes(y=pathway)) +
  geom_vline(xintercept=0,linetype="dashed",colour="grey70",linewidth=.45) +
  geom_hline(yintercept=1.5,colour="grey88",linewidth=.35) +
  geom_segment(aes(x=low,xend=high,yend=pathway,colour=col),linewidth=.7) +
  geom_point(aes(x=estimate,colour=col),size=2.7) +
  geom_text(aes(x=label_x,label=label),hjust=0,size=2.8,colour="#333333") +
  scale_colour_identity(guide="none") +
  scale_y_discrete(limits=rev(ALL_PATHS),labels=unname(COL_LABELS[rev(ALL_PATHS)]),
                   expand=expansion(add=.55),drop=FALSE) +
  scale_x_continuous(limits=c(x_left,x_right),breaks=x_breaks,
                     expand=expansion(mult=0)) +
  labs(title="Contributions to the women–men difference in self-hiring",
       x="Women − men difference in self-hiring (pp)",y=NULL) +
  theme_classic(base_size=9) +
  theme(plot.title=element_text(size=10,face="bold",hjust=.5,margin=margin(b=6)),
        plot.title.position="plot",axis.title.x=element_text(size=9,margin=margin(t=6)),
        axis.text=element_text(size=8,colour="#333333"),axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),axis.line.x=element_line(colour="grey75",linewidth=.35),
        plot.margin=margin(6,3,3,3))
P_2A <- (patchwork::free(P_2A_top,type="panel",side="l") / P_2A_bottom) +
  plot_layout(heights=c(.35,.65)) +
  plot_annotation(title="a  Decomposition of women–men self-hiring differences\n    by pathway",
                  theme=theme(plot.title=element_text(size=11.5,face="bold",hjust=0),
                              plot.title.position="plot"))

# b. Overall self-hiring probability and pathway composition.
HIER_LEVELS <- c("Top 100","101–500","501–1,500","1,501–2,500","2,501–3,500",">3,500")
STACK_LEVELS <- c("Direct self-hiring","Internal pre-entry pathway",
                  "External pre-entry pathway","Institution-unresolved pre-entry pathway")
PATH_COLORS <- setNames(c("#45B5AA","#F2A65A","#9B77D5","#999999"),STACK_LEVELS)
b_total <- read_source("2b.xlsx","Self_hiring_probability") %>%
  rename(total_pct=`Self-hiring rate (%)`)
b_comp <- read_source("2b.xlsx","Pathway_composition") %>%
  mutate(hierarchy=factor(hierarchy,levels=HIER_LEVELS),
         pathway=factor(pathway,levels=STACK_LEVELS),x=as.numeric(hierarchy)) %>%
  arrange(hierarchy,pathway)
COMP_BOTTOM <- 0
COMP_TOP <- 45
SELF_BOTTOM <- 52
SELF_TOP <- 79
PLOT_TOP <- 87
SELF_MAX <- max(
  40,
  ceiling((max(b_total$total_pct,na.rm=TRUE)+5)/10)*10
)

b_comp_plot <- b_comp %>%
  group_by(hierarchy) %>%
  mutate(
    cum_pct=cumsum(comp_pct),
    ymin_pct=lag(cum_pct,default=0),
    ymax_pct=cum_pct,
    ymin=COMP_BOTTOM+ymin_pct/100*(COMP_TOP-COMP_BOTTOM),
    ymax=COMP_BOTTOM+ymax_pct/100*(COMP_TOP-COMP_BOTTOM),
    ymid=(ymin+ymax)/2
  ) %>%
  ungroup() %>%
  mutate(
    xmin=x-0.33,
    xmax=x+0.33,
    label=if_else(
      pathway!="Institution-unresolved pre-entry pathway" &
        comp_pct>=6,
      sprintf("%.1f%%",comp_pct),
      ""
    ),
    text_col=if_else(
      pathway%in%c(
        "External pre-entry pathway",
        "Direct self-hiring"
      ),
      "white",
      "#252525"
    )
  )

b_total_plot <- b_total %>%
  mutate(
    y=SELF_BOTTOM+
      total_pct/SELF_MAX*(SELF_TOP-SELF_BOTTOM)
  )

comp_ticks <- tibble(value=c(0,50,100)) %>%
  mutate(
    y=COMP_BOTTOM+
      value/100*(COMP_TOP-COMP_BOTTOM)
  )

self_ticks <- tibble(
  value=seq(0,SELF_MAX,by=10)
) %>%
  mutate(
    y=SELF_BOTTOM+
      value/SELF_MAX*(SELF_TOP-SELF_BOTTOM)
  )

HIER_LABELS <- c(
  "Top 100",
  "101–500",
  "501–\n1,500",
  "1,501–\n2,500",
  "2,501–\n3,500",
  ">3,500"
)
PATH_LABELS <- c(
  "Direct self-hiring"="Direct self-hire",
  "Internal pre-entry pathway"="Internal retention",
  "External pre-entry pathway"="External return",
  "Institution-unresolved pre-entry pathway"="Institution-unresolved pathway"
)
P_2B <- ggplot() +
  
  # Lower panel: pathway composition
  geom_rect(
    data=b_comp_plot,
    aes(
      xmin=xmin,xmax=xmax,
      ymin=ymin,ymax=ymax,
      fill=pathway
    ),
    colour="white",
    linewidth=0.45
  ) +
  
  geom_text(
    data=filter(b_comp_plot,label!=""),
    aes(
      x=x,y=ymid,
      label=label,
      colour=text_col
    ),
    size=3,
    fontface="plain",
    show.legend=FALSE
  ) +
  
  scale_colour_identity() +
  
  # Upper panel: overall self-hire probability
  geom_line(
    data=b_total_plot,
    aes(x=x,y=y,group=1),
    colour="#303030",
    linewidth=0.75,
    lineend="round"
  ) +
  
  geom_point(
    data=b_total_plot,
    aes(x=x,y=y),
    shape=21,
    size=3,
    stroke=0.7,
    fill="white",
    colour="#303030"
  ) +
  
  geom_text(
    data=b_total_plot,
    aes(
      x=x,y=y,
      label=sprintf("%.1f",total_pct)
    ),
    nudge_y=4,
    size=3,
    colour="#303030"
  ) +
  
  # Subtle separator
  annotate(
    "segment",
    x=0.60,xend=6.40,
    y=48.5,yend=48.5,
    linewidth=0.3,
    colour="#E0E0E0"
  ) +
  
  # Lower percentage axis
  annotate(
    "segment",
    x=0.57,xend=0.57,
    y=COMP_BOTTOM,yend=COMP_TOP,
    linewidth=0.35,
    colour="#444444"
  ) +
  
  geom_segment(
    data=comp_ticks,
    aes(
      x=0.53,xend=0.57,
      y=y,yend=y
    ),
    linewidth=0.35,
    colour="#444444"
  ) +
  
  geom_text(
    data=comp_ticks,
    aes(x=0.48,y=y,label=value),
    hjust=1,
    size=3,
    colour="#444444"
  ) +
  
  # Upper percentage axis
  annotate(
    "segment",
    x=0.57,xend=0.57,
    y=SELF_BOTTOM,yend=SELF_TOP,
    linewidth=0.35,
    colour="#444444"
  ) +
  
  geom_segment(
    data=self_ticks,
    aes(
      x=0.53,xend=0.57,
      y=y,yend=y
    ),
    linewidth=0.35,
    colour="#444444"
  ) +
  
  geom_text(
    data=self_ticks,
    aes(x=0.48,y=y,label=value),
    hjust=1,
    size=3,
    colour="#444444"
  ) +
  
  # Section labels
  annotate(
    "text",
    x=0.78,
    y=SELF_TOP+5,
    label="Observed self-hiring (%)",
    hjust=0,
    vjust=1,
    size=3,
    fontface="plain",
    colour="#303030"
  ) +
  
  annotate(
    "text",
    x=0.05,
    y=(COMP_BOTTOM+COMP_TOP)/2,
    label="Pathway composition\namong self-hires (%)",
    angle=90,
    size=3,
    lineheight=0.95,
    colour="#303030"
  ) +
  
  # Bottom axis
  annotate(
    "segment",
    x=0.57,xend=6.42,
    y=0,yend=0,
    linewidth=0.35,
    colour="#444444"
  ) +
  
  scale_x_continuous(
    breaks=1:6,
    labels=HIER_LABELS,
    limits=c(0.05,6.43),
    expand=c(0,0)
  ) +
  
  scale_y_continuous(
    limits=c(0,PLOT_TOP),
    breaks=NULL,
    expand=c(0,0)
  ) +
  
  scale_fill_manual(
    values=PATH_COLORS,
    breaks=STACK_LEVELS,
    labels=unname(PATH_LABELS[STACK_LEVELS]),
    name=NULL
  ) +
  coord_cartesian(clip="off") +
  
  labs(
    title="b  Self-hiring probability and pathway composition\n    across doctoral institutional ranks",
    x="Doctoral-institution hierarchy",
    y=NULL
  ) +
  
  guides(
    fill=guide_legend(
      nrow=2,
      byrow=TRUE,
      override.aes=list(colour=NA)
    )
  ) +
  
  theme_classic(base_size=9) +
  
  theme(
    axis.line=element_blank(),
    axis.ticks.x=element_line(
      colour="#444444",
      linewidth=0.35
    ),
    axis.ticks.y=element_blank(),
    axis.ticks.length=grid::unit(3,"pt"),
    
    axis.text.x=element_text(
      size=9,
      colour="#303030",
      lineheight=0.88,
      margin=margin(t=5)
    ),
    axis.text.y=element_blank(),
    
    axis.title.x=element_text(
      size=9,
      colour="#303030",
      margin=margin(t=8)
    ),
    axis.title.y=element_blank(),
    
    legend.position="bottom",
    legend.direction="horizontal",
    legend.justification="center",
    legend.box.just="center",
    legend.text=element_text(size=9),
    legend.key.height=grid::unit(9,"pt"),
    legend.key.width=grid::unit(11,"pt"),
    legend.spacing.x=grid::unit(9,"pt"),
    legend.spacing.y=grid::unit(9,"pt"),
    legend.margin=margin(t=5),
    
    plot.title=element_text(
      size=11.5,
      face="bold",
      hjust=0
    ),
    plot.title.position="plot",
    plot.margin=margin(t=5,r=10,b=3,l=16)
  )
  

# c. Pathway-specific gender gaps.
PATH_2C <- c(
  "Direct self-hiring",
  "Internal pre-entry pathway",
  "External pre-entry pathway"
)

PATH_COL_2C <- c(
  "Direct self-hiring"="#45B5AA",
  "Internal pre-entry pathway"="#F2A65A",
  "External pre-entry pathway"="#9B77D5"
)

PATH_LABEL_2C <- c(
  "Direct self-hiring"="Direct self-hire",
  "Internal pre-entry pathway"="Internal retention",
  "External pre-entry pathway"="External return"
)

d_plot <- read_source("2c.xlsx","fig2c")
d_plot_2c <- d_plot %>%
  mutate(
    y_center=round(y_plot),
    y_plot=y_center+(y_plot-y_center)*1.6
  )

# Symmetric x-axis limits
X_LIM_2C <- max(
  2,
  ceiling(max(abs(c(
    d_plot_2c$conf_low_pp,
    d_plot_2c$conf_high_pp
  )),na.rm=TRUE)/2)*2
)

# Institutional hierarchy labels
HIER_LABELS_2C <- c(
  "Top 100",
  "101–500",
  "501–1,500",
  "1,501–2,500",
  "2,501–3,500",
  ">3,500"
)
P_2C <- ggplot(
  d_plot_2c,
  aes(x=difference_pp,y=y_plot,colour=pathway)
) +
  geom_hline(
    yintercept=seq(
      1.5,
      length(HIER_LEVELS)-0.5,
      by=1
    ),
    colour="grey93",
    linewidth=0.35
  ) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    colour="grey60",
    linewidth=0.4
  ) +
  geom_segment(
    aes(
      x=conf_low_pp,
      xend=conf_high_pp,
      yend=y_plot
    ),
    linewidth=0.65,
    lineend="round"
  ) +
  geom_point(
    size=2.5
  ) +
  scale_colour_manual(
    values=PATH_COL_2C,
    breaks=PATH_2C,
    labels=PATH_LABEL_2C,
    name=NULL,
    guide=guide_legend(
      nrow=1,
      byrow=TRUE
    )
  ) +
  scale_x_continuous(
    breaks=pretty(
      c(-X_LIM_2C,X_LIM_2C),
      n=5
    ),
    labels=function(x)ifelse(
      x>0,
      paste0("+",x),
      as.character(x)
    ),
    expand=expansion(mult=c(0.02,0.02))
  ) +
  scale_y_continuous(
    breaks=rev(seq_along(HIER_LEVELS)),
    labels=HIER_LABELS_2C,
    limits=c(0.4,length(HIER_LEVELS)+0.6),
    expand=c(0,0)
  ) +
  coord_cartesian(
    xlim=c(-X_LIM_2C,X_LIM_2C),
    clip="off"
  ) +
  labs(
    title="c  Gender differences in self-hiring pathways\n    by doctoral institutional rank",
    x="Women − men difference in pathway share (pp)",
    y=NULL
  ) +
  theme_classic(base_size=10,base_family="sans") +
  theme(
    plot.title=element_text(
      face="bold",
      size=11.5,
      hjust=0,
      margin=margin(b=10)
    ),
    plot.title.position="plot",
    axis.title.x=element_text(
      size=9.5,
      margin=margin(t=7)
    ),
    axis.text.x=element_text(
      size=8.5,
      colour="black"
    ),
    axis.text.y=element_text(
      size=9,
      colour="black",
      margin=margin(r=7)
    ),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.line.x=element_line(
      colour="black",
      linewidth=0.4
    ),
    axis.ticks.x=element_line(
      colour="black",
      linewidth=0.35
    ),
    axis.ticks.length=grid::unit(2,"pt"),
    panel.grid=element_blank(),
    legend.position="bottom",
    legend.justification="center",
    legend.direction="horizontal",
    legend.text=element_text(size=8.5),
    legend.key.width=grid::unit(0.8,"cm"),
    legend.key.height=grid::unit(0.35,"cm"),
    legend.spacing.x=grid::unit(0.15,"cm"),
    legend.margin=margin(t=5),
    plot.margin=margin(7,10,5,7)
  )

# d. Exported density coordinates (no repeated density estimation).
fig2e_plot <- read_source("2d.xlsx","density_curves") %>%
  mutate(gender_final=factor(recode(gender,"Women"="Female","Men"="Male"),
                            levels=c("Female","Male")))
female_col <- "#D62728"
male_col <- "#1F77B4"
text_col <- "#183153"
axis_col <- "#505A66"
grid_col <- "#E6EBF0"

# Original annotation positions were session objects, not exported.
arrow_y <- max(fig2e_plot$density)*0.90
label_y <- max(fig2e_plot$density)*0.98
P_2E <- ggplot(
  fig2e_plot,
  aes(
    x=log_rank_movement,
    colour=gender_final,
    fill=gender_final
  )
) +
  geom_area(aes(y=density),stat="identity",position="identity",
            alpha=0.08,colour=NA,show.legend=FALSE) +
  geom_line(aes(y=density),linewidth=0.95,key_glyph="path") +
  scale_colour_manual(
    values=c("Female"=female_col,"Male"=male_col),
    breaks=c("Female","Male"),
    labels=c("Women","Men"),
    name=NULL
  ) +
  scale_fill_manual(
    values=c("Female"=female_col,"Male"=male_col),
    guide="none"
  ) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    linewidth=0.4,
    colour="#777777"
  ) +
  annotate(
    "segment",
    x=-0.55,xend=-1.35,
    y=arrow_y,yend=arrow_y,
    colour=text_col,
    linewidth=0.4,
    arrow=grid::arrow(
      length=grid::unit(0.09,"cm"),
      type="closed"
    )
  ) +
  annotate(
    "text",
    x=-1.05,y=label_y,
    label="External lower\nrank",
    size=3,
    colour=text_col,
    lineheight=0.95
  ) +
  annotate(
    "segment",
    x=0.55,xend=1.35,
    y=arrow_y,yend=arrow_y,
    colour=text_col,
    linewidth=0.4,
    arrow=grid::arrow(
      length=grid::unit(0.09,"cm"),
      type="closed"
    )
  ) +
  annotate(
    "text",
    x=1.05,y=label_y,
    label="External higher\nrank",
    size=3,
    colour=text_col,
    lineheight=0.95
  ) +
  labs(
    title="d  Relative institutional ranks during external employment\n    among external-return self-hires",
    x="Log-transformed ratio of doctoral to external institution rank",
    y="Density"
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(face="bold",size=11.5,hjust=0),
    plot.title.position="plot",
    axis.title=element_text(size=9.5),
    axis.text=element_text(size=8.5,colour=axis_col),
    axis.line=element_line(colour="black",linewidth=0.4),
    axis.ticks=element_line(colour="black",linewidth=0.35),
    legend.position="top",
    legend.direction="horizontal",
    legend.text=element_text(size=9),
    legend.key.width=grid::unit(1.3,"lines"),
    panel.grid=element_blank(),
    plot.margin=margin(8,10,8,8)
  )
P_FIG2 <- (
  patchwork::wrap_elements(full=P_2A) |
    patchwork::wrap_elements(full=P_2B)
) / (
  patchwork::wrap_elements(full=P_2C) |
    patchwork::wrap_elements(full=P_2E)
) +
  plot_layout(
    widths=c(1,1),
    heights=c(1,1)
  )

print(P_FIG2)
