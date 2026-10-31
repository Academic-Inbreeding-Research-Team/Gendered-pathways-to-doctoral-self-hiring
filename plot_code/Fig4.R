
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(grid)

DATA_ROOT <- "."
if (!dir.exists(file.path(DATA_ROOT, "fig4&edfig3$edtable"))) DATA_ROOT <- ".."
DATA_DIR <- file.path(DATA_ROOT, "fig4&edfig3$edtable")
if (!dir.exists(DATA_DIR)) stop("Set DATA_ROOT to the directory containing the source-data folders.")
read_source <- function(file, sheet = 1) {
  data <- read_excel(file.path(DATA_DIR, file), sheet = sheet)
  # Excel labels may store a literal backslash-n instead of a line break.
  data %>% mutate(across(where(is.character), ~ gsub("\\n", "\n", .x, fixed=TRUE)))
}

COL_GENDER <- c("Women"="#D62728","Men"="#1F77B4")

COL_DIFF <- "#4C9A9A"

ROUTE_LEVELS <- c("Direct self-hire","Internal retention","External return")

STEP_D <- c(
  "Within-destination comparison sample",
  "Rank- and Career-stage-eligible sample",
  "only + Comparable institutional rank",
  "only + Career-stage adjustment"
)

# Source mapping: 4a -> a; 4b -> b; 4c/Curves -> c;
# 4d/Results -> d; 4e/Results -> e. Titles checked against Fig4.pdf.

# a. Scholarly-record differences by pathway and gender.
fig4a_results <- read_source("4a.xlsx")
fig4a_plot_data <- fig4a_results %>%
  mutate(route=factor(route,levels=rev(ROUTE_LEVELS)),
         gender=factor(gender,levels=c("Women","Men")),
         y=as.numeric(route)+if_else(gender=="Women",.16,-.16))


fig4a_pub <- filter(fig4a_plot_data,outcome=="Publications")
fig4a_jif <- filter(fig4a_plot_data,outcome %in% c("Mean JIF","JIF per publication"))
P_4A_pub <- ggplot(fig4a_pub,aes(estimate,y,colour=gender))+
  geom_vline(xintercept=0,linetype="dashed",colour="grey65",linewidth=.45)+
  geom_segment(aes(x=low,xend=high,yend=y),linewidth=.77)+geom_point(size=2.6)+
  scale_y_continuous(breaks=1:3,labels=rev(ROUTE_LEVELS),limits=c(.5,3.5))+
  scale_colour_manual(values=COL_GENDER)+
  scale_x_continuous(breaks=seq(-3,0.5,1)) +
  coord_cartesian(xlim=c(-3,0.5))+
  labs(title="Three-year pre-entry publication count",
       x="Adjusted difference (self-hire − non-self-hire)",
       y=NULL,colour=NULL)+
  theme_classic(base_size=9)+
  theme(axis.line.y=element_blank(),axis.ticks.y=element_blank(),
        axis.text.y=element_text(size=9),legend.position="top",
        plot.title=element_text(face="bold",size=10))


P_4A_jif <- ggplot(fig4a_jif,aes(estimate,y,colour=gender)) +
  geom_vline(xintercept=0,linetype="dashed",colour="grey65",linewidth=.45) +
  geom_segment(aes(x=low,xend=high,yend=y),linewidth=.77) +
  geom_point(size=2.6) +
  scale_y_continuous(breaks=1:3,labels=rev(ROUTE_LEVELS),limits=c(.5,3.5)) +
  scale_colour_manual(values=COL_GENDER) +
  scale_x_continuous(breaks=seq(-1,0.5,0.5)) +
  coord_cartesian(xlim=c(-1,0.5))+
  labs(title="Three-year pre-entry JIF per publication",
       x="Adjusted difference (self-hire − non-self-hire)",
       y=NULL,colour=NULL) +
  theme_classic(base_size=9) +
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(size=9),
    legend.position="top",
    plot.title=element_text(face="bold",size=10)
  )

P_4A_pub <- P_4A_pub +
  theme(
    plot.title=element_text(face="bold",size=10,hjust=.5),
    plot.title.position="panel"
  )

P_4A_jif <- P_4A_jif +
  theme(
    plot.title=element_text(face="bold",size=10,hjust=.5),
    plot.title.position="panel"
  )

P_4A <- (P_4A_pub|P_4A_jif)+plot_layout(widths=c(1,1),guides="collect")+
  plot_annotation(title="a  Scholarly differences before first faculty entry: self-hiring pathways versus non-self-hires",
                  theme=theme(plot.title=element_text(face="bold",size=12))) &
  theme(legend.position="top")

# b. Extensive and intensive margins.
fig4b_results <- read_source("4b.xlsx")
fig4b_plot_data <- fig4b_results %>%
  mutate(
    route=factor(route,levels=rev(ROUTE_LEVELS)),
    gender=factor(gender,levels=c("Women","Men")),
    y=as.numeric(route)+if_else(gender=="Women",.16,-.16)
  )

fig4b_zero <- filter(fig4b_plot_data,margin=="Zero recorded publications")
fig4b_positive <- filter(fig4b_plot_data,margin=="Positive-output publications")
P_4B_zero <- ggplot(
  fig4b_zero,
  aes(x=route,y=estimate,colour=gender)
) +
  geom_hline(
    yintercept=0,linetype="dashed",
    colour="grey70",linewidth=.45
  ) +
  geom_errorbar(
    aes(ymin=low,ymax=high),
    position=position_dodge(width=.45),
    width=.12,linewidth=.75
  ) +
  geom_point(
    position=position_dodge(width=.45),
    size=2.8
  ) +
  scale_colour_manual(
    values=COL_GENDER,
    breaks=c("Women","Men"),name=NULL
  ) +
  # scale_x_discrete(
  #   limits=ROUTE_LEVELS,
  #   labels=c("Direct self-hire"="Direct",
  #            "Internal retention"="Internal",
  #            "External return"="External return")
  # ) +
  scale_y_continuous(
    breaks=seq(-3,12,3),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(ylim=c(-3.2,12)) +
  labs(
    title="Three-year pre-entry zero-publication probability\namong all scholars",
    x=NULL,
    y="Adjusted difference\n(self-hire − non-self-hire; pp)"
  ) +
  theme_classic(base_size=9) +
  theme(
    plot.title=element_text(size=10,face="bold",hjust=.5),
    plot.title.position="panel",
    axis.text=element_text(size=8.5,colour="black"),
    axis.title.y=element_text(size=9),
    axis.ticks.x=element_blank(),
    legend.position="top",
    plot.margin=margin(5,5,5,5)
  )

# ================================================================
# Fig. 4b | Intensive margin
# ================================================================

P_4B_positive <- ggplot(
  fig4b_positive,
  aes(x=route,y=estimate,colour=gender)
) +
  geom_hline(
    yintercept=0,linetype="dashed",
    colour="grey70",linewidth=.45
  ) +
  geom_errorbar(
    aes(ymin=low,ymax=high),
    position=position_dodge(width=.45),
    width=.12,linewidth=.75
  ) +
  geom_point(
    position=position_dodge(width=.45),
    size=2.8
  ) +
  scale_colour_manual(
    values=COL_GENDER,
    breaks=c("Women","Men"),name=NULL
  ) +
  # scale_x_discrete(
  #   limits=ROUTE_LEVELS,
  #   labels=c("Direct self-hire"="Direct",
  #            "Internal retention"="Internal",
  #            "External return"="External return")
  # ) +
  scale_y_continuous(
    breaks=seq(-4,2,1),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(ylim=c(-4.2,2)) +
  labs(
    title="Three-year pre-entry publication count\namong scholars with publications",
    x=NULL,
    y="Adjusted count difference\n(self-hire − non-self-hire)"
  ) +
  theme_classic(base_size=9) +
  theme(
    plot.title=element_text(size=10,face="bold",hjust=.5),
    plot.title.position="panel",
    axis.text=element_text(size=8.5,colour="black"),
    axis.title.y=element_text(size=9),
    axis.ticks.x=element_blank(),
    legend.position="top",
    plot.margin=margin(5,5,5,5)
  )

# ================================================================
# Fig. 4b | Combine
# ================================================================

P_4B <- (P_4B_zero | P_4B_positive) +
  plot_layout(widths=c(1,1),guides="collect") +
  plot_annotation(
    title="b  Pre-entry differences in zero-publication probability and publication counts",
    theme=theme(
      plot.title=element_text(size=12,face="bold",hjust=0),
      plot.title.position="plot"
    )
  ) &
  theme(
    legend.position="top",
    # axis.text.x=element_text(angle=10,hjust=0.5,vjust=0.5)
  )

# c. Prestige gradients. Curves already contain estimates and confidence limits.
curves <- read_source("4c.xlsx","Curves")
fig4c_pub_curve <- filter(curves,outcome=="Publications")
fig4c_jif_curve <- filter(curves,outcome %in% c("Mean JIF","JIF per publication"))
d_min <- min(curves$prestige_z)
d_max <- max(curves$prestige_z)
fig4c_breaks <- seq(
  ceiling(d_min*2)/2,floor(d_max*2)/2,by=.5
)

plot_c_curve <- function(dat,title){
  ggplot(
    dat,aes(prestige_z,estimate,
            colour=gender,fill=gender,linetype=gender)
  )+
    geom_hline(yintercept=0,linetype="dashed",
               colour="grey65",linewidth=.45)+
    geom_ribbon(aes(ymin=low,ymax=high),
                alpha=.12,colour=NA,show.legend=FALSE)+
    geom_line(linewidth=.9)+
    scale_colour_manual(
      values=COL_GENDER,breaks=c("Women","Men"),name=NULL
    )+
    scale_fill_manual(values=COL_GENDER,guide="none")+
    scale_linetype_manual(
      values=c("Women"="22","Men"="solid"),guide="none"
    )+
    scale_x_continuous(
      limits=c(d_min,d_max),
      breaks=fig4c_breaks,
      labels=function(x) ifelse(
        abs(x)<1e-10,"Mean",sprintf("%+.1f SD",x)
      )
    )+
    coord_cartesian(ylim=c(-2.5,1))+
    labs(
      title=title,
      x="First-faculty institution prestige\n(standardized SpringRank; higher = more prestigious)",
      y="Adjusted difference\n(Self-hire − non-self-hire)"
    )+
    theme_classic(base_size=9)+
    theme(
      legend.position="top",
      plot.title=element_text(face="bold",size=10,hjust=.5),
      axis.text=element_text(size=8,colour="black")
    )
}

# Publications
P_4C_pub <- plot_c_curve(
  fig4c_pub_curve,
  "Three-year pre-entry publications"
) +
  coord_cartesian(ylim=c(-2.5,1)) +
  scale_y_continuous(breaks=seq(-2.5,1,by=1))

# JIF per publication
P_4C_jif <- plot_c_curve(
  fig4c_jif_curve,
  "Three-year pre-entry JIF per publication"
) +
  coord_cartesian(ylim=c(-0.8,0.4)) +
  scale_y_continuous(breaks=seq(-0.8,0.4,by=0.3))


P_4C <- (P_4C_pub | P_4C_jif)+
  plot_layout(widths=c(1,1),guides="collect")+
  plot_annotation(
    title="c  Scholarly differences before first faculty entry across institutional prestige",
    theme=theme(plot.title=element_text(face="bold",size=12))
  ) &
  theme(legend.position="top")

# d. Comparison conditions.
fig4d_results <- read_source("4d.xlsx","Results")
P_4D <- ggplot(
  fig4d_results,aes(x=estimate,y=y)
)+
  annotate(
    "rect",xmin=-Inf,xmax=Inf,
    ymin=.5,ymax=2.5,
    fill="grey96",colour=NA
  )+
  geom_hline(
    yintercept=2.5,colour="grey85",linewidth=.4
  )+
  geom_vline(
    xintercept=0,linetype="dashed",
    colour="grey60",linewidth=.45
  )+
  geom_segment(
    aes(x=low,xend=high,yend=y),
    linewidth=.85,colour=COL_DIFF
  )+
  geom_point(size=2.8,colour=COL_DIFF)+
  geom_text(
    aes(x=high+.06,label=estimate_label),
    hjust=0,size=2.9,colour=COL_DIFF
  )+
  scale_y_continuous(
    breaks=4:1,
    labels=STEP_D,
    limits=c(.5,4.5),
    expand=expansion(mult=0)
  )+
  coord_cartesian(
    xlim=c(
      min(fig4d_results$low)-.2,
      max(0,fig4d_results$high)+.45
    )
  )+
  labs(
    title="d  Publication count differences before first faculty entry\n    across comparison conditions",
    x="Adjusted difference (Self-hire − non-self-hire)",
    y=NULL
  )+
  theme_classic(base_size=9)+
  theme(
    plot.title=element_text(face="bold",size=11.5,hjust=0),
    plot.title.position="plot",
    axis.text.y=element_text(size=9.5,colour="black"),
    axis.text.x=element_text(size=9,colour="black"),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank()
  )

# e. Gender gaps after adjustment for scholarly records.
gap_results <- read_source("4e.xlsx","Results")
gap_plot <- gap_results %>%
  mutate(
    y_pos=5:1,
    row_lab=c(
      "Country and domain adjusted",
      "Country and domain adjusted",
      "+ Pre-entry publications",
      "+ Pre-entry JIF per publication",
      "+ Publications and JIF"
    ),
    label=sprintf("%+.2f",estimate_pp)
  )
P_4E <- ggplot(gap_plot,aes(x=estimate_pp,y=y_pos))+
  geom_vline(
    xintercept=0,linetype="dashed",
    colour="grey65",linewidth=.45
  )+
  geom_hline(
    yintercept=4.5,linetype="dashed",
    colour="grey65",linewidth=.45
  )+
  geom_segment(
    aes(x=conf_low_pp,xend=conf_high_pp,yend=y_pos),
    linewidth=.75,colour=COL_DIFF
  )+
  geom_point(size=3,colour=COL_DIFF)+
  geom_text(
    aes(x=conf_high_pp+.08,label=label),
    hjust=0,size=3,colour=COL_DIFF
  )+
  annotate(
    "text",x=-Inf,y=5.35,
    label=paste0(
      "Pre-entry analysis-eligible sample (n = ",
      format(gap_results$`Analytical sample (n)`[1],big.mark=",",scientific=FALSE),
      ")"
    ),
    hjust=0,vjust=0,
    size=2.8,fontface="bold",colour="grey30"
  )+
  annotate(
    "text",x=-Inf,y=4.35,
    label=paste0(
      "Common scholarly-record sample (n = ",
      format(gap_results$`Analytical sample (n)`[2],big.mark=",",scientific=FALSE),
      ")"
    ),
    hjust=0,vjust=0,
    size=2.8,fontface="bold",colour="grey30"
  )+
  scale_y_continuous(
    breaks=gap_plot$y_pos,
    labels=gap_plot$row_lab,
    limits=c(.6,5.65),
    expand=expansion(mult=0)
  )+
  scale_x_continuous(
    expand=expansion(mult=c(.04,.20))
  )+
  labs(
    title="e  Women–men differences in self-hiring probability\n    after accounting for pre-entry scholarly records",
    x="Adjusted probability difference\n(women − men; pp)",
    y=NULL
  )+
  theme_classic(base_size=9)+
  theme(
    plot.title=element_text(
      size=11.5,face="bold",hjust=0
    ),
    plot.title.position="plot",
    axis.text=element_text(size=9,colour="black"),
    axis.text.y=element_text(size=9.5),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    legend.position="none"
  )
P_FIG4 <- wrap_plots(
  A=wrap_elements(full=P_4A),
  B=wrap_elements(full=P_4B),
  C=wrap_elements(full=P_4C),
  D=P_4D,
  E=P_4E,
  design="AA\nBB\nCC\nDE",
  widths=c(1,1.1),
  heights=c(1,1.2,1.2,0.9)
)

print(P_FIG4)
