


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



library(gridExtra)
COL_DIFF <- "#4C9A9A"

COL_MALE <- "#1F77B4"

COL_FEMALE <- "#D62728"

COL_ZERO <- "grey65"

continent_order <- c("Americas","Asia","Europe","Africa","Oceania")



# a. Alternative adjustment specifications.
ed1a_plot_df <- read_source("ED1a.xlsx") %>%
  mutate(y=match(Specification,c("Adjusted (system + domain + cohort)",
     "Adjusted (system + domain)","Adjusted (domain)","Adjusted (system)")))
stopifnot(!anyNA(ed1a_plot_df$y))



P_ED1A <- ggplot(
  ed1a_plot_df %>% mutate(y_plot=y*1.5),
  aes(x=diff_pp,y=y_plot)
) +
  geom_vline(
    xintercept=0,
    linetype="dashed",
    linewidth=0.45,
    colour="grey65"
  ) +
  geom_errorbar(
    aes(xmin=conf_low_pp,xmax=conf_high_pp),
    orientation="y",
    width=0.10,
    linewidth=0.65,
    colour=COL_DIFF
  ) +
  geom_point(
    size=2.9,
    colour=COL_DIFF
  ) +
  geom_text(
    aes(
      x=diff_pp,
      y=y_plot-0.25,
      label=difference_label
    ),
    hjust=0.5,
    vjust=1,
    size=2.85,
    lineheight=1.15,
    fontface="bold",
    colour="black"
  ) +
  scale_x_continuous(
    breaks=c(0,1,2,3),
    labels=function(x) ifelse(x>0,paste0("+",x),as.character(x)),
    expand=expansion(mult=c(0,0))
  ) +
  scale_y_continuous(
    limits=c(0.5,6.6),
    breaks=c(1.5,3,4.5,6),
    labels=c(
      "Adjusted (system + domain + cohort)",
      "Adjusted (system + domain)",
      "Adjusted (domain)",
      "Adjusted (system)"
    )
  ) +
  labs(
    title="a  Women–men differences in self-hiring\n    under alternative adjustments",
    x="Women − men difference in self-hiring (pp)",
    y=NULL
  ) +
  coord_cartesian(
    xlim=c(-0.2,3.55),
    clip="off"
  ) +
  theme_classic(base_size=9,base_family="sans") +
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",
      size=12,
      hjust=0,
      margin=margin(b=10)
    ),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(
      size=10,
      colour="black",
      margin=margin(r=9)
    ),
    axis.text.x=element_text(size=10,colour="black"),
    axis.title.x=element_text(
      size=10,
      colour="black",
      margin=margin(t=6)
    ),
    plot.margin=margin(t=8,r=15,b=8,l=6)
  )





# b. System-specific probabilities and differences.
ed1b_selected <- read_source("ED1b.xlsx","ed1b_selected") %>%
  mutate(continent=factor(continent,levels=continent_order))
n_total <- nrow(ed1b_selected)
group_info <- ed1b_selected %>%
  group_by(continent) %>%
  summarise(ymin=min(y),ymax=max(y),ymid=mean(y),.groups="drop") %>%
  arrange(continent)

sep_df <- group_info %>%
  slice(-n()) %>%
  transmute(y_sep=ymin-0.5)

left_long <- ed1b_selected %>%
  select(first_faculty_system,y,male_prob,female_prob) %>%
  pivot_longer(
    cols=c(male_prob,female_prob),
    names_to="gender",
    values_to="prob"
  ) %>%
  mutate(gender=recode(gender,male_prob="Men",female_prob="Women"))

left_x_min <- min(ed1b_selected$male_prob,ed1b_selected$female_prob,na.rm=TRUE)
left_x_max <- max(ed1b_selected$male_prob,ed1b_selected$female_prob,na.rm=TRUE)
left_x_range <- left_x_max-left_x_min

right_x_min <- min(0,ed1b_selected$conf.low_pp,na.rm=TRUE)
right_x_max <- max(ed1b_selected$conf.high_pp,na.rm=TRUE)
right_x_range <- right_x_max-right_x_min
diff_label_x <- right_x_max+0.05*right_x_range


P_ED1B_left <- ggplot() +
  geom_segment(
    data=ed1b_selected,
    aes(x=male_prob,xend=female_prob,y=y,yend=y),
    linewidth=0.50,colour="grey75"
  ) +
  geom_point(
    data=left_long,
    aes(x=prob,y=y,colour=gender),
    size=2.35
  ) +
  geom_hline(
    data=sep_df,
    aes(yintercept=y_sep),
    linetype="dashed",linewidth=0.42,colour="grey78"
  ) +
  scale_colour_manual(
    values=c("Women"=COL_FEMALE,"Men"=COL_MALE),
    breaks=c("Women","Men"),
    name=NULL,
    guide=guide_legend(order=1,nrow=1,override.aes=list(size=2.6))
  ) +
  scale_x_continuous(
    breaks=pretty(c(left_x_min,left_x_max),n=5),
    expand=expansion(mult=c(0.02,0.04))
  ) +
  scale_y_continuous(
    breaks=ed1b_selected$y,
    labels=ifelse(
      ed1b_selected$first_faculty_system %in% c("Türkiye","Turkey"),
      "Turkey",
      ed1b_selected$first_faculty_system
    ),
    limits=c(0.5,n_total+0.5),
    expand=c(0,0)
  )+
  coord_cartesian(
    xlim=c(left_x_min-0.04*left_x_range,left_x_max+0.05*left_x_range),
    clip="off"
  ) +
  labs(x="Self-hiring (%)",y=NULL) +
  theme_classic(base_size=10,base_family="sans") +
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_text(size=10,colour="black"),
    axis.text.x=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10,colour="black",margin=margin(t=5)),
    legend.position="bottom",
    plot.margin=margin(t=4,r=8,b=5,l=4)
  )



P_ED1B_right <- ggplot(ed1b_selected,aes(x=estimate_pp,y=y)) +
  geom_vline(
    xintercept=0,linetype="dashed",
    linewidth=0.45,colour=COL_ZERO
  ) +
  geom_hline(
    data=sep_df,
    aes(yintercept=y_sep),
    inherit.aes=FALSE,
    linetype="dashed",linewidth=0.42,colour="grey78"
  ) +
  geom_errorbar(
    aes(xmin=conf.low_pp,xmax=conf.high_pp),
    orientation="y",width=0.09,
    linewidth=0.52,colour=COL_DIFF,
    show.legend=FALSE
  ) +
  geom_point(
    aes(colour="Women - men difference"),
    size=2.35
  ) +
  geom_text(
    aes(x=diff_label_x,label=diff_label),
    hjust=0,size=3,colour="black"
  ) +
  scale_colour_manual(
    values=c("Women - men difference"=COL_DIFF),
    name=NULL,
    guide=guide_legend(order=2,nrow=1,override.aes=list(size=2.6))
  ) +
  scale_x_continuous(
    breaks=pretty(c(right_x_min,right_x_max),n=5),
    labels=function(x) ifelse(x>0,paste0("+",x),as.character(x)),
    expand=expansion(mult=c(0.02,0.03))
  ) +
  scale_y_continuous(
    limits=c(0.5,n_total+0.5),
    expand=c(0,0)
  ) +
  coord_cartesian(
    xlim=c(right_x_min-0.04*right_x_range,right_x_max+0.09*right_x_range),
    clip="off"
  ) +
  labs(x="Women - men difference (pp)",y=NULL) +
  theme_classic(base_size=10,base_family="sans") +
  theme(
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=10,colour="black"),
    axis.title.x=element_text(size=10,colour="black",margin=margin(t=5)),
    legend.position="bottom",
    plot.margin=margin(t=4,r=58,b=5,l=2)
  )
continent_labels <- ed1b_selected %>%
  group_by(continent) %>%
  summarise(
    y_mid=(min(y)+max(y))/2,
    .groups="drop"
  )





P_ED1B_continent <- ggplot() +
  geom_text(
    data=continent_labels,
    aes(x=1,y=y_mid,label=continent),
    hjust=0.5,
    size=3.0,
    fontface="bold",
    colour="black"
  ) +
  geom_hline(
    data=sep_df,
    aes(yintercept=y_sep),
    linetype="dashed",
    linewidth=0.42,
    colour="grey78"
  ) +
  scale_x_continuous(
    limits=c(0.5,1.5),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    limits=c(0.5,n_total+0.5),
    expand=c(0,0)
  ) +
  coord_cartesian(clip="off") +
  theme_void(base_size=10,base_family="sans") +
  theme(
    plot.margin=margin(t=4,r=0,b=5,l=0)
  )




P_ED1B <- P_ED1B_continent + P_ED1B_left + P_ED1B_right +
  plot_layout(
    widths=c(0.30,1.00,1.22),
    guides="collect"
  ) +
  plot_annotation(
    title="b  Self-hiring rates and women–men differences across systems",
    theme=theme(
      plot.title.position="plot",
      plot.title=element_text(
        family="sans",
        face="bold",
        size=12,
        colour="black",
        hjust=0,
        margin=margin(b=8)
      )
    )
  ) &
  theme(
    legend.position="bottom",
    legend.justification="center",
    legend.direction="horizontal",
    legend.box="horizontal",
    legend.box.just="center",
    legend.text=element_text(family="sans",size=10,colour="black"),
    legend.key.width=unit(0.65,"cm"),
    legend.key.height=unit(0.30,"cm"),
    legend.spacing.x=unit(0.16,"cm"),
    legend.margin=margin(t=1,r=0,b=0,l=0)
  )





# c. System-by-domain heatmap; main sheet includes Overall cells.
COL_NEG_20 <- "#53338F"
COL_NEG_10 <- "#8B74B6"
COL_NEG_5 <- "#C8BDD9"
COL_ZERO <- "#FFFFFF"
COL_POS_5 <- "#C7E3D6"
COL_POS_10 <- "#63AA88"
COL_POS_20 <- "#0C6A4B"
COL_NA <- "#D9D9D9"


continent_order <- c("Americas","Asia","Europe","Africa","Oceania")
domain_order <- c(
  "Engineering and technology",
  "Agricultural and veterinary sciences",
  "Humanities and the arts",
  "Natural sciences",
  "Social sciences",
  "Medical and health sciences"
)


domain_plot_order <- c(domain_order,"Overall")
domain_labels <- c(
  "Engineering and technology"="Engineering",
  "Agricultural and veterinary sciences"="Agriculture",
  "Humanities and the arts"="Humanities",
  "Natural sciences"="Natural sciences",
  "Social sciences"="Social sciences",
  "Medical and health sciences"="Medical\n& health",
  "Overall"="Overall"
)
domain_x <- c(1,2,3,4,5,6,7.25)



ed1c_plot_df <- read_source("ED1c.xlsx","ed1c") %>%
  rename(first_faculty_system=`First-faculty academic system`,
         continent=Continent,first_faculty_domain=`First-faculty domain`)
system_order_c <- ed1c_plot_df %>% distinct(first_faculty_system,continent,y) %>%
  mutate(continent=factor(continent,levels=continent_order))
system_axis_c <- arrange(system_order_c,y)
group_info_c <- system_order_c %>%
  group_by(continent) %>%
  summarise(
    ymin=min(y),
    ymax=max(y),
    .groups="drop"
  ) %>%
  arrange(continent)

sep_df_c <- group_info_c %>%
  slice(-n()) %>%
  transmute(y_sep=ymin-0.5)
system_axis_c2 <- system_axis_c %>%
  mutate(
    system_label=ifelse(
      first_faculty_system %in% c("Türkiye","Turkey"),
      "Turkey",
      first_faculty_system
    )
  )




# Continent labels
# Assumes system_axis_c contains the continent classification
continent_axis_c <- system_axis_c2 %>%
  group_by(continent) %>%
  summarise(
    y_mid=(min(y)+max(y))/2,
    .groups="drop"
  )

Y_LIM_C <- c(0.5,max(system_axis_c2$y)+0.5)




# Left: continent labels
P_ED1C_continent <- ggplot() +
  geom_text(
    data=continent_axis_c,
    aes(x=1,y=y_mid,label=continent),
    hjust=0.5,
    size=3.0,
    fontface="bold",
    colour="black"
  ) +
  geom_hline(
    data=sep_df_c,
    aes(yintercept=y_sep),
    linetype="dashed",
    linewidth=0.40,
    colour="grey78"
  ) +
  scale_x_continuous(
    limits=c(0.5,1.5),
    expand=c(0,0)
  ) +
  scale_y_continuous(
    limits=Y_LIM_C,
    expand=c(0,0)
  ) +
  coord_cartesian(clip="off") +
  theme_void(base_size=9,base_family="sans") +
  theme(
    plot.margin=margin(t=8,r=0,b=4,l=0)
  )




# Right: system × domain heatmap
P_ED1C_heat <- ggplot(
  ed1c_plot_df,
  aes(x=x,y=y,fill=estimate_display)
) +
  geom_tile(
    width=0.92,
    height=0.88,
    colour="grey94",
    linewidth=0.40
  ) +
  geom_hline(
    data=sep_df_c,
    aes(yintercept=y_sep),
    inherit.aes=FALSE,
    linetype="dashed",
    linewidth=0.40,
    colour="grey78"
  ) +
  scale_fill_gradientn(
    colours=c(
      COL_NEG_20,COL_NEG_10,COL_NEG_5,
      COL_ZERO,
      COL_POS_5,COL_POS_10,COL_POS_20
    ),
    values=scales::rescale(
      c(-20,-10,-5,0,5,10,20)
    ),
    limits=c(-20,20),
    breaks=c(-20,-10,0,10,20),
    labels=c("<=-20","-10","0","+10",">=+20"),
    oob=scales::squish,
    na.value=COL_NA,
    name="Women - men\ndifference (pp)"
  ) +
  scale_x_continuous(
    breaks=domain_x,
    labels=unname(domain_labels[domain_plot_order]),
    expand=c(0,0),
    position="top"
  ) +
  scale_y_continuous(
    breaks=system_axis_c2$y,
    labels=system_axis_c2$system_label,
    limits=Y_LIM_C,
    expand=c(0,0)
  ) +
  coord_cartesian(
    xlim=c(0.5,7.75),
    clip="off"
  ) +
  labs(x=NULL,y=NULL) +
  theme_classic(base_size=9,base_family="sans") +
  theme(
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(
      size=10,
      colour="black",
      margin=margin(r=5)
    ),
    axis.text.x=element_text(
      size=10,
      colour="black",
      angle=30,
      hjust=0,
      vjust=0
    ),
    legend.position="none",
    plot.margin=margin(t=8,r=6,b=4,l=4)
  )
  



# Combine: continent + heatmap
# Reduce top margins
P_ED1C_continent <- P_ED1C_continent +
  theme(plot.margin=margin(t=0,r=0,b=4,l=0))

P_ED1C_heat <- P_ED1C_heat +
  theme(
    plot.margin=margin(t=0,r=6,b=4,l=4),
    axis.text.x=element_text(
      size=10,
      colour="black",
      angle=30,
      hjust=0,
      vjust=0,
      margin=margin(b=0,t=0)
    )
  )


P_ED1C <- (
  P_ED1C_continent + P_ED1C_heat
) +
  plot_layout(
    widths=c(0.2,1.00)
  ) +
  plot_annotation(
    title="c  Women–men differences in self-hiring\n    across systems and domains",
    theme=theme(
      plot.title.position="plot",
      plot.title=element_text(
        family="sans",
        face="bold",
        size=12,
        colour="black",
        hjust=0,
        lineheight=1.05,
        margin=margin(b=0)
      ),
      # plot.margin=margin(t=2,r=0,b=0,l=0)
    )
  ) &
  theme(legend.position="none")



# d. Domain-by-cohort heatmap; main sheet includes Overall cells.
domain_order_d <- c(
  "Engineering and technology",
  "Agricultural and veterinary sciences",
  "Humanities and the arts",
  "Natural sciences",
  "Social sciences",
  "Medical and health sciences"
)

domain_labels_d <- c(
  "Engineering and technology"="Engineering",
  "Agricultural and veterinary sciences"="Agriculture",
  "Humanities and the arts"="Humanities",
  "Natural sciences"="Natural sciences",
  "Social sciences"="Social sciences",
  "Medical and health sciences"="Medical & health"
)

cohort_order <- c("1990-94","1995-99","2000-04","2005-09","2010-14","2015-19","2020-24")
cohort_plot_order <- c(cohort_order,"Overall")
cohort_x <- c(1,2,3,4,5,6,7,8.55)



ed1d_plot_df <- read_source("ED1d.xlsx","ed1d")

P_ED1D <- ggplot(ed1d_plot_df,aes(x=x,y=y,fill=estimate_display)) +
  geom_tile(
    width=0.92,height=0.88,
    colour="grey94",linewidth=0.40
  ) +
  scale_fill_gradientn(
    colours=c(
      COL_NEG_20,COL_NEG_10,COL_NEG_5,
      COL_ZERO,
      COL_POS_5,COL_POS_10,COL_POS_20
    ),
    values=scales::rescale(c(-20,-10,-5,0,5,10,20)),
    limits=c(-20,20),
    breaks=c(-20,-10,0,10,20),
    labels=c("<=-20","-10","0","+10",">=+20"),
    oob=scales::squish,
    na.value=COL_NA,
    name="Women - men\ndifference in\nself-hiring (pp)"
  ) +
  scale_x_continuous(
    breaks=cohort_x,
    labels=c(
      "1990-94","1995-99","2000-04","2005-09",
      "2010-14","2015-19","2020-24","Overall"
    ),
    limits=c(0.5,9.05),
    expand=c(0,0),
    position="top"
  ) +
  scale_y_continuous(
    breaks=seq_along(domain_order_d),
    labels=rev(unname(domain_labels_d[domain_order_d])),
    limits=c(0.5,length(domain_order_d)+0.5),
    expand=c(0,0)
  ) +
  labs(
    title="d  Women–men differences in self-hiring\n    across domains and faculty-entry cohorts",
    x=NULL,y=NULL
  ) +
  theme_classic(base_size=9,base_family="sans") +
  theme(
    plot.title.position="plot",
    plot.title=element_text(
      face="bold",size=12,colour="black",
      hjust=0,margin=margin(b=8)
    ),
    axis.line=element_blank(),
    axis.ticks=element_blank(),
    axis.text.y=element_text(
      size=10,colour="black",margin=margin(r=5)
    ),
    axis.text.x=element_text(
      size=10,colour="black",angle=25,hjust=0,vjust=0
    ),
    legend.position="right",
    legend.title=element_text(
      size=10,
      lineheight=0.95,
      margin=margin(b=12)
    ),
    legend.text=element_text(size=10),
    legend.key.height=unit(1.05,"cm"),
    plot.margin=margin(t=8,r=8,b=4,l=6)
  )
P_ED1A_final <- P_ED1A +
  theme(plot.margin=margin(t=5,r=5,b=5,l=5))

P_ED1B_final <- P_ED1B

P_ED1C_final <- P_ED1C

P_ED1D_final <- P_ED1D +
  labs(caption=NULL) +
  theme(
    legend.position="right",
    plot.margin=margin(t=4,r=4,b=4,l=8)
  )




# Convert panels to grobs
gA <- ggplotGrob(P_ED1A_final)
gB <- patchwork::patchworkGrob(P_ED1B_final)
gC <- patchwork::patchworkGrob(P_ED1C_final)
gD <- ggplotGrob(P_ED1D_final)

# Row 1: A + B
# Compress Panel A vertically
gA_compact <- gridExtra::arrangeGrob(
  gA,
  grid::nullGrob(),
  ncol=1,
  heights=c(0.90,0.10)
)

# Row 1: compressed A + original B
p_row1 <- gridExtra::arrangeGrob(
  gA_compact,gB,
  ncol=2,
  widths=c(0.40,0.60)
)

# Row 2: C + D
p_row2 <- gridExtra::arrangeGrob(
  gC,gD,
  ncol=2,
  widths=c(0.52,0.48)
)

# Footnote
ED_FOOTNOTE <- grid::textGrob(
  "Grey: fewer than 20 women or 20 men per cell. Both heatmaps share a ±20 pp colour scale.",
  x=unit(0.02,"npc"),
  hjust=0,
  gp=gpar(fontsize=9,col="grey35")
)

# Final assembly
P_ED1 <- gridExtra::arrangeGrob(
  p_row1,
  p_row2,
  ED_FOOTNOTE,
  ncol=1,
  heights=unit.c(
    unit(10,"cm"),
    unit(12,"cm"),
    unit(0.7,"cm")
  )
)

grid.newpage()
grid.draw(P_ED1)
