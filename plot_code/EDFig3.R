


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

COL_PURPLE <- "#6A1B6A"




# a. Adjusted scholarly profiles.
ROUTES <- c("Direct self-hire","Internal retention","External return","External hire")
ed3a_pub <- read_source("ED3a.xlsx","Publications") %>% mutate(route=factor(route,levels=ROUTES))
ed3a_jif <- read_source("ED3a.xlsx","JIF") %>% mutate(route=factor(route,levels=ROUTES))


ED3A_PUB <- ggplot(ed3a_pub,aes(route,estimate,colour=gender))+
  geom_errorbar(aes(ymin=low,ymax=high),width=.08,linewidth=.65,position=position_dodge(width=.4))+
  geom_point(size=2.5,position=position_dodge(width=.4))+
  scale_colour_manual(values=COL_GENDER,breaks=c("Women","Men"))+
  scale_y_continuous(
    breaks=seq(0,7.5,1.5),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(ylim=c(0,7.5))+
  scale_x_discrete(labels=c("Direct self-hire"="Direct\nself-hire",
                            "Internal retention"="Internal\nretention",
                            "External return"="External\nreturn",
                            "External hire"="Non-self-hire"))+
  labs(title="Three-year pre-entry publication count",
       x=NULL,y="Adjusted mean publication count",colour=NULL)+
  theme_classic(base_size=9)+theme(plot.title=element_text(face="bold",hjust=.5,size=10.5),
                                   legend.position="top",
                                   axis.text.x=element_text(size=8.4,colour="black"),
                                   axis.text.y=element_text(colour="black"))



ED3A_JIF <- ggplot(ed3a_jif,aes(route,estimate,colour=gender))+
  geom_errorbar(aes(ymin=low,ymax=high),width=.08,linewidth=.65,position=position_dodge(width=.4))+
  geom_point(size=2.5,position=position_dodge(width=.4))+
  scale_colour_manual(values=COL_GENDER,breaks=c("Women","Men"))+
  scale_y_continuous(
    breaks=seq(0,3,0.5),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(ylim=c(0,3))+
  scale_x_discrete(labels=c("Direct self-hire"="Direct\nself-hire",
                            "Internal retention"="Internal\nretention",
                            "External return"="External\nreturn",
                            "External hire"="Non-self-hire"))+
  labs(title="Three-year pre-entry mean JIF",
       x=NULL,y="Adjusted mean JIF",colour=NULL)+
  theme_classic(base_size=9)+theme(plot.title=element_text(face="bold",hjust=.5,size=10.5),
                                   legend.position="top",
                                   axis.text.x=element_text(size=8.4,colour="black"),
                                   axis.text.y=element_text(colour="black"))


P_ED3A <- (ED3A_PUB|ED3A_JIF)+plot_layout(guides="collect")+
  plot_annotation(title="a  Scholarly records before first faculty entry: self-hiring pathways and non-self-hires",
                  theme=theme(plot.title=element_text(face="bold",size=11.5))) & 
  theme(legend.position="top")




# b. 

ed3b_data <- read_source("ED3b.xlsx","Density") %>%
  rename(gender_plot=gender) %>%
  mutate(hire_status=factor(hire_status,levels=c("Self-hire","Non-self-hire")))
ed3b_medians <- read_source("ED3b.xlsx","Median") %>%
  mutate(hire_status=factor(hire_status,levels=c("Self-hire","Non-self-hire")))
ed3b_xmax <- max(ed3b_data$log_value)
P_ED3B <- ggplot(ed3b_data,aes(x=log_value,y=density,colour=gender_plot,fill=gender_plot))+
  geom_area(stat="identity",position="identity",alpha=.12,colour=NA)+
  geom_line(linewidth=.8)+
  geom_vline(data=ed3b_medians,aes(xintercept=median,colour=gender_plot),
             linetype="dashed",linewidth=.55,show.legend=FALSE)+
  facet_wrap(~hire_status,nrow=1)+
  scale_colour_manual(values=COL_GENDER)+
  scale_fill_manual(values=COL_GENDER)+
  scale_y_continuous(
    breaks=seq(0,1,0.2),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(0,ed3b_xmax),
    ylim=c(0,1)
  )+
  labs(title="b  Distribution of\n    three-year pre-entry publication counts",
       x="log(1 + pre-entry publications)",
       y="Density",colour=NULL,fill=NULL)+
  theme_classic(base_size=9.5)+
  theme(plot.title=element_text(face="bold",size=11.5,hjust=0),
        plot.title.position="plot",
        legend.position="top",
        legend.title=element_blank(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        axis.text=element_text(colour="black"))



# c. 


ed3c_data <- read_source("ED3c.xlsx","Density") %>%
  rename(gender_plot=gender) %>%
  mutate(hire_status=factor(hire_status,levels=c("Self-hire","Non-self-hire")))
ed3c_medians <- read_source("ED3c.xlsx","Median") %>%
  mutate(hire_status=factor(hire_status,levels=c("Self-hire","Non-self-hire")))
ed3c_xmax <- max(ed3c_data$log_value)
P_ED3C <- ggplot(ed3c_data,aes(x=log_value,y=density,colour=gender_plot,fill=gender_plot))+
  geom_area(stat="identity",position="identity",alpha=.12,colour=NA)+
  geom_line(linewidth=.8)+
  geom_vline(data=ed3c_medians,aes(xintercept=median,colour=gender_plot),linetype="dashed",linewidth=.55,show.legend=FALSE)+
  facet_wrap(~hire_status,nrow=1)+
  scale_colour_manual(values=COL_GENDER)+scale_fill_manual(values=COL_GENDER)+
  scale_y_continuous(
    breaks=seq(0,1,0.2),
    expand=expansion(mult=0)
  ) +
  coord_cartesian(
    xlim=c(0,ed3c_xmax),
    ylim=c(0,1)
  )+
  labs(title="c  Distribution of\n    three-year pre-entry mean JIF",
       x="log(1 + pre-entry mean JIF)",
       y="Density",colour=NULL,fill=NULL)+
  theme_classic(base_size=9.5)+
  theme(plot.title=element_text(face="bold",size=11.5,hjust=0),
        plot.title.position="plot",
        legend.position="top",
        legend.title=element_blank(),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"),
        axis.text=element_text(colour="black"))




# d. Gender interactions by pathway.
ROUTES_D <- rev(ROUTES[1:3])
ed3d_pub <- read_source("ED3d.xlsx","Publications") %>% mutate(route=factor(route,levels=ROUTES_D))
ed3d_jif <- read_source("ED3d.xlsx","JIF") %>% mutate(route=factor(route,levels=ROUTES_D))
ED3D_PUB <- ggplot(ed3d_pub,aes(estimate,route))+
  geom_vline(xintercept=0,linetype="dashed",colour="grey55",linewidth=.45)+
  geom_segment(aes(x=low,xend=high,yend=route),linewidth=.75,colour=COL_DIFF)+
  geom_point(size=2.6,colour=COL_DIFF)+
  scale_x_continuous(
    breaks=seq(-1,2,0.5),
    expand=expansion(mult=0)
  )+
  coord_cartesian(xlim=c(-1,2))+
  labs(
    title="Three-year pre-entry publication count",
    x="Women − men difference in\nself-hire − non-self-hire",
    y=NULL
  )+
  theme_classic(base_size=9.5)+
  theme(
    plot.title=element_text(face="bold",size=10.5),
    axis.text=element_text(size=9.5,colour="black"),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank()
  )



ED3D_JIF <- ggplot(ed3d_jif,aes(estimate,route))+
  geom_vline(xintercept=0,linetype="dashed",colour="grey55",linewidth=.45)+
  geom_segment(aes(x=low,xend=high,yend=route),linewidth=.75,colour=COL_DIFF)+
  geom_point(size=2.6,colour=COL_DIFF)+
  scale_x_continuous(
    breaks=seq(-1,2,0.5),
    expand=expansion(mult=0)
  )+
  coord_cartesian(xlim=c(-1,2))+
  labs(
    title="Three-year pre-entry mean JIF",
    x="Women − men difference in\nself-hire − non-self-hire",
    y=NULL
  )+
  theme_classic(base_size=9.5)+
  theme(
    plot.title=element_text(face="bold",size=10.5),
    axis.text=element_text(size=9.5,colour="black"),
    axis.line.y=element_blank(),
    axis.ticks.y=element_blank()
  )

P_ED3D <- (ED3D_PUB|ED3D_JIF)+
  plot_annotation(
    title="d  Women–men differences in scholarly gaps across self-hiring pathways",
    theme=theme(plot.title=element_text(face="bold",size=11.5))
  )




# e-f. Rank-similarity thresholds.
ed3e_results <- read_source("ED3e.xlsx","Results")
P_ED3E <- ggplot(ed3e_results,aes(epsilon,estimate)) +
  geom_hline(yintercept=0,linetype="dashed",colour="grey55",linewidth=.45) +
  geom_ribbon(aes(ymin=low,ymax=high),fill=COL_PURPLE,alpha=.10) +
  geom_line(colour=COL_PURPLE,linewidth=.75) +
  geom_point(colour=COL_PURPLE,size=2.3) +
  scale_x_continuous(
    breaks=c(.010,.025,.050,.075,.100,.150,.200),
    labels=scales::label_number(accuracy=.001)
  ) +
  labs(
    title="e  Pre-entry publication differences across\n    doctoral–faculty institution rank similarity thresholds",
    x="SpringRank difference threshold (ε)",
    y="Three-year pre-entry publication count difference\n(self-hire − non-self-hire)"
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(face="bold",size=11,hjust=0),
    plot.title.position="plot",
    axis.text.x=element_text(angle=30,hjust=1,vjust=1,colour="black",size=9),
    axis.text=element_text(colour="black",size=9)
  )



ed3f_results <- read_source("ED3f.xlsx")
P_ED3F <- ggplot(ed3f_results,aes(epsilon,estimate)) +
  geom_hline(yintercept=0,linetype="dashed",colour="grey55",linewidth=.45) +
  geom_ribbon(aes(ymin=low,ymax=high),fill=COL_PURPLE,alpha=.10) +
  geom_line(colour=COL_PURPLE,linewidth=.75) +
  geom_point(colour=COL_PURPLE,size=2.3) +
  scale_x_continuous(
    breaks=c(.010,.025,.050,.075,.100,.150,.200),
    labels=scales::label_number(accuracy=.001)
  ) +
  labs(
    title="f  Pre-entry mean JIF differences across\n    doctoral–faculty institution rank similarity thresholds",
    x="SpringRank difference threshold (ε)",
    y="Three-year pre-entry mean JIF difference\n(self-hire − non-self-hire)"
  ) +
  theme_classic(base_size=10) +
  theme(
    plot.title=element_text(face="bold",size=11,hjust=0),
    plot.title.position="plot",
    axis.text.x=element_text(angle=30,hjust=1,vjust=1,colour="black",size=9),
    axis.text=element_text(colour="black",size=9)
  )




P_EDFIG3 <- wrap_plots(A=wrap_elements(full=P_ED3A),
                       B=P_ED3B,C=P_ED3C,D=wrap_elements(full=P_ED3D),
                       E=P_ED3E,F=P_ED3F,
                       design="AA\nBC\nDD\nEF",heights=c(1.4,0.8,1.1,1.3))


print(P_EDFIG3)
