###################
# Create Figure 3 #
###################

# load packages
library(readr)
library(expm)
library(patchwork)
library(tidyverse)
library(RColorBrewer)
library(cowplot)

# import transition matrices
transition_1 <- read_rds("data/transition_matrices/20240104_TM_r0_imm0.3.0.09_gamma.rds")
transition_2 <- read_rds("data/transition_matrices/20240104_TM_r1_imm0.3.0.09_gamma.rds")
transition_3 <- read_rds("data/transition_matrices/20240104_TM_r1_imm2.0.2_gamma.rds")
transition_4 <- read_rds("data/transition_matrices/20240104_TM_r1_imm14.0.05_gamma.rds")

# carrying capacity
K <- 1
# state space
states <- seq(0,1,by=0.01)

# starting state:
x <- numeric(length(states))
x[20] <- 1

# create color palette
pal <- brewer.pal(n=9,name='BuPu')

# state distribution after applying actions 50 times:
# regime 1
dynamics_1 <- rbind(data.frame(states=seq_along(x),
                                   probability=t(transition_1[,,1]) %^% 50 %*% x / sum(t(transition_1[,,1]) %^% 50 %*% x),
                                   action='0'),
                        data.frame(states=seq_along(x),
                                   probability=t(transition_1[,,26]) %^% 50 %*% x / sum(t(transition_1[,,26]) %^% 50 %*% x),
                                   action='0.25'),
                        data.frame(states=seq_along(x),
                                   probability=t(transition_1[,,51]) %^% 50 %*% x / sum(t(transition_1[,,51]) %^% 50 %*% x),
                                   action='0.5'),
                        data.frame(states=seq_along(x),
                                   probability=t(transition_1[,,76]) %^% 50 %*% x / sum(t(transition_1[,,76]) %^% 50 %*% x),
                                   action='0.75'),
                        data.frame(states=seq_along(x),
                                   probability=t(transition_1[,,101]) %^% 50 %*% x / sum(t(transition_1[,,101]) %^% 50 %*% x),
                                   action='1'))
dynamics_1_plot <- ggplot(data=dynamics_1)+geom_line(aes(x=states,y=probability,color=action),linewidth=1.5)+
  labs(x='','probability')+
  scale_x_continuous(breaks=c(-1,101),
                     labels=c('0','K'))+
  scale_color_manual(values=c(pal[3],pal[5],pal[6],pal[8],pal[9]))+
  ggtitle('A.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        legend.position = 'None')

# regime 2
dynamics_2 <- rbind(data.frame(states=seq_along(x),
                               probability=t(transition_2[,,1]) %^% 9 %*% x / sum(t(transition_2[,,1]) %^% 9 %*% x),
                               action='0'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_2[,,26]) %^% 9 %*% x / sum(t(transition_2[,,26]) %^% 9 %*% x),
                               action='0.25'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_2[,,51]) %^% 9 %*% x / sum(t(transition_2[,,51]) %^% 9 %*% x),
                               action='0.5'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_2[,,76]) %^% 9 %*% x / sum(t(transition_2[,,76]) %^% 9 %*% x),
                               action='0.75'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_2[,,101]) %^% 9 %*% x / sum(t(transition_2[,,101]) %^% 9 %*% x),
                               action='1'))
dynamics_2_plot <- ggplot(data=dynamics_2)+geom_line(aes(x=states,y=probability,color=action),linewidth=1.5)+
  labs(x='',y='probability')+
  scale_x_continuous(breaks=c(0,101),
                     labels=c('0','K'))+
  scale_color_manual(values=c(pal[3],pal[5],pal[6],pal[8],pal[9]))+
  ggtitle('B.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        legend.position = 'None')

# regime 3
dynamics_3 <- rbind(data.frame(states=seq_along(x),
                               probability=t(transition_3[,,1]) %^% 9 %*% x / sum(t(transition_3[,,1]) %^% 9 %*% x),
                               action='0'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_3[,,26]) %^% 9 %*% x / sum(t(transition_3[,,26]) %^% 9 %*% x),
                               action='0.25'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_3[,,51]) %^% 9 %*% x / sum(t(transition_3[,,51]) %^% 9 %*% x),
                               action='0.5'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_3[,,76]) %^% 9 %*% x / sum(t(transition_3[,,76]) %^% 9 %*% x),
                               action='0.75'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_3[,,101]) %^% 9 %*% x / sum(t(transition_3[,,101]) %^% 9 %*% x),
                               action='1'))
dynamics_3_plot <- ggplot(data=dynamics_3)+geom_line(aes(x=states,y=probability,color=action),linewidth=1.5)+
  labs(x='',y='probability')+
  scale_x_continuous(breaks=c(0,101),
                     labels=c('0','K'))+
  scale_color_manual(values=c(pal[3],pal[5],pal[6],pal[8],pal[9]))+
  ggtitle('C.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        legend.position = 'None')

# regime 4
dynamics_4 <- rbind(data.frame(states=seq_along(x),
                               probability=t(transition_4[,,1]) %^% 9 %*% x / sum(t(transition_4[,,1]) %^% 9 %*% x),
                               action='0'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_4[,,26]) %^% 9 %*% x / sum(t(transition_4[,,26]) %^% 9 %*% x),
                               action='0.25'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_4[,,51]) %^% 9 %*% x / sum(t(transition_4[,,51]) %^% 9 %*% x),
                               action='0.5'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_4[,,76]) %^% 9 %*% x / sum(t(transition_4[,,76]) %^% 9 %*% x),
                               action='0.75'),
                    data.frame(states=seq_along(x),
                               probability=t(transition_4[,,101]) %^% 9 %*% x / sum(t(transition_4[,,101]) %^% 9 %*% x),
                               action='1'))
dynamics_4_plot <- ggplot(data=dynamics_4)+geom_line(aes(x=states,y=probability,color=action),linewidth=1.5)+
  labs(x='state (species density)',y='probability',color='action\n(removal\neffort)')+
  scale_x_continuous(breaks=c(0,101),
                     labels=c('0','K'))+
  scale_color_manual(values=c(pal[3],pal[5],pal[6],pal[8],pal[9]))+
  ggtitle('D.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        legend.position = 'None')
dynamics_4_leg <- ggplot(data=dynamics_4)+geom_line(aes(x=states,y=probability,color=action),linewidth=1.5)+
  labs(x='state (species density)',y='probability',color='action\n(removal\neffort)')+
  scale_x_continuous(breaks=c(0,101),
                     labels=c('0','K'))+
  scale_color_manual(values=c(pal[3],pal[5],pal[6],pal[8],pal[9]))+
  ggtitle('D.')+
  theme_minimal()+
  theme(text = element_text(size = 15),
        legend.margin = margin(l=20),
        legend.title = element_text(hjust=0.5))

legend <- get_legend(dynamics_4_leg)

layout <- '
AAA#
AAA#
AAA#
BBB#
BBBE
BBBE
CCCE
CCCE
CCC#
DDD#
DDD#
DDD#
'

final_plot <- dynamics_1_plot+dynamics_2_plot+dynamics_3_plot+dynamics_4_plot+legend+plot_layout(design=layout)
ggsave('figures/longterm_dynamics_test.tiff',final_plot,dpi=700,width=4.5,height=12)
