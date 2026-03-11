library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(cowplot) 

d <- read_excel("new_dataset.xlsx")

d$Tcuv <- round(d$Tcuv, 0)

d$Genotype <- as.factor(d$Genotype)
d$tr <- as.factor(d$tr) 
d$Tcuv <- as.factor(d$Tcuv)
d$ETR <- as.numeric(d$ETR)

str(d)

shape_size = 2

d_1 <- d %>%
 select(Genotype, tr, Tleaf, Tcuv, PARtop, A, ci, ETR) %>%
  filter(PARtop > 1000) %>% 
  group_by(Genotype, Tcuv, tr) %>% 
  summarise(mean_A = mean(A),
            tleaf = mean(Tleaf),
            se = sd(A)/sqrt(n()),
            max = mean_A + se,
            min = mean_A - se,
            se_T = sd(Tleaf)/sqrt(n()),
            max_T = tleaf + se_T,
            min_T = tleaf - se_T,
            .groups = "drop") %>% 
  mutate(id = row_number())


plot_Ag <- d_1 %>% 
  ggplot(aes(x = tleaf,
             y = mean_A,
             groups = tr,
             shape = tr,
             linetype = tr,)) +
  geom_point(size = shape_size) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              colour = "black",
              linewidth = 0.6) +
  geom_errorbar(aes(ymin = min, ymax=max),
                width = 1,
                linetype = 1) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ylim(-5, 32) +
  xlim(0, 50) +
  ylab("") +
  xlab("") +
  facet_wrap(vars(Genotype)) +
  theme_bw() +
  theme(legend.position = "",
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0,-2.5,0), "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
        axis.ticks.length.y=unit(c(-2.5,2.5), "pt"))

d_2 <- d %>%
  select(Genotype, tr, Tleaf, Tcuv, PARtop, A, ci, ETR) %>%
  filter(PARtop < 1000) %>% 
  group_by(Genotype, Tcuv, tr) %>% 
  summarise(mean_Rd = mean(A)*(-1),
            tleaf = mean(Tleaf),
            se = sd(A)/sqrt(n()),
            max = mean_Rd + se,
            min = mean_Rd - se,
            se_T = sd(Tleaf)/sqrt(n()),
            max_T = tleaf + se_T,
            min_T = tleaf - se_T,
            .groups = "drop")%>% 
  mutate(id = row_number())

plot_Rd <- d_2 %>% 
  ggplot(aes(x = tleaf,
             y = mean_Rd,
             groups = tr,
             shape = tr,
             linetype = tr)) +
  geom_point(size = shape_size) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              colour = "black",
              linewidth = 0.6) +
  geom_errorbar(aes(ymin = min, ymax=max),
                width = 1,
                linetype = 1) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ylim(0, 6.5) +
  xlim(0, 50) +
  ylab("") +
  xlab("") +
  facet_wrap(vars(Genotype)) +
  theme_bw() +
  theme(legend.position = "",
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0,-2.5,0), "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
        axis.ticks.length.y=unit(c(-2.5,2.5), "pt"))


d_3 <- d %>%
  select(Genotype, tr, Tleaf, Tcuv, PARtop, A, ci, ETR) %>%
  filter(PARtop > 1000) %>% 
  group_by(Genotype, Tcuv, tr) %>% 
  summarise(mean_ci = mean(ci),
            tleaf = mean(Tleaf),
            se = sd(ci)/sqrt(n()),
            max = mean_ci + se,
            min = mean_ci - se,
            se_T = sd(Tleaf)/sqrt(n()),
            max_T = tleaf + se_T,
            min_T = tleaf - se_T,
            .groups = "drop")

plot_ci <- d_3 %>% 
  ggplot(aes(x = tleaf,
             y = mean_ci,
             groups = tr,
             shape = tr,
             linetype = tr)) +
  geom_point(size = shape_size) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              colour = "black",
              linewidth = 0.6) +
  geom_errorbar(aes(ymin = min, ymax=max),
                width = 1,
                linetype = 1) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ylim(150, 450) +
  xlim(0, 50) +
  ylab("") +
  xlab("") +
  labs(shape = "") +
  facet_wrap(vars(Genotype)) +
  theme_bw() +
  theme(legend.position = "",
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0,0,0), "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
        axis.ticks.length.y=unit(c(-2.5,2.5), "pt"))

d_4 <- d %>%
  select(Genotype, tr, Tleaf, Tcuv, PARtop, A, ci, ETR) %>%
  filter(PARtop > 1000) %>% 
  group_by(Genotype, Tcuv, tr) %>% 
  summarise(mean_ETR = mean(ETR),
            tleaf = mean(Tleaf),
            se = sd(ETR)/sqrt(n()),
            max = mean_ETR + se,
            min = mean_ETR - se,
            se_T = sd(Tleaf)/sqrt(n()),
            max_T = tleaf + se_T,
            min_T = tleaf - se_T,
            .groups = "drop")

plot_ETR <- d_4 %>% 
  ggplot(aes(x = tleaf,
             y = mean_ETR,
             groups = tr,
             shape = tr,
             linetype = tr)) +
  geom_point(size = shape_size) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              colour = "black",
              linewidth = 0.6) +
  geom_errorbar(aes(ymin = min, ymax=max),
                width = 1,
                linetype = 1) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ylim(0, 225) +
  xlim(0, 50) +
  ylab("") +
  xlab("") +
  facet_wrap(vars(Genotype)) +
  theme_bw() +
  theme(legend.position = "",
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing = unit(-0, "lines"),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.margin = unit(c(0,0,-2.5,0), "pt"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
        axis.ticks.length.y=unit(c(-2.5,2.5), "pt"))




plot_Ag

plot_Rd

plot_ci

plot_ETR


grid <- plot_grid(plot_Ag,plot_Rd,plot_ETR,plot_ci,
          ncol = 1 ,
          align = "v",
          axis = "lr")

ggsave("grid_fin.png",
       plot = grid,
       width = 4,
       height = 6,
       dpi = 700)

#-----------------------------------------------------------------------

d_5 <- d %>% 
  select(Genotype, tr, Tleaf, Tcuv, PARtop, A, ci, ETR, RA) %>% 
  filter(RA >= 0 & RA <= 2) %>% 
  group_by(Genotype, Tcuv, tr) %>% 
  summarise(mean_RA = mean(RA),
            tleaf = mean(Tleaf),
            se = sd(RA)/sqrt(n()),
            max = mean_RA + se,
            min = mean_RA - se,
            .groups = "drop")



plot_RA <- d_5 %>% 
  ggplot(aes(x = tleaf,
             y = mean_RA,
             groups = tr,
             shape = tr,
             linetype = tr)) +
  geom_point(size = shape_size) +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              colour = "black",
              linewidth = 0.6) +
  geom_errorbar(aes(ymin = min, ymax=max),
                width = 1,
                linetype = 1) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ylim(0, 1.6) +
  xlim(0, 50) +
  ylab("Rd/Ag (µmol m-2 s-1)") +
  xlab("Tleaf (°C)") +
  labs(shape = "") +
  facet_wrap(vars(Genotype)) +
  theme_bw() +
  theme(legend.position = "right",
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
        axis.ticks.length.y=unit(c(-2.5,2.5), "pt"))


ggsave("ra_03312023.png",
       plot = plot_RA,
       width = 8,
       height = 3.7,
       dpi = 700)

ticks_test <- d_5 %>% 
  ggplot(aes(x = tleaf,
             y = mean_RA,
             groups = tr,
             shape = tr,
             linetype = tr)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = FALSE,
              colour = "black",
              linewidth = 0.6) +
  geom_errorbar(aes(ymin = min, ymax=max),
                width = 1,
                linetype = 1) +
  scale_shape_manual(values = c(16, 1, 17, 2)) +
  scale_linetype_manual(values = c(1, 1, 2, 2)) +
  ylim(0, 1.6) +
  xlim(0, 50) +
  ylab("Rd/Ag (µmol m-2 s-1)") +
  xlab("Tleaf (°C)") +
  labs(shape = "") +
  facet_wrap(vars(Genotype)) +
  theme_bw() +
  theme(legend.position = "right",
        strip.background = element_blank(),
        panel.spacing = unit(0, "lines"),
        panel.grid.major = element_line(color = "transparent"),
        panel.grid.minor = element_line(color = "transparent"),
        
        axis.ticks.length.x=unit(c(-2.5,2.5), "pt"),
        axis.ticks.length.y=unit(c(-2.5,2.5), "pt"))

