#Analyses and data visualization with explanations for the paper "Evaluating
#O2:Ar, N2:Ar, and 29,30N2 using Membrane Inlet Mass Spectrometry configured
#to minimize oxygen interference"

#libraries
library(smatr) #for SMA analysis
library(rstatix) #for stats interpretations
library(tidyverse) #data viz
library(ggpubr) #data viz

#Read in compiled data
df <- read.csv("Compiled_Subcategories.csv", header = TRUE)
#Data are organized in one compiled csv that can be filtered out as needed

#Separating out individual experiments
bio <- df %>% filter(Experiment == "Bioassays") %>%
  select(-Site_Name, -Depth, -Time_Point)
gw <- df %>% filter(Experiment == "Groundwater") %>%
  select(-Lake, -Bottle, -Treatment, -Depth, -Time_Point)
pf <- df %>% filter(Experiment == "Profiles") %>%
  select(-Bottle, -Treatment, -Site_Name, -Time_Point)
sed <- df %>% filter(Experiment == "Sediment Incubations") %>%
  select(-Lake, -Temp, -Depth)

sed$Treatment <- factor(sed$Treatment, levels = c("SW", "LP", "N", "LP N")) 
#Reordering since R alphabetizes factors


#-------------------------------------------------------------------------------

#Standardized Major Axis (SMA) Regression 

## O2:Ar of separate experiments (will become figure 1)
#To directly compare O2:Ar and N2:Ar with and without a furnace, we used 
#standardized major axis regression because O2:Ar and N2:Ar both with and without the 
#furnace are measured with error and are correlated since they come from the 
#same sample, and no dependent-independent variable relationship is defined 
#(Friedman et al. 2013). 

#SMA for O2:Ar

bioO2.m <- sma(formula = O2Ar_F ~ O2Ar_NF, data = bio, method = "SMA", slope.test = 1, elev.test = 0)
bioO2.m
# SMA slope = 0.916, CI = 0.859-0.976, p = 0.007 (slope), p = 0.024 (intercept)
gwO2.m <- sma(formula = O2Ar_F ~ O2Ar_NF, data = gw, method = "SMA", slope.test = 1, elev.test = 0)
gwO2.m
# SMA slope = 0.949, CI = 0.833-1.081, p = 0.448 (slope), p < 0.001 (intercept)
pfO2.m <- sma(formula = O2Ar_F ~ O2Ar_NF, data = pf, method = "SMA", slope.test = 1, elev.test = 0)
pfO2.m
# SMA slope = 0.999, CI = 0.968-1.030, p = 0.927 (slope), p = 0.906 (intercept)
sedO2.m <- sma(formula = O2Ar_F ~ O2Ar_NF, data = sed, , method = "SMA", slope.test = 1, elev.test = 0)
sedO2.m
# SMA slope = 1.007, CI = 1.002-1.013, p = 0.006 (slope), p = 0.9997 (intercept)


#Bioassay plot
ggbioO <- ggplot(bio, aes(x = O2Ar_NF, y = O2Ar_F)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(0, 60), ylim = c(0, 60)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = seq(0,60, by = 10)) +
  scale_fill_manual(name = "Treatment", 
                    values = c("#3E3E49", "#E69F00", "#E6E6FA")) +
  geom_segment(x = 1, y = 2.831, xend = 61, yend = 57.491, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", x=35, y=1, label= "SMA Slope = 0.916, CI = 0.859 - 0.976", 
           size = 6) +
  theme(legend.position = c(0.15,0.76), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 8.5, y = 60, label= "A) Bioassay", size = 7)
ggbioO

#groundwater plot
gggwO <- ggplot(gw, aes(x = O2Ar_NF, y = O2Ar_F)) +
  geom_point(aes(fill = Experiment), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(0, 60), ylim = c(0, 60)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = seq(0,60, by = 10)) +
  scale_fill_manual(name = "Treatment", values = c("#E69F00")) +
  geom_segment(x = 2, y = 5.45, xend = 18, yend = 20.602, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", x=35, y=1, label= "SMA Slope = 0.949, CI = 0.833 - 1.081", 
           size = 6) +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 11.5, y = 60, label= "B) Groundwater", size = 7)
gggwO

#lake profile plot
ggproO <- ggplot(pf, aes(x = O2Ar_NF, y = O2Ar_F)) +
  geom_point(aes(fill = Lake), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(0, 60), ylim = c(0, 60)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = seq(0,60, by = 10)) +
  scale_fill_manual(name = "Lake", values = c("#E69F00", "#3E3E49")) +
  geom_segment(x = 2, y = 1.967, xend = 37, yend = 36.932, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", x=35, y=1, label= "SMA Slope = 0.999, CI = 0.968 - 1.030", 
           size = 6) +
  theme(legend.position = c(0.26,0.8), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 11.5, y = 60, label= "C) Lake Profiles", size = 7)
ggproO

#sediment plot
ggsedO <- ggplot(sed, aes(x = O2Ar_NF, y = O2Ar_F)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(0, 60), ylim = c(0, 60)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = seq(0,60, by = 10)) +
  scale_fill_manual(name = "Treatment", labels = c("SW", "+LP", "+N", "+LP +N"), 
                    values = c("#E6E6FA", "#3E3E49", "#0072B2", "#E69F00")) +
  geom_segment(x = 0, y = 1.43E-5, xend = 20.5, yend = 20.644, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", x=35, y=1, label= "SMA Slope = 1.007, CI = 1.002 - 1.013", 
           size = 6) +
  theme(legend.position = c(0.15,0.75), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 17.5, y = 60, label= "D) Sediment Incubation", size = 7)
ggsedO

#Displaying all 4 plots together (using ggpubr)
allO2 <- ggarrange(ggbioO, gggwO, ggproO, ggsedO, 
                   ncol = 2, nrow = 2, align = "hv")
allO2 <- annotate_figure(allO2, bottom = text_grob(
  bquote(~O[2]*':Ar - Furnace Off'), size = 24), 
  left = text_grob(bquote(~O[2]*':Ar - Furnace On'), size = 24, rot = 90))

ggsave("SMA O2Ar All.jpeg", allO2, height = 12, width = 12, units = "in", 
       dpi = 600, bg = "white")

#Sometimes ggsave and annotate_figure encounter a bug that puts the annotation 
#on a black bar so you can't see it. bg = "white" is a workaround.

#------------------------------------------------------------------------------#

#SMA for N2:Ar

bioN2.m <- sma(formula = N2Ar_F ~ N2Ar_NF, data = bio, method = "SMA", slope.test = 1, elev.test = 0)
bioN2.m
# SMA slope = 0.934, CI = 0.891-0.980, p = 0.005 (slope), p = 0.007 (intercept)
gwN2.m <- sma(formula = N2Ar_F ~ N2Ar_NF, data = gw, method = "SMA", slope.test = 1, elev.test = 0)
gwN2.m
# SMA slope = 1.009, CI = 0.949-1.074, p = 0.763 (slope), p = 0.745 (intercept)
pfN2.m <- sma(formula = N2Ar_F ~ N2Ar_NF, data = pf, method = "SMA", slope.test = 1, elev.test = 0)
pfN2.m
# SMA slope = 1.205, CI = 1.107-1.312, p < 0.001 (slope), p < 0.001 (intercept)
sedN2.m <- sma(formula = N2Ar_F ~ N2Ar_NF, data = sed, , method = "SMA", slope.test = 1, elev.test = 0)
sedN2.m
# SMA slope = 1.040, CI = 1.008-1.072, p = 0.014 (slope), p = 0.021 (intercept)


#Bioassay plot
ggbioN <- ggplot(bio, aes(x = N2Ar_NF, y = N2Ar_F)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(30, 46), ylim = c(30, 46)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(30,46, by = 2)) +
  scale_y_continuous(breaks = seq(30,46, by = 2)) +
  scale_fill_manual(name = "Treatment", 
                    values = c("#3E3E49", "#E69F00", "#E6E6FA")) +
  geom_segment(x = 31, y = 31.176, xend = 39, yend = 38.59, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", x=39.25, y=30,
           label= "SMA Slope = 0.934, CI = 0.891 - 0.980", size = 6) +
  theme(legend.position = c(0.15,0.78), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 32.25, y = 46, label= "A) Bioassay", size = 7)
ggbioN

#groundwater
gggwN <- ggplot(gw, aes(x = N2Ar_NF, y = N2Ar_F)) +
  geom_point(aes(fill = Experiment), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(30, 46), ylim = c(30, 46)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(30,46, by = 2)) +
  scale_y_continuous(breaks = seq(30,46, by = 2)) +
  scale_fill_manual(values = c("#E69F00")) +
  geom_segment(x = 33, y = 32.909, xend = 40, yend = 39.986, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", family = "Arial", x=39.25, y=30, 
           label= "SMA Slope = 1.009, CI = 0.949 - 1.074", size = 6) +
  theme(legend.position = "none", axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 33, y = 46, label= "B) Groundwater", size = 7)
gggwN

#lake profile plot
ggproN <- ggplot(pf, aes(x = N2Ar_NF, y = N2Ar_F)) +
  geom_point(aes(fill = Lake), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(30, 46), ylim = c(30, 46)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(30,46, by = 2)) +
  scale_y_continuous(breaks = seq(30,46, by = 2)) +
  scale_fill_manual(name = "Lake", values = c("#E69F00", "#3E3E49")) +
  geom_segment(x = 36, y = 35.528, xend = 40, yend = 40.348, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", family = "Arial", x=39.25, y=30, 
           label= "SMA Slope = 1.205, CI = 1.107 - 1.312", size = 6) +
  theme(legend.position = c(0.26,0.8), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 33, y = 46, label= "C) Lake Profiles", size = 7)
ggproN

#sediment plot
ggsedN <- ggplot(sed, aes(x = N2Ar_NF, y = N2Ar_F)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  coord_cartesian(xlim =c(30, 46), ylim = c(30, 46)) +
  theme_classic(base_size = 20) +
  scale_x_continuous(breaks = seq(30,46, by = 2)) +
  scale_y_continuous(breaks = seq(30,46, by = 2)) +
  scale_fill_manual(name = "Treatment", labels = c("SW", "+LP", "+N", "+LP +N"), 
                    values = c("#E6E6FA", "#3E3E49", "#0072B2", "#E69F00")) +
  geom_segment(x = 37, y = 37.025, xend = 45, yend = 45.345, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "dark gray", linewidth = 0.75) +
  annotate("text", family = "Arial", x=39.25, y=30, 
           label= "SMA Slope = 1.040, CI = 1.008 - 1.072", size = 6) +
  theme(legend.position = c(0.15,0.75), axis.title.x = element_blank(), 
        axis.title.y = element_blank()) +
  annotate("text", x = 34.75, y = 46, label= "D) Sediment Incubation", size = 7)
ggsedN

#Displaying all 4 plots together (using ggpubr)
allN2 <- ggarrange(ggbioN, gggwN, ggproN, ggsedN, ncol = 2, 
                   nrow = 2, align = "hv")
allN2 <- annotate_figure(allN2, bottom = text_grob(
  bquote(~N[2]*':Ar - Furnace Off'), size = 24), 
  left = text_grob(bquote(~N[2]*':Ar - Furnace On'), size = 24, rot = 90))

ggsave("SMA N2Ar All.jpeg", allN2, height = 12, width = 12, units = "in", 
       dpi = 600, bg = "white")


#-------------------------------------------------------------------------------

#O2 Bias Analysis (Figure 3)

#Adding a column in each set of data that is the difference between N2:Ar with 
#the furnace on and the furnace off

bio$Diff <- bio$N2Ar_F - bio$N2Ar_NF
gw$Diff <- gw$N2Ar_F - gw$N2Ar_NF
pf$Diff <- pf$N2Ar_F - pf$N2Ar_NF
sed$Diff <- sed$N2Ar_F - sed$N2Ar_NF

#Graphing each difference as a function of O2:Ar (without the furnace on)
#to check for a pattern

#Bioassays
ggbio <- ggplot(bio, aes(x = O2Ar_NF, y = Diff)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(name = "Treatment:", 
                    values = c("#333349", "#E69F00", "#E6E6FA")) +
  coord_cartesian(xlim =c(0, 60), ylim = c(-2.25, 2.25)) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0,60, by = 6)) +
  scale_y_continuous(breaks = seq(-2,2, by = 1)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = c(0.67,0.1), legend.direction = "horizontal",
        legend.key.size = unit(0.2, "cm"), 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  annotate("text", x = 7, y = 2.25, label= "A) Bioassay", size = 7)
ggbio

#Groundwater
gggw <- ggplot(gw, aes(x = O2Ar_NF, y = Diff)) +
  geom_point(aes(fill = Experiment), shape = 21, color = "black", size = 7) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(name = "Treatment:", values = c("#E6E6FA")) +
  coord_cartesian(xlim =c(0, 20), ylim = c(-2.25, 2.25)) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0,20, by = 2)) +
  scale_y_continuous(breaks = seq(-2,2, by = 1)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = "none") +
  annotate("text", x = 3.25, y = 2.25, label= "B) Groundwater", size = 7)
gggw

#Profiles
ggpf <- ggplot(pf, aes(x = O2Ar_NF, y = Diff)) +
  geom_point(aes(fill = Lake), shape = 21, color = "black", size = 7) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(name = "Lake:", values = c("#333349", "#E6E6FA")) +
  coord_cartesian(xlim =c(0, 40), ylim = c(-2.25, 2.25)) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0,40, by = 4)) +
  scale_y_continuous(breaks = seq(-2,2, by = 1)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = c(0.65,0.1), legend.direction = "horizontal",
        legend.key.size = unit(0.2, "cm"), 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  annotate("text", x = 6.75, y = 2.25, label= "C) Lake Profiles", size = 7)
ggpf

#Sediment
ggsed <- ggplot(sed, aes(x = O2Ar_NF, y = Diff)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(name = "Treatment:", labels = c("SW", "+LP", "+N", "+LP +N"), 
                    values = c("#333349", "#266EC6", "#E69F00","#E6E6FA")) +
  coord_cartesian(xlim =c(0, 20), ylim = c(-2.25, 2.25)) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0,20, by = 2)) +
  scale_y_continuous(breaks = seq(-2,2, by = 1)) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        legend.position = c(0.4,0.1), legend.direction = "horizontal",
        legend.key.size = unit(0.2, "cm"), 
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  annotate("text", x = 5.25, y = 2.25, label= "D) Sediment Incubation", size = 7)
ggsed


#Putting all 4 figures together
allbias <- ggarrange(ggbio, gggw, ggpf, ggsed, ncol = 2, nrow = 2, 
                     align = "hv") 
allbias <- annotate_figure(allbias, bottom = text_grob(bquote(~O[2]*':Ar'), 
                                                       size = 24), left = text_grob(bquote(~Delta*N[2]*':Ar'), 
                                                                                    size = 24, rot = 90))

ggsave("N2Ar Diff vs O2Ar_treatment.jpeg", allbias, height = 8, width = 12, 
       units = "in", dpi = 600, bg = "white")


#-------------------------------------------------------------------------------

#29,30N2 Analysis (Figure 4)

#This analysis is based off two new .csv files (Isotopes.csv and 
#Isotopes_long.csv)

it <- read.csv("Isotopes.csv", header = TRUE)
it_long <- read.csv("Isotopes_long.csv", header = TRUE)

#Paired t test assumes 3 things:
# 1. Two groups are paired. -- Came from the same bottle
# 2. No significant outliers -- no extreme outliers, see below
# 3. Normality - difference of pairs follow a normal distribution

#Compute difference between groups
#m/z 29/28
it29_28 <- it_long %>%
  filter(Gas == "N29_28") %>%
  mutate(diff = On - Off)

#m/z 30/28
it30_28 <- it_long %>%
  filter(Gas == "N30_28") %>%
  mutate(diff = On - Off)


#Identify any outliers
#m/z 29/28
it29_28 %>%
  identify_outliers(diff) #no outliers

#m/z 30/28
it30_28 %>%
  identify_outliers(diff) #no outliers

#Checking normality assumptions
#Shapiro-Wilks normality test for the differences of m/z 29/28
it29_28 %>% shapiro_test(diff)
#qqplot
ggqqplot(it29_28, "diff")
#P value = 0.0378 - differences are not normally distributed

#Shapiro-Wilks normality test for the differences of m/z 30/28
it30_28 %>% shapiro_test(diff)
#qqplot
ggqqplot(it30_28, "diff")
#p value = 0.1627 - can be assumed differences are normally distributed


#From above, both the differences  m/z 30/28 are normally distributed and 
#can be analyzed using a paired t test.

#Is there any significant difference in the mean m/z 30/28 read on the MIMS 
#with the furnace on and off?
ttest_3028 <- it %>%
  t_test(N30_28 ~ Treatment, paired = TRUE) %>%
  add_significance()
ttest_3028 #p < 0.0001 ****


#Since the differences of m/z 29/28 were not normally distributed, going to
#test the assumptions of a Wilcoxon Signed Rank Test, which is that the diffs
#between paired samples are distributed symmetrically around the mean

#Create a histogram
gghistogram(it29_28, x = "diff", y = "..density..", 
            fill = "steelblue", bins = 7, add_density = TRUE)

#Since the histograms are bimodal, a Wilcoxon signed rank test is not 
#appropriate. Therefore, going to use the sign test, which doesn't make any 
#assumptions about the data distributions. The sign test already assumes paired 
#or matched samples, so paired = true is not necessary.

#Is there any significant difference in the mean m/z 29/28 read on the MIMS 
#with the furnace on and off?
signtest_2928 <- it %>%
  sign_test(N29_28 ~ Treatment) %>%
  add_significance()
signtest_2928 #p = 0.442 ns

#Making graphs with the stats on them (Fig 4)
#Adding a parse to show the axes in correct scientific notation based on 
#journal requirements.
scientific_10 <- function(x) {
  parse(text=gsub("e\\+*", " %*% 10^", scales::scientific_format()(x)))
} #uses scales package. Will put axes 5*10^8 instead of 5E8 (for example)


#N29/28
it1 <- ggplot(it, aes(x = O2Ar, y = N29_28)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_fill_manual(name = "", values = c("#333349", "#E6E6FA")) +
  labs(x = NULL, y = bquote(''^29*N[2]*':'^28*N[2])) +
  theme(legend.position = c(0.8,0.95)) +
  theme(aspect.ratio = 1) +
  annotate("text", x = 47, y = 0.012, label= "Sign Test, S(27) = 11", 
           size = 5.5) +
  annotate("text", x = 44, y = 0.0116, label = "p = 0.442, n = 27", 
           size = 5.5) +
  annotate("text", x = 0, y = 0.013, label = "A)", size = 7)
it1

#N30/28
it2 <- ggplot(it, aes(x = O2Ar, y = N30_28)) +
  geom_point(aes(fill = Treatment), shape = 21, color = "black", size = 7) +
  theme_classic(base_size = 18) +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7)) +
  scale_fill_manual(name = "", values = c("#333349", "#E6E6FA")) +
  labs(x = NULL, y = bquote(''^30*N[2]*':'^28*N[2])) +
  theme(legend.position = "none") +
  theme(aspect.ratio = 1) +
  annotate("text", x = 45, y = 0.00195, label= "T-Test, t(26) = -8.2895,", 
           size = 5.5) +
  annotate("text", x = 42, y = 0.00185, label = "p < 0.0001, n = 27", 
           size = 5.5) +
  annotate("text", x = 0, y = 0.00195, label = "C)", size = 7)
it2


#Looking at the differences and plotting over O2:Ar so see if there is an O2 
#bias in measuring 29 and 30 N2
it3 <- ggplot(it29_28, aes(x = O2Ar, y = diff)) +
  geom_point(aes(fill = Gas), shape = 21, color = "black", size = 7) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = "#E6E6FA") +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, by = 10)) +
  scale_y_continuous(limits = c(-0.000625,0.000625), 
                     breaks = scales::pretty_breaks(n = 7), labels = scientific_10) +
  labs(x = NULL, y = bquote(Delta^29*N[2]*':'^28*N[2])) +
  theme_classic(base_size = 18) +
  theme(aspect.ratio = 1, legend.position = "none") +
  annotate("text", x = 0, y = 6E-4, label = "B)", size = 7)
it3

it4 <- ggplot(it30_28, aes(x = O2Ar, y = diff)) +
  geom_point(aes(fill = Gas), shape = 21, color = "black", size = 7) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  scale_fill_manual(values = "#E6E6FA") +
  scale_x_continuous(limits = c(0,60), breaks = seq(0,60, by = 10)) +
  scale_y_continuous(limits = c(-6.25E-4,6.25E-4), 
                     breaks = scales::pretty_breaks(n = 7), labels = scientific_10) +
  labs(x = NULL, y = bquote(Delta^30*N[2]*':'^28*N[2])) +
  theme_classic(base_size = 18) +
  theme(aspect.ratio = 1, legend.position = "none") +
  annotate("text", x = 0, y = 6E-4, label = "D)", size = 7)
it4


#Using ggarrange from the ggpubr package to put all 4 figures together
itfig <- ggarrange(it1, it3, it2, it4, ncol = 2, nrow = 2, align = "hv")
itfig <- annotate_figure(itfig, 
                         bottom = text_grob(bquote(~O[2]*':Ar'), size = 24))

ggsave("Isotope Comps.jpeg", itfig, height = 10, width = 11.5, units = "in", 
       dpi = 600, bg = "white")

#------------------------------------------------------------------------------#

#Looking at histograms of O2:Ar, ∆O2:Ar, N2:Ar, and ∆N2:Ar 
#Bioassays
hist(bio$O2Ar_NF)
hist(bio$O2Ar_F)
hist(bio$DeltO2Ar)
hist(bio$N2Ar_NF)
hist(bio$N2Ar_F)
hist(bio$DeltN2Ar)

#Groundwater
hist(gw$O2Ar_NF)
hist(gw$O2Ar_F)
hist(gw$DeltO2Ar)
hist(gw$N2Ar_NF)
hist(gw$N2Ar_F)
hist(gw$DeltN2Ar)

#Lake profiles
hist(pf$O2Ar_NF)
hist(pf$O2Ar_F)
hist(pf$DeltO2Ar)
hist(pf$N2Ar_NF)
hist(pf$N2Ar_F)
hist(pf$DeltN2Ar)

#Sediment incubations
hist(sed$O2Ar_NF)
hist(sed$O2Ar_F)
hist(sed$DeltO2Ar)
hist(sed$N2Ar_NF)
hist(sed$N2Ar_F)
hist(sed$DeltN2Ar)

#While O2:Ar and N2:Ar are not normally distributed, ∆O2:Ar and ∆N2:Ar
#appear to be closer to a normal distribution
#Check Supplemental Figures 2 and 3 for cleaned up versions of ∆O2:Ar and ∆N2:Ar

#------------------------------------------------------------------------------#

#Supplemental Figures 2 and 3
#Histograms of ∆O2:Ar and ∆N2:Ar

#∆O2:Ar
#Bioassay plot
histbioO <- ggplot(bio) +
  geom_histogram(aes(x = DeltO2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-45,45), breaks = seq(-40,40,20)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -35, y = 49, label = "A) Bioassays", size = 6)
histbioO

#Groundwater
histgwO <- ggplot(gw) +
  geom_histogram(aes(x = DeltO2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-15,15), breaks = seq(-12,12,6)) +
  scale_y_continuous(limits = c(0,30), breaks = seq(0,30,10)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -10.5, y = 30, label = "B) Groundwater", size = 6)
histgwO

#profiles
histpfO <- ggplot(pf) +
  geom_histogram(aes(x = DeltO2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-7,7), breaks = seq(-6,6,3)) +
  scale_y_continuous(limits = c(0,50), breaks = seq(0,50,10)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -5, y = 50, label = "C) Lake Profiles", size = 6)
histpfO

#Sediment
histsedO <- ggplot(sed) +
  geom_histogram(aes(x = DeltO2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,1)) +
  scale_y_continuous(limits = c(0,90), breaks = seq(0,90,15)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -1.6, y = 90, label = "D) Sediment", size = 6) +
  annotate("text", x = -1.41, y = 85, label = "Incubations", size = 6)
histsedO

#Displaying all 4 plots together (using ggpubr)
histDeltO2 <- ggarrange(histbioO, histgwO, histpfO, histsedO, 
                   ncol = 2, nrow = 2, align = "hv")
histDeltO2 <- annotate_figure(histDeltO2, bottom = text_grob(
  bquote('∆'~O[2]*':Ar'), size = 24), 
  left = text_grob(bquote('Frequency'), size = 24, rot = 90))

ggsave("Delta O2Ar Histogram.jpeg", histDeltO2, height = 12, width = 12, units = "in", 
       dpi = 600, bg = "white")



#∆N2:Ar
#Bioassay plot
histbioN <- ggplot(bio) +
  geom_histogram(aes(x = DeltN2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,1)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -1.55, y = 21, label = "A) Bioassays", size = 6)
histbioN

#Groundwater
histgwN <- ggplot(gw) +
  geom_histogram(aes(x = DeltN2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-1.5,1.5), breaks = seq(-1.5,1.5,0.5)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -1.05, y = 43, label = "B) Groundwater", size = 6)
histgwN

#profiles
histpfN <- ggplot(pf) +
  geom_histogram(aes(x = DeltN2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,1)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -1.4, y = 43, label = "C) Lake Profiles", size = 6)
histpfN

#Sediment
histsedN <- ggplot(sed) +
  geom_histogram(aes(x = DeltN2Ar), color = "black", fill = "lightgray", bins = 25) +
  scale_x_continuous(limits = c(-2,2), breaks = seq(-2,2,1)) +
  scale_y_continuous(limits = c(0,132), breaks = seq(0,132,25)) +
  theme_bw(base_size = 16) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  annotate("text", x = -1.6, y = 132, label = "D) Sediment", size = 6) +
  annotate("text", x = -1.39, y = 125, label = "Incubations", size = 6)
histsedN

#Displaying all 4 plots together (using ggpubr)
histDeltN2 <- ggarrange(histbioN, histgwN, histpfN, histsedN, 
                        ncol = 2, nrow = 2, align = "hv")
histDeltN2 <- annotate_figure(histDeltN2, bottom = text_grob(
  bquote('∆'~N[2]*':Ar'), size = 24), 
  left = text_grob(bquote('Frequency'), size = 24, rot = 90))

ggsave("Delta N2Ar Histogram.jpeg", histDeltN2, height = 12, width = 12, units = "in", 
       dpi = 600, bg = "white")


#-------------------------------------------------------------------------------
