library(ggplot2)
library(ggpubr)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

############################### Prepare data ###################################
#Upload data from files:
df <- read.csv("Data/main_data.csv", header = TRUE)

#Calculate relative improvement of CC-score and PAV-score of LV vs AV:
df$relative_increase_cc <- df$cc_lv / df$cc_av
df$relative_increase_pav <- df$pav_lv / df$pav_av

# Transform values into factors for making boxplots:
df$phi <- factor(df$phi, labels = c(0, 0.25, 0.5, 0.75, 1))
df$k <- factor(df$k, labels = c(8, 12, 16))
df$g <- factor(df$g, labels = c('g = 2', 'g = 6', 'g = 10'))
# add a column that indicates l relative to k:
df$lk <- with(df, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', 'l = k/2')))

#### with phi between 0 and 0.25: ####
df_small_phi <- read.csv("Data/small_phi.csv", header = TRUE)

# same data preparation as above:
df_small_phi$relative_increase_cc <- df_small_phi$cc_lv / df_small_phi$cc_av
df_small_phi$relative_increase_pav <- df_small_phi$pav_lv / df_small_phi$pav_av
df_small_phi$phi <- factor(df_small_phi$phi, labels = c(0.05, 0.1, 0.15, 0.2))
df_small_phi$k <- factor(df_small_phi$k, labels = c(8, 12, 16))
df_small_phi$g <- factor(df_small_phi$g, labels = c('g = 2', 'g = 6', 'g = 10'))
df_small_phi$lk <- with(df_small_phi, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', 'l = k/2')))

#To make a plot with more values of phi
## main data and for small phi combined
df_all_phi <- rbind(df[df$phi == 0,], df_small_phi)
df_all_phi <- rbind(df_all_phi, df[df$phi != 0,])

################################################################################

### diversity / CC-improvement ###

### CC-improvement boxplots with values of phi separated (0, 0.25, 0.5, 0.75, 1): 
ggplot(df, aes(x=phi, y=relative_increase_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "CC-improvement") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("CC-improvement, 1500 voters, 24 candidates, 2000 runs")

### CC-improvement boxplots with all values of phi:
### (used in paper)
dfx_all <- df_all_phi
### stick high values of phi together since they are all around 1:
dfx_all$phi[dfx_all$phi == 0.75] <- 0.5
dfx_all$phi[dfx_all$phi == 1] <- 0.5
dfx_all$phi[dfx_all$phi == 0.25] <- 0.5

ggplot(dfx_all, aes(x=factor(phi, levels=c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]')), y=relative_increase_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8), name = "CC-improvement", expand = expansion(mult = 0.01), limits = c(0, 7.5)) +
  scale_x_discrete(name = expression(phi), labels = c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]'))+
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))

### Same plot (CC-improvement) with values of g and l combined 
### (used in new version of paper)
ggplot(dfx_all, aes(x=factor(phi, levels=c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]')), y=relative_increase_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  #facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8), name = "CC-improvement", expand = expansion(mult = 0.01), limits = c(0, 7.5)) +
  scale_x_discrete(name = expression(phi), labels = c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]'))+
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))

### The CC-improvement for phi = 0 seems to be divided in two groups, one at 1 and one at 2.
### Density plot to explore this:
# (used in paper)
ggplot(df[df$phi == 0,], aes(x = relative_increase_cc)) + 
  geom_density() + 
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4, 4.5, 5), name = 'CC-improvement', limits = c(0,5)) + 
  scale_y_continuous(name = 'density') +
  theme(axis.text=element_text(size=14), 
        axis.title=element_text(size=15))

# Consider only the data for phi=0 and g!=2, to test the influence of other variables:
# Remove data for phi >0:
df0 <- df[df$phi==0,]
# Remove data for g = 2:
df0g <- df0[df0$g!='g = 2',]
# We use the logarithm since it is probably normally distributed, which is necessary for an ANOVA:
df0g$log_cc <- log(df0g$relative_increase_cc)
ggplot(df0g, aes(x=phi, y= log_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "CC-score(LV)/CC-score(AV)") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("Logarithm of CC-improvement,\n1500 voters, 24 candidates, 2000 runs, phi = 0, g = 6 or g = 10")+
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))
# The boxplots look normally distributed, we look at the qq-plots:
ggqqplot(df0g[df0g$k==8,][df0g$g == 'g = 10',][df0g$lk == 'l = 1',]$log_cc) # alter the relevant values to make all plots
# The data are more or less normally distributed. We can do an ANOVA:
res_cc_log <- aov(log_cc ~ g+ k+lk+g*lk+g*k+lk*k, data = df0g)
summary(res_cc_log)  #ANOVA shows that all variables are relevant.
# We do an eta squared test to measure the effect sizes of the variables:
eta_test_cc <- lsr::etaSquared(res_cc_log) #most relevant in explaining the variance: k, l, g
# xtable(eta_test_cc) # (For copying the results to LateX)

### proportionality / PAV-improvement ###

### PAV-improvement boxplots with larger values of phi separated: 
ggplot(df, aes(x=phi, y=relative_increase_pav, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "PAV-improvement") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("PAV-improvement, 1500 voters, 24 candidates, 2000 runs")


### PAV-improvement boxplots with all values of phi:
# values of phi of 0.25 and higher are all around 1, so we take them together.
### (used in paper)
ggplot(dfx_all, aes(x=factor(phi, levels=c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]')), y=relative_increase_pav, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(1,2), name = "PAV-improvement", expand = expansion(mult = 0.01), limits = c(0.5,2.8)) +
  scale_x_discrete(name = expression(phi), labels = c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]'))+
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))

# Consider only the data for phi=0, to test the influence of other variables:
# We use the logarithm since it is probably normally distributed, which is necessary for an ANOVA:
df0$log_pav <- log(df0$relative_increase_pav)
ggplot(df0, aes(x=phi, y= log_pav, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "PAV-score(LV)/PAV-score(AV)") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("Logarithm of PAV-improvement,\n1500 voters, 24 candidates, 2000 runs, phi = 0")
# Look normally distributed, look at qq-plot:
ggqqplot(df0[df0$k==12,][df0$g == 'g = 2',][df0$lk == 'l = k',]$log_pav) # alter the relevant values to make all plots
# Most of them are more or less normally distributed. We do an ANOVA:
res_pav_log <- aov(log_pav ~ g+ k+lk+g*lk+g*k+lk*k, data = df0)
summary(res_pav_log)  #ANOVA shows that all variables are relevant.
# We do an eta squared test to measure the effect sizes of the variables:
eta_test_pav <- lsr::etaSquared(res_pav_log) #most relevant in explaining the variance: l, then k and the interaction of g and l, then g
# xtable(eta_test_pav)


############################################################################
# We also looked at the welfare loss of using better (small) l.
#Data with AV-scores:
df_av <- read.csv("Data/with_AV_scores.csv", header = TRUE)

# Prepare data:
df_av$relative_increase_av <- df_av$av_lv / df_av$av_av
# If you want the loss instead of the increase (the inverse):
#df_av$relative_increase_av <- df_av$av_av / df_av$av_lv

df_av$phi <- factor(df_av$phi, labels = c(0, 0.25, 0.5, 0.75, 1))
df_av$k <- factor(df_av$k, labels = c(8, 12, 16))
df_av$g <- factor(df_av$g, labels = c('g = 2', 'g = 6', 'g = 10'))
# add a column that indicates l relative to k:
df_av$lk <- with(df_av, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', 'l = k/2')))

### main AV-loss plot: ###
ggplot(df_av, aes(x=phi, y=relative_increase_av, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  #facet_grid(lk ~ g) + # horizontal different values of g, vertical values of l.
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(name = "AV-'increase'") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("AV-loss, 1500 voters, 24 candidates, 2000 runs")

# Is there a significant difference in means between the different values of l?
mean(df_av[df_av$lk == 'l = 1',]$relative_increase_av) # 0.9743952
mean(df_av[df_av$lk == 'l = k/2',]$relative_increase_av) # 9795545
mean(df_av[df_av$lk == 'l = k',]$relative_increase_av) # 0.9785943
res_av_l <- aov(relative_increase_av ~ l, data = df_av)
summary(res_av_l)
lsr::etaSquared(res_av_l) # eta^2=0.046: small to medium effect.

df_av0 <- df_av[df_av$phi==0,]
mean(df_av0[df_av0$lk == 'l = 1',]$relative_increase_av) # 0.968366
mean(df_av0[df_av0$lk == 'l = k/2',]$relative_increase_av) # 0.9866517
mean(df_av0[df_av0$lk == 'l = k',]$relative_increase_av) # 0.9708888
res_av_l0 <- aov(relative_increase_av ~ l, data = df_av0)
summary(res_av_l0)
lsr::etaSquared(res_av_l0) # eta^2=0.011: small effect.

###!!! NOTE: for l=1 the mean is lowest, but then for l=k. For l=k/2 it is highest !!!###

####################
### Check the same (AV-improvement) with more different l, only for phi=0, with voters still choosing parties uniformly at random:
df_l <- read.csv("Data/AV_scores_more_l.csv", header = TRUE)
df_l$relative_increase_av <- df_l$av_lv / df_l$av_av

# add a column that indicates l relative to k:
df_l$lk <- with(df_l, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', ifelse(l == k/2, 'l = k/2', ifelse(l == k/4, 'l = k/4', 'l = 3k/4')))))

#df_l$phi <- factor(df_l$phi, labels = c(0, 0.1, 0.25))
df_l$k <- factor(df_l$k, labels = c(8, 12, 16))
df_l$g <- factor(df_l$g, labels = c('g = 2', 'g = 6', 'g = 10'))

# (used in paper)
ggplot(df_l, aes(x=factor(lk, levels=c('l = 1', 'l = k/4', 'l = k/2', 'l = 3k/4', 'l = k')), y=relative_increase_av)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_wrap(df_l$g ) + 
  scale_y_continuous(name = "AV-improvement") +
  scale_x_discrete(name = "l (relative to k)", labels = c('1', '0.25k', '0.5k', '0.75k', 'k')) +
  theme(axis.text=element_text(size=17), 
        axis.title=element_text(size=18), 
        strip.text.x = element_text(size = 17))
### Same as below (random partition) but much smaller effect! ###


################################################################################
### Voters partitioned randomly over parties ###

# We ran the original experiment again, but now with voters not being assigned 
# uniformly at random to a party, but randomly partitioned over the parties 
# (choosing a partition uniformly at random from all the possible partitions).
# This causes parties that are less similar in numbers of voters. 
df_rp <- read.csv("Data/main_data_RP.csv", header = TRUE)

df_rp$relative_increase_cc <- df_rp$cc_lv / df_rp$cc_av
df_rp$relative_increase_pav <- df_rp$pav_lv / df_rp$pav_av
df_rp$phi <- factor(df_rp$phi, labels = c(0, 0.25, 0.5, 0.75, 1))
df_rp$k <- factor(df_rp$k, labels = c(8, 12, 16))
df_rp$g <- factor(df_rp$g, labels = c('g = 2', 'g = 6', 'g = 10'))
# add a column that indicates l relative to k:
df_rp$lk <- with(df_rp, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', 'l = k/2')))

############################ CC ######################################################
### main CC-improvement boxplots: ###
ggplot(df_rp, aes(x=phi, y=relative_increase_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  #facet_grid(lk ~ g) + # horizontal different values of g, vertical values of l.
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "CC-improvement") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("CC-improvement, 1500 voters, 24 candidates, 2000 runs")

### small phi for random voter partition ###
df_small_phi_RP <- read.csv("Data/small_phi_RP.csv", header = TRUE)
df_small_phi_RP$relative_increase_cc <- df_small_phi_RP$cc_lv / df_small_phi_RP$cc_av
df_small_phi_RP$relative_increase_pav <- df_small_phi_RP$pav_lv / df_small_phi_RP$pav_av
df_small_phi_RP$phi <- factor(df_small_phi_RP$phi, labels = c(0.05, 0.1, 0.15, 0.2))
df_small_phi_RP$k <- factor(df_small_phi_RP$k, labels = c(8, 12, 16))
df_small_phi_RP$g <- factor(df_small_phi_RP$g, labels = c('g = 2', 'g = 6', 'g = 10'))
# add a column that indicates l relative to k:
df_small_phi_RP$lk <- with(df_small_phi_RP, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', 'l = k/2')))
df_small_phi_RP <- subset(df_small_phi_RP, select=-c(av_lv,av_av)) #remove the av columns to be able to stick together with other frame

df_all_phi_RP <- rbind(df_rp[df_rp$phi == 0,], df_small_phi_RP)
df_all_phi_RP <- rbind(df_all_phi_RP, df_rp[df_rp$phi != 0,])
dfx_all_RP <- df_all_phi_RP
# combine phi greater than 0 on the x-axis:
dfx_all_RP$phi[dfx_all_RP$phi == 0.75] <- 0.5
dfx_all_RP$phi[dfx_all_RP$phi == 1] <- 0.5
dfx_all_RP$phi[dfx_all_RP$phi == 0.25] <- 0.5

# CC-improvement plot with all phi combined for random voter partition
# (used in paper)
ggplot(dfx_all_RP, aes(x=factor(phi, levels=c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]')), y=relative_increase_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4,5), name = "CC-improvement", expand = expansion(mult = 0.01), limits = c(0, 6)) +
  scale_x_discrete(name = expression(phi), labels = c('0', '0.05', '0.1', '0.15', '0.2', '[0.25, 1]'))+
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))

#Look at only phi = 0, since others are all around 1.
df0_rp <- df_rp[df_rp$phi==0,]
#Remove data for g = 2:
df0_rp <- df0_rp[df0_rp$g!='g = 2',]
#plot the logarithm, maybe it is normally distributed:
df0_rp$log_cc <- log(df0_rp$relative_increase_cc)
ggplot(df0_rp, aes(x=phi, y= log_cc, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "CC-improvement") +
  scale_x_discrete(name = expression(phi)) +
  ggtitle("Logarithm of CC-improvement,\n1500 voters, 24 candidates, 2000 runs, phi = 0")
# Result: roughly normally distributed, least for l=1.

# ANOVA: 
res_cc_log <- aov(log_cc ~ g+ k+lk+g*lk+g*k+lk*k, data = df0_rp)
summary(res_cc_log)  #ANOVA shows that all variables are relevant.
eta_test_cc <- lsr::etaSquared(res_cc_log) #most relevant in explaining the variance: k, l, g
eta_test_cc
# Result: l is explaining most variance, k also a bit.
#xtable(eta_test_cc)


############################ PAV ######################################################

### main PAV-improvement boxplots: ###
# (used in paper) (For larger phi there is already a difference, so it is less interesting to look at phi between 0 and .25.)
ggplot(df_rp, aes(x=phi, y=relative_increase_pav, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85), outlier.size = 0.8) + 
  #facet_grid(lk ~ g) + # horizontal different values of g, vertical values of l.
  facet_grid(factor(lk, levels=c('l = 1', 'l = k/2', 'l = k')) ~ g) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(breaks=c(0,1,2,3,4, 5,6,7,8,9,10), name = "PAV-improvement", expand = expansion(mult = 0.01)) +
  scale_x_discrete(name = expression(phi)) +
  #ggtitle("PAV-improvement, 1500 voters, 24 candidates, 2000 runs") 
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))
# Looks roughly normally distributed. We can do an ANOVA, also for phi=0 and g>2 (for g=2 the l=k puts too much weight on 1):
res_pav <- aov(relative_increase_pav ~ g+ k+lk+g*lk+g*k+lk*k, data = df0_rp)
summary(res_pav)
eta_test_pav <- lsr::etaSquared(res_pav)
eta_test_pav
# Result: l is explaining most variance, then g.
#xtable(eta_test_pav)


#####################################
### Welfare loss / AV-improvement of using small l, when phi=0
# (same analysis as above in line 206 but with voters in random partition):
df_l_RP <- read.csv("Data/AV_scores_more_l_RP.csv", header = TRUE)
df_l_RP$relative_increase_av <- df_l_RP$av_lv / df_l_RP$av_av
df_l_RP$lk <- with(df_l_RP, ifelse(l==1, 'l = 1', ifelse(l==k, 'l = k', ifelse(l == k/2, 'l = k/2', ifelse(l == k/4, 'l = k/4', 'l = 3k/4')))))
df_l_RP$k <- factor(df_l_RP$k, labels = c(8, 12, 16))
df_l_RP$g <- factor(df_l_RP$g, labels = c('g = 2', 'g = 6', 'g = 10'))

### main AV-increase plot: ###
ggplot(df_l_RP, aes(x=phi, y=relative_increase_av, fill=k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_grid(g ~ factor(lk, levels=c('l = 1', 'l = k/4', 'l = k/2', 'l = 3k/4', 'l = k'))) + # 'levels' is to fix the order of lk
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  scale_y_continuous(name = "AV-increase") +
  scale_x_discrete(name = "phi") 

mean(df_l_RP[df_l_RP$lk == 'l = 1',]$relative_increase_av) # 0.7239583
mean(df_l_RP[df_l_RP$lk == 'l = k/4',]$relative_increase_av) # 0.8359995
mean(df_l_RP[df_l_RP$lk == 'l = k/2',]$relative_increase_av) # 0.8913257
mean(df_l_RP[df_l_RP$lk == 'l = 3k/4',]$relative_increase_av) # 0.87903
mean(df_l_RP[df_l_RP$lk == 'l = k',]$relative_increase_av)# 0.8211113
res_l <- aov(relative_increase_av ~ l, data = df_l_RP)
res_l
summary(res_l)
lsr::etaSquared(res_l) # partial eta squared: 0.06822781: medium effect

### (used in paper:)
ggplot(df_l_RP, aes(x=factor(lk, levels=c('l = 1', 'l = k/4', 'l = k/2', 'l = 3k/4', 'l = k')), y=relative_increase_av)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_wrap(df_l_RP$g ) + 
  scale_y_continuous(name = "AV-improvement") +
  scale_x_discrete(name = "l (relative to k)", labels = c('1', '0.25k', '0.5k', '0.75k', 'k')) +
  theme(axis.text=element_text(size=17), 
        axis.title=element_text(size=18), 
        strip.text.x = element_text(size = 17))
# like the same plot for no random partition, but stronger (see difference in y-axis scale!)

# With k split up:
ggplot(df_l_RP, aes(x=factor(lk, levels=c('l = 1', 'l = k/4', 'l = k/2', 'l = 3k/4', 'l = k')), y=relative_increase_av, fill = k)) + 
  geom_boxplot(position=position_dodge(0.85)) + 
  facet_wrap(df_l_RP$g ) + 
  scale_y_continuous(name = "AV-'improvement'") +
  scale_x_discrete(name = "l relative to k") +
  scale_fill_manual(values=c("#99C1C1","#00827F","#045D5D")) +
  theme(axis.text=element_text(size=13), 
        axis.title=element_text(size=14), 
        strip.text.x = element_text(size = 13), 
        strip.text.y = element_text(size = 13), 
        legend.text=element_text(size=13), 
        legend.title=element_text(size=13))
