library(ggplot2)
library(cowplot)

# import and QC data
dat_2016 <- read.csv("CWQT 2016- ALL SITES.csv")
head(dat_2016)
str(dat_2016)

# all sites, all dates rainfall_1 vs. enter0 
plot <- ggplot(data=dat_2016, aes(x=rainfall_1,y=entero_MPN_100_per_mL)) + geom_point()
plot

# all sites, all dates rainfall_pres_24 vs. enter0 
plot <- ggplot(data=dat_2016, aes(x=rainfall_pres_24,y=entero_MPN_100_per_mL)) + geom_point(position="jitter")
plot

# one plot for each site 
# rainfall_pres_24, rainfall_pres_48, rainfall_pres_72 vs. entero_MPN_100_per_mL
site_list <- unique(dat_2016$site)

for (i in seq_along(site_list)){
  
  temp_data <- subset(dat_2016, dat_2016$site==site_list[i])
  
  plot24 <- ggplot(data=temp_data, aes(x=rainfall_pres_24, y=entero_MPN_100_per_mL)) + geom_jitter(width=0.075, alpha=0.8) 
  plot24 <- plot24 + stat_summary(fun.data="mean_cl_boot", colour="red")
  plot24 <- plot24 + xlab("Rain last 24 hr?") + ylab("Entero MPN per 100 mL")
  plot24 <- plot24 + geom_hline(yintercept=35, linetype=2, colour="grey") + geom_hline(yintercept=104, linetype=2, colour="grey")
  plot24 <- plot24 + ggtitle(site_list[i])
  plot24 
  
  plot48 <- ggplot(data=temp_data, aes(x=rainfall_pres_48, y=entero_MPN_100_per_mL)) + geom_jitter(width=0.075, alpha=0.8) 
  plot48 <- plot48 + stat_summary(fun.data="mean_cl_boot", colour="red")
  plot48 <- plot48 + xlab("Rain last 48 hr?") + ylab("Entero MPN per 100 mL")
  plot48 <- plot48 + geom_hline(yintercept=35, linetype=2, colour="grey") + geom_hline(yintercept=104, linetype=2, colour="grey")
  plot48 
  
  plot72 <- ggplot(data=temp_data, aes(x=rainfall_pres_72, y=entero_MPN_100_per_mL)) + geom_jitter(width=0.075, alpha=0.8) 
  plot72 <- plot72 + stat_summary(fun.data="mean_cl_boot", colour="red")
  plot72 <- plot72 + xlab("Rain last 72 hr?") + ylab("Entero MPN per 100 mL")
  plot72 <- plot72 + geom_hline(yintercept=35, linetype=2, colour="grey") + geom_hline(yintercept=104, linetype=2, colour="grey")
  plot72 
  
  plot_all <- plot_grid(plot24, plot48, plot72, align="v", nrow=3)
  
  save_plot(paste("plots/",site_list[i],"_entero_vs_rain_pres.jpg"), plot_all, ncol=1, base_height=9,base_width=5)
                 
}


# one plot for each site 
# rainfall_sum_24, rainfall_sum_48, rainfall_sum_72 vs. entero_MPN_100_per_mL
site_list <- unique(dat_2016$site)

for (i in seq_along(site_list)){
  
  temp_data <- subset(dat_2016, dat_2016$site==site_list[i])
  
  plot24 <- ggplot(data=temp_data, aes(x=rainfall_sum_24, y=entero_MPN_100_per_mL)) + geom_point() + geom_smooth(method="lm")
  plot24 <- plot24 + geom_hline(yintercept=35, linetype=2, colour="grey") + geom_hline(yintercept=104, linetype=2, colour="grey")
  plot24 <- plot24 + xlab("24hr Rainfall total (in)") + ylab("Entero MPN per 100 mL")
  plot24 <- plot24 + ggtitle(site_list[i])
  plot24 
  
  plot48 <- ggplot(data=temp_data, aes(x=rainfall_sum_48, y=entero_MPN_100_per_mL)) + geom_point() + geom_smooth(method="lm")
  plot48 <- plot48 + xlab("48hr Rainfall total (in)") + ylab("Entero MPN per 100 mL")
  plot48 <- plot48 + geom_hline(yintercept=35, linetype=2, colour="grey") + geom_hline(yintercept=104, linetype=2, colour="grey")
  plot48 
  
  plot72 <- ggplot(data=temp_data, aes(x=rainfall_sum_72, y=entero_MPN_100_per_mL)) + geom_point() + geom_smooth(method="lm") 
  plot72 <- plot72 + xlab("72hr Rainfall total (in)") + ylab("Entero MPN per 100 mL")
  plot72 <- plot72 + geom_hline(yintercept=35, linetype=2, colour="grey") + geom_hline(yintercept=104, linetype=2, colour="grey")
  plot72 
  
  plot_all <- plot_grid(plot24, plot48, plot72, align="v", nrow=3)
  
  save_plot(paste("plots/",site_list[i],"_entero_vs_rain_sum.jpg"), plot_all, ncol=1, base_height=9, base_width=5)
  
}