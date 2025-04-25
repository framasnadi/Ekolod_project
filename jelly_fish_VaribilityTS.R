#############################################################
#                                                           #
#    Jelly vs Fish ST variability  approach                 #
#                                                           #
#   Author: Francesco Masnadi (DEEP, Stockholm University)  #
#                                                           #
#############################################################

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(rstudioapi)
library(ggpubr)
library(tidyr)
library(patchwork)

# load dbs (from 200khz freq)
jellyTsdb <- read_excel("C:/Users/frma6502/Desktop/SU/EKOLOD/ESP3output/test/200STjellytest_Aug_09_2022.xlsx",  sheet = "Single Targets")
fishTSdb <- read_excel("C:/Users/frma6502/Desktop/SU/EKOLOD/ESP3output/test/200STfishtest_Aug_09_2022.xlsx",  sheet = "Single Targets") 

# create ID and merge dbs
jellyTsdb$Type <- "Jelly"
jellyTsdb$ID <- interaction(jellyTsdb$Type, jellyTsdb$Track_ID) 
fishTSdb$Type <- "Fish"
fishTSdb$ID <- interaction(fishTSdb$Type, fishTSdb$Track_ID) 
#fishTSdb  <- fishTSdb %>% dplyr::filter(Target_range > 6 & Target_range <= 25) 
TSdb <- rbind(fishTSdb,jellyTsdb) %>% dplyr::filter(ID != "NA")

# Depth histogram
 ggplot( TSdb , aes(x = Target_range, fill = Type, color=Type)) + geom_density(position = "identity", alpha=0.4)+xlab("Depth")+theme_classic()+ theme(legend.position = "bottom")+ylab("Density") 
# reshape the final db based on the depth layer per species
jellyTsdb_layer <- jellyTsdb %>% filter(Target_range  < 10.1)
fishTSdb_layer <- fishTSdb %>% filter(Target_range  > 24)
TSdb <- rbind(fishTSdb_layer,jellyTsdb_layer) %>% dplyr::filter(ID != "NA")
depthplot <- ggplot( TSdb , aes(x = Target_range, fill = Type, color=Type)) + geom_density(position = "identity", alpha=0.4)+xlab("Depth")+theme_classic()+ theme(legend.position = "bottom")+ylab("Density") ; depthplot

#Subset <- TSdb %>% filter(Track_ID %in% c(32:36))
#ggplot( TSdb , aes(x = as.factor(Track_ID), y = TS_comp, color = Type)) + geom_boxplot(na.rm = T) #+ facet_wrap(~Type)
TSdb_var <- TSdb %>% 
  group_by(Type,  ID)  %>% 
  summarise(
    npings = n(),
    "Mean TS" = mean(TS_comp, na.rm = T),
    SD = sd(TS_comp, na.rm = T),
   "CV%" = abs(SD/`Mean TS`)*100 ,
    Span = max(TS_comp) - min(TS_comp),
    IQR =  IQR(TS_comp) # Interquartile Range (IQR = Q3 − Q1): The distance between the first and third quartiles—the interquartile range (IQR)—is a measure of variability. It indicates the spread of the middle 50% of the data. The IQR is an especially good measure of variability for skewed distributions or distributions with outliers
  )%>%
  mutate( avg.mean = median(`Mean TS`),
         avg.SD = median(SD),
         "avg.CV" = median(`CV%`),
         avg.span = median(Span),
         avg.IQR = median(IQR)
         )


# plots variability
levels_all <- levels(TSdb_var$ID)
# Get the index of the midpoint of fish and jelly IDs
fish_ids <- grep("^Fish", levels_all)
jelly_ids <- grep("^Jelly", levels_all)
fish_mid <- levels_all[round(mean(fish_ids))]
jelly_mid <- levels_all[round(mean(jelly_ids))]


CVplot <- ggplot( TSdb_var , aes(x = ID, y = `CV%`, color = Type)) + geom_point(alpha=0.15) +
  geom_hline( aes(yintercept = avg.CV, color = Type), 
              linetype = "dashed", show.legend = FALSE)+
  scale_x_discrete(
    breaks = c(fish_mid, jelly_mid),
    labels = c("Fish", "Jelly")
  )  +
  theme_classic()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL);CVplot


meanplot <- ggplot( TSdb_var , aes(x = ID, y = `Mean TS`, color = Type)) + geom_point(alpha=0.15) +
  geom_hline( aes(yintercept = avg.mean, color = Type), 
             linetype = "dashed", show.legend = FALSE)+
  scale_x_discrete(
    breaks = c(fish_mid, jelly_mid),
    labels = c("Fish", "Jelly")
  )  +
  theme_classic()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL)

spanplot <- ggplot( TSdb_var , aes(x = ID, y = Span, color = Type)) + geom_point(alpha=0.15) +
  geom_hline( aes(yintercept = avg.span, color = Type), 
              linetype = "dashed", show.legend = FALSE)+
  scale_x_discrete(
    breaks = c(fish_mid, jelly_mid),
    labels = c("Fish", "Jelly")
  )  +
  theme_classic()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL)

IQRplot <- ggplot( TSdb_var , aes(x = ID, y = IQR, color = Type)) + geom_point(alpha=0.15) +
  geom_hline( aes(yintercept = avg.IQR, color = Type), 
              linetype = "dashed", show.legend = FALSE)+
  scale_x_discrete(
    breaks = c(fish_mid, jelly_mid),
    labels = c("Fish", "Jelly")
  )  +
  theme_classic()+
  theme(legend.position = "none",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = NULL)

# Boxplots and violin plot
meanTSviolplot <-ggplot( TSdb_var , aes(x = Type, y = `Mean TS`, color = Type))  +  
  #geom_sina(alpha = 0.6, size= 2)+
  geom_violin(draw_quantiles = c(0.5 ) , aes(color = Type, fill = Type), alpha=0.4)+
  theme_classic() +
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +xlab(NULL)+ylab(NULL)

spanTSviolplot <-ggplot( TSdb_var , aes(x = Type, y = Span, color = Type))  +  
  #geom_sina(alpha = 0.6, size= 2)+
  geom_violin(draw_quantiles = c(0.5 ) , aes(color = Type, fill = Type), alpha=0.4)+
  theme_classic() +
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +xlab(NULL)+ylab(NULL)

IQRviolplot <-ggplot( TSdb_var , aes(x = Type, y = IQR, color = Type))  +  
 # geom_sina(alpha = 0.6, size= 2)+
  geom_violin(draw_quantiles = c(0.5 ) , aes(color = Type, fill = Type), alpha=0.4)+
  theme_classic() + 
  theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +xlab(NULL)+ylab(NULL)


# plot all together
combined_plot <- depthplot / (meanplot | meanTSviolplot)  /  (spanplot | spanTSviolplot) / (IQRplot | IQRviolplot)+ plot_layout(guides = "collect") & theme(legend.position = "bottom"); combined_plot

# Use non-parametric tests such as the Wilcoxon rank-sum test to test difference in variability metrics/proxies (e.g., SD or CV or IQR) between fish and jellyfish tracks.
wilcox.test(`Mean TS` ~ Type, data = TSdb_var)
wilcox.test(IQR ~ Type, data = TSdb_var)
#summary(aov(IQR ~ Type, data = TSdb_var))



# correlation npings vs variability
ggscatter(TSdb_var, x = "npings", y = "IQR",  color = "Type",
          add = "reg.line",  # Add regressin line
          add.params = list(color = "black", fill = "lightgray"), # Customize reg. line
          conf.int = TRUE, # Add confidence interval
         # title = "TP vs Phe",
)+ stat_cor(method = "spearman",alternative = "two.sided", digits = 1)+theme(legend.position = "right")+facet_wrap(~Type,scales= "free")


