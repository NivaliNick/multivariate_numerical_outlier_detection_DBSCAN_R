# Load packages
required_packages <- c('dplyr','ggpubr','ggplot2','ggtext','PupillometryR','fpc','dbscan', 'ggMarginal','plotly')

lapply(required_packages, require, character.only = TRUE)

# Load red wine quality dataset
wine_quality_red <- read_delim('winequality-red.csv',delim = ';')

# Remove duplicate rows
wine_quality_red <- unique(wine_quality_red)

# Create row id 
wine_quality_red$row_id <- 1:nrow(wine_quality_red)

# Plot individual distributions of density, alcohol and pH
density_distr <- ggplot(wine_quality_red, aes(x = '', y = density)) +
  geom_flat_violin(position = position_nudge(x = -.18))+
  geom_boxplot(width=0.1, position = position_nudge(x = -.18), outlier.colour = '#F8766D', outlier.alpha = 0.5) +
  coord_flip() + 
  labs(title = "<b>Density (g/mL)</b>") +
  xlab('') + 
  ylab('') +theme_minimal() + theme(
    plot.title = element_markdown(margin = margin(10,0,0,0), size = 12),
    panel.grid.major.y = element_blank()
  )

alcohol_distr <- ggplot(wine_quality_red, aes(x = '', y = alcohol)) +
  geom_flat_violin(position = position_nudge(x = -.18))+
  geom_boxplot(width=0.1, position = position_nudge(x = -.18), outlier.colour = '#F8766D', outlier.alpha = 0.5) +
  coord_flip() + 
  labs(title = "<b>Alcohol (%)</b>") +
  xlab('') + 
  ylab('') +theme_minimal() + theme(
    plot.title = element_markdown(margin = margin(10,0,0,0), size = 12),
    panel.grid.major.y = element_blank()
  )

pH_distr <- ggplot(wine_quality_red, aes(x = '', y = pH)) +
  geom_flat_violin(position = position_nudge(x = -.18))+
  geom_boxplot(width=0.1, position = position_nudge(x = -.18), outlier.colour = '#F8766D', outlier.alpha = 0.5) +
  coord_flip() + 
  labs(title = "<b>pH</b>") +
  xlab('') + 
  ylab('') +theme_minimal() + theme(
    plot.title = element_markdown(margin = margin(10,0,0,0), size = 12),
    panel.grid.major.y = element_blank()
  )

combined_ind_distr <- ggarrange(density_distr, alcohol_distr, pH_distr,
          ncol = 1, nrow = 3) +
  theme(plot.margin = margin(0.5,2.5,0.5,2.5, "cm"))

# Prepare data for DBSCAN
wine_quality_red_sel <- wine_quality_red %>%
  select(row_id, density, alcohol, pH)

wine_quality_red_sel <- data.frame(wine_quality_red_sel)
rownames(wine_quality_red_sel) <- wine_quality_red_sel$row_id
wine_quality_red_sel <- wine_quality_red_sel %>% select(-row_id) %>% na.omit()

# DBSCAN example 1: density vs alcohol
# Select and scale variables
wine_quality_red_sel_2d <- wine_quality_red_sel[,c('density','alcohol')]
wine_quality_red_sel_scal_2d <- scale(wine_quality_red_sel_2d)

# Create 'elbow' plot to detect suitable value of EPS
eps_plot <- dbscan::kNNdistplot(wine_quality_red_sel_scal_2d, k=10)

# Perform DBSCAN
clusters_2d <- fpc::dbscan(wine_quality_red_sel_scal_2d, eps = 0.3, MinPts = 10)

# Identify the data points that are assigned to the noise cluster (i.e. cluster 0)
wine_quality_red_sel_2d$cluster <- as.factor(clusters_2d$cluster)

outliers <- wine_quality_red_sel_2d[which(wine_quality_red_sel_2d$cluster == 0),]

# Mark univariate outliers to show difference univariate and multivariate outliers
wine_quality_red_sel_2d$outlier_density <- as.factor(ifelse(wine_quality_red_sel_2d$density < (quantile(wine_quality_red_sel_2d$density, .25)-1.5*IQR(wine_quality_red_sel_2d$density)) | wine_quality_red_sel_2d$density > (quantile(wine_quality_red_sel_2d$density, .75)+1.5*IQR(wine_quality_red_sel_2d$density)), '1', '0'))

wine_quality_red_sel_2d$outlier_alcohol <- as.factor(ifelse(wine_quality_red_sel_2d$alcohol < (quantile(wine_quality_red_sel_2d$alcohol, .25)-1.5*IQR(wine_quality_red_sel_2d$alcohol)) | wine_quality_red_sel_2d$alcohol > (quantile(wine_quality_red_sel_2d$alcohol, .75)+1.5*IQR(wine_quality_red_sel_2d$alcohol)), '1', '0'))

wine_quality_red_sel_2d$univar_outlier <- as.factor(ifelse(wine_quality_red_sel_2d$outlier_density == '1' | wine_quality_red_sel_2d$outlier_alcohol == '1', '0', '1'))

# Visualize 2D DBSCAN output
dbscan_output_2d <- ggplot(wine_quality_red_sel_2d , aes(density, alcohol, colour = cluster, shape=univar_outlier))+
  geom_point(alpha = 0.75) + theme_minimal() +
  scale_shape_manual(values=c(16, 3)) +
  labs(title = "<b>Unusual combinations of alcohol and density in red wine samples", 
       subtitle = "DBSCAN identifies 1 <span style='color:#F8766D;'>noise</span> and 1 <span style='color:#00BFC4;'>regular</span> cluster") +
  xlab('Density (g/mL)') + 
  ylab('Alcohol (%)') +
  theme(legend.position = "none",
        plot.title = element_markdown(margin = margin(10,0,0,0)),
        plot.subtitle = element_markdown(margin = margin(10,0,10,0)),
        axis.text.x = element_text(margin = margin(5,0,5,0)),
        axis.text.y = element_text(margin = margin(0,5,0,5)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  geom_vline(xintercept = (quantile(wine_quality_red_sel$density, .25)-1.5*IQR(wine_quality_red_sel$density)), alpha = 0.3, linetype = 'dashed') +
  geom_vline(xintercept = (quantile(wine_quality_red_sel$density, .75)+1.5*IQR(wine_quality_red_sel$density)), alpha = 0.3, linetype = 'dashed') +
  geom_hline(yintercept = (quantile(wine_quality_red_sel$alcohol, .75)+1.5*IQR(wine_quality_red_sel$alcohol)), alpha = 0.3, linetype = 'dashed')

# Add boxplot on top and side of scatterplot
dbscan_output_2d <- ggMarginal(dbscan_output_2d, type = 'boxplot', alpha = 0.5, size = 10)
dbscan_output_2d

# DBSCAN example 2: density, alcohol and pH
# Select and scale variables
wine_quality_red_sel_3d <- wine_quality_red_sel[,c('density','alcohol','pH')]
wine_quality_red_sel_scal_3d <- scale(wine_quality_red_sel_3d)

# Create 'elbow' plot to detect suitable value of EPS
eps_plot <- dbscan::kNNdistplot(wine_quality_red_sel_scal_3d, k=10)

# Perform DBSCAN
clusters_3d <- fpc::dbscan(wine_quality_red_sel_scal_3d, eps = 1, MinPts = 10)

# Identify the data points that are assigned to the noise cluster (i.e. cluster 0)
wine_quality_red_sel_3d$cluster <- as.factor(clusters_3d$cluster)

outliers <- wine_quality_red_sel_3d[which(wine_quality_red_sel_3d$cluster == 0),]

# Mark univariate outliers to show difference univariate and multivariate outliers
wine_quality_red_sel_3d$outlier_density <- as.factor(ifelse(wine_quality_red_sel_3d$density < (quantile(wine_quality_red_sel_3d$density, .25)-1.5*IQR(wine_quality_red_sel_3d$density)) | wine_quality_red_sel_3d$density > (quantile(wine_quality_red_sel_3d$density, .75)+1.5*IQR(wine_quality_red_sel_3d$density)), '1', '0'))

wine_quality_red_sel_3d$outlier_alcohol <- as.factor(ifelse(wine_quality_red_sel_3d$alcohol < (quantile(wine_quality_red_sel_3d$alcohol, .25)-1.5*IQR(wine_quality_red_sel_3d$alcohol)) | wine_quality_red_sel_3d$alcohol > (quantile(wine_quality_red_sel_3d$alcohol, .75)+1.5*IQR(wine_quality_red_sel_3d$alcohol)), '1', '0'))

wine_quality_red_sel_3d$outlier_pH <- as.factor(ifelse(wine_quality_red_sel_3d$pH < (quantile(wine_quality_red_sel_3d$pH, .25)-1.5*IQR(wine_quality_red_sel_3d$pH)) | wine_quality_red_sel_3d$pH > (quantile(wine_quality_red_sel_3d$pH, .75)+1.5*IQR(wine_quality_red_sel_3d$pH)), '1', '0'))

wine_quality_red_sel_3d$univar_outlier <- as.factor(ifelse(wine_quality_red_sel_3d$outlier_density == '1' | wine_quality_red_sel_3d$outlier_alcohol == '1' | wine_quality_red_sel_3d$outlier_pH == '1', '0', '1'))

# Visualize 3D DBSCAN output
# Create outlier groups for plotly visualization
wine_quality_red_sel_3d$outlier_group <- paste0(wine_quality_red_sel_3d$cluster, wine_quality_red_sel_3d$univar_outlier)

wine_quality_red_sel_3d <- wine_quality_red_sel_3d %>%
  mutate(
    color = case_when(
      outlier_group == "00" | outlier_group == "01" ~ "#F8766D",
      outlier_group == "11" | outlier_group == "10" ~ "#00BFC4"
    ),
    symbol = case_when(
      outlier_group == "00" | outlier_group == "10" ~ "circle",
      outlier_group == "01" | outlier_group == "11" ~ "cross"
    )
  )

# Adjust variable names for plotly visualization
wine_quality_red_sel_3d <- wine_quality_red_sel_3d %>% dplyr::rename('Density (g/mL)' = density, 'Alcohol (%)' = alcohol)

# Create plotly 3D scatterplot
plot_ly(wine_quality_red_sel_3d, x = ~`Density (g/mL)`, y = ~`Alcohol (%)`, z = ~pH, 
        marker = list(color = ~color, symbol = ~symbol)) %>%
  layout(title = '<b>Unusual combinations of alcohol, density and pH in red wine samples</b>')
