#Want to read in C2D and C3D for AA and AA_NA pre and post, then plot - try firstly 4 plots comparing
#each different sample, with C2D and C3D giving the same colour 

library(ggplot2)
library(dplyr)
library(scales)
library(plotly)
library(cowplot)
library(ggforce)
library(grid)
library(tidyverse)
library(patchwork)
library(svglite)
install.packages("svglite")
install.packages("ggforce")

# Read the CSV files for GPE and Nyquist
data_AA_NA_Postcycle <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA w NA full cell/Cap/cap_data_AA_NA_Postcycle.csv")
data_AA_NA_Precycle <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA w NA full cell/Cap/cap_data_AA_NA_Precycle.csv")
data_AA_Postcycle <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA full cell/Cap/cap_data_AA_Postcycle.csv")
data_AA_Precycle <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA full cell/Cap/cap_data_AA_Precycle.csv")

# Add sample and condition columns
data_AA_NA_Postcycle <- data_AA_NA_Postcycle %>% mutate(Sample = "AA_NA", Condition = "Postcycle")
data_AA_NA_Precycle <- data_AA_NA_Precycle %>% mutate(Sample = "AA_NA", Condition = "Precycle")
data_AA_Postcycle <- data_AA_Postcycle %>% mutate(Sample = "AA", Condition = "Postcycle")
data_AA_Precycle <- data_AA_Precycle %>% mutate(Sample = "AA", Condition = "Precycle")

#Now, repeat this for the DRT

DRT_AA_NA_Post <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA w NA full cell/CSV/av DRT/AA_NA_post.csv", skip = 3, header = TRUE)
DRT_AA_NA_Pre <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA w NA full cell/CSV/av DRT/AA_NA_pre.csv", skip = 3, header = TRUE)
DRT_AA_Post <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA full cell/CSV/av DRT/AA_post.csv", skip = 3, header = TRUE)
DRT_AA_Pre <- read.csv("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow/AA full cell/CSV/av DRT/AA_pre.csv", skip = 3, header = TRUE)

#Bind the condition and sample rows

DRT_AA_NA_Post <- DRT_AA_NA_Post %>% mutate(Sample = "AA_NA", Condition = "After cycling")
DRT_AA_NA_Pre <- DRT_AA_NA_Pre %>% mutate(Sample = "AA_NA", Condition = "Before cycling")
DRT_AA_Post <- DRT_AA_Post %>% mutate(Sample = "AA", Condition = "After cycling")
DRT_AA_Pre <- DRT_AA_Pre %>% mutate(Sample = "AA", Condition = "Before cycling")

#Read in text file showing cycling

setwd("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Redox flow")

# Filter data based on x-limits
x_limits <- c(1 * 3600, 2 * 3600)  # Convert hours to seconds
filtered_data <- subset(Evstex, time_adjusted >= x_limits[1] & time_adjusted <= x_limits[2])

# Identify the extreme points within the filtered data
extreme_points <- filtered_data[which(filtered_data$Ewe.V == max(filtered_data$Ewe.V) | filtered_data$Ewe.V == min(filtered_data$Ewe.V)), ]

# Create the plotdata:
a <- ggplot(Evstex, aes(x = time_adjusted / 3600, y = Ewe.V)) +
  geom_line(colour = "darkblue", linewidth = 1.5) +
  geom_point(data = extreme_points, aes(x = time_adjusted / 3600, y = Ewe.V), color = "red", size = 5, shape = 1, stroke = 3) +
  geom_text(data = extreme_points, aes(x = time_adjusted / 3600, y = Ewe.V, label = "EIS recorded"), hjust = -0.2, vjust = 0.45, size = 6) +
  labs(x = "t / hours", y = "Voltage / V") +  # Change axis labels
  theme_classic(base_size = 18) +  # Apply classic theme
  scale_x_continuous(expand = c(0, 0), limits = c(1, 2)) +  # Adjust xlim to match the new x values
  theme(axis.text = element_text(colour = "black", size = 18),
        axis.text.x = element_blank())+
  coord_fixed(ratio = 0.6)# Customize axis text

print(a)

ggsave("Examplesymcellechem.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       height = 21, 
       width = 29,
       units = "cm")

#To allow east insert

# Combine all data into one data frame for GPE and Nyquist
combined_data <- bind_rows(data_AA_NA_Precycle, data_AA_Precycle, data_AA_NA_Postcycle, data_AA_Postcycle)

comb_DRT <- bind_rows(DRT_AA_NA_Post, DRT_AA_NA_Pre, DRT_AA_Post, DRT_AA_Pre)

# Update facet labels with subscripts
comb_DRT$Sample <- factor(comb_DRT$Sample, levels = c("AA", "AA_NA"), 
                          labels = c(expression("AA in 1 M H"[2]*"SO"[4]), 
                                     expression("AA with 1 M NA in 1 M H"[2]*"SO"[4])))

# Reorder the levels of the Condition factor
comb_DRT$Condition <- factor(comb_DRT$Condition, levels = c("Before cycling", "After cycling"))

#useful <- ggplot(comb_DRT, aes(x = tau, y =gamma, colour = Sample, linetype = Condition))+
 # geom_path()+
  #scale_x_continuous(
   # trans = 'log10', 
    #limits = c(1e-5, 1e2),
    #breaks = trans_breaks('log10', function(x) 10^x),
    #labels = trans_format('log10', math_format(10^.x)))

#ggplotly(useful)
# Function to create the main plot for a given sample
create_main_plot <- function(sample){
  ggplot(subset(comb_DRT, Sample == sample), aes(x = tau, y = gamma, colour = Condition)) +
    geom_path(size = 1.25) +
    theme_classic(base_size = 18) +
    labs(x = NULL, y = NULL, colour = "EIS recorded:") + # Remove axis labels and legend
    scale_x_continuous(
      trans = 'log10', 
      limits = c(1e-5, 1e2),
      breaks = trans_breaks('log10', function(x) 10^x),
      labels = trans_format('log10', math_format(10^.x))) +
    scale_color_manual(values = c("Before cycling" = "red", "After cycling" = "darkblue")) +
    theme(axis.text = element_text(size =18, color = "black"),
          strip.background = element_blank(),
          strip.text = element_text(size = 18),
          legend.position = "none")+
    guides(colour = guide_legend(override.aes = list(size = 5)))+# Remove legend
    facet_wrap(~ Sample, labeller = label_parsed)+
    ylim(0, 150)# Ensure y-scales are the same
}

# Function to create the inset plot for a given sample
create_inset_plot <- function(sample) {
  ggplot(subset(comb_DRT, Sample == sample), aes(x = tau, y = gamma, colour = Condition)) +
    geom_path(size = 1.25) +
    theme_classic(base_size = 18) +
    scale_x_continuous(
      trans = 'log10', 
      limits = c(10^(-5.5), 1e-2), # Adjust limits for small tau values
      breaks = c(1e-5, 1e-4, 1e-3, 1e-2),
      labels = trans_format('log10', math_format(10^.x))) +
    theme(legend.position = "none") +
    labs(x = NULL, y = NULL) + # Remove axis labels
    scale_color_manual(values = c("Before cycling" = "red", "After cycling" = "darkblue")) +
    ylim(0, 4)+
    theme(axis.text = element_text(size =18, color = "black"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          strip.background = element_blank(),
          legend.position = "none") # Match tick label size to main plot
}

legend_plot <- ggplot(comb_DRT, aes(x = tau, y = gamma, colour = Condition)) +
  geom_path(size = 1.25) +
  theme_classic(base_size = 18) +
  scale_x_continuous(
    trans = 'log10', 
    limits = c(10^(-5.5), 1e-2), # Adjust limits for small tau values
    breaks = c(1e-5, 1e-4, 1e-3, 1e-2),
    labels = trans_format('log10', math_format(10^.x))) +
  theme(legend.position = "top") +
  labs(x = NULL, y = NULL, colour = "EIS recorded:") + # Remove axis labels
  scale_color_manual(values = c("Before cycling" = "red", "After cycling" = "darkblue")) +
  ylim(0, 4)+
  theme(axis.text = element_text(size =18, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        legend.position = "top",
        legend.text = element_text(size =18))

legend_plot


# Create plots for each sample
samples <- levels(comb_DRT$Sample)
plots <- lapply(samples, function(sample) {
  main_plot <- create_main_plot(sample)
  inset_plot <- create_inset_plot(sample)
  
  ggdraw() +
    draw_plot(main_plot) +
    draw_plot(inset_plot, x = 0.2, y = 0.55, width = 0.5, height = 0.3) # Adjust position and size of inset
})

# Combine all plots together
combined_plots <- plot_grid(plotlist = plots, ncol = 2, nrow = 1)
combined_plots

# Extract the legend from one of the plots
legend <- get_legend(legend_plot)

# Add shared axis labels
y_label <- textGrob(bquote(gamma*"ln"(tau)(~Omega~ "cm"^2)), rot = 90, gp = gpar(fontsize = 18))
x_label <- textGrob(bquote(tau*" (s)"), gp = gpar(fontsize = 18))


final_plot <- plot_grid(
    y_label, combined_plots, ncol = 2, rel_widths = c(0.05, 1)
  )
legend
final_plot_DRT_full <- plot_grid(legend,
  final_plot,
  x_label,
  nrow = 3,
  rel_heights = c(0.05, 1, 0.05)
)


print(final_plot_DRT_full)
ggsave("FullcellDRT.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       height = 21,
       width = 29, 
       units = "cm")

DRTs <- final_plot_DRT_full|final_plot_DRT 
DRTs

fulfig <- plot_Ny/final_plot_DRT_full + plot_layout(ncol = 1,nrow =2, heights = c(0.25, 0.75))
fulfig
filt_dat <- tibble(combined_data$Freq, combined_data$Re.Z., combined_data$X.Im.Z., combined_data$alphagpe, combined_data$C2D, combined_data$C3D, combined_data$Sample, combined_data$Condition)
names(filt_dat) <- c("f", "Re", "Im", "alpha", "CC2D", "CC3D", "Sample", "Condition")

filt_long <- pivot_longer(filt_dat, 
                          cols = CC2D:CC3D, 
                          names_to = "Model",
                          names_prefix = "C")

#Plot Nyquist data
filt_long
filt_long$Sample <- factor(filt_long$Sample, levels = c("AA", "AA_NA"), 
                           labels = c(expression("AA in 1 M H"[2]*"SO"[4]), 
                                      expression("AA with 1 M NA in 1 M H"[2]*"SO"[4])))
plot_Ny <- ggplot(filt_long, aes(x = Re, y = Im, color = Condition))+
  geom_point(size = 1.5)+
  theme_classic(base_size = 18)+
  facet_wrap(~Sample, labeller = label_parsed)+
  scale_color_manual(values = c("Postcycle" = "darkblue", "Precycle" = "red"))+
  scale_shape_manual(values = c("AA_NA" = 16, "AA" = 17))+
  coord_fixed()+
  theme(axis.text = element_text(size = 18, color = "black"),
        strip.background = element_blank(),
        legend.position = "top",
        strip.text = element_text(size = 18))+
  labs(y = bquote('-Im{Z} ('~Omega~ cm^2*')'),
       x = bquote('Re{Z} ('~Omega~ cm^2*')'),
       colour = "EIS recorded:")
  
plot_Ny

ggsave("FullcellNy.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       height = 15,
       width = 25, 
       units = "cm")

ggsave("FullcellDRT.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       plot = final_plot_DRT_full,
       height = 30, 
       width = 25,
       units = "cm")

setwd()
solution <- wrap_elements(plot_Ny)

comb <- wrap_plots(plot_Ny, final_plot_DRT_full)
comb
# Adjust the margins of each plot

plot_comb <- plot_grid(plot_Ny, final_plot_DRT_full, nrow = 2, ncol = 1, rel_widths = c(0.1, 1), rel_heights = c(1, 2))
plot_comb

combindeNyex <- plot_Ny|a + plot_layout(ncol = 2, width = unit(c(-1, 1), c("null", "cm")))
       combindeNyex                                 
plot_Ny <- plot_Ny + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
a <- a + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Align the plots horizontally with the same height
toprow <- plot_grid(plot_Ny, a, align = "h",axis = "tb", nrow = 1, ncol = 2, rel_heights = c(1, 0.5))

# Display the combined plot
print(toprow)
ggsave("FullcellNyandEvstex.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       height = 41,
       width = 58, 
       units = "cm")
filt_long$Condition <- factor(filt_long$Condition, levels = c("Precycle", "Postcycle"))
# Plot with facetting by model type
plot_C2D_C3D <- ggplot(filt_long, aes(x = f, y = value/1e-6, color = Condition, linetype = Model)) +
  geom_path(size = 1.25) +
  facet_wrap(~ Sample,  labeller = label_parsed) +
  labs( x = NULL, y = bquote(C~(mu*F/cm^2)), colour = "EIS recorded:",
        linetype = "Model:") +
  theme_classic(base_size =18) +
  scale_x_continuous(limits = c(1, 1e5),
                     trans = 'log10', 
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  scale_color_manual(values = c("Precycle" = "red", "Postcycle" = "darkblue"),
                     labels = c("Before cycling", "After cycling")) +
  scale_linetype_manual(values = c("C3D" = "solid", "C2D" = "dotted"),
                        labels = c(expression(C[3*D]), expression(C[2*D]))) +
  theme(axis.text = element_text(size = 18, color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = 18),
        legend.position = "none")+
  ylim(0, 2000)

# Print the plot
print(plot_C2D_C3D)

ggsave("Fullcellcap.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       height = 20,
       width = 20, 
       units = "cm")

plot_C2D_C3D_linetype <- ggplot(filt_long, aes(x = f, y = value/1e-6, linetype = Model)) +
  geom_path(size = 1.25) +
  facet_wrap(~ Sample,  labeller = label_parsed) +
  labs( x = NULL, y = bquote(C~(mu*F/cm^2)), colour = "EIS recorded:",
        linetype = "Model:") +
  theme_classic(base_size = 16) +
  scale_x_continuous(limits = c(1, 1e5),
                     trans = 'log10', 
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  
  scale_linetype_manual(values = c("C3D" = "solid", "C2D" = "dotted"),
                        labels = c(expression(C[3*D]), expression(C[2*D]))) +
  theme(axis.text = element_text(size = 16, color = "black"),
        strip.background = element_blank(),
        legend.text = element_text(size  =16),
        legend.title = element_text(size = 16)) +
  ylim(0, 2000)

# Print the plot
print(plot_C2D_C3D_linetype)

plot_C2D_C3D_color <- ggplot(filt_long, aes(x = f, y = value/1e-6, color = Condition)) +
  geom_path(size = 1.25) +
  facet_wrap(~ Sample,  labeller = label_parsed) +
  labs( x = NULL, y = bquote(C~(mu*F/cm^2)), colour = "EIS recorded:",
        linetype = "Model:") +
  theme_classic(base_size = 16) +
  scale_x_continuous(limits = c(1, 1e5),
                     trans = 'log10', 
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x))) +
  scale_color_manual(values = c("Precycle" = "red", "Postcycle" = "darkblue"),
                     labels = c("Before cycling", "After cycling")) +
  theme(axis.text = element_text(size = 16, color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size =16),
        legend.text = element_text(size  =16),
        legend.title = element_text(size = 16)) +
  ylim(0, 2000)

# Print the plot
print(plot_C2D_C3D_color)
#Now, plot alpha

plot_alpha <- ggplot(filt_long, aes(x = f, y = alpha, color = Condition))+
                       geom_path(size = 1.25)+
  ylim(0, 1)+
  scale_color_manual(values = c("Postcycle" = "darkblue", "Precycle" = "red"))+
  facet_wrap(~Sample, labeller = label_parsed)+
  scale_x_continuous(limits = c(1, 1e5),
                     trans = 'log10', 
                     breaks = trans_breaks('log10', function(x) 10^x),
                     labels = trans_format('log10', math_format(10^.x)))+
  theme_classic(base_size = 18)+
  theme(axis.text = element_text(size = 18, color = "black"),
        strip.background = element_blank(),
        legend.position = "none",
        strip.text = element_blank())+
  scale_linetype_manual(values = c("Precycle" = "solid", "Postcycle" = "dashed"))+
  labs(y = bquote(alpha[GPE]),
       x = "Frequency (Hz)" )
  
plot_alpha
label_C <- plot_C2D_C3D + plot_annotation(tag = "a)")


capplot <- plot_C2D_C3D / plot_alpha 
  
  
capplot
  
  
finalcap <- plot_grid(legend_model, capplot, ncol = 1, rel_widths = c(0.05, 1), rel_heights = c(0.035, 1)
)
 finalcap
capplot
ggsave("Fullcellcapfig.png",
       path = "C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures",
       height = 20,
       width = 25, 
       units = "cm")


# Function to extract the legend from a plot
get_legend <- function(my_plot) {
  tmp <- ggplotGrob(my_plot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



legend_plot_color <- plot_C2D_C3D_color + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 1))
legend_color <- get_legend(legend_plot_color)
legend_color

legend_plot_linetype <- plot_C2D_C3D_linetype + theme(legend.position = "bottom") + guides(linetype = guide_legend(nrow = 1))
legend_linetype <- get_legend(legend_plot_linetype)

plot_Ny <- plot_Ny + theme(legend.position = "none")
plot_C2D_C3D <- plot_C2D_C3D + theme(legend.position = "none")
plot_alpha <- plot_alpha + theme(legend.position = "none")
final_plot_DRT <- final_plot_DRT + theme(legend.position = "none")

# Assuming plot_C2D_C3D is the plot with the legend you want to extract
capplot <- plot_C2D_C3D/plot_alpha
capplot


# Combine the left side plots
left_side <- plot_grid(legend_color, plot_Ny, legend_linetype, capplot, ncol = 1,nrow = 4, rel_heights = c(0.15, 1, 0.1, 2), rel_widths = c(0.2, 1, 1, 1))

# Combine the left side with the right side and insert the legend
final_plot <- plot_grid(
  left_side,
  final_plot_DRT,
  ncol = 2,
  rel_widths = c(1, 1),
  rel_heights = c(1,1)
)


# Print the final plot
print(final_plot)

getwd()

setwd("C:/Users/kiera/OneDrive/Documenten/PhD/Paper drafts/Nadia AA NA paper/Figures")

ggsave("FullcellEIS.png",
       height = 21.5, 
       width = 38.25, 
       units = "cm")
#####

