#New general GPE - gives C3D. Method relatively similar, except we use a more general form for R 

#Want to write a general script which imports and filters EIS data, then spits out Ceff,C2D and C3D, and then we plot and compare the result

# Attempting to write a script to work through the gpe analysis presented in M. de Pauli et al. / Electrochimica Acta 320 (2019) 1343664
# First, set up environment
library(tidyverse)
library(ggplot2)
library(splines)
library(gatepoints)
library(ggrepel)
library(patchwork)
# First we need to import our impedance data, which will have three columns - into R, filter out any rows with a -ve value of -Im(Z),
# then plot log-log of -Im(Z)) vs Re(Z) and -Im(Z)) vs w
# So, first of all, set up data importer: 
#This requires file is in working directory
#So, first set wd:
setwd("C:/Users/kiera/OneDrive/Documenten/PhD/Data/Electrochemistry/Sam_Southern_Data/1st_cycle_1000/Unfilt CSV")

rawdata <- read_csv("eisstrip_filt.csv", col_names = FALSE)

names(rawdata) <- c("Freq", "Re", "Im")
names(rawdata)

rawdata
# This imports freq, Re(Z) and -Im(Z) if data is copied straight out of EC Lab

# Next step is to filter our data to remove rows with -ve -Im(Z), as these represent inductive responses and will not be used in analysis 
# To do this, we will use a limit that we only keep rows with column three value is >0
#Depending on sign of Imaginary column, may need *1 or -1 here
filter0 <- tibble(rawdata$Freq, rawdata$Re, rawdata$Im*-1)
names(filter0) <- c("Freq", "Re", "Im")

filter1 <- subset(filter0, filter0$Im > 0 )
names(filter1) <- c("Freq", "Re", "Im")

filter1
# This leaves us with a filtered data frame containing no inductive Im(Z)


# Also, can calculate Ceff = 1/2pif(-Zi)

Ceff <- 1/(2*pi*filter1$Freq*filter1$Im)

# Next, we want to plot -Im(Z) and Re(Z) on a log scale on each axis.

#To plot: 
ggplot(data = filter1, 
       mapping = aes(x = Re, y = Im)) +
  geom_point()

# To visualise Re and Rt, we need to convert this to a log log plot - with log scales for x and y 
# First, create dataframe with log values
plot_data_logNy <- tibble( x= log10(filter1$Re), y=log10(filter1$Im))


#Now, plotting function
ggplot(plot_data_logNy , aes(x = x, y = y))+
  geom_point()+
  labs(title='Log-Log Nyquist', x='Log(Re(Z))', y='Log(-Im(Z))')+
  geom_text_repel(aes(label = x, size = 1, alpha = 0.5 ))

#fit <- pracma::savgol(plot_data_logNy$y, 1.5, forder = 3)

Re <- 10^plot_data_logNy$x[[1]]
Re_Rt <- 10^plot_data_logNy$x[[length(plot_data_logNy$x)]]

Rt <- 2*(Re_Rt - Re)

Rf <- (Re*Rt)/(Re + Rt)
# First, calculate Clim = 1/8fRe for a disk electrode embedded in an
#insulating plane, in the absence of faradaic reactions
Clim <- 1/(8*filter1$Freq*Re)
# Also want to plot log(-Im(Z) vs log(w), so follow the same procedure as above

plot_data_logImw <- tibble(x=log10(filter1$Freq), y=log10(filter1$Im))

# Now plot:

ggplot(plot_data_logImw, aes(x = x, y = y))+
  geom_point()+
  labs(title='Log-Log -Im(Z) vs w', x='w', y='-Im(Z)')

# Okay, so we have the basic data - now the tricky part. We need to calculate values for alphaGPE, 
#equal to the negative derivative of log(-Im(Z)) vs log(w) at each point
#This is a simple method using gradient function, later on probably a good idea to improve this

alphagpe2 <- -1* pracma::gradient(plot_data_logImw$y, h1 = plot_data_logImw$x, h2 = plot_data_logImw$y)
alphagpe2w <- data_frame(plot_data_logImw$x, alphagpe2)

# Want to plot the fit vs the actual data, 
#This gives us alphagpe
ggplot(alphagpe2w,
       aes(x=plot_data_logImw$x, y=alphagpe2)) +
  geom_point()+
  ylim(0, 1)

combined <- data.frame(filter1, alphagpe2)

# Now we have all of the information required to calculate C2d as described in the paper above.
Qsin <- sin(combined$alphagpe2*3.14159265358979323846264/2)

Qdenom <- ((combined$Freq*2*pi)^combined$alphagpe2)*combined$Im

Q <- Qsin/Qdenom

# 10.542 is ReRt/Re + Rt, took this value from initial excel analysis. In future, need to add in route to obtaining this in script
C2d <- (Q^(1/combined$alphagpe2)*((Rf)^((1-combined$alphagpe2)/combined$alphagpe2)))

K <- (Q^(1/combined$alphagpe2))
# Now calculate C3D

C3d <-(Q^(1/combined$alphagpe2)*((filter1$Re)^((1-combined$alphagpe2)/combined$alphagpe2)))

Cg <- C3d*alphagpe2
# This now plots C2d against logw (here called x), giving a plot which allows identification of capacitance plateaus
plotting <- data.frame(plot_data_logImw$x, alphagpe2, Ceff, C2d, C3d, Cg, Clim, K)
names(plotting) <- c("lfreq", "alphagpe", "Ceff", "C2D", "C3D", "Cg", "Clim", "K")

col <- c("Ceff", "C2D", "C3D","Cg", "Clim", "K")


#Plots Ceff, C2d and C3d vs log(freq)

plotting_long <- plotting %>%
  select(lfreq, Ceff, C2D, C3D, Cg, Clim, K) %>%
  pivot_longer(cols = Ceff:K,
               names_to = "Method",
               names_prefix = "C",
               values_to = "Capacitance")
plotting_long

cap_plot <- ggplot(data = plotting_long, 
                  mapping = aes(x = lfreq, y = Capacitance/1E-6, colour = Method ))+
  geom_line()+
  ylim(c(0.00001, 10))+
  xlim(c(0, 6))+
  xlab("log(W)")+
  ylab("C (uF/cm2)")+
  theme_classic()
cap_plot

# 
#ggplot(plotting,
 #      mapping = aes(x = lfreq, y = Ceff)) +
  #geom_line() +
  #geom_line(mapping = aes(x = lfreq , y = C2D , color = "green")) +
  #geom_line(mapping = aes(x = lfreq, y = C3D, color = "blue")) +
  #geom_line(mapping = aes(x = lfreq, y = Clim, color = "red")) +
  #geom_line(mapping = aes(x = lfreq, y = K, color = "")) +
  #ylim(c(0.00001, 0.0005)) +
  #xlim(c(-1, 5)) +
  #xlab("log(W)") +
  #ylab("C (F/cm2)") +
  #scale_shape_discrete(
  #  name = "Capacitance",
  #  breaks = c("Ceff", "C2D", "C3D", "Clim", "K"),
  #  labels = c("Ceff", "C2D", "C3D", "Clim", "K")
  #)

plotting
#Plots alpha against logfreq
alpha_plot <- ggplot(data = plotting, 
       mapping = aes(x = lfreq,y = alphagpe))+
  geom_point()+
  ylim(c(0, 1))+
  xlim(c(1.5, 5))+
  #xlab("log(W)")+
  #ylab("alphagpe")+
  theme_classic()
alpha_plot

finalfig <- cap_plot|alpha_plot

finalfig

ggsave(filename = "finalfig_70.png",
       path = "C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Impedance/Cu-Cu OCV EIS/Cu_Cu_MEEP4D2000_30min_25_05_23/Cap analysis/Plots/",
       plot = finalfig,
       width = 15, 
       height = 7,
       unit = "cm")
# Trying to work out how to plot with a legend. i think we need to transform this data, from wide to long
#(currently, each frequency has several different C values - instead, we can create )


#Compare C3D and C2d to get idea of 3d or 2d distribution:

Shape <- C3d/C2d

shape_data <- data_frame(plot_data_logImw$x, Shape)
names(shape_data) <- c("lfreq", "shape")
ggplot(data = shape_data, 
       mapping = aes(x = lfreq, y = shape))+
  geom_point()+
  ylim(c(0,3))
#Easier to just make one data frame with everything in it, and export. So, we want
# log Z and Z'', logw, C2D, alphagpe, 
total_dataframe <- data.frame(filter1, plot_data_logNy, plot_data_logImw$x, alphagpe2, Ceff, C2d, C3d, Cg, K)


names(total_dataframe) <- c("Freq", "Re(Z)", "-Im(Z)","log(Re(z))", "log(-Im(Z))", "log(w)", "alphagpe", "Ceff", "C2D", "C3D", "Cg", "K")

#Now, want to save the data:


write.csv(total_dataframe,"C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Impedance/Cu-Cu OCV EIS/Cu_Cu_MEEP4D2000_30min_25_05_23/Cap analysis/Data/Cap_data_79.csv")

