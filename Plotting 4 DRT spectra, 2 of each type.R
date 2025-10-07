#Plot 4 lots of single file DRT

#Set wd to cella
setwd("C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Redox flow/AA w NA full cell/DRT/Comp/DRT")



cella <- readr::read_delim("Pre_AA_NA.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))

cella <- cella %>%
  mutate(cond = str_remove(path, "_AA_NA"))%>%
  mutate(cell = str_remove(path, "Pre_"))
cella
names(cella) <- c("t", "tau", "MAP", "mean", "up", "low", "Cond", "Cell")

cella$tau <- as.numeric(cella$tau)
cella$mean <- as.numeric(cella$mean)

#Load cell b (if need to change wd here)

setwd("C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Redox flow/AA w NA full cell/DRT/Comp/DRT")

cellb <- readr::read_delim("Post_AA_NA.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))

cellb <- cellb %>%
  mutate(cond = str_remove(path, "_AA_NA")) %>%
  mutate(cell = str_remove(path, "Post_"))

cellb
names(cellb) <- c("t", "tau", "MAP", "mean", "up", "low", "Cond", "Cell")

cellb$tau <- as.numeric(cellb$tau)
cellb$mean <- as.numeric(cellb$mean)

#Load cell c (change wd if needed)

setwd("C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Redox flow/AA full cell/DRT/Comp")

cellc <- readr::read_delim("Pre_AA_4.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, "_4.txt"))

cellc <- cellc %>%
  mutate(cond = str_remove(path, "_AA")) %>%
  mutate(cell = str_remove(path, "Pre_"))
cellc
names(cellc) <- c("t", "tau", "MAP", "mean", "up", "low", "Cond", "Cell")

cellc$tau <- as.numeric(cellc$tau)
cellc$mean <- as.numeric(cellc$mean)

#Load cell 4, change wd if needed

setwd("C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Redox flow/AA full cell/DRT/Comp")


celld <- readr::read_delim("Post_AA_4.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, "_4.txt")) %>%
  mutate(cond = str_remove(path, "_AA")) %>%
  mutate(cell = str_remove(path, "Post_"))

celld
names(celld) <- c("t", "tau", "MAP", "mean", "up", "low", "Cond", "Cell")

celld$tau <- as.numeric(celld$tau)
celld$mean <- as.numeric(celld$mean)

#Now, need to combine them all:

alldat <- dplyr::bind_rows("Pre_AA_NA"=cella, "Post_AA_NA"=cellb, "Pre_AA"=cellc, "Post_AA"=celld, .id = "group")

#Now plot:

stack <- ggplot(alldat,mapping = aes(x = tau, y = mean))+
  geom_line(size = 1, aes(linetype = Cond, colour = factor(Cell)))+
  scale_linetype_manual("EIS measured",
                        values = c("dotted", "solid"))+
  scale_colour_manual(values = c("#FF0000", "#0000FF"))+
  labs(x = bquote(tau*" (s)"),  
       y = bquote(gamma*"ln"(tau)(~Omega~ "cm"^2)),
       colour = "Catholyte",
       linetype = "Spectra taken") +
  theme_classic(base_size = 20)+
  scale_x_continuous(limits = c(10^-6, 10^2),
                     trans = 'log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  ylim(0, 1)+
  
  facet_grid(rows = factor(alldat$Cell, levels = c("AA_NA", "AA")))
stack 
stack

ggsave(glue::glue("Stackplot_AA_AANA_bef_af.png"), path = "C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Redox flow/Plots of DRT",
       plot = stack,
       width = 20, 
       height = 18,
       unit = "cm")
