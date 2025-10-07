#Plotting DRT of sym cells - want to initially plot all 6, with discharge/charge

library(patchwork)
#Set wd to full cell wd
setwd("C:/Users/kiera/Documents/PhD/Data/Electrochemistry data/Redox flow/AA symmetric cell/DRT files for plotting")

p0.01 <- readr::read_delim("p0.01.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))
p0.01$path <- as.numeric(p0.01$path)
p0.01
names(p0.01) <- c("t", "tau", "MAP", "mean", "up", "low")

p0.1 <- readr::read_delim("p0.1.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))
p0.1$path <- as.numeric(p0.1$path)
p0.1
names(p0.1) <- c("t", "tau", "MAP", "mean", "up", "low")

p0.3 <-  readr::read_delim("p0.3.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))
p0.3$path <- as.numeric(p0.3$path)
p0.3
names(p0.3) <- c("t", "tau", "MAP", "mean", "up", "low")

n0.01 <- readr::read_delim("n0.01.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))
n0.01$path <- as.numeric(n0.01$path)
n0.01
names(n0.01) <- c("t", "tau", "MAP", "mean", "up", "low")


n0.1 <- readr::read_delim("n0.1.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))
n0.1$path <- as.numeric(n0.1$path)
n0.1
names(n0.1) <- c("t", "tau", "MAP", "mean", "up", "low")

n0.3 <- readr::read_delim("n0.3.txt", id = "path", col_names = TRUE, skip = 2) %>%
  mutate(path = str_remove(path, ".txt"))
n0.3$path <- as.numeric(n0.3$path)
n0.3
names(n0.3) <- c("t", "tau", "MAP", "mean", "up", "low")

alldat <- dplyr::bind_rows("+0.3 V"=p0.3, "+0.1 V"=p0.1, "-0.01 V" = n0.01, "-0.1 V" = n0.1, "-0.3 V"=n0.3, .id = "group")

alldat$tau <- as.numeric(alldata$tau)
alldat$mean <- as.numeric(alldat$mean)

stacksym <- ggplot(alldat,mapping = aes(x = tau, y = mean, group = t, colour = t))+
  geom_path()+
  labs(x = bquote(tau*" (s)"),
       y = bquote(gamma*"ln"(tau)(~Omega~ "cm"^2)),
       colour = "Time after \nplating (h)",
       title = bquote("1 mAh/cm"^2 ~","~"0.25 mA/cm"^2)) +
  #scale_colour_gradient(low = 'blue', high = 'red')+
  theme_classic(base_size = 20)+
  scale_x_continuous(limits = c(10^-5.6, 10^2),
                     trans = 'log10',
                     breaks=trans_breaks('log10', function(x) 10^x),
                     labels=trans_format('log10', math_format(10^.x)))+
  ylim(0,0.1) +
  facet_grid(rows = factor(alldat$group, levels = c("+0.3 V", "+0.1 V", "-0.01 V", "-0.1 V", "-0.3 V")))
stacksym
stack




