library(ggplot2)
library(dplyr)

title <- "HDAT9700 Statistical Modelling II"

# Light mode
low <- "#FFDB01"
mid <- "#FEA923"
high <- "#FD5E55"
# mid <- "#FFDB01"
# high <- "#158cba"
# low <- "#FD5E55"
fontColor <- "grey10"


df <- expand.grid(x = 1:33, y = seq(0, 3.5, by=3.5/32)) %>%
  mutate(
    base = 0.2,
    lab = NA,
    h = NA
  )

for (i in 1:nchar(title)) {
  if (df$y[i] == 0) {
    df$lab[i] <- substr(title, i, i)
    df$h[i] <- min(2*round(runif(1, 0.5, 1), digits=1), 2.0)
  }
}

df$x1 <- c(3.7, rep(NA,nrow(df)-1))
df$y1 <- c(3, rep(NA,nrow(df)-1))




ggplot(df, aes(x, y, fill = y)) +
  geom_raster() +
  geom_point(data=df, aes(x=x1, y=y1), color = "red", size=10) +
  geom_col(data = df %>% filter(y==0), aes(x=x, y=h), fill = '#158cba') +
  geom_text(data=df, aes(x=x, y=base, label=lab), color="grey90") +
  scale_fill_gradient2(low = low, mid = mid, high = high, midpoint=3.5/2) +
  scale_y_continuous(limits = c(0, 3.5)) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill=NA, color=NA))

ggsave(here::here("images/light-mode.png"), height=2, width=10)


# dark mode
low1 <- "#386150"
mid1 <- "#26413C"
high1 <- "#1D1A31"
fontColor1 <- "grey90"

t <- df %>% filter(y==0) %>% select(h) %>% unlist() %>% as.vector()

df1 <- expand.grid(x = 1:33, y = seq(0.6, 2, .2)) %>%
  mutate(h1 = rep(t, 8)) %>%
  filter(y < h1) %>%
  mutate(on = rbinom(n(), 1, 0.3)) %>%
  filter(on==TRUE)

ggplot() +
  geom_raster(data=df, aes(x, y, fill = y)) +
  geom_col(data = df %>% filter(y==0), aes(x=x, y=h), fill = 'grey10') +
  geom_point(data=df, aes(x=x1, y=y1), color = "#f8f8ba", size=10) +
  geom_point(data = df1, aes(x=x, y=y), color=mid, shape=15, size=2) +
  geom_text(data=df, aes(x=x, y=base, label=lab), color="grey80") +
  scale_fill_gradient2(low = low1, mid = mid1, high = high1, midpoint=3.5/2) +
  scale_y_continuous(limits = c(0, 3.5)) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill=NA, color=NA))

ggsave(here::here("images/dark-mode.png"), height=2, width=10)

