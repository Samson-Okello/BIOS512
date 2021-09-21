library('tidyverse')
options(repr.plot.width=4, repr.plot.height=3, repr.plot.res = 300)

mpg %>% head


# Bar char (Bars in Y axis)
p = ggplot(mpg, aes(x = class))
p = p + geom_bar()
p

# Bar char (Bars in X axis)
p = ggplot(mpg, aes(y = class))
p = p + geom_bar()
p

# Histogram
p = ggplot(diamonds, aes(x=price))
p = p + geom_histogram()
p

# Histogram adjust # bins
p = ggplot(diamonds, aes(x=price))
p = p + geom_histogram(bins=10)
p

# Histogram adjust width of bins
p = ggplot(diamonds, aes(x=price))
p = p + geom_histogram(binwidth=1000)
p

p = ggplot(diamonds, aes(x=price))
p = p + geom_histogram(binwidth=1000)
p = p + facet_wrap( ~clarity, ncol=1, scales = "free_y")
p

p = ggplot(diamonds, aes(x=price))
p = p + geom_density()
p = p + facet_wrap( ~clarity, ncol=1, scales = "free_y")
p


p = ggplot(diamonds, aes(x=price, fill=clarity))
p = p + geom_density(alpha= 0.7, color =NA)
# p = p + facet_wrap( ~clarity, ncol=1, scales = "free_y")
p


p = ggplot(diamonds, aes(x=price))
p = p + geom_histogram()
p = p + facet_wrap( ~clarity, ncol=1, scales = "free_y")
p = geom_rug(alpha=0.25, aes(color=fill))
p


# Kernel density plots
p = ggplot(diamonds, aes(x=price))
p = p + geom_density()
p

# Kernel density plots
p = ggplot(diamonds, aes(x=price))
p = p + geom_density(bw = 100, fill= 'blue')
p


# Kernel density plots
p = ggplot(diamonds, aes(x=price, fill=color))
p = p + geom_histogram()
p


mpg$class %>% table
mpg %>%
    group_by(class) %>%
    summarize(sum_displ = sum(displ))

p = ggplot(mpg, aes(y = class, weight = displ))
p = p + geom_bar()
p

df = mpg %>%
    group_by(class) %>%
    summarize(sum_displ = sum(displ))  # creating sum of displ
df

p = ggplot(df, aes(y = class, x = sum_displ))
p = p + geom_col()
p

# Sort Class then plot vertical bar showing ascending
df.sorted = df %>% mutate(class = fct_reorder(class, sum_displ))
p = ggplot(df.sorted, aes(y = class, x = sum_displ))
p = p + geom_col()
p

# sort class then horizontal bar plot 
df.sorted = df %>% mutate(class = fct_reorder(class, sum_displ))
p = ggplot(df.sorted, aes(x = class, y = sum_displ))
p = p + geom_col()
p = p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p

# Same as above but flip axes
df.sorted = df %>% mutate(class = fct_reorder(class, sum_displ))
p = ggplot(df.sorted, aes(x = class, y = sum_displ))
p = p + geom_col()
p = p + coord_flip() # flips axes
p

p = ggplot(mpg, aes(y = class))
p = p + geom_bar()
p

mpg %>% head

p = ggplot(mpg, aes(y = class, fill = drv))
p = p + geom_bar()
p  = p + scale_fill_brewer(palette = "Dark2")
p

test.data = data.frame(
    cat1 = 100 + c(0, cumsum(runif(49, -20, 20))),
    cat2 = 150 + c(0, cumsum(runif(49, -10, 10))),
    date = seq(as.Date("2002-01-01"), by="1 month", length.out=100)
) %>% gather(category, value, -date)

test.data %>% head(2)

# line plot
p = ggplot(test.data, aes(x = date, y = value))
p = p + geom_line()
p

test.data %>% head(5)
# line plot with categorization 
p = ggplot(test.data, aes(x = date, y = value, group = category))
p = p + geom_line()
p

# Line plot with color catergorization
p = ggplot(test.data, aes(x = date, y = value, color = category))
p = p + geom_line()
p

mpg %>% head

# Scatter plot with lowess smoothening
p = ggplot(mpg, aes(x = displ, y = hwy))
p = p + geom_point()
p = p + geom_smooth()
p

p = ggplot(mpg, aes(x = displ, y = hwy, color = factor(cyl)))
p = p + geom_point()
p = p + geom_smooth()
p

# Heat map
nba.data = read.csv("http://datasets.flowingdata.com/ppg2008.csv") %>%
    gather(stat, value, -Name) %>%
    group_by(stat) %>%
    mutate(value.scaled = scales::rescale(value))

nba.data %>% head
p = ggplot(nba.data, aes(x = stat, y = Name, fill = value))
p = p + geom_tile()
p

options(repr.plot.width=6, repr.plot.height=8)
p = ggplot(nba.data, aes(x = stat, y = Name, fill = value.scaled))
p = p + geom_tile(color = 'white')
p

p = ggplot(nba.data, aes(x = stat, y = Name, fill = value.scaled))
p = p + geom_tile(color = 'white')
p = p + scale_fill_gradient(low = "white", high = "steelblue")
p

# Clearer heat map
p = ggplot(nba.data, aes(x = stat, y = Name, fill = value.scaled))
p = p + geom_tile(color = 'white')
p = p + scale_fill_gradient(low = "white", high = "steelblue")
p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# Bar chart with standard error bars 
mpg %>% head
options(repr.plot.width=4, repr.plot.height=3)
df = mpg %>%
    group_by(class) %>%
    summarize(mean_hwy = mean(hwy), sd_hwy = sd(hwy))

p = ggplot(df, aes(x = class))
p = p + geom_bar(aes(y = mean_hwy), stat = 'identity')
p = p + geom_errorbar(aes(ymin = mean_hwy + sd_hwy, ymax = mean_hwy - sd_hwy),
                      color = 'red', width = 0.2)
p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# Dot plots
options(repr.plot.width=4, repr.plot.height=3)
p = ggplot(mpg, aes(x = class, y = hwy))
p = p + geom_point()
p

# using alpha to blurr the points
options(repr.plot.width=4, repr.plot.height=3)
p = ggplot(mpg, aes(x = class, y = hwy))
p = p + geom_point(alpha = 0.15)
p

# adding jitter to spread the points
p = ggplot(mpg, aes(x = class, y = hwy))
p = p + geom_point(position = position_jitter(h = 0, w = 0.20), size = 1)
p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# Boxplot with mean of Y ploted 
p = ggplot(mpg, aes(x = class, y = hwy))
p = p + geom_boxplot()
p = p + stat_summary(fun.y=mean) 
p = p + theme(axis.text.x = element_text(angle=45, hjust =1))  # adjusts the angle of X axis label
p


# Box plot with individual points plotted
p = ggplot(mpg, aes(x = class, y = hwy))
p = p + geom_boxplot(fill = NA, outlier.shape = NA, color = '#003366')
p = p + geom_point(position = position_jitter(h = 0, w = 0.26), size = 0.5)
p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p


# Another version of box plot with class sorted in ascending hwy
mpg.sorted = mpg %>%
    mutate(class.sorted = fct_reorder(class, hwy))

p = ggplot(mpg.sorted, aes(x = class.sorted, y = hwy))
p = p + geom_boxplot()
p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

# Box plot with flipped class on y-axis
p = ggplot(mpg.sorted, aes(x = class.sorted, y = hwy))
p = p + geom_boxplot(fill = NA, outlier.shape = NA, color = '#003696', fatten = 1, lwd = 0.25)
p = p + geom_point(position = position_jitter(h = 0, w = 0.15), size = 0.75, alpha = 0.5)
p = p + theme_bw()
p = p + theme(panel.border = element_blank())
p = p + labs(x = 'class', y = 'highway miles per gallon')
p = p + coord_flip()  ## Flips the axes
p



vax_data = read_csv(
    "covid19_vaccinations_in_the_united_states.csv", 
    na = 'N/A',
    col_types = cols()
)
