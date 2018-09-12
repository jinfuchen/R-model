setwd('/Users/jinfu/Documents/workspace/R/R-model')

library(tidyr)
library(dplyr)
library(ggplot2)

########## summary mtcars dataset
names(mtcars)
summary(mtcars)
######################dplyr library######################
#1. select column
#select cloumn mpg
select(mtcars, mpg)
#don't select column mpg
select(mtcars, -mpg)
#select column from mpg to hp
select(mtcars, mpg:hp)
#select column starting from letter 'c'
select(mtcars, starts_with("c"))

#2. filter row
#filter row that cly == 8
filter(mtcars, cyl == 8)
#filter row that cyl < 6
filter(mtcars, cyl < 6)
#more conditions
filter(mtcars, cyl < 6 & vs == 1)

#3. mutate variables
#generate new variables, and keep the old variables
mutate(mtcars, displ_l = disp/61.0237, displ_2 = displ_l/2)
#generate new variables, but remove the old variables
transmute(mtcars, displ_l = disp/61.0237, displ_2 = displ_l/2)

#4. arrange data
#increasing order
arrange(mtcars, cyl, disp)
#deecreasing order
arrange(mtcars, desc(cyl), desc(disp))

#5. group_by and pipe handling
# %>%
mtcars %>%  group_by(mpg) %>% summarise(mpg_average = mean(mpg), disp_mean = mean(disp))

#6.sample data
#sample 5 rows
sample_n(mtcars, 5)
sample_n(mtcars, 5, replace = TRUE)

#7. ddply, split – apply – combine
#group by vs variable, and count the number of row in each group
ddply(mtcars, .(vs), count)
# group by cyl, and calculate the mean of mpg in each group 
ddply(mtcars,.(cyl), summarise, mean = mean(mpg))

######################tidyr library######################
df <- read.csv("./data/tidyr.csv")
#1.gather, merge two column two one column
gather(data = df, key = day, value = score, c(day1score,day2score))
#2. spread one column based on the value
spread(data = df, key = day, value = score)
#3. split info to two columns group and gender
separate(data = df, col = info, into = c("group","gender"))
#4. unite two column to one
unite(data = df, col = info, group, gender)

######################ggplot2 library######################
#1. point plot
p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()
p + geom_point(aes(color = qsec)) #color
p + geom_point(aes(shape = factor(gear))) #shape
p + geom_point(aes(size = qsec)) #size
p + geom_point(aes(size = 5)) #size
p + geom_point(aes(alpha = qsec)) #transparency
