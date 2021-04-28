ggplot(data = cps2, aes (x = fsecurity_cat))+geom_bar()

ggplot(data = cps2, aes (x = disability_cat))+geom_bar()

ggplot(data = cps2) +
  geom_histogram(aes (x = hhsize))

ggplot(data = cps2) +
  geom_histogram(aes (x = female))


ggplot(data = cps2) +
  geom_point(aes (x = hhsize, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = female, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = kids, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = elderly, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = black, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = hispanic, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = education, y = fsecurity_cat))

ggplot(data = cps2) +
  geom_point(aes (x = employed, y = fsecurity_cat))

# ggplot(data = cps2) +
#   geom_point(aes (x = disability.cat))
# disbility.cat is categorical and I should probably use something similar to a 
# frequency table like in SAS. Maybe use something else?


# CREATE CLUSTER
# START WITH THE X MATRIX OF NUMERICAL VARIABLES
cps_X = subset(cps2, select = -c(1,2,3))
# WE ALSO NEED TO STANDARDIZE THE VARIABLES IN ORDER TO LIMIT CONTROL, KEEP IT 
# EVEN BETWEEN ALL.
cps_stand = apply(cps_X, 2, function(x){(x - mean(x))/sd(x)})
summary(cps_stand)  


wss = (nrow(cps_stand)-1)*sum(apply(cps_stand,2,var))
for (i in 2:15) {
  wss[i] = sum(kmeans(cps_stand, centers = i)$withinss)
}

plot(1:15, wss, type = "b", xlab = "Number of clusters")

cps_means = kmeans(sb_stand, 7)
str(cps_means)

cps_X$km_cluster = as.numeric(cps_kmeans$cluster)
sb_long = melt(sb_X, id.vars = c("km_cluster"))

View(cps_long)


ggplot(data = cps_long) + 
  geom_boxplot(aes (x = km_cluster, y = value, fill = km_cluster)) +
  face_wrap(~variable, scales = "free")
