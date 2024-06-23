library(sjPlot)
library(ggplot2)

df <- read.csv('stabilization_data.csv')

df <- transform(df, estable=ifelse(diferencia<0.001&num_contextos>10,'yes', 'no'))

df$context <- as.factor(df$context)
df$estable <- as.factor(df$estable)
df

m1 <- glm(estable ~ num_contextos,
           data = df,
           family=binomial())
summary(m1)
plot_model(m1, type = "pred")

boxplot(cosine ~ num_contextos, data = df, col = "white")
stripchart(cosine ~ num_contextos, data = df, method = "jitter", pch=19, col = 2:4, vertical = TRUE, add = TRUE)

boxplot(diferencia ~ num_contextos, data = df, col = "white")
stripchart(diferencia ~ num_contextos, data = df, method = "jitter", pch=19, col = 2:4, vertical = TRUE, add = TRUE)

df_cosines <- read.csv('cosines.csv')
df_cosines$type <- as.factor(df_cosines$type)
df_cosines
#predir context segons cosine similarity
m2 <- glm(type ~ cosine,
          data = df_cosines,
          family=binomial())
summary(m2)
plot_model(m2, type = "pred")

boxplot(cosine ~ type, data = df_cosines, col = "white")
stripchart(cosine ~ type, data = df_cosines, method = "jitter", pch=19, col = 2:4, vertical = TRUE, add = TRUE)

df_cosines_neo = df_cosines[df_cosines$type == "neo", ]

ggplot(df_cosines_neo, aes(x=reorder(word, cosine), y=cosine)) + geom_point()
