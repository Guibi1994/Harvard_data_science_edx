library(tidyverse)
library(caret)
library(dslabs)
library(gridExtra)
library(tidyverse)
library(ggplot2)



# 1. Knn loop con saply ----
## ex1 ----

heights <- dslabs::heights


set.seed(1)
# 1. Crear una partición balanceada con "Caret"
test_index <- createDataPartition(
  ## 1.1. Selecioanmos la base y la variable a predecir
  heights$sex, 
  ## 1.2. Número de particiones
  times = 1, 
  ## 1.3. Distribución en test y training
  p = .5, list = FALSE)

## 1.4. Creamos el test set
test_set <- heights[test_index, ]

## 1.4. Creamos el training set
train_set <- heights[-test_index, ]




mi.knn <- function(k) {
  # Modelar
  m1 <- knn3(sex~height, data = train_set,k = k)
  
  # Predecir
  pr.test <- predict(
    m1, test_set, type = "class")
  
  # Evaluar
  caret::F_meas(pr.test, test_set$sex)
}

results <- sapply(seq(1,101,3), mi.knn) %>% 
  as.data.frame() %>% 
  mutate(k = seq(1,101,3)) %>% 
  rename(f1 = 1) #%>%

results %>% 
  filter(f1==max(results$f1))

results %>% 
  ggplot(aes(k,f1)) +
  geom_point()+
  geom_path()





## ex2 ----
data("tissue_gene_expression")


# Prepare data
pr <- tissue_gene_expression$x %>% 
  as.data.frame() %>% 
  mutate(y = tissue_gene_expression$y) %>% 
  select(y, everything())

names(pr) <- stringr::str_replace_all(
  names(pr), "-",replacement = "_")


# Divide data
set.seed(1)

indx <- caret::createDataPartition(pr$y,
                                   times = 1, p = .5, list = F)

test_set <- pr[indx,]
train_set <- pr[-indx,]


# Function
mi.knn <- function(k) {
  # modelar
  m1 <- knn3(y~.,train_set,k =k)
  # predecir
  pred <- predict(
    m1, test_set, type = "class")
  # evaluar
  confusionMatrix(
    pred, test_set$y)$overall[1]
  
}

# Loop

sapply(seq(1, 11, 2), mi.knn) %>% 
  as.data.frame() %>% 
  mutate(k = seq(1, 11, 2))


# 2. Modelos con Bootstraping ------------------------------------- ------------


## 2.1. Create multiple samples with cartet
data("mnist_27")
set.seed(1995)


ind <- createResample(mnist_27$train$y,10)

table(ind[1])[1:10]

ind %>% unlist() %>% 
  as_tibble() %>% 
  group_by(value) %>% 
  summarise(n = n())

message("With createResample(), sample have replacements")


## 2.2. Montecarlo simulation

set.seed(1)
data.frame(
  mean = mean(quantile(replicate(10000, rnorm(100,0,1)),.75)),
  sd = sd(quantile(replicate(10000, rnorm(100,0,1)),.75)))


set.seed(1)
pr <- replicate(10000,quantile(rnorm(100,0,1),.75))
mean(pr)
sd(pr)

## 2.3. Create  BOOTSTRAP
set.seed(1)
y <- rnorm(100,0,1)
set.seed(1)


ind <- createResample(y,10000)
pr <- c()

for (i in 1:10000) {
  pr <- c(pr, quantile(y[c(unlist(ind[i]))],.75))
  
}

mean(pr)
sd(pr)

# 3. Taller caert ----

## ex1.

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
{set.seed(1)
  fit <- train(x_subset, y, method = "glm")
  fit$results}



pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}


ind <- which(pvals <0.01)
length(ind)

x_subset <- x[ ,ind]
{set.seed(1)
  fit <- train(x_subset, y, method = "glm")
  fit$results}


