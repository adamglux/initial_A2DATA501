#### SCRAP -- do not commit 

loss <- read.csv("Data/loss.csv", header = T)

# make model
loss.model <- lm(loss ~ factor(gender) + weight + time + trt, data = loss)

typeof(loss.model)
loss.model$model

X <- model.matrix(loss.model)

diag_metrics <- augment(loss.model)
max(diag_metrics$.cooksd)
