library(tidyverse)
library(gridExtra)

# create data
my_dat = tibble(x1 = runif(1000, min=0, max=100),
                x2 = runif(1000, min=20, max=85),
                y = 2*x1 + x2**2)

ggplot(my_dat, aes(x1, y)) + geom_point() -> p1;ggplot(my_dat, aes(x2, y)) + geom_point() -> p2
grid.arrange(p1, p2, nrow=1)

# create test data
test = tibble(x1 = runif(1, 0, 100),
              x2 = runif(1, 20, 85))

# set the number of simulations
nruns = 40000
results = tibble(model_num = integer(nruns),
                 pred = numeric(nruns))

# run the simulations
for(mod in 1:nruns){
  if(mod %% (nruns/10) == 0){
    print(mod)
  }
  
  # sample the original data
  my_dat %>% sample_frac(size = 0.7) -> train
  
  model = lm(y ~ x1 + x2, data = train)
  
  pred = predict(model, test)
  
  results$model_num[mod] = mod
  results$pred[mod] = pred
}

ggplot(results, aes(pred)) +
  geom_histogram(bins=100) +
  geom_vline(xintercept = quantile(results$pred, c(0.025, 0.5, 0.975)))
