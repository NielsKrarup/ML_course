library(dplyr)

c <- 1e3
g <- c(47, 35*c)
b <- c(22, 40*c)
u <- c(21, 36*c)

sqrt(sum((g-u)^2))
sqrt(sum((b-u)^2))


# simulate hospital  ------------------------------------------------------

p_bias_sim_fun <- function(
    n_sim = 1e3,
    mu_team_vec = 0.5,
    teams = 1,
    threshold = 0.05,
    K = NULL){
  

#Set teams bias vector if only scalar supplied
if(length(mu_team_vec) != teams){
  mu_team_vec <- rep(mu_team_vec, length = teams)
  }
if(is.null(K)) K <- ceiling(log(0.05) / (-2*0.05^2))

#simulate teams x n_sim
res <-
  bind_rows(lapply(
    1:teams,
    FUN = function(team_in) {
      tibble(
        team = team_in,
        no_sim = 1:n_sim,
        emp_loss = rbinom(n = n_sim, size = K, prob = mu_team_vec[team_in]) / K,
        bias = mu_team_vec[team_in] - emp_loss
      )
    }
  ))

#Select team with lowest emp loss
select_res <- 
  res %>% group_by(no_sim) %>% 
  filter(emp_loss == min(emp_loss), !duplicated(bias)) %>% 
  distinct() %>% ungroup() %>% arrange(no_sim)

out <-
select_res %>% summarise(prop_bias_under_thres = mean(bias >= threshold)) %>% pull(prop_bias_under_thres)

return(list(select_res, p_bias = out, K))

}


p_bias_sim_fun(n_sim = 1e4, mu_team_vec = 0.5, teams = 20, K = 1200)


K_vec <- ceiling(seq(100, 900, length.out = 9))
p_vec <- sapply(K_vec, FUN = function(k) p_bias_sim_fun(K = k, n_sim = 1e4)$p_bias)

p_vec_teams_20 <- sapply(K_vec, FUN = function(k) p_bias_sim_fun(K = k, teams = 20, n_sim = 1e4)$p_bias)

plot(K_vec, p_vec, type = "b", 
     main = "prob of bias >= 0.05. For mu = 0.5 and 1 team compeeting", xlab = "K", ylab = "probability")
abline(h = 0.05, lty = 2)


#20 teams
plot(K_vec, p_vec_teams_20, type = "b", col = 2, ylim = range(p_vec, p_vec_teams_20),  
     main = "prob of bias >= 0.05. For mu = 0.5 and 20 teams compeeting", xlab = "K", ylab = "probability")
points(K_vec, p_vec, type = "b");abline(h = 0.05, lty = 2)


hh <- 
replicate(n = n_sim, expr = {
  min(rbinom(n = 20, size = K, prob = mu_team_vec)/K)
  
})
hist(hh, xlim = range(hh, mu));abline(v = c(mu, mu-threshold), lty = 1:2)



#sim x team
0.05*exp(0.05^2*2*1000)
