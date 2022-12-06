michelle = function(n_exp = 100, 
                    x0 = 0, p = c(0.5, 0.5), move = c(2, -1),
                    seed = 777,
                    max_moves = max(1000, -2 * x0 / sum(move * p))) {
  zero_visits = rep(FALSE, n_exp)
  print(max_moves)
  for (id_exp in 1:n_exp) {
    set.seed(seed + id_exp)
  
    moves = sample(move, size = max_moves, replace = TRUE, prob = p)
    x = x0 + cumsum(moves)
    zero_visits[id_exp] = (0 %in% x)
  }
  prob_hat = mean(zero_visits)
  return(prob_hat)
}

michelle(x0 = -100, n_exp = 100000, seed = 77)
