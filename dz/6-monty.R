# Bacamo dvije kocke. Koja je vjerojatnost da je njihova suma veća ili jednaka 7?
# P = 21/36 da ce biti >= 7

outcomes <- c(0, 1)
probabilities <- c(15/36, 21/36)

set.seed(2)
smpl_size = 100000
out <- sample(outcomes, prob = probabilities, size = smpl_size, replace = TRUE)

sum(out)

out.table <- table(out)

out.table

# 21/36 = 0.58333...
out.table['1']/smpl_size * 100

# rez je 0.58083...