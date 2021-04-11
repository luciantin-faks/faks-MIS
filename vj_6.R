

# 10 tickets - 1 gold, 9 blank
# 1/10 = 0.1
tickets <- c("gold", rep("blank", 9))

tickets

set.seed(2)

smpl_size <- 1000000

tickets.random <- sample(tickets, size = smpl_size, replace = TRUE)

tickets.table <- table(tickets.random)

tickets.table
# 9,93%

tickets.table['gold'] / smpl_size  * 100


#############################################

# 1 kocka
# 3, 4, 5 - nista  ->  3/6
# 1, 2 - -1 dolar  -> 2/6
# 6 - +3 dolara   ->  1/6 (teorisjki prob)

outcomes <- c(-1, 0, 3)
probabilities <- c(2/6, 3/6, 1/6)

set.seed(2)

out <- sample(outcomes, prob = probabilities, size = smpl_size, replace = TRUE)

sum(out)

out.table <- table(out)

out.table

# 33.3
out.table['-1']/smpl_size * 100


#############################################

# s -> jacina projektila
# s = uniform(0,12)
# s <= 5 -> nema stete
# s > 5 -> damage = 12 - s

set.seed(2)

sample <- runif(smpl_size, min = 0, max = 12)

strengths <- sample - 5
strengths[strengths < 0] <- 0

sum(strengths)
mean(strengths)










