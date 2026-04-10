
my_mat <- 1:16
dim(my_mat) <- c(4, 4)
my_mat
evs <- eigen(my_mat)

evs$val #вектор власних значень 
evs$vec #відповідні власні вектори

zero_vector <- rep(0, length(evs$val))
is_solution_a <- all(my_mat %*% zero_vector == evs$val * zero_vector)

is_solution_b <- all(my_mat %*% (evs$vec[1:4,1] + evs$vec[1:4,2]) == evs$val * (evs$vec[1:4,1] + evs$vec[1:4,2]))

is_solution_b <- all(my_mat %*% (evs$vec[1:4,1] + evs$vec[1:4,2]) == evs$val[4] * (evs$vec[1:4,1] + evs$vec[1:4,2]))


is_solution_b <- all(my_mat %*% (evs$vec[1,1:4] + evs$vec[2,1:4]) == evs$val * (evs$vec[1,1:4] + evs$vec[2,1:4]))


# Визначимо матрицю A та власне число lambda
A <- matrix(c(1, 2, 3, 4), nrow = 2)
lambda <- 2

# Визначимо два власні вектори
x1 <- c(1, 1)
x2 <- c(2, 2)

# Перевірка властивості (a) - нульовий вектор
zero_vector <- rep(0, length(x1))
is_solution_a <- all(A %*% zero_vector == lambda * zero_vector)

# Перевірка властивості (b) - сума власних векторів
is_solution_b <- all(A %*% (x1 + x2) == lambda * (x1 + x2))

# Виведення результатів
is_solution_a  # Повинно бути TRUE
is_solution_b  # Повинно бути FALSE

# Виведення результатів
cat("Властивість (a) - нульовий вектор:", is_solution_a, "\n")
cat("Властивість (b) - сума власних векторів:", is_solution_b, "\n")

























# Спочатку задаємо матрицю A і власне значення ??
A <- matrix(c(2, -1, -1, 3), nrow = 2)  # Приклад матриці A (змініть за необхідністю)
lambda <- 2  # Приклад власного значення ?? (змініть за необхідністю)

# Функція для перевірки, чи є вектор розв'язком рівняння Ax = ??x
is_eigen_vector <- function(A, lambda, x) {
  return(all(abs(A %*% x - lambda * x) < 1e-10))  # Перевіряємо за точністю 1e-10
}

# a. Перевірка, чи нульовий вектор x = (0, 0, ..., 0) є розв'язком рівняння Ax = ??x
zero_vector <- rep(0, ncol(A))  # Нульовий вектор відповідного розміру
is_zero_vector_solution <- is_eigen_vector(A, lambda, zero_vector)
cat("a. Нульовий вектор є розв'язком рівняння Ax = ??x:", is_zero_vector_solution, "\n")

# b. Перевірка, чи сума власних векторів x1 і x2 є розв'язком рівняння Ax = ??x
# Вам потрібно визначити власні вектори x1 і x2 і задати їх значення.
x1 <- c(1, 2)  # Приклад власного вектора x1 (змініть за необхідністю)
x2 <- c(-1, 3)  # Приклад власного вектора x2 (змініть за необхідністю)
is_sum_of_eigen_vectors_solution <- is_eigen_vector(A, lambda, x1 + x2)
cat("b. Сума власних векторів x1 і x2 є розв'язком рівняння Ax = ??x:", is_sum_of_eigen_vectors_solution, "\n")

