x <- c(3, 12, 7)
y <- c(4, 9, 11)

z <- x + y
z

x <- x + 2
x

y <- y + 50
y

sum(x)
mean(y)

w <- c(2, 7, NA, 20)
sum(w)

v <- sum(w, na.rm = TRUE)
typeof(v)

x >y

x <- append(x, 12)
x

u <- c(5:20)
u
u <- append(u, letters[6:10])
u
