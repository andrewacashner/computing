# 2024/06/19

def identity():
    return lambda x: x

def sum(n, m):
    return n + m

def partial(fn, arg):
    return lambda rest: fn(arg, rest) 

def plus_any_partial(n):
    return partial(sum, n)

def plus_one(n):
    return n + 1

def plus_any_def(n):
   def plusN(m):
       return n + m
   return plusN

def plus_any_lambda(n):
    return lambda m: n + m

def plus_two_param(n):
    return plus_any_lambda(2)(n)

def plus_two_partial_a():
    return lambda n: plus_any_lambda(2)(n)

def plus_two_partial_b(n):
    return partial(sum, 2)(n)

def plus_two_partial_c():
    return partial(sum, 2)

print(identity()(3))
print(sum(1, 2))
print(plus_one(2))
print(plus_any_def(1)(2))
print(plus_any_lambda(1)(2))
print(plus_any_partial(1)(2))
print(plus_two_param(1))
print(plus_two_partial_a()(1))
print(plus_two_partial_b(1))
print(plus_two_partial_c()(1))
