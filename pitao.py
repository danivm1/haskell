from itertools import islice

def fibLoop(n):
    f = [0, 1]
    for i in range(2, n):
        f.append(f[i-1] + f[i-2])
    return f[0:n]

def fibYield():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a+b

def fibRec(n):
    n -= 1
    if n < 2: return n
    
    return fibRec(n) + fibRec(n-1)


# print(f'Normal: {fibLoop(5)}')
# print(f'Yield: {list(islice(fibYield(), 5))}')
# print(f'Recursão: {fibRec(5)}')



def quicksort(xs: list):
    lesser, bigger = 1, 1
    while xs and lesser and bigger:
        x = xs.pop(0)
        lesser = [a for a in xs if a <= x]
        bigger = [a for a in xs if a >  x]
        xs = quicksort(lesser) + [x] + quicksort(bigger)
        
    return xs

# print(quicksort([5, 1, 9, 4, 6, 7, 3]))

import operator
from typing import List
def zipWith(a: operator, b: List, c: List) -> List:
    l = min(len(b), len(c))
    r = []
    
    for i in range(l):
        r.append(a(b[i], c[i]))
    
    return r
    
# print(zipWith(operator.mul, [1, 2, 3], [2, 2, 2, 10]))

# How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
import itertools
def sqrtSum() -> int:
    n = itertools.count(1)
    s = 0
    i = 0
    
    while s < 1000:
        s += next(n)**0.5
        i += 1
        
    return i

print(sqrtSum())