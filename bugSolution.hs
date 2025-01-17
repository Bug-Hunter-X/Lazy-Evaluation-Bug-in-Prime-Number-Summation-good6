The issue stems from the potential for the `sum` function to evaluate elements of the list `primes` indefinitely before `take 10` fully filters the infinite list.

The solution is to force evaluation of `take 10 primes` before passing it to `sum`.  This ensures that `sum` operates on a finite list.

```haskell
primes :: [Integer]
primes = sieve [2..] 
  where
    sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO ()
main = print $ sum $ take 10 primes -- Added $ to force evaluation
```

This explicit use of the `$` operator (or parentheses) forces the evaluation of `take 10 primes` before `sum` is applied, guaranteeing termination and the correct sum of the first 10 prime numbers.