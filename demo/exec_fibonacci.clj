((fib-it a b n) = (if (n < 1) a (fib-it b (a + b) (n - 1))))
((fib n) = (fib-it 0 1 n))
(fib 20)