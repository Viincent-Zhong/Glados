((fact x) = (if (x eq? 1) 1 (x * (fact (x - 1)))))
(fact 10)
