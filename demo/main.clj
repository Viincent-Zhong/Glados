(import demo/print.clj)
(x = 3000)
(if ((x % 4) eq? 3)
    (print true)
    (print false)
)
