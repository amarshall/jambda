(do
  (def inc (fn (x) (+ x 1)))
  (def dec (fn (x) (- x 1)))
  (def not (fn (x) (if x false true)))
  (def reduce (fn (f memo xs)
                  (if (any? xs)
                    (reduce f (f memo (first xs)) (rest xs))
                    memo)))
  )
