(do
  (def inc (fn (x) (+ x 1)))
  (def dec (fn (x) (- x 1)))
  (def not (fn (x) (if x false true)))
  (def reduce (fn (f memo xs)
                  (if (any? xs)
                    (reduce f (f memo (first xs)) (rest xs))
                    memo)))
  (def map (fn (f xs)
               (reduce
                 (fn (acc x) (conj acc (f x)))
                 (list)
                 xs)))

  (def filter (fn (f xs)
                  (reduce
                    (fn (acc x) (if (f x) (conj acc x) acc))
                    (list)
                    xs)))

  (def remove (fn (f xs) (filter (fn (x) (not (f x))) xs)))
  )
