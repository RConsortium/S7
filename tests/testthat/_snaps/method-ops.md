# `!` requires a length-1 signature

    Code
      method(`!`, list(Logical, class_missing)) <- (function(e1, e2) e1)
    Condition
      Error in `method<-`:
      ! `signature` must be length 1.

