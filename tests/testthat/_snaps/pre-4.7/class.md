# S7 object displays list objects nicely

    Code
      foo1(list(x = 1, y = list(a = 21, b = 22)), x = 3, y = list(a = 41, b = 42))
    Output
      <foo1> List of 2
       $ x: num 1
       $ y:List of 2
        ..$ a: num 21
        ..$ b: num 22
       @ x: num 3
       @ y: List of 2
       .. $ a: num 41
       .. $ b: num 42
