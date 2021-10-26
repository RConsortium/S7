# printing R7 objects work

    Code
      print(x)
    Output
      <range> <R7_object>
      @start   1
      @end    10
      @length  9

# printing R7 classes work

    Code
      range
    Output
      <R7_class>
      @name range
      @parent <R7_object>
      @properties
       $start  <numeric>
       $end    <numeric>
       $length <numeric>

# str with R7 objects work

    Code
      str(x)
    Output
       <R7_object>
      List of 4
       $ class       : chr [1:2] "range" "R7_object"
       $ object_class: <R7_object>
      function (start, end)  
        ..- attr(*, "name")= chr "range"
        ..- attr(*, "parent")= <R7_object>
      function ()  
        .. ..- attr(*, "name")= chr "R7_object"
        .. ..- attr(*, "properties")= list()
        .. ..- attr(*, "constructor")=function ()  
        .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. ..$ name       : chr "R7_object"
        .. ..$ properties : list()
        .. ..$ constructor:function ()  
        .. ..$ validator  :function (x)  
        .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        ..- attr(*, "properties")=List of 3
        .. ..$ start :List of 4
        .. .. ..$ name  : chr "start"
        .. .. ..$ class : chr "numeric"
        .. .. ..$ getter: NULL
        .. .. ..$ setter: NULL
        .. .. ..- attr(*, "class")= chr "R7_property"
        .. ..$ end   :List of 4
        .. .. ..$ name  : chr "end"
        .. .. ..$ class : chr "numeric"
        .. .. ..$ getter: NULL
        .. .. ..$ setter: NULL
        .. .. ..- attr(*, "class")= chr "R7_property"
        .. ..$ length:List of 4
        .. .. ..$ name  : chr "length"
        .. .. ..$ class : chr "numeric"
        .. .. ..$ getter:function (x)  
        .. .. ..$ setter:function (x, value)  
        .. .. ..- attr(*, "class")= chr "R7_property"
        ..- attr(*, "constructor")=function (start, end)  
        ..- attr(*, "validator")=function (x)  
      List of 7
        ..$ name       : chr "range"
        ..$ parent     : <R7_object>
      function ()  
        .. ..- attr(*, "name")= chr "R7_object"
        .. ..- attr(*, "properties")= list()
        .. ..- attr(*, "constructor")=function ()  
        .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. ..$ name       : chr "R7_object"
        .. ..$ properties : list()
        .. ..$ constructor:function ()  
        .. ..$ validator  :function (x)  
        .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        ..$ properties :List of 3
        .. ..$ start :List of 4
        .. .. ..$ name  : chr "start"
        .. .. ..$ class : chr "numeric"
        .. .. ..$ getter: NULL
        .. .. ..$ setter: NULL
        .. .. ..- attr(*, "class")= chr "R7_property"
        .. ..$ end   :List of 4
        .. .. ..$ name  : chr "end"
        .. .. ..$ class : chr "numeric"
        .. .. ..$ getter: NULL
        .. .. ..$ setter: NULL
        .. .. ..- attr(*, "class")= chr "R7_property"
        .. ..$ length:List of 4
        .. .. ..$ name  : chr "length"
        .. .. ..$ class : chr "numeric"
        .. .. ..$ getter:function (x)  
        .. .. ..$ setter:function (x, value)  
        .. .. ..- attr(*, "class")= chr "R7_property"
        ..$ constructor:function (start, end)  
        ..$ validator  :function (x)  
        ..$ class      : chr [1:2] "R7_class" "R7_object"
       $ start       : num 1
       $ end         : num 10

# str with R7 objects of base classes work

    Code
      str(y)
    Output
       <R7_object>
       num 1
       - attr(*, "object_class")= <R7_object>
      function (x)  
        ..- attr(*, "name")= chr "number"
        ..- attr(*, "parent")= <R7_object>
      function (x = numeric())  
        .. ..- attr(*, "name")= chr "numeric"
        .. ..- attr(*, "parent")= <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..- attr(*, "properties")= list()
        .. ..- attr(*, "constructor")=function (x = numeric())  
        .. ..- attr(*, "validator")=function (x)  
      List of 7
        .. ..$ name       : chr "numeric"
        .. ..$ parent     : <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..$ properties : list()
        .. ..$ constructor:function (x = numeric())  
        .. ..$ validator  :function (x)  
        .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        ..- attr(*, "properties")= list()
        ..- attr(*, "constructor")=function (x)  
        ..- attr(*, "validator")=function (x)  
      List of 7
        ..$ name       : chr "number"
        ..$ parent     : <R7_object>
      function (x = numeric())  
        .. ..- attr(*, "name")= chr "numeric"
        .. ..- attr(*, "parent")= <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..- attr(*, "properties")= list()
        .. ..- attr(*, "constructor")=function (x = numeric())  
        .. ..- attr(*, "validator")=function (x)  
      List of 7
        .. ..$ name       : chr "numeric"
        .. ..$ parent     : <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..$ properties : list()
        .. ..$ constructor:function (x = numeric())  
        .. ..$ validator  :function (x)  
        .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        ..$ properties : list()
        ..$ constructor:function (x)  
        ..$ validator  :function (x)  
        ..$ class      : chr [1:2] "R7_class" "R7_object"
      List of 2
       $ class       : chr [1:3] "number" "numeric" "R7_object"
       $ object_class: <R7_object>
      function (x)  
        ..- attr(*, "name")= chr "number"
        ..- attr(*, "parent")= <R7_object>
      function (x = numeric())  
        .. ..- attr(*, "name")= chr "numeric"
        .. ..- attr(*, "parent")= <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..- attr(*, "properties")= list()
        .. ..- attr(*, "constructor")=function (x = numeric())  
        .. ..- attr(*, "validator")=function (x)  
      List of 7
        .. ..$ name       : chr "numeric"
        .. ..$ parent     : <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..$ properties : list()
        .. ..$ constructor:function (x = numeric())  
        .. ..$ validator  :function (x)  
        .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        ..- attr(*, "properties")= list()
        ..- attr(*, "constructor")=function (x)  
        ..- attr(*, "validator")=function (x)  
      List of 7
        ..$ name       : chr "number"
        ..$ parent     : <R7_object>
      function (x = numeric())  
        .. ..- attr(*, "name")= chr "numeric"
        .. ..- attr(*, "parent")= <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..- attr(*, "properties")= list()
        .. ..- attr(*, "constructor")=function (x = numeric())  
        .. ..- attr(*, "validator")=function (x)  
      List of 7
        .. ..$ name       : chr "numeric"
        .. ..$ parent     : <R7_object>
      function ()  
        .. .. ..- attr(*, "name")= chr "R7_object"
        .. .. ..- attr(*, "properties")= list()
        .. .. ..- attr(*, "constructor")=function ()  
        .. .. ..- attr(*, "validator")=function (x)  
      List of 6
        .. .. ..$ name       : chr "R7_object"
        .. .. ..$ properties : list()
        .. .. ..$ constructor:function ()  
        .. .. ..$ validator  :function (x)  
        .. .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        .. ..$ properties : list()
        .. ..$ constructor:function (x = numeric())  
        .. ..$ validator  :function (x)  
        .. ..$ class      : chr [1:2] "R7_class" "R7_object"
        ..$ properties : list()
        ..$ constructor:function (x)  
        ..$ validator  :function (x)  
        ..$ class      : chr [1:2] "R7_class" "R7_object"

# str R7 classes work

    Code
      str(range)
    Output
       <R7_object>
      function (start, end)  
       - attr(*, "name")= chr "range"
       - attr(*, "parent")= <R7_object>
      function ()  
        ..- attr(*, "name")= chr "R7_object"
        ..- attr(*, "properties")= list()
        ..- attr(*, "constructor")=function ()  
        ..- attr(*, "validator")=function (x)  
      List of 6
        ..$ name       : chr "R7_object"
        ..$ properties : list()
        ..$ constructor:function ()  
        ..$ validator  :function (x)  
        ..$ class      : chr [1:2] "R7_class" "R7_object"
       - attr(*, "properties")=List of 3
        ..$ start :List of 4
        .. ..$ name  : chr "start"
        .. ..$ class : chr "numeric"
        .. ..$ getter: NULL
        .. ..$ setter: NULL
        .. ..- attr(*, "class")= chr "R7_property"
        ..$ end   :List of 4
        .. ..$ name  : chr "end"
        .. ..$ class : chr "numeric"
        .. ..$ getter: NULL
        .. ..$ setter: NULL
        .. ..- attr(*, "class")= chr "R7_property"
        ..$ length:List of 4
        .. ..$ name  : chr "length"
        .. ..$ class : chr "numeric"
        .. ..$ getter:function (x)  
        .. ..$ setter:function (x, value)  
        .. ..- attr(*, "class")= chr "R7_property"
       - attr(*, "constructor")=function (start, end)  
       - attr(*, "validator")=function (x)  
      List of 7
       $ srcref     : 'srcref' int [1:8] 5 17 7 3 17 3 5 7
       $ name       : chr "range"
       $ parent     : <R7_object>
      function ()  
        ..- attr(*, "name")= chr "R7_object"
        ..- attr(*, "properties")= list()
        ..- attr(*, "constructor")=function ()  
        ..- attr(*, "validator")=function (x)  
      List of 6
        ..$ name       : chr "R7_object"
        ..$ properties : list()
        ..$ constructor:function ()  
        ..$ validator  :function (x)  
        ..$ class      : chr [1:2] "R7_class" "R7_object"
       $ properties :List of 3
        ..$ start :List of 4
        .. ..$ name  : chr "start"
        .. ..$ class : chr "numeric"
        .. ..$ getter: NULL
        .. ..$ setter: NULL
        .. ..- attr(*, "class")= chr "R7_property"
        ..$ end   :List of 4
        .. ..$ name  : chr "end"
        .. ..$ class : chr "numeric"
        .. ..$ getter: NULL
        .. ..$ setter: NULL
        .. ..- attr(*, "class")= chr "R7_property"
        ..$ length:List of 4
        .. ..$ name  : chr "length"
        .. ..$ class : chr "numeric"
        .. ..$ getter:function (x)  
        .. ..$ setter:function (x, value)  
        .. ..- attr(*, "class")= chr "R7_property"
       $ constructor:function (start, end)  
       $ validator  :function (x)  
       $ class      : chr [1:2] "R7_class" "R7_object"

