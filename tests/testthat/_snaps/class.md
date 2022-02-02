# R7_class: can be printed

    Code
      my_class
    Output
      <R7_class>
      @name my_class
      @parent <R7_object>
      @properties

# R7_class: str yields all details when used at top-level

    Code
      str(my_class)
    Output
      <my_class/R7_object> constructor
      @ name       :  chr "my_class"
      @ parent     :  <R7_object> constructor
      @ properties :  list()
      @ constructor:  function ()  
      @ validator  :  function (x)  
      @ class      :  chr [1:2] "R7_class" "R7_object"
    Code
      str(list(my_class))
    Output
      List of 1
       $ : <my_class/R7_object> constructor

# classes can use unions in properties

    <my_class>@name must be of class <character>, <factor>:
    - `value` is of class <numeric>

# constructor  types check their values

    `.data` must be <integer> not <character>

# can get class from base constructor

    Could not find class for constructor function

