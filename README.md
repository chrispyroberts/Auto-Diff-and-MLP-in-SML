# Implementing Automatic Differentiation and a Multilayer Perceptron in SML (A Functional Programming Language)


We represent values as a datatype, which contain refernce cells to their data and gradient, as well as a list of children, and a backward function.

```SML
datatype value = Value of {
  data: real ref,
  grad: real ref,
  backward: unit -> unit,
  children: value list,
  nodeID : int
  };
```

We can then define operations on these values, such as addition. The addition operation takes in two inputs.

```SML
fun add (Value a, Value b) =
let
  val newData = ref (!(#data a) + !(#data b))
  val newGrad = ref 0.0

  # Define the backwards pass function
  fun outBackward () =
    ((#grad a) := !(#grad a) + 1.0 * !outGrad;
     (#grad b) := !(#grad b) + 1.0 * !outGrad)

  val children = [Value a, Value b]
in
  mkValue(outData, outGrad, outBackward, children)
end
```

