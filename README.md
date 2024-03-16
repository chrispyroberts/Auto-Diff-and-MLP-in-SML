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
