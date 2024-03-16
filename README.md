# Implementing Automatic Differentiation in SML (A Functional Programming Language)


We represent values as a datatype

```SML
datatype value = Value of {
  data: real ref,           (* The numerical value of the node *)

  grad: real ref,           (* The gradient of the node w.r.t the final output.
                               Initially set to 0.0 because gradients are accumulated
                               during the backward pass. *)

  backward: unit -> unit,   (* When called computes the gradients of the node's parents in
                               the computational graph based on the node's gradient. *)

  children: value list,     (* Nodes that wer eused to compute the current node's value *)

  nodeID : int              (* We use nodeID to later define a function that checks whether
                               two nodes are equal using their ID's *)
  };
```

Since SML is a functional language, and most things are immutable, we need to use reference cells in order to keep track of gradients and update them using lambda functions. We can then define operations using this value datatype.

```SML
fun add (Value a, Value b) =
let
  val outData = ref (!(#data a) + !(#data b))
  val outGrad = ref 0.0

  (* For addition, the gradient of each operand w.r.t the output is 1,
     so we simply add the value at outGrad to each operands grad value.
     This reflects the chain rule in differentiation *)
  fun outBackward () =
    ((#grad a) := !(#grad a) + 1.0 * !outGrad;
     (#grad b) := !(#grad b) + 1.0 * !outGrad)

  val children = [Value a, Value b]
in
  mkValue(outData, outGrad, outBackward, children)
end
```

We can create similar functions for multiplication, tanh, sigmoid, exponential, etc, which allows us to make simple mathematical expressions! We just need to pay attention to properly implementing the backwards gradient calculations. 

```sml
(* mkDefaultValue : real -> value
   Requires: true
   Ensures: mkDefaultValue a => v, where v has a data value of a 
*)

val a = mkDefaultValue 1.0;
val b = mkDefaultValue 6.0;
val c = mkDefaultValue 3.0;
val d = add(a, b)
val e = mul(d, c)
```

Corresponding Computational Graph:
![image](https://github.com/chrispyroberts/Auto-Diff-and-MLP-in-SML/assets/98184754/c1ae01fa-dd25-48d2-8e4e-dccf02653e6b)

When we call the backwards pass, we update the gradients of this computational graph. We need to implement this feature however by building the topological list from the computational graph and then recursively applying the backwards pass. When building the topological list, we start at the final node, and recursively build the topological graph on the current node's children, only adding the current node to our list once all the current node's children have been added. 

```SML
(* Function to build the list of nodes in topological order *)
fun buildTopo ((Value v), visited) =
  if List.exists (valueEq (Value v)) visited
  then (visited, [])
  else
    let
        val newVisited = (Value v) :: visited

        fun searchChildren (child, (vis, topo)) =
            let
                val (newVis, newTopo) = buildTopo (child, vis)
            in
                (newVis, newTopo @ topo)
            end

        val (finalVisited, childrenTopo) = foldl searchChildren (newVisited, []) (#children v)
    in
      (finalVisited, childrenTopo @ [Value v])
    end
```

For our current computational graph, this function will give us the following list:

```SML
[a, b, d, c, e] : value list
```

With this list, we call the backwards function stored in each value in reverse order, starting by setting the gradient of ```e``` to ```1.0```, then calling the backwards function stored in subsequent value until we reach ```a```, at which point the gradient has been computed.

```SML
(* Backwards function *)
fun backwards (Value node) =
    let
        fun applyBackward (_, []) = ()
          | applyBackward ((Value startNode), nodes) =
            let
                fun apply [] = ()
                  | apply ((Value v) :: vs) = ((#backward v) (); apply vs)
            in
              ((#grad startNode) := 1.0; apply (List.rev nodes))
            end

        val (_, topo) = buildTopo ((Value node), [])
    in
        applyBackward((Value node), topo)
    end
```







