# Implementing Automatic Differentiation in SML (A Functional Programming Language)


We represent values as a datatype

```SML
datatype value = Node of value list 
               | Data of real ref           (* The numerical value of the node                              *)
               | Grad of real ref           (* The gradient of the node w.r.t the final output.
                                               Initially set to 0.0 because gradients are accumulated
                                               during the backward pass.                                    *)
               | Backward of unit -> unit   (* When called computes the gradients of the node's parents
                                               in the computational graph based on the node's gradient.     *)
               | Children of value list     (* Nodes that wer eused to compute the current node's value     *)
               | NodeID of int              (* We use nodeID to later define a function that checks whether
                                               two nodes are equal using their ID's                         *)
```

Since SML is a functional language, and most things are immutable, we need to use reference cells in order to keep track of gradients and update them using lambda functions. We can then define operations using this value datatype.

Here's a list of our helper functions that perform simple operations on the value datatype
```SML
(* Returns the data ref cell of a node *)
fun getData(Node a : value) : real ref

(* Returns the grad ref cell of a node+ *)
fun getGrad(Node a : value) : real ref 

(* Returns the backward function of a node *)
fun getBackward(Node a : value) : unit -> unit

(* Returns the children of a node *)
fun getChildren(Node a : value) : value list 

(* Returns the ID of a node *)
fun getNodeID(Node a : value) : int

(* Makes a value with Data d, gradient 0, the identity unit function, no children, and a unique node ID *)
fun mkDefaultValue (d : real) : value

(* returns a value based on specified input values. Automatically assignes a unique nodeID *)
fun mkValue(d : real ref, g : real ref, b : unit -> unit, c : value list) : value

(* checks if two nodes are equal by comparing their node ID *)
fun valueEq (x : value) (y : value) : bool
```

Now we're ready to start building out some mathematical operations on the value datatype. We'll only show addition here for the sake of brevity.

```SML
(* Derivative:  
    d(add(x, y)) / dx = 1.0
    d(add(x, y)) / dy = 1.0
*)
fun add(x : value, y : value) : value =
  let
    val xData = getData(x)
    val yData = getData(y)
    val xGrad = getGrad(x)
    val yGrad = getGrad(y)

    val outData = ref (!xData + !yData)
    val outGrad = ref 0.0

    fun outBackwards () =
      let
        val dx = 1.0
        val dy = 1.0
      in
        (xGrad := !xGrad + dx * !outGrad;
         yGrad := !yGrad + dy * !outGrad)
      end

    val children = [x, y]

  in
    mkValue(outData, outGrad, outBackwards, children)
  end
```

We can create similar functions for multiplication, tanh, sigmoid, exponential, etc, which allows us to make simple mathematical expressions! We just need to pay attention to properly implementing the backwards gradient calculations. 

```SML
val a = mkDefaultValue 1.0;
val b = mkDefaultValue 6.0;
val c = mkDefaultValue 3.0;
val d = add(a, b)
val e = mul(d, c)
```

Corresponding Computational Graph:
![image](https://github.com/chrispyroberts/Auto-Diff-and-MLP-in-SML/assets/98184754/c1ae01fa-dd25-48d2-8e4e-dccf02653e6b)

When we call the backwards pass, we update the gradients of this computational graph. This works by building the topological list from the computational graph and then calling foldr over the returned list and applying the backwards functions stored at each element. When building the topological list, we start at the final node, and recursively build the topological graph on the current node's children, only adding the current node to our list once all the current node's children have been added. 

```SML
fun buildTopo (n, visited) =
  if List.exists (valueEq n) visited
  then (visited, [])
  else

    let
        val newVisited = n :: visited

        fun searchChildren (child, (vis, topo)) =
            let
                val (newVis, newTopo) = buildTopo (child, vis)
            in
                (newVis, newTopo @ topo)
            end

        val (finalVisited, childrenTopo) = foldl searchChildren (newVisited, []) (getChildren n)
    in
      (finalVisited, childrenTopo @ [n])
    end
```

For our current computational graph, this function will give us the following list:

```SML
[a, b, d, c, e] : value list
```

With this list, we call the backwards function stored in each value in reverse order, starting by setting the gradient of ```e``` to ```1.0```, then calling the backwards function stored in each subsequent value until we reach ```a```, at which point the gradient has been computed.

```SML
(* Backwards function *)
fun backwards (x) =
    let
        fun applyBackward (_, []) = ()
          | applyBackward (startNode, nodes) = 
            (getGrad(startNode) := 1
             foldr (fn (n, ()) => getBackward(n) ()) () nodes)

        val (_, topo) = buildTopo (x, [])
    in
        applyBackward(x, topo)
    end
```

Now we can call the backwards pass on the final node to update all the gradients.

```SML
val () = backwards(e)
```

![image](https://github.com/chrispyroberts/Auto-Diff-and-MLP-in-SML/assets/98184754/bd6db3f1-1213-4197-a7cf-d8559660a21d)


