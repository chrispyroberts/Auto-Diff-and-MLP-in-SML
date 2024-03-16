val globalIDCounter = ref 0

(* Define the value datatype with a record *)

datatype value = Node of value list 
               | Data of real ref 
               | Grad of real ref 
               | Backward of unit -> unit 
               | Children of value list 
               | NodeID of int


(* ALL OF THESE FUNCTIONS SHOULD GO INTO SOME LIBRARY SML FILE THAT WE GIVE TO STUDENTS *)
(*------------------------------------------ *)
(* Returns the data ref cell of a node *)
fun getData(Node a : value) : real ref =
  let

    val (Data d)::(Grad g)::(Backward b)::(Children c)::(NodeID id)::[] = a
  in
    d
  end

(* Returns the grad ref cell of a node *)
fun getGrad(Node a : value) : real ref =
  let
    val (Data d)::(Grad g)::(Backward b)::(Children c)::(NodeID id)::[] = a
  in
    g
  end 

(* Returns the backward function of a node *)
fun getBackward(Node a : value) : unit -> unit =
  let
    val (Data d)::(Grad g)::(Backward b)::(Children c)::(NodeID id)::[] = a
  in
    b
  end 

(* Returns the backward function of a node *)
fun getChildren(Node a : value) : value list =
  let
    val (Data d)::(Grad g)::(Backward b)::(Children c)::(NodeID id)::[] = a
  in
    c
  end 

(* Returns the backward function of a node *)
fun getNodeID(Node a : value) : int =
  let
    val (Data d)::(Grad g)::(Backward b)::(Children c)::(NodeID id)::[] = a
  in
    id
  end 

(* real -> value *)
fun mkDefaultValue d = 
  let
    val data = Data (ref d)
    val grad = Grad (ref 0.0)
    val backwards = Backward (fn () => ())
    val children = Children []
    val id = NodeID (globalIDCounter := !globalIDCounter + 1; !globalIDCounter)
  in
    Node [data, grad, backwards, children, id]
  end

(* real ref * real ref * (() -> ()) * value list list * int -> value list  *)
fun mkValue(d, g, b, c) =
  let
    val data = Data d
    val grad = Grad g
    val backwards = Backward b
    val children = Children c
    val id = NodeID (globalIDCounter := !globalIDCounter + 1; !globalIDCounter)
  in
    Node [data, grad, backwards, children, id]
  end


(* Check if two values are equal by comparing their unique ID *)
(* (value list * value list) -> bool *)
fun valueEq x y = (getNodeID x) = (getNodeID y)
