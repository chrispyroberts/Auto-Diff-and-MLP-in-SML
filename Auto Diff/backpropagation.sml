(* Function to build the list of nodes in topological order *)
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


(* Backwards function *)
fun backwards (x) =
    let
        fun applyBackward (_, []) = ()
          | applyBackward (startNode, nodes) = 
              (getGrad(startNode) := 1.0;
              foldr (fn (n, ()) => getBackward(n) ()) () nodes)

        val (_, topo) = buildTopo (x, [])
    in
        applyBackward(x, topo)
    end
