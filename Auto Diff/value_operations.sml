(* Implemented operations:
    - addition
    - multiplication
    - tanh                      *)



(* value list * value list -> value list *)
(* Derivative:  
    d(add(x, y)) / dx = 1.0
    d(add(x, y)) / dy = 1.0

*)
fun add(x : value, y : value) =
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

(* value list * value list -> value list *)
(* Derivative:  
    d(mul(x, y)) / dx = y
    d(mul(x, y)) / dy = x

*)
fun mul(x, y) =
  let
    val xData = getData(x)
    val yData = getData(y)
    val xGrad = getGrad(x)
    val yGrad = getGrad(y)

    val outData = ref (!xData * !yData)
    val outGrad = ref 0.0

    fun outBackwards () =
      let
        val dx = !yData * !outGrad
        val dy = !xData * !outGrad
      in
        (xGrad := !xGrad + dx * !outGrad;
         yGrad := !yGrad + dy * !outGrad)
      end
    
    val children = [x, y]
  in
    mkValue(outData, outGrad, outBackwards, children)
  end

(* value list * value list -> value list *)
(* Derivative
    d(tanH(x))/dx = 1 - tanh(x)^2  *
*)
fun tanh (x) =
    let
        val xData = getData(x)
        val xGrad = getGrad(x)

        val exp2x = Math.exp(2.0 * !xData)
        val outData = ref ((exp2x - 1.0) / (exp2x + 1.0))
        val outGrad = ref 0.0

        fun outBackward () =
          let
            val dx = 1.0 - (!outData)*(!outData)
          in
            (xGrad := !xGrad +  dx * !outGrad)
          end

        val children = [x]
    in
        mkValue (outData, outGrad, outBackward, children)
    end