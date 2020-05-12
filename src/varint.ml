type error = UnexpectedEof

module type S = sig

  type t
  
  val to_cstruct: t -> Cstruct.t
  val of_cstruct: Cstruct.t -> (t * Cstruct.t, error) result
  
  val put_uvarint: Cstruct.t -> t -> int

  
end




module Make (I: Stdint.Int) = struct
  type t = I.t 


  let of_int, to_int = I.of_int, I.to_int
  
  let (lor), (land), (lsl), (asr) = I.(logor, logand, shift_left, shift_right )
  let e = of_int 0x80

  let shft = of_int 0x7F
  

  let put_uvarint buf t =

    let n = ref t in 

    let rec aux off  =


      let cont () =
        let b = !n lor e |> to_int in
        let _ = Cstruct.set_uint8 buf off b in
        let _ = n := !n asr 7 in
        aux (off + 1)
      in
      
        
      if !n >= e then
        cont ()
       
      else
        let _ = Cstruct.set_uint8 buf off (to_int !n) in
        off+1
    in
    
    aux 0



  let to_cstruct t =
    let b = Cstruct.create 10 in
    let w = put_uvarint b t in
    let c, _ = Cstruct.split b w in
    c 



  
  let of_cstruct buf =
    let v = ref I.zero in
    let y = ref 0 in
    
    let rec aux off =
      let b = Cstruct.get_uint8 buf off |> of_int in
      
      let cont () =

       let _ = v := !v lor (b land shft) lsl !y in
       let _ = y := !y + 7 in

       aux (Int.add off 1)

  
      in


      if b land e <> I.zero then
        cont ()
      else
        let _ = v := !v lor (b land shft) lsl !y  in
        let _s, rest = Cstruct.split buf (Int.add 1 off) in
        (!v, rest)
  
    in
    try
      Ok (aux 0)
    with
      _ -> Error UnexpectedEof




  
  

  
    

end 


