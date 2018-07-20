module type VarIntEncoding = sig
  type t
         
  val encode: t -> Cstruct.t
  val decode: Cstruct.t -> t
                             
end


module type Integer = sig
  type t
         
  val logor: t -> t -> t
  val logand: t -> t -> t
                          
  val shift_left: t -> int -> t
  val shift_right: t -> int -> t

  val of_int: int -> t
  val to_int: t -> int

  val size: int
                                                                        
end 



                                              
let get_slice buf =
  let off = Mstruct.offset buf in
  let pos = (0 - off) in
  Mstruct.shift buf pos;
  Mstruct.sub buf 0 off |> Mstruct.to_cstruct
              

       
                        
module Make (INT: Integer) = struct

  type t = INT.t

  open INT
             

  let (lor), (land), (lsl), (asr) = logor, logand, shift_left, shift_right

                                                                
  let d = of_int 0x80

  let zero = of_int 0
                    
  let shft = of_int 0x7F
                    
                    

  let encode t =
    let buf = Mstruct.create size in
    let n = ref t in

    while (!n >= d) do
      let b = !n lor d in
      Mstruct.set_uint8 buf (to_int b);

      n := !n asr 7 
    done;

    Mstruct.set_uint8 buf (to_int !n);

    get_slice buf

    



  let max_i = if size = 64 then 70 else 35
                                
                    
             
                       

  let decode buf0 =
    let buf = Mstruct.of_cstruct buf0 in

    let read () =
      Mstruct.get_uint8 buf 
    in

    

    let v = ref (of_int 0) in
    let y = ref 0 in

    let rec aux () =
      let b = read () |> of_int in

      if b land d <> zero then
        begin

          v := !v lor (b land shft) lsl !y; 
          y := !y + 7;


          aux () 
        end



      else
        v := !v lor ( (b land shft) lsl !y);
      
      

    in
    
    aux (); 
    !v 
                 

    
                                 
                                 
end 

                            

module I32 = struct
  include Int32
  let size = 32
end
               
module VarInt32 = Make(I32)

module I64 = struct
  include Int64
  let size = 64
end

               
module VarInt64 = Make(I64)


    
