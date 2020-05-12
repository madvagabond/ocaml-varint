type error = UnexpectedEof


module type S = sig

  type t
  
  val to_cstruct: t -> Cstruct.t
  val of_cstruct: Cstruct.t -> (t * Cstruct.t, error) result

  (** 
     put_uvarint buffer off int  


     be careful with this function be sure that the buffer has enough space 
     the max sizes for 64 bit is 10, 5 for 32 bit, and 3 for 16 bit so be sure you have that extra space allocated just in case
  
  *)

  val put_uvarint: Cstruct.t -> t -> int

  
end





module Make: functor (I: Stdint.Int) -> S with type t = I.t


