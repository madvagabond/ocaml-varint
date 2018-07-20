

module type VarIntEncoding = sig
  type t
         
  val encode: t -> Cstruct.t
  val decode: Cstruct.t -> t
                             
end


module VarInt32: VarIntEncoding with type t = int32
module VarInt64: VarIntEncoding with type t = int64



