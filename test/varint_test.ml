open Varint.Encoding
open OUnit2

       
let test_varint32 ctx =
  let i = Random.int32 1000000000l in
  let buf = VarInt32.encode i in
  let got = VarInt32.decode buf in

  
  assert_equal i got


let test_varint64 ctx =
  let i = Random.int64 8446744073709551615L in
  let buf = VarInt64.encode i in
  let got = VarInt64.decode buf in

  assert_equal i got



let suite =
  "VarInt Suite" >:::
    [
      "testing VarInt32" >:: test_varint32;
      "testing VarInt64" >:: test_varint64; 
    ]
               
let () =
  run_test_tt_main suite
  
