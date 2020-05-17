module U64 = Varint.Make(Stdint.Uint64)



let test_case i =
  let vi = Stdint.Uint64.of_int i in 
  let b = U64.to_cstruct vi in
  let (j, _rest) = U64.of_cstruct b |>  Result.get_ok in
  vi = j

  
let test =

  let range = QCheck.int_range 0 max_int in 

  QCheck.Test.make ~count:1000 ~name:"varint codec" range test_case



let _ = QCheck.Test.check_exn test ~long:true
    
