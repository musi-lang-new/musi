open Instr

let test_arithmetic () =
  let program =
    {
      constants = [||]
    ; procs =
        [|
           {
             name = "main"
           ; param_count = 0
           ; local_count = 1
           ; code =
               [ LdcI4_2; LdcI4_3; Add; Stloc_0; Ldloc_0; LdcI4_1; Sub; Ret ]
           ; external_proc = false
           }
        |]
    ; records = []
    }
  in
  let vm = Vm.create program in
  let exit_code = Vm.run vm in
  Alcotest.(check int) "arithmetic result" 0 exit_code

let test_comparison () =
  let program =
    {
      constants = [||]
    ; procs =
        [|
           {
             name = "main"
           ; param_count = 0
           ; local_count = 0
           ; code = [ LdcI4_5; LdcI4_3; Cgt; Pop; Ret ]
           ; external_proc = false
           }
        |]
    ; records = []
    }
  in
  let vm = Vm.create program in
  let exit_code = Vm.run vm in
  Alcotest.(check int) "comparison result" 0 exit_code

let test_text_constant () =
  let program =
    {
      constants = [| CText "hello" |]
    ; procs =
        [|
           {
             name = "main"
           ; param_count = 0
           ; local_count = 0
           ; code = [ Ldstr 0; Pop; Ret ]
           ; external_proc = false
           }
        |]
    ; records = []
    }
  in
  let vm = Vm.create program in
  let exit_code = Vm.run vm in
  Alcotest.(check int) "text constant" 0 exit_code

let test_procedure_call () =
  let program =
    {
      constants = [||]
    ; procs =
        [|
           {
             name = "main"
           ; param_count = 0
           ; local_count = 0
           ; code = [ LdcI4_5; LdcI4_3; Call 1; Pop; Ret ]
           ; external_proc = false
           }
         ; {
             name = "add"
           ; param_count = 2
           ; local_count = 2
           ; code = [ Ldloc_0; Ldloc_1; Add; Ret ]
           ; external_proc = false
           }
        |]
    ; records = []
    }
  in
  let vm = Vm.create program in
  let exit_code = Vm.run vm in
  Alcotest.(check int) "procedure call" 0 exit_code

let () =
  let open Alcotest in
  run
    "VM"
    [
      ( "execution"
      , [
          test_case "arithmetic" `Quick test_arithmetic
        ; test_case "comparison" `Quick test_comparison
        ; test_case "text constant" `Quick test_text_constant
        ; test_case "procedure call" `Quick test_procedure_call
        ] )
    ]
