open Alcotest
module Span = Span

let test_make () =
  let span = Span.make 1 10 20 in
  check int "file id" 1 (Span.file span);
  check int "start position" 10 (Span.start span);
  check int "end position" 20 (Span.end_ span)

let test_dummy () =
  let span = Span.dummy in
  check int "dummy file id" 0 (Span.file span);
  check int "dummy start" 0 (Span.start span);
  check int "dummy end" 0 (Span.end_ span)

let test_len () =
  let span = Span.make 0 5 15 in
  check int "length calculation" 10 (Span.len span)

let test_zero_length () =
  let span = Span.make 0 10 10 in
  check int "zero length span" 0 (Span.len span)

let test_large_positions () =
  let span = Span.make 999 1000000 2000000 in
  check int "large file id" 999 (Span.file span);
  check int "large start" 1000000 (Span.start span);
  check int "large end" 2000000 (Span.end_ span);
  check int "large length" 1000000 (Span.len span)

let () =
  run
    "Span"
    [
      ( "Span"
      , [
          test_case "make" `Quick test_make
        ; test_case "dummy" `Quick test_dummy
        ; test_case "len" `Quick test_len
        ; test_case "zero_length" `Quick test_zero_length
        ; test_case "large_positions" `Quick test_large_positions
        ] )
    ]
