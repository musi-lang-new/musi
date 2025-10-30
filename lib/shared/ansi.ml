let make_color code s =
  let buf = Buffer.create 16 in
  Buffer.add_string buf code;
  Buffer.add_string buf s;
  Buffer.add_string buf "\027[0m";
  Buffer.contents buf

let bold s = make_color "\027[1m" s
let red s = make_color "\027[31m" s
let green s = make_color "\027[32m" s
let yellow s = make_color "\027[33m" s
let cyan s = make_color "\027[36m" s
