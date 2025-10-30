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
let bold_red s = make_color "\027[0;1;31m" s
let bold_yellow s = make_color "\027[0;1;33m" s
let bold_cyan s = make_color "\027[0;1;36m" s
let bold_green s = make_color "\027[0;1;32m" s
