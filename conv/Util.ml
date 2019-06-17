open Printf
let snake_case =
  let re1 = Re.Pcre.regexp ("([A-Z]+)([A-Z][a-z]{2,})") in
  let re2 = Re.Pcre.regexp ("([a-z0-9])([A-Z])") in
  let re3 = Re.compile (Re.Pcre.re ("-")) in
  let underscore re s =
    let replace groups =
      sprintf ("%s_%s")
        (Re.Group.get groups 1) (Re.Group.get groups 2) in
    Re.replace re ~f:replace s in
  fun s  ->
    let len = String.length s in
    if len > 1
    then
      let s = underscore re1 s in
      let s = underscore re2 s in
      let s = Re.replace_string re3 ~by:(("_")) s in
      (sprintf (("%c")) (s.[0])) ^
        (String.lowercase_ascii (String.sub s 1 ((String.length s) - 1)))
    else s

let is_keyword =
  function
  | (("external"))
    |(("object"))
    |(("to"))
    |(("type")) -> true
  | _ -> false

let normalize n =
  let n = if (n.[0]) = '$' then String.sub n 1 ((String.length n) - 1) else n in
  let n = (snake_case n) |> String.lowercase_ascii in
  if is_keyword n then n ^ (("_")) else n

