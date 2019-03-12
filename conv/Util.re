
open Printf;

let snake_case = {
  let re1 = Re.Pcre.regexp("([A-Z]+)([A-Z][a-z]{2,})");
  let re2 = Re.Pcre.regexp("([a-z0-9])([A-Z])");
  let re3 = Re.compile(Re.Pcre.re("-"));
  let underscore = (re, s) => {
    let replace = groups =>
      sprintf("%s_%s", Re.Group.get(groups, 1), Re.Group.get(groups, 2));
    Re.replace(re, ~f=replace, s);
  };
  s => {
    let len = String.length(s);
    if (len > 1) {
      let s = underscore(re1, s);
      let s = underscore(re2, s);
      let s = Re.replace_string(re3, ~by="_", s);
      sprintf("%c", s.[0])
      ++ String.lowercase_ascii(String.sub(s, 1, String.length(s) - 1));
    } else {
      s;
    };
  };
};


let is_keyword =
  fun
  | "external"
  | "object"
  | "to"
  | "type" => true
  | _ => false;


let normalize = n => {
  let n =
    if (n.[0] == '$') {
      String.sub(n, 1, String.length(n) - 1);
    } else {
      n;
    };
  let n = snake_case(n) |> String.lowercase_ascii;
  if (is_keyword(n)) {
    n ++ "_";
  } else {
    n;
  };
};

