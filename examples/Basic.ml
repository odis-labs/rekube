module M = struct
  let make ~name  ~version  () =
    ignore (name, version)
end

let m = M [%bs.obj {
  name = "m1";
  version = "something"
}]
