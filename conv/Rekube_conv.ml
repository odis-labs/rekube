module List = struct
  include List

  let filter_map f l =
    let rec recurse acc l = match l with
    | [] -> List.rev acc
    | x::l' ->
        let acc' = match f x with | None -> acc | Some y -> y::acc in
        recurse acc' l'
        in recurse [] l
end

let print fmt =
  Fmt.kpf (fun formatter -> Format.pp_print_newline formatter ()) Fmt.stdout fmt

open Stdio
module Kubernetes = Rekube.Kubernetes
module Json = Yojson.Safe
module Json_util = Yojson.Safe.Util


let rec yaml_to_json (y : Yaml.value) : Json.t =
  match y with
  | `Null -> `Null
  | `Bool x -> `Bool x
  | `Float x when ceil x = x -> `Int (int_of_float x)
  | `Float x -> `Float x
  | `String x -> `String x
  | `A xs -> `List (List.map yaml_to_json xs)
  | `O xs -> `Assoc (List.map (fun (k, v) -> (k, yaml_to_json v)) xs)


let imports = ref []

let import path =
  let full_path = String.split_on_char '.' path in
  let full_path_rev = List.rev full_path in
  let last = List.hd full_path_rev in
  let import_path = Fmt.strf "module %s = %s;" last path in
  if not (List.exists (fun i -> i = import_path) !imports) then begin
    imports := import_path :: !imports;
  end;
  last


module Parsetree_encoder = struct
  open Migrate_parsetree.Ast_406
  module Exp = Ast_helper.Exp
  module Const = Ast_helper.Const

  type 'a t = 'a -> Parsetree.expression

  let unit () =
    Exp.construct (Location.mknoloc (Longident.Lident "()")) None

  let bool x =
    Exp.construct (Location.mknoloc (Longident.Lident (string_of_bool x))) None

  let char x =
    Exp.constant (Const.char x)

  let int x =
    Exp.constant (Const.int x)

  let int32 x =
    Exp.constant (Const.int32 x)

  let int64 x =
    Exp.constant (Const.int64 x)

  let float x =
    Exp.constant (Const.float (string_of_float x))

  let string : string t = fun x ->
    Exp.constant (Const.string x)

  let list enc : 'a list t = fun xs ->
    List.fold_right (fun x acc -> [%expr [%e enc x] :: [%e acc]]) xs [%expr []]

  let array enc : 'a array t = fun xs ->
    Exp.array (Array.to_list (Array.map enc xs))

  let pair enc_a enc_b : ('a * 'b) t = fun (a, b) ->
    Exp.tuple [enc_a a; enc_b b]

  let triple enc_a enc_b enc_c : ('a * 'b * 'c) t = fun (a, b, c) ->
    Exp.tuple [enc_a a; enc_b b; enc_c c]

  let option enc : 'a option t = fun x ->
    match x with
    | Some y -> [%expr Some [%e enc y]]
    | None -> [%expr None]

  let rec encode : type a. a Depyt.t -> a t = function
    | Depyt.Self s    -> encode s.self
    | Depyt.Like b    -> like b
    | Depyt.Prim x    -> prim x
    | Depyt.List t    -> list (encode t)
    | Depyt.Array a   -> array (encode a)
    | Depyt.Tuple t   -> tuple t
    | Depyt.Option t  -> option (encode t)
    | Depyt.Record r  -> record r
    | Depyt.Variant _ -> failwith "variant types are not supported"

  and tuple : type a. a Depyt.tuple -> a t = function
    | Depyt.Pair (x,y)     -> pair (encode x) (encode y)
    | Depyt.Triple (x,y,z) -> triple (encode x) (encode y) (encode z)

  and like : type a b. (a, b) Depyt.like -> b t =
    fun {x; g; _ } b -> encode x (g b)

  and prim : type a. a Depyt.prim -> a t = function
    | Depyt.Unit   -> unit
    | Depyt.Bool   -> bool
    | Depyt.Char   -> char
    | Depyt.Int    -> int
    | Depyt.Int32  -> int32
    | Depyt.Int64  -> int64
    | Depyt.Float  -> float
    | Depyt.String -> string

  and record: type a. a Depyt.record -> a t = fun r x ->
    let make =
      let path = String.(sub r.rname 0 (length r.rname - 2)) in
      let path = path ^ ".make" in
      Exp.ident (Location.mknoloc (Longident.parse path))
    in

    let fields = Depyt.fields r in
    let args = List.filter_map (fun f -> field f x) fields in
    Exp.apply make args

  and field: type a. a Depyt.a_field -> a -> (Asttypes.arg_label * Parsetree.expression) option = fun f record ->
    let Depyt.Field f = f in
    let field_type = f.ftype in
    match field_type with
    | Depyt.Option subtype ->
      let field_val = f.fget record in
      begin match field_val with
        | Some subval -> Some (Asttypes.Labelled f.fname, (encode subtype) subval)
        | None -> None
      end
    | _ ->
      let field_val = f.fget record in
      Some (Asttypes.Labelled f.fname, (encode field_type) field_val)
end

module Print = struct
  let unit ppf () = Fmt.string ppf "()"
  let bool = Fmt.bool
  let char = Fmt.char
  let int = Fmt.int
  let int32 = Fmt.int32
  let int64 = Fmt.int64
  let float = Fmt.float
  let string ppf x = Fmt.pf ppf "%S" x

  let list ?(break=false) pp_elt ppf vs =
    let rec loop = function
      | [] -> ()
      | v :: vs ->
        if vs = [] then (Fmt.pf ppf "@[%a@]" pp_elt v) else
          (Fmt.pf ppf "@[%a@],@ " pp_elt v; loop vs)
    in
    if not break then
      (Fmt.pf ppf "@[<1>["; loop vs; Fmt.pf ppf "]@]")
    else
      (Fmt.pf ppf "[@;<0 2>@[<0>"; loop vs; Fmt.pf ppf "@]@,]")

  let array = Fmt.Dump.array
  let pair = Fmt.Dump.pair
  let triple a b c ppf (x, y, z) = Fmt.pf ppf "(%a, %a, %a)" a x b y c z
  let option = Fmt.Dump.option

  let rec t: type a. a Depyt.t -> a Fmt.t = function
  | Depyt.Self s    -> t s.self
  | Depyt.Like b    -> like b
  | Depyt.Prim t    -> prim t
  | Depyt.List (Depyt.Record _ as l) -> list ~break:true (t l)
  | Depyt.List l    -> list (t l)
  | Depyt.Array a   -> array (t a)
  | Depyt.Tuple t   -> tuple t
  | Depyt.Option x  -> option (t x)
  | Depyt.Record r  -> record r
  | Depyt.Variant _ -> failwith "Unexpected variant type in kubernetes object"

  and tuple: type a. a Depyt.tuple -> a Fmt.t = function
  | Depyt.Pair (x,y)     -> pair (t x) (t y)
  | Depyt.Triple (x,y,z) -> triple (t x) (t y) (t z)

  and like: type a b. (a, b) Depyt.like -> b Fmt.t =
    fun {x; g; _ } ppf b -> t x ppf (g b)

  and prim: type a. a Depyt.prim -> a Fmt.t = function
  | Depyt.Unit   -> unit
  | Depyt.Bool   -> bool
  | Depyt.Char   -> char
  | Depyt.Int    -> int
  | Depyt.Int32  -> int32
  | Depyt.Int64  -> int64
  | Depyt.Float  -> float
  | Depyt.String -> string

  and record: type a. a Depyt.record -> a Fmt.t = fun r ppf x ->
    let fields = Depyt.fields r in
    let fields =
      List.filter (fun (Depyt.Field f) ->
          match f.ftype, f.fget x with
          | Depyt.Option _, None -> false
          | _ -> true)
        fields in
    let name = String.sub r.rname 0 (String.length r.rname - 2) in
    let name = import name in
    Fmt.pf ppf "%s {@;<100 2>@[<v 0>%a@]@;}" name (Fmt.list ~sep:(Fmt.always ",@,") (field x)) fields

  and field: type a. a -> a Depyt.a_field Fmt.t = fun record ppf f ->
    let Depyt.Field f = f in
    let field_type = f.ftype in
    let fmt label pp x = Fmt.pf ppf "%S: %a" label pp x in
    let field_val = f.fget record in
    match field_type, field_val with
    | Depyt.Option subtype, Some x -> fmt f.fname (t subtype) x
    | Depyt.Option _, None -> ()
    | _ -> fmt f.fname (t field_type) field_val
end


module type Object = sig
  type t
  [@@deriving yojson]

  val type_info : unit -> t Depyt.t

  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end

let find_object ~api_version ~kind =
  match (api_version, kind) with
  | ("apps/v1", "Deployment") -> (module Rekube.Kubernetes.Definitions.Api.Apps.V1.Deployment : Object)
  | ("extensions/v1beta1", "Deployment") -> (module Rekube.Kubernetes.Definitions.Api.Extensions.V1beta1.Deployment : Object)
  | _ -> invalid_arg("Unknown object kind")


let main _style_renderer _log_level manifest_file =
  let obj =
    match Filename.extension manifest_file with
    | ".json" ->
      Json.from_file manifest_file
    | ".yaml" | ".yml" ->
      let content = In_channel.read_all manifest_file in
      yaml_to_json begin
          match Yaml.of_string content with
          | Ok yaml -> yaml
          | Error (`Msg m) -> failwith m
        end
    | _ ->
      failwith "unknown extension (only yaml and json supported)"
  in
  let api_version = Json_util.to_string (Json_util.member "apiVersion" obj) in
  let kind = Json_util.to_string (Json_util.member "kind" obj) in
  let (module X) = find_object ~api_version ~kind in
  let x =
    match X.of_yojson obj with
    | Ok x -> x
    | Error m -> failwith m in
  let typ = X.type_info () in
  let out = Fmt.strf "%a" (Print.t typ) x in
  Fmt.pr "open Rekube;@.%a@.@.let %s = %s;@."
    (Fmt.list Fmt.string) !imports (String.lowercase_ascii kind) out


open Cmdliner

let manifest_file =
  Arg.(
    info [] ~docv:"FILE" ~doc:"Kubernetes manifest file to be converted. YAML and JSON formats are supported."
    |> pos 0 (some string) None
    |> required
  )

let () =
  Term.(
    (pure(main) $ Fmt_cli.style_renderer() $ Logs_cli.level() $ manifest_file,
     info "rekube" ~version:"0.1.0" ~doc:"Kubernetes configuration toolkit")
    |> eval
    |> exit
  )

