open Migrate_parsetree.Ast_406
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident


let fail loc s =
  raise (Location.Error (Location.error ~loc ("Error: " ^ s)))


let mk_func ~loc longident =
  let l = Longident.flatten longident in
  let l = List.append l ["make"] in
  (* TODO: Extend the loc col num to include "make". *)
  let longident =
    match Longident.unflatten l with
    | Some lid -> lid
    | None -> failwith (Printf.sprintf "Invalid longident: [%s]" (String.concat "." l)) in
  Exp.ident ~loc (Location.mkloc longident loc)

let unit ?loc () =
  Exp.construct ?loc (Location.mknoloc (Lident "()")) None

let should_rewrite = ref false

let rec expr mapper e =
  match e.pexp_desc with
  (* X { "aaa": vvv } *)
  | Pexp_construct (
      {txt=longident; loc},
      Some {pexp_desc=
        Pexp_extension (
          {txt="bs.obj"; _},
          PStr [{pstr_desc = Pstr_eval({pexp_desc=Pexp_record (fields, None); _}, _); _}]
        );
        _
      }) ->

    let field_to_arg field =
      match field with
      | ({txt=Lident name; _}, value) -> (Labelled name, expr mapper value)
      | _ -> assert false (* invalid field name *) in
    let args = List.append (List.map field_to_arg fields) [(Nolabel, unit ~loc ())] in
    Exp.apply ~loc (mk_func ~loc longident) args

  (* X { } *)
  | Pexp_construct(
      {txt=longident; loc},
      Some {pexp_desc=Pexp_object {pcstr_self={ppat_desc = Ppat_any; _}; pcstr_fields=[]}; _}
    ) ->
    let args = [(Nolabel, unit ~loc ())] in
    Exp.apply ~loc (mk_func ~loc longident) args

  (* Object without constructor: {"x": v} *)
  | Pexp_extension (
      {txt="bs.obj"; _},
      PStr [{pstr_desc = Pstr_eval({pexp_desc=Pexp_record (_fields, None); _}, _); _}]
    ) -> fail e.pexp_loc "Object literal without annotation"

  | _ -> default_mapper.expr mapper e


let () =
  let rewriter _config _cookies = { default_mapper with expr } in
  Migrate_parsetree.(Driver.register ~name:"rekube.ppx" Versions.ocaml_406 rewriter)

