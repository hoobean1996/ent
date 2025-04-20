[@@@ocaml.warning "-69"]

module type Ent = sig
  type t
  type field

  val table_name : string
  val string_of_field : field -> string
  val all_fields : field list
end

module User = struct
  type t = { id : int; name : string; email : string }
  type field = Id | Name | Email

  let table_name = "users"

  let string_of_field = function
    | Id -> "id"
    | Name -> "name"
    | Email -> "email"

  let all_fields = [ Id; Name; Email ]
end

module Query = struct
  type 'f expr = Eq of 'f * string | Or of 'f expr * 'f expr

  type ('a, 'f) t = {
    ent : (module Ent with type t = 'a and type field = 'f);
    fields : 'f list option;
    where_clause : 'f expr option;
  }

  let from (type a f) (module E : Ent with type t = a and type field = f) :
      (a, f) t =
    { ent = (module E); fields = None; where_clause = None }

  (* type a定义了一个local abstract type *)
  (* 重新设计的函数签名 *)
  let select : type a f. f list -> (a, f) t -> (a, f) t =
   fun fields query -> { query with fields = Some fields }

  let where : type a f. f expr -> (a, f) t -> (a, f) t =
   fun expr query -> { query with where_clause = Some expr }

  let ( =. ) field value = Eq (field, value)
  let ( ||. ) e1 e2 = Or (e1, e2)

  let to_sql : type a f. (a, f) t -> string =
   fun query ->
    let (module E) = query.ent in
    let fields_str =
      match query.fields with
      | None -> "*"
      | Some fs ->
          String.concat ", " (List.map (fun f -> E.string_of_field f) fs)
    in
    let rec string_of_expr = function
      | Eq (field, value) ->
          Printf.sprintf "%s = %s" (E.string_of_field field) value
      | Or (e1, e2) ->
          Printf.sprintf "(%s OR %s)" (string_of_expr e1) (string_of_expr e2)
    in
    let where_str =
      match query.where_clause with
      | None -> ""
      | Some expr -> " WHERE " ^ string_of_expr expr
    in
    Printf.sprintf "SELECT %s FROM %s%s" fields_str E.table_name where_str
end

open Query

let () =
  let open User in
  from (module User)
  |> where (Name =. "tt" ||. (Email =. "t@gmail.com"))
  |> select [ Id; Name; Email ]
  |> to_sql |> Printf.printf "%s\n"
