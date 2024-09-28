module String_map = Map.Make (String)
module String_set = Set.Make (String)

let tuple_equal x y (x1, y1) (x2, y2) = x x1 x2 && y y1 y2

module Buf : sig
  type t

  val string : t -> string -> unit
  val newline : t -> unit
  val indent : t -> unit
  val dedent : t -> unit
  val create : int -> t
  val contents : t -> string
  val clear : t -> unit
end = struct
  type t =
    { mutable indent : int
    ; mutable at_start_of_line : bool
    ; buffer : Buffer.t
    }

  let string t s =
    if t.at_start_of_line
    then
      for _ = 0 to (t.indent * 2) - 1 do
        Buffer.add_char t.buffer ' '
      done;
    t.at_start_of_line <- false;
    Buffer.add_string t.buffer s
  ;;

  let newline t =
    Buffer.add_string t.buffer "\n";
    t.at_start_of_line <- true
  ;;

  let indent t = t.indent <- t.indent + 1
  let dedent t = t.indent <- t.indent - 1

  let create capacity =
    { indent = 0; at_start_of_line = true; buffer = Buffer.create capacity }
  ;;

  let contents t = Buffer.contents t.buffer

  let clear t =
    Buffer.clear t.buffer;
    t.at_start_of_line <- true;
    t.indent <- 0
  ;;
end

let iter_sep_by xs ~f ~sep =
  match xs with
  | [] -> ()
  | x :: xs ->
    f x;
    List.iter
      (fun x ->
        sep ();
        f x)
      xs
;;

module M : sig
  module Type_expr : sig
    type t =
      | Named of string
      | Reference of string
      | Poly of (t * t list)
  end

  module Type : sig
    type t =
      | Alias of Type_expr.t
      | Record of (string * Type_expr.t) list
      | Variant of (string * Type_expr.t) list
  end

  module State : sig
    type t = Type.t String_map.t

    val set_type : string -> Type.t -> t -> t
  end

  val generate : history:(State.t -> State.t) list -> type_order:string list -> string
end = struct
  module Type_expr = struct
    type t =
      | Named of string
      | Reference of string
      | Poly of (t * t list)
  end

  module Type = struct
    type t =
      | Alias of Type_expr.t
      | Record of (string * Type_expr.t) list
      | Variant of (string * Type_expr.t) list
  end

  module Otype_expr = struct
    type t =
      | Named of string
      | Reference of (string * int)
      | Poly of (t * t list)

    let rec equal a b =
      match[@warning "-4"] a, b with
      | Named a, Named b -> String.equal a b
      | Reference a, Reference b -> tuple_equal String.equal Int.equal a b
      | Poly a, Poly b -> (tuple_equal equal (List.equal equal)) a b
      | _, _ -> false
    ;;

    let rec create ~get_version (type_ : Type_expr.t) : t =
      match type_ with
      | Named name -> Named name
      | Reference name -> Reference (name, get_version name)
      | Poly (t, ts) -> Poly (create ~get_version t, List.map (create ~get_version) ts)
    ;;

    let rec pp buf = function
      | Named name -> Buf.string buf name
      | Reference (name, version) ->
        Buf.string buf (Printf.sprintf "%s.V%d.t" name version)
      | Poly (t, ts) ->
        (match ts with
         | [] -> ()
         | [ t ] ->
           pp buf t;
           Buf.string buf " "
         | _ :: _ :: _ ->
           Buf.string buf "(";
           iter_sep_by ts ~sep:(fun () -> Buf.string buf ",") ~f:(pp buf);
           Buf.string buf ") ");
        pp buf t
    ;;
  end

  module Otype = struct
    type t =
      | Alias of Otype_expr.t
      | Record of (string * Otype_expr.t) list
      | Variant of (string * Otype_expr.t) list

    let equal a b =
      match[@warning "-4"] a, b with
      | Alias a, Alias b -> Otype_expr.equal a b
      | Record a, Record b -> List.equal (tuple_equal String.equal Otype_expr.equal) a b
      | Variant a, Variant b -> List.equal (tuple_equal String.equal Otype_expr.equal) a b
      | _, _ -> false
    ;;

    let create ~get_version (t : Type.t) : t =
      match t with
      | Alias t -> Alias (Otype_expr.create ~get_version t)
      | Record ts ->
        Record (List.map (fun (name, t) -> name, Otype_expr.create ~get_version t) ts)
      | Variant ts ->
        Variant (List.map (fun (name, t) -> name, Otype_expr.create ~get_version t) ts)
    ;;

    let pp buf = function
      | Alias t ->
        Buf.string buf "type t = ";
        Otype_expr.pp buf t
      | Record fields ->
        Buf.string buf "type t = ";
        Buf.newline buf;
        Buf.indent buf;
        Buf.string buf "{ ";
        iter_sep_by
          fields
          ~sep:(fun () -> Buf.string buf "; ")
          ~f:(fun (name, t) ->
            Buf.string buf name;
            Buf.string buf " : ";
            Otype_expr.pp buf t;
            Buf.newline buf);
        Buf.string buf "}";
        Buf.newline buf;
        Buf.dedent buf
      | Variant cases ->
        Buf.string buf "type t = ";
        Buf.newline buf;
        Buf.indent buf;
        List.iter
          (fun (name, t) ->
            Buf.string buf (Printf.sprintf "| %s of " name);
            Otype_expr.pp buf t;
            Buf.newline buf)
          cases;
        Buf.dedent buf
    ;;
  end

  module State = struct
    type t = Type.t String_map.t

    let set_type key value t = String_map.add key value t
  end

  module Versions = struct
    type t =
      { current_version : int
      ; hd : Otype.t
      ; tl : Otype.t list
      }

    let init hd = { current_version = 1; hd; tl = [] }

    let add v t =
      if Otype.equal v t.hd
      then t
      else { current_version = t.current_version + 1; hd = v; tl = t.hd :: t.tl }
    ;;

    let current_version t = t.current_version

    let pp buf ~name t =
      Buf.string buf (Printf.sprintf "module %s = struct" name);
      Buf.newline buf;
      Buf.indent buf;
      iter_sep_by
        (List.rev (List.mapi (fun i t -> i + 1, t) (List.rev (t.hd :: t.tl))))
        ~sep:(fun () -> Buf.newline buf)
        ~f:(fun (i, otype) ->
          Buf.string buf (Printf.sprintf "module V%d = struct" i);
          Buf.newline buf;
          Buf.indent buf;
          Otype.pp buf otype;
          Buf.dedent buf;
          Buf.string buf "end";
          Buf.newline buf);
      Buf.dedent buf;
      Buf.string buf "end";
      Buf.newline buf
    ;;
  end

  let find_by_type_name name types =
    match String_map.find_opt name types with
    | Some x -> x
    | None -> failwith (Printf.sprintf "undefined type '%s'" name)
  ;;

  let pp_versions buf versions ~type_order =
    iter_sep_by
      type_order
      ~sep:(fun () -> Buf.newline buf)
      ~f:(fun name -> Versions.pp buf ~name (find_by_type_name name versions))
  ;;

  let generate ~history ~type_order =
    let rec collect_versions (prev : State.t) (acc : Versions.t String_map.t) history =
      match history with
      | [] -> acc
      | update :: history ->
        let next = update prev in
        let maybe_update_version acc name =
          match String_map.find_opt name next with
          | None -> acc
          | Some next_type ->
            String_map.update
              name
              (function
                | None ->
                  let next_otype =
                    Otype.create
                      ~get_version:(fun n ->
                        if String.equal n name
                        then 1
                        else Versions.current_version (find_by_type_name n acc))
                      next_type
                  in
                  Some (Versions.init next_otype)
                | Some versions ->
                  let next_otype =
                    Otype.create
                      ~get_version:(fun n ->
                        if String.equal n name
                        then Versions.current_version versions
                        else Versions.current_version (find_by_type_name n acc))
                      next_type
                  in
                  Some (Versions.add next_otype versions))
              acc
        in
        let acc = List.fold_left maybe_update_version acc type_order in
        collect_versions next acc history
    in
    let versions = collect_versions String_map.empty String_map.empty history in
    let buf = Buf.create 1024 in
    pp_versions buf versions ~type_order;
    Buf.contents buf
  ;;
end

open M

let () =
  let history =
    let int = Type.Named "int" in
    let string = Type.Named "string" in
    let point = Type.Reference "Point" in
    let player = Type.Reference "Player" in
    let list t = Type.Poly (Named "list", [ t ]) in
    [ (fun state ->
        state
        |> State.set_type "Point" (Record [ "x", int; "y", int ])
        |> State.set_type "Player" (Record [ "position", point; "health", int ])
        |> State.set_type "World" (Record [ "players", list player; "health", int ]))
    ; (fun state ->
        state |> State.set_type "Point" (Record [ "x", int; "y", int; "z", int ]))
    ; (fun state ->
        state
        |> State.set_type
             "Player"
             (Record [ "position", point; "health", int; "name", string ]))
    ]
  in
  print_string (generate ~history ~type_order:[ "Point"; "Player"; "World" ])
;;
