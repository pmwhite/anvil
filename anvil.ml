module String_map = Map.Make (String)

let tuple_equal x y (x1, y1) (x2, y2) = x x1 x2 && y y1 y2

module Buf : sig
  type t

  val string : t -> string -> unit
  val newline : t -> unit
  val indent : t -> unit
  val dedent : t -> unit
  val create : int -> t
  val contents : t -> string
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

module Type_expr = struct
  type t =
    | Ext of string * t list
    | Ref of string * t list
end

module Type1 = struct
  type t =
    | Alias of Type_expr.t
    | Record of (string * Type_expr.t) list
    | Variant of (string * Type_expr.t list) list
  [@@warning "-37"]
end

module Version_scheme = struct
  type t =
    | Fixed
    | Versioned
end

module Type = struct
  type t =
    { version_scheme : Version_scheme.t
    ; type_ : Type1.t
    }
end

module State = struct
  type t = Type.t String_map.t
end

module Change = struct
  type t = State.t -> State.t
end

module O = struct
  let ( !! ) name = Type_expr.Ext (name, [])
  let ( ! ) name = Type_expr.Ref (name, [])
  let ( $ ) name args = Type_expr.Ref (name, args)
  let ( $$ ) name args = Type_expr.Ext (name, args)

  let record ?(version_scheme = Version_scheme.Fixed) name fields state =
    String_map.add name { Type.version_scheme; type_ = Record fields } state
  ;;

  let variant ?(version_scheme = Version_scheme.Fixed) name cases state =
    String_map.add name { Type.version_scheme; type_ = Variant cases } state
  ;;
end

module Otype_expr = struct
  type t = T of (string * t list)

  let rec equal (T a) (T b) = (tuple_equal String.equal (List.equal equal)) a b

  let rec create ~get_version (t : Type_expr.t) : t =
    match t with
    | Ext (name, args) -> T (name, List.map (create ~get_version) args)
    | Ref (name, args) ->
      T
        ( Printf.sprintf "%s.V%d.t" name (get_version name)
        , List.map (create ~get_version) args )
  ;;

  let rec pp buf (T (name, args)) =
    (match args with
     | [] -> ()
     | [ t ] ->
       pp buf t;
       Buf.string buf " "
     | _ :: _ :: _ ->
       Buf.string buf "(";
       iter_sep_by args ~sep:(fun () -> Buf.string buf ",") ~f:(pp buf);
       Buf.string buf ") ");
    Buf.string buf name
  ;;
end

module Otype1 = struct
  type t =
    | Alias of Otype_expr.t
    | Record of (string * Otype_expr.t) list
    | Variant of (string * Otype_expr.t list) list

  let equal a b =
    match[@warning "-4"] a, b with
    | Alias a, Alias b -> Otype_expr.equal a b
    | Record a, Record b -> List.equal (tuple_equal String.equal Otype_expr.equal) a b
    | Variant a, Variant b ->
      List.equal (tuple_equal String.equal (List.equal Otype_expr.equal)) a b
    | _, _ -> false
  ;;

  let create ~get_version (t : Type1.t) : t =
    match t with
    | Alias t -> Alias (Otype_expr.create ~get_version t)
    | Record ts ->
      Record (List.map (fun (name, t) -> name, Otype_expr.create ~get_version t) ts)
    | Variant ts ->
      Variant
        (List.map
           (fun (name, ts) -> name, List.map (Otype_expr.create ~get_version) ts)
           ts)
  ;;

  let pp buf = function
    | Alias t ->
      Buf.string buf "type t = ";
      Otype_expr.pp buf t
    | Record fields ->
      Buf.string buf "type t =";
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
      Buf.string buf "type t =";
      Buf.newline buf;
      Buf.indent buf;
      List.iter
        (fun (name, ts) ->
          (match ts with
           | [] -> Buf.string buf (Printf.sprintf "| %s" name)
           | _ :: _ ->
             Buf.string buf (Printf.sprintf "| %s of " name);
             iter_sep_by ts ~sep:(fun () -> Buf.string buf " * ") ~f:(Otype_expr.pp buf));
          Buf.newline buf)
        cases;
      Buf.dedent buf
  ;;
end

module Otype = struct
  type t =
    | Fixed of Otype1.t
    | Versioned of
        { hd : Otype1.t
        ; tl : Otype1.t list
        }

  let pp buf t =
    match t with
    | Fixed t -> Otype1.pp buf t
    | Versioned { hd; tl } ->
      let indexed_cases =
        List.rev (List.mapi (fun i t -> i + 1, t) (List.rev (hd :: tl)))
      in
      iter_sep_by
        indexed_cases
        ~sep:(fun () -> Buf.newline buf)
        ~f:(fun (i, t) ->
          Buf.string buf (Printf.sprintf "module V%d = struct" i);
          Buf.newline buf;
          Buf.indent buf;
          Otype1.pp buf t;
          Buf.dedent buf;
          Buf.string buf "end";
          Buf.newline buf);
      Buf.newline buf;
      Buf.string buf "type t =";
      Buf.newline buf;
      Buf.indent buf;
      List.iter
        (fun (i, _) ->
          Buf.string buf (Printf.sprintf "| V%d of V%d.t" i i);
          Buf.newline buf)
        (List.rev indexed_cases);
      Buf.dedent buf
  ;;
end

module Versions : sig
  type t

  val init : get_version:(string -> int) -> Type.t -> t
  val add : get_version:(string -> int) -> Type.t -> t -> t
  val current_version : t -> int
  val pp : Buf.t -> name:string -> t -> unit
end = struct
  type t =
    { current_version : int
    ; hd : Otype.t
    ; tl : Otype.t list
    }

  let init ~get_version (t : Type.t) =
    let hd : Otype.t =
      match t.version_scheme with
      | Fixed -> Fixed (Otype1.create ~get_version t.type_)
      | Versioned -> Versioned { hd = Otype1.create ~get_version t.type_; tl = [] }
    in
    { current_version = 1; hd; tl = [] }
  ;;

  let add ~get_version (a : Type.t) t =
    let version_scheme = a.version_scheme in
    let a = Otype1.create ~get_version a.type_ in
    match version_scheme, t.hd with
    | Fixed, Fixed b ->
      if Otype1.equal a b
      then t
      else { current_version = t.current_version + 1; hd = Fixed a; tl = t.hd :: t.tl }
    | Fixed, Versioned _ ->
      { current_version = t.current_version + 1; hd = Fixed a; tl = t.hd :: t.tl }
    | Versioned, Fixed _ ->
      { current_version = t.current_version + 1
      ; hd = Versioned { hd = a; tl = [] }
      ; tl = t.hd :: t.tl
      }
    | Versioned, Versioned b ->
      if Otype1.equal a b.hd
      then t
      else { t with hd = Versioned { hd = a; tl = b.hd :: b.tl } }
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
    | changes :: history ->
      let next = List.fold_left (fun acc change -> change acc) prev changes in
      let maybe_update_version acc name =
        match String_map.find_opt name next with
        | None -> acc
        | Some next_type ->
          String_map.update
            name
            (function
              | None ->
                Some
                  (Versions.init
                     ~get_version:(fun n ->
                       if String.equal n name
                       then 1
                       else Versions.current_version (find_by_type_name n acc))
                     next_type)
              | Some versions ->
                Some
                  (Versions.add
                     ~get_version:(fun n ->
                       if String.equal n name
                       then Versions.current_version versions
                       else Versions.current_version (find_by_type_name n acc))
                     next_type
                     versions))
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
