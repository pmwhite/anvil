module Type_expr : sig
  type t
end

module Change : sig
  type t
end

module O : sig
  (** Use a pre-existing type. *)
  val ( !! ) : string -> Type_expr.t

  (** Use a managed type. *)
  val ( ! ) : string -> Type_expr.t

  (** Call a pre-existing type constructor with the given arguments. *)
  val ( $$ ) : string -> Type_expr.t list -> Type_expr.t

  (** Call a managed type constructor with the given arguments. *)
  val ( $ ) : string -> Type_expr.t list -> Type_expr.t

  val record : ?versioned:bool -> string -> (string * Type_expr.t) list -> Change.t
  val variant : ?versioned:bool -> string -> (string * Type_expr.t list) list -> Change.t
end

val generate : history:Change.t list list -> type_order:string list -> string
