module Type_expr : sig
  type t
end

module Type : sig
  type t
end

module State : sig
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

  val record : string -> (string * Type_expr.t) list -> State.t -> State.t
end

val generate : history:(State.t -> State.t) list -> type_order:string list -> string
