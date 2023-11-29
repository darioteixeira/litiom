open Eliom_content


(********************************************************************************)
(** {1 Module types}                                                            *)
(********************************************************************************)

(********************************************************************************)
(** {2 Bases for functors}                                                      *)
(********************************************************************************)

module type DESCRIBABLE =
sig
    type t

    val describe: t -> string
end


module type ENUMERABLE =
sig
    type t

    val all: t list
end


module type CHOOSABLE =
sig
    include Litiom_type.STRINGABLE
    include DESCRIBABLE with type t := t
    include ENUMERABLE with type t := t
end


(********************************************************************************)
(** {2 Partial results of functors}                                             *)
(********************************************************************************)

module type S =
sig
    include Litiom_type.S
    include DESCRIBABLE with type t := t
    include ENUMERABLE with type t := t

    val choose:
        ?a:Html5_types.select_attrib Html5.F.attrib list ->
        name:[ `One of t ] Eliom_parameter.param_name ->
        ?value:t ->
        ?allowed:t list ->
        ?transform:(string -> string) ->
        unit ->
        [> Html5_types.select ] Html5.F.elt
end


(********************************************************************************)
(** {1 Functors}                                                                *)
(********************************************************************************)

module Make (Base: CHOOSABLE): S with type t = Base.t =
struct
    include Litiom_type.Make (Base)
    include (Base: DESCRIBABLE with type t := t)
    include (Base: ENUMERABLE with type t := t)

    let choose ?a ~name ?value ?(allowed = all) ?(transform = String.capitalize) () =
        let (elem_hd, elem_tl) = match allowed with
            | hd :: tl -> (hd, tl)
            | []       -> invalid_arg "Litiom_choice.choose" in
        let is_selected = match value with
            | Some v -> fun item -> item = v
            | None   -> fun item -> false in
        let option_of_item item =
            Html5.F.Form.Option ([], item, Some (Html5.F.pcdata (transform (describe item))), is_selected item) in
        select ?a ~name (option_of_item elem_hd) (List.map option_of_item elem_tl)
end

