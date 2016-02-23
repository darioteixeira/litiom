(********************************************************************************)
(*  Litiom_type.ml
    Copyright (c) 2011-2015 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

open Eliom_content


(********************************************************************************)
(** {1 Module types}                                                            *)
(********************************************************************************)

module type STRINGABLE =
sig
    type t

    val of_string: string -> t
    val to_string: t -> string
end


module type PARAMABLE =
sig
    include STRINGABLE

    val param:
        string ->
        (t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameter.param_name) Eliom_parameter.params_type

    val typ: t Html5.F.Form.param
end


module type S =
sig
    include PARAMABLE

    val input:
        ?a:Html5_types.input_attrib Html5.F.attrib list ->
        input_type:[< Html5_types.input_type ] ->
        ?name:[< t Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
        ?value:t ->
        unit ->
        [> Html5_types.input ] Html5.F.elt

    val checkbox :
        ?a:Html5_types.input_attrib Html5.F.attrib list ->
        ?checked:bool ->
        name:[ `Set of t ] Eliom_parameter.param_name ->
        value:t ->
        unit ->
        [> Html5_types.input ] Html5.F.elt

    val radio:
        ?a:Html5_types.input_attrib Html5.F.attrib list ->
        ?checked:bool ->
        name:[ `Radio of t ] Eliom_parameter.param_name ->
        value:t ->
        unit ->
        [> Html5_types.input ] Html5.F.elt

    val button:
        ?a:Html5_types.button_attrib Html5.F.attrib list ->
        button_type:[< Eliom_form_sigs.button_type ] ->
        name:[< t Eliom_parameter.setone ] Eliom_parameter.param_name ->
        value:t ->
        unit ->
        Html5_types.button_content Html5.F.elt list ->
        [> Html5_types.button ] Html5.F.elt

    val select:
        ?a:Html5_types.select_attrib Html5.F.attrib list ->
        ?required:Html5_types.pcdata Html5.F.elt ->
        name:[ `One of t ] Eliom_parameter.param_name ->
        t Html5.F.Form.select_opt ->
        t Html5.F.Form.select_opt list ->
        [> Html5_types.select ] Html5.F.elt

    val multiple_select:
        ?a:Html5_types.select_attrib Html5.F.attrib list ->
        ?required:Html5_types.pcdata Html5.F.elt ->
        name:[ `Set of t ] Eliom_parameter.param_name ->
        t Html5.F.Form.select_opt ->
        t Html5.F.Form.select_opt list ->
        [> Html5_types.select ] Html5.F.elt
end


module type TEXTUAL =
sig
    include S with type t = string

    val textarea:
        ?a:Html5_types.textarea_attrib Html5.F.attrib list ->
        name:[< string Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
        ?value:string ->
        unit ->
        [> Html5_types.textarea ] Html5.F.elt
end


(********************************************************************************)
(** {1 Functors}                                                                *)
(********************************************************************************)

module Make_s (Base: PARAMABLE): S with type t = Base.t =
struct
    open Html5.F

    include Base

    let input ?a ~input_type ?name ?value () = Form.input ?a ~input_type ?name ?value typ
    let checkbox ?a ?checked ~name ~value () = Form.checkbox ?a ?checked ~name ~value typ
    let radio ?a ?checked ~name ~value () = Form.radio ?a ?checked ~name ~value typ
    let button ?a ~button_type ~name ~value () = Form.button ?a ~button_type ~name ~value typ
    let select ?a ?required ~name fl ol = Form.select ?a ?required ~name typ fl ol 
    let multiple_select ?a ?required ~name fl ol = Form.multiple_select ?a ?required ~name typ fl ol 
end


module Make (Base: STRINGABLE): S with type t = Base.t = Make_s
(struct
    include Base

    let param = Eliom_parameter.user_type ~of_string ~to_string
    let typ = Html5.F.Form.user to_string
end)


(********************************************************************************)
(** {1 Predefined modules based on primitive types}                             *)
(********************************************************************************)

module Float = Make_s
(struct
    type t = float

    let of_string = float_of_string
    let to_string = string_of_float

    let param = Eliom_parameter.float
    let typ = Html5.F.Form.float
end)

module Int = Make_s
(struct
    type t = int

    let of_string = int_of_string
    let to_string = string_of_int

    let param = Eliom_parameter.int
    let typ = Html5.F.Form.int
end)

module Int32 = Make_s
(struct
    type t = int32

    let of_string = Int32.of_string
    let to_string = Int32.to_string

    let param = Eliom_parameter.int32
    let typ = Html5.F.Form.int32
end)

module Int64 = Make_s
(struct
    type t = int64

    let of_string = Int64.of_string
    let to_string = Int64.to_string

    let param = Eliom_parameter.int64
    let typ = Html5.F.Form.int64
end)

module Bool = Make_s
(struct
    type t = bool

    let of_string = bool_of_string
    let to_string = string_of_bool

    let param = Eliom_parameter.bool
    let typ = Html5.F.Form.bool
end)

module String =
struct
    include (Make_s
    (struct
        type t = string

        let of_string x = x
        let to_string x = x

        let param = Eliom_parameter.string
        let typ = Html5.F.Form.string
    end))

    let textarea = Html5.F.Form.textarea
end

