(********************************************************************************)
(*  Litiom_type.mli
    Copyright (c) 2011-2016 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(** Facilities for improving modularity and abstraction in Eliom applications.

    Suppose you have a service [coucou1] which accepts an [int32] as parameter.
    The traditional way to declare and register the service, and to create a
    form towards it would be as follows:
    {v
    let coucou1_handler x () =
        Lwt.return
            (html
            (head (title (pcdata "Coucou1")) [])
            (body [p [pcdata ("X = " ^ Int32.to_string x)]]))

    let coucou1_service =
        Eliom_registration.Html5.register_service
            ~path:["coucou1"]
            ~get_params:(Eliom_parameter.int32 "x")
            coucou1_handler

    let coucou1_form enter_x =
        [
        fieldset
            [
            label [pcdata "Enter X:"];
            Form.input ~input_type:`Text ~name:enter_x Form.int32;
            ]
        ]
    v}

    This is, however, a brittle solution: if, for example, you were to change
    [coucou1]'s parameter from an [int32] to an [int64], you would need to make
    changes in various locations in your code, which in a large project would
    likely be spread over multiple files.  Even though Eliom leverages OCaml's
    strong typing to ensure that you would not forget any code still using the old
    [int32], this state of affairs still makes refactoring cumbersome.  Moreover,
    you do not have the ability to abstract away the type accepted by [coucou1].

    This module suggests a different approach to handling types in Eliom
    applications.  The basic principle is that all functions related to a given
    type (namely service parameter declaration and the various form input widgets)
    should reside in a single module named after the type.  The new version of
    our sample code would look as follows:
    {v
    module Coucou = Litiom_type.Int32

    let coucou2_handler x () =
        Lwt.return
            (html
                (head (title (pcdata "Coucou2")) [])
                (body [p [pcdata ("X = " ^ Coucou.to_string x)]]))

    let coucou2_service =
        Eliom_registration.Html5.register_service
            ~path:["coucou2"]
            ~get_params:(Coucou.param "x")
            coucou2_handler

    let coucou2_form enter_x =
        [
        fieldset
            [
            label [pcdata "Enter X:"];
            Coucou.input ~input_type:`Text ~name:enter_x ();
            ]
        ]
    v}

    If you prefer Eliom5's convention of using [Form.input], this module also
    defines [Coucou.typ] as the witness parameter to [Form.input].  The function
    [coucou2_form] defined above has therefore an alternative formulation:
    {v
    let coucou2_form enter_x =
        [
        fieldset
            [
            label [pcdata "Enter X:"];
            Form.input ~input_type:`Text ~name:enter_x Coucou.typ;
            ]
        ]
    v}

    Modifying [coucou2]'s type from an [int32] to an [int64] illustrates the
    advantage of this approach, since only one line of code has to be changed:
    {v
    module Coucou = Litiom_type.Int64
    v}

    Moreover, user-defined types are easily accommodated.  [Litiom_type] provides
    a functorial interface to create new modules compatible with its approach:
    {v
    module Coucou = Litiom_type.Make
    (struct
        type t = [ `A | `B | `C ]

        let of_string = function
            | "A" -> `A
            | "B" -> `B
            | "C" -> `C
            | x   -> invalid_arg ("Coucou.of_string: " ^ x)

        let to_string = function
            | `A -> "A"
            | `B -> "B"
            | `C -> "C"
    end)
    v}
*)

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

module type S =
sig
    include STRINGABLE

    val param:
        string ->
        (t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameter.param_name) Eliom_parameter.params_type

    val typ: t Html5.F.Form.param

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

module Make: functor (Base: STRINGABLE) -> S with type t = Base.t


(********************************************************************************)
(** {1 Predefined modules}                                                      *)
(********************************************************************************)

module Float: S with type t = float
module Int: S with type t = int
module Int32: S with type t = int32
module Int64: S with type t = int64
module Bool: S with type t = bool
module String: TEXTUAL

