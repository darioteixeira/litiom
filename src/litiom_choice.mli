(********************************************************************************)
(*  Litiom_choice.mli
    Copyright (c) 2011-2015 Dario Teixeira <dario.teixeira@nleyten.com>
*)
(********************************************************************************)

(** Facilities for making select boxes more convenient and easier to use.

    Eliom's [select] family of functions provides a powerful and complete
    toolbox for building select boxes in HTML5 forms.  However, with great
    power comes great complexity, and [select] is notoriously cumbersome.
    This module aims to simplify the creation of select boxes for all those
    situations where the full power of Eliom's [select] is unnecessary.
    Moreover, it complements {!Litiom_type}, on which it depends.

    The module's facilities are available via functor {!Make}.  The functor's
    parameter must satisfy the {!CHOOSABLE} signature, which is a superset of
    {!Litiom_type.STRINGABLE} (alongside the components already present in
    {!Litiom_type.STRINGABLE}, {!CHOOSABLE} also requires the definition of a
    function [describe] mapping each element into a human-friendly description,
    and a value [all] with the enumeration of all members of the type).  Finally,
    note that the functor's resulting signature {!S} is a superset of {!Litiom_type.S}.

    As an example, let us create a module [Suit] satisfying {!S}.  Note the
    invocation of {!Make} with a module satisfying the {!CHOOSABLE} interface:
    {v
    module Suit = Litiom_choice.Make
    (struct
        type t = Clubs | Spades | Diamonds | Hearts

        let of_string = function
            | "c" -> Clubs
            | "s" -> Spades
            | "d" -> Diamonds
            | "h" -> Hearts
            | x   -> invalid_arg ("Suit.of_string: " ^ x)

        let to_string = function
            | Clubs    -> "c"
            | Spades   -> "s"
            | Diamonds -> "d"
            | Hearts   -> "h"

        let describe = function
            | Clubs    -> "clubs (♣)"
            | Spades   -> "spades (♠)"
            | Diamonds -> "diamonds (♦)"
            | Hearts   -> "hearts (♥)"

        let all = [Clubs; Spades; Diamonds; Hearts]
    end)
    v}

    Since {!S} is a superset of {!Litiom_type.S}, [Suit] may now be used in all
    circumstances described in {!Litiom_type}.  In addition, [Suit] also offers
    function {!S.choose}, which makes the creation of select boxes as simple as
    is illustrated by the code below. (Note that function [choose] has other
    options, available via optional parameters.)
    {v
    let coucou_handler suit () =
        Lwt.return
            (html
            (head (title (pcdata "Coucou")) [])
            (body [p [pcdata "You have chosen "; pcdata (Suit.describe suit)]]))

    let coucou_service =
        Eliom_registration.Html5.register_service
            ~path:["coucou"]
            ~get_params:(Suit.param "suit")
            coucou_handler

    let coucou_form suit =
        [
        label ~a:[a_for suit] [pcdata "Choose suit:"];
        Suit.choose ~name:suit ();
        button ~button_type:`Submit [pcdata "Submit"];
        ]
    v}
*)

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
(** {2 Results of functors}                                                     *)
(********************************************************************************)

module type S =
sig
    include Litiom_type.S
    include DESCRIBABLE with type t := t
    include ENUMERABLE with type t := t

    (** Convenience function for building select boxes.  If not provided,
        optional parameter [value] defaults to first element of the [all]
        list provided during the functor creation.  Similarly, parameter
        [allowed] is by default equal to [all].  Finally, the optional
        [transform] is a function applied after [describe].  By default,
        [String.uppercase] is used.
    *)
    
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

module Make: functor (Base: CHOOSABLE) -> S with type t = Base.t

