(********************************************************************************)
(*	Litiom_type.mli
	Copyright (c) 2011-2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)

(**	Blah blah blah.
*)

open Eliom_content


(********************************************************************************)
(**	{1 Module types}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Bases for functors}							*)
(********************************************************************************)

module type SIMPLE_BASE =
sig
	type t

	val of_string: string -> t
	val to_string: t -> string
end


module type CHOICE_BASE =
sig
	include SIMPLE_BASE

	val describe: t -> string
	val elems: t * t list
end


(********************************************************************************)
(**	{2 Partial results of functors}						*)
(********************************************************************************)

module type SIMPLE_SEMI =
sig
	type t

	val param:
		string ->
		(t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameter.param_name) Eliom_parameter.params_type

	val input:
		?a:Html5_types.input_attrib Html5.F.attrib list ->
		input_type:[< `Hidden | `Password | `Submit | `Text ] ->
		?name:[< t Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
		?value:t ->
		unit ->
		[> Html5_types.input ] Html5.F.elt

	val image_input:
		?a:Html5_types.input_attrib Html5.F.attrib list ->
		name:[< (t * Eliom_parameter.coordinates) Eliom_parameter.oneradio ] Eliom_parameter.param_name ->
		value:t ->
		?src:Html5.F.uri ->
		unit ->
		[> Html5_types.input ] Html5.F.elt

	val checkbox:
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
		name:[< t Eliom_parameter.setone ] Eliom_parameter.param_name ->
		value:t ->
		Html5_types.button_content Html5.F.elt list ->
		[> Html5_types.button ] Html5.F.elt

	val select:
		?a:Html5_types.select_attrib Html5.F.attrib list ->
		name:[< `One of t ] Eliom_parameter.param_name ->
		t Html5.F.select_opt ->
		t Html5.F.select_opt list ->
		[> Html5_types.select ] Html5.F.elt
end


module type CHOICE_SEMI =
sig
	type t

	val choose:
		?a:Html5_types.select_attrib Html5.F.attrib list ->
		name:[< `One of t ] Eliom_parameter.param_name ->
		?value:t ->
		?allowed:(t * t list) ->
		unit ->
		[> Html5_types.select ] Html5.F.elt
end


module type TEXTUAL_SEMI =
sig
	type t = string

	val textarea:
		?a:Html5_types.textarea_attrib Html5.F.attrib list ->
		name:[< string Eliom_parameter.setoneradio ] Eliom_parameter.param_name ->
		?value:string ->
		unit ->
		[> Html5_types.textarea ] Html5.F.elt
end


(********************************************************************************)
(**	{2 Full results of functors}						*)
(********************************************************************************)

module type SIMPLE_S =
sig
	type t

	include SIMPLE_BASE with type t := t
	include SIMPLE_SEMI with type t := t
end


module type CHOICE_S =
sig
	type t

	include CHOICE_BASE with type t := t
	include SIMPLE_SEMI with type t := t
	include CHOICE_SEMI with type t := t
end


module type TEXTUAL_S =
sig
	type t = string

	include SIMPLE_BASE with type t := t
	include SIMPLE_SEMI with type t := t
	include TEXTUAL_SEMI with type t := t
end


(********************************************************************************)
(**	{1 Functors}								*)
(********************************************************************************)

module Make_simple : functor (Base: SIMPLE_BASE) -> SIMPLE_S with type t = Base.t
module Make_choice : functor (Base: CHOICE_BASE) -> CHOICE_S with type t = Base.t
module Make_textual : functor (Base: SIMPLE_BASE with type t = string) -> TEXTUAL_S


(********************************************************************************)
(**	{1 Predefined modules based on primitive types}				*)
(********************************************************************************)

module Int : SIMPLE_S with type t = int
module Int32 : SIMPLE_S with type t = int32
module Int64 : SIMPLE_S with type t = int64
module Float : SIMPLE_S with type t = float
module String : TEXTUAL_S

