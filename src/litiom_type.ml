(********************************************************************************)
(*	Litiom_type.ml
	Copyright (c) 2011 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)

open Eliom_pervasives


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
		(t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameters.param_name) Eliom_parameters.params_type

	val input:
		?a:XHTML_types.input_attrib XHTML.M.attrib list ->
		input_type:[< `Hidden | `Password | `Submit | `Text ] ->
		?name:[< t Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
		?value:t ->
		unit ->
		[> XHTML_types.input ] XHTML.M.elt

	val image_input:
		?a:XHTML_types.input_attrib XHTML.M.attrib list ->
		name:[< (t * Eliom_parameters.coordinates) Eliom_parameters.oneradio ] Eliom_parameters.param_name ->
		value:t ->
		?src:XHTML.M.uri ->
		unit ->
		[> XHTML_types.input ] XHTML.M.elt

	val checkbox:
		?a:XHTML_types.input_attrib XHTML.M.attrib list ->
		?checked:bool ->
		name:[ `Set of t ] Eliom_parameters.param_name ->
		value:t ->
		unit ->
		[> XHTML_types.input ] XHTML.M.elt

	val radio:
		?a:XHTML_types.input_attrib XHTML.M.attrib list ->
		?checked:bool ->
		name:[ `Radio of t ] Eliom_parameters.param_name ->
		value:t ->
		unit ->
		[> XHTML_types.input ] XHTML.M.elt

	val button:
		?a:XHTML_types.button_attrib XHTML.M.attrib list ->
		name:[< t Eliom_parameters.setone ] Eliom_parameters.param_name ->
		value:t ->
		XHTML_types.button_content XHTML.M.elt list ->
		[> XHTML_types.button ] XHTML.M.elt

	val select:
		?a:XHTML_types.select_attrib XHTML.M.attrib list ->
		name:[< `One of t ] Eliom_parameters.param_name ->
		t Eliom_output.Xhtml.select_opt ->
		t Eliom_output.Xhtml.select_opt list ->
		[> XHTML_types.select ] XHTML.M.elt
end


module type CHOICE_SEMI =
sig
	type t

	val choose:
		?a:XHTML_types.select_attrib XHTML.M.attrib list ->
		name:[< `One of t ] Eliom_parameters.param_name ->
		?value:t ->
		?allowed:(t * t list) ->
		unit ->
		[> XHTML_types.select ] XHTML.M.elt
end


module type TEXTUAL_SEMI =
sig
	type t = string

	val textarea:
		?a:XHTML_types.textarea_attrib XHTML.M.attrib list ->
		name:[< string Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
		?value:string ->
		rows:int ->
		cols:int ->
		unit ->
		[> XHTML_types.textarea ] XHTML.M.elt
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

module Make_simple (Base: SIMPLE_BASE) : SIMPLE_S with type t = Base.t =
struct
	include Base

	let param = Eliom_parameters.user_type ~of_string ~to_string
	let input ?a = Eliom_output.Xhtml.user_type_input to_string ?a
	let image_input ?a = Eliom_output.Xhtml.user_type_image_input to_string ?a
	let checkbox = Eliom_output.Xhtml.user_type_checkbox to_string
	let radio = Eliom_output.Xhtml.user_type_radio to_string
	let button ?a = Eliom_output.Xhtml.user_type_button to_string ?a
	let select ?a = Eliom_output.Xhtml.user_type_select to_string ?a

end


module Make_choice (Base: CHOICE_BASE) : CHOICE_S with type t = Base.t =
struct
	include Base
	include (Make_simple (Base) : SIMPLE_SEMI with type t := t)

	let choose ?a ~name ?value ?(allowed = elems) () =
		let (elem_hd, elem_tl) = allowed in
		let option_of_item item =
			let is_selected = match value with
				| Some v -> item = v
				| None   -> false
			in Eliom_output.Xhtml.Option ([], item, Some (XHTML.M.pcdata (describe item)), is_selected)
		in select ?a ~name (option_of_item elem_hd) (List.map option_of_item elem_tl)
end


module Make_textual (Base: SIMPLE_BASE with type t = string) : TEXTUAL_S =
struct
	include Base
	include (Make_simple (Base) : SIMPLE_SEMI with type t := t)

	let textarea = Eliom_output.Xhtml.textarea
end


(********************************************************************************)
(**	{1 Predefined modules matching {!SIMPLE_S}}				*)
(********************************************************************************)

module Int : SIMPLE_S with type t = int =
struct
	type t = int

	let of_string = int_of_string
	let to_string = string_of_int

	let param = Eliom_parameters.int
	let input = Eliom_output.Xhtml.int_input
	let image_input = Eliom_output.Xhtml.int_image_input
	let checkbox = Eliom_output.Xhtml.int_checkbox
	let radio = Eliom_output.Xhtml.int_radio
	let button = Eliom_output.Xhtml.int_button
	let select = Eliom_output.Xhtml.int_select
end


module Int32 : SIMPLE_S with type t = int32 =
struct
	type t = int32

	let of_string = Int32.of_string
	let to_string = Int32.to_string

	let param = Eliom_parameters.int32
	let input = Eliom_output.Xhtml.int32_input
	let image_input = Eliom_output.Xhtml.int32_image_input
	let checkbox = Eliom_output.Xhtml.int32_checkbox
	let radio = Eliom_output.Xhtml.int32_radio
	let button = Eliom_output.Xhtml.int32_button
	let select = Eliom_output.Xhtml.int32_select
end


module Int64 : SIMPLE_S with type t = int64 =
struct
	type t = int64

	let of_string = Int64.of_string
	let to_string = Int64.to_string

	let param = Eliom_parameters.int64
	let input = Eliom_output.Xhtml.int64_input
	let image_input = Eliom_output.Xhtml.int64_image_input
	let checkbox = Eliom_output.Xhtml.int64_checkbox
	let radio = Eliom_output.Xhtml.int64_radio
	let button = Eliom_output.Xhtml.int64_button
	let select = Eliom_output.Xhtml.int64_select
end


module Float : SIMPLE_S with type t = float =
struct
	type t = float

	let of_string = float_of_string
	let to_string = string_of_float

	let param = Eliom_parameters.float
	let input = Eliom_output.Xhtml.float_input
	let image_input = Eliom_output.Xhtml.float_image_input
	let checkbox = Eliom_output.Xhtml.float_checkbox
	let radio = Eliom_output.Xhtml.float_radio
	let button = Eliom_output.Xhtml.float_button
	let select = Eliom_output.Xhtml.float_select
end


module String : TEXTUAL_S =
struct
	type t = string

	external of_string: string -> t = "%identity"
	external to_string: t -> string = "%identity"

	let param = Eliom_parameters.string
	let input = Eliom_output.Xhtml.string_input
	let image_input = Eliom_output.Xhtml.string_image_input
	let checkbox = Eliom_output.Xhtml.string_checkbox
	let radio = Eliom_output.Xhtml.string_radio
	let button = Eliom_output.Xhtml.string_button
	let select = Eliom_output.Xhtml.string_select
	let textarea = Eliom_output.Xhtml.textarea
end

