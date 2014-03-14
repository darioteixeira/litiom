(********************************************************************************)
(*	Litiom_type.ml
	Copyright (c) 2011-2012 Dario Teixeira (dario.teixeira@yahoo.com)
*)
(********************************************************************************)

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
	val all: t list
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
		input_type:
			[< `Button | `Checkbox | `Color | `Date | `Datetime | `Datetime_local | `Email
			| `File | `Hidden | `Image | `Month | `Number | `Password | `Radio | `Range
			| `Reset | `Search | `Submit | `Tel | `Text | `Time | `Url | `Week ] ->
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
		?required:Html5_types.pcdata Html5.F.elt ->
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
		?allowed:t list ->
		?transform:(string -> string) ->
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

module Make_simple (Base: SIMPLE_BASE): SIMPLE_S with type t = Base.t =
struct
	include Base

	let param = Eliom_parameter.user_type ~of_string ~to_string
	let input ?a = Html5.F.user_type_input to_string ?a
	let image_input ?a = Html5.F.user_type_image_input to_string ?a
	let checkbox = Html5.F.user_type_checkbox to_string
	let radio = Html5.F.user_type_radio to_string
	let button ?a = Html5.F.user_type_button to_string ?a
	let select ?a = Html5.F.user_type_select to_string ?a

end


module Make_choice (Base: CHOICE_BASE): CHOICE_S with type t = Base.t =
struct
	include Base
	include (Make_simple (Base): SIMPLE_SEMI with type t := t)

	let choose ?a ~name ?value ?(allowed = all) ?(transform = String.capitalize) () =
		let (elem_hd, elem_tl) = match allowed with
			| hd :: tl -> (hd, tl)
			| []	   -> invalid_arg "Litiom_type.choose" in
		let is_selected = match value with
			| Some v -> fun item -> item = v
			| None   -> fun item -> false in
		let option_of_item item =
			Html5.F.Option ([], item, Some (Html5.F.pcdata (transform (describe item))), is_selected item) in
		select ?a ~name (option_of_item elem_hd) (List.map option_of_item elem_tl)
end


module Make_textual (Base: SIMPLE_BASE with type t = string): TEXTUAL_S =
struct
	include Base
	include (Make_simple (Base): SIMPLE_SEMI with type t := t)

	let textarea = Html5.F.textarea
end


(********************************************************************************)
(**	{1 Predefined modules based on primitive types}				*)
(********************************************************************************)

module Int: SIMPLE_S with type t = int =
struct
	type t = int

	let of_string = int_of_string
	let to_string = string_of_int

	let param = Eliom_parameter.int
	let input = Html5.F.int_input
	let image_input = Html5.F.int_image_input
	let checkbox = Html5.F.int_checkbox
	let radio = Html5.F.int_radio
	let button = Html5.F.int_button
	let select = Html5.F.int_select
end


module Int32: SIMPLE_S with type t = int32 =
struct
	type t = int32

	let of_string = Int32.of_string
	let to_string = Int32.to_string

	let param = Eliom_parameter.int32
	let input = Html5.F.int32_input
	let image_input = Html5.F.int32_image_input
	let checkbox = Html5.F.int32_checkbox
	let radio = Html5.F.int32_radio
	let button = Html5.F.int32_button
	let select = Html5.F.int32_select
end


module Int64: SIMPLE_S with type t = int64 =
struct
	type t = int64

	let of_string = Int64.of_string
	let to_string = Int64.to_string

	let param = Eliom_parameter.int64
	let input = Html5.F.int64_input
	let image_input = Html5.F.int64_image_input
	let checkbox = Html5.F.int64_checkbox
	let radio = Html5.F.int64_radio
	let button = Html5.F.int64_button
	let select = Html5.F.int64_select
end


module Float: SIMPLE_S with type t = float =
struct
	type t = float

	let of_string = float_of_string
	let to_string = string_of_float

	let param = Eliom_parameter.float
	let input = Html5.F.float_input
	let image_input = Html5.F.float_image_input
	let checkbox = Html5.F.float_checkbox
	let radio = Html5.F.float_radio
	let button = Html5.F.float_button
	let select = Html5.F.float_select
end


module String: TEXTUAL_S =
struct
	type t = string

	external of_string: string -> t = "%identity"
	external to_string: t -> string = "%identity"

	let param = Eliom_parameter.string
	let input = Html5.F.string_input
	let image_input = Html5.F.string_image_input
	let checkbox = Html5.F.string_checkbox
	let radio = Html5.F.string_radio
	let button = Html5.F.string_button
	let select = Html5.F.string_select
	let textarea = Html5.F.textarea
end

