(********************************************************************************)
(**	Litiom_wizard module.

	Copyright (c) 2008 Dario Teixeira (dario.teixeira\@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Lwt
open XHTML.M
open Eliom_parameters


(********************************************************************************)
(* Submit module.								*)
(********************************************************************************)

(**	The type of the [Submit] module.
*)
module type SUBMIT =
sig
	type t = Proceed | Cancel
	val of_string : string -> t
	val to_string : t -> string
	val param_label : string
	val param : (t, [`WithoutSuffix], [`One of t] Eliom_parameters.param_name) Eliom_parameters.params_type
	val make_controls : [< t Eliom_parameters.setoneopt] Eliom_parameters.param_name -> [> `Fieldset] XHTML.M.elt
end


(**	The [Submit] module will do this and that.
*)
module Submit : SUBMIT =
struct
	type t = Proceed | Cancel

	let of_string = function
		| "Proceed"	-> Proceed
		| "Cancel"	-> Cancel
		| x		-> raise (Invalid_argument x)

	let to_string = function
		| Proceed	-> "Proceed"
		| Cancel	-> "Cancel"

	let param_label = "submit"

	let param = Eliom_parameters.user_type of_string to_string param_label

	let make_controls enter_submit =
		fieldset ~a:[a_class ["wizard_buttons"]]
			[
			Eliom_predefmod.Xhtml.user_type_input
				~input_type:`Submit
				~name:enter_submit
				~value:Cancel
				to_string;

			Eliom_predefmod.Xhtml.user_type_input
				~input_type:`Submit
				~name:enter_submit
				~value:Proceed
				to_string
			]

end


(********************************************************************************)
(* Canvas module.								*)
(********************************************************************************)

(**	The [Canvas] module will do this and that.
*)
module Canvas =
struct
	let inter ~form_contents ~next_step ~carry sp x =
		let create_form (enter_next_params, enter_submit) =
			(form_contents ~carry enter_next_params) @ [Submit.make_controls enter_submit]
		in Lwt.return [div [Eliom_predefmod.Xhtml.post_form next_step sp create_form ()]]

	let final ~form_contents ~carry sp x =
		Lwt.return [div (form_contents ~carry)]
end


(********************************************************************************)
(* Handler module.								*)
(********************************************************************************)

(**	The [Handler] module will do this and that.
*)
module Handler =
struct
	let initial ~carrier ~next_step_register ~tree_builder ~canvas =
		fun sp () () ->
			let carry = carrier () () in
			let next_step = next_step_register ~carry sp in
			let canvas_tree = Litiom_blocks.sink (canvas ~next_step ~carry)
			in tree_builder sp canvas_tree

	let inter ~carrier ~next_step_register ~cancel_canvas ~tree_builder ~canvas ~carry =
		fun sp () (this_params, submit_param) ->
			let canvas_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry this_params in
					let next_step = next_step_register ~carry sp
					in Litiom_blocks.sink (canvas ~next_step ~carry)
				| Submit.Cancel ->
					cancel_canvas
			in tree_builder sp canvas_tree

	let final ~carrier ~cancel_canvas ~tree_builder ~canvas ~carry =
		fun sp () (this_params, submit_param) ->
			let canvas_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry this_params
					in Litiom_blocks.sink (canvas ~carry)
				| Submit.Cancel ->
					cancel_canvas
			in tree_builder sp canvas_tree

	end


(********************************************************************************)
(* Error_handler module.							*)
(********************************************************************************)

(**	The [Error_handler] module will do this and that.
*)
module Error_handler =
struct
	let inter ~cancel_canvas ~error_canvas ~tree_builder =
		fun sp exc_list ->
			Eliom_sessions.get_post_params ~sp >>= fun params ->
			let maybe_submit =
				try
					let (_, submit_raw) = List.find (fun (k, v) -> k = Submit.param_label) params
					in Some (Submit.of_string submit_raw)
				with
					| Not_found
					| Invalid_argument _ -> None in
			let canvas_tree = match maybe_submit with
				| None
				| Some Submit.Proceed	-> error_canvas exc_list
				| Some Submit.Cancel	-> cancel_canvas
			in tree_builder sp canvas_tree
end


(********************************************************************************)
(* Register module.								*)
(********************************************************************************)

(**	The [Register] module will do this and that.
*)
module Register =
struct
	let initial ~fallback ~handler =
		Eliom_predefmod.Xhtml.register fallback handler

	let inter ~fallback ~handler ~error_handler ~params ~carry sp =
		Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
			~sp
			~fallback
			~post_params: (params ** Submit.param)
			~error_handler
			(handler ~carry)

end


(********************************************************************************)
(* Steps module.								*)
(********************************************************************************)

(**	The [Steps] module does this and that.
*)
module Steps =
struct

	(**	Adds the first step.
	*)
	let make_first
		~fallback
		~tree_builder
		~carrier
		~form_contents
		~next_step_register =

		let canvas = Canvas.inter
				~form_contents in
		let handler = Handler.initial
				~carrier
				~next_step_register
				~tree_builder
				~canvas in
		let register = Register.initial
				~fallback
				~handler
		in register


	(**	Adds a middle step.
	*)
	let make_middle
		~fallback
		~tree_builder
		~carrier
		~form_contents
		~next_step_register
		~cancel_canvas
		~error_canvas
		~params =

		let canvas = Canvas.inter
				~form_contents in
		let handler = Handler.inter
				~carrier
				~next_step_register
				~cancel_canvas
				~tree_builder
				~canvas in
		let error_handler = Error_handler.inter
				~cancel_canvas
				~error_canvas
				~tree_builder in
		let register = Register.inter
				~fallback
				~handler
				~error_handler
				~params
		in register


	(**	Adds the last step.
	*)
	let make_last
		~fallback
		~tree_builder
		~carrier
		~form_contents
		~cancel_canvas
		~error_canvas
		~params =

		let canvas = Canvas.final
				~form_contents in
		let handler = Handler.final
				~carrier
				~cancel_canvas
				~tree_builder
				~canvas in
		let error_handler = Error_handler.inter
				~cancel_canvas
				~error_canvas
				~tree_builder in
		let register = Register.inter
				~fallback
				~handler
				~error_handler
				~params
		in register
end


(********************************************************************************)
(* Carrier module.								*)
(********************************************************************************)

(**	The [Carrier] module does this and that.
*)
module Carrier =
struct
	let pairify a b = (a, b)

	let discard a b = ()
end


