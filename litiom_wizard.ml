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


module type SUBMIT =
sig
	type t = Proceed | Cancel
	val of_string : string -> t
	val to_string : t -> string
	val param : (t, [`WithoutSuffix], [`One of t] Eliom_parameters.param_name) Eliom_parameters.params_type
	val make_controls : [< t Eliom_parameters.setoneopt] Eliom_parameters.param_name -> [> `Fieldset] XHTML.M.elt
end


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

	let param = Eliom_parameters.user_type of_string to_string "submit"

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
(* Carrier module.								*)
(********************************************************************************)

module Carrier =
struct
	let pairify a b = (a, b)

	let discard a b = ()
end


(********************************************************************************)
(* Raw steps module.								*)
(********************************************************************************)

module Step =
struct
	(**	Adds the first step.
	*)
	let make_first ~fallback ~tree_builder ~carrier ~form_contents ~next_step_register =

		let canvas ~next_step ~carry sp x =
			let create_form (enter_next_params, enter_submit) =
				(form_contents ~carry enter_next_params) @ [Submit.make_controls enter_submit]
			in Lwt.return [div [Eliom_predefmod.Xhtml.post_form next_step sp create_form ()]] in

		let handler = fun sp () () ->
			let carry = carrier () () in
			let next_step = next_step_register ~carry sp in
			let canvas_tree = Litiom_blocks.sink (canvas ~next_step ~carry)
			in tree_builder sp canvas_tree in

		let register =
			Eliom_predefmod.Xhtml.register fallback handler

		in register


	(**	Adds a middle step.
	*)
	let make_middle ~fallback ~tree_builder ~cancel_canvas ~params ~carrier ~form_contents ~next_step_register =

		let canvas ~next_step ~carry sp x =
			let create_form (enter_next_params, enter_submit) =
				(form_contents ~carry enter_next_params) @ [Submit.make_controls enter_submit]
			in Lwt.return [div [Eliom_predefmod.Xhtml.post_form next_step sp create_form ()]] in

		let handler ~carry = fun sp () (this_params, submit_param) ->
			let canvas_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry this_params in
					let next_step = next_step_register ~carry sp
					in Litiom_blocks.sink (canvas ~next_step ~carry)
				| Submit.Cancel ->
					cancel_canvas
			in tree_builder sp canvas_tree in

		let register ~carry sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (params ** Submit.param)
				(handler ~carry)

		in register


	(**	Adds the last step.
	*)
	let make_last ~fallback ~tree_builder ~cancel_canvas ~params ~carrier ~form_contents =

		let canvas ~carry sp x =
			Lwt.return [div (form_contents ~carry)] in

		let handler ~carry = fun sp () (this_params, submit_param) ->
			let canvas_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry this_params
					in Litiom_blocks.sink (canvas ~carry)
				| Submit.Cancel ->
					cancel_canvas
			in tree_builder sp canvas_tree in

		let register ~carry sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (params ** Submit.param)
				(handler ~carry)

		in register

end

