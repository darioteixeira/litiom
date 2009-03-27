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
	val param_label : string
	val param: (t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameters.param_name) Eliom_parameters.params_type
	val make_controls : [< t Eliom_parameters.setoneradio ] Eliom_parameters.param_name -> [> `Fieldset ] XHTML.M.elt

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
(* Handlers.									*)
(********************************************************************************)

module Handler =
struct
end


(********************************************************************************)
(* Carriers.									*)
(********************************************************************************)

module Carriers =
struct
	let none ~carried sp gp pp = `Success ()
end


(********************************************************************************)
(* Steps module.								*)
(********************************************************************************)

module Steps =
struct
	let make_common ~path ~get_params =
		let fallback = Eliom_services.new_service ~path ~get_params ()
		in fallback


	let make_last ~common:fallback ~form_maker ~content ~post_params =
		let handler carried sp gp (pp, submit_param) =
			content ~carry:carried sp gp pp
		and register handler carried sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (post_params ** Submit.param)
				(handler carried)
		in (register, form_maker, handler)


	let make_middle ~common:fallback ~form_maker ~carrier ~content ~post_params ~next =
		let handler carried sp gp (pp, submit_param) =
			let result = carrier ~carried sp gp pp
			in match result with
				| `Success carry ->
					let (next_register, next_form_maker, next_handler) = next in
					let form_maker (enter_next, enter_submit) =
						(next_form_maker ~carry enter_next) @ [Submit.make_controls enter_submit] in
					let next_service = next_register next_handler carry sp in
					let form = Eliom_predefmod.Xhtml.post_form next_service sp form_maker gp
					in content ~carry ~form sp gp pp
				| `Failure ->
					failwith "oops"
		and register handler carried sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (post_params ** Submit.param)
				(handler carried)
		in (register, form_maker, handler)


	let make_skippable ~common:fallback ~form_maker ~carrier ~content ~post_params ~next =
		let handler carried sp gp (pp, submit_param) =
			let result = carrier ~carried sp gp pp
			in match result with
				| `Skip carry ->
					let (_, _, next_handler) = next
					in next_handler carried sp gp (None, Submit.Proceed)
				| `Success carry ->
					let (next_register, next_form_maker, next_handler) = next in
					let real_next_handler carried sp gp (pp, submit_param) = next_handler carried sp gp (Some pp, submit_param) in
					let form_maker (enter_next, enter_submit) =
						(next_form_maker ~carry enter_next) @ [Submit.make_controls enter_submit] in
					let next_service = next_register real_next_handler carry sp in
					let form = Eliom_predefmod.Xhtml.post_form next_service sp form_maker gp
					in content ~carry ~form sp gp pp
				| `Failure ->
					failwith "oops"
		and register handler carried sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (post_params ** Submit.param)
				(handler carried)
		in (register, form_maker, handler)


	let make_first ~common:fallback ~carrier ~content ~next =
		let handler sp gp () =
			let result = carrier ~carried:() sp gp ()
			in match result with
				| `Success carry ->
					let (next_register, next_form_maker, next_handler) = next in
					let form_maker (enter_next, enter_submit) =
						(next_form_maker ~carry enter_next) @ [Submit.make_controls enter_submit] in
					let next_service = next_register next_handler carry sp in
					let form = Eliom_predefmod.Xhtml.post_form next_service sp form_maker gp
					in content ~carry ~form sp gp ()
				| `Failure ->
					failwith "oops"
		in Eliom_predefmod.Xhtml.register fallback handler
end

