(********************************************************************************)
(**	Litiom_wizard module.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira\@yahoo.com)

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
(* Carriers.									*)
(********************************************************************************)

module Carriers =
struct
	let none ~carried sp gp pp = `Continue ()

	let past ~carried sp gp pp = `Continue carried

	let present ~carried sp gp pp = `Continue pp

	let all ~carried sp gp pp = `Continue (carried, pp)
end


(********************************************************************************)
(* Steps module.								*)
(********************************************************************************)

module Steps =
struct
	let error_handler ~cancelled_content ~error_content = fun sp exc_list ->
		Eliom_sessions.get_post_params ~sp >>= fun params ->
		let maybe_submit =
			try
				let (_, submit_raw) = List.find (fun (k, v) -> k = Submit.param_label) params
				in Some (Submit.of_string submit_raw)
			with
				| Not_found
				| Invalid_argument _ -> None
		in match maybe_submit with
			| None
			| Some Submit.Proceed   -> error_content sp exc_list
			| Some Submit.Cancel    -> cancelled_content sp


	let make_common ~path ~get_params ~cancelled_content ~error_content () =
		let fallback = Eliom_services.new_service ~path ~get_params ()
		in (fallback, cancelled_content, error_content)


	let get_common ~common ?cancelled_content ?error_content () =
		let (fallback, default_cancelled_content, default_error_content) = common in
		let cancelled_content = match cancelled_content with
			| Some thing	-> thing
			| None 		-> default_cancelled_content
		and error_content = match error_content with
			| Some thing	-> thing
			| None 		-> default_error_content
		in ((fallback : ('a, unit, [ `Attached of [ `Internal of [ `Coservice | `Service ] * [ `Get ] ] Eliom_services.a_s ], 'b, 'c, unit, [ `Registrable ]) Eliom_services.service :> ('get,unit, [> `Attached of [> `Internal of [> `Service ] * [> `Get] ] Eliom_services.a_s ], 'tipo,'gn, unit, [> `Registrable ]) Eliom_services.service), cancelled_content, error_content)


	let make_last ~common ~normal_content ?cancelled_content ?error_content ~post_params () =
		let (fallback, cancelled_content, error_content) = get_common ~common ?cancelled_content ?error_content () in
		let handler carried sp gp (pp, submit_param) =
			 match submit_param with
				| Submit.Proceed	-> normal_content ~carried sp gp pp
				| Submit.Cancel		-> cancelled_content sp
		and register handler carried sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (post_params ** Submit.param)
				~error_handler: (error_handler ~cancelled_content ~error_content)
				(handler carried)
		in (register, handler)


	let make_intermediate ~common ~carrier ~form_maker ~normal_content ?cancelled_content ?error_content ~post_params ~next () =
		let (fallback, cancelled_content, error_content) = get_common ~common ?cancelled_content ?error_content () in
		let handler carried sp gp (pp, submit_param) = match submit_param with
			| Submit.Proceed ->
				let result = carrier ~carried sp gp pp
				in (match result with
					| `Continue carry ->
						let (next_register, next_handler) = next in
						let make_form (enter_next, enter_submit) =
							(form_maker ~carried ~carry enter_next) @ [Submit.make_controls enter_submit] in
						let next_service = next_register next_handler carry sp in
						let form = Eliom_predefmod.Xhtml.post_form next_service sp make_form gp
						in normal_content ~carried ~carry ~form sp gp pp
					| `Fail ->
						error_content sp [])
			| Submit.Cancel ->
				cancelled_content sp
		and register handler carried sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (post_params ** Submit.param)
				~error_handler: (error_handler ~cancelled_content ~error_content)
				(handler carried)
		in (register, handler)


	let make_skippable ~common ~carrier ~form_maker ~normal_content ?cancelled_content ?error_content ~post_params ~next () =
		let (fallback, cancelled_content, error_content) = get_common ~common ?cancelled_content ?error_content () in
		let handler carried sp gp (pp, submit_param) = match submit_param with
			| Submit.Proceed ->
				let result = carrier ~carried sp gp pp
				in (match result with
					| `Skip carry ->
						let (_, next_handler) = next
						in next_handler carry sp gp (None, Submit.Proceed)
					| `Continue carry ->
						let (next_register, next_handler) = next in
						let real_next_handler carry sp gp (pp, submit_param) =
							next_handler carry sp gp (Some pp, submit_param) in
						let make_form (enter_next, enter_submit) =
							(form_maker ~carried ~carry enter_next) @ [Submit.make_controls enter_submit] in
						let next_service = next_register real_next_handler carry sp in
						let form = Eliom_predefmod.Xhtml.post_form next_service sp make_form gp
						in normal_content ~carried ~carry ~form sp gp pp
					| `Fail ->
						error_content sp [])
			| Submit.Cancel ->
				cancelled_content sp
		and register handler carried sp =
			Eliom_predefmod.Xhtml.register_new_post_coservice_for_session
				~sp
				~fallback
				~post_params: (post_params ** Submit.param)
				~error_handler: (error_handler ~cancelled_content ~error_content)
				(handler carried)
		in (register, handler)


	let make_first_handler ~common ~carrier ~form_maker ~normal_content ?error_content ~next () =
		let (_, _, error_content) = get_common ~common ?error_content () in
		let handler sp gp pp =
			let carried = () in
			let result = carrier ~carried sp gp pp
			in match result with
				| `Continue carry ->
					let (next_register, next_handler) = next in
					let make_form (enter_next, enter_submit) =
						(form_maker ~carried ~carry enter_next) @ [Submit.make_controls enter_submit] in
					let next_service = next_register next_handler carry sp in
					let form = Eliom_predefmod.Xhtml.post_form next_service sp make_form gp
					in normal_content ~carried ~carry ~form sp gp pp
				| `Fail ->
					error_content sp []
		in handler


	let make_first ?sp ~common ~carrier ~form_maker ~normal_content ?error_content ~next () =
		let (fallback, _, _) = get_common ~common () in
		let handler = make_first_handler ~common ~carrier ~form_maker ~normal_content ?error_content ~next ()
		in Eliom_predefmod.Xhtml.register ?sp ~service:fallback handler


	let make_first_with_post ~common ~carrier ~form_maker ~normal_content ~fallback_content ~post_params ?error_content ~next () =
		let (fallback, _, _) = get_common ~common () in
		let () = Eliom_predefmod.Xhtml.register ~service:fallback fallback_content in
		let handler = make_first_handler ~common ~carrier ~form_maker ~normal_content ?error_content ~next () in
		Eliom_predefmod.Xhtml.register_new_post_service ~fallback ~post_params handler
end

