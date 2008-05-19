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

	let inter ~carrier ~next_step_register ~cancelled_canvas ~tree_builder ~canvas ~carry =
		fun sp () (this_params, submit_param) ->
			let canvas_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry this_params in
					let next_step = next_step_register ~carry sp
					in Litiom_blocks.sink (canvas ~next_step ~carry)
				| Submit.Cancel ->
					cancelled_canvas
			in tree_builder sp canvas_tree

	let final ~carrier ~cancelled_canvas ~tree_builder ~canvas ~carry =
		fun sp () (this_params, submit_param) ->
			let canvas_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry this_params
					in Litiom_blocks.sink (canvas ~carry)
				| Submit.Cancel ->
					cancelled_canvas
			in tree_builder sp canvas_tree

	end


(********************************************************************************)
(* Error_handler module.							*)
(********************************************************************************)

(**	The [Error_handler] module will do this and that.
*)
module Error_handler =
struct
	let inter ~cancelled_canvas ~error_canvas ~tree_builder =
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
				| Some Submit.Cancel	-> cancelled_canvas
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
(* Carriers module.								*)
(********************************************************************************)

(**	The [Carriers] module includes some simple predefined functions that encode
	the typical actions that each step of a wizard can take concerning passing
	its parameters to the subsequent step.
*)
module Carriers =
struct
	(**	This function will pass on to the next step a pair consisting of
		the parameters accumulated from the previous step and the form
		parameters given to the current step.
	*)
	let carry_both previous current = (previous, current)


	(**	This function will pass on to the next step only the parameters
		accumulated from the previous step, discarding the form parameters
		given to the current step.
	*)
	let carry_previous previous current = previous


	(**	This function will pass on to the next step only the form parameters
		given to the current step, discarding the parameters accumulated
		from the previous step.
	*)
	let carry_current previous current = current


	(**	This function will discard all parameters, passing on to the next
		step only a value of type unit.
	*)
	let carry_none previous current = ()
end


(********************************************************************************)
(* Raw_steps module.								*)
(********************************************************************************)

(**	The {!Raw_steps} module provides a low-level interface to the construction
	of wizard-like interactions.  Wizard steps created via this module must be
	declared in reversed sequential order; this is because the user is expected
	to explicitly provide the next wizard step as a parameter.  Note that there
	are special functions to create the first and last steps of the wizard
	({!make_first} and {!make_last}, respectively).  All other steps must be
	created with {!make_middle}.

	At last, note that even though this module's function signatures look
	intimidating, they are actually fairly straightforward to use.  Check
	the introduction for a small example.
*)
module Raw_steps =
struct

	(**	Creates the first step of the wizard.
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


	(**	Creates a wizard's intermediate step (ie, a step which is neither the first nor the last).
	*)
	let make_middle
		~fallback
		~tree_builder
		~carrier
		~form_contents
		~next_step_register
		~cancelled_canvas
		~error_canvas
		~params =

		let canvas = Canvas.inter
				~form_contents in
		let handler = Handler.inter
				~carrier
				~next_step_register
				~cancelled_canvas
				~tree_builder
				~canvas in
		let error_handler = Error_handler.inter
				~cancelled_canvas
				~error_canvas
				~tree_builder in
		let register = Register.inter
				~fallback
				~handler
				~error_handler
				~params
		in register


	(**	Creates the last step of the wizard.
	*)
	let make_last
		~fallback
		~tree_builder
		~carrier
		~form_contents
		~cancelled_canvas
		~error_canvas
		~params =

		let canvas = Canvas.final
				~form_contents in
		let handler = Handler.final
				~carrier
				~cancelled_canvas
				~tree_builder
				~canvas in
		let error_handler = Error_handler.inter
				~cancelled_canvas
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
(* Standard module.								*)
(********************************************************************************)

(**	The Standard module provides a high-level interface to the construction
	of wizards.
*)
module Standard =
struct
end

