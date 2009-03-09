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
(* Content module.								*)
(********************************************************************************)

(**	The [Content] module will do this and that.
*)
module Content =
struct
	let inter ~content_maker ~next_step ~gp ~pp ~carry sp bp =
		let create_form () =
			content_maker ~sp ~bp ~gp ~pp ~carry enter_next_params >>= fun form_contents ->
			Lwt.return
				(fun (enter_next_params, enter_submit) ->
					Lwt.return (form_contents @ [Submit.make_controls enter_submit]))
		in create_form () >>= fun created_form ->
		Lwt.return [div [Eliom_predefmod.Xhtml.post_form next_step sp created_form gp]]

	let final ~content_maker ~gp ~pp ~carry sp bp =
		Lwt.return [div (content_maker ~sp ~bp ~gp ~pp ~carry ())]
end


(********************************************************************************)
(* Handler module.								*)
(********************************************************************************)

(**	The [Handler] module will do this and that.
*)
module Handler =
struct
	let initial ~carrier ~next_step_register ~tree_builder ~content =
		fun sp gp () ->
			let carry = carrier () () in
			let next_step = next_step_register ~carry sp in
			let content_tree = Litiom_blocks.sink (content ~next_step ~gp ~pp ~carry)
			in tree_builder sp frame_tree

	let inter ~carrier ~next_step_register ~cancelled_canvas ~tree_builder ~frame ~carry =
		fun sp gp (pp, submit_param) ->
			let frame_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry pp in
					let next_step = next_step_register ~carry sp
					in Litiom_blocks.sink (frame ~next_step ~gp ~pp ~carry)
				| Submit.Cancel ->
					cancelled_canvas
			in tree_builder sp frame_tree

	let final ~carrier ~cancelled_canvas ~tree_builder ~frame ~carry =
		fun sp gp (pp, submit_param) ->
			let frame_tree = match submit_param with
				| Submit.Proceed ->
					let carry = carrier carry pp
					in Litiom_blocks.sink (frame ~gp ~pp ~carry)
				| Submit.Cancel ->
					cancelled_canvas
			in tree_builder sp frame_tree

	end


(********************************************************************************)
(* Error_handler module.							*)
(********************************************************************************)

(**	The [Error_handler] module will do this and that.
*)
module Error_handler =
struct
	let inter ~cancelled_canvas ~error_frame ~tree_builder =
		fun sp exc_list ->
			Eliom_sessions.get_post_params ~sp >>= fun params ->
			let maybe_submit =
				try
					let (_, submit_raw) = List.find (fun (k, v) -> k = Submit.param_label) params
					in Some (Submit.of_string submit_raw)
				with
					| Not_found
					| Invalid_argument _ -> None in
			let frame_tree = match maybe_submit with
				| None
				| Some Submit.Proceed	-> error_frame exc_list
				| Some Submit.Cancel	-> cancelled_canvas
			in tree_builder sp frame_tree
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
(* Top-level public functions.							*)
(********************************************************************************)


(**	Creates the first step of the wizard.
*)
let make_first
	~fallback
	~tree_builder
	~content_maker
	~next_step_register
	~params

	let content = Content.inter
			~content_maker in
	let handler = Handler.initial
			~next_step_register
			~tree_builder
			~content in
	let register = Register.initial
			~fallback
			~handler
			~params
	in register


(**	Creates a wizard's intermediate step (ie, a step which is neither the first nor the last).
*)
let make_middle
	~fallback
	~tree_builder
	~content_maker
	~next_step_register
	~cancelled_canvas_tree
	~error_canvas_tree
	~params =

	let content = Frame.inter
			~content_maker in
	let handler = Handler.inter
			~next_step
			~cancelled_canvas
			~tree_builder
			~frame in
	let error_handler = Error_handler.inter
			~cancelled_canvas
			~error_frame
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
	~content_maker
	~cancelled_canvas
	~error_frame
	~params =

	let frame = Frame.final
			~content_maker in
	let handler = Handler.final
			~cancelled_canvas
			~tree_builder
			~frame in
	let error_handler = Error_handler.inter
			~cancelled_canvas
			~error_frame
			~tree_builder in
	let register = Register.inter
			~fallback
			~handler
			~error_handler
			~params
	in register

