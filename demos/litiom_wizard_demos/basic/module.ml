(********************************************************************************)
(* Demo of a basic 3-step wizard with no skippable steps nor POST parameters	*)
(* given to the first step.							*)
(********************************************************************************)

open XHTML.M
open Litiom_wizard


(********************************************************************************)

let fallback =
	Eliom_services.new_service ~path: [""] ~get_params: Eliom_parameters.unit ()


(********************************************************************************)

let step3 =
	let carrier ~carry_in sp () _ =
		Lwt.return (`Proceed ()) in
	let normal_content ~carry_in:x ~carry_out sp () y =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 3")) [])
			(body
				[
				p [pcdata ("X is " ^ (string_of_int x))];
				p [pcdata ("Y is " ^ (string_of_int y))];
				p [pcdata ("X + Y is " ^ (string_of_int (x+y)))];
				]))
	in Steps.make_last
		~fallback
		~carrier
		~normal_content
		~post_params: (Eliom_parameters.int "y")
		()


(********************************************************************************)

let step2 =
	let carrier ~carry_in sp () x =
		Lwt.return (`Proceed x) in
	let form_maker ~carry_in ~carry_out enter_y =
		Lwt.return
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_y"] [pcdata "Enter int Y:"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_y"] ~input_type:`Text ~name:enter_y ();
				]
			] in
	let normal_content ~carry_in ~carry_out ~form sp () x =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 2")) [])
			(body [form]))
	in Steps.make_intermediate
		~fallback
		~carrier
		~form_maker
		~normal_content
		~post_params: (Eliom_parameters.int "x")
		~next: step3
		()


(********************************************************************************)

let step1 =
	let form_maker ~carry_in ~carry_out enter_x =
		Lwt.return
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_x"] [pcdata "Enter int X:"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_x"] ~input_type:`Text ~name:enter_x ();
				]
			] in
	let normal_content ~carry_in ~carry_out ~form sp () () =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 1")) [])
			(body [form]))
	in Steps.make_first
		~fallback
		~carrier: Carriers.none
		~form_maker
		~normal_content
		~next: step2
		()

