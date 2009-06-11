(********************************************************************************)
(* Demo for a 4-step wizard whose third step may be skipped.			*)
(********************************************************************************)

open XHTML.M
open Litiom_wizard


(********************************************************************************)

let common =
	let cancelled_content sp =
		Lwt.return
			(html
			(head (title (pcdata "Wizard cancelled")) [])
			(body [p [pcdata "You cancelled!"]]))
	and error_content sp exc_list =
		Lwt.return
			(html
			(head (title (pcdata "Wizard error")) [])
			(body [p [pcdata "There was an error!"]]))
	in Steps.make_common
		~path: [""]
		~get_params: Eliom_parameters.unit
		~cancelled_content
		~error_content
		()


(********************************************************************************)

let step4 =
	let normal_content ~carry_in:(x, y) sp () maybe_z =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 4")) [])
			(body
				[
				p [pcdata ("X is " ^ (string_of_int x))];
				p [pcdata ("Y is " ^ (string_of_int y))];
				p [pcdata ("Z is " ^ match maybe_z with Some z -> string_of_int z | None -> "(none)")]
				]))
	in Steps.make_last
		~common
		~normal_content
		~post_params: (Eliom_parameters.int "z")
		()


(********************************************************************************)

let step3 =
	let carrier ~carry_in:x sp () y =
		Lwt.return (if x > y then `Proceed (x, y) else `Skip (x, y)) in
	let form_maker ~carry_in ~carry_out enter_z =
		Lwt.return
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_z"] [pcdata "Enter int Z:"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_z"] ~input_type:`Text ~name:enter_z ();
				]
			] in
	let normal_content ~carry_in ~carry_out ~form sp () y =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 3")) [])
			(body [form]))
	in Steps.make_skippable
		~common
		~carrier
		~form_maker
		~normal_content
		~post_params: (Eliom_parameters.int "y")
		~next: step4
		()


(********************************************************************************)

let step2 =
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
		~common
		~carrier: Carriers.present
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
		~common
		~carrier: Carriers.present
		~form_maker
		~normal_content
		~next: step2
		()

