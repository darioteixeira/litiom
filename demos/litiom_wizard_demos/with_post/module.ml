(********************************************************************************)
(* Demo of 3-step wizard whose first step takes POST parameters.		*)
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
		~path: ["wizard"]
		~get_params: Eliom_parameters.unit
		~cancelled_content
		~error_content
		()


(********************************************************************************)

let step3 =
	let normal_content ~carried:x sp () y =
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
		~common
		~normal_content
		~post_params: (Eliom_parameters.int "y")
		()


(********************************************************************************)

let step2 =
	let carrier ~carried sp () x =
		`Proceed x in
	let form_maker ~carried ~carry enter_y =
		[
		fieldset ~a:[a_class ["form_fields"]]
			[
			label ~a:[a_for "enter_y"] [pcdata "Enter int Y:"];
			Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_y"] ~input_type:`Text ~name:enter_y ();
			]
		] in
	let normal_content ~carried ~carry ~form sp () x =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 2")) [])
			(body [form]))
	in Steps.make_intermediate
		~common
		~carrier
		~form_maker
		~normal_content
		~post_params: (Eliom_parameters.int "x")
		~next: step3
		()


(********************************************************************************)

let step1 =
	let form_maker ~carried ~carry enter_x =
		[
		fieldset ~a:[a_class ["form_fields"]]
			[
			label ~a:[a_for "enter_x"] [pcdata "Enter int X:"];
			Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_x"] ~input_type:`Text ~name:enter_x ();
			]
		] in
	let normal_content ~carried ~carry ~form sp () pp =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 1")) [])
			(body	[
				p [pcdata ("The POST parameter is " ^ pp)];
				form
				])) in
	let fallback_content sp () () =
		Lwt.return
			(html
			(head (title (pcdata "Wizard fallback")) [])
			(body [p [pcdata "This is the fallback"]]))
	in Steps.make_first_with_post
		~common
		~carrier: Carriers.present
		~form_maker
		~normal_content
		~fallback_content
		~post_params: (Eliom_parameters.string "s")
		~next: step2
		()


(********************************************************************************)

let main_handler sp () () =
	let step1_form enter_s =
		[
		fieldset
			[
			label ~a:[a_for "enter_s"] [pcdata "Enter string:"];
			Eliom_predefmod.Xhtml.string_input ~a:[a_id "enter_s"] ~input_type:`Text ~name:enter_s ();
			Eliom_predefmod.Xhtml.string_input ~input_type:`Submit ~value:"Click" ()
			]
		]
	in Lwt.return
		(html
		(head (title (pcdata "Main")) [])
		(body [Eliom_predefmod.Xhtml.post_form step1 sp step1_form ()]))

let main =
	Eliom_predefmod.Xhtml.register_new_service
		~path: [""]
		~get_params: Eliom_parameters.unit
		main_handler

