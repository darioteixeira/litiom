(********************************************************************************)
(* Demo of 3-step wizard whose first step takes POST parameters.		*)
(********************************************************************************)

open XHTML.M
open Litiom_wizard


(********************************************************************************)

let fallback =
	Eliom_services.new_service
		~path: ["wizard"]
		~get_params: Eliom_parameters.unit
		()


(********************************************************************************)

let step3 =
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
		~carrier: Carriers.none
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

let step1_handler =
	let form_maker ~carry_in ~carry_out enter_x =
		Lwt.return
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_x"] [pcdata "Enter int X:"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_x"] ~input_type:`Text ~name:enter_x ();
				]
			] in
	let normal_content ~carry_in ~carry_out ~form sp () pp =
		Lwt.return
			(html
			(head (title (pcdata "Wizard step 1")) [])
			(body	[
				p [pcdata ("The POST parameter is " ^ pp)];
				form
				]))
	in Steps.make_first_handler
		~carrier: Carriers.present
		~form_maker
		~normal_content
		~next: step2
		()

(********************************************************************************)

let fallback_content sp () () =
	Lwt.return
		(html
		(head (title (pcdata "Wizard fallback")) [])
		(body [p [pcdata "This is the fallback"]]))

let () =
	Eliom_predefmod.Xhtml.register ~service:fallback fallback_content

let step1 =
	Eliom_predefmod.Xhtml.register_new_post_service
		~fallback
		~post_params: (Eliom_parameters.string "s")
		step1_handler


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
