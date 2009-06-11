(********************************************************************************)
(**	The [Litiom_wizard] module offers routines aiming to automate and
	simplify the construction of wizard-like interactions in websites.

	Copyright (c) 2009 Dario Teixeira (dario.teixeira\@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Litiom_wizard Tutorial}						*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Introduction}							*)
(********************************************************************************)

(**	This module aims to simplify the construction of wizard-like interactions
	in websites.  By {i wizard-like}, we mean that this interaction is akin to
	that commonly found on GUI applications, where the user is presented with
	an ordered sequence of dialog steps.

	Wizard-like interactions can of course be built directly with [Eliom].  More
	specifically, the developer can make use of attached coservices registered
	in session tables, thereby taking advantage of the continuation-based
	programming paradigm offered by [Eliom].  However, since this is a fairly
	common and repetitive pattern, it makes sense to "scratch the boilerplate"
	and to create routines to automate the task.  Herein lies the rationale
	for the {!Litiom_wizard} module.

	So what does {!Litiom_wizard} offer?  Essentially, the developer only
	needs to specify the non-repetitive contents of each dialog's form.
	{!Litiom_wizard} will automatically take care of adding and managing
	the "Cancel" and "Proceed" buttons that animate the wizard, handling
	the "discardable forms" problem that arises when the user presses
	"Cancel" without filling a form, and of course, registering and invoking
	each successive step in the wizard.

	The above described facilities are offered by the {!Steps} submodule, and
	this document continues with a tutorial explaining how that submodule should
	be used.  The tutorial is based on a simple example: a three-step wizard
	that asks for an integer [x] on the first step, another integer [y] on the
	second step, and presents the result of [x+y] on the third and last step.
*)


(********************************************************************************)
(**	{3 Using the steps module}						*)
(********************************************************************************)

(**	The {!Steps} module provides a moderately low-level interface to the
	construction of a wizard.  By {i low-level}, it is meant that the user
	must declare each wizard step explicitly and separately;  moreover, the
	wizard steps must be declared in reverse sequential order (a tell-tale
	sign that the underlying [Eliom] mechanism is not rendered completely
	opaque to the user of the module).

	Only linear wizards are supported.  There is however support for wizards
	where one or more intermediate steps may be skipped.  The wizard may take
	GET parameters, in which case they are common to all steps of the wizard.
	Each step (except the last) also creates a form whose values are passed
	to the subsequent step as POST parameters.  There is also a facility for
	carrying any values from one step to the next.

	We begin by declaring the commonalities to all steps of the wizard.
	These include the URL path, the GET parameters, and the default handlers
	to be invoked when the user cancels the wizard or an error occurs (note
	that each wizard step may override these defaults).  The common elements
	are declared via the {!Steps.make_common} function, as examplified by the
	following code:

	{v
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
	v}


	The {!Steps} module has three main step-creation functions, some of which
	have variant forms.  The functions are as follows:

	{ul
		{li {!Steps.make_last} is only used once per wizard, to create the
		final step.}
		{li {!Steps.make_first} is also only used once per wizard, to create
		the initial step.  (There is a variant {!Steps.make_first_with_post}
		that can be used if the first step has POST parameters).}
		{li {!Steps.make_intermediate} is used to create the intermediate (ie,
		neither initial nor final) steps of the wizard.  In a wizard with N
		steps, there will be N-2 intermediate steps.  (Note that there is a
		variant {!Steps.make_skippable} that creates an intermediate step that
		may be skipped).}}


	Remember that we are creating a wizard with 3 steps, none of which are skippable
	(note that the first and last steps are never skippable!), and whose first step
	takes no POST parameters.  We will therefore use functions {!Steps.make_first},
	{!Steps.make_intermediate}, and {!Steps.make_last} to create steps 1, 2, and 3,
	respectively.

	Because the steps must be declared in reverse order, we begin by the last one.
	Do bear in mind, however, that it is often easier to write and understand the
	code if you begin by the first step.  The mandatory parameters to function
	{!Steps.make_last} include the [common] elements that were previosuly declared,
	the [normal_content] handler that produces the actual contents of the wizard,
	and the [post_params] stating the parameters for this step.  You may optionally
	declare new [cancelled_content] or [error_content] handlers if you don't wish
	to use the default ones.

	The [normal_content] resembles a normal [Eliom] handler, but takes extra named
	parameters.  For the last wizard step, only the extra parameter [carry_in] is given.
	This parameter contains whatever value was carried over from the previous step.
	In this case, the value [x] is carried.

	{v
	let step3 =
		let normal_content ~carry_in:x sp () y =
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
	v}


	We now declare the second-to-last step of wizard (which also happens to be the second
	step).  This step is neither the first nor the last, and because it may not be skipped
	it is declared via the {!Steps.make_intermediate} function.  Besides the already described
	[common], [normal_content], and [post_params] parameters, an intermediate step requires
	also the provision of a [carrier] function, a [form_maker], and a pointer to the step
	that follows.

	The [carrier] is an important concept in [Litiom_wizard].  It is basically a function
	that given the parameters carried from previous steps, plus the GET and POST parameters
	given to the current step, decides whether the wizard should continue as normal, fail
	altogether, or if the current step is skippable, be skipped.  In the first and last
	cases, the value to be carried over to the next step must also be provided.  The return
	values are respectively [(`Proceed 'a) Lwt.t], [`Cancel Lwt.t], and [(`Skip 'b) Lwt.t].
	Note that the carrier function must be written in a cooperative fashion using the [Lwt]
	module.  As for the present concrete example, remember that during the declaration of
	[step3], we stated that the value [x] was carried over from the previous step.
	Therefore, the carrier for [step2] must return [`Proceed x]).

	The [form_maker] is a function that creates the form for the next step.  The approach
	is similar to that used by [Eliom.lwt_post_form], with the difference that [form_maker]
	takes two additional named parameters: [carry_in] with the value carried over from the
	previous step, and [carry_out] with the result to be carried over to the next step, as
	returned by the [carrier] function.  Note that the "Cancel" and "Proceed" buttons
	that animate the wizard are automatically added, and you do not need to worry about
	those.

	The [normal_content] function is similar to the one described for the final step,
	with the difference that in the initial and intermediate steps this function takes
	two additional parameters: [carry_out] is the value to be carried over to the next
	step, and [form] is the corresponding generated form.

	{v
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
			~common
			~carrier
			~form_maker
			~normal_content
			~post_params: (Eliom_parameters.int "x")
			~next: step3
			()
	v}


	Finally we declare the first step of the wizard.  The parameters given to function
	{!Steps.make_first} are similar to those given in the intermediate step.  To note
	is the fact that no POST parameters are given, and that instead of providing a
	custom carrier function, we instead rely on one of the standard carriers defined
	in the {!Carriers} module.

	{v
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
			~carrier: Carriers.none
			~form_maker
			~normal_content
			~next: step2
			()
	v}


	*)


(********************************************************************************)
(**	{2 Public API}								*)
(********************************************************************************)

(********************************************************************************)
(**	{3 Submit module}							*)
(********************************************************************************)

module type SUBMIT =
  sig
    type t = Proceed | Cancel
    val of_string : string -> t
    val to_string : t -> string
    val param_label : string
    val param :
      (t, [ `WithoutSuffix ], [ `One of t ] Eliom_parameters.param_name)
      Eliom_parameters.params_type
    val make_controls :
      [< t Eliom_parameters.setoneradio ] Eliom_parameters.param_name ->
      [> `Fieldset ] XHTML.M.elt
  end

module Submit : SUBMIT


(********************************************************************************)
(**	{3 Carriers module}							*)
(********************************************************************************)

module Carriers :
sig
	val none : carry_in:'a -> 'b -> 'c -> 'd -> [> `Proceed of unit ] Lwt.t
	val past : carry_in:'a -> 'b -> 'c -> 'd -> [> `Proceed of 'a ] Lwt.t
	val present : carry_in:'a -> 'b -> 'c -> 'd -> [> `Proceed of 'd ] Lwt.t
	val both : carry_in:'a -> 'b -> 'c -> 'd -> [> `Proceed of 'a * 'd ] Lwt.t
end


(********************************************************************************)
(**	{3 Steps module}							*)
(********************************************************************************)

module Steps :
  sig
    val error_handler :
      cancelled_content:(Eliom_sessions.server_params -> 'a Lwt.t) ->
      error_content:(Eliom_sessions.server_params -> 'b -> 'a Lwt.t) ->
      Eliom_sessions.server_params -> 'b -> 'a Lwt.t
    val make_common :
      path:Ocsigen_extensions.url_path ->
      get_params:('a, [< Eliom_services.suff ] as 'b, 'c)
                 Eliom_parameters.params_type ->
      cancelled_content:'d ->
      error_content:'e ->
      unit ->
      ('a, unit,
       [> `Attached of
            [> `Internal of [> `Service ] * [> `Get ] ] Eliom_services.a_s ],
       'b, 'c, unit, [> `Registrable ])
      Eliom_services.service * 'd * 'e
    val get_common :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [ `Coservice | `Service ] * [ `Get ] ]
                  Eliom_services.a_s ],
              'b, 'c, unit, [ `Registrable ])
             Eliom_services.service * 'd * 'e ->
      ?cancelled_content:'d ->
      ?error_content:'e ->
      unit ->
      ('a, unit,
       [> `Attached of
            [> `Internal of [ `Coservice | `Service ] * [> `Get ] ]
            Eliom_services.a_s ],
       'b, 'c, unit, [> `Registrable ])
      Eliom_services.service * 'd * 'e
    val make_last :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [ `Coservice | `Service ] * [ `Get ] ]
                  Eliom_services.a_s ],
              [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
             Eliom_services.service *
             (Eliom_sessions.server_params ->
              Eliom_predefmod.Xhtml.page Lwt.t) *
             (Eliom_sessions.server_params ->
              (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      normal_content:(carry_in:'d ->
                      Eliom_sessions.server_params ->
                      'e -> 'f -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?cancelled_content:(Eliom_sessions.server_params ->
                          Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?error_content:(Eliom_sessions.server_params ->
                      (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      post_params:('g, [ `WithoutSuffix ], 'h) Eliom_parameters.params_type ->
      unit ->
      (('i ->
        Eliom_sessions.server_params ->
        'a -> 'g * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t) ->
       'i ->
       Eliom_sessions.server_params ->
       ('a, 'g * Submit.t,
        [> `Attached of
             [> `Internal of [> `Coservice ] * [> `Post ] ]
             Eliom_services.a_s ],
        'b, 'c, 'h * [ `One of Submit.t ] Eliom_parameters.param_name,
        [> `Registrable ])
       Eliom_services.service) *
      ('d ->
       Eliom_sessions.server_params ->
       'e -> 'f * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t)
    val make_intermediate :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [ `Coservice | `Service ] * [ `Get ] ]
                  Eliom_services.a_s ],
              [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
             Eliom_services.service *
             (Eliom_sessions.server_params ->
              Eliom_predefmod.Xhtml.page Lwt.t) *
             (Eliom_sessions.server_params ->
              (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(carry_in:'d ->
               Eliom_sessions.server_params ->
               'e -> 'f -> [< `Cancel | `Proceed of 'g ] Lwt.t) ->
      form_maker:(carry_in:'d ->
                  carry_out:'g ->
                  'h -> Xhtmltypes.form_content XHTML.M.elt list Lwt.t) ->
      normal_content:(carry_in:'d ->
                      carry_out:'g ->
                      form:[> Xhtmltypes.form ] XHTML.M.elt ->
                      Eliom_sessions.server_params ->
                      'e -> 'f -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?cancelled_content:(Eliom_sessions.server_params ->
                          Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?error_content:(Eliom_sessions.server_params ->
                      (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      post_params:('i, [ `WithoutSuffix ], 'j) Eliom_parameters.params_type ->
      next:('k ->
            'g ->
            Eliom_sessions.server_params ->
            ('e, 'l, [< Eliom_services.post_service_kind ],
             [< Eliom_services.suff ], 'm,
             'h *
             [< Submit.t Eliom_parameters.setoneradio ]
             Eliom_parameters.param_name, [< Eliom_services.registrable ])
            Eliom_services.service) *
           'k ->
      unit ->
      (('n ->
        Eliom_sessions.server_params ->
        'a -> 'i * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t) ->
       'n ->
       Eliom_sessions.server_params ->
       ('a, 'i * Submit.t,
        [> `Attached of
             [> `Internal of [> `Coservice ] * [> `Post ] ]
             Eliom_services.a_s ],
        'b, 'c, 'j * [ `One of Submit.t ] Eliom_parameters.param_name,
        [> `Registrable ])
       Eliom_services.service) *
      ('d ->
       Eliom_sessions.server_params ->
       'e -> 'f * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t)
    val make_skippable :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [ `Coservice | `Service ] * [ `Get ] ]
                  Eliom_services.a_s ],
              [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
             Eliom_services.service *
             (Eliom_sessions.server_params ->
              Eliom_predefmod.Xhtml.page Lwt.t) *
             (Eliom_sessions.server_params ->
              (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(carry_in:'d ->
               Eliom_sessions.server_params ->
               'e -> 'f -> [< `Cancel | `Proceed of 'g | `Skip of 'h ] Lwt.t) ->
      form_maker:(carry_in:'d ->
                  carry_out:'g ->
                  'i -> Xhtmltypes.form_content XHTML.M.elt list Lwt.t) ->
      normal_content:(carry_in:'d ->
                      carry_out:'g ->
                      form:[> Xhtmltypes.form ] XHTML.M.elt ->
                      Eliom_sessions.server_params ->
                      'e -> 'f -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?cancelled_content:(Eliom_sessions.server_params ->
                          Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?error_content:(Eliom_sessions.server_params ->
                      (string * exn) list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      post_params:('j, [ `WithoutSuffix ], 'k) Eliom_parameters.params_type ->
      next:(('h ->
             Eliom_sessions.server_params ->
             'e -> 'l * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t) ->
            'g ->
            Eliom_sessions.server_params ->
            ('e, 'm, [< Eliom_services.post_service_kind ],
             [< Eliom_services.suff ], 'n,
             'i *
             [< Submit.t Eliom_parameters.setoneradio ]
             Eliom_parameters.param_name, [< Eliom_services.registrable ])
            Eliom_services.service) *
           ('h ->
            Eliom_sessions.server_params ->
            'e -> 'l option * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      unit ->
      (('o ->
        Eliom_sessions.server_params ->
        'a -> 'j * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t) ->
       'o ->
       Eliom_sessions.server_params ->
       ('a, 'j * Submit.t,
        [> `Attached of
             [> `Internal of [> `Coservice ] * [> `Post ] ]
             Eliom_services.a_s ],
        'b, 'c, 'k * [ `One of Submit.t ] Eliom_parameters.param_name,
        [> `Registrable ])
       Eliom_services.service) *
      ('d ->
       Eliom_sessions.server_params ->
       'e -> 'f * Submit.t -> Eliom_predefmod.Xhtml.page Lwt.t)
    val make_first_handler :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [ `Coservice | `Service ] * [ `Get ] ]
                  Eliom_services.a_s ],
              'b, 'c, unit, [ `Registrable ])
             Eliom_services.service * 'd *
             (Eliom_sessions.server_params -> 'e list -> 'f Lwt.t) ->
      carrier:(carry_in:unit ->
               Eliom_sessions.server_params ->
               'g -> 'h -> [< `Cancel | `Proceed of 'i ] Lwt.t) ->
      form_maker:(carry_in:unit ->
                  carry_out:'i ->
                  'j -> Xhtmltypes.form_content XHTML.M.elt list Lwt.t) ->
      normal_content:(carry_in:unit ->
                      carry_out:'i ->
                      form:[> Xhtmltypes.form ] XHTML.M.elt ->
                      Eliom_sessions.server_params -> 'g -> 'h -> 'f Lwt.t) ->
      ?error_content:(Eliom_sessions.server_params -> 'e list -> 'f Lwt.t) ->
      next:('k ->
            'i ->
            Eliom_sessions.server_params ->
            ('g, 'l, [< Eliom_services.post_service_kind ],
             [< Eliom_services.suff ], 'm,
             'j *
             [< Submit.t Eliom_parameters.setoneradio ]
             Eliom_parameters.param_name, [< Eliom_services.registrable ])
            Eliom_services.service) *
           'k ->
      unit -> Eliom_sessions.server_params -> 'g -> 'h -> 'f Lwt.t
    val make_first :
      ?sp:Eliom_sessions.server_params ->
      common:('a, unit,
              [ `Attached of
                  [ `Internal of Eliom_services.servcoserv * [ `Get ] ]
                  Eliom_services.a_s ],
              [< Eliom_services.suff ], 'b, unit, [ `Registrable ])
             Eliom_services.service * 'c *
             (Eliom_sessions.server_params ->
              'd list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(carry_in:unit ->
               Eliom_sessions.server_params ->
               'a -> unit -> [< `Cancel | `Proceed of 'e ] Lwt.t) ->
      form_maker:(carry_in:unit ->
                  carry_out:'e ->
                  'f -> Xhtmltypes.form_content XHTML.M.elt list Lwt.t) ->
      normal_content:(carry_in:unit ->
                      carry_out:'e ->
                      form:[> Xhtmltypes.form ] XHTML.M.elt ->
                      Eliom_sessions.server_params ->
                      'a -> unit -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?error_content:(Eliom_sessions.server_params ->
                      'd list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      next:('g ->
            'e ->
            Eliom_sessions.server_params ->
            ('a, 'h, [< Eliom_services.post_service_kind ],
             [< Eliom_services.suff ], 'i,
             'f *
             [< Submit.t Eliom_parameters.setoneradio ]
             Eliom_parameters.param_name, [< Eliom_services.registrable ])
            Eliom_services.service) *
           'g ->
      unit -> unit
    val make_first_with_post :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of Eliom_services.servcoserv * [ `Get ] ]
                  Eliom_services.a_s ],
              [< Eliom_services.suff ] as 'b, 'c, unit, [ `Registrable ])
             Eliom_services.service * 'd *
             (Eliom_sessions.server_params ->
              'e list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(carry_in:unit ->
               Eliom_sessions.server_params ->
               'a -> 'f -> [< `Cancel | `Proceed of 'g ] Lwt.t) ->
      form_maker:(carry_in:unit ->
                  carry_out:'g ->
                  'h -> Xhtmltypes.form_content XHTML.M.elt list Lwt.t) ->
      normal_content:(carry_in:unit ->
                      carry_out:'g ->
                      form:[> Xhtmltypes.form ] XHTML.M.elt ->
                      Eliom_sessions.server_params ->
                      'a -> 'f -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      fallback_content:(Eliom_sessions.server_params ->
                        'a -> unit -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      post_params:('f, [ `WithoutSuffix ], 'i) Eliom_parameters.params_type ->
      ?error_content:(Eliom_sessions.server_params ->
                      'e list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      next:('j ->
            'g ->
            Eliom_sessions.server_params ->
            ('a, 'k, [< Eliom_services.post_service_kind ],
             [< Eliom_services.suff ], 'l,
             'h *
             [< Submit.t Eliom_parameters.setoneradio ]
             Eliom_parameters.param_name, [< Eliom_services.registrable ])
            Eliom_services.service) *
           'j ->
      unit ->
      ('a, 'f,
       [> `Attached of
            [> `Internal of Eliom_services.servcoserv * [> `Post ] ]
            Eliom_services.a_s ],
       'b, 'c, 'i, [> `Registrable ])
      Eliom_services.service
  end
