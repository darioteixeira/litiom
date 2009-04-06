(********************************************************************************)
(**	Litiom_wizard module.

	This module offers routines aiming to automate and simplify
	the construction of wizard-like interactions in websites.

	Copyright (c) 2008 Dario Teixeira (dario.teixeira\@yahoo.com)

	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)


(********************************************************************************)
(**	{2 Introduction}							*)
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
	that asks for an integer [a] on the first step, another integer [b] on the
	second step, and presents the result of [a+b] on the third and last step.
*)


(********************************************************************************)
(**	{2 Tutorial}								*)
(********************************************************************************)

(**	The {!Steps} module provides a moderately low-level interface to the
	construction of a wizard.  By {i low-level}, it is meant that the user
	must declare each wizard step explicitly and separately; moreover, the
	wizard steps must be declared in reverse sequential order (a tell-tale
	sign that the underlying [Eliom] mechanism is not rendered completely
	opaque to the user of the module).

	We begin by declaring the blocks that constitute the site.  {!Litiom_wizard}
	relies on {!Litiom_blocks}, and therefore these blocks must be built to
	satisfy the requirements of the latter module.  The first block, [get_login]
	is a source node that returns the currently logged in user (or suppose it
	does; to simplify the example if just returns a fixed integer).  The two
	blocks that follow, [header] and [footer] will appear in every page.  Finally,
	the blocks [cancelled_canvas] and [error_frame] are the special blocks that
	should be shown should the user cancel the wizard or an error situation occur,
	respectively.  Note that with the exception of [get_login], all blocks are
	are sinks, returning actual XHTML content:

	{v
	let get_login sp () =
		Lwt.return 10

	let header _ _ =
		Lwt.return [div [h1 [pcdata "header"]]]

	let footer _ _ =
		Lwt.return [div [h1 [pcdata "footer"]]]

	let cancelled_canvas _ _ =
		Lwt.return [div [h1 [pcdata "Cancelled!"]]]

	let error_frame exc_list = fun _ _ ->
		Lwt.return [div [h1 [pcdata "Error!"]]]
	v}


	We then define the standard page builder, using the plumbing primitives from
	{!Litiom_blocks}.  Note that the every page is composed of three visible boxes:
	a constant header and footer, and a "frame" box which varies from page to page.

	{v
	let standard_handler ~page_title sp frame_tree =
		let tree =
			container
				(source get_login)
				[
				sink header;
				frame_tree;
				sink footer
				]
		in run_tree sp tree >>= fun page_body ->
		Lwt.return
			(html
				(head (title (pcdata page_title)) [])
				(body page_body))
	v}


	We must also declare the fallback for the wizard steps.  This fallback will be
	passed to all the functions that create the wizard steps.

	{v
	let fallback =
        Eliom_services.new_service
                ~path: [""]
                ~get_params: Eliom_parameters.unit
                ()
	v}

	
	We can at last create the various steps of the wizard.  Remember that the steps
	must be declared in reverse order.  We therefore begin with the last, created by
	function {!Steps.make_last}.  As for the parameters to this function, here's
	what you should know:

	{ul
		{li [fallback] is the customary [Eliom] fallback for expired services, etc.}
		{li [tree_builder] is a function that constructs a page.  This function takes
		two arguments: the usual [sp] server parameters, and a [frame_tree] containing
		the block that defines the frame.}
		{li [carrier] defines how the step's parameters should be given to the [contents]
		function and eventually carried over to the next step.  Each step (except the first)
		will typically have two sets of parameters: those carried over from the previous step,
		and the steps specific to the current step.  The function specified by [carrier] takes
		the two sets of parameters and should return the value to be passed on to the next step.
		You can define your own carrier function, or use one of those provided by the {!Carriers}
		module.}
		{li [contents] is the function that produces the step's contents.  For all steps except
		the last, the contents should be a form; as for the last step, the contents are simply
		an XHTML element.  As for the parameters to the [contents] function itself, they are
		as follows:
		{ul
			{li [sp] are the server parameters, as defined by [Eliom].}
			{li [bp] are the block parameter, as commonly used with {!Litiom_blocks}.}
			{li [gp] are the GET parameters of the service.}
			{li [pp] are POST parameters of the service.}
			{li [carry] is the result of the [carrier] function described above.}}}
		{li [cancelled_canvas] is the special frame that should be displayed (instead of [contents])
		should the user press the "Cancel" button.}
		{li [error_frame] is the special frame that should be displayed (instead of [contents])
		should there be an error in the forms parameters.}
		{li [params] are the step's specific (POST) parameters.  These should be specified in the
		[Eliom_parameters] format.}}

	{v
	let step3 =
		let step3_contents ~sp ~bp ~gp ~pp ~carry =
			let (a, b) = carry
			in      [
				p [pcdata (Printf.sprintf "%d + %d = %d" a b (a + b))];
				p [pcdata (Printf.sprintf "Login = %d" bp)]
				]
		in Litiom_wizard.Steps.make_last
			~fallback
			~tree_builder: (standard_handler ~page_title:"Step 3/3")
			~carrier: Litiom_wizard.Carriers.carry_both
			~contents: step3_contents
			~cancelled_canvas: (sink cancelled_canvas)
			~error_frame: (fun exc_list -> sink (error_frame exc_list))
			~params: (Eliom_parameters.int "b")
	v}


	Since this example contains a total of only three steps, there is only one intermediate
	(ie, neither initial nor final) step.  It must be created with the {!Steps.make_middle}
	function.  Should there have been more intermediate steps, all of them would likewise have
	been created via this function.  Note that the function's parameters are nearly identical
	to those described for {!Steps.make_last}, differing only in the addition of the
	[next_step_register] parameter.

	{v
	let step2 =
		let step2_contents ~sp ~bp ~gp ~pp ~carry enter_b =
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_b"] [pcdata "Enter number 'B':"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_b"] ~input_type:`Text ~name:enter_b ();
				]
			]
		in Litiom_wizard.Steps.make_middle
			~fallback
			~tree_builder: (standard_handler ~page_title:"Step 2/3")
			~carrier: Litiom_wizard.Carriers.carry_current
			~contents: step2_contents
			~next_step_register: step3
			~cancelled_canvas: (sink cancelled_canvas)
			~error_frame: (fun exc_list -> sink (error_frame exc_list))
			~params: (Eliom_parameters.int "a")
	v}


	Finally, we create the first step of the wizard.  Note that the first step
	does not take any POST parameters, and does not require the provision of
	special frames for error and cancellation situations.

	{v
	let step1 =
		let step1_contents ~sp ~bp ~gp ~pp ~carry enter_a =
			[
			fieldset ~a:[a_class ["form_fields"]]
				[
				label ~a:[a_for "enter_a"] [pcdata "Enter number 'A':"];
				Eliom_predefmod.Xhtml.int_input ~a:[a_id "enter_a"] ~input_type:`Text ~name:enter_a ();
				]
			]
		in Litiom_wizard.Steps.make_first
			~fallback
			~tree_builder: (standard_handler ~page_title:"Step 1/3")
			~carrier: Litiom_wizard.Carriers.carry_none
			~contents: step1_contents
			~next_step_register: step2
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
    val none : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of unit ]
    val past : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of 'a ]
    val present : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of 'd ]
    val all : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of 'a * 'd ]
  end


(********************************************************************************)
(**	{3 Steps module}							*)
(********************************************************************************)

module Steps:
sig
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
      normal_content:(carried:'d ->
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
      carrier:(carried:'d ->
               Eliom_sessions.server_params ->
               'e -> 'f -> [< `Continue of 'g | `Fail ]) ->
      form_maker:(carried:'d ->
                  carry:'g -> 'h -> Xhtmltypes.form_content XHTML.M.elt list) ->
      normal_content:(carried:'d ->
                      carry:'g ->
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
      carrier:(carried:'d ->
               Eliom_sessions.server_params ->
               'e -> 'f -> [< `Continue of 'g | `Fail | `Skip of 'h ]) ->
      form_maker:(carried:'d ->
                  carry:'g -> 'i -> Xhtmltypes.form_content XHTML.M.elt list) ->
      normal_content:(carried:'d ->
                      carry:'g ->
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
      carrier:(carried:unit ->
               Eliom_sessions.server_params ->
               'a -> unit -> [< `Continue of 'e | `Fail ]) ->
      form_maker:(carried:unit ->
                  carry:'e -> 'f -> Xhtmltypes.form_content XHTML.M.elt list) ->
      normal_content:(carried:unit ->
                      carry:'e ->
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
      carrier:(carried:unit ->
               Eliom_sessions.server_params ->
               'a -> 'f -> [< `Continue of 'g | `Fail ]) ->
      form_maker:(carried:unit ->
                  carry:'g -> 'h -> Xhtmltypes.form_content XHTML.M.elt list) ->
      normal_content:(carried:unit ->
                      carry:'g ->
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

