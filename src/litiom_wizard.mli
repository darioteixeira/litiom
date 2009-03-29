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
module Carriers :
  sig
    val none : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of unit ]
    val past_only : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of 'a ]
    val present_only : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of 'd ]
    val all : carried:'a -> 'b -> 'c -> 'd -> [> `Continue of 'a * 'd ]
  end
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
      common:'a * 'b * 'c ->
      ?cancelled_content:'b -> ?error_content:'c -> unit -> 'a * 'b * 'c
    val make_last :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
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
    val make_middle :
      common:('a, unit,
              [ `Attached of
                  [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
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
                  [ `Internal of [< `Coservice | `Service ] * [ `Get ] ]
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
      common:('a, unit, [< Eliom_services.internal_service_kind ],
              [< Eliom_services.suff ], 'b, 'c, [ `Registrable ])
             Eliom_services.service * 'd *
             (Eliom_sessions.server_params ->
              'e list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      carrier:(carried:unit ->
               Eliom_sessions.server_params ->
               'a -> unit -> [< `Continue of 'f | `Fail ]) ->
      form_maker:(carried:unit ->
                  carry:'f -> 'g -> Xhtmltypes.form_content XHTML.M.elt list) ->
      normal_content:(carried:unit ->
                      carry:'f ->
                      form:[> Xhtmltypes.form ] XHTML.M.elt ->
                      Eliom_sessions.server_params ->
                      'a -> unit -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      ?error_content:(Eliom_sessions.server_params ->
                      'e list -> Eliom_predefmod.Xhtml.page Lwt.t) ->
      next:('h ->
            'f ->
            Eliom_sessions.server_params ->
            ('a, 'i, [< Eliom_services.post_service_kind ],
             [< Eliom_services.suff ], 'j,
             'g *
             [< Submit.t Eliom_parameters.setoneradio ]
             Eliom_parameters.param_name, [< Eliom_services.registrable ])
            Eliom_services.service) *
           'h ->
      unit -> unit
  end
