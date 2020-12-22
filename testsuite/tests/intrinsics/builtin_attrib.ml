external test : unit -> int64 = "test" "test"
[@@noalloc] [@@builtin] [@@only_generative_effects] [@@no_coeffects]

external test2 : unit -> int64 = "test" "test"
[@@builtin] [@@only_generative_effects] [@@no_coeffects]

external test3 : unit -> int64 = "test" "test"
[@@builtin] [@@only_generative_effects] [@@no_effects]
