val init_typing_env: unit -> Env.t

val type_check: Env.t -> Ast.decl list -> Env.t
