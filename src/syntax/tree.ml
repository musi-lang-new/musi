type trivia = Token.token list

type typ_kind =
  | Named of { name : Musi_shared.Interner.symbol }
  | Func of { param_typs : typ list; ret_typ : typ }
  | Optional of { inner_typ : typ }
  | Fallible of { pass_typ : typ; fail_typ : typ option }
  | Array of { elem_typ : typ }
  | Tuple of { elem_typs : typ list }
  | Error

and typ = {
    kind : typ_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

type pat_kind =
  | Wildcard
  | Ident of { name : Musi_shared.Interner.symbol }
  | IntLit of { value : string }
  | BoolLit of { value : bool }
  | TextLit of { value : Musi_shared.Interner.symbol }
  | Tuple of { pats : pat list }
  | Record of { fields : (Musi_shared.Interner.symbol * pat) list }
  | Error

and pat = {
    kind : pat_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable typ : typ_ref option
}

and typ_ref = int
and symbol_ref = int

type stmt_kind =
  | Expr of { expr : expr }
  | Decl of { decl : decl }
  | Bind of {
        mutable_ : bool
      ; name : Musi_shared.Interner.symbol
      ; typ : typ option
      ; init : expr
    }
  | Assign of { name : Musi_shared.Interner.symbol; value : expr }
  | Return of { value : expr option }
  | Break of { value : expr option }
  | Continue
  | While of { cond : expr; body : stmt list }
  | For of { pat : pat; iter : expr; body : stmt list }
  | Error

and stmt = {
    kind : stmt_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable sym : symbol_ref option
}

and expr_kind =
  | IntLit of { value : string }
  | BinLit of { value : string }
  | TextLit of { value : Musi_shared.Interner.symbol }
  | BoolLit of { value : bool }
  | UnitLit
  | Ident of { name : Musi_shared.Interner.symbol }
  | Binary of { op : Token.t; lhs : expr; rhs : expr }
  | Unary of { op : Token.t; operand : expr }
  | Call of { callee : expr; args : expr list }
  | If of { cond : expr; then_br : expr; else_br : expr option }
  | Match of { expr : expr; cases : match_case list }
  | Block of { stmts : stmt list }
  | Array of { elems : expr list }
  | Tuple of { elems : expr list }
  | Record of { fields : (Musi_shared.Interner.symbol * expr) list }
  | FieldAccess of { receiver : expr; field : Musi_shared.Interner.symbol }
  | IndexAccess of { receiver : expr; index : expr }
  | Try of { expr : expr }
  | Defer of { expr : expr }
  | Error

and expr = {
    kind : expr_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable typ : typ_ref option
  ; mutable sym : symbol_ref option
}

and match_case = {
    pat : pat
  ; guard : expr option
  ; body : expr
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and param = {
    name : Musi_shared.Interner.symbol
  ; typ : typ
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and record_field = {
    name : Musi_shared.Interner.symbol
  ; typ : typ
  ; default_value : expr option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and choice_case = {
    name : Musi_shared.Interner.symbol
  ; typ : typ option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and trait_method = {
    name : Musi_shared.Interner.symbol
  ; params : param list
  ; ret_typ : typ option
  ; body : stmt list option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and decl_kind =
  | Func of {
        name : Musi_shared.Interner.symbol
      ; params : param list
      ; ret_typ : typ option
      ; body : stmt list
    }
  | Record of { name : Musi_shared.Interner.symbol; fields : record_field list }
  | Choice of { name : Musi_shared.Interner.symbol; cases : choice_case list }
  | Trait of { name : Musi_shared.Interner.symbol; methods : trait_method list }
  | Alias of { name : Musi_shared.Interner.symbol; typ : typ }
  | Error

and decl = {
    kind : decl_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable sym : symbol_ref option
}

type program = decl list
