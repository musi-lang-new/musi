type trivia = Token.token list

type modifier_set = {
    exportness : bool
  ; externness : bool
  ; unsafeness : bool
  ; constness : bool
  ; asyncness : bool
}

let empty_modifier_set =
  {
    exportness = false
  ; externness = false
  ; unsafeness = false
  ; constness = false
  ; asyncness = false
  }

type typ_ref = int
type symbol_ref = int

type constraint_ = {
    trait : Musi_shared.Interner.symbol
  ; args : typ list
  ; span : Musi_shared.Span.t
}

and typ_kind =
  | Named of { name : Musi_shared.Interner.symbol }
  | Func of { param_typs : typ list; ret_typ : typ }
  | Optional of { inner_typ : typ }
  | Fallible of { pass_typ : typ; fail_typ : typ option }
  | Array of { elem_typ : typ }
  | Tuple of { elem_typs : typ list }
  | Generic of { name : Musi_shared.Interner.symbol; args : typ list }
  | Where of { typ : typ; constraints : constraint_ list }
  | Infer
  | Error

and typ = {
    kind : typ_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; modifiers : modifier_set
}

and pat_kind =
  | Wildcard
  | Ident of { name : Musi_shared.Interner.symbol }
  | IntLit of { value : string }
  | BoolLit of { value : bool }
  | TextLit of { value : Musi_shared.Interner.symbol }
  | Tuple of { pats : pat list }
  | Record of { fields : (Musi_shared.Interner.symbol * pat) list }
  | Array of { pats : pat list }
  | Range of { start : pat; end_ : pat; inclusive : bool }
  | Choice of { member : Musi_shared.Interner.symbol; pat : pat option }
  | Or of { pats : pat list }
  | Rest of { name : Musi_shared.Interner.symbol option }
  | Error

and pat = {
    kind : pat_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable typ : typ_ref option
}

and template_part =
  | Text of { value : Musi_shared.Interner.symbol }
  | Expr of { expr : expr }

and param = {
    name : Musi_shared.Interner.symbol
  ; typ : typ
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
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
  | Range of { start : expr; end_ : expr; inclusive : bool }
  | Async of { expr : expr }
  | Await of { expr : expr }
  | Closure of { params : param list; ret_typ : typ option; body : expr }
  | Cast of { expr : expr; typ : typ }
  | Test of { expr : expr; typ : typ }
  | Template of { parts : template_part list }
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

and stmt_kind =
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
  | Defer of { stmt : stmt }
  | Unsafe of { stmts : stmt list }
  | Error

and stmt = {
    kind : stmt_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable sym : symbol_ref option
}

and field = {
    name : Musi_shared.Interner.symbol
  ; typ : typ
  ; default_value : expr option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and choice_member = {
    name : Musi_shared.Interner.symbol
  ; typ : typ option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and import_item = {
    name : Musi_shared.Interner.symbol
  ; alias : Musi_shared.Interner.symbol option
  ; span : Musi_shared.Span.t
}

and decl_kind =
  | Func of {
        name : Musi_shared.Interner.symbol
      ; params : param list
      ; ret_typ : typ option
      ; body : stmt list option
    }
  | Record of { name : Musi_shared.Interner.symbol; fields : field list }
  | Choice of {
        name : Musi_shared.Interner.symbol
      ; members : choice_member list
    }
  | Trait of { name : Musi_shared.Interner.symbol; methods : decl list }
  | Alias of { name : Musi_shared.Interner.symbol; typ : typ }
  | Import of {
        path : Musi_shared.Interner.symbol
      ; items : import_item list option
    }
  | Export of { decl : decl }
  | Extern of { abi : Musi_shared.Interner.symbol option; decls : decl list }
  | Error

and decl = {
    kind : decl_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; modifiers : modifier_set
  ; mutable sym : symbol_ref option
}

type program = decl list
