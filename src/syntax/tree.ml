type trivia = Token.token list
type typ_ref = int
type symbol_ref = int

type modifiers = {
    exported : bool
  ; constness : bool
  ; unsafeness : bool
  ; asyncness : bool
  ; externness : bool * Musi_shared.Interner.symbol option
}

let default_modifiers =
  {
    exported = false
  ; constness = false
  ; unsafeness = false
  ; asyncness = false
  ; externness = (false, None)
  }

type typ_kind =
  | Named of { name : Musi_shared.Interner.symbol }
  | Func of { param_typs : typ list; ret_typ : typ }
  | Optional of { inner_typ : typ }
  | Fallible of { pass_typ : typ; fail_typ : typ option }
  | Array of { elem_typ : typ }
  | ArrayRepeat of { elem_typ : typ; count_typ : typ }
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
  | IntLit of { value : string; suffix : Token.suffix option }
  | BinLit of { value : string; suffix : Token.suffix option }
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
  | ArrayRepeat of { elem : expr; count : expr }
  | Tuple of { elems : expr list }
  | RecordLiteral of { fields : (Musi_shared.Interner.symbol * expr) list }
  | RecordExpr of { fields : field list; methods : stmt list }
  | ChoiceExpr of { cases : choice_case list; methods : stmt list }
  | TraitExpr of { methods : stmt list }
  | FuncExpr of {
        params : param list
      ; ret_typ : typ option
      ; body : stmt list option
    }
  | Bind of { mutable_ : bool; pat : pat; typ : typ option; init : expr }
  | Assign of { lhs : expr; rhs : expr }
  | Return of { value : expr option }
  | Break of { value : expr option }
  | Continue
  | While of { cond : expr; body : stmt list }
  | For of { pat : pat; iter : expr; body : stmt list }
  | Field of { receiver : expr; field : Musi_shared.Interner.symbol }
  | Index of { receiver : expr; index : expr }
  | Try of { expr : expr }
  | Defer of { expr : expr }
  | Range of { start : expr; end_ : expr; inclusive : bool }
  | Async of { expr : expr }
  | Await of { expr : expr }
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

and stmt_kind = ExprStmt of { expr : expr } | Error

and stmt = {
    kind : stmt_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; decorators : decorator list
  ; modifiers : modifiers
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

and choice_case = {
    name : Musi_shared.Interner.symbol
  ; typ : typ option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

and decorator = {
    name : Musi_shared.Interner.symbol
  ; args : expr list
  ; span : Musi_shared.Span.t
}

and constraint_ = {
    intf : Musi_shared.Interner.symbol
  ; args : typ list
  ; span : Musi_shared.Span.t
}

and import_export_item = {
    name : Musi_shared.Interner.symbol
  ; alias : Musi_shared.Interner.symbol option
  ; span : Musi_shared.Span.t
}

and import_export_kind =
  | Namespace of { alias : Musi_shared.Interner.symbol }
  | Named of { items : import_export_item list }
  | ReExport of {
        path : Musi_shared.Interner.symbol
      ; items : import_export_item list option
    }

and decl_kind =
  | Import of { path : Musi_shared.Interner.symbol; kind : import_export_kind }
  | Export of {
        path : Musi_shared.Interner.symbol option
      ; kind : import_export_kind
    }
  | Alias of { name : Musi_shared.Interner.symbol; typ : typ }
  | Error

and decl = {
    kind : decl_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; decorators : decorator list
  ; modifiers : modifiers
  ; mutable sym : symbol_ref option
}

type program = decl list
