type trivia = Token.token list
type ty_ref = int
type symbol_ref = int

type modifiers = {
    exported : bool
  ; weakness : bool
  ; constness : bool
  ; unsafeness : bool
  ; asyncness : bool
  ; externness : bool * Interner.symbol option
}

val default_modifiers : modifiers

type ty_kind =
  | Named of { name : Interner.symbol }
  | Proc of { param_tys : ty list; ret_ty : ty }
  | Optional of { inner_ty : ty }
  | Fallible of { pass_ty : ty; fail_ty : ty option }
  | Array of { elem_ty : ty }
  | ArrayRepeat of { elem_ty : ty; count_ty : ty }
  | Tuple of { elem_tys : ty list }
  | Generic of { name : Interner.symbol; args : ty list }
  | Where of { ty : ty; constraints : constraint_ list }
  | Ptr of { inner : ty }
  | Ref of { inner : ty }
  | Infer
  | Error

and ty = {
    kind : ty_kind
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and pat_kind =
  | Wildcard
  | Ident of { name : Interner.symbol }
  | IntLit of { value : string }
  | BoolLit of { value : bool }
  | TextLit of { value : Interner.symbol }
  | Tuple of { pats : pat list }
  | Record of { fields : (Interner.symbol * pat) list }
  | Array of { pats : pat list }
  | Range of { start : pat; end_ : pat; inclusive : bool }
  | Choice of { member : Interner.symbol; pat : pat option }
  | Or of { pats : pat list }
  | Rest of { name : Interner.symbol option }
  | ValueBinding of { name : Interner.symbol; pat : pat }
  | Error

and pat = {
    kind : pat_kind
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable ty : ty_ref option
}

and template_part =
  | Text of { value : Interner.symbol }
  | Expr of { expr : expr }

and param = {
    name : Interner.symbol
  ; ty : ty
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and expr_kind =
  | IntLit of { value : string; suffix : Token.suffix option }
  | BinLit of { value : string; suffix : Token.suffix option }
  | TextLit of { value : Interner.symbol }
  | BoolLit of { value : bool }
  | UnitLit
  | Ident of { name : Interner.symbol }
  | Binary of { op : Token.t; lhs : expr; rhs : expr }
  | Unary of { op : Token.t; operand : expr }
  | Call of { callee : expr; args : expr list }
  | If of { cond : expr; then_br : expr; else_br : expr option }
  | Match of { expr : expr; cases : match_case list }
  | Block of { stmts : stmt list }
  | Array of { elems : expr list }
  | ArrayRepeat of { elem : expr; count : expr }
  | Tuple of { elems : expr list }
  | RecordLit of { fields : (Interner.symbol * expr) list }
  | Record of { fields : field list; methods : stmt list }
  | Choice of { cases : choice_case list; methods : stmt list }
  | Interface of { methods : stmt list }
  | Proc of { params : param list; ret_ty : ty option; body : stmt list option }
  | Bind of { mutable_ : bool; pat : pat; ty : ty option; init : expr }
  | Assign of { lhs : expr; rhs : expr }
  | Return of { value : expr option }
  | Break of { value : expr option }
  | Continue
  | While of { cond : expr; body : stmt list }
  | For of { pat : pat; iter : expr; body : stmt list }
  | Field of { receiver : expr; field : Interner.symbol }
  | Index of { receiver : expr; index : expr }
  | Try of { expr : expr }
  | Defer of { expr : expr }
  | Range of { start : expr; end_ : expr; inclusive : bool }
  | Async of { expr : expr }
  | Await of { expr : expr }
  | Cast of { expr : expr; ty : ty }
  | Test of { expr : expr; ty : ty }
  | Template of { parts : template_part list }
  | Error

and expr = {
    kind : expr_kind
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable ty : ty_ref option
  ; mutable sym : symbol_ref option
}

and match_case = {
    pat : pat
  ; guard : expr option
  ; body : expr
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and stmt_kind =
  | Expr of { expr : expr }
  | Import of { path : Interner.symbol; kind : import_export_kind }
  | Export of {
        path : Interner.symbol option
      ; kind : import_export_kind
    }
  | Alias of { name : Interner.symbol; ty : ty }
  | Error

and stmt = {
    kind : stmt_kind
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
  ; decorators : decorator list
  ; modifiers : modifiers
  ; mutable sym : symbol_ref option
}

and field = {
    name : Interner.symbol
  ; ty : ty
  ; default_value : expr option
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and choice_case = {
    name : Interner.symbol
  ; ty : ty option
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and decorator = {
    name : Interner.symbol
  ; args : expr list
  ; span : Span.t
}

and constraint_ = {
    intf : Interner.symbol
  ; args : ty list
  ; span : Span.t
}

and import_export_item = {
    name : Interner.symbol
  ; alias : Interner.symbol option
  ; span : Span.t
}

and import_export_kind =
  | Namespace of { alias : Interner.symbol }
  | Named of { items : import_export_item list }
  | ReExport of {
        path : Interner.symbol
      ; items : import_export_item list option
    }

type program = stmt list
