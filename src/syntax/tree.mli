(** Token trivia for formatting *)
type trivia = Token.token list

(** Modifier flags *)
type modifier_set = {
    exportness : bool
  ; externness : bool
  ; unsafeness : bool
  ; constness : bool
  ; asyncness : bool
}

(** Type reference *)
type typ_ref = int

(** Symbol reference *)
type symbol_ref = int

(** Type constraint *)
type constraint_ = {
    trait : Musi_shared.Interner.symbol
  ; args : typ list
  ; span : Musi_shared.Span.t
}

(** Type node kinds *)
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

(** Type AST node *)
and typ = {
    kind : typ_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; modifiers : modifier_set
}

(** Pattern node kinds *)
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
  | Choice of { variant : Musi_shared.Interner.symbol; pat : pat option }
  | Or of { pats : pat list }
  | Rest of { name : Musi_shared.Interner.symbol option }
  | Error

(** Pattern AST node *)
and pat = {
    kind : pat_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable typ : typ_ref option
}

(** Template part *)
and template_part =
  | Text of { value : Musi_shared.Interner.symbol }
  | Expr of { expr : expr }

(** Function parameter *)
and param = {
    name : Musi_shared.Interner.symbol
  ; typ : typ
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Expression node kinds *)
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

(** Expression AST node *)
and expr = {
    kind : expr_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable typ : typ_ref option
  ; mutable sym : symbol_ref option
}

(** Match case *)
and match_case = {
    pat : pat
  ; guard : expr option
  ; body : expr
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Statement node kinds *)
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

(** Statement AST node *)
and stmt = {
    kind : stmt_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; mutable sym : symbol_ref option
}

(** Record field definition *)
and record_field = {
    name : Musi_shared.Interner.symbol
  ; typ : typ
  ; default_value : expr option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Choice variant case *)
and choice_case = {
    name : Musi_shared.Interner.symbol
  ; typ : typ option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Trait method definition *)
and trait_method = {
    name : Musi_shared.Interner.symbol
  ; params : param list
  ; ret_typ : typ option
  ; body : stmt list option
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Import item *)
and import_item = {
    name : Musi_shared.Interner.symbol
  ; alias : Musi_shared.Interner.symbol option
  ; span : Musi_shared.Span.t
}

(** Declaration node kinds *)
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
  | Import of {
        path : Musi_shared.Interner.symbol
      ; items : import_item list option
    }
  | Export of { decl : decl }
  | Extern of { abi : Musi_shared.Interner.symbol option; decls : decl list }
  | Error

(** Declaration AST node *)
and decl = {
    kind : decl_kind
  ; span : Musi_shared.Span.t
  ; leading : trivia
  ; trailing : trivia
  ; modifiers : modifier_set
  ; mutable sym : symbol_ref option
}

(** Complete program *)
type program = decl list
