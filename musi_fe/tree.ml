type trivia = Token.token list
type ty_ref = int
type symbol_ref = int
type abi = Interner.symbol
type 'a separated = { items : 'a list; separators : Span.t list }

type 'a delimited = {
    open_span : Span.t
  ; items : 'a list
  ; separators : Span.t list
  ; close_span : Span.t
}

type node_kind =
  | ExprIntLit of { value : string; suffix : Token.suffix option }
  | ExprBinLit of { value : string; suffix : Token.suffix option }
  | ExprTextLit of { value : Interner.symbol }
  | ExprBoolLit of { value : bool }
  | ExprUnitLit
  | ExprIdent of { name : Interner.symbol }
  | ExprBinary of { op : Token.t; lhs : node; rhs : node }
  | ExprUnary of { op : Token.t; operand : node }
  | ExprAssign of { lhs : node; rhs : node }
  | ExprCall of { callee : node; args : node delimited }
  | ExprField of { receiver : node; field : Interner.symbol }
  | ExprIndex of { receiver : node; index : node }
  | ExprIf of { cond : node; then_br : node; else_br : node option }
  | ExprMatch of { scrutinee : node; cases : match_case separated }
  | ExprWhile of { cond : node; body : node }
  | ExprFor of { pat : node; iterable : node; body : node }
  | ExprBlock of { body : node separated; unsafe_ : bool; asyncness : bool }
  | ExprReturn of { value : node option }
  | ExprBreak of { value : node option }
  | ExprContinue
  | ExprArray of { items : node delimited }
  | ExprArrayList of { item : node; count : node }
  | ExprTuple of { items : node delimited }
  | ExprRecordLit of { fields : record_field delimited }
  | ExprProc of {
        params : param delimited
      ; ret_ty : ty option
      ; body : node option
      ; asyncness : bool
      ; unsafe_ : bool
      ; external_ : abi option
    }
  | ExprRecord of {
        fields : record_field_def delimited
      ; methods : node list
      ; unsafe_ : bool
    }
  | ExprChoice of { variants : choice_variant delimited; methods : node list }
  | ExprInterface of { signatures : node delimited; unsafe_ : bool }
  | ExprBind of {
        mutable_ : bool
      ; weakness : bool
      ; pat : node
      ; ty : ty option
      ; init : node
    }
  | ExprTry of { inner : node }
  | ExprDefer of { inner : node }
  | ExprRange of { start : node; end_ : node; inclusive : bool }
  | ExprCast of { inner : node; target : ty }
  | ExprTest of { inner : node; target : ty }
  | ExprImport of { source : Interner.symbol; kind : import_export_kind }
  | ExprExport of { source : Interner.symbol option; kind : import_export_kind }
  | PatWildcard
  | PatBind of { inner : node }
  | PatOr of { alts : node separated }
  | PatRest of { name : Interner.symbol option }
  | PatExpr of { inner : node }
  | Error

and node = {
    kind : node_kind
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
  ; decorators : decorator list
  ; exported : bool
  ; mutable ty : ty_ref option
  ; mutable sym : symbol_ref option
}

and ty_kind =
  | TyNamed of { name : Interner.symbol }
  | TyProc of { params : ty list; ret : ty }
  | TyOptional of { inner : ty }
  | TyFallible of { ok : ty; err : ty option }
  | TyArray of { item : ty }
  | TyArrayList of { item : ty; count : node }
  | TyTuple of { items : ty list }
  | TyPtr of { inner : ty }
  | TyRef of { inner : ty }
  | TyInfer
  | TyError

and ty = { kind : ty_kind; span : Span.t; leading : trivia; trailing : trivia }

and param = {
    name : Interner.symbol
  ; ty : ty
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and match_case = {
    pat : node
  ; guard : node option
  ; body : node
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and decorator = { name : Interner.symbol; args : node list; span : Span.t }

and record_field = {
    name : Interner.symbol
  ; value : node
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and record_field_def = {
    name : Interner.symbol
  ; ty : ty
  ; default : node option
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and choice_variant = {
    name : Interner.symbol
  ; payload : ty option
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

and import_export_item = {
    name : Interner.symbol
  ; alias : Interner.symbol option
  ; span : Span.t
}

and import_export_kind =
  | Namespace of { alias : Interner.symbol }
  | Named of { items : import_export_item list }

type program = node list
