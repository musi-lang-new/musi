(** Abstract syntax tree for Musi.

    Expression-oriented design where everything is a node. No separate statement
    type - declarations, control flow, and expressions all use the unified [node]
    type with [node_kind] variants. *)

(** Trivia tokens (whitespace, comments) attached to nodes. *)
type trivia = Token.token list

(** Type reference for semantic analysis. *)
type ty_ref = int

(** Symbol reference for name resolution. *)
type symbol_ref = int

(** ABI string for extern procedures (e.g., ["C"], ["system"]). *)
type abi = Interner.symbol

(** Comma/semicolon-separated list with separator spans. *)
type 'a separated = {
    items : 'a list
  ; separators : Span.t list  (** Spans of separator tokens *)
}

(** Delimited list with open/close bracket spans. *)
type 'a delimited = {
    open_span : Span.t  (** Opening bracket/paren *)
  ; items : 'a list
  ; separators : Span.t list  (** Spans of separator tokens *)
  ; close_span : Span.t  (** Closing bracket/paren *)
}

(** Node kinds: expressions, patterns, and error nodes. *)
type node_kind =
  (* Expr: Literals *)
  | ExprIntLit of { value : string; suffix : Token.suffix option }
  | ExprBinLit of { value : string; suffix : Token.suffix option }
  | ExprTextLit of { value : Interner.symbol }
  | ExprBoolLit of { value : bool }
  | ExprUnitLit
  | ExprIdent of { name : Interner.symbol }
  (* Expr: Operators *)
  | ExprBinary of { op : Token.t; lhs : node; rhs : node }
  | ExprUnary of { op : Token.t; operand : node }
  | ExprAssign of { lhs : node; rhs : node }
  (* Expr: Calls & Access *)
  | ExprCall of { callee : node; args : node delimited }
  | ExprField of { receiver : node; field : Interner.symbol }
  | ExprIndex of { receiver : node; index : node }
  (* Expr: Control Flow *)
  | ExprIf of { cond : node; then_br : node; else_br : node option }
  | ExprMatch of { scrutinee : node; cases : match_case separated }
  | ExprWhile of { cond : node; body : node }
  | ExprFor of { pat : node; iterable : node; body : node }
  | ExprBlock of { body : node separated; unsafe_ : bool; asyncness : bool }
  | ExprReturn of { value : node option }
  | ExprBreak of { value : node option }
  | ExprContinue
  (* Expr: Compound *)
  | ExprArray of { items : node delimited }
  | ExprArrayList of { item : node; count : node }
  | ExprTuple of { items : node delimited }
  | ExprRecordLit of { fields : record_field delimited }
  (* Expr: Definitions *)
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
  | ExprBinding of {
        mutable_ : bool
      ; weakness : bool
      ; pat : node
      ; ty : ty option
      ; init : node
    }
  (* Expr: Misc *)
  | ExprTry of { inner : node }
  | ExprDefer of { inner : node }
  | ExprRange of { start : node; end_ : node; inclusive : bool }
  | ExprCast of { inner : node; target : ty }
  | ExprTest of { inner : node; target : ty }
  | ExprImport of { source : Interner.symbol; kind : import_export_kind }
  | ExprExport of { source : Interner.symbol option; kind : import_export_kind }
  (* Pat: Patterns *)
  | PatWildcard
  | PatBind of { inner : node }
  | PatOr of { alts : node separated }
  | PatRest of { name : Interner.symbol option }
  | PatExpr of { inner : node }
  (* Error *)
  | Error

(** Unified AST node for expressions, patterns, and statements. *)
and node = {
    kind : node_kind
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
  ; decorators : decorator list
  ; exported : bool
  ; mutable ty : ty_ref option  (** Filled by type checker *)
  ; mutable sym : symbol_ref option  (** Filled by name resolver *)
}

(** Type annotation kinds. *)
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

(** Type annotation node. *)
and ty = { kind : ty_kind; span : Span.t; leading : trivia; trailing : trivia }

(** Procedure parameter. *)
and param = {
    name : Interner.symbol
  ; ty : ty
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Match case with pattern, optional guard, and body. *)
and match_case = {
    pat : node
  ; guard : node option
  ; body : node
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Decorator/attribute (e.g., [@inline], [@deprecated]). *)
and decorator = { name : Interner.symbol; args : node list; span : Span.t }

(** Record literal field ([name := value]). *)
and record_field = {
    name : Interner.symbol
  ; value : node
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Record definition field ([name: Type := default]). *)
and record_field_def = {
    name : Interner.symbol
  ; ty : ty
  ; default : node option
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Choice/enum variant ([case Name(Payload)]). *)
and choice_variant = {
    name : Interner.symbol
  ; payload : ty option
  ; span : Span.t
  ; leading : trivia
  ; trailing : trivia
}

(** Import/export item with optional alias. *)
and import_export_item = {
    name : Interner.symbol
  ; alias : Interner.symbol option
  ; span : Span.t
}

(** Import/export kind (namespace or named items). *)
and import_export_kind =
  | Namespace of { alias : Interner.symbol }
  | Named of { items : import_export_item list }

(** Top-level program (list of nodes). *)
type program = node list
