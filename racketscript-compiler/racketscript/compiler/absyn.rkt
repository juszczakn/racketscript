#lang typed/racket/base

(require "language.rkt"
         "ident.rkt")

(provide (all-defined-out))

(struct Module  ([id      : Symbol]
                 [path    : Path]
                 [lang    : (U Symbol String (Listof Symbol))]
                 [imports : (Setof (U Path Symbol))]
                 [quoted-bindings : (Setof Symbol)]
                 [forms   : (Listof ModuleLevelForm)])
  #:transparent)

(define-language Absyn
  #:alias
  [Program      TopLevelForm]
  [ModuleName   (U Symbol Path)]
  [IdentName    Symbol]

  #:forms
  ;; Top Level Forms

  [TopLevelForm             GeneralTopLevelForm
                            Expr
                            Module
                            Begin]

  [GeneralTopLevelForm      Expr
                            (DefineValues [ids : TopLevelDefineIds] [expr : Expr])
                            ;; DefineSyntaxes

                            ;; Same as ILRequire, but can't use to
                            ;; avoid cyclic depnedency.
                            (JSRequire [alias : TopLevelIdent]
                                       [path : (U Symbol Path-String)]
                                       [mode : (U 'default '*)])
                            #;Require*]

  ;; Module Level Forms
  [Provide*            (Listof Provide)]
  [Provide             (SimpleProvide     [id       : TopLevelIdent])
                       (RenamedProvide    [local-id : TopLevelIdent]
                                          [exported-id : IdentName])
                       (AllDefined        [exclude : (Setof TopLevelIdent)])
                       (PrefixAllDefined  [prefix-id : IdentName]
                                          [exclude : (Setof TopLevelIdent)])]
  [ModuleLevelForm     GeneralTopLevelForm
                       Provide*
                       SubModuleForm]

  [SubModuleForm       Module]

  ;; Expressions

  [Expr    Ident
           (TopId             [id       : IdentName])
           (VarRef            [var      : (Option (U IdentName TopId))])
           (Quote             [datum    : Any])


           Begin
           (Begin0            [expr0    : Expr]
                              [expr*    : (Listof Expr)])

           (PlainApp          [lam      : Expr]    [args  : (Listof Expr)])
           (PlainLambda       [formals  : Formals] [exprs : (Listof Expr)])
           (CaseLambda        [clauses  : (Listof PlainLambda)])


           (If                [pred     : Expr]
                              [t-branch : Expr]
                              [f-branch : Expr])

           ;; This also acts as LetRecValues because Absyn is
           ;; freshened.
           (LetValues         [bindings : (Listof LocalBindingPair)]
                              [body     : (Listof Expr)])
           (Set!              [id       : LocalIdent] [expr : Expr])

           (WithContinuationMark   [key    : Expr]
                                   [value  : Expr]
                                   [result : Expr])]

  [Ident  LocalIdent
          ImportedIdent
          TopLevelIdent]

  [Begin   (Listof TopLevelForm)]

  ;; Bindings and Formal Arguments

  [LocalBindingPair      (Pairof Args Expr)]
  [TopLevelDefineIds     (Listof TopLevelIdent)]

  [Args         (Listof LocalIdent)]

  [Formals      LocalIdent
                (Listof LocalIdent)
                (Pairof (Listof LocalIdent) LocalIdent)])

(struct TopLevelIdent  ([id : IdentName]) #:transparent)
(struct LocalIdent     ([id : IdentName]))
(struct ImportedIdent
  ([id : IdentName]
   [src-mod : Module-Path]
   [reachable? : Boolean])
  #:transparent)

(: lambda-arity (-> PlainLambda (U Natural arity-at-least)))
(define (lambda-arity f)
  (define frmls (PlainLambda-formals f))
  (cond
    [(LocalIdent? frmls) (arity-at-least 0)]
    [(list? frmls) (length frmls)]
    [(pair? frmls) (arity-at-least (length (car frmls)))]))


(: fresh-id (-> Symbol LocalIdent))
(define (fresh-id sym)
  (LocalIdent (fresh-id-symbol sym)))
