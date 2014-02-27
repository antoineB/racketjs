#langf typed/racket

(provide
 print-js-ast
 (struct-out VariableAccess)
 (struct-out ArrayCall)
 (struct-out Null)
 (struct-out Undefined)
 (struct-out VariableDcl)
 (struct-out Assign)
 (struct-out IfStmt)
 (struct-out WhileStmt)
 (struct-out ForStmt)
 (struct-out FunctionExpr)
 (struct-out FunctionCall)
 (struct-out ArrayExpr)
 (struct-out ObjectExpr)
 (struct-out TypeOfExpr)
 (struct-out InstanceOfExpr)
 (struct-out Binary)
 (struct-out Unary)
 (struct-out Postfix)
 (struct-out Infix)
 (struct-out Literal)
 (struct-out Empty)
 (struct-out ConditionalOp)
 (struct-out FieldAccess)
 ;;Try
 ;;Catch
 (struct-out New)
 (struct-out Return))

(define-type JsExpr
  (U
   FieldAccess
   VariableAccess
   Null
   Undefined
   Assign
   FunctionExpr
   FunctionCall
   ArrayExpr
   ObjectExpr
   TypeOfExpr
   InstanceOfExpr
   Binary
   Unary
   Postfix
   Infix
   Literal
   New
   ArrayCall
   ConditionalOp
   Empty))
  
(define-type JsStmt
  (U
   VariableDcl
   IfStmt
   WhileStmt
   ForStmt
   ;;Try
   ;;Catch
   Return))

(define-type JsAst
  (U
   JsExpr
   JsStmt))

(struct: Literal ([value : (U String Char Number Boolean)]) #:transparent)
(struct: VariableDcl ([name : String] [right : JsExpr]) #:transparent)
(struct: Assign ([left : JsExpr] [right : JsExpr]) #:transparent)
(struct: IfStmt ([test : JsExpr] [then : (Listof JsAst)] [else : (Listof JsAst)]))
(struct: WhileStmt ([test : JsExpr] [body : (Listof JsAst)]))
(struct: ForStmt ([init : JsExpr] [test : JsExpr] [step : JsExpr] [body : (Listof JsAst)]))
(struct: FunctionExpr ([args : (Listof String)] [body : (Listof JsAst)]) #:transparent)
(struct: FunctionCall ([expr : JsExpr] [args : (Listof JsExpr)]) #:transparent)
(struct: ArrayExpr ([exprs : (Listof JsExpr)]))
(struct: ArrayCall ([expr : JsExpr] [indice : JsExpr]))
(struct: ObjectExpr ([pairs  : (Listof (Pairof String JsExpr))]))
(struct: Empty ())
(struct: ConditionalOp ([test : JsExpr] [then : JsExpr] [else : JsExpr]))

(struct: FieldAccess ([expr : JsExpr] [name : String]))

(struct: New ([expr : JsExpr]) #:transparent)
(struct: Return ([expr : JsExpr]) #:transparent)
;; (struct: Try ())
;; (struct: Catch ())

(struct: VariableAccess ([names : (Listof String)]) #:transparent)

(struct: Null ())
(struct: Undefined ())
(struct: TypeOfExpr ([expr : JsExpr]))
(struct: InstanceOfExpr ([left : JsExpr] [right : JsExpr]))
(struct: Binary ([op : Symbol] [left : JsExpr] [right : JsExpr]) #:transparent)
(struct: Unary ([op : Symbol] [expr : JsExpr]))
(struct: Postfix ([op : Symbol] [expr : JsExpr]))
(struct: Infix ([op : Symbol] [expr : JsExpr]))

(: with-paren (String * -> String))
(define (with-paren . expr)
  (string-append "(" (string-join expr "") ")"))

(: with-semicolon (String * -> String))
  (define (with-semicolon . expr)
    (string-append (string-join expr "") ";"))

(: statement-sequence ((Listof JsAst) -> String))
(define (statement-sequence exprs)
  (string-join (map (lambda: ([elem : JsAst]) (with-semicolon (print-js-ast elem))) exprs) ""))

(: print-js-ast (JsAst -> String))
(define (print-js-ast expr)
  (match expr
    [(Literal (? number? value))
     (number->string value)]

    [(Literal (? string? value))
     (format "~a" value)]

    [(Literal (? boolean? value))
     (if value "true" "false")]

    [(Literal (? char? value))
     (string value)]

    [(Binary op left right)
     (with-paren
      (print-js-ast left)
      (case op
	[(BOOLEAN_OR) "||"]
	[(BOOLEAN_AND) "&&"]
	[(IS_IDENTICAL) "==="]
	[(IS_NOT_IDENTICAL) "!=="]
	[(IS_EQUAL) "=="]
	[(IS_NOT_EQUAL) "!="]
	[(SMALLER) "<"]
	[(IS_SMALLER_OR_EQUAL) "<="]
	[(GREATER) ">"]
	[(IS_GREATER_OR_EQUAL) ">="]
	[(PLUS) "+"]
	[(MINUS) "-"]
	[(MULT) "*"]
	[(DIV) "/"]
	[(MOD) "%"]
	[else (raise "Some binary expression unexpected")])
      (print-js-ast right))]

    [(Unary op right)
     (with-paren
      (case op
	[(PLUS)  "+"]
	[(MINUS) "-"]
	[(NEG)   "!"]
	[else (raise "Some unaty expression unexpected")])
      (print-js-ast right))]

    [(Infix op right)
     (with-paren
      (case op
	[(INC) "++"]
	[(DEC) "--"]
	[else (raise "Some infix expression unexpected")])
      (print-js-ast right))]

    [(Postfix op right)
     (with-paren
      (case op
	[(INC) "++"]
	[(DEC) "--"]
	[else (raise "Some postfix expression unexpected")])
      (print-js-ast right))]

    [(TypeOfExpr right)
     (with-paren
      "typeof "
      (print-js-ast right))]

    [(InstanceOfExpr left right)
     (with-paren
      (print-js-ast left)
      " instanceof "
      (print-js-ast right))]

    [(Null)
     "(null)"]

    [(Undefined)
     "(void 0)"]

    [(VariableDcl name right)
     (format "var ~a = ~a;"
	     name
	     (print-js-ast right))]

    [(Assign left right)
     (with-paren
      (print-js-ast left)
      " = "
      (print-js-ast right))]


    [(IfStmt test then else)
     (format " if (~a) { ~a } else { ~a }"
	     (print-js-ast test)
	     (statement-sequence then)
	     (statement-sequence else))]

    [(ConditionalOp test then else)
     (with-paren
      (print-js-ast test) " ? "
      (print-js-ast then) " : "
      (print-js-ast else))]
    
    [(WhileStmt test body)
     (format " while (~a) { ~a }"
	     (print-js-ast test)
	     (statement-sequence body))]

    [(ForStmt init test step body)
     (format " for (~a ; ~a ; ~a) { ~a }"
	     (print-js-ast init)
	     (print-js-ast test)
	     (print-js-ast step)
	     )]

    [(FunctionExpr args body)
     (format "(function(~a) { ~a })"
	     (string-join args ",")
	     (statement-sequence body))]

    [(FunctionCall expr args)
     (format 
      "~a(~a)"
      (print-js-ast expr)
      (string-join (map print-js-ast args) ","))]
     
    [(ArrayExpr exprs)
     (format 
      "[~a]"
      (string-join (map print-js-ast exprs) ","))]
    
    [(ArrayCall expr indice)
     (format "(~a[~a])"
	     (print-js-ast expr)
	     (print-js-ast indice))]

    [(ObjectExpr pairs)
     (format 
      "{~a}"
      (string-join
       (map
	(lambda: ([elem : (Pairof String JsAst)])
	  (format "~s:~a"
		  (car elem)
		  (print-js-ast (cdr elem))))
	pairs)
       ","))]

    [(VariableAccess names)
     (string-join names ".")]

    [(FieldAccess expr name)
     (format "~a.~a"
             (print-js-ast expr)
             name)]
    
    [(New expr)
     (with-paren 
      "new "
      (print-js-ast expr))]

    [(Empty)
     ""]
    
    ;; Try
    ;; Catch

    [(Return expr)
     (string-append
      "return "
      (print-js-ast expr))]))
