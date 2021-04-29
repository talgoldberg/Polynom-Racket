#lang pl 02

;;308361476

#|

Q1 : BNF (SE)
BNF for "SE": A similar simple language of "string expressions". Only digits 0, ..., 9 are allowed as valid characters within strings.
The legal operators that can be used in these expressions are string, string-length and string-append, and also string-insert
And number-> string. It is also legal to accept expressions of the form "<D>", where <D> represents a (final) digit sequence.

The following grammar requirements:
A string is allowed with a sequence of any number of characters.
String-allowed is allowed with a sequence of any number of
Expressions representing strings. String allowed with
An expression representing a string, a character and a natural number.
Number-> A string is allowed with a natural number.
|----------------------------------------------------------------1.a---------------------------------------------------|

<SE>::= <num-digit>        //1
       |<string-digit>       //2
       |<char-num>       //3

<num-digit>::= <num>                  //4
              |<num> <num-digit>      //5
              

<char-digit>  ::= <char-num>                  //6
                  |<char-num> <char-digit>    //7

<char-num>::=\#<num>   //8

<string-num>::= "<num>"   //9
                |"<num><string-num>"   //10

<num>::= 0
         |1
         |2
         |3
         |4    //11
         |5
         |6
         |7
         |8
         |9




<string-digit> ::=   <string-num>       //12
                   | <num-digit>
                   | {string <char-digit>}    //13
                   | {string-append <string-combanition>}  //14
                   | {string-insert <string-combanition> <char-num> <digit-combanition>}   //15
                   | {string-length <string-combanition>}   //16
                   | (number->string <digit-combanition>)   //17
                   | 𝜆    //18



<string-combanition> ::=  𝜆    //19
                         |<string-num>    //20
                         |𝜆 <string-combanition>  //21
                         |<string-num> <string-combanition>   //22
                         |{string-append <string-combanition>}<string-combanition>   //23
                         |{string <char-digit>} <string-combanition>                   //24
                         |{number->string <digit-combanition>} <string-combanition>  //25
                         |{string-insert <string-combanition> <char-num> <digit-combanition>} <string-combanition>   //26

<digit-combanition> ::= <num-digit>   //27
                        |{string-length <string-combanition>}   /28


|------------------------------------------------------------1.b------------------------------------------------------------------|


Example 1:
(string-append  "901" "66" )

<SE>

<string-digit> //2    ??
<string-digit>::{string-append <string-combanition>}  //14   --------------------------->   (string-append ??)
<string-combanition>::{number->string <digit-combanition>} <string-combanition>  //25 --->  (string-append (number->string ??)??)??
<digit-combanition> ::= <num-digit> //4 //11                //27 -------------------------> (string-append (number->string 901)??)??
                        |{string-length <string-combanition>} //28 
<string-num>    //20       ------------------------------------------------------------>     (string-append (number->string 901)??)??
<string-num>::= "<num>" //11         //9   -------------------------------------------->     (string-append (string-append (number->string 901)"66")
                |"<num><string-num>"   //10


RESULT: (string-append "901" "66")


Example 2:
(string-length "123")

<SE>

<string-digit> //2    ??

<string-digit>::{string-length <string-combanition>}   //16  ------------------------------->    (string-length ??)
<string-combanition>:: {number->string <digit-combanition>} <string-combanition>  //25 ----->    (string-length (number->string ??))??
<digit-combanition> ::= <num-digit> //4 //11                //27 --------------------------->    (string-length (number->string 123))
                        |{string-length <string-combanition>} //28 


RESULT: (string-length "123")



Example 3:
(string-insert "789" \#6 5)

<SE>

<string-digit> //2 ----> ??

<string-digit>::{string-insert <string-combanition> <char-num> <digit-combanition>} //15 ---> (string-insert ?? ?? ??)

<string-combanition>::<string-num>  //20 -----------------------------------> (string-insert "789" ?? ??)

<char-num>:: <char-num>::=\#<num>   //8 -----------------------------------> (string-insert "789" \#6 ??)

<digit-combanition>:: <digit-combanition> ::= <num-digit>   //27   --------> (string-insert "789" \#6 (string-length ??))
                        |{string-length <string-combanition>}   /28 

<string-combanition>:: <string-num> :: //20 <string-num>::= "<num>"  //9 --> (string-insert "789" \#6 (string-length "12345"))
                                            |"<num><string-num>"   //10


RESULT: (string-insert "789" \#6 5)



|#




#|

Q2 :
Function: sum-of-squares:(without use map)
The function receives a list, and returns the amount of squares for each member in the list,
with useing foldl function and square function.
Function: square : get number and calculate is square and return is square.

|#

(: sum-of-squares : (Listof Number) -> Number)
(: square : Number -> Number)

(define (sum-of-squares lst)
          (cond
            [(null? lst) 0]
            [else (+ (foldl + 0 (list(square (first lst)))) (sum-of-squares (rest lst)))]))


(define (square x)(* x x))



(test (sum-of-squares '(4 5 6)) => 77)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(-2 -1 13)) => 174)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(2 2 5 1)) => 34)
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(10 2 2)) => 108)
(test (sum-of-squares '(10)) => 100)
(test (square 4) => 16)
(test (square -2) => 4)



#|

Q3.a :
In this question I created a polynomial using a list of numbers (coefficients),
and after a polynomial is created the input X (number) will return a result of the polynomial.

|#

(: createPolynomial : (Listof Number) -> (Number -> Number));; <fill-in> : (Number -> Number) for input and use polyX and poly.

(define  (createPolynomial lst)

  (: poly : (Listof Number) Number Integer Number -> Number)

(define (poly argsL x power accum)
    (cond
      [(null? argsL) accum]
      [else (+ accum (* (first argsL) (expt x power)) (poly (rest argsL) x (+ 1 power) accum))])) ;; use in  tail recursion for calculate the input: x (number) for polynom
  (: polyX : Number -> Number)
     (define (polyX x)
       (poly lst x 0 0));; <fill-in> : (poly lst x 0 0) : to return a number for input x.
  polyX) ;; <fill-in> : polyX : to return for createPolynomial (Number -> Number). 
 


(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) => (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5 (expt 0 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4 (expt 11 2)) (* 5 (expt 11 3))))
(test (p2345 4) => (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5 (expt 4 3))))

(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6 (expt 11 2))))
(test (p536 6) => (+ (* 5 (expt 6 0)) (* 3 (expt 6 1)) (* 6 (expt 6 2))))

(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)


#|

Q3.b :
In this question I created an expression that defines a polynomial (PLANG),
with help that defines expressions of type (AE).
and after an expression of a polynomial is created, we want to calculate it using a list of coefficients and a list of points.

|#

(define-type PLANG
  [Poly (Listof AE) (Listof AE)]);; <fill-in> : (Listof AE) 

(define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])

(: parse-sexpr : Sexpr -> AE)

 (define (parse-sexpr sexpr)
   (match sexpr
     [(number: n) (Num n)]
     [(list '+ lhs rhs) (Add (parse-sexpr lhs) 
                             (parse-sexpr rhs))]
     [(list '- lhs rhs) (Sub (parse-sexpr lhs)  (parse-sexpr rhs))]
     [(list '* lhs rhs) (Mul (parse-sexpr lhs) 
                             (parse-sexpr rhs))]
     [(list '/ lhs rhs) (Div (parse-sexpr lhs) 
                             (parse-sexpr rhs))]
     [else (error 'parse-sexpr "bad syntax in ~s" 
                  sexpr)]))


(: parse : String -> PLANG)
(: parse-sexp :  Sexpr -> PLANG)                ;; HELP-FUNCTION : get a Sexpr and return PLANG.
(: parse-sexp* : (Listof Sexpr) -> (Listof AE)) ;; HELP-FUNCTION : get a list of Sexpr and return list of AE.


;; parse-sexp :
;; Checks if there is a match for two-list expressions.
;; List 1: (list of Sexpr) Contains poly symbol and other arguments (type AE). 
;; List 2: (list of Sexpr) Contains arguments (type AE).
;; Use an auxiliary function (parse-sexp*) to return list of (type AE) And at the end approached the builder of PLANG.

(define (parse-sexp sexp)
  (match sexp             
    [(list (list 'poly) (list z ...)) (error 'parse "parse: at least one coefficient is required in ~s" sexp)]
    [(list (list 'poly t ...) '()) (error 'parse "parse: at least one point is  required in ~s" sexp)]
    [(list (list 'poly x ...) (list y ...)) (Poly (parse-sexp* x) (parse-sexp* y))]
    [else (error 'parse "parse: bad syntax in ~s" sexp)]))
   

;; parse-sexp* :
;;Receives a list of Sexpr And recursively creates a list of (type AE) with the help of parse-sexpr

(define (parse-sexp* lst)
  (if (null? lst)
      null
  (append (list (parse-sexpr(first lst))) (parse-sexp* (rest lst)))))


(define (parse str)
(let([code (string->sexpr str)]) (parse-sexp code)));; <fill-in> : (parse-sexp code) to return PLANG




 
 (test(parse "{{poly {+ 3 4} 4} {6 {- {+ 2 5} {* 10 4}}}}") => (Poly (list (Add (Num 3) (Num 4)) (Num 4)) (list (Num 6) (Sub (Add (Num 2) (Num 5)) (Mul (Num 10) (Num 4))))))
 (test(parse "{{poly 1 2 3} {1 2 3}}") => (Poly (list (Num 1) (Num 2) (Num 3)) (list (Num 1) (Num 2) (Num 3))))
 (test(parse "{{poly 4/5 } {1/2 2/3 3}}") => (Poly (list (Num 4/5)) (list (Num 1/2) (Num 2/3) (Num 3))))
 (test(parse "{{poly 2 3} {4}}") => (Poly (list (Num 2) (Num 3)) (list (Num 4))))
 (test(parse "{{poly 1 1 0} {-1 3 3}}") => (Poly (list (Num 1) (Num 1) (Num 0)) (list (Num -1) (Num 3) (Num 3))))
 (test(parse "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}") => (Poly (list (Div (Num 4) (Num 2)) (Sub (Num 4) (Num 1))) (list (Sub (Num 8) (Num 4)))))
 (test(parse "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => (Poly (list (Add (Num 0) (Num 1)) (Num 1) (Mul (Num 0) (Num 9))) (list (Sub (Num 4) (Num 5)) (Num 3) (Div (Num 27) (Num 9)))))
 (test(parse "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))")
 (test(parse "{{poly 1 2} {} }") =error> "parse: at least one point is  required in ((poly 1 2) ())")
 (test(parse "{{poly } {1 2 3} }")=error> "parse: at least one coefficient is required in ((poly) (1 2 3))")
 (test(parse "{{poly 4/5 } {poly 1/2 2/3 3} {1 2 4} {1 2}}") =error> "parse: bad syntax in ((poly 4/5) (poly 1/2 2/3 3) (1 2 4) (1 2))")
 (test(parse "{{poly 2 3} {}}") =error> "parse: at least one point is  required in ((poly 2 3) ())")
 (test(parse "{{poly 1 1 3} }") =error> "parse: bad syntax in ((poly 1 1 3))")
 (test(parse "{{} }") =error> "parse: bad syntax in (())")
 (test(parse "{{} {}}") =error> "parse: bad syntax in (() ())")
 (test(parse "{{1 2 4} {1 2 3}}") =error> "parse: bad syntax in ((1 2 4) (1 2 3))")
 (test(parse "{{poly} {1 2 3} {poly}}") =error> "parse: bad syntax in ((poly) (1 2 3) (poly))")



( : eval :  AE -> Number)
(define (eval expr)
   (cases expr
     [(Num n) n]
     [(Add l r) (+ (eval l) (eval r))]
     [(Sub l r) (- (eval l) (eval r))]
     [(Mul l r) (* (eval l) (eval r))]
     [(Div l r) (/ (eval l) (eval r))]))

(: eval-poly-list : (Listof Number) (Listof Number) ->  (Listof Number)) ;; HELP-FUNCTION 

(: poly-help : (Listof Number) Number Integer -> Number) ;; HELP-FUNCTION 

(define (poly-help lst N ACC) ;;calculate a polynomial. and return a number
    (cond
      [(null? lst) 0]
      [else (+ (* (first lst) (expt N ACC)) (poly-help (rest lst) N (+ 1 ACC)))]))

;;eval-poly-list :
;;The function gets two lists of numbers from the function eval-poly-help
;;List 1: of coefficients
;;List 2: of points.
;;And uses the function (poly-help) to calculate a polynomial.
;;And finally returns the required list.

(define (eval-poly-list ls rs)
  (cond
    [(null? rs) null]
    [else (append (list(poly-help ls (first rs) 0)) (eval-poly-list ls (rest rs)))]))



;;eval-poly-help :
;;The function gets a list of (type AE), And is aided by the function eval
;;In order to decipher the expression of AE to a number, And finally returns a list of numbers.

(: eval-poly-help : (Listof AE) -> (Listof Number));; HELP-FUNCTION 
(define (eval-poly-help ls)
  (cond
    [(null? ls) null]
    [else (append(list(eval (first ls))) (eval-poly-help (rest ls)))]))
  

;;eval-poly :
;;The function gets PLANG and checks if there is a match of two lists of (type AE)
;;with help from function (eval-poly-help) , and return list of number with help from function (eval-poly-list).

(: eval-poly : PLANG -> (Listof Number)) ;; <fill-in> : (Listof Number)
 (define (eval-poly p-expr)
   (cond
     [(null? p-expr) null]           ;; <fill-in> : code
     [else
      (cases p-expr
        [(Poly lst rst) (eval-poly-list (eval-poly-help lst) (eval-poly-help rst))])]))
  

(: run : String -> (Listof Number))
 (define (run str) ;; return list of number 
 (eval-poly (parse str)))

(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4 2 7} {1 4 9}}") => '(13 124 589))
(test (run "{{poly 1 2 3} {1 2 3}}") => '(6 17 34))
(test (run "{{poly 4/5 } {1/2 2/3 3}}") => '(4/5 4/5 4/5))
(test (run "{{poly 2 3} {4}}") => '(14))
(test (run "{{poly 1 1 0} {-1 3 3}}") => '(0 4 4))
(test (run "{{poly {/ 4 2} {- 4 1}} {{- 8 4}}}")=> '(14))
(test (run "{{poly {* 4 -2} {+ 5 2} {- 2 1}} {{- 8 4} {+ 1 1}}}") => '(36 10))
(test (run "{{poly {+ 0 1} 1 {* 0 9}} {{- 4 5} 3 {/ 27 9}}}") => '(0 4 4))
(test (run "{{poly } {1 2} }") =error> "parse: at least one coefficient is required in ((poly) (1 2))")

