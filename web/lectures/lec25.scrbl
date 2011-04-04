#lang scribble/manual

@(require "../utils.rkt")

@title[#:tag "lec25"]{4/4: One Tool, Many Languages}

@section{Announcements}

@itemlist[
@item{During the final exam period, we will have a tournament of your computer
players.} 

@item{The time, date, and location of the final exam are not yet determined.}
]

@section{The derivative function}

The derivative from calculus is a higher-order function. In this class, we
implement numeric approximations to the derivative in various languages that
we've seen this year.  All of our languages can express higher-order functions,
some easily, and some not so easily.  

@section{ISL with lambda}

@codeblock{
 (define ep 0.000001)
 ;; [Number -> Number] -> [Number -> Number]
 ;; Compute the derivative of f
 (define (deriv f)
   (lambda (x)
     (/ (- (f (+ x ep))
           (f (- x ep)))
        (* 2 ep))))
}

@section{@racketmodname[class5]}

The first version:

@codeblock{
#lang class5
(define ep 0.0001)

;; A [IFun X Y] implements:
;; apply : X -> Y
;; Apply this function to the given argument.

;; [IFun Number Number]
(define-class sqr%
 (implements ifun<%>)
 (define/public (apply x)
   (* x x)))

;; [IFun [IFun Number Number] [IFun Number Number]]
(define-class deriv%
 ;; [IFun Number Number] -> [IFun Number Number]
 (define/public (apply f)
   (local [(define-class f*
             ;; Number -> Number
             (define/public (apply x)
               (/ (- (f . apply (+ x ep))
                     (f . apply (- x ep)))
                  (* 2 ep))))]
     (new f*))))
}

A version where @r[ep] is a field, and with interfaces:


@codeblock{
#lang class5

;; A [IFun X Y] implements:
(define-interface ifun<%>
 [;; apply : X -> Y
  ;; Apply this function to the given argument.
  apply])

;; [IFun Number Number]
(define-class sqr%
 (implements ifun<%>)
 (define/public (apply x)
   (* x x)))

;; [IFun [IFun Number Number] [IFun Number Number]]
(define-class deriv%
 (implements ifun<%>)

 (fields ep)

 (define/public (apply f)
   (local [(define-class f*
             (implements ifun<%>)
             ;; Number -> Number
             (define/public (apply x)
               (/ (- (f . apply (+ x (ep)))
                     (f . apply (- x (ep))))
                  (* 2 (ep)))))]
     (new f*))))


;; a default version where the epsilon is already specified
(define (deriv/e%)
 (deriv% 0.0001))
}

@section{Java}


With anonymous inner classes:

@verbatim|{

interface IFun<X,Y> {
   // Apply this function to the given argument.
   Y apply(X x);
}

class Sqr implements IFun<Double,Double> {
   Sqr() {};

   public Double apply(Double x) {
       return x*x;
   }
}

class Sin implements IFun<Double,Double> {
   Sin() {};

   public Double apply(Double x) {
       return Math.sin(x);
   }
}

class Deriv implements IFun<IFun<Double,Double>,IFun<Double,Double>> {
   Double ep;

   Deriv() {
       this.ep = 0.0001;
   }

   Deriv(Double ep) {
       this.ep = ep;
   }

   public IFun<Double,Double> apply(final IFun<Double,Double> f) {    
       return new IFun<Double,Double>() {
          public Double apply(Double x) {
            return (f.apply(x+ep) - f.apply(x-ep)) / (2 * ep);
          }
       };
   }
}

class Main {
   public static void main(String[] args) {
       System.out.println( new Sqr().apply(5.0) );
       System.out.println( new Deriv().apply(new Sqr()).apply(5.0) );
       System.out.println( new Deriv().apply(new Sin()).apply(5.0) );
       System.out.println( new Deriv(0.000001).apply(new Sin()).apply(5.0) );
       System.out.println( Math.cos(5.0) );
   }
}
}|

With lifted classes:

@verbatim|{

interface IFun<X,Y> {
   // Apply this function to the given argument.
   Y apply(X x);
}

class Sqr implements IFun<Double,Double> {
   Sqr() {};

   public Double apply(Double x) {
       return x*x;
   }
}

class Sin implements IFun<Double,Double> {
   Sin() {};

   public Double apply(Double x) {
       return Math.sin(x);
   }
}

class FDeriv implements IFun<Double,Double> {
   IFun<Double,Double> f;
   Double ep;

   FDeriv(IFun<Double,Double> f, Double ep) {
       this.f = f;
       this.ep = ep;
   }

   public Double apply(Double x) {
       return (f.apply(x+ep) - f.apply(x-ep)) / (2 * ep);
   }
}

class Deriv implements IFun<IFun<Double,Double>,IFun<Double,Double>> {
   Double ep;

   Deriv() {
       this.ep = 0.0001;
   }

   Deriv(Double ep) {
       this.ep = ep;
   }

   public IFun<Double,Double> apply(final IFun<Double,Double> f) {    
       return new FDeriv(f, ep);
   }
}

class Main {
   public static void main(String[] args) {
       System.out.println( new Sqr().apply(5.0) );
       System.out.println( new Deriv().apply(new Sqr()).apply(5.0) );
       System.out.println( new Deriv().apply(new Sin()).apply(5.0) );
       System.out.println( new Deriv(0.000001).apply(new Sin()).apply(5.0) );
       System.out.println( Math.cos(5.0) );
   }
}
}|
