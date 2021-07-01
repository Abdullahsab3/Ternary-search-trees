#lang r6rs

(library
 (ternary-tree)
 (export null-tree null-tree? new ternary-tree? value value! left left! right right! middle middle!)
 (import (rnrs base)
         (srfi :9)
         (rnrs mutable-pairs)
         (rnrs io simple))

 ;; Deel 1 Ternary Tree
 (define-record-type ternary-tree
   (new v l r m)
   ternary-tree?
   (v value value!)
   (l left left!)
   (r right right!)
   (m middle middle!))

 (define null-tree ())

 (define (null-tree? node)
   (eq? node null-tree)))