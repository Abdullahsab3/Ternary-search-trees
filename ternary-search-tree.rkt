#lang r6rs
(library
 (ternary-search-tree)
 (export new from-scheme-list ternary-search-tree? find insert! word-complete)
 (import (rnrs base)
         (srfi :9)
         (rnrs io simple)
         (rnrs mutable-pairs)
         (prefix (tree ternary-tree) tree:))

 ;; Deel 2 TST

 ;; Extra procedures om verder te abstraheren.
 ;; De value van de node is een cons-cel met het karakter in de car van de cons-cel..
 ;; .. en de value (die meegegeven wordt bij het oproepen van insert!) in de cdr van de cons-cel. 
 (define (make-assoc char val) (cons char val))
 (define (char node) (car (tree:value node)))
 (define (val node)  (cdr (tree:value node)))
 (define (val! node new-val) (set-cdr! (tree:value node) new-val))
 

 (define-record-type tst
   (make r ce cl i-c ca)
   ternary-search-tree?
   (r root root!)
   (ce char-equality)
   (cl char-lesser)
   (i-c ith-char)
   (ca c-append))


 (define (new char=? char<? ith-char append-char-to-str)
   (make tree:null-tree char=? char<? ith-char append-char-to-str))



 (define (from-scheme-list slst char=? char<? ith-char append-char-to-str)
   (let loop
     ;; De lijst wordt gereversed om ervoor te zorgen dat de volgorde van het inserten ..
     ;; ..van de pairs<string V> van de lijst bewaard wordt.
     ;; Bij terugkomen uit de recursie wordt eerst het de laatste pair<string V> van de lijst geinsert,..
     ;; ..en daarom wordt de lijst gereversed.
     ((lst (reverse slst)))
     (if (null? lst)
         (new char=? char<? ith-char append-char-to-str)
         (let ((string (car (car lst)))
               (value (cdr (car lst))))
           (insert! (loop (cdr lst)) string value)))))


 ;; Gegeven een tst en een string (de key) zal de procedure find-key de key zoeken in de tst.
 ;; De node die het laatste karakter van de key bijhoudt wordt teruggegeven indien de key gevonden wordt.
 ;; Indien niet gevonden, wordt #f teruggegeven.
 ;; Deze procedure wordt gedefinieerd om code duplicatie te vermijden; het wordt namelijk gebruikt in ..
 ;; .. de find procedure, de insert! procedure en de word-complete procedure.
 ;; Proceduretype: (TST<char v> string -> ternary-tree ∪ {#f})

 (define (find-key tst string)
   (define <<? (char-lesser tst))
   (define ith (ith-char tst))
   (define str-length (string-length string))
   (let find-key
     ((node (root tst))
      (idx 0))
     (if (tree:null-tree? node)
         #f
         (let ((node-key (char node))
               (ith-character (ith string idx)))
           (cond
             ((<<? node-key ith-character)
              (find-key (tree:right node) idx))
             ((<<? ith-character node-key)
              (find-key (tree:left node) idx))
             ;; Als we aan het einde van key zitten, geven we node terug.
             ;; Anders dalen we verder af in het middelste kind.
             (else (if (= idx (- str-length 1))
                       node
                       (find-key (tree:middle node)
                                 (+ idx 1)))))))))


 (define (find tst string)
   (let ((found-node (find-key tst string)))
     (if found-node
         (val found-node)
         ;; indien de key niet gevonden wordt, geeft de procedure find-key #f terug..
         ;; ..en found-node wordt dus #f.
         found-node)))


 (define (insert! tst string val)
   (define <<? (char-lesser tst))
   (define ==? (char-equality tst))
   (define ith (ith-char tst))
   (define str-length (string-length string))
   ;; Indien de key al in de ternary-search tree zit,..
   ;; .. wordt enkel de value van de node aangepast.
   (let ((found-key (find-key tst string)))
     (if found-key
         (val! found-key val)
         (let insert-iter
           ((parent tree:null-tree)
            ;; bij de destructieve child! procedures,..
            ;; .. worden telkens de nieuw aangemaakte nodes teruggegeven.
            ;; Het argument van de child wordt in de tweede tak van de conditional..
            ;; destructief aangepast naar deze node.
            (child! (lambda (ignore child) (root! tst child) (root tst)))
            (child (root tst))
            (idx 0))
           (cond  ((= idx (- str-length 1))
                   (if (tree:null-tree? child)
                       (child! parent (tree:new (make-assoc (ith string idx) val)
                                                tree:null-tree
                                                tree:null-tree
                                                tree:null-tree))
                       (insert-iter child (lambda (parent child)
                                            (if (<<? (ith string idx) (char parent))
                                                (tree:left! parent child)
                                                (tree:right! parent child)))
                                    tree:null-tree idx)))
                  ((tree:null-tree? child)
                   ;; de binding van de child (een argument in de let) wordt aangepast ..
                   ;; ..en "geupdatet".
                   ;; De waarde van de child (namelijk (root tst)) was al geevalueerd ..
                   ;; en dat was in dit geval een null-tree, dus moest ik een manier bedenken om child te updaten.
                   ;; Ik kon (root tst) niet als argument voor de child meegeven want dan blijft dat 'gehardcodeerd' ..
                   ;; .. op (root tst) en elke keer het algoritme in dit tak terechtkomt en insert-iter oproept, past het de (root tst) aan, ..
                   ;; ..wat natuurlijk niet de bedoeling is.
                   ;; Ik moest dus ervoor zorgen dat ik effectief afdaal in de boom.
                   ;; Daarom geef ik telkens bij destructieve operaties (die meegegeven worden als actuele parameter voor child!)..
                   ;;.. de destructief aangepaste node terug.
                   ;; De binding van child wordt destructief aangepast en child wordt dan gebonden aan deze node.
                   (set! child
                         (child! parent (tree:new (make-assoc (ith string idx) '())
                                                  tree:null-tree
                                                  tree:null-tree
                                                  tree:null-tree)))
                   (insert-iter child (lambda (parent child) (tree:middle! parent child)
                                        (tree:middle parent))
                                tree:null-tree  (+ idx 1)))
                  ((<<? (char child) (ith string idx))
                   (insert-iter child (lambda (parent child) (tree:right! parent child)
                                        (tree:right parent))
                                (tree:right child) idx))
                  ((<<? (ith string idx) (char child))
                   (insert-iter child (lambda (parent child) (tree:left! parent child)
                                        (tree:left parent))
                                (tree:left child) idx))
                  ((==? (ith string idx) (char child))
                   (if (tree:null-tree? (tree:middle child))
                       (insert-iter child (lambda (parent child) (tree:middle! parent child)
                                            (tree:middle parent))
                                    (tree:middle child) (+ idx 1))
                       (insert-iter child child! (tree:middle child) (+ idx 1)))))))
     tst))


 ;; Deel 3   Automatische woordsuggestie

 (define (word-complete tst string)
   (define char-append (c-append tst))

   ;; een extra procedure om de suffixen te verzamelen.
   (define (get-suffixes node str)
     (if (tree:null-tree? node)
         '()
         (append (get-suffixes (tree:left node) str)
                 (get-suffixes (tree:right node) str)
                 (get-suffixes (tree:middle node) (char-append str (char node)))
                 ;; indien de huidige node een value heeft, ..
                 ;; .. voegen we de string toe samen met de value (in een conscel) aan het resultaat.
                 (if (not (null? (val node)))
                     (list (cons (char-append str (char node))
                                 (val node)))
                     '()))))

   (let ((found-key (find-key tst string)))
     (if found-key
         (let ((suffixes (get-suffixes (tree:middle found-key) string)))
           (if (null? (val found-key))
               suffixes
               ;; indien de node een value heeft, voegen we de key en bijhorende value toe ..
               ;; .. aan het resultaat.
               (cons (cons string (val found-key))
                     suffixes)))
         ;; indien de key niet in de tst zit, wordt #f teruggegeven.
         found-key)))

 ;; Een procedure om een karakter achteraan toe te voegen aan een string.
 ;; (deze procedure wordt meegegeven bij het maken van een nieuwe tst ..
 ;; .. als actuele parameter van de laatste formele parameter van new, namelijk append-char-to-str.)
 (define (append-char-to-string str char)
   (string-append str (string char))))






