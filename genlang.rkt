#lang racket

; based vaguely on http://www.zompist.com/gen.html

; SELECTION
(define (% threshold) (< (random 100) threshold))

(struct RankedList (dropoff ls lsb) #:transparent)
(define (RL dropoff ls)
  (RankedList dropoff ls ls))
(define (rget rl)
  (let ([df (RankedList-dropoff rl)] [ls (RankedList-ls rl)] [lsb (RankedList-lsb rl)])
    (if (% df)
        (first ls)
        (one-of (if (equal? (length ls) 1) 
                    lsb
                    (RankedList df (rest ls) (if lsb lsb ls)))))))

(define (one-of seq) 
  (cond
    [(vector? seq) (vector-ref seq (random (vector-length seq)))]
    [(list? seq) (list-ref seq (random (length seq)))]
    [(string? seq) (one-of (string->list seq))]
    [(RankedList? seq) (rget seq)]
    [else seq]))

; LANGUAGE DEF
(struct Lang (name syl rep raw) #:transparent)

(define-syntax deflang
  (syntax-rules ()
    [(deflang «name» «syl» «rep»)
     (define «name»
       (Lang (quote «name») «syl» «rep»
             (λ ([n 3])
               (apply string-append (build-list n (λ (n) ((Lang-syl «name»))))))))]))

(deflang Tamadh
  (λ () 
    (let* ([cn (RL 30 '(t m n s v r l k x d b p h f))]
           [vw (RL 30 '(A E I a e i o O y J Y j u w))]
           ; ^Categories^ vSyllable Typesv
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd) 
    (regexp-replaces 
     wrd
     '([#rx"O$" "u"] 
       [#rx"^y" "Y"] [#rx"bb" "b"] [#rx"pp" "p"] [#rx"xx" "x"]
       [#rx"a([AEIaeioOyJYju])" "A\\1"] [#rx"i([AEIaeioOyJYju])" "I\\1"] [#rx"e([AEIaeioOyJYju])" "E\\1"]
       [#rx"a" "â"] [#rx"A" "á"] [#rx"e" "ê"] [#rx"E" "é"] [#rx"i" "î"] [#rx"I" "í"] [#rx"o" "ó"] [#rx"O" "ä"] 
       [#rx"y" "ý"] [#rx"Y" "áì"] [#rx"j" "êì"] [#rx"J" "éì"] [#rx"u" "äu"] 
       [#rx"b" "dh"] [#rx"p" "th"] [#rx"x" "hr"]))))

(deflang Silsi
  (λ ()
    (let* ([cn '(s l n r f v m t d p k b dh th z)]
           [fcn (RL 15 (append cn '(h)))]
           [pcn (RL 15 (append cn '(nn rr ss ll ff mm zz)))]
           [avw (RL 30 '(é í á ä ê â î ú ó))]
           [yvw (RL 30 '(í ê ó ä á é ú))]
           [syls (RL 30 `((,fcn ,avw) (,avw ,pcn) (,fcn ,avw ,pcn) (,avw)
                                      (,fcn y ,yvw) (,fcn y ,yvw ,pcn) (y ,yvw ,pcn) (y ,yvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"lll+" "ll"] [#rx"rrr+" "rr"] [#rx"nnn+" "nn"] [#rx"mmm+" "mm"] 
                        [#rx"sss+" "ss"] [#rx"fff+" "ff"] [#rx"zzz+" "zz"]))))

(deflang Ertydon
  (λ ()
    (let* ([c (RL 30 '(r l n m k s p d j f θ t g))]
           [w (RL 30 '(k p d f θ t g))]
           [v (RL 30 '(a i e ei ai o u))]
           
           [syls (RL 30 `((,v) (,v h) (,c ,v) (,c ,v ,c) (,c ,v h) (,v ,c) (,w r ,v) (,w r ,v ,c) (,w r ,v h)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"θ" "th"] [#px"(.)\\1" "\\1"] [#rx"([^aeo])h" "\\1"]))))

#|
 | Must end in a consonant. 
 | Pairs of consonants (and no more than that) in the middle of words only, not to start or end. 
 | Syllables may begin with consonants or vowels and contain only one vowel. 
 | H can't end a word.
 | let's say that ch isn't allowed to be in clusters.
 | Aa syllables are always emphasized so you don't want them next to each other.
 | aa, a, e, i, o, u, Y in Ryganaavlan only...
 | k, s, p, r, v, sh, h, n, t, d, b, f, l, th, ch, G and Y in Ryganaavlan only
 |#
(deflang Leraal
  (λ ()
    (let* ([cn '(m k s p r v sh n t d b f l th h ch)]
           [fcn (RL 15 (append '(h ch) cn))]
           [pcn (RL 15 (append '(h) cn))]
           [vw (RL 30 '(e a i aa o u))]
           
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#px"([^a])\\1" "\\1"] [#rx"aa(..?aa)" "a\\1"]))))

(deflang Lat
  (λ ()
    (let* ([cn '(b c d f g h j k l m n p q r s t v w x y z 
                   ch sh th ts st)]
           [vw '(a e i o u y)]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  identity)

(deflang Dwarvish
  (λ ()
    (let* ([cn '(d p t k h n m s c v l r b st sh j w f ch g z x)]
           [fcn (RL 15 (append cn '(bh rh)))]
           [pcn (RL 15 (append cn '(nd)))]
           [vw (RL 30 '(e a i u o ae))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Kayfal
  (λ ()
    (let* ([cn (RL 15 '(s k f c d t n l r m v h nd sk y st kh sh p b j w g z x))]
           [vw (RL 15 '(e a i u o))]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw ,cn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"^nd" "d"]))))

(deflang Anavasi
  (λ ()
    (let* ([cn '(n v d t p k m h l r s c b j z)]
           [fcn (RL 15 (append cn '()))]
           [pcn (RL 15 (append cn '()))]
           [vw (RL 30 '(i a e u o ae))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"aee" "ae"] [#rx"aae" "ae"]))))

(deflang Aiha
  (λ ()
    (let* ([cn '(l r n m p f s t h k d v y b w sh ts)]
           [fcn (RL 15 (append cn '(pr)))]
           [pcn (RL 15 (append cn '(ll rr mm nn ns)))]
           [vw (RL 30 '(a i e u o ae ai au))]
           [syls (RL 30 `((,fcn ,vw) (,vw ,pcn) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"lll+" "ll"] [#rx"rrr+" "rr"] [#rx"nnn+" "nn"] [#rx"mmm+" "mm"]))))

(deflang Elannwyn 
  (λ ()
    (let* ([cn '(m n r l k d sh s t w h j z v f b ts)]
           [fcn (RL 15 (append cn '()))]
           [pcn (RL 15 (append cn '(y nn mm)))]
           [vw (RL 30 '(e i a o u uu ae))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"aee" "ae"] [#rx"aae" "ae"] [#rx"nnn+" "nn"] [#rx"mmm+" "mm"]))))

(deflang Aluvai
  (λ ()
    (let* ([cn '(s f z d v th t k p l n r m b sh ch j w sk)]
           [fcn (RL 15 (append cn '(kh)))]
           [pcn (RL 15 (append cn '(ss rr)))]
           [vw (RL 30 '(i e a y u o ae ai))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Ceirene
  (λ ()
    (let* ([cn '(m r n l s t v c d k p f th b sh j w ts sk)]
           [fcn (RL 15 (append cn '()))]
           [pcn (RL 15 (append cn '(ns tt)))]
           [vw (RL 30 '(a e i o u y ai ae))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Mahlirou ; Antimoun Joralina Taphinieu Dianaevo Adianera
  (λ ()
    (let* ([cn '(t c j m n h l r ph d s v b k p y w)]
           [fcn (RL 15 (append cn '(ch)))]
           [pcn (RL 15 (append cn '()))]
           [vw '(i a e u o eu au ae)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '(iu ou ya)))]
           [syls (RL 30 `((,fcn ,pvw) (,vvw ,pcn) (,fcn ,pvw ,pcn) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"php?h" "ph"]))))

(deflang Obsidian
  (λ ()
    (let* ([cn '(l n s t m r d f sh th g)]
           [fcn (RL 15 (append cn '(k b c h j p v w y)))]
           [pcn (RL 15 (append cn '(ss nd ph st)))]
           [vw (RL 30 '(i e a u o y))]
           [syls (RL 30 `((,fcn ,vw) (,vw ,pcn) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Elemental ; I should write more comments because I have no memory of what this one was for
  (λ ()
    (let* ([cn '(v r l t k m n s sh f d p th b z)]
           [fcn (RL 15 (append cn '(w y)))]
           [pcn (RL 15 (append cn '(h)))]
           [vw (RL 30 '(e a i u o ei ae iu))]
           [syls (RL 30 `((,fcn ,vw) (,vw ,pcn) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"shh" "sh"]))))

(deflang Peskae
  (λ ()
    (let* ([cn '(m r t s f l n k z d c h p v ts st sk)]
           [fcn (RL 15 (append cn '(kh)))]
           [pcn (RL 15 (append cn '(nn rr ss ll ff mm vv zz)))]
           [vw '(e a i u o ei ai ae)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '(ya yi yu yo)))]
           [syls (RL 30 `((,fcn ,pvw) (,vvw ,pcn) (,fcn ,pvw ,pcn) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Gnomish
  (λ ()
    (let* ([cn '(l r t d n m k s v f h p b w z j th)]
           [fcn (RL 15 (append cn '(pr qu)))]
           [pcn (RL 15 (append cn '(ll rr ss st)))]
           [vw  '(i o e a u)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '(y)))]
           [syls (RL 30 `((,fcn ,pvw ,pcn) (,fcn ,pvw) (,vvw ,pcn) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"lll+" "ll"] [#rx"rrr+" "rr"] [#rx"sss+" "ss"] [#rx"ss+t" "st"]))))

(deflang Eriali
  (λ ()
    (let* ([cn '(l r n m t d s th k v f h z dh sh zh)]
           [fcn (RL 15 (append cn '(p b)))]
           [pcn (RL 15 (append cn '()))]
           [vw '(e i a u y o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 30 `((,vvw ,pcn) (,fcn ,pvw) (,fcn ,pvw ,pcn) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Svaaric
  (λ ()
    (let* ([cn '(s k v t r l f m n d j h sh y c kh z g p b)]
           [fcn (RL 15 (append cn '()))]
           [pcn (RL 15 (append cn '()))]
           [vw '(i e a u o ii y aa ai)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 30 `((,vvw ,pcn) (,fcn ,pvw) (,fcn ,pvw ,pcn) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Gemstone
  (λ ()
    (let* ([cn '(l n r m v z f s d t k p h g b)]
           [fcn (RL 15 (append cn '()))]
           [pcn (RL 15 (append cn '()))]
           [vw '(i a e u o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 30 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Enemy
  (λ ()
    (let* ([cn '(t k s f d r g l p n b ts m x h z)]
           [fcn (RL 15 (append cn '(v)))]
           [pcn (RL 15 (append cn '(sk st)))]
           [vw '(e a i o u)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '(ye ya yi yo)))]
           [syls (RL 30 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Edie
  (λ ()
    (let* ([cn '(b c d f g h j k l m n p q r s t v w x y z 
                   ch sh th ts st)]
           [vw '(a e i o u y)]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))]
           [fsyl `(,vw d ,vw)])
      (apply string-append (append (map symbol->string (map one-of fsyl)) 
                                   (map symbol->string (map one-of (one-of syls)))))))
  identity)
(deflang Emily
  (λ ()
    (let* ([cn '(b c d f g h j k l m n p q r s t v w x y z 
                   ch sh th ts st)]
           [vw '(a e i o u y)]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))]
           [fsyl (RL 30 `((,vw m l) (,vw m ,vw l) (,vw m)))])
      (apply string-append (append (map symbol->string (map one-of (one-of fsyl))) 
                                   (map symbol->string (map one-of (one-of syls)))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"^([^l]*)$" "\\1l"]))))

(define (word lang [wn 4] [wr #t])
  ((Lang-rep lang) ((Lang-raw lang) (if wr (add1 (random wn)) wn))))
(define (name lang [wn 4] [wr #t])
  (string-titlecase (word lang wn wr)))

(define (words lang n [wn 4] [wr #t])
  (build-list n (λ (n) (word lang wn wr))))
(define (names lang n [wn 4] [wr #t])
  (map string-titlecase (words lang n wn wr)))

(define (matching-name lang expr [wn 4] [wr #t])
  (let ([nm (name lang wn wr)])
    (if (regexp-match? expr nm)
        nm
        (matching-name lang expr wn wr))))
(define (matching-names lang expr n [wn 4] [wr #t])
  (build-list n (λ (n) (matching-name lang expr wn wr))))

(define (matches-name lang exprs [wn 4] [wr #t])
  (let ([nm (name lang wn wr)])
    (if (andmap (λ (expr) (regexp-match? expr nm)) exprs)
        nm
        (matches-name lang exprs wn wr))))
(define (matches-names lang exprs n [wn 4] [wr #t])
  (build-list n (λ (n) (matches-name lang exprs wn wr))))
(define (match-names-in langs exprs n [wn 4] [incname #t] [wr #t])
  (map (λ (lang) 
         (let ([nlist (matches-names lang exprs n wn wr)]) 
           (if incname
               (list (Lang-name lang) nlist)
               nlist))) 
       langs))
(define langlist
  (list Lat Ertydon Dwarvish Kayfal Anavasi Aiha Aluvai Ceirene Mahlirou Obsidian 
        Elemental Peskae Gnomish Eriali Svaaric Gemstone Enemy))
(define short-langlist
  (list Lat Dwarvish Kayfal Anavasi Aiha Aluvai Mahlirou Obsidian Peskae Gnomish Eriali Gemstone))
(define fant-langlist
  (list Lat Dwarvish Mahlirou Obsidian Gnomish))

(define (text lang n [wn 4] [wr #t])
  (apply string-append (map (λ (s) (string-append s " ")) (words lang n wn wr))))
