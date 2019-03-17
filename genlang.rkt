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
(struct Lang (name setting syl rep raw) #:transparent)

(define-syntax deflang
  (syntax-rules ()
    [(deflang «name» «set» «syl» «rep»)
     (define «name»
       (Lang (quote «name») «set» «syl» «rep»
             (λ ([n 3])
               (apply string-append (build-list n (λ (n) ((Lang-syl «name»))))))))]))

(define (tamadh-replace wrd)
  (regexp-replaces 
   wrd
   '([#rx"O$" "u"] 
     [#rx"^w" "o"] [#rx"^y" "Y"] [#rx"bb" "b"] [#rx"pp" "p"] [#rx"xx" "x"]
     [#rx"a([AEIaeioOyJYju])" "A\\1"] [#rx"i([AEIaeioOyJYju])" "I\\1"] [#rx"e([AEIaeioOyJYju])" "E\\1"]
     [#rx"a" "â"] [#rx"A" "á"] [#rx"e" "ê"] [#rx"E" "é"] [#rx"i" "î"] [#rx"I" "í"] [#rx"o" "ó"] [#rx"O" "ä"] 
     [#rx"y" "ý"] [#rx"Y" "áì"] [#rx"j" "êì"] [#rx"J" "éì"] [#rx"u" "äu"] 
     [#rx"b" "dh"] [#rx"p" "th"] [#rx"x" "hr"])))

(deflang Tamadh "Viath"
  (λ () 
    (let* ([cn (RL 30 '(t m n s v r l k x d b p h f))]
           [vw (RL 30 '(A E I a e i o O y J Y j u w))]
           ; ^Categories^ vSyllable Typesv
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  tamadh-replace)

(deflang Silsi "Sile"
  (λ ()
    (let* ([cn '(s l n r f v m t d p k b dh th z)]
           [fcn (RL 15 (append cn '(h)))]
           [pcn (RL 15 (append cn '(nn rr ss ll ff mm zz)))]
           [avw (RL 30 '(é í á ä ê â î ú û ó))]
           [yvw (RL 30 '(í ê ó ä á é ú â))]
           [syls (RL 30 `((,fcn ,avw) (,avw ,pcn) (,fcn ,avw ,pcn) (,avw)
                                      (,fcn y ,yvw) (,fcn y ,yvw ,pcn) (y ,yvw ,pcn) (y ,yvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"lll+" "ll"] [#rx"rrr+" "rr"] [#rx"nnn+" "nn"] [#rx"mmm+" "mm"] 
                        [#rx"sss+" "ss"] [#rx"fff+" "ff"] [#rx"zzz+" "zz"]))))

(deflang Lat "generic"
  (λ ()
    (let* ([cn '(b c d f g h j k l m n p q r s t v w x y z 
                   ch sh th ts st)]
           [vw '(a e i o u y)]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  identity)

(deflang Phon "generic"
  (λ ()        ;               c ç       j
    (let* ([cn '(l r m n ŋ þ ð ʃ ʒ ц ч ծ ջ w y h
                   p b f v t d s z k g х γ)]
           [vw '(a e i o u y æ)]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw) (,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  identity)

(deflang Ainurin "Arda"
  (λ ()
    (let* ([cn '(l r m n h þ ð ʃ ʒ w y
                   p b f v t d s z k g х γ)]
           [vw '(a e i o u â ê ô û æ)]
           [syls (RL 30 `((,cn ,vw) (,vw ,cn) (,vw) (,cn ,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  identity)

(deflang Eivarne "Threefold" ; Eianvar
  (λ ()
    (let* ([cn '(v r l t k m n s sh f d p th b z)]
           [fcn (RL 15 (append cn '(w y)))]
           [pcn (RL 15 (append cn '(h)))]
           [vw (RL 30 '(e a i u o ei ai ae iu))]
           [syls (RL 30 `((,fcn ,vw) (,vw ,pcn) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Nuimena "Nuime"
  (λ ()
    (let* ([cn '(l r n m t d s th k v f h z dh sh zh kh)]
           [fcn (RL 15 (append cn '(p b)))]
           [pcn (RL 15 (append cn '(mb mp nd nt)))]
           [vw '(e i a u o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '(y)))]
           [syls (RL 30 `((,vvw ,pcn) (,fcn ,pvw) (,fcn ,pvw ,pcn) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#px"(.)\\1" "\\1"]))))

(deflang Ertydon "Elcenia"
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
 | m, k, s, p, r, v, sh, h, n, t, d, b, f, l, th, ch, G and Y in Ryganaavlan only
 |#
(deflang Leraal "Elcenia"
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
     '([#rx"h$" "th"] [#px"([^a])\\1" "\\1"] [#rx"aa(..?aa)" "a\\1"]
                      [#rx"ch([^aeiou])" "\\1"] [#rx"([^aeiou])ch" "\\1"]))))

(deflang Ryganaavlan-Leraal "Elcenia"
  (λ ()
    (let* ([cn '(m k s p r v sh n t d b f l th h ch g y)]
           [fcn (RL 15 (append '(h ch) cn))]
           [pcn (RL 15 (append '(h) cn))]
           [vw (RL 30 '(e a i aa o u y))]
           
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"h$" "th"] [#px"([^a])\\1" "\\1"] [#rx"aa(..?aa)" "a\\1"]
                      [#rx"ch([^aeiou])" "\\1"] [#rx"([^aeiou])ch" "\\1"]))))

(deflang Dwarvish "Thedas"
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

(deflang Kayfal "star wars"
  (λ ()
    (let* ([cn (RL 15 '(s k f c d t n l r m v h nd sk y st kh sh p b j w g z x))]
           [vw (RL 15 '(e a i u o))]
           [syls (RL 30 `((,cn ,vw) (,cn ,vw ,cn) (,vw ,cn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"^nd" "d"]))))

(deflang Anavasi "Facet"
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

(deflang Aiha "taieli"
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

(deflang Elannwyn "Quinn"
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

(deflang Aluvai "Suranse"
  (λ ()
    (let* ([cn '(s f z d v th t k p l n r m b sh ch w sk)]
           [fcn (RL 15 (append cn '(kh)))]
           [pcn (RL 15 (append cn '(ss rr)))]
           [vw (RL 30 '(i e a y u o ae ai))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Ceirene "Suranse"
  (λ ()
    (let* ([cn '(m r n l s t v c d k p f th b sh w ts sk)]
           [fcn (RL 15 (append cn '()))]
           [pcn (RL 15 (append cn '(ns tt)))]
           [vw (RL 30 '(a e i o u y ai ae))]
           [syls (RL 30 `((,vw ,pcn) (,fcn ,vw) (,fcn ,vw ,pcn) (,vw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Ruikni "Suranse"
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

(deflang Mahlirou "lindworm"
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

(deflang Obsidian "Thilanushinyel"
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

(deflang Peskae "not-wasteland"
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
     '([#rx"lll+" "ll"] [#rx"rrr+" "rr"] [#rx"sss+" "ss"] [#rx"zzz+" "zz"] [#rx"nnn+" "nn"]
                        [#rx"mmm+" "mm"] [#rx"fff+" "ff"] [#rx"vvv+" "vv"]))))

(deflang Gnomish "Eberron"
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

(deflang Svaaric "Alethia"
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

(deflang Gemstone "Indigo"
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
     '([#rx"sz" "ss"] [#rx"ds" "dz"] [#rx"tz" "ts"]))))

(deflang Nenastine "Nenassa"
  (λ ()
    (let* ([cn '(n l m r s v f t k d p b th sh z)]
           [fcn (RL 15 (append cn '(h)))]
           [pcn (RL 15 (append cn '()))]
           [vw '(a e i u o ai)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '(ya ye yi yu yo)))]
           [syls (RL 30 `((,fcn ,pvw) (,vvw ,pcn) (,vvw) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"^([^aeiou]*)y" "\\1"] [#rx"(y.)y" "\\1i"] [#rx"([^aeiou][^aeiou])y" "\\1i"]
                                  [#px"([aeiou])\\1" "\\1"]))))

(deflang Darall "Kitaloei"
  (λ ()
    (let* ([cn '(t f s d v p k b g r m l n z h)]
           [fcn (RL 15 cn)]
           [pcn (RL 15 '(mm nn ll rr))]
           [vw '(i e u o a)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Mirestava "shift-elements"
  (λ ()
    (let* ([cn '(l p t k m s d r n g v f z h b x j c
                   sk st)]
           [fcn (RL 15 (append cn '(pl tr pr dr tl sl br zr dl sr zl bl kr kl)))]
           [pcn (RL 15 (append cn '(mb mp nd nt)))]
           [vw '(a u i e o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Alticar "Dishonored"
  (λ ()
    (let* ([cn '(s t v k l d f n p r m h b z c q)]
           [fcn (RL 15 cn)]
           [pcn (RL 30 '(n l r m nd ld lt nt rd))]
           [vw '(e u a i o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Keriani "Ankeda"
  (λ ()
    (let* ([cn (RL 15 '(k t s f p d v z b h l r n m))]
           [cnx (RL 15 '(k t s f p d v z b))]
           [cnr (RL 30 '(l r))]
           [cnn (RL 30 '(n m))]
           [vw (RL 30 '(e a i u o))]
           [syls (RL 60 `((,cn ,vw) (,vw) (,cnx ,cnr ,vw) (,cn ,vw ,cnn) (,cnx ,cnr ,vw ,cnn) (,cn ,vw ,cnn ,cnx) (,cnx ,cnr ,vw ,cnn ,cnx)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#px"([aeiou])\\1" "\\1"]))))

(deflang Laantharei "Ankeda"
  (λ ()
    (let* ([cn '(r s k l f n m t p v d z)]
           [fcn (RL 15 cn)]
           [pcn (RL 30 (append cn '(ss ts nn)))]
           [vw '(a i u e o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#px"(.)\\1\\1" "\\1\\1"] [#rx"s[mn]" "ss"]))))

(deflang Celestial "[celestial guardians]"
  (λ ()
    (let* ([cn (RL 15 '(f v r t l p s k n d m b z h))]
           [cnx (RL 15 '(f v t p k d b))]
           [cnr (RL 30 '(l r s))]
           [cnn (RL 30 '(n m l s))]
           [vw (RL 30 '(e a i u o ya ye yi yo yu))]
           [syls (RL 60 `((,cn ,vw) (,vw) (,cnx ,cnr ,vw) (,cn ,vw ,cnn) (,cnx ,cnr ,vw ,cnn) (,cn ,vw ,cnn ,cnx) (,cnx ,cnr ,vw ,cnn ,cnx)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#px"([aeiou])\\1" "\\1"] [#rx"s([dbv])" "z\\1"] [#rx"([dbv])s" "\\1z"]
                                 [#rx"h([^aeiouy])" "\\1"]))))

(deflang Five "[video game afterlife]"
  (λ () ; nb: degenerate letters h (consonant), y (vowel)
    (let* ([cn '()] ; language does not work yet. consonants not implemented
           [cn1 '(p t þ k)]
           [cn2 '(f s c x)]
           [cn3 '(b d ð g)]
           [cn4 '(v z j q)]
           [cn5 '(m l n r)]
           [vw '(o u a e i)]
           [syls (RL 60 `((,cn ,vw) (,vw) (,vw ,cn) (,cn ,vw ,cn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Avirisei "Nightmare"
  (λ ()
    (let* ([cn (RL 15 '(t f r s v l p n k m d z dz b ts))]
           [cnx (RL 15 '(t f s v p k d z b))]
           [cnr (RL 30 '(r l s))]
           [cnn (RL 30 '(l n m s))]
           [vw (RL 30 '(a e u i o))]
           [syls (RL 60 `((,cn ,vw) (,vw) (,cnx ,cnr ,vw) (,cn ,vw ,cnn) (,cnx ,cnr ,vw ,cnn) (,cn ,vw ,cnn ,cnx) (,cnx ,cnr ,vw ,cnn ,cnx)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#px"([aeiou])\\1" "\\1"] [#rx"s([dbv])" "z\\1"] [#rx"([dbv])s" "\\1z"]
                                 [#rx"h([^aeiouy])" "\\1"]))))

(deflang Arivath "Corth" ; Arifea/Arifene, Ashari
  (λ () 
    (let* ([cn '(f t n s v l d th r p k m sh z zh b dh kh g)]
           [fcn (RL 15 (append '(h) cn '(ts tc dz dj)))]
           [pcn (RL 15 cn)]
           [vw '(a o i e u ya yo yi ye yu)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"hh" "h"]))))

(deflang Parikai "Sunfire"
  (λ ()
    (let* ([cn '(s v f t p l z n k m h r d b g w tc dj x q)]
           [fcn (RL 15 cn)]
           [pcn (RL 15 '(l r n m))]
           [vw '(a i e u o)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Vàjo "Kelovea"
  (λ ()
    (let* ([cn '(l m n r v k h ʒ t j d dy ty f s z c x)]
           [fcn (RL 15 cn)]
           [pcn (RL 15 (append cn '(g)))]
           [vw '(u i e á o à a å ai au eá ei oi áu iá uá)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw '()))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '([#rx"(d|j|s|sh|ty)$" "\\1"] [#rx"g(.+)$" "\\1"]
                                   [#rx"([^uieáoàaår])([^uieáoàaåy])" "\\1"]
                                   [#rx"ʒ" "zh"] [#rx"c" "sh"] [#rx"x" "ch"]))))

(deflang Avikana "Amenta"
  (λ ()
    (let* ([cn '(k m n d v t p s r l h c g)]
           [fcn (RL 15 cn)]
           [vw '(a i e o u)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw vw '(ya yi ei yo yu)))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))

(deflang Cubist "sex cube"
  (λ ()
    (let* ([cn '(k m n d sh z b r l t f v p zh s)]
           [fcn (RL 15 (append cn '(h)))]
           [pcn (RL 15 cn)]
           [vw '(e a u i)]
           [vvw (RL 30 (append vw '()))]
           [pvw (RL 30 (append vw vw '(ei ai ya yi yu)))]
           [syls (RL 60 `((,fcn ,pvw) (,vvw) (,vvw ,pcn) (,fcn ,pvw ,pcn)))])
      (apply string-append (map symbol->string (map one-of (one-of syls))))))
  (λ (wrd)
    (regexp-replaces
     wrd
     '())))


(define (word lang [wn 4] [wr #t])
  ((Lang-rep lang) ((Lang-raw lang) (if wr (add1 (random wn)) wn))))
(define (name lang [wn 4] [wr #t])
  (string-titlecase (word lang wn wr)))

(define (words lang n [wn 4] [wr #t])
  (build-list n (λ (n) (word lang wn wr))))
(define (names lang n [wn 4] [wr #t])
  (map string-titlecase (words lang n wn wr)))

(define max-tries 5000)

(define (matching-name lang expr [wn 4] [wr #t] [iter 0])
  (let ([nm (name lang wn wr)])
    (if (regexp-match? expr nm)
        nm
        (if (> iter max-tries)
            ""
            (matching-name lang expr wn wr (add1 iter))))))
(define (matching-names lang expr n [wn 4] [wr #t])
  (build-list n (λ (n) (matching-name lang expr wn wr))))

(define (matches-name lang exprs [wn 4] [wr #t] [iter 0])
  (let ([nm (name lang wn wr)])
    (if (andmap (λ (expr) (regexp-match? expr nm)) exprs)
        nm
        (if (> iter max-tries)
            ""
            (matches-name lang exprs wn wr (add1 iter))))))
(define (matches-names lang exprs n [wn 4] [wr #t])
  (build-list n (λ (n) (matches-name lang exprs wn wr))))
(define (match-names-in langs exprs n [wn 4] [incname #t] [wr #t])
  (map (λ (lang) 
         (let ([nlist (matches-names lang exprs n wn wr)]) 
           (if incname
               (list (Lang-name lang) nlist)
               nlist))) 
       langs))

(define (quick-staple piece langs [wn 4] [wr #t])
  (map (λ (l) (string-append piece (word l wn wr))) langs))
(define (staple piece langs [exprs '()] [wn 4] [wr #t])
  (map (λ (l) 
         (string-titlecase 
          (string-append piece 
                         (matches-name l exprs wn wr))))
       langs))
(define (quick-tail piece langs [wn 4] [wr #t])
  (map (λ (l) (string-titlecase (string-append (word l wn wr) piece))) langs))

(define langlist
  (list Lat Eivarne Nuimena Ertydon Dwarvish Kayfal Anavasi Aiha Aluvai Ceirene Ruikni Mahlirou 
        Obsidian Peskae Gnomish Svaaric Gemstone Nenastine Darall Mirestava Alticar Keriani
        Laantharei Celestial Avirisei Arivath Parikai Avikana Cubist))
(define short-langlist
  (list Lat Nuimena Dwarvish Kayfal Anavasi Aiha Aluvai Mahlirou Obsidian Peskae Gnomish Gemstone))
(define fant-langlist
  (list Lat Dwarvish Mahlirou Obsidian Gnomish Nenastine))
(define elc-langlist
  (list Ertydon Leraal Ryganaavlan-Leraal))
(define weird-langlist
  (list Tamadh Silsi Phon Ainurin Vàjo))

(define get-patterns
  (let ([patternmap
         (hash
          ; Kappa
          'Aral     '(#rx"^A" #rx"a[^aeiou]*[aeiouy]*$")
          'Serg     '(#rx"^[TSZ]" #rx"r" #rx"[kcqg]")
          'Tomis    '(#rx"^[TSZ]")
          'Dorca    '(#rx"^D" #rx"r" #rx"[ck]")
          'Rosti    '(#rx"^R" #rx"[sz]" #rx"[td]")
          'Yuri     '(#rx"ri")
          'Xav      '(#rx"[SsZz][aeiouy]*[vfp]")
          'Gregor   '(#rx"([Oo]r)|([Rr]o)")
          'Ezar     '(#rx"^E[aeiouy*][stdz]")
          'Dalibor  '(#rx"^[DZTS]" #rx"[lr].*[pvfb].*[lr]")
          'Vorbarra '(#rx"[AEae]r")
          'Kosigan  '(#rx"^K[aeiouy]*[stzd]")
          'Piotr    '(#rx"^[PFBV]" #rx"[tdsz]" #rx"[lr]")
          'Miles    '(#rx"^[AEISMNTRLF]" #rx".[lr].")
          'Aaron    '(#rx"^[AEIOU]" #rx"n$")
          'Maran    '(#rx"[Mm]ar")
          'Tarro    '(#rx"[Tt]" #rx"[lro]")
          'Luar     '(#rx"[Ll]" #rx"[Rr]" #rx"[^aeiou]$")
          'Nimire   '(#rx"^[AEIOUYMNLR][aeiouymnlr]*$" #rx"i")
          'Eights   '(#rx"[Aa]nn?[aei]")
          'Chainsaw '(#rx"[sz][aeiouy]*n.*[td]")
          'Sefton   '(#rx"[SsTt].*[pfbv]")
          'Ga*el    '(#rx"^[CGK]" #rx"[mnrl]")
          'Libby    '(#rx"[Ll][aeiou]*[sz]")
          'Lynne    '(#px"(.)\\1")
          'Chantal  '(#rx"[LlNn]" #rx"^[^r]*$")
          
          ; Moriwen
          'Zari     '(#rx"^[SZT]h?[ai]+[^szt][ai]")
          
          ; Maggie
          'Anise    '(#rx"^A[aeiouy]*nn?[aeiouy]*s")
          'Sawyer   '(#rx"^S.*r")
          'Ante     '(#rx"^A" "n" "t")
          'Alle     '(#rx"(^Al)|(al[aeiouy]*$)")
          'Liri     '(#rx"[Ll][iy]r")
          'Sissy    '(#rx"[CScs][iy]s")
          'Mirelle  '(#rx"^M[aeiouy]*r+[aeiouy]*l+")
          'Endevar  '(#rx"^En" #rx"[vf]")
          'Hazel    '(#rx"^H[aeiouy]+([fsvz]+|[tdsz]h)[aeiouy]+[lr]")
          'Khythen  '(#rx"^[KC]h?[aeiouy]+[^aeiouy]+[aeiouy]+n")
          
          ; Aestrix
          'Yvette   '(#rx"[Vv]et")
          'Nadia    '(#rx"[Aa]" #rx"[Ii]" #rx"[Dd]" #rx"[Nn]")
          
          ; Root
          'Kazi     '(#rx"^[KZ]" #rx"[Zz]")
          )])
    (λ (s) (hash-ref patternmap s '()))))

(define (text lang [n 50] [wn 4] [wr #t])
  (apply string-append (map (λ (s) (string-append s " ")) (words lang n wn wr))))
