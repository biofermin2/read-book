#!/usr/bin/sbcl

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(load "/home/hiro/howm/junk/utils.lisp") ; => T

(defun set-tts (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせたttsをセットし読み上げる。"
	(unless (null lst)
		(if (standard-char-p (car lst)) ;リストの先頭の文字が英数字ならば
				(progn
					(setq tts "festival --tts")	;yes　festival 改行は削除しない。dotと読み上げてしまう。
					)				;	英語の文字列を単語単位で辞書データから意味を引っ張ってhtml化する処理
				(progn
					(setq tts "ojtalk"))) ;no ojtalk
		(setq lst (delete #\Newline lst))
		(format t "~A~%" lst)
		(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts)))) ; => SET-TTS

;;					(setq lst (delete #\Newline lst)) ;ojtalkの時は改行を削除（止まるので。）

(defun read-book (file)
	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
	(let ((lst ()))
		(with-open-file (in file :direction :input)
			(do ((char (read-char in nil 'eof)
								 (read-char in nil 'eof)))
					((eql char 'eof))
				(when (or (and (not (null lst)) ;lstが空ではなく、かつ
											 (not (eq (type-of char) (type-of (car lst))))) ;前のchar≠今のchar
									(eq (car lst) #\。)
									(eq (car lst) #\.)
									(eq (car lst) #\！)
									(eq (car lst) #\!)
									(eq (car lst) #\？)
									(eq (car lst) #\?)
									(eq (car lst) #\:)
									(eq (car lst) #\;)
									(eq char #\Space)
									)
				(set-tts (nreverse lst))
				(setq lst ()))
				(setq lst (cons char lst))))))	; => READ-BOOK
;; (eq char #\)を廃止して(eq (car lst) #\)に変更　[2017-04-19 20:33:17]
;; その方が綺麗にセンテンス毎にS式が纏まる。
;; それまでは記号だけが残ったため。
;; (eq char #\Space);単語毎分割出来るが読むには不便


;(read-book "~/test.txt")								; =>

(car lst)																; => #\i
(setq word 'a)													; => A
(setq word 'b)													; => B
word																		; => B
(push 'a word)													; => (A . B)
(push 'b word)													; => (B A . B)

(let ((acc nil)
			(ls nil)
			(lst '(#\i #\s #\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s
 #\h #\?)))
	(labels ((word-set (lst)
						 (if (eq (car lst) #\Space)
								 (push acc ls)
								 (progn
									 (push (car lst) acc)
									 (setq lst (cdr lst))
									 (word-set (lst))))))
		(format t "~A" (reverse ls))))			; => NILNIL
acc																			; => (#\s #\i #\i)

ls																			; => ((#\s #\i #\i))
lst																			; => (#\i #\s #\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s #\h #\?)

NIL
NIL


acc																			; => (#\i)
(car lst)																; => #\
(if (eq (car lst) #\Space)
		nil
		t)																	; => NIL
(setq ls nil)														; => NIL
(push acc ls)														; => ((#\s #\i #\i))
(car lst)																; => #\s
(push (car lst) acc)										; => (#\s #\i #\i)
(setq lst (cdr lst))										; => (#\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s #\h #\?)
#\?)
()
(format t "~A" (reverse ls))						; => ((s i i))NIL

(defun word-set (lst)
	(let ((acc nil)
				(ls nil))
		(if (eq (car lst) #\Space)
				(progn
					(setq ls acc)
					(format t "~A" ls))
				(progn
					(push (car lst) acc)
					(word-set (cdr lst))))))			; => WORD-SET
(word-set lst)													; => NILNIL
ls																			; =>
(setq acc nil)													; => NIL
(push (car lst) acc)										; => (#\i)


(defun argv ()
  (or
   #+clisp (ext:argv)
   #+sbcl sb-ext:*posix-argv*
   #+abcl ext:*command-line-argument-list*
   #+clozure (ccl::command-line-arguments)
   #+gcl si:*command-args*
   #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
   #+cmu extensions:*command-line-strings*
   #+allegro (sys:command-line-arguments)
   #+lispworks sys:*line-arguments-list*
   nil))

(defun main ()
	(let ((file-name (third (argv))))
		(if (equal nil file-name)
				(format t "You did not supply a file name")
				(read-book file-name))))

(main)																	; =>



;; 英語の辞書を登録するハッシュテーブルを定義（大文字小文字区別なし）
(boundp '*edic*)																			 ; => NIL
(defparameter *edic* (make-hash-table :test #'equalp)) ; => *EDIC*



;; ハッシュテーブルに辞書データを登録する関数の定義
(defun make-dic (file)
	(with-open-file (str file :direction :input :if-does-not-exist nil)
									(do ((line (read-line str)
														 (read-line str nil 'eof)))
											((eq line 'eof))
										(let* ((s-line (split-string line #\Space))
													 (key (string-right-trim '(#\tab) (car s-line)))
													 (val (cdr s-line)))
											(setf (gethash key *edic*) val))))) ; => MAKE-DIC
;; 指定辞書ファイルをハッシュテーブルに登録
(make-dic "/home/hiro/howm/ejdic-hand-utf8.txt") ; => NIL
(gethash "zzz" *edic*)													 ; => ("グーグー(いびきの音)")
T
;; 英語の部分の処理
(setq lst '(i s   i t   a l r e a d y   f i n i s h ?)) ; => (I S I T A L R E A D Y F I N I S H ?)
(car lst)																								; => I
(setq lst (cdr lst))																			; => (S I T A L R E A D Y F I N I S H ?)
(car lst)																									; => S
(setq lst (cdr lst))																				; => (I T A L R E A D Y F I N I S H ?)
;;(cons (car lst) (gethash (car lst) *edic*)) ; => (IT)
;;(gethash (car lst) *edic*)									; => NIL
;;NIL
;;(write-to-string (car lst))							; => "IT"
;; 成功！
;;(gethash (write-to-string (car lst)) *edic*) ; => ("《すでに述べられた物・事または幼児,動物などを指

;;(cons (write-to-string (car lst)) (gethash (write-to-string (car lst)) *edic*)) ; => ("IT" "《すでに
;;冗長すぎるので、

;;(cons key (gethash key *edic*))					; => ("IT" "《すでに述べられた物・事または幼児,動物などを指
;;(setq val (gethash key *edic*))					; => ("《すでに述べられた物・事または幼児,動物などを指して》


(setq key (write-to-string (car lst)))	; => "I"
(setq val (car (gethash key *edic*)))		; => "iodineの化学記号"
(cons key val)													; => ("IT" . "《すでに述べられた物・事または幼児,動物などを指して》『それは』(『を』),そのものは(を),そのことは(を)")



;; 失敗
;; (let* ((lst (list 'it's 'too 'late))
;; 			(key (write-to-string (car lst)))
;; 			(val (car (gethash key *edic*))))
;; 	(apply #'(lambda (key val) (push (cons key val))) lst)) ; =>
;; (defun make-cons (lst)
;; 	(let* ((key (write-to-string (car lst)))
;; 				 (val (car (gethash key *edic*))))
;; 		(mapcar #'(lambda (key val) (cons key val)) lst))) ; => MAKE-CONS



(list 'it's 'too 'late)									; => (IT S TOO LATE)
(funcall #'+ 1 2 3 4 5)									; => 15
(apply #'+ '(1 2 3 4))									; => 10
(mapcar #'+ '(1 2 3 4 5))								; => (1 2 3 4 5)
(mapcar #'- '(1 2 3 4 5 6 7))														; => (-1 -2 -3 -4 -5 -6 -7)
(mapcar #'not '(t nil t nil t nil))											; => (NIL T NIL T NIL T)
((mapcar #'cons )																				; => NIL

(mapcar #'(lambda (lst)
						(let ((key (write-to-string (car lst)))
									(val (car (gethash key *edic*))))
							(push (cons key val) ())
							(setq ))))								; =>
																				;
(listp lst)															; => T
(listp '(it's too late))								; => T
(setq lst '(it's too late))							; => (IT 'S TOO LATE)

dolistを使うか？
mapcar?
(setq lst (cdr lst))<-これも使えるか?
(car lst)
(mapcar #')


(list (cons 'a 'b) (cons 'c 'd))				; => ((A . B) (C . D))
(list (cons "a" "b") (cons "c" "d"))		; => (("a" . "b") ("c" . "d"))
(setq lst2 (list (cons "a" "b") (cons "c" "d"))) ; => (("a" . "b") ("c" . "d"))
(push (cons "e" "f") lst2)											 ; => (("e" . "f") ("a" . "b") ("c" . "d"))
(push lst2 (cons "x" "y"))											 ; => error

(setq cons-data '(("試験　テスト" . "test")
									("くず　廃品　無価値なもの　安っぽい" . "junk")
									("自宅　居宅　住居　故郷　本拠地" . "home")) ; => (("試験　テスト" . "test") ("くず　廃品　無価値なもの　安っぽい" . "junk")

;;成功！ ((key .val)(key . val))-> <span title="val">key</span>
(defun make-html (c)
		(cl-who:with-html-output (*standard-output*)
			(loop for (key . val) in c
				 do (htm (:head (:title "翻訳ページ"))
							 (:body (:span :title val (str key))))))) ; => MAKE-HTML
(make-html cons-data)																		; =>



T
(gethash "@" *edic*)										; => ("単価…で/…につき")
T
(gethash "")
(write-to-string 'it)										; => "IT"


;; htmlをfileに書き込む
(defun write-html (file)
(with-open-file (out file
										 :direction :output
										 :if-exists :append
										 :if-does-not-exist :create)
	(format out "~A" (make-html cons-data))))				; => WRITE-HTML
(write-html "test.html")								; => <head><title>翻訳ページ</title></head><body><span title='試験　テスト'>test</span></body><head><title>翻訳ページ</title></head><body><span title='くず　廃品　無価値なもの　安っぽい'>junk</span></body><head><title>翻訳ページ</title></head><body><span title='自宅　居宅　住居　故郷　本拠地'>home</span></body>NIL
																				; =>
;; cl-whoの使い方について実験
(ql:quickload :cl-who)									; => To load "cl-who":
  Load 1 ASDF system:
    cl-who
; Loading "cl-who"

(use-package :cl-who)										; => T
(in-package :cl-who)										; => #<PACKAGE "CL-WHO">
(setq txt (cons meaning lst))						; =>

(defun make-html ()
		(cl-who:with-html-output (*standard-output*)
		(loop for (meaning . word) in '(("試験　テスト" . "test")
																		("くず　廃品　無価値なもの　安っぽい" . "junk")
																		("自宅　居宅　住居　故郷　本拠地" . "home"))
			 do (htm (:head (:title "翻訳ページ"))
							 (:body (:span :title meaning (str word))))))) ; => MAKE-HTML
(make-html)																									 ; => <head><title>翻訳ページ</title></head><body><span title='試験　テスト'>test</span></body><head><title>翻訳ページ</title></head><body><span title='くず　廃品　無価値なもの　安っぽい'>junk</span></body><head><title>翻訳ページ</title></head><body><span title='自宅　居宅　住居　故郷　本拠地'>home</span></body>NIL
																				; <head><title>翻訳ページ
																				; </title></head><body><span title='試験
																				; 　テス
																				; ト'>test</span></body><head><title>翻
																				; 訳ページ</title></head><body><span
																				; title='くず　廃品　無価値なもの　安っ
																				; ぽい'>junk</span></body><head><title>
																				; 翻訳ページ</title></head><body><span
																				; title='自宅　居宅　住居　故郷　本拠
																				; 地'>home</span></body>NIL



(cl-who:with-html-output (*standard-output*)
  (loop for (link . title) in '(("http://zappa.com/" . "Frank Zappa")
                                ("http://marcusmiller.com/" . "Marcus Miller")
                                ("http://www.milesdavis.com/" . "Miles Davis"))
        do (htm (:span :title link
                  (str title))
                )))											; =>

(defun translate-page (title)
(let ((title "□□物語"))
	(cl-who:with-html-output-to-string
		(str nil :prologue t)
	(:html (:head (:title (str title)))
				 (:body (:h1 "第一章"))))))			; => TRANSLATE-PAGE
(trans)
(cl-who:with-html-output (*standard-output*)
  (loop for (meaning . word) in '(("http://zappa.com/" . "Frank Zappa")
                                ("http://marcusmiller.com/" . "Marcus Miller")
                                ("http://www.milesdavis.com/" . "Miles Davis"))
        do (htm (:span :title meaning
                  (str word))						; =>
                )))											; => <span title='http://zappa.com/'>Frank Zappa</span><span title='http://marcusmiller.com/'>Marcus Miller</span><span title='http://www.milesdavis.com/'>Miles Davis</span>NIL

(package-name *package*)								; => "CL-WHO"
(use-package :cl-who)										; => T

(symbol-package 'with-html-output)			; => #<PACKAGE "CL-WHO">

;; 成功！
(let ((ls nil))
	(dolist (x lst)
		(when (eq x #\Space)
			(setq x #\tab))
		(push x ls))
	(format t "~A~%" (reverse ls)))				; => (i s 	 i t 	 a l r e a d y 	 f i n i s h ?)


(let ((key nil)
			(ls nil))
	(dolist (char lst)
		(if (eq char #\Space)
			(progn
				(setq val (car (gethash key *edic*)))
				(setq key (write-to-string key))
							(push ls (cons key val)))
			(progn
				(push char key)))))							; =>

(cons 'a 'b)														; => (A . B)

(setq key (write-to-string (car lst)))	; => "I"
(setq val (car (gethash key *edic*)))		; => "iodineの化学記号"

(setf lst '(#\i #\s #\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s #\h #\?)) ; => (#\i #\s #\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s
 #\h #\?)
																				;#\h #\?)
(car '("is" "it"))											; => "is"
lst																			; => (#\i #\s #\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s
#\h #\?)
(setq lst '(i s   i t a l r e a d y   f i n i s h e d ?)) ; => (I S I T A L R E A D Y F I N I S H E D ?)

(let ((key nil)
			(val nil)
			(ls nil)
			(lst (split-by-one-space (concatenate 'string lst))))
	(dolist (char lst)
		(if (eq char #\Space)								; =>
				(progn
					(setq val (car (gethash key *edic*)))
					(setq ls (push (cons key val) ls))
					(format t "key=~a,val=~a,ls=~a" key val ls))
			(push char key))))										; => NIL
																				; =>

(setq lst (split-by-one-space (concatenate 'string lst))) ; =>
lst																												; => ("is" "it" "already" "finish?")

;; 成功
(let ((ls nil))
(dolist (key lst)
	(setf val (gethash key *edic*))
	(push (cons key val) ls)
	(print ls)))													; =>
(("is" "beの三人称・単数・現在"))
(("it" "《すでに述べられた物・事または幼児,動物などを指して》『それは』(『を』),そのものは(を),そのことは(を)" "/"
  "《その場の状況で相手に何であるか分かるような物・事または人を指して》『それは』(『を』),そのことは(を)" "/"
  "《天候・時間・距離・事情・状態などを指して》" "/" "《It" "seems(happens," "appears…" ")thatなどの形で》"
  "/" "《形式主語として》" "/" "《形式目的語として》" "/" "《It" "is" "…" "that(who,"
  "which)の形で…を強調して》" "/" "《ある種の動詞・前置詞に添える形式上の目的語として》" "/" "〈C〉(遊戯の)鬼" "/"
  "〈U〉《話》理想,第一人者" "/" "〈U〉《話》性的魅力")
 ("is" "beの三人称・単数・現在"))
(("already" "《肯定文で》『もう』,『すでに』" "/" "《驚き・意外の意を表して》《疑問文で》もう,早くも;《否定文で》まさか")
 ("it" "《すでに述べられた物・事または幼児,動物などを指して》『それは』(『を』),そのものは(を),そのことは(を)" "/"
  "《その場の状況で相手に何であるか分かるような物・事または人を指して》『それは』(『を』),そのことは(を)" "/"
  "《天候・時間・距離・事情・状態などを指して》" "/" "《It" "seems(happens," "appears…" ")thatなどの形で》"
  "/" "《形式主語として》" "/" "《形式目的語として》" "/" "《It" "is" "…" "that(who,"
  "which)の形で…を強調して》" "/" "《ある種の動詞・前置詞に添える形式上の目的語として》" "/" "〈C〉(遊戯の)鬼" "/"
  "〈U〉《話》理想,第一人者" "/" "〈U〉《話》性的魅力")
 ("is" "beの三人称・単数・現在"))
(("finish?")
 ("already" "《肯定文で》『もう』,『すでに』" "/" "《驚き・意外の意を表して》《疑問文で》もう,早くも;《否定文で》まさか")
 ("it" "《すでに述べられた物・事または幼児,動物などを指して》『それは』(『を』),そのものは(を),そのことは(を)" "/"
  "《その場の状況で相手に何であるか分かるような物・事または人を指して》『それは』(『を』),そのことは(を)" "/"
  "《天候・時間・距離・事情・状態などを指して》" "/" "《It" "seems(happens," "appears…" ")thatなどの形で》"
  "/" "《形式主語として》" "/" "《形式目的語として》" "/" "《It" "is" "…" "that(who,"
  "which)の形で…を強調して》" "/" "《ある種の動詞・前置詞に添える形式上の目的語として》" "/" "〈C〉(遊戯の)鬼" "/"
  "〈U〉《話》理想,第一人者" "/" "〈U〉《話》性的魅力")
 ("is" "beの三人称・単数・現在")) NIL

	"is"
"it"
"already"
"finish?" NIL


(setq lst (list (concatenate 'string lst))) ; =>
(split-by-one-space lst)										; => (("is it already finish?"))
(car (list (concatenate 'string lst)))	; => "is it already finish?"
(car '("is" "it"))											; => "is"

(concatenate 'string lst)														 ; => "is it already finish?"
(split-by-one-space  (concatenate 'string lst))			 ; => ("is" "it" "already" "finish?")

(split-by-one-space "singing in the rain") ; => ("singing" "in" "the" "rain")
(#\f #\i #\n #\i #\s #\h #\?))

	(setq lst (concatenate 'list lst))			; => (#\i #\s #\  #\i #\t #\  #\a #\l #\r #\e #\a #\d #\y #\  #\f #\i #\n #\i #\s
 #\h #\?)
(setq lst '(#\s #\i))										; => (#\s #\i)
(nreverse lst)													; => (#\i #\s)
(setq lst (reverse lst))								; => (#\i #\s)
lst																			; => (#\i #\s)
(format t "~A" (car (reverse lst)))			; => sNIL
(subst " " "" lst)											; => (#\i #\s)
(write-to-string lst)										; => "(#\\i #\\s)"
(write-to-string (reverse lst))					; => "(#\\i #\\s)"
(concatenate 'string lst)								; => "is"
(delete '! '(it's too late!))						; => (IT 'S TOO LATE!)
