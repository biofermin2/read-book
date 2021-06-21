#!/usr/bin/sbcl
;; 英語の箇所だけ単語単位でwebAPIを叩いて、翻訳し、その翻訳
;;内容を取得。その翻訳情報を<span title= "<info>">word</span>
;;というhtmlで囲う。
;;日本語箇所はすっ飛ばす。

;; 高速だが実行時の検査を省略していてデバッグもしづらい
;; 最終的なリリース向け
(declaim (optimize (speed 3) (debug 0) (safety 0)))
;; 実行時に検査をしてデバッグもしやすいが遅い
;; 開発途中のデバッグ向け
;; (declaim (optimize (speed 0) (debug 3) (safety 3)))
(setq lst '(#\c #\a #\n #\d #\i #\d #\a #\t #\e)) ; => (#\c #\a #\n #\d #\i #\d #\a #\t #\e)

(ql:quickload :trivial-shell)						; => To load "trivial-shell":
; Loading "trivial-shell"

(defun system (cmd-str)
 	(trivial-shell:shell-command cmd-str)) ; => SYSTEM

;; 成功
(defun strike-api (lst)
	"リストの文字列が日本語か英語を判定し
英語ならwebAPIに投げ意味を取得。"
	(unless (null lst)
		(when (standard-char-p (car lst)) ;リストの先頭の文字が英数字ならば
					(setq meaning (system (concatenate 'string "w3m \"http://eow.alc.co.jp/" lst "/UTF-8/?ref=sa\" | tail --lines=+35 | head --lines=-62")))))) ; => STRIKE-API
(strike-api lst)												; => "
;; 失敗
(defun strike-api2 (lst)
	"リストの文字列が日本語か英語を判定し英語ならwebAPIに投げ意味を取得。"
	(unless (null lst)
		(when (standard-char-p (car lst)) ;リストの先頭の文字が英数字ならば
					(setq meaning (system (concatenate 'string "wget \'https://glosbe.com/gapi/translate?from=en&dest=ja&format=json&phrase=" lst	"&pretty=true\'"))))))
;;(strike-api2 lst)											 	; =>
(system (concatenate 'string "wget \'https://glosbe.com/gapi/translate?from=en&dest=ja&format=json&phrase=" lst	"&pretty=true\'")) ; => ""

(system "translate test")


;; htmlを書き込む
(defun write-html (file)
(with-open-file (out file
										 :direction :output
										 :if-exists :append
										 :if-does-not-exist :create)
	(format out "~A" (make-html))))				; => WRITE-HTML
(write-html "test.html")								; => <head><title>翻訳ページ</title></head><body><span title='試験　テスト'>test</span></body><head><title>翻訳ページ</title></head><body><span title='くず　廃品　無価値なもの　安っぽい'>junk</span></body><head><title>翻訳ページ</title></head><body><span title='自宅　居宅　住居　故郷　本拠地'>home</span></body>NIL
																				; =>
;; cl-whoの使い方について実験
(ql:quickload :cl-who)									; => To load "cl-who":
(in-package :cl-who)										; => #<PACKAGE "CL-WHO">

(setq txt (cons meaning lst))						; =>

;; ここで出てくるリストに関してはformatの~Sでつくる
(format t "~S" 1 3)											; => 1
(print 'a)															; =>
A A
(princ 'a)															; => AA
(prin1 'a)															; => AA


;; これ使えそう
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
                )))											; => <span
																				; title='http://zappa.com/'>Frank
																				; Zappa</span><span
																				; title='http://marcusmiller.com/'>Marcus Miller</span><span title='http://www.milesdavis.com/'>Miles Davis</span>NIL




;; 空白文字はstandard-charになる。
(standard-char-p #\ )										; => T
(standard-char-p #\Space)								; => T





(concatenate 'string "w3m 'http://eow.alc.co.jp/" lst "/UTF-8/?ref=sa' | tail --lines=+35 | head --lines=-62") ; => "w3m 'http://eow.alc.co.jp/can/UTF-8/?ref=sa' | tail --lines=+35 | head --lines=-62"


(defun trans-book (file)
	"日英の判定をせず各言語毎に文字列をまとめ、
切り替わるタイミングでstrike-apiに渡す。"
	(let  ((lst ()))
		(with-open-file (in file :direction :input)
			(do ((char (read-char in nil 'eof)
								 (read-char in nil 'eof)))
					((eql char 'eof))
				(when (or (and (not (null lst)) ;lstが空ではなく、かつ
											 (not (eq (type-of char) (type-of (car lst))))) ;前≠現char
									(eq char #\Space)
									(eq char #\.)
									(eq char #\!)
									(eq char #\?)
									(eq char #\:)
									(eq char #\;)
									)
					(strike-api lst)
					(print lst)
					(setq lst ()))
				(setq lst (cons char lst))))))

(eq #\　 #\ )														; => NIL
(eq #\Space #\　)												; => NIL
;;つまり全角スペースは半角スペースと同じではないし、
;; #\Spaceとも同じではない。
(eql #\  #\　)													; => NIL
(equal #\  #\　)												; => NIL
(equalp #\  #\　)												; => NIL
(char-equal #\  #\　)										; => NIL
(type-of #\　)													; => EXTENDED-CHAR
(type-of #\ )														; => STANDARD-CHAR
(type-of #\Space)												; => STANDARD-CHAR
(eq #\  #\Space)												; => T
(setq char #\Space)											; => #\
				(eq char #\ )										; => T
;;standard-char かつ　spaceではない
(and (standard-char-p char) (not (eq char #\ ))) ; => NIL


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
;;(read-book (cadr (argv)))										; =>
;(read-book (third (argv)))										; =>

;;(parse-integer "42")										; => 42

(defun main ()
	(let ((file-name (third (argv))))
		(if (equal nil file-name)
				(format t "You did not supply a file name")
				(read-book file-name))))
;; argvは("/usr/bin/sbcl" "read-book-v2.2.lisp" "~/test.txt")
;; という感じで表示されるので、ここで読み込みたい引数はthirdとなる。
(main) ;mainの実行
;; script化成功！[2017-04-03 21:24:01]
;; echo '   '| festival --tts
;; とやっているので、echo ' ' '| festival --tts
;; みたいになるとエラーでストップしてしまう。
;; アポストロフィーのsの時は致命的。
;; [2017-04-06 16:24:43]修正済

;; 数字に関しては半角は英語読み全角は日本語読みになっているが、
;; 前後の文字列による自動判別はできないものだろうか？
;; 今後の課題だ。

(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts))
(setq word 'test)												; => TEST
word																		; => TEST

(asdf:run-shell-command (concatenate 'string "translate " "word")) ; => 1
(sb-ext:run-program "/usr/local/bin/translate" '("test") :output *standard-output*) ; => Traceback (most recent call last):
  File "/usr/local/bin/translate", line 43, in <module>
    print phrase_jp
UnicodeEncodeError: 'ascii' codec can't encode characters in position 0-1: ordinal not in range(128)
#<SB-IMPL::PROCESS :EXITED 1>

(ql:quickload :cl-htmlgen)							; =>
(ql:system-apropos "html")							; => #<SYSTEM buildnode-html5 / buildnode-20150113-git / quicklisp 2017-01-24>
(ql:quickload :htmlgen)									; => To load "htmlgen":
(ql:quickload :cl-project)							; => To load "cl-project":
  Load 1 ASDF system:
    cl-project
; Loading "cl-project"
(:CL-PROJECT)

;(system "pwd")													 ; => "/home/hiro/howm/junk

;(system "w3m http://yahoo.co.jp")
																				; => 成功！
(ql:quickload :dexador)									; =>
(require 'asdf)													; => NIL
(require 'asdf-install)									; =>

(system "/usr/bin/python /home/hiro/translate.py test") ; => ""

(sb-ext:run-program "/usr/bin/python" '("/home/hiro/core/bin/translate.py"
																				"test") :output *standard-output*) ; => Traceback (most recent call last):
  File "/home/hiro/core/bin/translate.py", line 43, in <module>
    print phrase_jp
UnicodeEncodeError: 'ascii' codec can't encode characters in position 0-1: ordinal not in range(128)
#<SB-IMPL::PROCESS :EXITED 1>
#<SB-IMPL::PROCESS :EXITED 2>
#<SB-IMPL::PROCESS :EXITED 0>
File "/home/hiro/translate.py", line 43, in <module>
    print phrase_jp
UnicodeEncodeError: 'ascii' codec can't encode characters in position 0-1: ordinal not in range(128)
#<SB-IMPL::PROCESS :EXITED 1>

(sb-ext:run-program "/usr/local/bin/translate" '("test") :output *standard-output*) ; => Traceback (most recent call last):
  File "/usr/local/bin/translate", line 43, in <module>
    print phrase_jp
UnicodeEncodeError: 'ascii' codec can't encode characters in position 0-1: ordinal not in range(128)
#<SB-IMPL::PROCESS :EXITED 1>


(ql:quickload :dexador)									; => To load "dexador":
  Load 1 ASDF system:
    dexador
; Loading "dexador"
(:DEXADOR)

(defun split-by-one-space (string)
    "空白文字一文字で分割された文字列の部分文字列のリストを返します。
2つの連続した空白文字があると、
間に空の文字列が存在するものとみなされることに注意してください。"
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))
;; ⇒ SPLIT-BY-ONE-SPACE
;; (split-by-one-space "Singing in the rain")
;; ⇒ ("Singing" "in" "the" "rain")
;; (split-by-one-space "Singing in the  rain")
;; ⇒ ("Singing" "in" "the" "" "rain")
;; (split-by-one-space "Cool")
;; ⇒ ("Cool")
;; (split-by-one-space " Cool ")
;; ⇒ ("" "Cool" "")
(defun join-string-list (string-list)
    "文字列のリストを連結します。
それぞれの単語の間に空白文字を挿入します。"
    (format nil "~{~A~^ ~}" string-list))
;; ⇒ JOIN-STRING-LIST
;; (join-string-list '("We" "want" "better" "examples"))
;; ⇒ "We want better examples"
;; (join-string-list '("Really"))
;; ⇒ "Really"
;; (join-string-list '())
;; ⇒ ""
;; (join-string-list
;;    (nreverse
;;     (split-by-one-space
;;      "Reverse this sentence by word")))
;; ⇒ "word by sentence this Reverse"

これを使って英語の箇所だけ分割し、そのリストを使って
意味を調べるか？

(split-sequence:SPLIT-SEQUENCE #\Space "Please split this string.")
⇒ ("Please" "split" "this" "string.")
;;; hash table で辞書作成
;; 一旦単語辞書をkeyを単語、valueを意味という形で登録する。
;; 日英の読み装置で読み上げの際、英語はS式でセンテンス単位で
;;  読み上げていたが、そのs式をまんま使えばよさ気。

(defparameter *test* (make-hash-table :test #'equalp)) ; => *TEST*
;; setfの場合何度実行してもvalueは追加されず、外側に括弧もつかない。
(setf (gethash 'test *test*) 'これはテストです) ; => これはテストです
(gethash 'test *test*)													; => これはテストです
T
T
(setq a 'test)													; => TEST
(setq aa `(gethash ,a *test*))					; => (GETHASH TEST *TEST*)
(eval 'aa)															; => (GETHASH TEST *TEST*)

;;pushの場合括弧が外側につく。そしてpushする度に同じkeyに
;;valueが追加されていく。
(push "これもテストです" (gethash 'test2 *test*)) ; => ("これもテストです" "これもテストです" "これもテストです")
(gethash 'test2 *test*)														; => ("これもテストです" "これもテストです" "これもテストです")
T
T
(gethash 'test2 *test*)																 ; => ("これもテストです")

T
`(,setf (,gethash ,'test3 ,*test*) "テスト３")				 ; =>
*																											 ; => NIL
(defparameter *edic* (make-hash-table :test #'equalp)) ; => *EDIC*
;; equalpだと大文字小文字の区別をしないので。省略した場合eqlが使われる。
;;(setf file "/home/hiro/howm/ejdic-hand-utf8.txt")

;; ハッシュテーブルに辞書データを登録
;; #2
(defun make-dic (file)
	(with-open-file (str file :direction :input :if-does-not-exist nil)
									(do ((line (read-line str)
														 (read-line str nil 'eof)))
											((eq line 'eof))
										(progn (setq s-line (split-string line #\Space))
													 (setq key (car s-line))
													 (setq val (cdr s-line))
													 (setf (gethash key *edic*) val))))) ; => MAKE-DIC
+																															 ; => (GETHASH 'ZZZ *EDIC*)
`(,+ (+ 2 3) ,(+ 34 4))																		; => ((GETHASH 'ZZZ *EDIC*) (+ 2 3) 38)
(eval `(+ 3 4 4 ,(+ 3 4)))																; => 18
;; #1
(defun make-dic (file)
	(with-open-file (str file :direction :input :if-does-not-exist nil)
									(do ((line (read-line str)
														 (read-line str nil 'eof)))
											((eq line 'eof))
										(let* ((s-line (split-string line #\Space))
													 (key (string-right-trim '(#\tab) (car s-line)))
													 (val (cdr s-line)))
											(setf (gethash key *edic*) val)))))						; => MAKE-DIC
;;(push val (gethash key *edic*))にすると外側に括弧がさらに追加される。
(make-dic "/home/hiro/howm/test2.txt")					 ; => NIL
(make-dic "/home/hiro/howm/ejdic-hand-utf8.txt") ; => NIL
;; trimしてあげないとkeyの右側にスペースがある。
;; 結局key+#\Spaceという状態になっているので、keyだけで
;; 検索をかけても完全マッチしないためnilが返る。

;; 文字列先頭・末尾の半角ペース、改行、タブを削除する
;;
;; (defun trim (str)
;; 	(string-trim '(#\Space #\newline #\tab) str))
;; 全角のスペースも削除する
;; (coerce "　" 'character) ;; 全角スペースの character 表記を取得
;; -> #\IDEOGRAPHIC_SPACE
(defun trimm (str)
 (string-trim '(#\Space #\newline #\tab #\IDEOGRAPHIC_SPACE) str))

(string-trim " " " trim me ")						; => "trim me"


(gethash '@ *edic*)											; => NIL
NIL
(gethash "@" *edic*)										; => ("単価…で/…につき")
T
T
T
(gethash 'a.b. *edic*)									; => NIL
NIL
(gethash "a.b." *edic*)									; => ("B.A.")
T
T
;; quoteではだめでダブルクォートで囲む必要あり。

(gethash "zzz" *edic*)									; => ("グーグー(いびきの音)")
T
T
NIL
T


;; ハッシュテーブルの表示
(maphash #'(lambda (key value)
             (format t "~A=>~A~%" key value))
         *edic*)												; =>


 ;;ハッシュの任意のデータ削除
 remhash key hash-table
 clrhash hash-table
 ;; ハッシュの格納データ数表示
 (hash-table-count *edic*)		; => 10
;; ハッシュの表全体の削除
 (clrhash *edic*)												; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {AFCF6B9}>
(hash-table-count *edic*)								; => 0


(defun split-string (string &optional (separator #\space)
                            omit-nulls)
  (labels ((pos (str sep)
             (position sep str
                       :test #'(lambda (x y)
                                 (find y x :test #'char=)))))
    (let ((sep (string separator))
          (acc '()))
      (do* ((str string (subseq str (1+ n)))
            (n (pos str sep) (pos str sep)))
           ((null n)
            (nreverse (if omit-nulls
                          `(,str ,@acc)
                        (remove "" `(,str ,@acc)
                                :test #'string=))))
        (push (subseq str 0 n) acc)))))


	;; (setf (gethash key ht) value)
	;; (push value (gethash key ht))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))														; => WHILE

	(let ((x 0))
		(while (< x 10)
			(+ x 1)
			(print x)))												; =>

(with-open-file (s p)
    (do ((l (read-line s) (read-line s nil 'eof)))
        ((eq l 'eof) "Reached end of file.")
     (format t "~&*** ~A~%" l)))

(maphash #'(lambda (key value)
             (princ (format "key=>%S,val=>%S\n" key val)))
         *edic*)												; =>
;; ハッシュにキーが存在するか調べる。
(defun hash-exists-p (key table)
  (let ((novalue (make-symbol "<nil>")))
    (not (eq (gethash key table novalue) novalue))))

(hash-exists-p 'test *test*)						; => T

(defun test ()
	(let ((lst '(it's too late)))
		(if (null lst)
				nil
				(progn
					(setf lst (cdr lst))
					(print (car lst))
					(test (car lst))))))					; => TEST
(test)																	; =>
(test '(it's too late))									; =>
