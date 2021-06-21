(declaim (optimize (speed 3) (debug 0) (safety 0)))

(load "/home/hiro/howm/junk/utils.lisp") ; => T

(defun set-tts (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせたttsをセットし読み上げる。"
	(unless (null lst)
		(if (standard-char-p (car lst)) ;リストの先頭の文字が英数字ならば
				(progn
					(setq tts "festival --tts")	;yes　festival 改行は削除しない。dotと読み上げてしまう。
					(make-kv-cell lst))	;	英語の文字列を単語単位で辞書データから意味を引っ張ってhtml化する処理
				(progn
					(setq tts "ojtalk"))) ;no ojtalk
		(setq lst (delete #\Newline lst))
		(format t "~A~%" lst)
		(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts)))) ; => SET-TTS

(defun read-book (file)
	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
	(let ((lst ()))
		(with-open-file (in file :direction :input)
			(do ((char (read-char in nil 'eof)
								 (read-char in nil 'eof)))
					((eql char 'eof))
				(when (or (and (not (null lst)) ;lstが空ではなく、かつ
											 (not (eq (type-of char) (type-of (car lst))))) ;前のchar≠今のchar
									;; (let ((mrk '(#\。 #\. #\! #\！ #\? #\？ #\: #\;)))
									;; 	(dolist (m mrk)
									;; 		(eq (car lst) m)))
									(eq (car lst) #\。)
									(eq (car lst) #\.)
									(eq (car lst) #\！)
									(eq (car lst) #\!)
									(eq (car lst) #\？)
									(eq (car lst) #\?)
									(eq (car lst) #\:)
									(eq (car lst) #\;)
;									(eq char #\Space)
									)
				(set-tts (nreverse lst))
				(setq lst ()))
				(setq lst (cons char lst))))))	; => READ-BOOK

(read-book "~/test.txt")								; =>
(("peach" "(共犯者・仲間などを)密告する,裏切る《+『against』(『on, upon』)+『名』》 / …‘を'裏切る")) (p e a c h)
(car (も も た ろ う))
(("
")) NIL
(。)
(ね こ)

(("cat" "『猫』;(ライオン,トラ,ヒョウなどの)ネコ科の動物 ")) (c a t)
(が う ま れ た 。)

(("late"
  "(定刻・通常・予定の時間より)『遅れた』,遅い / (時刻が)『遅い』,遅く始まる,遅くまで続く;夜更けの / (時期が)『遅い』,終りごろの,後期の / 『最近の』,最新の(recent) / 《the~,one's~》『前の』,先の,前任の(former) / 《the~,one's~》『故』…,つい先ごろ死んだ / (定刻などより)『遅れて』,遅く / (時刻が)『遅く』,遅くまで,(特に)夜更けに;(時期が)遅く,終りごろに / 『最近』,近ごろ(recently) ")
 ("too"
  "…もまた,その上 / 『あまりにも』,過度に,必要以上に / 『非常に』,大変,はなはだ(very, extremely) / 《話》《相手の否定の言葉に,肯定で応じて》ところがどうして")
 ("
it's")) (i t ' s   t o o   l a t e !)

(("finish"
  "…『終える』,済ます / …‘を'食べ尽くす,使い尽くす《+『名』+『off(up)』,+『off(up)』+『名』》 / …『の仕上げをする』,磨きをかける《『名』『off(up)』,+『off(up)』+『名』》 / 《話》〈人〉‘を'参らせる / 〈物事が〉『終わる』;〈人が〉やり終える,終わりにする《+『off』(『up』)》 / (ことの)『最終段階』,結末 / (家具などの)表面,表面の仕上げ(手触り);洗練 / (ニスなど)仕上げの材料 / 死,滅亡 ")
 ("already" "《肯定文で》『もう』,『すでに』 / 《驚き・意外の意を表して》《疑問文で》もう,早くも;《否定文で》まさか")
 ("it"
  "《すでに述べられた物・事または幼児,動物などを指して》『それは』(『を』),そのものは(を),そのことは(を) / 《その場の状況で相手に何であるか分かるような物・事または人を指して》『それは』(『を』),そのことは(を) / 《天候・時間・距離・事情・状態などを指して》 / 《It seems(happens, appears… )thatなどの形で》 / 《形式主語として》 / 《形式目的語として》 / 《It is … that(who, which)の形で…を強調して》 / 《ある種の動詞・前置詞に添える形式上の目的語として》 / 〈C〉(遊戯の)鬼 / 〈U〉《話》理想,第一人者 / 〈U〉《話》性的魅力 ")
 ("
is")) (i s   i t   a l r e a d y   f i n i s h ?)

(("
")) NIL
(終 わ り 。)
NIL



(defparameter *edic* (make-hash-table :test #'equal)) ; => *EDIC*

;; ハッシュテーブルに辞書データを登録する関数の定義
(defun make-dic (file)
	(with-open-file (str file :direction :input :if-does-not-exist nil)
									(do ((line (read-line str)
														 (read-line str nil 'eof)))
											((eq line 'eof))
										(let* ((s-line (split "	 " line))
													 (key (string-right-trim '(#\tab) (car s-line)))
													 (val (cdr s-line)))
											(setf (gethash key *edic*) val))))) ; => MAKE-DIC
;; 英語の辞書のセットがうまくいってない。#\Spaceだけではうまくいかない。
;; 実際見ると#\tab#\Spaceと複数使われている。
;; 実際の単語辞書で使われているデリミタをコピー->OK[2017-04-24 16:36:07]

;; 指定辞書ファイルをハッシュテーブルに登録
(make-dic "/home/hiro/howm/ejdic-hand-utf8.txt") ; =>
;; (gethash "cat" *edic*)									; => ("『猫』;(ライオン,トラ,ヒョウなどの)ネコ科の動物 ")
;; (gethash "CAT" *edic*)								; => ("clear air turbulence晴天乱流 ")
;; 上記のような例があるので、equalpで*edic*を定義するのはやめる。

;; 確認テスト
;;(gethash "@" *edic*)										; => ("単価…で/…につき")


;;(gethash "zzz" *edic*)													 ; => ("グーグー(いびきの音)")
(remove #\! '("it's" "fine" "today!"))	; => ("it's" "fine" "today!")
(delete #\! '(it's fine today!))				; => (IT 'S FINE TODAY!)
(remove #\! '(#\i #\t #\!))							; => (#\i #\t)
(remove #\! '("#\!"))										; => ("#!")


(defun make-kv-cell (lst)
	(let ((ls nil)
				(lst (split " " lst))
				(mrk '(#\, #\. #\? #\! #\; #\:)))
		(dolist (key lst)
			(dolist (m mrk)
				(setq key (delete m key)))
			(setq key (concatenate 'string key)) ;concatenateはギリギリ手前にする。
			(setf val (gethash key *edic*))
			(push (cons key val) ls))
		(print ls)))												; => MAKE-KV-CELL
(make-kv-cell lst)						 					; =>

;; しかし、現段階でvalは必要なのだろうか？
;; htmlを作る段階でひっぱってきてもいいような気がしたが。
;; つまりkeyだけをhashテーブルか何かに登録しておき、
;; htmlを作る段階でそこからkeyを元にvalをひっぱるという方法ではどうだろうか？[2017-04-24 23:57:22]


;; (defun omit-mrk (lst)
;; 	(let ((mrk '(#\, #\. #\? #\! #\' #\; #\:)))
;; 		(dolist (m mrk)
;; 			(setq lst (delete m lst)))
;; 		(setq lst (concatenate 'string lst)))) ; => OMIT-MRK

;; removeする時はchar
;; splitはcharでも文字列でもOK
;; concatenateはcharの時のみ。文字列にする。
;; つまりremoveの前にconcatenateをしてはいけない。

(mapcar #'+ '(1 2 3) '(4 5 6))					; => (5 7 9)
(mapcar #'remove '(#\! #\?) '(#\! #\a #\b #\?)) ; =>
(funcall #'+ 1 2 3)											; => 6
(apply #'+ '2 '(3 4))			 						; => 9
(mapcan #'remove '(#\! #\?) '(#\! #\a #\b #\?)) ; =>
;; 1,remove
;; 2,concatenate
;; 3,split
;; の順で加工する。


;; (coerce #\a 'string)										; =>
;; (coerce "a" 'character)									; => #\a
;; (string #\a)														; => "a"

;; ;; concatenate はcharacter<->stringに使える。
;; (concatenate 'list "abc")			 					; => (#\a #\b #\c)
;; (concatenate 'string '(#\a #\b #\c))		; => "abc"


;; (defun add-space (lst)
;; 	(let ((ls ()))
;; 		(when (or (eq (car lst) #\,)
;; 							(eq (car lst) #\.)
;; 							(eq (car lst) #\?)
;; 							(eq (car lst) #\!)
;; 							(eq (car lst) #\;)
;; 							(eq (car lst) #\:))
;; 			(setq ls (concatenate 'string " " (car lst)))
;; 			(print ls)
;; 			(add-space (cdr lst)))))			 		; => ADD-SPACE
;; (add-space lst)													; => NIL
;; lst																			; => (#\i #\t #\' #\s #\  #\f #\i #\n #\e #\  #\d #\a #\y #\, #\  #\t #\o #\d #\a
;; 																									 #\y #\!)

;; (append '(1 3 4) '(a b c))							; => (1 3 4 A B C)
;; (append '(" ") '("a"))									; => (" " "a")
;; (concatenate 'string " " "a")						; => " a"

;; (defun omit-mrk(lst)
;; 	(let ((mrk '(#\, #\. #\? #\! #\; #\:)))
;; 		(dolist (m mrk)
;; 			(setq lst (concatenate 'string (remove m lst)))
;; 			(print lst))))										; => OMIT-MRK



(describe 'schar)		; => COMMON-LISP:SCHAR
  [symbol]

SCHAR names a compiled function:
  Lambda-list: (STRING INDEX)
  Declared type: (FUNCTION (SIMPLE-STRING (MOD 536870909))

(VALUES CHARACTER &OPTIONAL))
  Documentation:
    SCHAR returns the character object at an indexed position in a string
       just as CHAR does, except the string must be a simple-string.
  Known attributes: foldable, flushable, unsafely-flushable
  Source file: SYS:SRC;CODE;STRING.LISP

(SETF SCHAR) names a compiled function:
  Lambda-list: (G98 G99 G100)
  Derived type: (FUNCTION (CHARACTER SIMPLE-STRING (MOD 536870909))
                 (VALUES CHARACTER &OPTIONAL))
  Inline proclamation: INLINE (inline expansion available)
  Source file: SYS:SRC;CODE;SETF-FUNS.LISP

(SETF SCHAR) has setf-expansion: SB-KERNEL:%SCHARSET

;; ;;文字列から順番を指定して文字を取り出す
;; (setq s "is it finished?")					; => "is it finished?"
;; (char s 14)													; => #\?
;; ;; char よりもscharの方が高速
;; (schar s 7)													; => #\i
;; ;; 文字列は配列であるためarefやeltも使える。
;; (aref s 10)															; => #\s
;; (elt s 10)															; => #\s





;; トラブルの原因はconcatenateしてしまうと文字列に切り替わり
;; charではなくなる。その際にremoveで扱えなくなるから。
;; #\aじゃなくて"a"となるって事。removeする時は#\aである必要あり。
;; つまりdolistは使えない。


(setq lst '(#\i #\t #\' #\s #\Space #\f #\i #\n #\e #\Space #\d #\a #\y #\, #\Space #\t #\o #\d #\a #\y #\!))


;; (char-p #\a)														; =>
;; (alpha-char-p #\a)											; => T
;; (characterp #\a)												; => T
;; (characterp "is")												; => NIL
;; (stringp "is")													; => T

;; (apropos 'char)													; => ALPHA-CHAR-P (fbound)
;; BASE-CHAR
;; CHAR (fbound)
;; CHAR-CODE (fbound)

;; (apropos 'string)												; => ASDF/BACKWARD-INTERFACE::CONTROL-STRING
;; ASDF/CACHE:NORMALIZE-NAMESTRING (fbound)
;; ASDF/CACHE::NORMALIZED-NAMESTRING

;; (describe 'base-string)									; => COMMON-LISP:BASE-STRING
;;   [symbol]

;; BASE-STRING names the built-in-class #<BUILT-IN-CLASS BASE-STRING>:
;;   Class precedence-list: BASE-STRING, STRING, VECTOR, ARRAY, SEQUENCE, T
;;   Direct superclasses: STRING
;;   Direct subclasses: SIMPLE-BASE-STRING
;;   No direct slots.

;; BASE-STRING names a primitive type-specifier:
;;   (undocumented)

;; [2017-04-24 16:39:13] 'sとか'mとかも辞書にはあるが、くっついているので、
;; 検索がかからない。'(アポストロフィー)の前に空白を追加する必要あり。

(coerce "a" 'character)				 					; => #\a
;;(coerce '#\a 'string)										; =>

;;成功！ ((key .val)(key . val))-> <span title="val">key</span>
(defun make-html (c)
		(cl-who:with-html-output (*standard-output*)
			(loop for (key . val) in c
				 do (htm (:head (:title "翻訳ページ"))
							 (:body (:span :title val (str key))))))) ; => MAKE-HTML
(make-html cons-data)																		; => <head><title>翻訳ページ</title></head><body><span title='test'>試験　テスト</span></body><head><title>翻訳ページ</title></head><body><span title='junk'>くず　廃品　無価値なもの　安っぽい</span></body><head><title>翻訳ページ</title></head><body><span title='home'>自宅　居宅　住居　故郷　本拠地</span></body>NIL

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

(:CL-WHO)
(use-package :cl-who)										; => T
