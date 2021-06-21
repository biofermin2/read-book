(locally (declaim (optimize (speed 3) (debug 3) (safety 0))))
;; ユーティリティーファイルの読み込み
(load "/home/hiro/howm/junk/utils.lisp") ; => T
(load "/home/hiro/howm/lisp_dir/svg.lisp") ; => T
(defparameter *edic* (make-hash-table :test #'equalp)
	"辞書用データの格納領域の定義")				; => *EDIC*

(defparameter *kv-alst* nil
	"センテンスにおけるkeyとvalueの一時格納域の定義") ; => *KV-ALST*

;; (setq file "~/test.txt")								; => "~/test.txt"
;; (namestring file)												; => "~/test.txt"
;; (setq p (pathname file))								; => #P"~/test.txt"
;; (directory-namestring p)								; => "~/"
;; (pathname-directory p)									; => (:ABSOLUTE :HOME)
;; (pathname-name p)												; => "test"
;; (pathname-type p)												; => "txt"

;; (make-pathname
;;  :directory (pathname-directory p)
;;  :name (pathname-name p)
;;  :type "html")													; => #P"~/test.html"

;; (setq out-file
;; 			(make-pathname
;; 			 :directory (pathname-directory p)
;; 			 :name (pathname-name p)
;; 			 :type "html"))										; => #P"~/test.html"

;; (unless (probe-file "/var/log/pacman.log")
;;   (format *error-output* "File not found~%"))

(defun write-html (file)
	(let* ((pn (pathname file)) ;file.txt->file.html
				(outfile (make-pathname
									:directory (pathname-directory pn)
									:name (pathname-name pn)
									:type "html")))
		(with-open-file (*standard-output* outfile
																			 :direction :output
																			 :if-exists :supersede)
			(html
				(head
					(title "翻訳ページ"))
				(body
					(set-htm (reverse *kv-alst*))))))) ; => WRITE-HTML


(defun set-htm (lst)
	(let ((k (caar lst))
				(v (cdar lst)))
		(unless (null lst)
				(progn
					(span k v)
					(princ "&nbsp;")
					(set-htm (cdr lst))))))						; => SET-HTM


;; 末尾再帰を使えばいちいちlstをsetqし直さなくていい。

;; (setq lst '(ab bb bc bd be))						; => (AB BB BC BD BE)
;; (defun fa (lst)
;; 	(if lst
;; 			(progn
;; 				(print lst)
;; 				(fa (cdr lst)))
;; 			nil))															; => FA
;; (fa lst)																; =>
;; (AB BB BC BD BE)
;; (BB BC BD BE)
;; (BC BD BE)
;; (BD BE)
;; (BE) NIL


;; ;; 昇順で並んだ1からNまでの総和を求める関数
;; (defun gausu (n)
;; 	(/ (* (+ n 1) n) 2))									; => GAUSU
;; (gausu 123)															; => 7626
;; (gausu 100)															; => 5050

;; スケールを小さくして考え、その中から公式を見つける。

(defun set-tts (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせたttsをセットし読み上げる。"
	(let ((tts nil))
		(unless (null lst)
			(if (standard-char-p (car lst)) ;リストの先頭の文字が英数字ならば
					(progn
						(setq tts "festival --tts")	;t　festival 改行は削除しない。dotと読み上げてしまう。
						(make-kv-cell lst))	;	html化する処理
					(progn
						(setq tts "ojtalk") ;nil ojtalk
						(push (list (coerce lst 'string)) *kv-alst*)))
			(setq lst (delete #\Newline lst))
			(format t "~A~&" lst) ;~&はfresh-lineと同じ。
			(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts))))) ; => SET-TTS

;; read-char &optional input-stream eof-error-p eof-value recursive-p => char

(defun read-book (file)
	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
	(let ((lst nil))
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
				(setq lst nil))
				(setq lst (cons char lst))))
					(write-html file)))	; => READ-BOOK

(read-book "~/test_dir/ruルー語.txt")

(defun make-dic (file ht)
	"ハッシュテーブルに辞書データを登録する関数"
	(with-open-file (str file
											 :direction :input
											 :if-does-not-exist nil)
									(do ((line (read-line str)
														 (read-line str nil 'eof)))
											((eq line 'eof))
										(let* ((s-line (split "	 " line))
													 (key (string-right-trim '(#\tab) (car s-line)))
													 (val (cdr s-line)))
											(setf (gethash key ht) val))))) ; => MAKE-DIC

; 英語の辞書のセットがうまくいってない。#\Spaceだけではうまくいかない。
;; 実際見ると#\tab#\Spaceと複数使われている。
;; 実際の単語辞書で使われているデリミタをコピー->OK[2017-04-24 16:36:07]

;; 指定辞書データファイルをハッシュテーブルに登録
(make-dic "/home/hiro/howm/ejdic-hand-utf8.txt" *edic*) ; => NIL
;; (gethash "cat" *edic*)									; => ("『猫』;(ライオン,トラ,ヒョウなどの)ネコ科の動物 ")
;; (gethash "CAT" *edic*)								; => ("clear air turbulence晴天乱流 ")
;; 上記のような例があるので、equalpで*edic*を定義するのはやめる。

;; 確認テスト
;;(gethash "@" *edic*)										; => ("単価…で/…につき")
;;(gethash "zzz" *edic*)													 ; => ("グーグー(いびきの音)")

;; (remove #\! '("it's" "fine" "today!"))	; => ("it's" "fine" "today!")
;; (delete #\! '(it's fine today!))				; => (IT 'S FINE TODAY!)
;; (remove #\! '(#\i #\t #\!))							; => (#\i #\t)
;; (remove #\! '("#\!"))										; => ("#!")
;; removeする際にlist内はcharである必要がある。
(defun make-kv-cell (lst)
	(let ((lst (split " " lst)) ;リスト内の文字列を単語単位で分割
				(val nil)
				(mrk '(#\, #\. #\? #\! #\; #\:)))
		(dolist (key lst) ; センテンスから単語を順次切り出す
			(dolist (m mrk) ; 単語の清浄化
				(setq key (delete m key)))
			(setq key (coerce key 'string)) ;concatenateはギリギリ手前にする。
			(setq val (gethash key *edic*))
			(push (cons key val) *kv-alst*))))																; => MAKE-KV-CELL

;; *kv-alst*																																; => (("です。")
;;  ("mountain"
;;   "『山』 / 《the…Mountains》…『山脈』 / 《a~》山(のような…),(…の)山《+『of』+『名』》 / 《a~》多数(の…),多量(の…)《+『of』+『名』》")
;;  ("塵も積もれば")
;;  ("

;; ")
;;  ("だから。")
;;  ("philosophy"
;;   "〈U〉『哲学』 / 〈C〉哲学体系 / 〈U〉(学問・知識などの)原理,理論《+『of』+『名』》 / 〈U〉〈C〉人生哲学,人生観;信条 / 〈U〉冷静,沈着;悟り,達観 ")
;;  ("僕の")
;;  ("
;; ")
;;  ("していくのが、")
;;  ("up"
;;   "《上への動作》『上へ』,上のほうへ / 《上の位置》『上に』,上のほうで / 『起こして』,立てて,直立して / (地図・紙面などの)『上へ(に)』,北のほうへ(に) / (遠くから)『近くへ,中心へ』;(周辺から)中心へ;(川下から)川上へ;(地方から)都会へ / (大きさ・量・価値・地位などが)『上のほうへ』,高く / (話者,話題の場所へ)『近づいて』,追いついて / 『活動して』,活動状態に / (物事・人が)現れて・起こって / 『すっかり』,完全に,終わって,…し尽くして / 保管(貯蔵)して;くるんだ(閉ざした)状態に / 《動詞を省略した命令文で》 / (野球・クリケットで)打席へ / (競技で)勝ち越して / (対抗の競技で)おのおの / 《移動・位置》『…の高いほうへ』,をのぼって;…の上のほうに / (川の)『上流へ』;(流れ・風)に逆らって;…の内陸(奥地)のほうへ / (自分が今いる所,またはある地点から前方へ)『…に沿って』 / 『上へ向かう』,(列車などが)上りの;上り坂の,(情勢などが)上向きの / 上り,上昇,上り坂 / 上昇気運,向上,幸運 / 上りの列車(バス・エレボーターなど) / 値上がり / …‘を'持ち上げる / …‘を'増大する,〈賃金・物価など〉‘を'引き上げる / 〈人〉‘を'昇進させる / 立ち上がる,起き上がる / 《and を伴って》急に(突然)…する")
;;  ("growing") ("をしながら")
;;  ("

;; experience")
;;  ("。") ("daughter" "『娘』 / 女の子孫 / (娘のように)生み出されたもの ") ("箱入り")
;;  ("

;; ")
;;  ("。")
;;  ("water"
;;   "〈U〉水 / 《しばしば複数形で》『海』(『湖,川』)『の水』 / 《複数形で》流れる水;(海・川・湖などの)波立つ水《+of+名》 / 《複数形で》(特定の)水域,領海,近海 / 〈U〉潮位,水位;《the water》水面 / 〈U〉分泌液(涙・汗・尿など) / 〈U〉溶液,…水 / 《複数形で》(飲用の)鉱泉水 / 〈U〉(織物・金属などの)波紋 / …‘に'『水をかける』(まく,やる) / 〈動物〉‘に'水を飲ませる / …‘に'水を供給する;…‘を'潅漑(かんがい)する / 〈液体など〉‘を'水で割る,薄める《+名+down,+down+名》 / 〈織物・金属など〉‘に'波紋(波模様)をつける / 〈動物などが〉水を飲む:〈船・機関などが〉給水を受ける / 〈目・口などが〉分泌液を出す")
;;  ("寝耳に")
;;  ("

;; ")
;;  ("しようぜ！")
;;  ("together"
;;   "『いっしょに』,共に,連れ立って / 『一団(一体)となるように』 / 『互いに』(…し合う) / 『協力して』,一致して,調和して / ひっくるめて,総合して / 同時に,一度に"))


;; (defun make-kv-cell (lst)
;; 	(let ((lst (split " " lst)) ;リスト内の文字列を単語単位で分割
;; 				(val nil)
;; 				(mrk '(#\, #\. #\? #\! #\; #\:)))
;; 		(dolist (key lst) ; センテンスから単語を順次切り出す
;; 			(dolist (m mrk) ; 単語の清浄化
;; 				(setq key (delete m key)))
;; 			(setq key (concatenate 'string key)) ;concatenateはギリギリ手前にする。
;; 			(setq val (gethash key *edic*))
;; 			(push (cons key val) *kv-alst*)))) ; => MAKE-KV-CELL

;; そもそも!とか?が付くから問題になっている。
;; ここでの処理はセンテンスから単語に分割し、かつ、単語の清浄化を
;; 行っている。またこの処理は英語のパートのみに行われる。
;; しかし、html化する際には日本語の部分も必要ではないか？
;; html化する処理に日英の判定を入れるべきか。
;; html化する処理の概要
;; 日本語の場合lst単位でhtmlに出力
;; 英語の場合lstをkeyに分割し、そこにvalを付け足してhtmlに出力。
;; labelsを使えば末尾再帰を使って書き換える事ができそう。


;; 最後のkv-alstに入れる処理を無くして直接
;; htmlを作成する処理に飛ばしてしまった方が
;; いいのではないだろうか？

;; しかし、現段階でvalは必要なのだろうか？
;; htmlを作る段階でひっぱってきてもいいような気がしたが。
;; つまりkeyだけをhashテーブルか何かに登録しておき、
;; htmlを作る段階でそこからkeyを元にvalをひっぱるという方法ではどうだろうか？[2017-04-24 23:57:22]

;; make-kv-cellに組み込んだため廃止[2017-04-30 11:30:33]
;; (defun omit-mrk (lst)
;; 	(let ((mrk '(#\, #\. #\? #\! #\' #\; #\:)))
;; 		(dolist (m mrk)
;; 			(setq lst (delete m lst)))
;; 		(setq lst (concatenate 'string lst)))) ; => OMIT-MRK

;; removeする時はchar
;; splitはcharでも文字列でもOK
;; concatenateはcharの時のみ。文字列にする。
;; つまりremoveの前にconcatenateをしてはいけない。

;; (mapcar #'+ '(1 2 3) '(4 5 6))					; => (5 7 9)
;; (mapcar #'remove '(#\! #\?) '(#\! #\a #\b #\?)) ; =>
;; (funcall #'+ 1 2 3)											; => 6
;; (apply #'+ '2 '(3 4))			 						; => 9
;; (mapcan #'remove '(#\! #\?) '(#\! #\a #\b #\?)) ; =>

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

;; (describe 'schar)		; => COMMON-LISP:SCHAR
;;   [symbol]

;; SCHAR names a compiled function:
;;   Lambda-list: (STRING INDEX)
;;   Declared type: (FUNCTION (SIMPLE-STRING (MOD 536870909))

;; (VALUES CHARACTER &OPTIONAL))
;;   Documentation:
;;     SCHAR returns the character object at an indexed position in a string
;;        just as CHAR does, except the string must be a simple-string.
;;   Known attributes: foldable, flushable, unsafely-flushable
;;   Source file: SYS:SRC;CODE;STRING.LISP

;; (SETF SCHAR) names a compiled function:
;;   Lambda-list: (G98 G99 G100)
;;   Derived type: (FUNCTION (CHARACTER SIMPLE-STRING (MOD 536870909))
;;                  (VALUES CHARACTER &OPTIONAL))
;;   Inline proclamation: INLINE (inline expansion available)
;;   Source file: SYS:SRC;CODE;SETF-FUNS.LISP

;; (SETF SCHAR) has setf-expansion: SB-KERNEL:%SCHARSET

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

;;(coerce "a" 'character)						; => #\a
;;(coerce '#\a 'string)										; =>
;; (ql:quickload :cl-who)									; => To load "cl-who":
;; (use-package :cl-who)										; => T

;; (setq kv-cell '(("試験　テスト" "test")
;; 									("くず　廃品　無価値なもの　安っぽい" "junk")
;; 									("自宅　居宅　住居　故郷　本拠地" "home"))) ; => (("試験　テスト" "test") ("くず　廃品　無価値なもの　安っぽい" "junk") ("自宅　居宅　住居　故郷　本拠地" "home"))

;;成功！ ((key .val)(key . val))-> <span title="val">key</span>
;; (defun make-html (c)
;; 	(cl-who:with-html-output (*standard-output*)
;; 		(htm (:head (:title "翻訳ページ"))
;; 				 (:body
;; 					(loop for (key val) in c
;; 						 do (:span :title key (str val))))))) ; => MAKE-HTML
;; (make-html kv-cell)																; =>


;; (defun make-html (c)
;; 		(cl-who:with-html-output (*standard-output*)
;; 			(loop for (key . val) in c
;; 				 do (htm (:head (:title "翻訳ページ"))
;; 							 (:body (:span :title val (str key))))))) ; => MAKE-HTML

;; titleなどを繰り返してしまっている。
;; 自分で位置からフルスクラッチで書きなおすか？[2017-04-25 20:49:42]


;; htmlをfileに書き込む
;; (defun write-html (file)
;; (with-open-file (out file
;; 										 :direction :output
;; 										 :if-exists :append
;; 										 :if-does-not-exist :create)
;; 	(format out " ~A~T" (make-html kv-cell)))) ; => WRITE-HTML
;; (write-html "test.html")										 ; =>
;; 単語間の空白を入れれない。&nbsp; と入れると空白となるとあるが、それを
;; どうやっていれたらいいのかがわからない。formatではダメ？

    ;; 「&nbsp;」は通常の半角スペースと同じサイズの空白。
    ;; 「&ensp;」はそれより少し広めの空白。
    ;; 「&emsp;」はさらに広めの空白。
    ;; 「&thinsp;」は、最初の「&nbsp;」よりも狭い(細い)空白です。
;; <pre>タグを使うと</pre>見たまま表示できるとの事。
;; cl-whoの使い方について実験
;; (html
;;  (head
;; 	(title "タイトル"))
;;  (body
;; 	(span :title title )))
;; (defun make-webpage (lst)
;; 	(if (eq (car lst) html)
;; 			(print "<" (car lst) ">")))

;; (defmethod convert-tag-to-string-list ((tag (eql :red)) attr-list body body-fn)
;;     (declare (ignore attr-list))
;;     (nconc (cons "<font color='red'>" (funcall body-fn body)) (list "</font>")))

;; (with-html-output (*standard-output*)
;;     (:red (:b "Bold and red"))					; =>
;;     (values))														; => <font color='red'><b>Bold and red</b></font>

;; (with-html-output (*standard-output* nil :prologue t)
;; 	(:html (:head
;; 					(:title "翻訳ページ"))
;; 				 (:body "Not much there"))
;; 	(values))															; => <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
;; <html><head><title>翻訳ページ</title></head><body>Not much there</body></html>
;; <html><body>Not much there</body></html>

;; (with-html-output (*standard-output* nil :prologue t :indent t)
;;     (:html (:body :bgcolor "white"
;;              (:span :title "value" (str "key"))))
;;     (values))														; => <!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

;; <html>
;;   <body bgcolor='white'>
;;     <span title='value'>key
;;     </span>
;;   </body>
;; </html>


;; (ql:system-apropos :html)								; => #<SYSTEM buildnode-html5 / buildnode-20150113-git / quicklisp 2017-01-24>
;; #<SYSTEM buildnode-xhtml / buildnode-20150113-git / quicklisp 2017-01-24>
;; #<SYSTEM chtml-matcher / chtml-matcher-20111001-git / quicklisp 2017-01-24>
;; #<SYSTEM cl-html-diff / cl-html-diff-20130128-git / quicklisp 2017-01-24>
;; #<SYSTEM cl-html-parse / cl-html-parse-20161031-git / quicklisp 2017-01-24>
;; #<SYSTEM cl-html5-parser / cl-html5-parser-20160531-git / quicklisp 2017-01-24>
;; #<SYSTEM cl-html5-parser-cxml / cl-html5-parser-20160531-git / quicklisp 2017-01-24>
;; #<SYSTEM cl-html5-parser-tests / cl-html5-parser-20160531-git / quicklisp 2017-01-24>
;; #<SYSTEM cl-htmlprag / cl-htmlprag-20160628-git / quicklisp 2017-01-24>
;; #<SYSTEM closure-html / closure-html-20140826-git / quicklisp 2017-01-24>
;; #<SYSTEM common-html / common-html-20160421-git / quicklisp 2017-01-24>
pp;; #<SYSTEM common-html-test / common-html-20160421-git / quicklisp 2017-01-24>
;; #<SYSTEM fxml/html5 / fxml-20160929-git / quicklisp 2017-01-24>
;; #<SYSTEM geneva-html / geneva-20161204-git / quicklisp 2017-01-24>
;; #<SYSTEM html-encode / html-encode-1.2 / quicklisp 2017-01-24>
;; #<SYSTEM html-match / bknr-web-20140713-git / quicklisp 2017-01-24>
;; #<SYSTEM html-match.test / bknr-web-20140713-git / quicklisp 2017-01-24>
;; #<SYSTEM html-sugar / html-sugar-20110829-http / quicklisp 2017-01-24>
;; #<SYSTEM html-template / html-template-0.9.2 / quicklisp 2017-01-24>
;; #<SYSTEM htmlgen / portableaserve-20150923-git / quicklisp 2017-01-24>
;; #<SYSTEM macro-html / macro-html-20151218-git / quicklisp 2017-01-24>
;; #<SYSTEM monkeylib-html / monkeylib-html-20120107-git / quicklisp 2017-01-24>
;; #<SYSTEM monkeylib-markup-html / monkeylib-markup-html-20120208-git / quicklisp 2017-01-24>
;; #<SYSTEM xhtmlambda / xhtmlambda-20160531-git / quicklisp 2017-01-24>
;; #<SYSTEM xhtmlgen / xhtmlgen-20170124-git / quicklisp 2017-01-24>
;; #<SYSTEM xhtmlgen-test / xhtmlgen-20170124-git / quicklisp 2017-01-24>
;; (ql:update-client)											; => The most up-to-date client, version 2017-03-06, is already installed.
;; T
;; (ql:update-all-dists)										; =>


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
				(trans-book file-name))))
;; argvは("/usr/bin/sbcl" "read-book-v2.2.lisp" "~/test.txt")
;; という感じで表示されるので、ここで読み込みたい引数はthirdとなる。
																				; =>
;(car '("read-book-v2.2.lisp" "/home/hiro/tmp"))
																				; => "read-book-v2.2.lisp"
;(read-book (second '("read-book-v2.2.lisp" "/home/hiro/tmp")))
																				; => "/home/hiro-mg3hiro/tmp"
(main) ;mainの実行

;; 読み上げが終わってから翻訳が出るので、
;; それを変えたい。
;; いっその事翻訳機能は別に動かす事にして先に翻訳を終わらせるか？
