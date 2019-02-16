#!/usr/bin/sbcl
;; -*- mode: Lisp; coding:utf-8 -*-

'(:program-name "read-book-v3.lisp"
	:copyright 2017 :author "Higashi, Hiromitsu"
	:license "MIT")
;; => (:PROGRAM-NAME "read-book-v3.lisp" :COPYRIGHT 2017 :AUTHOR "Higashi, Hiromitsu"
 ;; :LICENSE "MIT")

;;; LOAD

(load "/home/hiro/howm/junk/utils.lisp") ; => To load "trivial-shell":
(load "~/quicklisp/dists/quicklisp/software/cl-mecab/cl-mecab.asd") ; => T

;; quicklispのhomeを調べる時は以下を実行する。[2017-06-13 14:57:02]
;;ql:*quicklisp-home*					; => #P"/home/hiro/quicklisp/"

(ql:quickload	'(:cl-ppcre :dexador :cl-mecab) :silent t) ; => (:CL-PPCRE :DEXADOR :CL-MECAB)
																				;
;; 大域変数の設定

(defparameter *edic* (make-hash-table :test #'equalp)
	"辞書用データの格納領域の定義　大文字小文字区別なし")				; => *EDIC*

;; (defparameter *edic* (make-hash-table :test #'equal)		; => #<HASH-TABLE :TEST EQUAL :COUNT 0 {CBC9EF1}>
;; 	"辞書用データの格納領域の定義　大文字小文字区別あり") ; => *EDIC*

;; keyの事も考えないといけないから絶対変更不可エラーの元[2017-06-20 07:54:26]
(defparameter *edic-cnt* (make-hash-table :test #'equalp)
	"辞書登録単語の出現回数をセット")			; => *EDIC-CNT*

(defparameter *limit* 3 "単語の意味を表示する回数")			 ; => *LIMIT*

(hash-table-count *edic*)								; => 64

;;; 関数定義
(defun save-edic ()
	"ハッシュテーブルの保存"
	(let ((outfile (make-pathname
									 :directory "/home/hiro/howm/junk/"
									 :name "edic"
									 :type "db")))
		(with-open-file (out outfile
																			 :direction :output
																			 :if-exists :supersede)
			(with-standard-io-syntax
				(print *edic* out)))))					; => SAVE-EDIC

(defun save-edic-cnt ()
	"ハッシュテーブルの保存"
	(let ((outfile (make-pathname
									 :directory "/home/hiro/howm/junk/"
									 :name "edic-cnt"
									 :type "db")))
		(with-open-file (out outfile
																			 :direction :output
																			 :if-exists :supersede)
			(with-standard-io-syntax
				(print *edic-cnt* out)))))			; => SAVE-EDIC-CNT


(defun load-edic (file)
	"ハッシュテーブルの読み込み"
	(with-open-file (in file)
		(with-standard-io-syntax
			(setf *edic* (read in)))))				; => LOAD-EDIC

(defun load-edic-cnt (file)
	"ハッシュテーブルの読み込み"
	(with-open-file (in file)
		(with-standard-io-syntax
			(setf *edic-cnt* (read in)))))		; => LOAD-EDIC-CNT

(load-edic "/home/hiro/howm/junk/edic.db") ; => #<HASH-TABLE :TEST EQUALP :COUNT 64 {D60A641}>
(load-edic-cnt "/home/hiro/howm/junk/edic-cnt.db") ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C552F39}>
;;(save-edic-cnt)																		 ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C4E0CA9}>
;; ハッシュテーブルが固定されてたため、変な値が出ていたエラーを直す[2017-06-14 22:27:45]
;; 結局各ハッシュテーブルに対応するため汎用性をもたせる。
;; (defun load-db (file ht)
;; 	"ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
;; 	(with-open-file (in file)
;; 		(with-standard-io-syntax
;; 			(setf ht (read in)))))						; => LOAD-DB

;; (load-db "/home/hiro/howm/junk/edic.db" *edic*) ; => #<HASH-TABLE :TEST EQUALP :COUNT 9 {CE85AB1}>
;; (load-db "/home/hiro/howm/junk/edic-cnt.db" *edic-cnt*) ; => #<HASH-TABLE :TEST EQUALP :COUNT 9 {CE96151}>

;; 引数設定できるように変更（汎用化）
;; (defun save-db (ht ofile)
;; 	"ハッシュテーブルの保存"
;; 	(let ((outfile (make-pathname
;; 									:directory "/home/hiro/howm/junk/"
;; 									:name ofile
;; 									:type "db")))
;; 		(with-open-file (out outfile
;; 												 :direction :output
;; 												 :if-exists :supersede)
;; 			(with-standard-io-syntax
;; 				(print ht out)))))							; => SAVE-DB

;; (save-db *edic* "edic")									; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {CD916B9}>

;;(save-db *edic-cnt* "edic-cnt")					; =>
;(save-edic-cnt)													; => #<HASH-TABLE :TEST EQL :COUNT 0 {AC14631}>
;; (maphash #'(lambda (key val) (format t "key=~A,val=~A~&" key val)) *edic-cnt*) ; => key=The,val=15

;;(clrhash *edic*)												; => #<HASH-TABLE :TEST EQUAL :COUNT 0 {DE2D6C9}>
;;(clrhash *edic-cnt*)										; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {CA39701}>
;;(save-edic-cnt)													; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {CA39701}>
;; ppcreバージョン[2017-06-13 04:30:37]
;; シンプルに単機能にする。
(defun web-scraping (key)
	"webサイトの特定の部位（ここでは単語の意味）を抽出"
	(let* ((uri "http://ejje.weblio.jp/content/")
				 (article (dex:get (concatenate 'string uri key))))
		(ppcre:register-groups-bind (td)
				("(?is)<td class=content-explanation>(.*?)</td>"
				 article)
			(ppcre:split "\\s" td))))					; => WEB-SCRAPING

;; web-scrapingの中にハッシュ表への格納も入れる。
;; web-scrapingする手前でmecabで日本語の処理もしておく。[2017-06-07 18:04:24]

;; 先頭から文字を読み込み、#\Newlineなら削除
;; standard-char-pで分岐したところにはスペースを追加する。
;; で、句点（. or 。）が出たらS式にする。

;; 読み上げのタイミングは？文字が日本語英語で切り替わったタイミングか
;; 文節末まで来たら。
;; (defun word-unit-process (key)
;; 	"単語単位処理"
;; 	(let ((val nil)
;; 				(cnt 0))
;; 		;;(print key)
;; 		(unless (string-equal key "")
;; 			(setf (gethash key *edic-cnt*) (1+ (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
;; 			;;(setq cnt (incf (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
;; 			(or (setf val (gethash key *edic*));ハッシュから単語の意味を引っ張ってくるか、
;; 					(setf val (web-scraping key)))	;webスクレイピングする。
;; 			(setf (gethash key *edic*) val);*edic*に意味を登録
;; 			(when (<= (gethash key *edic-cnt*) *limit*)					 ;意味を表示する回数を設定
;; 				(print (list key val (gethash key *edic-cnt*)))))))

(defun word-unit-process (key)
	"単語単位処理"
	(let ((val nil)
				(cnt 0))
		;;(print key)
		(unless (string-equal key "")
			;;(setf (gethash key *edic-cnt*) (1+ (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
			(setq cnt (incf (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
			(or (setf val (gethash key *edic*));ハッシュから単語の意味を引っ張ってくるか、
					(setf val (web-scraping key)))	;webスクレイピングする。
			(setf (gethash key *edic*) val);*edic*に意味を登録
			(when (<= cnt *limit*)					 ;意味を表示する回数を設定
				(print (list key val cnt))))))

;; 1+ incfでは若干意味合いが違う。
;; (setf (gethash key ht) (1+ (gethash key ht)))=(incf (gethash key ht))

;;(hash-table-count *edic-cnt*)						; => 0
(defun jep (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせたttsをセットし読み上げる。"
	(let ((tts nil)
				(lst-w nil))
		(unless (equal lst "")
			(if (standard-char-p (car (coerce lst 'list))) ;リストの先頭の文字が英数字ならば
					(progn
						(setq lst-w lst) ;日本語処理と合わせるためにコピーを取る。
						(setq tts "festival --tts")
						)					;yes　festival 改行は削除しない。dotと読み上げてしまう。
					(progn
						;; 分かち書き
						(setq lst-w (cl-mecab:with-mecab ("-Owakati")
													(cl-mecab:parse lst)))
						;; (push (mapcar #'car (cl-mecab:with-mecab ()
						;; 											(cl-mecab:parse* lst))) lst-w)
						;; (setq lst-w (reverse lst-w))
						(setq tts "ojtalk"))))	;no ojtalk
		;;			(format t "~A~%" lst)
		;;(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts));廃止uiopに切り替え[2017-06-13 23:09:43]
		(setq lst-w (ppcre:split "[ ,.?!;:=_、。\\n]"	lst-w));単語単位で分割したリスト
		;;(print lst-w)
		(mapcar #'word-unit-process lst-w)
		(ignore-errors (uiop:run-program (concatenate 'string "echo \"" lst "\" | " tts)))))	; => JEP

;; ppcre:splitの中に-という記号を省くように設定したら、うまく動作しなくなったので注意！[2017-06-14 19:20:37]
;; gethashはoptionalとしてデフォルト値を設定できるので、数値なので0を初期値設定しておく。
;; これをしておかないとエラーが出る。[2017-06-13 14:41:45]
;; keyの出現回数のカウント数を格納する。スクレイピング前に持ってくる。
;;(setf (gethash key *edic-cnt*) (incf (gethash key *edic-cnt* 0))) ; => 4

;; 成功
;;(uiop:run-program "echo \"おはようございます\" | ojtalk") ; => NIL
;;(uiop:run-program "echo \"Good Morning!\" | festival --tts") ; => NIL

(defun read-book (file)
 	"日英各言語毎に文字列を日英各文にまとめ、jepに渡す。"
 	(let ((lst nil))
 		(with-open-file (in file :direction :input)
 			(do ((char (read-char in nil 'eof)
 								 (read-char in nil 'eof)))
 					((eql char 'eof))
				(when (or (and (not (equal lst "")) ;lstが空ではなく、かつ
											 (not (eq (type-of char) (type-of (car lst)))))
									(eq (car lst) #\。)
									(eq (car lst) #\.)
									(eq (car lst) #\！)
									(eq (car lst) #\!)
									(eq (car lst) #\？)
									(eq (car lst) #\?)
									)
					(setq lst (string-trim '(#\Space #\tab #\Newline)(coerce (nreverse lst) 'string))) ;表記されたまま一文単位で表示
					;; (setq lst2 (ppcre:all-matches-as-strings "(\\w+)" (coerce lst1
					;; 																													'string))) ;辞書作成用リスト
					;;					(format t "~a \[~a \%\]" lst (/ (* char-cnt 100.0) total-char-cnt))
					(prin1 lst)
					(jep lst); 読み上げ部(さらにその中に翻訳部）
					;;					(ignore-errors (mapcar #'web-scraping lst2))
					(fresh-line)
					(setq lst nil)
					(save-edic)
					(save-edic-cnt))
				(push char lst)))))											; => READ-BOOK

;; (setq char-cnt (1+ char-cnt))
;; 				(when (eq char-cnt 1)
;; 					(setq total-char-cnt (multiple-value-bind (line)
;; 																	 (ppcre:split "[ ]" (system "wc -m ~/test_dir/walden.txt"))
;; 																 (parse-integer (car line)))))

;; 全体の行数表示
;; (multiple-value-bind (line)
;; 		(ppcre:split "[ ]" (system "wc -l ~/test_dir/walden.txt"))
;; 	(print (car line)))										; =>

;; (cl-mecab:with-mecab ("-Owakati")
;; 					(cl-mecab:parse "今日の天気は、晴れです。")) ; => "今日 の 天気 は 、 晴れ です 。 "

;; lst-e ,lst-jのように日本語と英語のリストを分けて処理にするか？
;; 順番としては表示してから読み上げ。

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

;; [2017-05-27 06:35:01]
;; 翻訳用と読み上げ用、別形式でデータを用意する。
;; 読み上げ用の方はS式は切り替えタイミングか、
;; センテンス単位で。
;; 翻訳用は単語単位で区切ったもの。これについては
;; できた。

;; (setq lst1 '("a" "b"))									; => ("a" "b")
;; (setq lst2 '("d" "e"))									; => ("d" "e")
;; (append lst1 lst2)											; => ("a" "b" "d" "e")
;; (list (car lst1))												; => ("a")
;; (cdr lst1)															; => ("b")
;; (coerce (append lst1 lst2) 'list)				; => ("a" "b" "d" "e")
;; (princ (append lst1 lst2))							; => (a b d e)("a" "b" "d" "e")


;; 単語をカウントして特定数値以上出てくる単語は無視する設定にする。[2017-06-12 12:18:10]
;; そうではなく、漢字のルビのように最初だけ意味を表示して、２回めないし、
;; オプションで設定した回数以上のものは無視する。

;; ppcreで複数デリミタを設定して一気に文字列分割したい時は以下のようにする。[2017-06-12 15:36:57]
;;(ppcre:split "[,!.;:?]" "aa!bb.cc'this is a test'?dd,ee;ff:gg") ; => ("aa" "bb" "cc'this is a test'" "dd" "ee" "ff" "gg")

;; (ppcre:split "[。、]" "今日は、いい天気でした。") ; => ("今日は" "いい天気でした")
;; (car (ppcre:split "[。、]" "今日は、いい天気でした。")) ; => "今日は"


;; ボツ[2017-06-13 14:43:32]
;; (defun make-cnt ()
;; 	(let ((cnt 0))
;; 		(lambda () (incf cnt))))								; => MAKE-CNT

;; (setf cnt1 (make-cnt))									; => #<CLOSURE (LAMBDA () :IN MAKE-CNT) {EBB7CED}>
;; (setq key "walden")											; => "walden"
;; (setq cnt 0)														; => 0
;; (setf val (cons (web-scraping key) (incf cnt))) ; =>


;; (let ((val nil)
;; 			(cnt 0))
;; 	(defun make-data (key)
;; 		(setf val (gethash key *edic*))
;; 		(setf cnt (incf cnt))))							; => MAKE-DATA

;; (setq lst (list "key" "val" 1))				; => ("key" "val" 1)
;; (car lst)															; => "key"
;; (cdr lst)															; => ("val" 1)
;; (cadr lst)														; => "val"
;; (caddr lst)														; => 1
;; (first lst)														; => "key"
;; (second lst)													; => "val"
;; (third lst)														; => 1

;; (push (cons "it" "それ") *kv-alst*)			; => (("it" . "それ"))
;; (print *kv-alst*)												; =>
;; (("it" . "それ")) (("it" . "それ"))

;; (if (setf val (gethash "It" *edic*))
;; 		(print val)
;; 	(print nil))							; =>
;; "それは、それを、それに" "それは、それを、それに"
;; (cl-mecab:with-mecab ("-Owakati")
;; 					(cl-mecab:parse "今日の天気は晴れです"))
;; ; => "今日 の 天気 は 晴れ です\"

;; (cl-mecab:with-mecab ()
;; 					(cl-mecab:parse "今日の天気は晴れです"))
;; ; => "今日	名詞,副詞可能,*,*,*,*,今日,キョウ,キョー,,
;; ;; の	助詞,連体化,*,*,*,*,の,ノ,ノ,,
;; ;; 天気	名詞,一般,*,*,*,*,天気,テンキ,テンキ,,
;; ;; は	助詞,係助詞,*,*,*,*,は,ハ,ワ,,
;; ;; 晴れ	動詞,自立,*,*,一段,連用形,晴れる,ハレ,ハレ,はれ/晴/晴れ,
;; ;; です	助動詞,*,*,*,特殊・デス,基本形,です,デス,デス,,
;; ;; EOS"

;; (cl-mecab:with-mecab ()
;; 					(cl-mecab:parse* "今日の天気は晴れです"))
;; ;; ; => (("今日" "名詞" "副詞可能" "*" "*" "*" "*" "今日" "キョウ" "キョー" "" "")
;; ;;  ("の" "助詞" "連体化" "*" "*" "*" "*" "の" "ノ" "ノ" "" "")
;; ;;  ("天気" "名詞" "一般" "*" "*" "*" "*" "天気" "テンキ" "テンキ" "" "")
;; ;;  ("は" "助詞" "係助詞" "*" "*" "*" "*" "は" "ハ" "ワ" "" "")
;; ;;  ("晴れ" "動詞" "自立" "*" "*" "一段" "連用形" "晴れる" "ハレ" "ハレ" "はれ/晴/晴れ" "")
;; ;;  ("です" "助動詞" "*" "*" "*" "特殊・デス" "基本形" "です" "デス" "デス" "" ""))

;; (setq key "aaa")																							 ; => "aaa"
;; (setq cnt (gethash key *edic-cnt*))														 ; => NIL
;; (setf (gethash key *edic-cnt*) (+ 1 (gethash key *edic-cnt*))) ; =>
;; (setq aaa 1)																									 ; => 1

;; (intern "my-symbol")										; => |my-symbol|
;; (setq |my-symbol| 1)										; => 1
;; |my-symbol|															; => 1
;; (symbol-name '|my-symbol|)							; => "my-symbol"
;; "my-symbol"															; => "my-symbol"
;; |my-symbol|															; => 1
;; (setq |aaa| 1)													; => 1
;; |aaa|																		; => 1
;; (symbol-name '|aaa|)										; => "aaa"
;; (string '|aaa|)													; => "aaa"
;; (intern "aaa")													; => |aaa|
;; (setq (intern "aaa") 1)									; =>
;; (elt "aaa" 0)														; =>
;; :INTERNAL
;; |aaa|																		; => 1
;; (incf |aaa|)														; => 3
;; (symbolp '|aaa|)												; => T
;; (symbolp 2)															; => NIL
;; (symbolp (intern "aaa"))								; => T
;; (intern "aaa")													; => |aaa|
;; :INTERNAL
;; (s)
;; (type-of |aaa|)													; => (INTEGER 0 536870911)
;; |aaa|																		; => 2

;; (export 'my-symbol)											; => T

;; NIL
;; (defun force (lazy-value)
;; 	(funcall lazy-value))									; => FORCE

;; (defmacro lazy (&body body)
;; 	(let ((forced (gensym))
;; 				(value (gensym)))
;; 		`(let ((,forced nil)
;; 					 (,value nil))
;; 			 (lambda ()
;; 				 (unless ,forced
;; 					 (setf ,value (progn ,@body))
;; 					 (setf ,forced t))
;; 				 ,value))))											; => LAZY
;; 高速だが実行時の検査を省略していてデバッグもしづらい
;; 最終的なリリース向け
;;(declaim (optimize (speed 3) (debug 0) (safety 0)))

;; 実行時に検査をしてデバッグもしやすいが遅い
;; 開発途中のデバッグ向け
;; (declaim (optimize (speed 0) (debug 3) (safety 3)))
;; (defun read-book (file)
;; 	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
;; 	(with-open-file (str file :direction :input :if-does-not-exist nil)
;; 									(do ((line (read-line str)
;; 														 (read-line str nil 'eof)))
;; 											((eq line 'eof))
;; 										(setq line (ppcre:all-matches-as-strings "(\\w+)" line))
;; 										(unless (eq line nil)
;; 											(print line)))))	; => READ-BOOK

;; (defun web-scraping (key)
;; 		(let* ((article-html (dex:get (concatenate 'string "http://ejje.weblio.jp/content/" key)))
;; 					 (s-key "class=content-explanation>")
;; 					 (start (+ (length s-key) (search s-key article-html))))
;; 			(subseq article-html start (+ start (search "</td>" (subseq article-html start)))))) ; => WEB-SCRAPING
;; (web-scraping "walden")									; =>
;; (setf (gethash key *edic*) val)

;; (dex:get "http://ejje.weblio.jp/content/walden") ; =>
;; (eq "" "")																	 ; => NIL
;; (setq key "")																 ; => ""
;; (eq key "")																	 ; => NIL
;; (eql key "")																 ; => NIL
;; (equal key "")															 ; => T
;; (equalp key "")															 ; => T
;; (char-equal key "")													 ; =>
;; (string-equal key "")												 ; => T

;; 汚い自作バージョン廃止[2017-06-13 04:24:51]
;; (defun web-scraping (key)
;; 	(let* ((article-html (dex:get (concatenate 'string "http://ejje.weblio.jp/content/" key)))
;; 				 (s-key "class=content-explanation>")
;; 				 (start (+ (length s-key) (search s-key article-html)))
;; 				 (val nil)
;; 				 (c nil))
;; 		(unless (or (setq val (gethash key *edic*)) (string-equal key ""))
;; 			(setq val (subseq article-html start (+ start (search "</td>" (subseq article-html start)))))
;; 			(setf (gethash key *edic*) val))
;; 		(setq c (cons key val))
;; 		(print c)))												; => WEB-SCRAPING
;; web scrapingしてデータを集めてくる事にするので廃止。[2017-06-13 14:58:42]
;; (defun make-dic (file ht)
;; 	"ハッシュテーブルに辞書データを登録する関数"
;; 	(with-open-file (str file :direction :input :if-does-not-exist nil)
;; 									(do ((line (read-line str)
;; 														 (read-line str nil 'eof)))
;; 											((eq line 'eof))
;; 										(let* ((s-line (split "	 " line))
;; 													 (key (string-right-trim '(#\tab) (car s-line)))
;; 													 (val (cdr s-line)))
;; 											(setf (gethash key ht) val))))) ; => MAKE-DIC
;;(make-dic "/home/hiro/howm/ejdic-hand-utf8.txt" *edic*) ; => NIL

;;(ppcre:all-matches-as-strings "(\\w+)" "this is 2 pens=") ; => ("this" "is" "2" "pens")

;;(append '("abc") '("def"))							; => ("abc" "def")
;; (concatenate 'list '("abc" "edf" "fee")) ; => ("abc" "edf" "fee")
;; (concatenate 'string "abc" "def")				 ; => "abcdef"
;; (setq lst '("abc" "bed" "deb"))					 ; => ("abc" "bed" "deb")
;; (format nil "~{ ~A~}" lst)							 ; => " abc bed deb"
;; (format nil "~{~A ~}" lst)							 ; => "abc bed deb "
;; (format nil "~{~A~}" lst)								 ; => "abcbeddeb"
;;  (defparameter *my-string* (string "カレー食べたい")) ; => *MY-STRING*
;;  (map 'string #'(lambda (c) (print c)) *my-string*)				; =>
;; #\KATAKANA_LETTER_KA
;; #\KATAKANA_LETTER_RE
;; #\KATAKANA-HIRAGANA_PROLONGED_SOUND_MARK
;; #\U98DF
;; #\HIRAGANA_LETTER_BE
;; #\HIRAGANA_LETTER_TA
;; #\HIRAGANA_LETTER_I "カレー食べたい"
;; (null "")									 ; => NIL
;; (eq "" nil)								 ; => NIL
;; (eql "" "")															; => NIL
;; (equal "" "")							 ; => T
;; (equalp "" "")													; => T

;; (cl-mecab:with-mecab ()
;; 					(cl-mecab:parse* "今日の天気は晴れです"))

;; (coerce  '(#\h #\a #\r #\e) 'string)		; => "hare"
;; (coerce "it's fine day" 'list)					; => (#\i #\t #\' #\s #\  #\f #\i #\n #\e #\  #\d #\a #\y)
;; (coerce  (coerce "it's fine day" 'list) 'string) ; => "it's fine day"
;; (time (let ((lst '("It" "s" "fine" "day" "today")))
;; 				(labels ((f (lst)
;; 									 (print (car lst))
;; 									 (unless (null (cdr lst))
;; 										 (f (cdr lst)))))
;; 					(f lst))))									 	; =>

;; ;; 末尾再帰の処理を書いてみたが、mapcarを使った方が速そうだ。
;; (defun recursive (lst)
;; 	(print (car lst))
;; 	(unless (null (cdr lst))
;; 		(recursive (cdr lst))))							; => RECURSIVE
;; (time (recursive lst))									; =>

;; (setq lst '("It" "s" "fine" "day" "today")) ; => ("It" "s" "fine" "day" "today")
;; (time (mapcar #'print lst))							; =>
;; timeで計測した結果mapcarの方が自作再帰関数よりプロセスサイクルが少ない。
;; (graphic-char-p #\1)										; => T
;; (not (null (digit-char-p #\1)))					; => T
;; (not (null (digit-char-p #\a)))					; => NIL
;; (unless (null (digit-char-p #\1))
;; 	(print 'ok))													; =>

;; (numberp #\1)														; => NIL
;; (alphanumericp #\1)											; => T
;; (alpha-char-p #\2)											; => NIL
;; (digit-char-p #\1)											; => 1
;; (digit-char-p #\a)											; => NIL

;; (setq lst '("ほげ" "ふー" "ばー"))			; => ("ほげ" "ふー" "ばー")

;; リスト内の文字列を逐一舐めていく関数
;; (defun f (lst)
;; 	(print (car lst))
;; 	(when (not (null (cdr lst)))
;; 		(f (cdr lst))))											; => F
;; (f lst)																	; =>
;; "ほげ"
;; "ふー"
;; "ばー" NIL
;; (car lst)																; => "ほげ"
;; (car (cdr lst))													; => "ふー"
;; (car (cdr (cdr (cdr lst))))							; => NIL
;; (print (car lst))												; =>
;; "ほげ" "ほげ"

;;(type-of char)													; => STANDARD-CHAR
;; シンプルな表現
;;				(standard-char-p char)									; => T
;; 冗長な表現
;;				(eq (type-of char) 'standard-char)			; => T

;; (defun read-book (file)
;; 	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
;; 	(let ((lst nil))
;; 		(with-open-file (in file :direction :input)
;; 			(do ((char (read-char in nil 'eof)
;; 								 (read-char in nil 'eof)))
;; 					((eql char 'eof))
;; 				(when (or (and (not (null lst)) ;lstが空ではなく、かつ
;; 											 (not (eq (type-of char) (type-of (car lst))))) ;前のchar≠今のchar
;; 									(eq (car lst) #\。)
;; 									(eq (car lst) #\.)
;; 									(eq (car lst) #\！)
;; 									(eq (car lst) #\!)
;; 									(eq (car lst) #\？)
;; 									(eq (car lst) #\?)
;; 									(eq (car lst) #\:)
;; 									(eq (car lst) #\;)
;; 									)
;; 					(set-tts (nreverse lst))
;; 					(setq lst nil))
;; 				(setq lst (cons char lst))))))
;;(read-book (cadr (argv)))										; =>
																				;(read-book (third (argv)))										; =>

;;(parse-integer "42")										; => 42
;; (setq lst-w '("it's" "fine" "day"))			; => ("it's" "fine" "day")
;; (mapcar  #'(lambda (key) (setf (gethash key *edic-cnt*) (incf (gethash key *edic-cnt* 0)))) lst-w) ; => (2 2 2)

;; 現在の行が全体のうちの何％の位置にいるのかを表示する。
;; load-dbされてない気がする。

;; クロージャーが効いてて辞書が有効に次のプログラム実行時に効いてない。[2017-06-18 20:59:04]
;; クロージャーではなくて~/howm/junk//edic.dbというファイルを探している。
;; そこらへんの対応すればOKっぽい。
;;(maphash #'(lambda (key val) (format t "key=~A,val=~A~&" key val)) *edic*)
																				; => NIL
;;(maphash #'(lambda (key val) (format t "key=~A,val=~A~&" key val)) *edic-cnt*)

;; festival だけしばらくsuspendされない問題。[2017-06-20 08:04:56]
;; 方向性としてはとりあえず全単語を先にハッシュテーブルに読み込ませておきたい。
;; 平行処理が必要か？[2017-06-20 08:06:20]
