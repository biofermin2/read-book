#!/usr/bin/sbcl --noinform
																				; -*- mode: Lisp; coding:utf-8 -*-
;; 開発ゴール
;; 前回はoutファイルを作ってしおり機能を実現したが、
;; 今回はwith-open-streamを使って実現出来ないか
;; 試してみる。->完了[2017-07-13 20:44:46]
;; 最後の単語の訳の表示が遅れる。原因を調べる必要あり。
;; ->完了[2017-07-13 20:45:01]　最後の単語出力後に(fresh-line)を追加すればOK

;; どうも見てると""を読み飛ばしている。
;; cl-ppcreの方で"を抜く処理が必要。
;; あと、*や=もついでに削除した方がいいかもしれない。

;; 現在読み込み位置の表示機能の追加
;; 日本語文字１文字は３文字分としてカウントされている。
;; ->対応済[2017-07-14 06:54:26]

;; 全角スペースだとmaybe invalid headerと表示される。[2017-07-15 09:45:41]
;; 更新情報
;; load-dbの汎用化
;; ストリーム上でのしおり機能の実現
;; いちいち外部ファイルに読み書きする手間が減った。
;; 現在読み込み位置の表示機能の追加


;; \の入った文は読み飛ばしている様子[2017-07-06 06:27:18]
;; ”で囲まれているところに\"となっているので、ppcreで"も削除対象にする[2017-07-13 18:06:34]
;; ->うまくいかなかった。おそらくfestival側に渡す際の文字列の問題。
;; なぜなら読み上げだけがうまく行ってない。表示はOKだから。[2017-07-13 20:46:04]




;; 当初最後に読み込んだ一文を記録してそれをsearchかけて
;; substとか使ってそれ以前の文を空白と置換して削除する
;; という方法を考えていたが、同じ文章が出てくる場合には
;; 思った通りの動作をしない可能性があるので、
;; char-countを作ってそれでやってみる。
;; subseqを使うとよさ気。
;; unwind-protectはwith-系のマクロではデフォルトで装備しているから
;; 特に設ける必要なし。

;; 全てのwith-open-fileには:external-format :utf-8をつけた方がいいらしい。
(funcall (lambda ()
					 '(:program-name "read-book.lisp"
						 :version "v3.4"
						 :copyright 2017 :author "Higashi, Hiromitsu"
						 :license "MIT")))					; => (:PROGRAM-NAME "read-book.lisp" :VERSION "v3.4" :COPYRIGHT 2017 :AUTHOR
 ;; "Higashi, Hiromitsu" :LICENSE "MIT")

;;; LOAD

(load "/home/hiro/howm/junk/utils.lisp"
			:external-format :utf-8)					; => T
(load "~/quicklisp/dists/quicklisp/software/cl-mecab/cl-mecab.asd"
			:external-format :utf-8)					; =>

;; quicklispのhomeを調べる時は以下を実行する。[2017-06-13 14:57:02]
;;ql:*quicklisp-home*					; => #P"/home/hiro/quicklisp/"

(ql:quickload	'(:cl-ppcre :dexador :cl-mecab :cl-rainbow) :silent t) ; => (:CL-PPCRE :DEXADOR :CL-MECAB)
(setf cl-rainbow:*enabled* t)
;; (print (cl-rainbow:color :red "red string"))
;; (print (cl-rainbow:color #x5599ff "rgb color code"))
;; (loop for c across "rainbow" do (format t "~A" (cl-rainbow:color (random #xffffff) c)))
;;:black :red :green :yellow :blue :magenta :cyan :white :default



;; 大域変数の設定

(defparameter *edic* (make-hash-table :test #'equalp)
	"辞書用データの格納領域の定義　大文字小文字区別なし") ; => *EDIC*

;; (defparameter *edic* (make-hash-table :test #'equal)		; => #<HASH-TABLE :TEST EQUAL :COUNT 0 {CBC9EF1}>
;; 	"辞書用データの格納領域の定義　大文字小文字区別あり") ; => *EDIC*

;; keyの事も考えないといけないから絶対変更不可エラーの元[2017-06-20 07:54:26]
(defparameter *edic-cnt* (make-hash-table :test #'equalp)
	"辞書登録単語の出現回数をセット")			; => *EDIC-CNT*

(defparameter *last-char* (make-hash-table :test #'equal)
	"最後に読み込んだ文字の位置をセット")	; => *LAST-CHAR*

(defparameter *limit* 10 "単語の意味を表示する回数") ; => *LIMIT*

(defparameter *repeat-read-time* 2 "例文読み上げの繰り返し回数")

;;(hash-table-count *edic*)								; => 0

;;; 関数定義
;; saveは問題なし[2017-07-05 20:38:11]
;; ファイル名が自動的に大文字になるので、小文字に戻す事によりエラー解決[2017-07-05 21:51:02]
(defun save-db (ht ofile)
	"ハッシュテーブルの保存"
	(let ((outfile (make-pathname
									:directory '(:absolute "home" "hiro" "howm" "junk")
									:name (string-downcase ofile);[2017-07-05 21:51:17]修正
									:type "db")))
	;;	(print outfile)
		(with-open-file (out outfile
												 :direction :output
												 :if-exists :supersede
												 :external-format :utf-8)
			(with-standard-io-syntax
				(print ht out)))))							; => SAVE-DB
;;(save-db *edic* "edic")									; => #<HASH-TABLE :TEST EQUALP :COUNT 1 {B865071}>
;;(save-db *edic-cnt* "edic-cnt")					; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C830031}>
;; load側に問題があるように思われる。[2017-07-05 20:38:23]
;; (defun load-db (file ht)
;; 	"ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
;; 	(let ((infile (make-pathname
;; 							 :directory '(:absolute "home" "hiro" "howm" "junk")
;; 							 :name (string-downcase file)
;; 							 :type "db")))
;; 					(with-open-file (in infile :if-does-not-exist nil)
;; 					(with-standard-io-syntax
;; 						(setf ht (read in))))))			; => LOAD-DB

;; ;; (load-db 'edic *edic*)				 ; => #<HASH-TABLE :TEST EQUALP :COUNT 36 {D7B2131}>
;; ;; (load-db 'edic-cnt *edic-cnt*) ; => #<HASH-TABLE :TEST EQUALP :COUNT 36 {CE71659}>
;; ;; (load-db 'last-char *last-char*)				; => #<HASH-TABLE :TEST EQUAL :COUNT 1 {CE79761}>
;; (load-db "edic" *edic*)					 ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C8E96C1}>
;; (load-db  "edic-cnt" *edic-cnt*)				; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C86E6C1}>
;; (load-db "last-char" *last-char*)				; => #<HASH-TABLE :TEST EQUAL :COUNT 1 {C875751}>

;; (setq file "~/howm/junk/edic.db")				; => "~/howm/junk/edic.db"
;; (pathname-directory (pathname file))		; => (:ABSOLUTE :HOME "howm" "junk")
;; (pathname-name (pathname file))					; => "edic"
;; (pathname-type (pathname file))					; => "db"
;; (intern (string-downcase (concatenate 'string "*" (pathname-name (pathname file)) "*"))) ; => |*edic*|

;; 汎用化成功！[2017-07-06 12:20:58]
(defun load-db (file ht)
	"ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
	(let ((infile (make-pathname
							 :directory "/home/hiro/howm/junk/"
							 :name (string-downcase file)
							 :type "db")))
		(with-open-file (in infile :if-does-not-exist nil)
			(with-standard-io-syntax
				(setf ht (read in))))))					; => LOAD-DB

;;(load-db 'edic *edic*)				; => #<HASH-TABLE :TEST EQUALP :COUNT 586 {C5E4031}>
;;(hash-table-count *edic*)			; => 586
;;(clrhash *edic*)										 ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C0B0031}>
;;(clrhash *edic-cnt*)								 ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C830031}>
(setf *edic* (load-db 'edic *edic*)) ; => #<HASH-TABLE :TEST EQUALP :COUNT 586 {C61D031}>
;;(hash-table-count *edic*)								; => 586
(setf *edic-cnt* (load-db 'edic-cnt *edic-cnt*)) ; => #<HASH-TABLE :TEST EQUALP :COUNT 586 {C830031}>
;;(hash-table-count *edic-cnt*)								 ; => 586
(setf *last-char* (load-db 'last-char *last-char*)) ; => #<HASH-TABLE :TEST EQUAL :COUNT 5 {C31D9F9}>
;;(hash-table-count *last-char*)											; => 5


;; (defun load-db (file ht)
;; 	"ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
;; 	(let (ht (string-downcase ht))
;; 		(with-open-file (in file :if-does-not-exist nil)
;; 			(with-standard-io-syntax
;; 				(setf ht (read in))))))					; => LOAD-DB


;; read-from-stringもinternも文字列の証拠であるダブルクォーテーションを
;; はずす役割があるが、read-from-stringは大文字に変換され、また文字数を
;; 多値として返す。internはシンボルに変換するのみ。



;; (defun save-edic ()
;; 	"ハッシュテーブルの保存"
;; 	(let ((outfile (make-pathname
;; 									:directory "/home/hiro/howm/junk/"
;; 									:name "edic"
;; 									:type "db")))
;; 		(with-open-file (out outfile
;; 												 :direction :output
;; 												 :if-exists :supersede
;; 												 :if-does-not-exist :create)
;; 			(with-standard-io-syntax
;; 				(print *edic* out)))))					; => SAVE-EDIC

;; (defun save-edic-cnt ()
;; 	"ハッシュテーブルの保存"
;; 	(let ((outfile (make-pathname
;; 									:directory "/home/hiro/howm/junk/"
;; 									:name "edic-cnt"
;; 									:type "db")))
;; 		(with-open-file (out outfile
;; 												 :direction :output
;; 												 :if-exists :supersede
;; 												 :if-does-not-exist :create)
;; 			(with-standard-io-syntax
;; 				(print *edic-cnt* out)))))			; => SAVE-EDIC-CNT

;; (defun save-last-char ()
;; 	"読み上げた文章と最後に読み上げた箇所の記録"
;; 	(let ((outfile (make-pathname
;; 									:directory "/home/hiro/howm/junk/"
;; 									:name "last-char"
;; 									:type "db")))
;; 		(with-open-file (out outfile
;; 												 :direction :output
;; 												 :if-exists :supersede
;; 												 :if-does-not-exist :create)
;; 			(with-standard-io-syntax
;; 				(print *last-char* out)))))			; => SAVE-LAST-CHAR
;;(save-last-char)												; => #<HASH-TABLE :TEST EQUAL :COUNT 0 {D07E6C1}>
;; loadもsaveと同じく汎用性をもたせようとしたが、失敗。[2017-07-06 11:53:16]
;; ->load-dbした結果をさらにhashtableにsetしてあげると出来た。。。[2017-07-06 12:20:17]

;; (defun load-edic (file)
;; 	"ハッシュテーブルの読み込み"
;; 	(with-open-file (in file
;; 											:if-does-not-exist nil
;; 											:external-format :utf-8)
;; 		(with-standard-io-syntax
;; 			(setf *edic* (read in)))))				; => LOAD-EDIC

;; (defun load-edic-cnt (file)
;; 	"ハッシュテーブルの読み込み"
;; 	(with-open-file (in file
;; 											:if-does-not-exist nil
;; 											:external-format :utf-8)
;; 		(with-standard-io-syntax
;; 			(setf *edic-cnt* (read in)))))		; => LOAD-EDIC-CNT

;; (defun load-last-char (file)
;; 	"ハッシュテーブルの読み込み"
;; 	(with-open-file (in file
;; 											:if-does-not-exist nil
;; 											:external-format :utf-8)
;; 		(with-standard-io-syntax
;; 			(setf *last-char* (read in)))))		; => LOAD-LAST-CHAR

;; (load-edic "~/howm/junk/edic.db")				; => #<HASH-TABLE :TEST EQUALP :COUNT 3697 {DB91A11}>

;; (load-edic-cnt "~/howm/junk/edic-cnt.db") ; => #<HASH-TABLE :TEST EQUALP :COUNT 3483 {E108119}>
;; (load-last-char "~/howm/junk/last-char.db") ; =>

;;(save-edic-cnt)																		 ; => #<HASH-TABLE :TEST EQUALP :COUNT 0 {C4E0CA9}>
;; ハッシュテchar固定されてたため、変な値が出ていたエラchar[2017-06-14 22:27:45]
;; ->fileをstringで囲んで対処。[2017-07-05 20:05:32]
;; 結局各ハッシュテーブルに対応するため汎用性をもたせる。

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
																(ppcre:split "\\s" td)))) ; => WEB-SCRAPING

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
				(princ (list (cl-rainbow:color :cyan key)
										 (cl-rainbow:color :magenta (car val))
										 (cl-rainbow:color :blue cnt)))
				(fresh-line);ここの入れると各単語に空白行が追加されるので、縦に間延び
				;;して見難くなったり、表示しきれなくなる場合が出るので却下[2017-07-13 17:59:45]
				))))														; => WORD-UNIT-PROCESS

;; 1+ incfでは若干意味合いが違う。
;; (setf (gethash key ht) (1+ (gethash key ht)))=(incf (gethash key ht))

;;(hash-table-count *edic-cnt*)						; => 0
(defun jep (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせた処理を行う。"
	(let ((tts nil)
				(lst-w nil))
		(unless (equal lst "")
			(if (standard-char-p (car (coerce lst 'list))) ;リストの先頭が英数字ならば
					(progn
						(setq lst-w lst) ;日本語処理と合わせるためにコピーを取る。
						(setq tts "festival --tts"));yes　festival 改行は削除しない。
					(progn
						(setq lst-w (cl-mecab:with-mecab ("-Owakati")
													(cl-mecab:parse lst)))						; 分かち書き
						;; (push (mapcar #'car (cl-mecab:with-mecab ()
						;; 											(cl-mecab:parse* lst))) lst-w)
						;; (setq lst-w (reverse lst-w))
						(setq tts "ojtalk"))))	;no ojtalk
		;;			(format t "~A~%" lst)
		;;(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts));廃止uiopに切り替え[2017-06-13 23:09:43]
		(setq lst-w (ppcre:split "[ ,.?!;:=_、。\\\"\\n\\*\\--]"	lst-w));単語単位の分割リスト
		;;(print lst-w)
		(mapcar #'word-unit-process lst-w)
		(fresh-line) ;最後の単語だけ読み上げ時に表示されなかったトラブル対処。消すな！
		;;[2017-07-13 18:00:20]
		;;(terpri)
		(ignore-errors
			(dotimes (x *repeat-read-time*)
				(uiop:run-program
				 (concatenate 'string "echo \"" lst "\" | " tts)
				 ;;											(concatenate 'string "echo " lst " | " tts)
				 )))))													; => JEP

;;(jep "こんにちは")											; =>
;;(jep "hello")														; =>

;; ppcre:splitの中に-という記号を省くように設定したら、うまく動作しなくなったので注意！[2017-06-14 19:20:37]
;; gethashはoptionalとしてデフォルト値を設定できるので、数値なので0を初期値設定しておく。
;; これをしておかないとエラーが出る。[2017-06-13 14:41:45]
;; keyの出現回数のカウント数を格納する。スクレイピング前に持ってくる。
;;(setf (gethash key *edic-cnt*) (incf (gethash key *edic-cnt* 0))) ; => 4

;; 成功
;;(uiop:run-program "echo \"おはようございます\" | ojtalk") ; => NIL
;;(uiop:run-program "echo \"Good Morning!\" | festival --tts") ; => NIL
;; ファイルの最終書き込み日時の表示
(defun stamp-write-date (file)
	(multiple-value-bind
				;;					(sec min hr day mon yr dow d-p tz)
				(sec min hr day mon yr)
			(decode-universal-time (file-write-date file))
		(format nil "~d-~2,'0d-~2,'0d\ ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec))) ; => STAMP-WRITE-DATE

;; 成功！　しおり機能

;; (with-open-file (in file :direction :input :element-type '(unsigned-byte 8))
;;   (let ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in))))
;; 		(read-sequence buf in)
;; 		(let* ((pn (pathname file))
;; 					 (ofile (make-pathname
;; 									 :directory (pathname-directory pn)
;; 									 :name (pathname-name pn)
;; 									 :type "out"))
;; 					 (char-cnt (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0)))
;; 			(with-open-file (out ofile :direction :output :if-exists
;; 													 :supersede :if-does-not-exist :create)
;; 				(write-string (subseq (sb-ext:octets-to-string buf :external-format
;; 																											 :utf-8) char-cnt) out)))))

;; (let ((ofile (make-pathname
;; 								:directory "/home/hiro/howm/junk/"
;; 								:name "edic"
;; 								:type "db")))
;; 	(with-open-file (out ofile
;; 											 :direction :output
;; 											 :if-exists :supersede)
;; 		(with-standard-io-syntax
;; 			(print *edic* out))))

;; infileを利用してそこからoutfileを自動生成するのに参考になりそう。
;; (defun write-html (file)
;; 	 (let* ((pn (pathname file))
;; 					(outfile (make-pathname
;; 										:directory (pathname-directory pn)
;; 										:name (pathname-name pn)
;; 										:type "html")))
;; 		 (with-open-file (*standard-output* outfile
;; 																				:direction :output
;; 																				:if-exists :supersede)
;; 			 (html
;; 				 (head
;; 					 (title "翻訳ページ"))
;; 				 (body
;; 					 (set-htm (reverse *kv-alst*))))))) ; => WRITE-HTML
;; hash-cntはindexに置き換えたため廃止[2017-07-13 17:02:07]
;;(defvar hash-cnt)												; => HASH-CNT

;;ここにしおり機能追加するか？
;; (defun bookmark (file)
;; 	(with-open-file (in file
;; 											:direction :input
;; 											:element-type '(unsigned-byte 8)
;; 											:external-format :utf-8)
;; 		(let ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in))))
;; 			(read-sequence buf in)
;; 			(setq hash-cnt
;; 						(lambda () (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0)))
;; 			(with-open-stream (*standard-output* (write-string
;; 																						(subseq
;; 															 (sb-ext:octets-to-string buf :external-format :utf-8)
;; 															 (funcall hash-cnt)) *standard-output*)))))) ; => BOOKMARK


;; (defun bookmark (file)
;; 	(with-open-file (in file
;; 											:direction :input
;; 											:element-type '(unsigned-byte 8)
;; 											:external-format :utf-8)
;; 		(let ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in))))
;; 			(read-sequence buf in)
;; 			(setf hash-cnt (lambda () (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0))
;; 						strm (make-string-output-stream))
;; 			(write-string (subseq (sb-ext:octets-to-string buf :external-format :utf-8) hash-cnt) strm)))) ; => BOOKMARK



;; (defun bookmark (file ofile)
;; 	(with-open-file (in file
;; 											:direction :input
;; 											:element-type '(unsigned-byte 8)
;; 											:external-format :utf-8)
;; 		(let ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in))))
;; 			(read-sequence buf in)
;; 			(setq hash-cnt
;; 						(lambda () (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0)))
;; 			(with-open-file (out ofile
;; 													 :direction :output
;; 													 :if-exists	:supersede
;; 													 :if-does-not-exist :create)
;; 				(write-string
;; 				 (subseq
;; 					(sb-ext:octets-to-string buf :external-format :utf-8)
;; 					(funcall hash-cnt)) out)))))	; => BOOKMARK


;; しおり機能end
;; 条件分岐でchar-cntが0ならそのまま0以外ならしおり機能を通してファイルを変更というのにすれば
;; いいのではないか？[2017-07-02 07:59:35]

;; クロージャーのカウンター作成関数の定義
;; (defun make-cnt ()
;; 			(let ((cnt 0))
;; 				(lambda () (incf cnt))))						; => MAKE-CNT
;; (setf char-cnt (make-cnt))							; => #<CLOSURE (LAMBDA () :IN MAKE-CNT) {E1DA41D}>
;; (setq file "~/test_dir/w.txt")					; => "~/test_dir/w.txt"
;; (setq char-cnt (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0)) ; => 0
;; (zerop char-cnt)												; => T
;; (if (zerop (setq char-cnt (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0)))
;; 		(print t)
;; 		(print nil))												; =>

;; (lambda (file)
;; 	(let ((pn (pathname file)))
;; 		(setf ofile (make-pathname :directory (pathname-directory pn)
;; 															 :name (pathname-name pn)
;; 															 :type "out")))) ; => #<FUNCTION (LAMBDA (FILE)) {BE1114D}>

;; read-bookのletで定義したofileがbookmark関数の方で読み込めないのは
;; 引数として渡してないから。
;; bookmarkの方でもofileの定義をしなくていいようにするには引数として渡す必要あり。
;; 例：(bookmark file ofile)
;; ;; 末尾再帰の処理を書いてみたが、mapcarを使った方が速そうだ。
;; (defun recursive (lst)
;; 	(print (car lst))
;; 	(unless (null (cdr lst))
;; 		(recursive (cdr lst))))							; => RECURSIVE
;; (time (recursive lst))									; =>
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;   100.00% CPU
;;   37,504 processor cycles
;;   0 bytes consed

;; "It"
;; "s"
;; "fine"
;; "day"
;; "today" NIL

;; (setq lst '("It" "s" "fine" "day" "today")) ; => ("It" "s" "fine" "day" "today")
;; (time (mapcar #'print lst))									; =>
;; (defun bookmark (file)
;; 	"文字単位でのストリームからの読み込み処理"
;; 	(with-open-file (in file
;; 											:direction :input
;; 											:element-type '(unsigned-byte 8)
;; 											:external-format :utf-8)
;; 		(let ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in)))
;; 					(index 10))
;; 			(read-sequence buf in)
;; 			(with-open-stream (s (make-string-input-stream (subseq (sb-ext:octets-to-string buf :external-format :utf-8) index)))
;; 				(loop for char = (read-char s nil) while char
;; 						 do (print char))
;; 				))))														; => BOOKMARK
(defun total-char-cnt (file)
	"ファイルの全文字数を表示する"
	(with-open-file (in file)
		(do ((char (read-char in nil nil)
							 (read-char in nil nil))
				 (cnt -1 (incf cnt)));どうも最後のnilもカウントするので、その分も引いておく必要あり[2017-07-15 12:44:31]
				((eql char nil) cnt))))	; => TOTAL-CHAR-CNT

(defun read-book (file)
 	"日英各言語毎に文字列を日英各文にまとめ、日英判定処理のjepに渡す。"
	(let* ((lst nil)
				 ;;(char-cnt 0)
				 (index (gethash
								 (concatenate 'string file (stamp-write-date file))
								 *last-char* 0)));最後に読み込んだ文字の位置情報
		(if (progn (uiop:run-program "echo \"前回しおりをはさんだところから読みますか？\" | ojtalk")
							 (y-or-n-p "前回しおりをはさんだ所から読みますか？"))
				(progn
					(format t "では、しおりの箇所から読みます。~%")
					(uiop:run-program "echo \"では、しおりの箇所から読みます。\" | ojtalk"))
				(progn
					(format t "では、本の最初から読み直します。~%")
					(uiop:run-program "echo \"では、本の最初から読み直します。\" | ojtalk")
					(setf index 0)));文章の冒頭から読み上げるようにindexをゼロリセットする。
		(with-open-file (in file
												:direction :input
												:element-type '(unsigned-byte 8)
												:external-format :utf-8)
			;; (do ((char (read-char in nil 'eof)
			;; 					 (read-char in nil 'eof))
			;; 		 (char-cnt 0 (incf char-cnt)))
			;; 		((eql char 'eof)
			;; 		 (progn (setq t-cnt char-cnt)
			;; 						(print t-cnt))))
			;;(print (file-length in));全文字数の値としては使えない[2017-07-15 11:39:38]
			(let* ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in)))
						 ;;(char-cnt 0) ;やはりdoに格納する[2017-07-15 11:06:42]
						 (total-cnt (total-char-cnt file)))
				(read-sequence buf in)
				(with-open-stream	(s (make-string-input-stream
															(subseq (sb-ext:octets-to-string buf
																															 :external-format
																															 :utf-8) index)))
					;;(length buf)
					;; (loop for char = (read-char s nil) while char
					;; 	 do
					(do ((char (read-char s nil 'eof)
										 (read-char s nil 'eof))
							 (char-cnt 0 (incf char-cnt))) ;最初のカウントは0ではなく1にする。要注意。
							((eql char 'eof) (print '読了))
						;;		 (incf char-cnt) ;doに格納したため必要なし[2017-07-13 20:04:36]
						;; (setq char-cnt
						;; 			(+ (1+ char-cnt)
						;; 				 (gethash (concatenate 'string
						;; 															 file (stamp-write-date file)) *last-char* 0)))
						;;							 (print char)
						(when (or (and (not (equal lst "")) ;lstが空ではなく、かつ
													 (not (eq (type-of char) (type-of (car lst)))))
											(eq (car lst) #\。)
											(eq (car lst) #\.)
											(eq (car lst) #\！)
											(eq (car lst) #\!)
											(eq (car lst) #\？)
											(eq (car lst) #\?)
											);最初の一文字がここを通過する。
							;;(print char)
							;;(print lst)
							;;(setq char-cnt (+ char-cnt (length lst)))
							;;								  		 (setq char-cnt (+ char-cnt (length lst)))
							(setq lst (string-trim '(#\Space #\tab #\Newline)(coerce (nreverse lst) 'string))) ;表記されたまま一文単位で表示
							;; (setq lst2 (ppcre:all-matches-as-strings "(\\w+)" (coerce lst1
							;; 																													'string))) ;辞書作成用リスト
							;;					(format t "~a \[~a \%\]" lst (/ (* char-cnt 100.0) total-char-cnt))
							;; (print lst)
							;;(print (length (sb-ext:string-to-octets buf)))
							;; (format t
							;; 				"~a \[~d/~d==>~d\%\]~&"
							;; 				(cl-rainbow:color :yellow lst)
							;; 				(cl-rainbow:color :red (+ char-cnt index))
							;; 				(cl-rainbow:color :red total-cnt)
							;; 				(cl-rainbow:color :red (round (float (/ (* (+ char-cnt index) 100) total-cnt)))))
							(format t	"~a " (cl-rainbow:color :yellow lst))
							(princ (cl-rainbow:color :red "["))
							(format t "~:d/~:d" (+ char-cnt index) total-cnt)
							(princ (cl-rainbow:color :red " ==> "))
							(format t "~d\%" (round (float (/ (* (+ char-cnt index) 100) total-cnt))))
							(princ (cl-rainbow:color :red "]"))
							(fresh-line)
							;;(format t "~a \[~d\%\]~&" lst (round (float (/ (* (+ char-cnt index) 100) total-cnt))))
							;;(sleep 8)
							(setf (gethash (concatenate 'string file (stamp-write-date file)) *last-char*) (+ char-cnt index))
							(save-db *last-char* 'last-char)
							(jep lst); 読み上げ部(さらにその中に翻訳部）
							;;					(ignore-errors (mapcar #'web-scraping lst2))
							;;(fresh-line)
							(terpri)
							(setq lst nil)
							(save-db *edic* 'edic)
							(save-db *edic-cnt* 'edic-cnt))
						(push char lst)))))))											; => READ-BOOK


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
;;(time (read-book "~/test_dir/walden.txt"))		; =>

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
				(read-book file-name)
				)))
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

;;(ppcre:split "[。、]" "今日は、いい天気でした。") ; => ("今日は" "いい天気でした")


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
;; (defparameter *my-string* (string "Groucho Marx")) ; => *MY-STRING*
;; (map 'string #'(lambda (c) (print c)) *my-string*)		 ; =>
;; #\G
;; #\r
;; #\o
;; #\u
;; #\c
;; #\h
;; #\o
;; #\
;; #\M
;; #\a
;; #\r
;; #\x "Groucho Marx"
;; (null "")									 ; => NIL
;; (eq "" nil)								 ; => NIL
;; (eql "" "")															; => NIL
;; (equal "" "")							 ; => T
;; (equalp "" "")													; => T

;; (cl-mecab:with-mecab ()
;; 					(cl-mecab:parse* "今日の天気は晴れです"))

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
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;   100.00% CPU
;;   37,504 processor cycles
;;   0 bytes consed

;; "It"
;; "s"
;; "fine"
;; "day"
;; "today" NIL

;; (setq lst '("It" "s" "fine" "day" "today")) ; => ("It" "s" "fine" "day" "today")
;; (time (mapcar #'print lst))									; =>
;; Evaluation took:
;;   0.000 seconds of real time
;;   0.000000 seconds of total run time (0.000000 user, 0.000000 system)
;;   100.00% CPU
;;   26,801 processor cycles
;;   0 bytes consed
;; "It"
;; "s"
;; "fine"
;; "day"
;; "today" ("It" "s" "fine" "day" "today")

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


;; (setf (get (intern "my") 'value) "私の") ; => "私の"
;; (setf (get (intern "my") 'count) 3)			 ; => 3
;; (symbol-plist (intern "my"))						 ; => (COUNT 3 VALUE "私の")

;; (car (symbol-plist (intern "my")))			; => COUNT
;; (get (intern "my") 'count)							; => 3
;; (setf (get (read-from-string "mine") 'value) "私のもの") ; => "私のもの"
;; (get (read-from-string "mine") 'value)									 ; => "私のもの"
;; ;; internもread-from-stringも同じように使えるが、internの方が
;; ;; 表記が短いかな。[2017-06-20 20:47:33]

;; ;; ファイルの最終書き込み日時の表示
;; (let ((file "~/test_dir/ruルー語.txt"))
;; 	(multiple-value-bind
;; 				(sec min hr day mon yr dow d-p tz)
;; 			(decode-universal-time (file-write-date file))
;; 		(format nil "~d-~2,'0d-~2,'0d\ ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec))) ; => "2017-06-19 07:32:39"

;; (mapcar #'remove '(#\! #\?) #(#\a #\!))	; =>

;; (remove #\! '(#\a #\!))									; => (#\a)
;; (make-sequence 'vector 4)															; => #(0 0 0 0)
;;(length ' (#\Newline #\tab #\Space))		; => 3
;; (length "あいう")												; => 3
;; (length "abc")													; => 3

;; (alpha-char-p #\Newline)								; => NIL
;; (alphanumericp #\tab)										; => NIL
;; (standard-char-p #\Space)								; => T
;; (type-of #\Newline)											; => STANDARD-CHAR
;; (type-of #\あ)													; => EXTENDED-CHAR
;; (type-of #\Tab)													; => BASE-CHAR
;; (alphanumericp #\tab)										; => NIL
;; (type-of #\space)												; => STANDARD-CHAR
;; (standard-char-p #\Tab)									; => NIL
;; tab,space,newline共にextended-charではない。
;; かつ、alphanumericpではnilになる。
;;(subseq "abcdeあい　うえお" 7)						; => "　うえお"
;; これでわかる通り、subseqはabcはもちろんあいうもそれぞれ１文字としてカウントしている。
;; 問題なのはsequenceのlengthをとった時の大きさが実際の文字数とは違うという事。
;; これは3byte文字である日本語
