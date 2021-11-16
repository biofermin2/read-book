#!/usr/bin/sbcl --noinform
;; 開発ゴール
;; ver3.5
;; cl-rainbow,cl-mecabをとりあえず、廃止。->完了[2017-08-19 16:51:45]
;; 日本語部は読み上げのみの処理にして英語のところだけスクレイピングをかけて意味を調べる処理に変更
;; ->新jepの採用[2017-10-13 20:53:15]これにより日本語読み上げの速度がupした。

;; defmethodを使って日本語英語の処理を分けたい。
;; 出来れば、予めsort処理などを行って、単語は一気に調べあげるようにしたい。
;; 並列処理を使えないか？

;; 英単語の意味が抽出出来なかった（NILになった)場合、カウントもNILのままにするべき。
;; scraping出来なかったら二度と意味がNIL以外とならないため。

;; 文節毎に切りたい。ピリオドないし、句点、あるいは？や！で文章を切る。
;; jepの箇所をlabelsを使って処理するようにする。
;; イメージは以下の通り
;; (labels ((英語処理 (c)
;; 					 (body))
;; 				 (日本語処理 (c)
;; 					 (body)))
;; 	(if (standard-char-p c)
;; 			(英語処理 (c))
;; 			(日本語処理 (c))))


(funcall (lambda ()
					 '(:program-name "read-book.lisp"
						 :version "dev3.5"
						 :copyright 2019 :author "Higashi, Hiromitsu"
						 :license "MIT"
						 )))
; => (:PROGRAM-NAME "read-book.lisp" :VERSION "dev3.5" :COPY;; RIGHT 2019 :AUTHOR "Higashi, Hiromitsu" :LICENSE "MIT")

;;; LOAD

(load "/home/hiro/howm/junk/utils.lisp"
			:external-format :utf-8) ; => T
(qload :cl-ppcre :dexador :cl-interpol)	; => (:CL-PPCRE :DEXADOR :CL-INTERPOL)
(named-readtables:in-readtable :interpol-syntax)		


;;; 大域変数の設定

(defparameter *edic* (make-hash-table :test #'equalp)
	"辞書用データの格納領域の定義　大文字小文字区別なし") ; => *EDIC*

;; keyの事も考えないといけないから絶対変更不可エラーの元[2017-06-20 07:54:26]
(defparameter *edic-cnt* (make-hash-table :test #'equalp)
	"辞書登録単語の出現回数をセット") ; => *EDIC-CNT*

(defparameter *last-char* (make-hash-table :test #'equal)
	"最後に読み込んだ文字の位置をセット") ; => *LAST-CHAR*

(defparameter *limit* 10 "単語の意味を表示する回数") ; => *LIMIT*

(defparameter *repeat-read-time* 1 "例文読み上げの繰り返し回数") ; => *REPEAT-READ-TIME*

;; ディレクトリの設定
(setf dir "/home/hiro/howm/junk/")			; => "/home/hiro/howm/junk/"

(defun save-db (ht ofile)
  "ハッシュテーブルの保存"
  (let ((outfile (make-pathname
		  :directory dir
		  :name (string-downcase ofile);[2017-07-05 21:51:17]修正
		  :type "db")))
    (with-open-file (out outfile
			 :direction :output
			 :if-exists :supersede
			 :external-format :utf-8)
		    (with-standard-io-syntax
		     (print ht out))))) ; => SAVE-DB


(defun load-db (file ht)
  "ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
  (let ((infile (make-pathname
		 :directory dir
		 :name (string-downcase file)
		 :type "db")))
    (with-open-file (in infile :if-does-not-exist nil)
		    (with-standard-io-syntax
		     (setf ht (read in))))))					; => LOAD-DB

(setf *edic* (load-db 'edic *edic*)) ; => #<HASH-TABLE :TEST EQUALP :COUNT 586 {C61D031}>
(setf *edic-cnt* (load-db 'edic-cnt *edic-cnt*)) ; => #<HASH-TABLE :TEST EQUALP :COUNT 586 {C830031}>
(setf *last-char* (load-db 'last-char *last-char*)) ; => #<HASH-TABLE :TEST EQUAL :COUNT 5 {C31D9F9}>

;; シンプルに単機能にする。
(setf uri "http://ejje.weblio.jp/content/") ; => "http://ejje.weblio.jp/content/"

(defun web-scraping (key)
  "webサイトの特定の部位（ここでは単語の意味）を抽出"
  (let* (;(uri "http://ejje.weblio.jp/content/")
	 (article (dex:get (concatenate 'string uri key))))
    (ppcre:register-groups-bind (td)
				(#?'(?is)<td class=\"content-explanation ej\">(.*?)</td>' article)
				(ppcre:split "\\s" td))))

(defun jep (lst)
  "リストの文字列が日本語か英語を判定してそれに合わせた処理を行う。"
  (let ((tts nil)
					;(lst-w nil)
	)
    (unless (equal lst "")
      (if (standard-char-p (car (coerce lst 'list))) ;リストの先頭が英数字ならば
	  ;;(setq lst-w lst) ;日本語処理と合わせるためにコピーを取る。
	  (setq tts "festival --tts");yes　festival 改行は削除しない。
	(setq tts "ojtalk")))	;no ojtalk
    (setq lst (ppcre:split "[ ,.?!;:=_、。\\\"\\n\\*\\--]" lst));単語単位の分割リスト
    (mapcar #'word-unit-process lst)
    (fresh-line) ;最後の単語だけ読み上げ時に表示されなかったトラブル対処。消すな！
    (ignore-errors
      (dotimes (x *repeat-read-time*)
	(uiop:run-program
	 (concatenate 'string "echo \"" lst "\" | " tts)
	 ;;											(concatenate 'string "echo " lst " | " tts)
	 )))))													; => JEP

;; ファイルの最終書き込み日時の表示
(defun stamp-write-date (file)
  (multiple-value-bind
      ;;					(sec min hr day mon yr dow d-p tz)
      (sec min hr day mon yr)
      (decode-universal-time (file-write-date file))
    (format nil "~d-~2,'0d-~2,'0d\ ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec))) ; => STAMP-WRITE-DATE


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
		    (let* ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in)))
			   (total-cnt (total-char-cnt file)))
		      (read-sequence buf in)
		      (with-open-stream	(s (make-string-input-stream
					    (subseq (sb-ext:octets-to-string buf
									     :external-format
									     :utf-8) index)))
					(do ((char (read-char s nil 'eof)
						   (read-char s nil 'eof))
					     (char-cnt 0 (incf char-cnt))) ;最初のカウントは0ではなく1にする。要注意。
					    ((eql char 'eof) (print '読了))
					  (when (or (and (not (equal lst "")) ;lstが空ではなく、かつ
							 (not (eq (type-of char) (type-of (car lst)))))
						    (eq (car lst) #\。)
						    (eq (car lst) #\.)
						    (eq (car lst) #\！)
						    (eq (car lst) #\!)
						    (eq (car lst) #\？)
						    (eq (car lst) #\?)
						    );最初の一文字がここを通過する。
					    (setq lst (string-trim '(#\Space #\tab #\Newline)(coerce (nreverse lst) 'string))) ;表記されたまま一文単位で表示
					    (format t	"~a " lst)
					    (princ "[")
					    (format t "~:d/~:d" (+ char-cnt index) total-cnt)
					    (princ " ==> ")
					    (format t "~d\%" (round (float (/ (* (+ char-cnt index) 100) total-cnt))))
					    (princ "]")
					    (fresh-line)
					    (setf (gethash (concatenate 'string file (stamp-write-date file)) *last-char*) (+ char-cnt index))
					    (save-db *last-char* 'last-char)
					    (jep lst); 読み上げ部(さらにその中に翻訳部）
					    (terpri)
					    (setq lst nil)
					    (save-db *edic* 'edic)
					    (save-db *edic-cnt* 'edic-cnt))
					  (push char lst)))))))											; => READ-BOOK


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

(main) ;mainの実行

