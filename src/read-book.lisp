(defpackage #:read-book
  (:use :cl :ppcre :cl-interpol :cl-mecab :cl-rainbow)
  (:shadowing-import-from :dex
			  :get)
  (:export :read-book))			; => #<PACKAGE "READ-BOOK">
(in-package #:read-book)		; => #<PACKAGE "READ-BOOK">

(defun system (control-string &rest args)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *verbose-out*.  Returns the shell's exit code."
  (let ((command (apply #'format nil control-string args)))
    (format t "; $ ~A~%" command)
    #+sbcl
    ;; (sb-impl::process-exit-code
     (sb-ext:run-program
      "/usr/bin/bash"
      (list  "-c" command)
       :input nil :output nil)
      ;; :input nil :output *standard-output*)
     ;; )
    #+(or cmu scl)
    (ext:process-exit-code
     (ext:run-program
      "/bin/sh"
      (list  "-c" command)
      :input nil :output *verbose-out*))
    #+clisp             ;XXX not exactly *verbose-out*, I know
    (ext:run-shell-command  command :output :terminal :wait t))) ; => SYSTEM

(named-readtables:in-readtable :interpol-syntax) ; => #<NAMED-READTABLE :INTERPOL-SYNTAX {1004461F33}>
;; 色を付けたい時t,付けたくない時nil
(setf cl-rainbow:*enabled* t)		; => T

;; 色をつけない設定にしてから$ read-book.lisp > outfile
;;とすると’、翻訳した内容が、outfileの中に書き込まれる。

;; (print (cl-rainbow:color :red "red string"))
;; (print (cl-rainbow:color #x5599ff "rgb color code"))
;; (loop for c across "rainbow" do (format t "~A" (cl-rainbow:color (random #xffffff) c)))
;;:black :red :green :yellow :blue :magenta :cyan :white :default

;;  (require 'asdf)									; => NIL
;; (require 'cl-rainbow)							; =>

;;(ql::recursively-install "cl-rainbow")	; =>
;;(user-homedir-pathname)									; => #P"/home/hiro/"
;; 大域変数の設定

(defparameter *edic* (make-hash-table :test #'equalp)
  "辞書用データの格納領域の定義　大文字小文字区別なし") ; => *EDIC*

;; (defparameter *edic* (make-hash-table :test #'equal)		; => #<HASH-TABLE :TEST EQUAL :COUNT 0 {CBC9EF1}>
;; 	"辞書用データの格納領域の定義　大文字小文字区別あり") ; => *EDIC*

;; keyの事も考えないといけないから絶対変更不可エラーの元[2017-06-20 07:54:26]
(defparameter *edic-cnt* (make-hash-table :test #'equalp)
  "辞書登録単語の出現回数をセット")	; => *EDIC-CNT*

(defparameter *last-char* (make-hash-table :test #'equal)
  "最後に読み込んだ文字の位置をセット")	; => *LAST-CHAR*

(defparameter *limit* 10 "単語の意味を表示する回数") ; => *LIMIT*

(defparameter *repeat-read-time* 1 "例文読み上げの繰り返し回数") ; => *REPEAT-READ-TIME*

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
	(print ht out)))))		; => SAVE-DB

;; 汎用化成功！[2017-07-06 12:20:58]
(defun load-db (file ht)
  "ロード元のファイルを読み込み登録用の辞書テーブルに展開する"
  (let ((infile (make-pathname
		 :directory "/home/hiro/howm/junk/"
		 :name (string-downcase file)
		 :type "db")))
    (with-open-file (in infile :if-does-not-exist nil)
      (with-standard-io-syntax
	(setf ht (read in))))))		; => LOAD-DB

(setf *edic* (load-db 'edic *edic*))	; => #<HASH-TABLE :TEST EQUALP :COUNT 7939 {10056B8023}>
(setf *edic-cnt* (load-db 'edic-cnt *edic-cnt*)) ; => #<HASH-TABLE :TEST EQUALP :COUNT 7708 {1005788023}>
(setf *last-char* (load-db 'last-char *last-char*)) ; => #<HASH-TABLE :TEST EQUAL :COUNT 66 {100577CB43}>


;; ppcreバージョン[2017-06-13 04:30:37]
(defun web-scraping (key)
  "webサイトの特定の部位（ここでは単語の意味）を抽出"
  (let* ((uri "http://ejje.weblio.jp/content/")
	 (article (dex:get (concatenate 'string uri key))))
    (ppcre:register-groups-bind (td)
				(#?'(?is)<td class=\"content-explanation ej\">(.*?)</td>' article)
				(ppcre:split "\\s" td)))) ; => WEB-SCRAPING


(defun word-unit-process (key)
  "単語単位処理"
  (let ((val nil)
	(cnt 0))
    ;;(print key)
    (unless (string-equal (string-trim #(#\Space) key) "")
      ;;(setf (gethash key *edic-cnt*) (1+ (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
      (setq cnt (incf (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
      (or (setf val (gethash key *edic*));ハッシュから単語の意味を引っ張ってくるか、
	  (setf val (web-scraping key)))	;webスクレイピングする。
      (setf (gethash key *edic*) val);*edic*に意味を登録
      (when (<= cnt *limit*)					 ;意味を表示する回数を設定
					;(jep (list key (car val)))
	(princ (list (cl-rainbow:color :cyan key)
		     (cl-rainbow:color :magenta (car val))
		     (cl-rainbow:color :blue cnt)))
	(fresh-line)
	(ignore-errors (uiop:run-program (concatenate 'string #?{echo \'} key #?{\' | } #?{festival --tts}))
				(uiop:run-program (concatenate 'string #?{echo \'} (car val) #?{\' | } #?{ojtalk})))
					;(jep (list key (car val)))

	(fresh-line);ここの入れると各単語に空白行が追加されるので、縦に間延び
	;;して見難くなったり、表示しきれなくなる場合が出るので却下[2017-07-13 17:59:45]
	))))				; => WORD-UNIT-PROCESS


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
    (setq lst-w (ppcre:split "[ ,.?!;:=_、。\\\"\\n\\*\\--]" lst-w));単語単位の分割リスト
    ;;(print lst-w)
    (mapcar #'word-unit-process lst-w)
    (fresh-line) ;最後の単語だけ読み上げ時に表示されなかったトラブル対処。消すな！
    ;;[2017-07-13 18:00:20]
    ;;(terpri)
    (ignore-errors
      (dotimes (x *repeat-read-time*)
	(uiop:run-program
	 (concatenate 'string "echo \"" lst "\" | " tts)
	 )))))				; => JEP


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
	((eql char nil) cnt))))		; => TOTAL-CHAR-CNT

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
	      (setq lst (string-trim '(#\Space #\tab #\Newline)(coerce (nreverse lst) 'string))) ;表記されたまま一文単位で表示
	      (format t	"~a " (cl-rainbow:color :yellow lst))
	      (write-string (cl-rainbow:color :red "["))
	      (format t "~:d/~:d" (+ char-cnt index) total-cnt)
	      (write-string (cl-rainbow:color :red " ==> "))
	      (format t "~d\%" (round (float (/ (* (+ char-cnt index) 100) total-cnt))))
	      (write-string (cl-rainbow:color :red "]"))
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
	    (push char lst)))))))	; => READ-BOOK


;; (defun main (&rest argv)
;; (declare (ignorable argv))
;; (let ((file-name argv))
;;   (if file-name
;;       (read-book file-name)
;;       (format t "You did not supply a file name")
;;     )))					; => MAIN
