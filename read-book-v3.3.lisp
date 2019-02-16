#!/usr/bin/sbcl --noinform
																				; -*- mode: Lisp; coding:utf-8 -*-
;; 更新情報
;; しおり機能
;; save-dbの汎用化

'(:program-name "read-book.lisp"
	:version "3.3"
	:copyright 2017 :author "Higashi, Hiromitsu"
	:license "MIT")
																				; => (:PROGRAM-NAME "read-book.lisp" :VERSION "3.3" :COPYRIGHT 2017 :AUTHOR "Higashi, Hiromitsu" :LICENSE "MIT")

;;; LOAD

(load "/home/hiro/howm/junk/utils.lisp"
			:external-format :utf-8)					; => T
(load "~/quicklisp/dists/quicklisp/software/cl-mecab/cl-mecab.asd"
			:external-format :utf-8)					; => T

(ql:quickload	'(:cl-ppcre :dexador :cl-mecab) :silent t) ; => (:CL-PPCRE :DEXADOR :CL-MECAB)
																				;
;; 大域変数の設定

(defparameter *edic* (make-hash-table :test #'equalp)
	"辞書用データの格納領域の定義　大文字小文字区別なし") ; => *EDIC*

(defparameter *edic-cnt* (make-hash-table :test #'equalp)
	"辞書登録単語の出現回数をセット")			; => *EDIC-CNT*

(defparameter *last-char* (make-hash-table :test #'equal)
	"最後に読み込んだ文字の位置をセット")	; => *LAST-CHAR*

(defparameter *limit* 10 "単語の意味を表示する回数") ; => *LIMIT*

(defparameter *repeat-read-time* 1 "例文読み上げの繰り返し回数")

;;; 関数定義
(defun save-db (ht ofile)
	"ハッシュテーブルの保存"
	(let ((outfile (make-pathname
									:directory '(:absolute "home" "hiro" "howm" "junk")
									:name (string-downcase ofile);[2017-07-05 21:51:17]修正
									:type "db")))
		(with-open-file (out outfile
												 :direction :output
												 :if-exists :supersede
												 :external-format :utf-8)
			(with-standard-io-syntax
				(print ht out)))))							; => SAVE-DB

(defun load-edic (file)
	"ハッシュテーブルの読み込み"
	(with-open-file (in file
											:if-does-not-exist nil
											:external-format :utf-8)
		(with-standard-io-syntax
			(setf *edic* (read in)))))				; => LOAD-EDIC

(defun load-edic-cnt (file)
	"ハッシュテーブルの読み込み"
	(with-open-file (in file
											:if-does-not-exist nil
											:external-format :utf-8)
		(with-standard-io-syntax
			(setf *edic-cnt* (read in)))))		; => LOAD-EDIC-CNT

(defun load-last-char (file)
	"ハッシュテーブルの読み込み"
	(with-open-file (in file
											:if-does-not-exist nil
											:external-format :utf-8)
		(with-standard-io-syntax
			(setf *last-char* (read in)))))		; => LOAD-LAST-CHAR

(load-edic "~/howm/junk/edic.db")				; => #<HASH-TABLE :TEST EQUALP :COUNT 3697 {DB91A11}>
(load-edic-cnt "~/howm/junk/edic-cnt.db") ; => #<HASH-TABLE :TEST EQUALP :COUNT 3483 {E108119}>
(load-last-char "~/howm/junk/last-char.db") ; =>

(defun web-scraping (key)
	"webサイトの特定の部位（ここでは単語の意味）を抽出"
	(let* ((uri "http://ejje.weblio.jp/content/")
				 (article (dex:get (concatenate 'string uri key))))
		(ppcre:register-groups-bind (td)
																("(?is)<td class=content-explanation>(.*?)</td>"
																 article)
																(ppcre:split "\\s" td)))) ; => WEB-SCRAPING

(defun word-unit-process (key)
	"単語単位処理"
	(let ((val nil)
				(cnt 0))
		(unless (string-equal key "")
			(setq cnt (incf (gethash key *edic-cnt* 0))) ;*edic-cnt*にカウント数登録
			(or (setf val (gethash key *edic*));ハッシュから単語の意味を引っ張ってくるか、
					(setf val (web-scraping key)))	;webスクレイピングする。
			(setf (gethash key *edic*) val);*edic*に意味を登録
			(when (<= cnt *limit*)					 ;意味を表示する回数を設定
				(print (list key val cnt)))))) ; => WORD-UNIT-PROCESS

(defun jep (lst)
	"文字列が日本語か英語を判定してそれに合わせた処理をする。"
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
						(setq tts "ojtalk"))))	;no ojtalk
		(setq lst-w (ppcre:split "[ ,.?!;:=_、。\\n]"	lst-w));単語単位の分割リスト
		(mapcar #'word-unit-process lst-w)
		(sleep 1)
		(ignore-errors
			(dotimes (x *repeat-read-time*)
										 (uiop:run-program
											(concatenate 'string "echo \"" lst "\" | " tts)) )))) ; =>


(defun stamp-write-date (file)
	"ファイルの最終書き込み日時の表示"
	(multiple-value-bind
				(sec min hr day mon yr)
			(decode-universal-time (file-write-date file))
		(format nil "~d-~2,'0d-~2,'0d\ ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec))) ; => STAMP-WRITE-DATE

(defvar hash-cnt)												; => HASH-CNT

(defun bookmark (file ofile)
	"しおり機能：前回読んだ位置を記録"
	(with-open-file (in file
											:direction :input
											:element-type '(unsigned-byte 8)
											:external-format :utf-8)
		(let ((buf (make-sequence '(vector (unsigned-byte 8)) (file-length in))))
			(read-sequence buf in)
			(setq hash-cnt
						(lambda () (gethash (concatenate 'string file (stamp-write-date file)) *last-char* 0)))
			(with-open-file (out ofile
													 :direction :output
													 :if-exists	:supersede
													 :if-does-not-exist :create)
				(write-string
				 (subseq
					(sb-ext:octets-to-string buf :external-format :utf-8)
					(funcall hash-cnt)) out)))))	; => BOOKMARK

(defun read-book (file)
 	"日英各言語毎に文字列を日英各文にまとめる"
	(let* ((lst nil)
				 (char-cnt 0)
				(pn (pathname file))
				(ofile (make-pathname
								:directory (pathname-directory pn)
								:name (pathname-name pn)
								:type "out")))
		(if (progn (uiop:run-program "echo \"前回しおりをはさんだところから読みますか？\" | ojtalk")
							 (y-or-n-p "前回しおりをはさんだ所から読みますか？"))
				(progn
					(princ "では、しおりの箇所から読みます")
					(uiop:run-program "echo \"では、しおりの箇所から読みます。\" | ojtalk"))
				(progn
					(princ "では、本の最初から読み直します。")
					(uiop:run-program "echo \"では、本の最初から読み直します。\" | ojtalk")
					(setf (gethash (concatenate 'string file (stamp-write-date file)) *last-char*) 0)))
		(bookmark file ofile)
		(with-open-file (in ofile
												:direction :input
												:external-format :utf-8)
 			(do ((char (read-char in nil 'eof)
 								 (read-char in nil 'eof)))
 					((eql char 'eof))
				(incf char-cnt)
				(when (or (and (not (equal lst ""))
											 (not (eq (type-of char) (type-of (car lst)))))
									(eq (car lst) #\。)
									(eq (car lst) #\.)
									(eq (car lst) #\！)
									(eq (car lst) #\!)
									(eq (car lst) #\？)
									(eq (car lst) #\?)
									)
					(setq lst (string-trim '(#\Space #\tab #\Newline)(coerce (nreverse lst) 'string))) ;表記されたまま一文単位で表示
					(print lst)
					(setf (gethash (concatenate 'string file (stamp-write-date file)) *last-char*) (+ char-cnt (funcall hash-cnt)))
					(save-db *last-char* 'last-char)
					(jep lst); 読み上げ部(さらにその中に翻訳部）
					(fresh-line)
					(setq lst nil)
					(save-db *edic* 'edic)
					(save-db *edic-cnt* 'edic-cnt))
				(push char lst)))))											; => READ-BOOK

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
				;(bookmark file-name)
				)))

(main) ;mainの実行
