#!/usr/bin/sbcl

(locally (declare (optimize (speed 3) (debug 3) (safety 0))))

(load "/home/hiro/howm/junk/utils.lisp") ; => T
(load "/home/hiro/howm/lisp_dir/svg.lisp") ; => T

(defparameter *edic* (make-hash-table :test #'equalp)
	"辞書用データの格納領域の定義")				; => *EDIC*

(defparameter *kv-alst* nil
	"センテンスにおけるkeyとvalueの一時格納域の定義") ; => *KV-ALST*

;; (defun set-htm (lst)
;; 	(let ((k (caar lst))
;; 				(v (cdar lst)))
;; 		(unless (null lst)
;; 				(progn
;; 					(span k v)
;; 					(princ "&nbsp;")
;; 					(set-htm (cdr lst))))))						; => SET-HTM
(defun set-htm (lst)
	(let ((k (caar lst))
				(v (cdar lst))
				(ls nil))
		(if (null lst)
				(progn
					(p ls)
					(setq ls nil))
				(progn
					(push (span k v) ls)
					(push (princ "&nbsp;") ls)
					(set-htm (cdr lst))))))				; => SET-HTM

(defun write-html (file)
	(let* ((pn (pathname file))
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
				 (p
					 (set-htm (reverse *kv-alst*)))))))) ; => WRITE-HTML

(defun make-kv-cell (lst)
	(let ((lst (split " " lst))
				(val nil)
				(mrk '(#\, #\. #\? #\! #\; #\:)))
		(dolist (key lst)
			(dolist (m mrk)
				(setq key (delete m key)))
			(setq key (string-trim '(#\Space #\newline #\tab) (coerce key 'string)))
			(setq val (gethash key *edic*))
			(push (cons key val) *kv-alst*)))) ; => MAKE-KV-CELL

(defun set-tts (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせたttsをセットし読み上げる。"
	(let ((tts nil))
		(unless (null lst)
			(if (standard-char-p (car lst))
					(progn
						(setq tts "festival --tts")
						(make-kv-cell lst))
					(progn
						(setq tts "ojtalk")
						(push (list (coerce lst 'string)) *kv-alst*)))
			(setq lst (delete #\Newline lst))
			(format t "~A~&" lst)
			(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts))))) ; => SET-TTS

(defun read-book (file)
	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
	(let ((lst nil))
		(with-open-file (in file :direction :input)
			(do ((char (read-char in nil 'eof)
								 (read-char in nil 'eof)))
					((eql char 'eof))
				(when (or (and (not (null lst))
											 (not (eq (type-of char) (type-of (car lst)))))
									(eq (car lst) #\。)
									(eq (car lst) #\.)
									(eq (car lst) #\！)
									(eq (car lst) #\!)
									(eq (car lst) #\？)
									(eq (car lst) #\?)
									(eq (car lst) #\:)
									(eq (car lst) #\;)
									)
				(set-tts (nreverse lst))
				(setq lst nil))
				(setq lst (cons char lst))))
					(write-html file)))	; => READ-BOOK

(defun make-dic (file ht)
	"ハッシュテーブルに辞書データを登録する関数"
	(with-open-file (str file :direction :input :if-does-not-exist nil)
									(do ((line (read-line str)
														 (read-line str nil 'eof)))
											((eq line 'eof))
										(let* ((s-line (split "	 " line))
													 (key (string-right-trim '(#\tab) (car s-line)))
													 (val (cdr s-line)))
											(setf (gethash key ht) val))))) ; => MAKE-DIC

(make-dic "/home/hiro/howm/ejdic-hand-utf8.txt" *edic*) ; => NIL

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

(main)
