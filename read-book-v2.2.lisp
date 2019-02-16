#!/usr/bin/sbcl --noinform

;;; 日英読み上げ装置ver.2
;(typep char 'character)									; => T
;(typep lst 'list)												; => T
;(apropos 'list)

(defun set-tts (lst)
	"リストの文字列が日本語か英語を判定してそれに合わせたttsをセットし読み上げる。"
	(let ((tts nil))
		(unless (null lst)
			(if (standard-char-p (car lst)) ;リストの先頭の文字が英数字ならば
					(setq tts "festival --tts") ;yes　festival 改行は削除しない。dotと読み上げてしまう。
					(progn
						;(setq lst (delete #\Newline lst))
						(setq tts "ojtalk"))) ;no ojtalk
			;(setq lst (delete #\Newline lst)) ;ojtalkの時は改行を削除（止まるので。）
			(format t "~A~%" lst)
			;(asdf:run-shell-command (concatenate 'string "echo \"" lst "\"| " tts))
			(ignore-errors (uiop:run-program (concatenate 'string "echo \"" lst "\" | " tts))) ;。の時にエラーが出る。
			)))					; => NIL

(defun read-book (file)
	"日英各言語毎に文字列をまとめ、set-ttsに渡す。"
	(let ((lst nil))
		(with-open-file (in file :direction :input)
			(do ((char (read-char in nil 'eof)
								 (read-char in nil 'eof)))
					((eql char 'eof))
				(when (eq char #\Newline)
						(setq char #\Space))
				(when (or (and (not (null lst)) ;lstが空ではなく、かつ
											 (not (numberp char))
											 (not (eq (type-of char) (type-of (car lst))))) ;前のchar≠今のchar
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
				(setq lst (cons char lst))))))

;; (numberp #\1)														; => NIL
;; (numberp #\a)														; => NIL
;; (numberp 1)															; => T

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
