;;; �����ɤ߾夲����ver.1
(defun yomi (sentence lang)
	(if (eql lang 'en)
			(setf tts "festival --tts")
		(setf tts "ojtalk"))
	(asdf:run-shell-command (concatenate 'string "echo '" sentence "'| " tts))) ; => YOMI

(defun read-book (file)
	(setq lst ())
	(with-open-file (in file :direction :input)
									(do ((char (read-char in nil 'eof)
														 (read-char in nil 'eof)))
											((eql char 'eof))
										(if (standard-char-p char)
												(progn (when (char-equal char #\.)
																 (yomi (reverse lst) 'en)
																 (setq lst ())) ;lst��nil�ǥꥻ�å�
															 (unless (char-equal char #\Newline)
																 (setf lst (cons char lst))))
												(progn (when (char-equal char #\��)
																 (yomi (reverse lst) 'ja)
																 (setq lst ())) ; lst��nil�ǥꥻ�å�
														 	 (setf lst (cons char lst))))))) ; => READ-BOOK
