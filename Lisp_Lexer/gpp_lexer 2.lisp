; *********************************************
; *  341 Programming Languages                *
; *  Fall 2020                                *
; *  Author: Elif Akgun                       *
; *********************************************

;if user enters file name it reads file line by line then sends the line to the lexer 
;if user does not enter file name it reads console then sends input to the lexer 
(defun gppinterpreter(&optional file)
	(setq flag 0)

    (with-open-file (stream "parsed_lisp.txt" :direction :output))

    (when (equal file nil) ;terminalden okuyacaksa
	    (setq line "")

	    (loop while (and (string-not-equal line "(exit)") (= flag 0)) do ;reads until a incorrect statement
	    	(format t ">")
	    	(setq line (read-line))

	    	(if (/= 0 (length line))
	    		(setf flag (lexer line "parsed_lisp.txt"))
	    		(return-from gppinterpreter))
	    )	
	    (return-from gppinterpreter)    	
    )

    ;dosya okuyacaksa
	(let ((in (open file)))
		(when in
			(loop for line = (read-line in nil)
				while line do
				(if (= flag 0) ;reads until a incorrect statement
					(setf flag (lexer line "parsed_lisp.txt"))
					(return-from gppinterpreter)
				)
			)
			(close in)		
		)
	)    
)

;chechs if item is member of list
(defun is-member(list_ item)
	(loop for i from 0 to (- (length list_) 1) do
		(if (string-equal item (nth i list_))
			(return-from is-member T))
	)
	(return-from is-member NIL)
)

(defun lexer(line fileout)
	(setq letter '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))
	(setq number '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

	(setq len (length line))
	(setq first_ 0)
	(setq last_ 1)
	(setq flag 0)

	(loop while (/= first_ len) do
		(setq chr (subseq line first_ last_)) ; current char
		(setq unknown 0)

		;;;;;;;;;;;;;;;;;;;;; OPERATOR CHECKING ;;;;;;;;;;;;;;;;;;;;;
		(when (string-equal chr "+")
			;(format t "OP_PLUS~%")
			(write-to-file fileout "OP_PLUS~%")
			(setf unknown 1)
		)
		(when (string-equal chr "-")
			;(format t "OP_MINUS~%")
			(write-to-file fileout "OP_MINUS~%")
			(setf unknown 1)
		)
		(when (string-equal chr "/")
			;(format t "OP_DIV~%")
			(write-to-file fileout "OP_DIV~%")
			(setf unknown 1)
		)
		(when (string-equal chr "*")
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))

			(if (/= first_ len)
				(progn
					(setq chr (subseq line first_ last_))
					(if (string-equal chr "*")
						;(format t "OP_DBLMULT~%")
						(write-to-file fileout "OP_DBLMULT~%")
						(progn
							(setq first_ (- first_ 1))
							(setq last_ (- last_ 1))
							(setq chr (subseq line first_ last_))
							;(format t "OP_MULT~%")
							(write-to-file fileout "OP_MULT~%")
						)
					)
				)
				(progn
					(setq first_ (- first_ 1))
					(setq last_ (- last_ 1))
					(setq chr (subseq line first_ last_))						
					;(format t "OP_MULT~%") ;else
					(write-to-file fileout "OP_MULT~%")
				)
			)
			(setf unknown 1)
		)
		(when (string-equal chr "(")
			;(format t "OP_OP~%")
			(write-to-file fileout "OP_OP~%")
			(setf unknown 1)
		)	
		(when (string-equal chr ")")
			;(format t "OP_CP~%")
			(write-to-file fileout "OP_CP~%")
			(setf unknown 1)
		)	
		(when (string-equal chr "\"")
			(if (= flag 0)
				(progn
					;(format t "OP_OC~%")
					(write-to-file fileout "OP_OC~%")
					(setq flag 1)
				)
				(progn
					;(format t "OP_CC~%")
					(write-to-file fileout "OP_CC~%")
					(setq flag 0)
				)					
			)
			(setf unknown 1)
		)	
		(when (string-equal chr ",")
			;(format t "OP_COMMA~%")
			(write-to-file fileout "OP_COMMA~%")
			(setf unknown 1)
		)	

		;;;;;;;;;;;;;;;;;;;;; VALUE CHECKING ;;;;;;;;;;;;;;;;;;;;;	
		(when (is-member number chr)
			(when (string-not-equal chr "0")	
				(loop while (and (/= first_ len) (is-member number chr)) do
					(setq first_ (+ 1 first_))
					(setq last_ (+ 1 last_))	
					(if (/= first_ len)
						(setq chr (subseq line first_ last_)))				
				)									
				(setq first_ (- first_ 1))
				(setq last_ (- last_ 1))
				(setq chr (subseq line first_ last_))
			)
			;(format t "VALUE~%")
			(write-to-file fileout "VALUE~%")
			(setf unknown 1)			
		)	

		;;;;;;;;;;;;;;;;;;;;; COMMENT CHECKING ;;;;;;;;;;;;;;;;;;;;;
		(when (string-equal chr ";")
			(setq first_ (+ 1 first_))
			(setq last_ (+ 1 last_))

			(if (/= first_ len)
				(progn			
					(setq chr (subseq line first_ last_))
					(if (string-equal chr ";")
						(progn
							;(format t "COMMENT~%")
							(write-to-file fileout "COMMENT~%")
							(return-from lexer 0)
						)
						(progn
							(setq first_ (- first_ 1))
							(setq last_ (- last_ 1))
							(setq chr (subseq line first_ last_))
						)
					)
				)
				(progn
					(setq first_ (- first_ 1))
					(setq last_ (- last_ 1))
					(setq chr (subseq line first_ last_))						
				)
			)
		)

		;;;;;;;;;;;;;;;;;;;;; IDENTIFIER and KEYWORD CHECKING ;;;;;;;;;;;;;;;;;;;;;
		(when (is-member letter chr)	
			(setq str "")

			(loop while(and (/= first_ len) (or (is-member number chr) (is-member letter chr))) do		
				(setq str (concatenate 'string str chr))		
				(setq first_ (+ 1 first_))
				(setq last_ (+ 1 last_))

				(if (/= first_ len)
					(setq chr (subseq line first_ last_)))
			)
			(setq first_ (- first_ 1))
			(setq last_ (- last_ 1))	
			(setq chr (subseq line first_ last_))

			(when (not (is-keyword str fileout)) ;checks if it is a keyword
				;(format t "IDENTIFIER~%")
				(write-to-file fileout "IDENTIFIER~%")
			)

			(setf unknown 1)
		)

		;ignore white spaces
		(if (or (string-equal chr (string #\Space)) (string-equal chr (string #\Tab)) (string-equal chr (string #\Newline)))
			(setf unknown 1))

		(when (= 0 unknown)
			;(format t "SYNTAX_ERROR ~A cannot be tokenized~%" chr)
			(write-to-file fileout "SYNTAX_ERROR ")
			(write-to-file fileout chr)
			(write-to-file fileout " cannot be tokenized~%")
			;;(return-from lexer 1)
		)

		(setq first_ (+ 1 first_))
		(setq last_ (+ 1 last_))
	)

	0
)

;checks if str is a keyword
(defun is-keyword(str fileout)
	(when (string-equal str "and")
		;(format t "KW_AND~%")
		(write-to-file fileout "KW_AND~%")
		(return-from is-keyword T))
	(when (string-equal str "or")
		;(format t "KW_OR~%")
		(write-to-file fileout "KW_OR~%")
		(return-from is-keyword T))
	(when (string-equal str "not")
		;(format t "KW_NOT~%")
		(write-to-file fileout "KW_NOT~%")
		(return-from is-keyword T))	
	(when (string-equal str "equal")
		;(format t "KW_EQUAL~%")
		(write-to-file fileout "KW_EQUAL~%")
		(return-from is-keyword T))	
	(when (string-equal str "less")
		;(format t "KW_LESS~%")
		(write-to-file fileout "KW_LESS~%")
		(return-from is-keyword T))	
	(when (string-equal str "nil")
		;(format t "KW_NIL~%")
		(write-to-file fileout "KW_NIL~%")
		(return-from is-keyword T))	
	(when (string-equal str "list")
		;(format t "KW_LIST~%")
		(write-to-file fileout "KW_LIST~%")
		(return-from is-keyword T))	
	(when (string-equal str "append")
		;(format t "KW_APPEND~%")
		(write-to-file fileout "KW_APPEND~%")
		(return-from is-keyword T))	
	(when (string-equal str "concat")
		;(format t "KW_CONCAT~%")
		(write-to-file fileout "KW_CONCAT~%")
		(return-from is-keyword T))	
	(when (string-equal str "set")
		;(format t "KW_SET~%")
		(write-to-file fileout "KW_SET~%")
		(return-from is-keyword T))																		
	(when (string-equal str "deffun")
		;(format t "KW_DEFFUN~%")
		(write-to-file fileout "KW_DEFFUN~%")
		(return-from is-keyword T))	
	(when (string-equal str "for")
		;(format t "KW_FOR~%")
		(write-to-file fileout "KW_FOR~%")
		(return-from is-keyword T))	
	(when (string-equal str "if")
		;(format t "KW_IF~%")
		(write-to-file fileout "KW_IF~%")
		(return-from is-keyword T))	
	(when (string-equal str "exit")
		;(format t "KW_EXIT~%")
		(write-to-file fileout "KW_EXIT~%")
		(return-from is-keyword T))								
	(when (string-equal str "load")
		;(format t "KW_LOAD~%")
		(write-to-file fileout "KW_LOAD~%")
		(return-from is-keyword T))	
	(when (string-equal str "disp")
		;(format t "KW_DISP~%")
		(write-to-file fileout "KW_DISP~%")
		(return-from is-keyword T))	
	(when (string-equal str "true")
		;(format t "KW_TRUE~%")
		(write-to-file fileout "KW_TRUE~%")
		(return-from is-keyword T))						
	(when (string-equal str "false")
		;(format t "KW_FALSE~%")
		(write-to-file fileout "KW_FALSE~%")
		(return-from is-keyword T))
	(return-from is-keyword NIL)	
)

;writes tokens to the file
(defun write-to-file (file token)
	(with-open-file (str file :direction :output
                     		  :if-exists :append
                     		  :if-does-not-exist :create)
  		(format str token)
  		(close str)
  	)
)

(setq line *args*)

(if (= 0 (length line))
	(gppinterpreter)
	(progn
		(if (= 1 (length line))
			(gppinterpreter (car line))
			(print "You entered wrong command line.")
		)		
	)
)
