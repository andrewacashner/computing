;; autoscore.scm -- Andrew A. Cashner, 2015/07/18
;; Developing functions for autogenerating Lilypond code

;; TODO automate indentation
;; Aliases for boolean values for lyrics or function?
;; Add options for figured bass, incipitstaves, instrumentnames, staffgroup,
;; choirstaff, etc.			 
;; Read values from input file and/or set this up as part of script chain

(define (displayln str)
  "Write a given string plus a newline."
  (begin (display str) (newline)))

(define (make-ly-voice-staff voice)
  "Return a string with Lilypond new Staff command with given Voice"
  (let ([voicename (car voice)] [lyrics-bool (cdr voice)])
    (string-concatenate
     (list "    \\new Staff\n"
           "       <<\n"
           "        \\new Voice = \"" 
           voicename "\" { \\Music" voicename " }\n"
	   (if (eq? #t lyrics-bool)
	       (string-concatenate
		(list "        \\new Lyrics \\lyricsto \"" 
                      voicename "\" { \\Lyrics" voicename " }\n"))
	       "")
	   "       >>\n"))))

(define (make-ly-score voicelist)
  "Return a string with a complete Lilypond score command for given voices"
  (string-concatenate
   (list 
     "\\version \"2.19\"\n\n"
     "\\include \"music.ly\"\n\n"
     "\\score {\n  <<\n"
	 (string-concatenate (map-in-order make-ly-voice-staff voicelist))
	 "  >>\n}\n")))

;; Each voice is stored as a pair where the car is a string for the voice name
;; and the cdr is a boolean value indicating whether or not to include the
;; lyrics command
(define (voice+lyrics name)
  "Do include lyrics"
  (cons name #t))
(define (voice name)
  "Do not include lyrics"
  (cons name #f))

(define (write-file outfile str)
  "Write string to file, creating or overwriting file as needed"
  (let ((outfileport (open-file outfile "w")))
    (display str outfileport)
    (close-output-port outfileport)))

(define (write-score outfile voices)
  "Append score command to file"
  (write-file outfile (make-ly-score voices)))


