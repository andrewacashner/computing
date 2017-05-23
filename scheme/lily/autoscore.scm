;; autoscore.scm -- Andrew A. Cashner, 2015/07/18
;; Developing functions for autogenerating Lilypond code

;; TODO 
;; automate indentation?
;; one command to add voices rather than making list of (add-voice ...)?
;;  
;; Aliases for boolean values for lyrics or function?
;; Add options for staffgroup, choirstaff, layout
;; Read values from input file and/or set this up as part of script chain
;; Write empty outline of input music.ly and lyrics.ly file with voicenames

;;*****************************************************************************

;; LIST UTILITIES
;; From rosettacode.org
(define (flatten ls) 
  "Flatten list: ((one two) three (four)) => (one two three four)"
  (cond ((null? ls) '())
        ((not (pair? ls)) (list ls))
        (else (append (flatten (car ls))
                      (flatten (cdr ls))))))

;; CREATE VOICES
;; Each voice is stored as a name plus a pair of booleans for including lyrics &
;; figures 
;; To access voice name, car; lyrics, cadr; figures, cddr
;; TODO reimplement with keywords?
(define (voice name)
  "Include only music, not lyrics or figures"
  (cons name (cons #f #f)))

(define (voice+lyrics name)
  "Include music, lyrics but not figures"
  (cons name (cons #t #f)))

(define (voice+figures name)
  "Include music, figures but not lyrics"
  (cons name (cons #f #t)))

(define (voice+lyrics+figures name)
  "Include music, lyrics, and figures"
  (cons name (cons #t #t)))

;; WRITE LILYPOND COMMANDS

;; Create parts of Lilypond commands
(define (lycommand name)
  "Return list of strings with backlash + command" 
  (list "\\" name " "))

(define (lycommand-combine part1 part2)
  "Combine two strings to make lycommand string list (e.g., 'Music' + 'SI')" 
  (lycommand (list part1 part2)))

(define (enquote text)
  "Return list of strings enclosed in quotation marks" 
  (list "\"" text "\""))

(define (enbrace text)
  "Return list of strings enclosed in curly braces {}" 
  (list "{ " text "}"))

(define start-ly-group "<<\n")
(define end-ly-group ">>\n")

;;; Indentation
; (define indent-char #\space)
; (define indent-width 2)
; 
; (define (indent degree)
;   "Return list of strings of space characters * given degrees for indentation,
;   based on value of indent-width and indent-char"
;   (make-string (* indent-width degree) indent-char))

;;**********************

;; Staff (only the staff; the voice, lyrics, figures are defined separately)
;; TODO add optional instrumentname, incipitstaff
(define (new-staff contents)
  "Return list of strings with new staff command including contents" 
  (list (lycommand "new") "Staff\n" start-ly-group contents end-ly-group))

;; Incipit staff
(define (new-incipit voicename longname shortname)
  "Return a string with new incipit command using voicename in definition"
  "including long and short instrument name strings" 
  (list 
    (lycommand "IncipitStaff") (enquote longname) " " (enquote shortname) " " 
    (enbrace (lycommand-combine "Incipit" voicename)) "\n"))


;; Instrument name
(define (new-instrument-name longname shortname)
  "Return a string with new instrument name command using voicename in definition"
  "including long and short instrument name strings" 
  (list (lycommand "InstrumentName") (enquote longname) " " (enquote shortname) "\n"))

;; Voice
(define (new-voice name)
  "Return a string with new voice command using voicename in definition" 
  (list 
    (lycommand "new") "Voice = " (enquote name) " " 
    (enbrace (lycommand-combine "Music" name)) "\n"))


;; Lyrics
;; TODO add optional alignAboveContext
(define (new-lyrics name)
  "Return a string with new lyrics command using voicename in definition" 
  (list 
    (lycommand "new") "Lyrics " (lycommand "lyricsto") (enquote name) 
    (enbrace (lycommand-combine "Lyrics" name)) "\n"))

;; Figures
(define (new-figures name)
  "Return a string with new FiguredBass command using voicename in definition" 
  (list 
    (lycommand "new") "FiguredBass " 
    (enbrace (lycommand-combine "Figures" name)) "\n"))

;;******************************************************************
;; USER COMMAND TO CREATE VOICES
;; Arg 1: Name of voice used for internal commands 
;;        (e.g., "SI", used in "MusicSI" and "LyricsSI")
;; Arg 2: Long name of voice as it should appear in instrumentName
;;        (e.g., "SOPRANO I")
;; Arg 3: Short name of voice (e.g., "S. I")
;; Arg 4: Series of characters specifying components to be included, 
;;        in addition to Voice
;;          - l  Lyrics
;;          - f  FiguredBass
;;          - i  IncipitStaff
;;        These may be in any order, e.g., "vli", "vif"
;; RETURNS single instance of a 'voice' data structures /* TODO ? */
;;*******************************************************************

(define (add-voice codename longname shortname components) 
  (let ([component-ls (string->list components)])
    (let ([component-bools 
            (list
                (if (member #\l component-ls) #t #f)
                (if (member #\f component-ls) #t #f) 
                (if (member #\i component-ls) #t #f))])
      (list codename longname shortname component-bools))))

(define (get-codename voice)        (list-ref voice 0))
(define (get-longname voice)        (list-ref voice 1))
(define (get-shortname voice)       (list-ref voice 2))
(define (get-components voice)      (list-ref voice 3))
(define (get-lyrics-bool voice)     (list-ref (get-components voice) 0))
(define (get-figures-bool voice)    (list-ref (get-components voice) 1))
(define (get-incipit-bool voice)    (list-ref (get-components voice) 2))

;; Create whole staff command and specify contents
;; The 'voice' is in the form '(name lyrics-bool . figures-bool) and is created
;; using the procedures 'voice', 'voice+lyrics', etc.
(define (make-ly-voice-staff voice)
  "Create a staff including voice plus other components if requested"
  (let 
    ([voicename (get-codename voice)])
     (new-staff 
       (list 
            (if (eq? #t (get-incipit-bool voice)) 
              ; If there is incipit staff, we do not need to add instrument
              ; names again
              (new-incipit voicename (get-longname voice) (get-shortname voice))
              (new-instrument-name (get-longname voice) (get-shortname voice))) 

            (new-voice voicename) ; Always include Voice

            (if (eq? #t (get-lyrics-bool voice))
              (new-lyrics voicename)
              "") 

            (if (eq? #t (get-figures-bool voice)) 
              (new-figures voicename)
              "")))))

;;****************************************************
;; CREATE WHOLE SCORE

(define autoscore-header-comment
  "%% File automatically generated by autoscore.scm\n")

(define ly-current-version "2.19")

(define default-includes 
  (list 
    "incipit-staves.ly" 
    "music.ly"))
(define ly-include default-includes)

(define (add-include-files ls)
  "User command to add list of filenames to list of default includes" 
  (set! ly-include (append default-includes ls)))

(define (create-include-names include-ls) 
  "Make a list of include commands for all files in ly-include-files list"
  (if (null? include-ls) 
    '() 
    (list
        (lycommand "include") (enquote (car include-ls)) "\n" 
        (create-include-names (cdr include-ls)))))

(define (make-ly-score voicelist)
  "Return a string with a complete Lilypond score command for given voices"
   (string-concatenate 
     (flatten 
       (list
         autoscore-header-comment "\n" 
         (lycommand "version") (enquote ly-current-version) "\n" 
         (create-include-names ly-include) "\n" 
         (lycommand "score") "{\n" 
         start-ly-group 
         (map-in-order make-ly-voice-staff voicelist) 
         end-ly-group 
         "}\n"))))


;; OUTPUT

;; To standard output
(define (displayln str)
  "Write a given string plus a newline."
  (begin (display str) (newline)))

;; To file
(define (write-file outfile str)
  "Write string to file, creating or overwriting file as needed"
  (let ((outfileport (open-file outfile "w")))
    (display str outfileport)
    (close-output-port outfileport)))

;; ONE COMMAND TO WRITE THE SCORE FILE
;; 'voices' should be defined as a list of names & booleans defined with, e.g.
;; (voice+lyrics arg):
;; For example:
;;   (define voices
;;     (list
;;       (voice+lyrics "Soprano")
;;       (voice+figures "Bass")))

(define (write-score outfile voices)
  "Append score command to file"
  (write-file outfile (make-ly-score voices)))


