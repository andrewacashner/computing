;; autoscore.scm -- Andrew A. Cashner, 2015/07/18
;; Developing functions for autogenerating Lilypond code

;; TODO automate indentation
;; Aliases for boolean values for lyrics or function?
;; Add options for figured bass, incipitstaves, instrumentnames, staffgroup,
;; choirstaff, etc.			 
;; Read values from input file and/or set this up as part of script chain

;;*****************************************************************************

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
  "Return string with backlash + command"
  (string-concatenate
    (list "\\" name " ")))

(define (lycommand-combine part1 part2)
  "Combine two strings to make lycommand string (e.g., 'Music' + 'SI')"
  (lycommand (string-concatenate (list part1 part2))))

(define (enquote text)
  "Return string enclosed in quotation marks"
  (string-concatenate
    (list "\"" text "\"")))

(define (enbrace text)
  "Return string enclosed in curly braces {}"
  (string-concatenate
    (list "{ " text "}")))

(define start-ly-group "<<\n")
(define end-ly-group ">>\n")

;;; Indentation
; (define indent-char #\space)
; (define indent-width 2)
; 
; (define (indent degree)
;   "Return string of space characters * given degrees for indentation,
;   based on value of indent-width and indent-char"
;   (make-string (* indent-width degree) indent-char))

;;**********************

;; Staff (only the staff; the voice, lyrics, figures are defined separately)
;; TODO add optional instrumentname, incipitstaff
(define (new-staff contents)
  "Return string with new staff command including contents"
  (string-concatenate
    (list (lycommand "new") "Staff\n"
          start-ly-group
          contents
          end-ly-group)))

;; Incipit staff
(define (new-incipit voicename longname shortname)
  "Return a string with new incipit command using voicename in definition"
  "including long and short instrument name strings"
  (string-concatenate
    (list
      (lycommand "IncipitStaff") (enquote longname) " " (enquote shortname) " "
      (enbrace (lycommand-combine "Incipit" voicename)) "\n")))


;; Instrument name
(define (new-instrument-name longname shortname)
  "Return a string with new instrument name command using voicename in definition"
  "including long and short instrument name strings"
  (string-concatenate
    (list 
      (lycommand "InstrumentName") (enquote longname) " " (enquote shortname) "\n")))

;; Voice
(define (new-voice name)
  "Return a string with new voice command using voicename in definition"
  (string-concatenate
    (list 
      (lycommand "new") "Voice = " (enquote name) " " 
      (enbrace (lycommand-combine "Music" name)) "\n")))


;; Lyrics
;; TODO add optional alignAboveContext
(define (new-lyrics name)
  "Return a string with new lyrics command using voicename in definition"
  (string-concatenate
    (list 
      (lycommand "new") "Lyrics "
      (lycommand "lyricsto") (enquote name)
      (enbrace (lycommand-combine "Lyrics" name)) "\n")))

;; Figures
(define (new-figures name)
  "Return a string with new FiguredBass command using voicename in definition"
  (string-concatenate
    (list 
      (lycommand "new") "FiguredBass "
      (enbrace (lycommand-combine "Figures" name)) "\n")))

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

(define voicelist (list '()))

(define (add-voice codename longname shortname components) 
  (let ([namestrings (cons longname shortname)] 
        [component-ls (string->list components)])
    (let ([component-bools 
            (list
                (if (member #\l component-ls) #t #f)
                (if (member #\f component-ls) #t #f) 
                (if (member #\i component-ls) #t #f))]) 
      (list codename namestrings component-bools))))

(define (get-codename voice) 
  (car voice))

(define (get-namestrings voice) 
  (cadr voice))
(define (get-longname voice) 
  (car (get-namestrings voice)))
(define (get-shortname voice)
  (cdr (get-namestrings voice)))

(define (get-components voice)
  (caddr voice))
(define (get-lyrics-bool voice)
  (car (get-components voice)))
(define (get-figures-bool voice)
  (cadr (get-components voice)))
(define (get-incipit-bool voice)
  (caddr (get-components voice)))

;; Create whole staff command and specify contents
;; The 'voice' is in the form '(name lyrics-bool . figures-bool) and is created
;; using the procedures 'voice', 'voice+lyrics', etc.
(define (make-ly-voice-staff voice)
  "Create a staff including voice plus other components if requested"
  (let 
    ([voicename (get-codename voice)])
     (new-staff 
       (string-concatenate 
         [list 
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
              "")]))))

;;****************************************************
;; CREATE WHOLE SCORE

(define autoscore-header-comment
  "%% File automatically generated by autoscore.scm\n")
(define ly-current-version "2.19")
(define ly-include 
  (list 
    "incipit-staves.ly" 
    "music.ly")) ; TODO make a list that can be modified

(define (add-include-names include-ls) 
  (string-concatenate 
    (list 
      (if (null? include-ls) 
        ""
        [string-concatenate 
          (list 
            (lycommand "include") (enquote (car include-ls)) "\n" 
            (add-include-names (cdr include-ls)))]))))

(define (make-ly-score voicelist)
  "Return a string with a complete Lilypond score command for given voices"
  (string-concatenate
   (list
     autoscore-header-comment "\n"
     (lycommand "version") (enquote ly-current-version) "\n"
     (add-include-names ly-include) "\n"
     (lycommand "score") "{\n"
     start-ly-group
     (string-concatenate (map-in-order make-ly-voice-staff voicelist)) 
     end-ly-group
     "}\n")))


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


