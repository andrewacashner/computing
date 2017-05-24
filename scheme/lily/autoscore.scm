;; autoscore.scm -- Andrew A. Cashner, 2015/07/18
;; Automatically generating Lilypond code from configuration file in pure Scheme

;;*****************************************************************************

;; LIST UTILITIES
;; From rosettacode.org
(define (flatten ls) 
  "Flatten list: ((one two) three (four)) => (one two three four)"
  (cond ((null? ls) '())
        ((not (pair? ls)) (list ls))
        (else (append (flatten (car ls))
                      (flatten (cdr ls))))))

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

;;**********************

;; Staff (only the staff; the voice, lyrics, figures are defined separately)
(define (new-staff contents)
  "Return list of strings with new staff command including contents" 
  (list 
    "\n" (lycommand "new") "Staff\n" start-ly-group contents end-ly-group))

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

;; Staff group name
;; NB current version of \ChoirStaffName only takes one arg
(define (new-staff-group-name type name)
  (list 
    (lycommand-combine type "Name") (enquote name) "\n")) 

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

;; Create whole staff command and specify contents using user-defined 'voice'
;; object with its components specifying whether to include lyrics, figures,
;; incipit
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

;; Helper commands used in add-voice command
(define (get-codename voice)        (list-ref voice 0))
(define (get-longname voice)        (list-ref voice 1))
(define (get-shortname voice)       (list-ref voice 2))
(define (get-components voice)      (list-ref voice 3))
(define (get-lyrics-bool voice)     (list-ref (get-components voice) 0))
(define (get-figures-bool voice)    (list-ref (get-components voice) 1))
(define (get-incipit-bool voice)    (list-ref (get-components voice) 2))

 
;;***********************************
;; USER COMMANDS

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
;; RETURNS single instance of a 'voice' data structure (object)
;;*******************************************************************

(define (add-voice codename longname shortname components) 
  (let ([component-ls (string->list components)])
    (let ([component-bools 
            (list
                (if (member #\l component-ls) #t #f)
                (if (member #\f component-ls) #t #f) 
                (if (member #\i component-ls) #t #f))])
      (list codename longname shortname component-bools))))

(define (add-ungrouped-staves voicelist)
  "Return list for ungrouped staves, processing all the voices"
  "with their components"
  (map-in-order make-ly-voice-staff voicelist))

(define (add-staff-group grouptype codename groupname voicelist)
  "Return list for whole staff group (e.g., ChoirStaff, StaffGroup)"
  "Include name if one is given"
  (list
    "\n" (lycommand "new") grouptype " = " (enquote codename) "\n"
    start-ly-group 
    (if (eq? 0 (string-length groupname))
      "" 
      (new-staff-group-name grouptype groupname))
    (add-ungrouped-staves voicelist) ; Process all the voices
    end-ly-group "\n"))


;;****************************************************
;; CREATE WHOLE SCORE

(define autoscore-header-comment
  "%% File automatically generated by autoscore.scm\n\n")

(define ly-current-version "2.19")

(define default-includes 
  (list 
    "villancico.ly"
    "header.ly"
    "music.ly"
    "lyrics.ly"))

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

(define layout "")

(define (add-layout str)
  (set! layout 
    (list 
      "\n" (lycommand "layout") "{\n" str "\n}\n\n")))

(define (make-ly-score contents)
  "Return a string with a complete Lilypond score command for given voices"
   (string-concatenate 
     (flatten 
       (list
         autoscore-header-comment 
         (lycommand "version") (enquote ly-current-version) "\n\n" 
         (create-include-names ly-include) "\n" 
         (lycommand "score") "{\n" 
         start-ly-group 
         contents
         end-ly-group 
         layout
         "}\n"))))

;;******************************************************************************
;; CREATE EMPTY OUTLINE OF MUSIC INPUT FILE 
;;******************************************************************************

(define (make-ly-input-incipit voice)
  "Make list with contents of empty incipit command" 
  (if (eq? #t (get-incipit-bool voice)) 
    (list "Incipit" (get-codename voice) " = {\n\n}\n\n")
    '()))

(define (make-ly-input-music voice)
  "Make list for empty music command"
  (list
    "Music" (get-codename voice) " = {\n\n}\n\n"))

(define (make-ly-input-lyrics voice)
  "Make list for empty lyrics command"
  (if (eq? #t (get-lyrics-bool voice)) 
    (list "Lyrics" (get-codename voice) " = " 
          (lycommand "lyricmode") "{\n\n}\n\n")
  '()))

(define (make-ly-input-figures voice)
  "Make list for empty figures command"
  (if (eq? #t (get-figures-bool voice)) 
    (list "Figures" (get-codename voice) " = " 
          (lycommand "figuremode") "{\n\n}\n\n")
    '()))

(define (header-line key)
  "Make list with contents of single header key-value line"
  (list key " = " (enquote "") "\n"))

(define header-keys
  (list 
    "title"
    "composer"
    "poet"
    "source"))

; Make list for empty header command
(define make-ly-input-header 
  (list
    autoscore-header-comment
    (lycommand "header") "{\n"
    (map-in-order header-line header-keys)
    "}\n"))

;; Make string out of header list
(define make-ly-input-outline-header
  (string-concatenate (flatten (list make-ly-input-header))))

(define (make-ly-input-outline-music voices)
  "Return a string with the outline of a Lilypond input file for given score"
  "e.g., 'MusicSI = { }\n MusicSII = { }'"
  (string-concatenate
    (flatten 
      (list
        autoscore-header-comment 
        (map-in-order make-ly-input-incipit voices)
        (map-in-order make-ly-input-music   voices)
        (map-in-order make-ly-input-figures voices)))))

(define (make-ly-input-outline-lyrics voices)
  "Return a string with the outline of a Lilypond LYRICS input file"
  (string-concatenate
    (flatten 
      (list
        autoscore-header-comment 
        (map-in-order make-ly-input-lyrics  voices)))))

;;******************************************************************************
;; OUTPUT

;; To standard output
(define (displayln str)
  "Write a given string plus a newline."
  (begin (display str) (newline)))

;; Create or overwrite file
(define (write-file outfile str)
  "Write string to file, creating or overwriting file as needed"
  (let ((outfileport (open-file outfile "w")))
    (display str outfileport)
    (close-output-port outfileport)))

;; Append to file
(define (append-file outfile str)
  "Write string to file, appending if file exists"
  (let ((outfileport (open-file outfile "a")))
    (display str outfileport)
    (close-output-port outfileport)))

;;********************************************
;; USER COMMANDS

;; WRITE THE SCORE FILE
(define (write-score outfile voices)
  "Append score command to file"
  (write-file outfile (make-ly-score voices)))

;; WRITE THE INPUT FILE OUTLINES
(define (write-input-outline-header outfile) 
  "Write outline of HEADER input file only if file does not exist."
  (if (access? outfile F_OK)
    (displayln (list "Input file" outfile "already exists")) 
    (write-file outfile make-ly-input-outline-header)))

(define (write-input-outline-music outfile voices) 
  "Write outline of MUSIC input file only if file does not exist."
  (if (access? outfile F_OK)
    (displayln (list "Input file" outfile "already exists")) 
    (write-file outfile (make-ly-input-outline-music voices))))

(define (write-input-outline-lyrics outfile voices) 
  "Write outline of LYRICS input file only if file does not exist."
  (if (access? outfile F_OK)
    (displayln (list "Input file" outfile "already exists")) 
    (write-file outfile (make-ly-input-outline-lyrics voices))))

;; Default values
(define header-file "header.ly")
(define music-input-file "music.ly")
(define lyrics-input-file "lyrics.ly")
(define score-file "score.ly")

;; ONE COMMAND TO WRITE FILES USING DEFAULTS
(define (make-input-files voices)
  "Create input outlines and score file using default filenames and"
  "user-defined data:"
  "  - 'voices' is a list of 'add-voice' commands"
  (write-input-outline-header header-file)
  (write-input-outline-music  music-input-file  voices)
  (write-input-outline-lyrics lyrics-input-file voices))

(define (make-score-file score)
  "Create input outlines and score file using default filenames and"
  "user-defined data:"
  "  - 'score' is a list of 'add-staff-group' or 'add-ungrouped-staves'"
  "    commands using predefined list(s) of voices"
  (write-score score-file score))

