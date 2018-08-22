
(make-music
  'SequentialMusic
  'elements
  (list (make-music
          'KeyChangeEvent
          'pitch-alist
          (list (cons 0 0)
                (cons 1 0)
                (cons 2 0)
                (cons 3 0)
                (cons 4 0)
                (cons 5 0)
                (cons 6 0))
          'tonic
          (ly:make-pitch -1 0))
        (make-music
          'TimeSignatureMusic
          'beat-structure
          '()
          'denominator
          4
          'numerator
          4)
        (make-music
          'NoteEvent
          'duration
          (ly:make-duration 0)
          'pitch
          (ly:make-pitch 0 5))))

