\ Morse code and musci notes using sox in Linux system
\ 2022/11/03
: beep.base     s" play -n synth " ;
: dur.short     s" 0.1" ;
: dur.mid       s" 0.2" ;
: dur.long      s" 0.4" ;
: beep.short    beep.base dur.short s+ system ;
: beep.long     beep.base dur.long s+ system ;

: gap.base      s" sleep " ;
: gap.short     gap.base dur.short s+ system ;
: gap.mid       gap.base dur.mid s+ system ;
: gap.long      gap.base dur.long s+ system ;
: gap.tone      gap.short ;
: gap.char      gap.mid ;
: gap.word      gap.long ;

: |             gap.char ;
: _             gap.word ;
: dot           beep.short gap.tone ;
: dash          beep.long gap.tone ;

: S dot dot dot | ;
: O dash dash dash | ;

: tone      beep.base ;
: 8         tone dur.short s+ ;
: 4         tone dur.mid s+ ;
: 2         tone dur.long s+ ;

: wave      s"  sine " ;
: a'        wave s" 440" s+ ;
: b'        wave s" 494" s+ ;
: cis''     wave s" 554" s+ ;
: d''       wave s" 587" s+ ;
: e''       wave s" 659" s+ ;
: fis''     wave s" 740" s+ ;
: gis''     wave s" 831" s+ ;
: a''       wave s" 880" s+ ;

: , s+ system ;

S O S _ S O S _ S O S

\ Oh when the saints go marching in
8 a' , 8 cis'' , 8 d'' , 2 e'' , | |
8 a' , 8 cis'' , 8 d'' , 2 e'' , | |
8 a' , 8 cis'' , 8 d'' , 2 e'' , 2 cis'' , 2 a' , 2 cis'' , 2 b' , | | 

