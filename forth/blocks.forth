( large letter F )
( "Starting Forth", ch. 3, 2022/11/02 )

: star      [char] * emit ;
: stars     0 do star loop ;
: margin    cr 30 spaces ;
: blip      margin star ;
: bar       margin 5 stars ;
: F         bar blip bar blip blip cr ;
