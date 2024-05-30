
pitch + interval = pitch
pitch - pitch = interval

Patterns:
    Pitch: /[a-zA-Z]{1}[bb|b|#|##].*[0-9].* /
    Interval: /[mMPda]{1}[0-9].* / (1+) [+ 2x, 3x d, a]
        - but Interval a4 vs pitch a4?

Examples: 
a#4 + P8 = a#5 | Pitch(a#4) + Interval(P8)
eb3 + m3 = g3  | Pitch(eb3) + Interval(m3)
gb5 - P5 = eb5 | Pitch(gb5) - Interval(P5)
gb5 - eb5 = P5 | Pitch(gb5) - Pitch(eb5)

== Additions TBD:
|interval| = n (absolute chromatic value)
ToInterval(n) = interval
interval -+ interval = ToInterval(|interval| -+ |interval|)
interval /* interval = ToInterval(|interval| -+ |interval|) = interval

Examples
m2 + m2 = M2 (not d3?)
P8 - m3 = M6
m2 * 2 = |m2 + m2| = M2
P8 / 2 = |P8| / 2 = 6 = d5
|m2| = 1
|M2| = 2
|m2 + m2| = 2




