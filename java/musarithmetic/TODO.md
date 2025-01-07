# Quality.of(String), Accid.of(String)

- Replace list filtering with a static hash table?
(DONE)

# Interval

- Validate quality degree matching (e.g., P5 not M5)
(DONE)

# Pitch.inc(Interval)
    
- Instead of inputing "c#5 m2" or "C#5 -m2" (where negative is operator
  attached to interval), input as expression "c#5 + m2" or "c#5 - m2"; then
  remove the negative handling from Interval and deal with it in inc method
  here
(DONE)

- Cannot do c4 + d15 or c4 -A8
(DONE)
