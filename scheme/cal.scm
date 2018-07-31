(schedule
  '((date-range 2018-08-31 2018-12-15) 
    (weekdays "TR") 
    (recur 'week) 
    (exclude 2018-09-02 (date-range 2018-11-24 2018-11-31)))

; use goops to create day data structure with isodate, weekday, note

; date-range makes list of all day structures within range
; use weekdays as test to determine which days to include
; likewise with recurrence
; exclude removes from list
; result is list, e.g., T 2018-08-31 R 2018-09-02 ...

