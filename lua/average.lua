#!/usr/bin/env lua

-- Average or arithmetic mean
-- Andrew Cashner, 2023/11/07

function mean(t)
   local sum = 0
   local count = 0
   
   for k,v in pairs(t) do
       n = tonumber(v)
       if n then
           sum = sum + n
           count = count + 1
       end
   end
  
   return (sum / count)
end

print(mean(arg))
