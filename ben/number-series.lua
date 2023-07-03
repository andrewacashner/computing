
require "math"
function repeatingDigit(base, digits)
    -- (1, 3)  = 111  = 1 * 10^2  + 1 * 10^1 + 1
    -- (1, -3) = .111 = 1 * 10^-3 + 1 * 10^-2 + 1* 10^-1
    n = 0
    if digits > 0 then
        for pow = 0, digits - 1 do
            n = n + base * 10^pow
        end
    else
        for pow = -1, digits, -1 do
            n = n + base * 10^pow
        end
    end
    return n
end

function mysteryRatio(base, digits)
    n = repeatingDigit(base, digits)
    f = repeatingDigit(base, digits * -1)
    print(n * f)

    for i = 0, digits do
        n = n * f  
        print(n)
    end
    return
end



