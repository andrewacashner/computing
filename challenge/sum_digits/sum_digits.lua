function sum_digits()
    local sum = 0
    for n = 0, 9 do
        sum = sum + 5 * (9 + 2 * n)
    end

    return sum + 1
end

print(sum_digits())
