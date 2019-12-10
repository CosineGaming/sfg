local prime = 2
local MAX_PRIME = 50000
while prime < MAX_PRIME do
    local is_prime = true
    for fact=2,prime/2 do -- sqrt should go here, in the interest of fairness it does not
        if (prime % fact) == 0 then
            is_prime = false
            break
        end
    end
    if is_prime then
        --print(prime) -- comment out for true perf test
    end
    prime = prime + 1
end

