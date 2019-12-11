local prime = 2
local MAX_PRIME = 50000
while prime < MAX_PRIME do
    local fact = 2
    local is_prime = true
    local running = true -- obviously break is better, but fairness
    while fact<prime/2 and running do -- sqrt should go here, in the interest of fairness it does not
        if (prime % fact) == 0 then
            is_prime = false
            running = false
        end
        fact = fact + 1
    end
    if is_prime then
        --print(prime) -- comment out for true perf test
    end
    prime = prime + 1
end

