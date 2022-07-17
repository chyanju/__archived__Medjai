%builtins range_check
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)

func mint{range_check_ptr}(total_supply : Uint256, amount : Uint256):
	alloc_locals
	uint256_check(amount) # ensure amount is valid
	let (new_supply  : Uint256, is_overflow) = uint256_add(total_supply, amount) #ignores overflow bit
        assert is_overflow = 0
	return ()
end	

func main{range_check_ptr}():
	alloc_locals
        let sym1 = symbolic(felt, 'sym1')
	let sym2 = symbolic(felt, 'sym2')
	let sym3 = symbolic(felt, 'sym3')
	let sym4 = symbolic(felt, 'sym4')
	let total_supply = Uint256(1, 2) # symbolic
	let amount = Uint256(340282366920938463463374607431768211455, 340282366920938463463374607431768211453) # symbolic
	mint(total_supply, amount)
	ret
end
