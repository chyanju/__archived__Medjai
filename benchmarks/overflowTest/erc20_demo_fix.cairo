%builtins range_check
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)

func mint{range_check_ptr}(total_supply : Uint256, amount : Uint256):
	alloc_locals
	uint256_check(amount)
	let (new_supply  : Uint256, is_overflow) = uint256_add(total_supply, amount)
	assert is_overflow = 0
	let (res : felt) = uint256_lt(new_supply, total_supply)
	verify res = 0
	return ()
end	

func main{range_check_ptr}():
	alloc_locals
        let sym1 = symbolic(felt, 'sym1')
	let sym2 = symbolic(felt, 'sym2')
	let sym3 = symbolic(felt, 'sym3')
	let sym4 = symbolic(felt, 'sym4')
	let total_supply = Uint256(sym1, sym2)
	let amount = Uint256(sym3, sym4)
	mint(total_supply, amount)
	ret
end
