%builtins range_check
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)

func mint{range_check_ptr}(balance : Uint256, amount : Uint256):
	alloc_locals
#	uint256_check(amount)
#        uint256_check(balance)
	let (newBalance : Uint256, is_overflow) = uint256_add(balance, amount)
	assert overflow = 0
	let (res : felt) = uint256_lt(newBalance, balance)
	# verify res != 0
	assert res = res
	return ()
end	

func main{range_check_ptr}():
	alloc_locals
	let bal = Uint256(0, 1)
	let amount = Uint256(0, 5)
	mint(bal, amount)
	ret
end
