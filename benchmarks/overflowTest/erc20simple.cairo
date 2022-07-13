%builtins range_check
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)

func mint{range_check_ptr}(balance : Uint256, amount : Uint256):
	alloc_locals
	let (newBalance, _  : Uint256) = uint256_add(balance, amount)
	let (res : felt) = uint256_lt(newBalance, balance)
	verify res != 0
	return ()
end	

func main{range_check_ptr}():
	alloc_locals
	let bal = Uint256(0, 1)
	let amount = Uint256(0, 5)
	mint(bal, amount)
	ret
end 
