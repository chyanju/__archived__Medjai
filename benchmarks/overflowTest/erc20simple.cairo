%builtins range_check
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)

func mint{range_check_ptr}(balance : Uint256, amount : Uint256):
	alloc_locals
	#uint256_check(amount)
        #uint256_check(balance)
	let (newBalance  : Uint256, is_overflow) = uint256_add(balance, amount)
        #assert is_overflow = 0
	let (res : felt) = uint256_lt(newBalance, balance)
	# verify res != 0
	assert res = 0
	return ()
end	

func main{range_check_ptr}():
	alloc_locals
	let bal = Uint256(0, 10855508365998393641091968349285210317209604012915728563382650775839384272899)
	let amount = Uint256(0, -3618502788666131213697322783095070105282824848410658236509717448704103809026)
	mint(bal, amount)
	ret
end
