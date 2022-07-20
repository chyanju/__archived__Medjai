%builtins range_check
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)

func mint{range_check_ptr}(total_supply : Uint256, amount : Uint256):
    alloc_locals
    uint256_check(amount)
    let (new_supply  : Uint256, _) = uint256_add(total_supply, amount)
    let (res : felt) = uint256_lt(new_supply, total_supply)
    assert res = 0 # Can check the counter-example concretely with an assert instead of verify
    return ()
end

func main{range_check_ptr}():
    alloc_locals
    let total_supply = Uint256(1, 2)
    let amount = Uint256(340282366920938463463374607431768211455, 340282366920938463463374607431768211453)
    mint(total_supply, amount)
    ret
end
