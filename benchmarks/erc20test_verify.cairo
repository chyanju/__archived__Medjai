%lang starknet
%builtins pedersen range_check

from starkware.cairo.common.cairo_builtins import HashBuiltin
from starkware.cairo.common.uint256 import (Uint256, uint256_add, uint256_check, uint256_lt)
from starkware.cairo.common.math import assert_not_zero

@storage_var
func total_supply() -> (res : Uint256):
end

@storage_var
func balances(account : felt) -> (res : Uint256):
end

const SHIFT = 2 ** 128
const ALL_ONES = 2 ** 128 - 1
const HALF_SHIFT = 2 ** 64

func mint{syscall_ptr : felt*, pedersen_ptr : HashBuiltin*, range_check_ptr}(
    recipient : felt, amount : Uint256
):
    alloc_locals
    assert_not_zero(recipient)
    uint256_check(amount)

    let (balance : Uint256) = balances.read(account=recipient)
    # overflow is not possible because sum is guaranteed to be less than total supply
    # which we check for overflow below
    let (new_balance, _ : Uint256) = uint256_add(balance, amount)
    let (res : felt) = uint256_lt(new_balance, balance)
    verify res != 1
    balances.write(recipient, new_balance)

    let (local supply : Uint256) = total_supply.read()
    let (local new_supply : Uint256, is_overflow) = uint256_add(supply, amount)
    assert (is_overflow) = 0

    total_supply.write(new_supply)
    return ()
end

func main():
    [ap] = 1000; ap++
    [ap] = 2000; ap++
    [ap] = [ap - 2] + [ap - 1]; ap++
    ret
end
