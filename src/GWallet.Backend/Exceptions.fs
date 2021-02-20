namespace GWallet.Backend

exception InsufficientFunds
exception InsufficientBalanceForFee of Option<decimal>

exception InvalidPassword
exception DestinationEqualToOrigin
exception AddressMissingProperPrefix of seq<string>

// with one value means that lenght is mandatory, 2 values means there's a lower limit and upper limit
exception AddressWithInvalidLength of seq<int>

exception AddressWithInvalidChecksum of Option<string>
exception AccountAlreadyAdded

exception InvalidDestinationAddress of msg: string
