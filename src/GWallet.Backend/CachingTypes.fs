namespace GWallet.Backend

open System

type CachedValue<'T> = ('T * DateTime)

type NotFresh<'T> =
    | NotAvailable
    | Cached of CachedValue<'T>

type MaybeCached<'T> =
    | NotFresh of NotFresh<'T>
    | Fresh of 'T

type PublicAddress = string
type private DietCurrency = string
type private ServerIdentifier = string

type DietCache =
    {
        UsdPrice: Map<DietCurrency, decimal>
        Addresses: Map<PublicAddress, List<DietCurrency>>
        Balances: Map<DietCurrency, decimal>
    }
