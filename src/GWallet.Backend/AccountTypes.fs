namespace GWallet.Backend

open System.IO

type WatchWalletInfo =
    {
        UtxoCoinPublicKey: string
        EtherPublicAddress: string
    }

type FileRepresentation =
    {
        Name: string
        Content: unit -> string
    }

    static member FromFile (file: FileInfo) =
        {
            Name = Path.GetFileName file.FullName
            Content = (fun _ -> File.ReadAllText file.FullName)
        }

type ConceptAccount =
    {
        Currency: Currency
        FileRepresentation: FileRepresentation
        ExtractPublicAddressFromConfigFileFunc: FileRepresentation -> string
    }

type AccountKind =
    | Normal
    | ReadOnly
    | Archived

    static member All () =
        seq {
            yield Normal
            yield ReadOnly
            yield Archived
        }

type IAccount =
    abstract Currency : Currency
    abstract PublicAddress : string

[<AbstractClass>]
type BaseAccount
    (
        currency: Currency,
        accountFile: FileRepresentation,
        fromAccountFileToPublicAddress: FileRepresentation -> string
    ) =
    member val AccountFile = accountFile

    abstract Kind : AccountKind

    interface IAccount with
        member val Currency = currency
        member val PublicAddress = fromAccountFileToPublicAddress accountFile


type NormalAccount
    (
        currency: Currency,
        accountFile: FileRepresentation,
        fromAccountFileToPublicAddress: FileRepresentation -> string
    ) =
    inherit BaseAccount (currency, accountFile, fromAccountFileToPublicAddress)

    member internal __.GetEncryptedPrivateKey () =
        accountFile.Content ()

    override __.Kind = AccountKind.Normal

type ReadOnlyAccount
    (
        currency: Currency,
        accountFile: FileRepresentation,
        fromAccountFileToPublicAddress: FileRepresentation -> string
    ) =
    inherit BaseAccount (currency, accountFile, fromAccountFileToPublicAddress)

    override __.Kind = AccountKind.ReadOnly

type ArchivedAccount
    (
        currency: Currency,
        accountFile: FileRepresentation,
        fromAccountFileToPublicAddress: FileRepresentation -> string
    ) =
    inherit BaseAccount (currency, accountFile, fromAccountFileToPublicAddress)

    member internal __.GetUnencryptedPrivateKey () =
        accountFile.Content ()

    override __.Kind = AccountKind.Archived
