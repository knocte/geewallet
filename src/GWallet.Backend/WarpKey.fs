// because of the use of obsolete NBitcoin.Crypto.Pbkdf2
#nowarn "44"

namespace GWallet.Backend

open System
open System.Text
open System.Security.Cryptography

open GWallet.Backend.FSharpUtil.UwpHacks

// .NET implementation for WarpWallet: https://keybase.io/warp/
module WarpKey =

    let XOR (a: array<byte>) (b: array<byte>): array<byte> =
        if a.Length <> b.Length then
            raise <| ArgumentException ()
        else
            let result = Array.create<byte> a.Length (byte 0)
            for i = 0 to (a.Length - 1) do
                result.[i] <- ((a.[i]) ^^^ (b.[i]))
            result

    let Scrypt (passphrase: string) (salt: string): array<byte> =
        // FIXME: stop using mutable collections
        let passphraseByteList = System.Collections.Generic.List<byte>()
        passphraseByteList.AddRange (Encoding.UTF8.GetBytes(passphrase))
        passphraseByteList.Add (byte 1)

        let saltByteList = System.Collections.Generic.List<byte>()
        saltByteList.AddRange (Encoding.UTF8.GetBytes(salt))
        saltByteList.Add (byte 1)

        NBitcoin.Crypto.SCrypt.ComputeDerivedKey(passphraseByteList.ToArray(),
                                                 saltByteList.ToArray(),
                                                 262144, 8, 1, Nullable<int>(), 32)

    let PBKDF2 (passphrase: string) (salt: string): array<byte> =
        // FIXME: stop using mutable collections
        let passphraseByteList = System.Collections.Generic.List<byte>()
        passphraseByteList.AddRange (Encoding.UTF8.GetBytes(passphrase))
        passphraseByteList.Add (byte 2)

        let saltByteList = System.Collections.Generic.List<byte>()
        saltByteList.AddRange (Encoding.UTF8.GetBytes(salt))
        saltByteList.Add (byte 2)

        use hashAlgo = new HMACSHA256(passphraseByteList.ToArray())

        // TODO: remove nowarn when we switch to .NET BCL's impl instead of NBitcoin.Crypto
        NBitcoin.Crypto.Pbkdf2.ComputeDerivedKey(hashAlgo, saltByteList.ToArray(), 65536, 32)

    let private LENGTH_OF_PRIVATE_KEYS = 32
    let CreatePrivateKey (passphrase: string) (salt: string) =
        let scrypt = Scrypt passphrase salt
        let pbkdf2 = PBKDF2 passphrase salt
        let privKeyBytes = XOR scrypt pbkdf2
        if privKeyBytes.Length <> LENGTH_OF_PRIVATE_KEYS then
            failwith <| SPrintF2 "Something went horribly wrong because length of privKey was not %i but %i"
                      LENGTH_OF_PRIVATE_KEYS privKeyBytes.Length
        privKeyBytes

