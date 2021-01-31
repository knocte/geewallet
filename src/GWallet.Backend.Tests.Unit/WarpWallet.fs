namespace GWallet.Backend.Tests.Unit

open System

open NUnit.Framework

open GWallet.Backend

[<TestFixture>]
// testcases taken from https://github.com/keybase/warpwallet/blob/master/test/spec.json
type WarpWallet() =

    let BytesToHexString (bytes: array<byte>): string =
        BitConverter.ToString(bytes).Replace("-", "").ToLower()

    let TestValues (passphrase: string) (salt: string)
                   (scrypt: string) (pbkdf2: string) (xor: string) (privKeyWif: string) (pubAddress: string) =

        let scryptBytes = WarpKey.Scrypt passphrase salt
        Assert.That(BytesToHexString scryptBytes, Is.EqualTo scrypt)

        let pbkdf2Bytes = WarpKey.PBKDF2 passphrase salt
        Assert.That(BytesToHexString pbkdf2Bytes, Is.EqualTo pbkdf2)

        let privKeyBytes = WarpKey.XOR scryptBytes pbkdf2Bytes
        Assert.That(BytesToHexString privKeyBytes, Is.EqualTo xor)

        let privKeyBytes = WarpKey.CreatePrivateKey passphrase salt
        let LENGTH_OF_PRIVATE_KEYS = 32
        // we use uncompressed keys here in the tests (as opposed to the real wallet) because the original test vectors
        // of warp wallet are outdated, having 5-prefixed private keys instead of K/L ones
        let privKey = NBitcoin.Key(privKeyBytes, LENGTH_OF_PRIVATE_KEYS, false)
        Assert.That(privKey.GetWif(NBitcoin.Network.Main).ToWif(), Is.EqualTo privKeyWif)

        let pubKey = privKey.PubKey.GetAddress(NBitcoin.ScriptPubKeyType.Legacy, NBitcoin.Network.Main)
        Assert.That(pubKey.ToString(), Is.EqualTo pubAddress)

    [<Test>]
    member __.``vector 01``() =
        TestValues "ER8FT+HFjk0" "7DpniYifN6c"
                   "b58e47817de4d3901694b68bc8566ed5af9bec21e7a3bd56be114e2004ac148b"
                   "daab156024167271f4f894e91213f6cd52cd243dd19c7126075d0c1debeef114"
                   "6f2552e159f2a1e1e26c2262da459818fd56c81c363fcc70b94c423def42e59f"
                   "5JfEekYcaAexqcigtFAy4h2ZAY95vjKCvS1khAkSG8ATo1veQAD"
                   "1J32CmwScqhwnNQ77cKv9q41JGwoZe2JYQ"

    [<Test>]
    member __.``vector 02``() =
        TestValues "YqIDBApDYME" "G34HqIgjrIc"
                   "145c2af767a1116477fec267e758cf57614e5d7fa175f7d9acde5b4984d44181"
                   "ce5cbcf5c2d90be3e22b9e098ffc7b824827fa26f49f87fcf4b7865e47edc413"
                   "da009602a5781a8795d55c6e68a4b4d52969a75955ea70255869dd17c3398592"
                   "5KUJA5iZ2zS7AXkU2S8BiBVY3xj6F8GspLfWWqL9V7CajXumBQV"
                   "19aKBeXe2mi4NbQRpYUrCLZtRDHDUs9J7J"

    [<Test>]
    member __.``vector 03``() =
        TestValues "FPdAxCygMJg" "X+qaSwhUYXw"
                   "653083a95e681134205d7f4bf9c3f58a48bb8e7ef7715cba4e800bcc802c368a"
                   "4a5a7a04c713922d43a9a103de4ff1c420c47db03b542f275be4939712b08557"
                   "2f6af9ad997b831963f4de48278c044e687ff3cecc25739d1564985b929cb3dd"
                   "5JBAonQ4iGKFJxENExZghDtAS6YB8BsCw5mwpHSvZvP3Q2UxmT1"
                   "14Pqeo9XNRxjtKFFYd6TvRrJuZxVpciS81"

    [<Test>]
    member __.``vector 04``() =
        TestValues "gdoyAj5Y+jA" "E+6ZzCnRqVM"
                   "2f4ef304adad15cc851f3b2b4999c418ac4b4bff8958b09a0ccfae603084b814"
                   "75fe4aebad1d28d523e26c3128bac4518e78b9ad77e3be10aa643e82b67ea96c"
                   "5ab0b9ef00b03d19a6fd571a612300492233f252febb0e8aa6ab90e286fa1178"
                   "5JWE9LBvFM5xRE9YCnq3FD35fDVgTPNBmksGwW2jj5fi6cSgHsC"
                   "1KiiYhv9xkTZfcLYwqPhYHrSbvwJFFUgKv"

    [<Test>]
    member __.``vector 05``() =
        TestValues "bS7kqw6LDMJbvHwNFJiXlw" "tzsvA87xk+Rrw/5qj580kg"
                   "39fa48c58d6e8b81f98dc04cc8b898444410a1fc887a41d47648ba4eb93fe3ac"
                   "f5ea0f92683ec7986393b197129b380315bb9a08c36a70415bbab94b6492e1a6"
                   "cc104757e5504c199a1e71dbda23a04751ab3bf44b1031952df20305ddad020a"
                   "5KNA7T1peej9DwF5ZALUeqBq2xN4LHetaKvH9oRr8RHdTgwohd7"
                   "17ZcmAbJ35QJzAbwqAj4evo4vL5PwA8e7C"

    [<Test>]
    member __.``vector 06``() =
        TestValues "uyVkW5vKXX3RpvnUcj7U3Q" "zXrlmk3p5Lxr0vjJKdcJWQ"
                   "235d2b1480eee07bdc04114fd83294facd8f252ac142685595046bb2e680e9dc"
                   "239cc5df0fb8d80078e8fe5aac99552aca03dd1cab7276a4cb909083dc05dd8f"
                   "00c1eecb8f56387ba4ecef1574abc1d0078cf8366a301ef15e94fb313a853453"
                   "5Hpcw1rqoojG7LTHo4MrEHBwmBQBXQQmH6dEa89ayw5qMXvZmEZ"
                   "1ACJ7MhCRRTPaEvr2utat6FQjsQgC6qpE6"

    [<Test>]
    member __.``vector 07``() =
        TestValues "5HoGwwEMOgclQyzH72B9pQ" "UGKv/5nY3ig8bZvMycvIxQ"
                   "2dc5e1ad04bbd8b586563d7728e8c211537b78a8966ad71641634332c140317d"
                   "0b91675d6ab1486fdd543f9b6949109f056fa24785c93a2a8405c207840af1f3"
                   "265486f06e0a90da5b0202ec41a1d28e5614daef13a3ed3cc5668135454ac08e"
                   "5J7Ag5fBArgKN9ocVJs4rcQw1chZjHrqAb4YRuny6YiieJc5iG3"
                   "1Mtb2o7AsTRAR3vjtSYjq1rgB8Q6A76avD"

    [<Test>]
    member __.``vector 08``() =
        TestValues "TUMBDBWh8ArOK0+jO5glcA" "dAMOvN2WaEUTC/V5yg0eQA"
                   "68a1588996ce0da29e1b219c9a1e2d663a97d8c91d52bc38ff4e9bc5bd7b462c"
                   "9d8aee7f419bb46d093158fe63eca5bfe38a4c7a43387ac4b0127be800bfce41"
                   "f52bb6f6d755b9cf972a7962f9f288d9d91d94b35e6ac6fc4f5ce02dbdc4886d"
                   "5KgG93ePJJ8HC2tnTerThNUnXbjyeBpUCBDRn5ZxMRB9GxiwJEK"
                   "1B2VuTAHERd2GmBK522LFLUTwYWcW1vXH6"

    [<Test>]
    member __.``vector 09``() =
       TestValues "rDrc5eIhSt2qP8pnpnSMu1u2/mP6KTqS" "HGM1/qHoT3XX61NXw8H1nQ"
                  "e95a1ef65063f5ab041375b33f9f4d958cd11a32ca852032cfac38d0b3b1c90d"
                  "fafde347cb69a0aac6035506eff73d6411cff64ebadbc776617ecbcb6b759202"
                  "13a7fdb19b0a5501c21020b5d06870f19d1eec7c705ee744aed2f31bd8c45b0f"
                  "5HxwfzgQ2yem9uY5UxdiaKYPgUR251FCVHw1VuHC5bSW5NVLaok"
                  "12XD7BtiU1gydRzQm3cAoui2RQjhVJfNPg"

    [<Test>]
    member __.``vector 10``() =
        TestValues "Brd8TB3EDhegSx2wy2ffW0oGNC29vkCo" "dUBIrYPiUZ6BD/l+zBhthA"
                   "9f5cc8f45b5221352a487c81c7adfba40f8e3a3d860f3eaa8a0c4e0925ad054c"
                   "24aafad8ea524a72756a3ac56297615e056aad6df9fc8b6aeae8e338d167248a"
                   "bbf6322cb1006b475f224644a53a9afa0ae497507ff3b5c060e4ad31f4ca21c6"
                   "5KF4ozGWXGZAqNydQg65JQ4XnJaUpBkU9g59C287GrbLfWVmYHL"
                   "1CD93Tgj74uKh87dENR2GMWB1kpCidLZiS"

    [<Test>]
    member __.``vector 11``() =
        TestValues "eYuYtFxU4KrePYrbHSi/8ncAKEb+KbNH" "le5MMmWaj4AlGcRevRPEdw"
                   "f9f7f8d6d88c355416e53e1f0c82fb0f752551564e5d9c18ab63d6e42dd99eb0"
                   "4c420683306f07bff28cfb525af04d95bb5f9a168b9daebc5038a32590d2de2e"
                   "b5b5fe55e8e332ebe469c54d5672b69ace7acb40c5c032a4fb5b75c1bd0b409e"
                   "5KCK9EtgvjsQcPcZcfMoqcHwZKzA1MLfPUvDCYE1agiNf56CfAk"
                   "18mugeQN8uecTBE9psW2uQrhRBXZJkhyB7"

    [<Test>]
    member __.``vector 12``() =
       TestValues "TRGmdIHpnsSXjEnLc+U+MrRV3ryo8trG" "DhZNEt9hx08i6uMXo5DOyg"
                  "22c525d1fa3820c90b4f405b204c3599d9de67cbf851bf7e0f192fb61bb15f8c"
                  "51543758d7bbfd715a732f6359a4c3a92b9fc42d979db79b0af801c9f7d77824"
                  "739112892d83ddb8513c6f3879e8f630f241a3e66fcc08e505e12e7fec6627a8"
                  "5JhBaSsxgNBjvZWVfdVQsnMzYf4msHMQ7HRaHLvvMy1CEgsTstg"
                  "19QCgqHnKw8vrJph7wWP3nKg9tFixqYwiB"
