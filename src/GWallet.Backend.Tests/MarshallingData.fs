namespace GWallet.Backend.Tests

open System
open System.IO
open System.Reflection

open NUnit.Framework
open Newtonsoft.Json.Linq

open GWallet.Backend
open GWallet.Backend.UtxoCoin
open GWallet.Backend.Ether

module MarshallingData =

    let private executingAssembly = Assembly.GetExecutingAssembly()
    let private version = executingAssembly.GetName().Version.ToString()
    let private binPath = executingAssembly.Location |> FileInfo
    let private prjPath = Path.Combine(binPath.Directory.FullName, "..") |> DirectoryInfo
    let private isUnix = not <| Config.IsWindowsPlatform()

    let private RemoveJsonFormatting (jsonContent: string): string =
        jsonContent.Replace("\r", String.Empty)
                   .Replace("\n", String.Empty)
                   .Replace("\t", String.Empty)

    let private InjectCurrentVersion (jsonContent: string): string =
        jsonContent.Replace("{version}", version)

    let private InjectCurrentDir (jsonContent: string): string =
        jsonContent.Replace("{prjDirAbsolutePath}", prjPath.FullName.Replace("\\", "/"))

    let internal Sanitize =
        RemoveJsonFormatting
        >> InjectCurrentVersion
        >> InjectCurrentDir

    let private ReadEmbeddedResource resourceName =
        let assembly = Assembly.GetExecutingAssembly()
        use stream = assembly.GetManifestResourceStream resourceName
        if null = stream then
            failwithf "Embedded resource %s not found" resourceName
        use reader = new StreamReader(stream)
        reader.ReadToEnd()
            |> Sanitize

    let UnsignedSaiTransactionExampleInJson =
        ReadEmbeddedResource "unsignedAndFormattedSaiTransaction.json"

    let SignedSaiTransactionExampleInJson =
        ReadEmbeddedResource "signedAndFormattedSaiTransaction.json"

    let BasicExceptionExampleInJson =
        ReadEmbeddedResource "basicException.json"

    let RealExceptionExampleInJson =
        ReadEmbeddedResource "realException.json"

    let InnerExceptionExampleInJson =
        ReadEmbeddedResource "innerException.json"

    let CustomExceptionExampleInJson =
        ReadEmbeddedResource "customException.json"

    let CustomFSharpExceptionExampleInJson =
        ReadEmbeddedResource "customFSharpException.json"

    let FullExceptionExampleInJson =
        ReadEmbeddedResource "fullException.json"


    let SerializedExceptionsAreSame actualJsonString expectedJsonString =

        let actualJsonException = JObject.Parse actualJsonString
        let expectedJsonException = JObject.Parse expectedJsonString

        let fullBinaryFormPath = "Value.FullBinaryForm"
        let tweakStackTraces () =

            let fullBinaryFormBeginning = "AAEAAAD/////AQAA"
            let stackTracePath = "Value.HumanReadableSummary.StackTrace"
            let stackTraceFragment = "ExceptionMarshalling.fs"

            let tweakStackTraceAndBinaryForm (jsonEx: JObject) (assertBinaryForm: bool) =
                let stackTraceJToken = jsonEx.SelectToken stackTracePath
                Assert.That(stackTraceJToken, Is.Not.Null, sprintf "Path %s not found in %s" stackTracePath (jsonEx.ToString()))
                let initialStackTraceJToken = stackTraceJToken.ToString()
                if initialStackTraceJToken.Length > 0 then
                    Assert.That(initialStackTraceJToken, Is.StringContaining stackTraceFragment)
                    let endOfStackTrace = initialStackTraceJToken.Substring(initialStackTraceJToken.IndexOf stackTraceFragment)
                    let tweakedEndOfStackTrace =
                        if isUnix then
                            endOfStackTrace
                                .Replace(":line 42", ":41 ")
                                .Replace(":line 65", ":64 ")
                        else
                            endOfStackTrace
                    stackTraceJToken.Replace (tweakedEndOfStackTrace |> JToken.op_Implicit)

                let binaryFormToken = jsonEx.SelectToken fullBinaryFormPath
                Assert.That(binaryFormToken, Is.Not.Null, sprintf "Path %s not found in %s" fullBinaryFormPath (jsonEx.ToString()))
                let initialBinaryFormJToken = binaryFormToken.ToString()
                if assertBinaryForm then
                    Assert.That(initialBinaryFormJToken, Is.StringStarting fullBinaryFormBeginning)
                binaryFormToken.Replace (fullBinaryFormBeginning |> JToken.op_Implicit)

            tweakStackTraceAndBinaryForm actualJsonException true
            tweakStackTraceAndBinaryForm expectedJsonException false

        tweakStackTraces()

        let actualBinaryForm = (actualJsonException.SelectToken fullBinaryFormPath).ToString()
        Assert.That(actualJsonException.ToString(), Is.EqualTo (expectedJsonException.ToString()),
                    sprintf "Exceptions didn't match. Full binary form was %s" actualBinaryForm)

        true

    let internal SomeDate = DateTime.Parse "2018-06-14T16:50:09.133411"

    let private someEtherMinerFee =
        Ether.MinerFee(21000L, 6969L, SomeDate, Currency.ETC)

    let private someUnsignedEtherTransactionProposal =
        {
            OriginAddress = "0xf3j4m0rjx94sushh03j";
            Amount = TransferAmount(10.01m, 12.02m, Currency.ETC);
            DestinationAddress = "0xf3j4m0rjxdddud9403j";
        }

    let EmptyCachingDataExample =
        { UsdPrice = Map.empty; Addresses = Map.empty; Balances = Map.empty; }

    let EmptyCachingDataExampleInJson =
        sprintf """{
  "Version": "%s",
  "TypeName": "%s",
  "Value": {
    "UsdPrice": {},
    "Addresses": {},
    "Balances": {}
  }
}"""        version (EmptyCachingDataExample.GetType().FullName)

    let private balances = Map.empty.Add(Currency.BTC.ToString(), 0m)
                                    .Add(Currency.ETC.ToString(), 123456789.12345678m)
    let private addresses = Map.empty.Add("1fooBarBaz", [Currency.BTC.ToString()])
                                     .Add("0xFOOBARBAZ", [Currency.ETC.ToString()])
    let private fiatValues = Map.empty.Add(Currency.ETH.ToString(), 161.796m)
                                      .Add(Currency.ETC.ToString(), 169.99999999m)
    let SofisticatedCachingDataExample = { UsdPrice = fiatValues; Addresses = addresses; Balances = balances; }

    let SofisticatedCachingDataExampleInJson =
        sprintf """{
  "Version": "%s",
  "TypeName": "%s",
  "Value": {
    "UsdPrice": {
      "ETC": 169.99999999,
      "ETH": 161.796
    },
    "Addresses": {
      "0xFOOBARBAZ": [
        "ETC"
      ],
      "1fooBarBaz": [
        "BTC"
      ]
    },
    "Balances": {
      "BTC": 0.0,
      "ETC": 123456789.12345678
    }
  }
}"""        version (typedefof<DietCache>.FullName)

    let private someUnsignedBtcTransactionProposal =
        {
            OriginAddress = "16pKBjGGZkUXo1afyBNf5ttFvV9hauS1kR";
            Amount = TransferAmount(10.01m, 12.02m, Currency.BTC);
            DestinationAddress = "13jxHQDxGto46QhjFiMb78dZdys9ZD8vW5";
        }

    let private someBtcTransactionInputs =
        [ { TransactionHash = "4d129e98d87fab00a99ebc88688752b588ec7d38c2ba5dc86d3563a6bc4c691f"
            OutputIndex = 1
            ValueInSatoshis = int64 1000
            DestinationInHex = "a9145131075257d8b8de8298e7c52891eb4b87823b9387" } ]

    let private realUsdPriceDataSample =
        [ (Currency.BTC.ToString(), 9156.19m);
          (Currency.LTC.ToString(), 173.592m);
          (Currency.ETH.ToString(), 691.52m);
          (Currency.ETC.ToString(), 19.8644m);
          (Currency.SAI.ToString(), 1.00376m) ]
            |> Map.ofSeq

    let private realAddressesSample =
        Map.empty.Add("3Buz1evVsQeHtDfQAmwfAKQsUzAt3f4TuR",[Currency.BTC.ToString()])
                 .Add("0xba766d6d13E2Cc921Bf6e896319D32502af9e37E",[Currency.ETH.ToString();
                                                                    Currency.SAI.ToString()
                                                                    Currency.ETC.ToString()])
                 .Add("MJ88KYLTpXVigiwJGevzyxfGogmKx7WiWm",[Currency.LTC.ToString()])

    let private realBalancesDataSample =
        Map.empty.Add(Currency.BTC.ToString(), 0.0m)
                 .Add(Currency.ETH.ToString(), 7.08m)
                 .Add(Currency.ETC.ToString(), 8.0m)
                 .Add(Currency.SAI.ToString(), 1.0m)
                 .Add(Currency.LTC.ToString(), 0.0m)

    let private realCachingDataExample =
        { UsdPrice = realUsdPriceDataSample; Addresses = realAddressesSample; Balances = realBalancesDataSample; }

    let private someBtcMinerFee = UtxoCoin.MinerFee(10L, SomeDate, Currency.BTC)
    let private someBtcTxMetadata =
        {
            Fee = someBtcMinerFee;
            Inputs = someBtcTransactionInputs
        }
    let UnsignedBtcTransactionExample =
        {
            Proposal = someUnsignedBtcTransactionProposal;
            Cache = realCachingDataExample;
            Metadata = someBtcTxMetadata;
        }

    let UnsignedBtcTransactionExampleInJson =
        ReadEmbeddedResource "unsignedAndFormattedBtcTransaction.json"

    let SignedBtcTransactionExample =
        {
            TransactionInfo = UnsignedBtcTransactionExample;
            RawTransaction = "0200000000010111b6e0460bb810b05744f8d38262f95fbab02b168b070598a6f31fad438fced4000000001716001427c106013c0042da165c082b3870c31fb3ab4683feffffff0200ca9a3b0000000017a914d8b6fcc85a383261df05423ddf068a8987bf0287873067a3fa0100000017a914d5df0b9ca6c0e1ba60a9ff29359d2600d9c6659d870247304402203b85cb05b43cc68df72e2e54c6cb508aa324a5de0c53f1bbfe997cbd7509774d022041e1b1823bdaddcd6581d7cde6e6a4c4dbef483e42e59e04dbacbaf537c3e3e8012103fbbdb3b3fc3abbbd983b20a557445fb041d6f21cc5977d2121971cb1ce5298978c000000";
        }

    let SignedBtcTransactionExampleInJson =
        ReadEmbeddedResource "signedAndFormattedBtcTransaction.json"

    let private someEtherTxMetadata =
        {
            Fee = someEtherMinerFee;
            TransactionCount = int64 69;
        }
    let UnsignedEtherTransactionExample =
        {
            Proposal = someUnsignedEtherTransactionProposal;
            Cache = EmptyCachingDataExample;
            Metadata = someEtherTxMetadata;
        }

    let private someEtherMinerFeeForSaiTransfer = Ether.MinerFee(37298L,
                                                                 3343750000L,
                                                                 SomeDate,
                                                                 Currency.ETH)
    let private someSaiTxMetadata =
        {
            Fee = someEtherMinerFeeForSaiTransfer
            TransactionCount = int64 7;
        }
    let private someUnsignedSaiTransactionProposal =
        {
            OriginAddress = "0xba766d6d13E2Cc921Bf6e896319D32502af9e37E";
            Amount = TransferAmount(1m, 7.08m, Currency.SAI)
            DestinationAddress = "0xDb0381B1a380d8db2724A9Ca2d33E0C6C044bE3b";
        }
    let UnsignedSaiTransactionExample =
        {
            Proposal = someUnsignedSaiTransactionProposal
            Cache = realCachingDataExample;
            Metadata = someSaiTxMetadata
        }
    let someSaiTransactionInfo =
        {
            Proposal = someUnsignedSaiTransactionProposal
            Cache = realCachingDataExample;
            Metadata = someSaiTxMetadata
        }
    let SignedSaiTransactionExample =
        {
            TransactionInfo = someSaiTransactionInfo
            RawTransaction = "f8a80784c74d93708291b29489d24a6b4ccb1b6faa2625fe562bdd9a2326035980b844a9059cbb000000000000000000000000db0381b1a380d8db2724a9ca2d33e0c6c044be3b0000000000000000000000000000000000000000000000000de0b6b3a764000026a072cdeb03affd5977c76366efbc1405fbb4fa997ce72c1e4554ba9ec5ef772ddca069d522ea304efebd2537330870bc1ca9e9a6fe3eb5f8d8f66c1b82d9fc27a4bf";
        }

    let someEtherTransactionInfo =
        {
            Proposal = someUnsignedEtherTransactionProposal;
            Cache = SofisticatedCachingDataExample;
            Metadata = someEtherTxMetadata;
        }
    let SignedEtherTransactionExample =
        {
            TransactionInfo = someEtherTransactionInfo;
            RawTransaction = "doijfsoifjdosisdjfomirmjosmi";
        }
    let SignedEtherTransactionExampleInJson =
        ReadEmbeddedResource "signedAndFormattedEtherTransaction.json"

    let UnsignedEtherTransactionExampleInJson =
        ReadEmbeddedResource "unsignedAndFormattedEtherTransaction.json"
