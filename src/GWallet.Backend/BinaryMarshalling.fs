namespace GWallet.Backend

open System
open System.IO
open System.Text
open System.Runtime.Serialization.Formatters.Binary

module BinaryMarshalling =

    let private binFormatter = BinaryFormatter ()

    let Serialize obj : array<byte> =
        use memStream = new MemoryStream ()
        binFormatter.Serialize (memStream, obj)
        memStream.ToArray ()

    let Deserialize (buffer: array<byte>) =
        use memStream = new MemoryStream (buffer)
        memStream.Position <- 0L
        binFormatter.Deserialize memStream

    let SerializeToString obj : string =
        let byteArray = Serialize obj
        Convert.ToBase64String byteArray

    let DeserializeFromString (byteArrayString: string) =
        let byteArray = Convert.FromBase64String byteArrayString
        Deserialize byteArray
