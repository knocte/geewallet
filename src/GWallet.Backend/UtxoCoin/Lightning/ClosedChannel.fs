namespace GWallet.Backend.UtxoCoin.Lightning

open System
open System.Linq

open NBitcoin

open DotNetLightning.Utils
open DotNetLightning.Serialization.Msgs
open DotNetLightning.Channel
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin

open FSharp.Core
open ResultUtils.Portability

type internal CloseChannelError =
    | CloseCommandFailed of ChannelError
    | RecvFailed of RecvMsgError
    | RecvPeerError of BrokenChannel * PeerErrorMessage
    | ExpectedShutdownMsg of ILightningMsg
    | RemoteShutdownCommandFailed of BrokenChannel * ChannelError
    | ExpectedClosingSignedMsg of ILightningMsg
    | ApplyClosingSignedFailed of ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | CloseCommandFailed err -> SPrintF1 "Failed to apply close command to the channel: %s" err.Message
            | RecvFailed err -> SPrintF1 "Failed to receive response from peer: %s" (err :> IErrorMsg).Message
            | RecvPeerError (_, err) -> SPrintF1 "Peer responded with an error: %s" (err :> IErrorMsg).Message
            | ExpectedShutdownMsg msg ->
                SPrintF2 "Expected to receive a Shutdown message from peer, but got %A: %s" msg (msg.ToString())
            | RemoteShutdownCommandFailed (_, err) ->
                SPrintF1 "Failed to apply RemoteShutdown command to the channel: %s" err.Message
            | ExpectedClosingSignedMsg msg ->
                SPrintF2 "Expected to receive a ClosingSigned message from peer, but got %A: %s" msg (msg.ToString())
            | ApplyClosingSignedFailed err ->
                SPrintF1 "Failed to apply ClosingSigned command to the channel: %s" err.Message


(*
    +-------+                              +-------+
    |       |--(1)-----  shutdown  ------->|       |
    |       |<-(2)-----  shutdown  --------|       |
    |       |                              |       |
    |       | <complete all pending HTLCs> |       |
    |   A   |                 ...          |   B   |
    |       |                              |       |
    |       |--(3)-- closing_signed  F1--->|       |
    |       |<-(4)-- closing_signed  F2----|       |
    |       |              ...             |       |
    |       |--(?)-- closing_signed  Fn--->|       |
    |       |<-(?)-- closing_signed  Fn----|       |
    +-------+                              +-------+
*)

type ClosedChannel()= 
    static member internal InitiateCloseChannel(connectedChannel: ConnectedChannel): Async<Result<ClosedChannel, CloseChannelError>> =
        async {
            let ourPayoutScript =
                connectedChannel.Channel.Channel.Config.ChannelOptions.ShutdownScriptPubKey.Value

            let! shutdownSendResult = ClosedChannel.InitiateShutdown connectedChannel ourPayoutScript

            match shutdownSendResult with
            | Error e -> return Error <| e
            | Ok connectedChannelAfterShutdownSent ->
                let! closingSignedExchangeResult = ClosedChannel.RunClosingSignedExchange connectedChannelAfterShutdownSent ourPayoutScript

                match closingSignedExchangeResult with
                | Error e -> return Error <| e
                | Ok _ -> return Ok (ClosedChannel())
        }

    static member internal AcceptCloseChannel(connectedChannel: ConnectedChannel, shutdownMsg: ShutdownMsg): Async<Result<ClosedChannel, CloseChannelError>> =
        async {
            let ourPayoutScript =
                connectedChannel.Channel.Channel.Config.ChannelOptions.ShutdownScriptPubKey.Value

            let! handleRemoteShutdownResult = ClosedChannel.HandleRemoteShutdown connectedChannel shutdownMsg false

            match handleRemoteShutdownResult with
            | Error e -> return Error <| e
            | Ok connectedChannelAfterShutdownReceived ->
                let! closingSignedExchangeResult = ClosedChannel.RunClosingSignedExchange connectedChannelAfterShutdownReceived ourPayoutScript

                match closingSignedExchangeResult with
                | Error e -> return Error <| e
                | Ok _ -> return Ok (ClosedChannel())

        }

    static member private InitiateShutdown connectedChannel ourPayoutScript: Async<Result<ConnectedChannel, CloseChannelError>> =
        async {
            Infrastructure.LogDebug "Sending shutdown message"
            let channelWrapper = connectedChannel.Channel
            let peerWrapper = connectedChannel.PeerNode
            match OperationClose.Create ourPayoutScript with
            | Error e -> return failwith (SPrintF1 "Failed to create OperationClose: %s" (e.ToString()))
            | Ok op ->
                let channelCommand = ChannelCommand.Close op

                let closeChannelMsgRes, channelWrapperAfterCloseChannel =
                    channelWrapper.ExecuteCommand channelCommand
                    <| function
                    | (AcceptedOperationShutdown (closeChannelMsg) :: []) -> Some closeChannelMsg
                    | _ -> None

                match closeChannelMsgRes with
                | Error channelError -> return Error <| (CloseCommandFailed channelError)
                | Ok closeChannelMsg ->
                    let! peerWrapperAfterCloseChannel = peerWrapper.SendMsg closeChannelMsg
                    Infrastructure.LogDebug "Waiting for response to shutdown message"

                    let connectedChannelAfterShutdownMessageSent = 
                        { connectedChannel with 
                            Channel = channelWrapperAfterCloseChannel 
                            PeerNode = peerWrapperAfterCloseChannel }

                    let! receiveRemoteShutdownRes = ClosedChannel.ReceiveRemoteShutdown connectedChannelAfterShutdownMessageSent
                    match receiveRemoteShutdownRes with
                    | Error err ->
                        return Error <| err
                    | Ok (connectedChannelAfterShutdownMessageReceived, shutdownMsg) ->
                        return! (ClosedChannel.HandleRemoteShutdown connectedChannelAfterShutdownMessageReceived shutdownMsg true)       
        }

    static member private ReceiveRemoteShutdown connectedChannel: Async<Result<(ConnectedChannel * ShutdownMsg), CloseChannelError>> =
        async {
            let! recvChannelMsgRes = connectedChannel.PeerNode.RecvChannelMsg()
            match recvChannelMsgRes with
            | Error (RecvMsg recvMsgError) -> return Error <| RecvFailed recvMsgError
            | Error (ReceivedPeerErrorMessage (peerWrapperAfterShutdownChannel, errorMessage)) ->
                let connectedChannelAfterError =
                    { connectedChannel with
                        PeerNode = peerWrapperAfterShutdownChannel }

                let brokenChannel =
                    { BrokenChannel.ConnectedChannel = connectedChannelAfterError }

                return Error <| RecvPeerError(brokenChannel, errorMessage)
            | Ok (peerWrapperAfterMessageReceived, channelMsg) ->
                let connectedChannelAfterMessageReceived =
                    { connectedChannel with
                        PeerNode = peerWrapperAfterMessageReceived } 

                match channelMsg with
                | :? ShutdownMsg as shutdownMsg ->
                    return Ok (connectedChannelAfterMessageReceived, shutdownMsg)
                | :? FundingLockedMsg ->
                    Infrastructure.LogDebug "Received FundingLocked when expecting Shutdown - ignoring"
                    return! ClosedChannel.ReceiveRemoteShutdown connectedChannelAfterMessageReceived
                | msg ->
                    return Error <| ExpectedShutdownMsg msg
        }

    static member private HandleRemoteShutdown (connectedChannel: ConnectedChannel) (shutdownMsg: ShutdownMsg) (sentOurs: bool): Async<Result<ConnectedChannel, CloseChannelError>> =
        async {
            Infrastructure.LogDebug "Received remote shutdown message"

            let peerWrapperAfterShutdownChannelReceived = connectedChannel.PeerNode

            let remoteShutdownCmd =
                ChannelCommand.RemoteShutdown shutdownMsg

            let shutdownResponseResult, channelWrapperAfterShutdownResponse =
                connectedChannel.Channel.ExecuteCommand remoteShutdownCmd
                <| function
                | (AcceptedShutdownWhenNoPendingHTLCs (msg, nextState)) :: [] -> Some(msg, nextState)
                | _ -> None

            match shutdownResponseResult with
            | Error err ->
                let connectedChannelAfterError =
                    { connectedChannel with
                          Channel = channelWrapperAfterShutdownResponse }

                let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message

                let brokenChannel =
                    { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }

                return Error
                       <| RemoteShutdownCommandFailed(brokenChannel, err)
            | Ok shutdownResponseMsg ->
                match shutdownResponseMsg with
                // Depending on if we are the funder or not we have to send the first ClosingSigned
                | closingSignedOpt, negotiationData ->

                    let mutable peerWrapperAfterShutdownResponse = peerWrapperAfterShutdownChannelReceived
                    if not sentOurs then
                        Infrastructure.LogDebug "Responding with our shutdown"
                        let! p = peerWrapperAfterShutdownChannelReceived.SendMsg negotiationData.LocalShutdown
                        peerWrapperAfterShutdownResponse <- p

                    match closingSignedOpt with
                    | Some closingSigned ->
                        Infrastructure.LogDebug "We are funder, sending initial ClosingSigned"
                        Infrastructure.LogDebug(SPrintF1 "ClosingSigned: %A" closingSigned)
                        let! peerWrapperAfterClosingSigned =
                            peerWrapperAfterShutdownChannelReceived.SendMsg closingSigned

                        return Ok
                                   { connectedChannel with
                                         PeerNode = peerWrapperAfterClosingSigned
                                         Channel = channelWrapperAfterShutdownResponse }
                    | None ->
                        Infrastructure.LogDebug "We are not funder, waiting for initial ClosingSigned from peer"

                        return Ok
                                   { connectedChannel with
                                         PeerNode = peerWrapperAfterShutdownResponse
                                         Channel = channelWrapperAfterShutdownResponse }
        }

    static member private RunClosingSignedExchange connectedChannel ourPayoutScript: Async<Result<ConnectedChannel, CloseChannelError>> =
        async {
            Infrastructure.LogDebug "Starting closingSigned exchange loop"

            let rec exchange channel =
                async {
                    Infrastructure.LogDebug "Waiting for next closingSigned message from peer"
                    let recvChannelMsgRes = channel.PeerNode.RecvChannelMsg() |> Async.RunSynchronously

                    match recvChannelMsgRes with
                    | Error (RecvMsg _) ->
                        // We assume the peer closed the connection after broadcasting the tx
                        Infrastructure.LogDebug "Peer closed connection after successful closing negotiation"
                        channel.SaveToWallet()
                        return Ok channel
                    | Error (ReceivedPeerErrorMessage (peerWrapperAfterClosingSignedReceived, errorMessage)) ->
                        let connectedChannelAfterError =
                            { channel with
                                  PeerNode = peerWrapperAfterClosingSignedReceived }

                        let brokenChannel =
                            { BrokenChannel.ConnectedChannel = connectedChannelAfterError }

                        return Error
                               <| RecvPeerError(brokenChannel, errorMessage)
                    | Ok (peerWrapperAfterClosingSignedReceived, channelMsg) ->
                        let connectedChannelAfterClosingSignedReceived =
                            { channel with
                                  PeerNode = peerWrapperAfterClosingSignedReceived }

                        match channelMsg with
                        | :? ClosingSignedMsg as closingSignedMsg ->
                            Infrastructure.LogDebug "Received closingSigned from peer"
                            Infrastructure.LogDebug(SPrintF1 "ClosingSigned: %A" closingSignedMsg)

                            let closingSignedCmd =
                                ChannelCommand.ApplyClosingSigned closingSignedMsg

                            let closingSignedResponseResult, channelWrapperAfterClosingSignedResponse =
                                connectedChannelAfterClosingSignedReceived.Channel.ExecuteCommand
                                    closingSignedCmd
                                <| function
                                | (WeProposedNewClosingSigned (msg, _)) :: [] -> Some(Some msg, None)
                                | (MutualClosePerformed (finalizedTx, _)) :: [] -> Some(None, Some finalizedTx)
                                | _ -> None

                            match closingSignedResponseResult with
                            | Ok closingSignedResponseMsg ->
                                match closingSignedResponseMsg with
                                | (Some msg, None) ->
                                    Infrastructure.LogDebug "Responding with new closingSigned"
                                    Infrastructure.LogDebug(SPrintF1 "ClosingSigned: %A" msg)
                                    let! peerWrapperAfterClosingSignedResponse =
                                        peerWrapperAfterClosingSignedReceived.SendMsg msg

                                    let connectedChannelAfterClosingSignedResponse =
                                        { connectedChannelAfterClosingSignedReceived with
                                              PeerNode = peerWrapperAfterClosingSignedResponse
                                              Channel = channelWrapperAfterClosingSignedResponse }

                                    let! result = exchange connectedChannelAfterClosingSignedResponse

                                    return result
                                | (None, Some finalizedTx) ->
                                    Infrastructure.LogDebug "Mutual close performed"
                                    Infrastructure.LogDebug(SPrintF1 "FinalizedTX: %A" finalizedTx)
                                    Infrastructure.LogDebug(SPrintF1 "ourPayoutScript: %A" ourPayoutScript)

                                    let! _txid =
                                        let signedTx: string = finalizedTx.Value.ToHex()
                                        Infrastructure.LogDebug(SPrintF1 "Broadcasting tx: %A" signedTx)
                                        Account.BroadcastRawTransaction Currency.BTC signedTx

                                    Infrastructure.LogDebug(SPrintF1 "Got tx: %A" _txid)

                                    let connectedChannelAfterMutualClosePerformed =
                                        { connectedChannelAfterClosingSignedReceived with
                                              PeerNode = peerWrapperAfterClosingSignedReceived
                                              Channel = channelWrapperAfterClosingSignedResponse }

                                    connectedChannelAfterMutualClosePerformed.SaveToWallet()

                                    return Ok connectedChannelAfterMutualClosePerformed
                                | _ ->
                                    // This should never happen
                                    return failwith "Expected to receive either new closingSigned or mutualClosePerformed"
                            | Error err -> return Error <| ApplyClosingSignedFailed err
                        | _ -> return Error <| ExpectedClosingSignedMsg channelMsg
                }

            let! result = exchange connectedChannel

            return result
        }

    static member internal CheckClosingFinished(fundingTxId: TxId): Async<Result<bool, CloseChannelError>> =
        async {
            let fundingTxIdHash = fundingTxId.Value.ToString()

            let! fundingVerboseTransaction =
                Server.Query Settings.Currency
                             (QuerySettings.Default ServerSelectionMode.Fast)
                             (ElectrumClient.GetBlockchainTransactionVerbose fundingTxIdHash)
                             None

            let parsedTransaction =
                NBitcoin.Transaction.Parse(fundingVerboseTransaction.Hex, Account.GetNetwork Settings.Currency)

            let maybeOutput =
                // TODO: find a better heuristic to check if this is the output we are looking for
                Seq.tryFind (fun (o: TxOut) -> o.ScriptPubKey.IsScriptType ScriptType.Witness) parsedTransaction.Outputs

            match maybeOutput with
            | None ->
                Infrastructure.LogDebug "Did not find an output with the correct type"
                return Ok false
            | Some output ->
                let sha =
                    NBitcoin.Crypto.Hashes.SHA256(output.ScriptPubKey.ToBytes())

                let reversedSha = sha.Reverse().ToArray()

                let scripthash =
                    NBitcoin.DataEncoders.Encoders.Hex.EncodeData reversedSha

                let! unspentOutputs =
                    Server.Query Settings.Currency
                                 (QuerySettings.Default ServerSelectionMode.Fast)
                                 (ElectrumClient.GetUnspentTransactionOutputs scripthash)
                                 None

                let outputIsUnspent =
                    Array.exists (fun o -> o.Value = output.Value.Satoshi) unspentOutputs

                return Ok(not outputIsUnspent)
        }
