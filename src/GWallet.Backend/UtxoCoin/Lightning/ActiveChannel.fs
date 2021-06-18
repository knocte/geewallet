namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Crypto
open DotNetLightning.Channel
open DotNetLightning.Utils
open DotNetLightning.Serialization
open DotNetLightning.Serialization.Msgs
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks
open GWallet.Backend.UtxoCoin
open GWallet.Backend.UtxoCoin.Lightning.Watcher

type internal LockFundingError =
    | FundingNotConfirmed of FundedChannel * BlockHeightOffset32
    | FundingOnChainLocationUnknown of FundedChannel
    | RecvFundingLocked of RecvMsgError
    | FundingLockedPeerErrorResponse of BrokenChannel * PeerErrorMessage
    | ExpectedFundingLocked of ILightningMsg
    | InvalidFundingLocked of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | FundingNotConfirmed(_, remainingConfirmations) ->
                SPrintF1
                    "Funding not yet confirmed on-chain. %i more confirmations required"
                    remainingConfirmations.Value
            | FundingOnChainLocationUnknown _ ->
                "Funding appears to be confirmed but its on-chain location has not been indexed yet"
            | RecvFundingLocked err ->
                SPrintF1 "Error receiving funding locked: %s" (err :> IErrorMsg).Message
            | FundingLockedPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our funding_locked with an error: %s" (err :> IErrorMsg).Message
            | ExpectedFundingLocked msg ->
                SPrintF1 "Expected funding_locked message, got %A" (msg.GetType())
            | InvalidFundingLocked (_, err) ->
                SPrintF1 "Invalid funding_locked message: %s" err.Message
        member self.ChannelBreakdown: bool =
            match self with
            | FundingNotConfirmed _ -> false
            | FundingOnChainLocationUnknown _ -> false
            | RecvFundingLocked recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | FundingLockedPeerErrorResponse _ -> true
            | ExpectedFundingLocked _ -> false
            | InvalidFundingLocked _ -> true

    member internal self.PossibleBug =
        match self with
        | RecvFundingLocked err -> err.PossibleBug
        | FundingNotConfirmed _
        | FundingOnChainLocationUnknown _
        | FundingLockedPeerErrorResponse _
        | ExpectedFundingLocked _
        | InvalidFundingLocked _ -> false

type internal ReconnectActiveChannelError =
    | Reconnect of ReconnectError
    | LockFunding of LockFundingError
    interface IErrorMsg with
        member self.Message =
            match self with
            | Reconnect err ->
                SPrintF1 "Error reconnecting: %s" (err :> IErrorMsg).Message
            | LockFunding err ->
                SPrintF1 "Error locking funding: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | Reconnect reconnectError -> (reconnectError :> IErrorMsg).ChannelBreakdown
            | LockFunding lockFundingError -> (lockFundingError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | Reconnect err -> err.PossibleBug
        | LockFunding err -> err.PossibleBug

type internal SendCommitError =
    | RecvRevokeAndAck of RecvMsgError
    | CommitmentSignedPeerErrorResponse of BrokenChannel * PeerErrorMessage
    | ExpectedRevokeAndAck of ILightningMsg
    | InvalidRevokeAndAck of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvRevokeAndAck err ->
                SPrintF1 "Error receiving revoke_and_ack: %s" (err :> IErrorMsg).Message
            | CommitmentSignedPeerErrorResponse (_, err) ->
                SPrintF1 "Peer responded to our commitment_signed with an error message: %s" (err :> IErrorMsg).Message
            | ExpectedRevokeAndAck msg ->
                SPrintF1 "Expected revoke_and_ack, got %A" (msg.GetType())
            | InvalidRevokeAndAck (_, err) ->
                SPrintF1 "Invalid revoke_and_ack: %s" err.Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvRevokeAndAck recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | CommitmentSignedPeerErrorResponse _ -> true
            | ExpectedRevokeAndAck _ -> false
            | InvalidRevokeAndAck _ -> true

    member internal self.PossibleBug =
        match self with
        | RecvRevokeAndAck err -> err.PossibleBug
        | CommitmentSignedPeerErrorResponse _
        | ExpectedRevokeAndAck _
        | InvalidRevokeAndAck _ -> false

type internal RecvCommitError =
    | RecvCommitmentSigned of RecvMsgError
    | PeerErrorMessageInsteadOfCommitmentSigned of BrokenChannel * PeerErrorMessage
    | ExpectedCommitmentSigned of ILightningMsg
    | InvalidCommitmentSigned of BrokenChannel * ChannelError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvCommitmentSigned err ->
                SPrintF1 "Error receiving commitment_signed: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfCommitmentSigned (_, err) ->
                SPrintF1 "Peer sent us an error message instead of commitment_signed: %s" (err :> IErrorMsg).Message
            | ExpectedCommitmentSigned msg ->
                SPrintF1 "Expected commitment_signed, got %A" (msg.GetType())
            | InvalidCommitmentSigned (_, err) ->
                SPrintF1 "Invalid commitment signed: %s" err.Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvCommitmentSigned recvMsgError -> (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorMessageInsteadOfCommitmentSigned _
            | InvalidCommitmentSigned _ -> true
            | ExpectedCommitmentSigned _ -> false

    member internal self.PossibleBug =
        match self with
        | RecvCommitmentSigned err -> err.PossibleBug
        | PeerErrorMessageInsteadOfCommitmentSigned _
        | ExpectedCommitmentSigned _
        | InvalidCommitmentSigned _ -> false

type internal SendMonoHopPaymentError =
    | InvalidMonoHopPayment of ActiveChannel * InvalidMonoHopUnidirectionalPaymentError
    | SendCommit of SendCommitError
    | RecvCommit of RecvCommitError
    interface IErrorMsg with
        member self.Message =
            match self with
            | InvalidMonoHopPayment (_, err) ->
                SPrintF1 "Invalid monohop payment: %s" err.Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | InvalidMonoHopPayment _ -> false
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | InvalidMonoHopPayment _ -> false
        | SendCommit err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug

and internal RecvMonoHopPaymentError =
    | RecvMonoHopPayment of RecvMsgError
    | PeerErrorMessageInsteadOfMonoHopPayment of BrokenChannel * PeerErrorMessage
    | InvalidMonoHopPayment of BrokenChannel * ChannelError
    | ExpectedMonoHopPayment of ILightningMsg
    | RecvCommit of RecvCommitError
    | SendCommit of SendCommitError
    interface IErrorMsg with
        member self.Message =
            match self with
            | RecvMonoHopPayment err ->
                SPrintF1 "Error receiving monohop payment message: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfMonoHopPayment (_, err) ->
                SPrintF1 "Peer sent us an error message instead of a monohop payment: %s" (err :> IErrorMsg).Message
            | InvalidMonoHopPayment (_, err) ->
                SPrintF1 "Invalid monohop payment message: %s" err.Message
            | ExpectedMonoHopPayment msg ->
                SPrintF1 "Expected monohop payment msg, got %A" (msg.GetType())
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
        member self.ChannelBreakdown: bool =
            match self with
            | RecvMonoHopPayment recvMsgError ->
                (recvMsgError :> IErrorMsg).ChannelBreakdown
            | PeerErrorMessageInsteadOfMonoHopPayment _ -> true
            | InvalidMonoHopPayment _ -> true
            | ExpectedMonoHopPayment _ -> false
            | RecvCommit recvCommitError ->
                (recvCommitError :> IErrorMsg).ChannelBreakdown
            | SendCommit sendCommitError ->
                (sendCommitError :> IErrorMsg).ChannelBreakdown

    member internal self.PossibleBug =
        match self with
        | RecvMonoHopPayment err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug
        | SendCommit err -> err.PossibleBug
        | PeerErrorMessageInsteadOfMonoHopPayment _
        | InvalidMonoHopPayment _
        | ExpectedMonoHopPayment _ -> false

and internal AcceptUpdateFeeError =
    | RecvUpdateFee of RecvMsgError
    | PeerErrorMessageInsteadOfUpdateFee of BrokenChannel * PeerErrorMessage
    | ExpectedUpdateFee of ILightningMsg
    | InvalidUpdateFee of BrokenChannel * ChannelError
    | RecvCommit of RecvCommitError
    | SendCommit of SendCommitError
    interface IErrorMsg with
        member self.ChannelBreakdown: bool =
            (self :> IErrorMsg).ChannelBreakdown
        member self.Message =
            match self with
            | RecvUpdateFee err ->
                SPrintF1 "Error receiving update fee message: %s" (err :> IErrorMsg).Message
            | PeerErrorMessageInsteadOfUpdateFee (_, err) ->
                SPrintF1 "Peer sent us an error message instead of an update fee message: %s" (err :> IErrorMsg).Message
            | ExpectedUpdateFee msg ->
                SPrintF1 "Expected update fee msg, got %A" (msg.GetType())
            | InvalidUpdateFee (_, err) ->
                SPrintF1 "Invalid update fee message: %s" err.Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message

and internal UpdateFeeError =
    | SendCommit of SendCommitError
    | RecvCommit of RecvCommitError
    interface IErrorMsg with
        member self.ChannelBreakdown: bool =
            (self :> IErrorMsg).ChannelBreakdown
        member self.Message =
            match self with
            | SendCommit err ->
                SPrintF1 "Error sending commitment: %s" (err :> IErrorMsg).Message
            | RecvCommit err ->
                SPrintF1 "Error receiving commitment: %s" (err :> IErrorMsg).Message

and internal ActiveChannel =
    {
        ConnectedChannel: ConnectedChannel
    }
    interface IDisposable with
        member self.Dispose() =
            (self.ConnectedChannel :> IDisposable).Dispose()

    static member private LockFunding (fundedChannel: FundedChannel)
                                      (confirmationCount: BlockHeightOffset32)
                                      (absoluteBlockHeight: BlockHeight)
                                      (txIndexInBlock: TxIndexInBlock)
                                          : Async<Result<ActiveChannel, LockFundingError>> = async {
        let theirFundingLockedMsgOpt = fundedChannel.TheirFundingLockedMsgOpt
        if confirmationCount < fundedChannel.ConnectedChannel.MinimumDepth then
            failwith
                "LockFunding called when required confirmation depth has not been reached"
        let connectedChannel = fundedChannel.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel
        let ourFundingLockedMsgAndChannelRes =
            channel.Channel.ApplyFundingConfirmedOnBC
                absoluteBlockHeight
                txIndexInBlock
                confirmationCount
        let channelAfterFundingConfirmed, ourFundingLockedMsg = UnwrapResult ourFundingLockedMsgAndChannelRes "DNL error creating funding_locked msg"

        let! peerNodeAfterFundingLockedSent = peerNode.SendMsg ourFundingLockedMsg
        let! theirFundingLockedMsgRes = async {
            match theirFundingLockedMsgOpt with
            | Some theirFundingLockedMsg -> return Ok (peerNodeAfterFundingLockedSent, theirFundingLockedMsg)
            | None ->
                let! recvChannelMsgRes = peerNodeAfterFundingLockedSent.RecvChannelMsg()
                match recvChannelMsgRes with
                | Error (RecvMsg recvMsgError) -> return Error <| RecvFundingLocked recvMsgError
                | Error (ReceivedPeerErrorMessage (peerNodeAfterFundingLockedReceived, errorMessage)) ->
                    let connectedChannelAfterError = 
                        {
                            connectedChannel with
                                PeerNode = peerNodeAfterFundingLockedReceived
                                Channel = 
                                    {
                                        Channel = channelAfterFundingConfirmed
                                    }
                        }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| FundingLockedPeerErrorResponse
                        (brokenChannel, errorMessage)
                | Ok (peerNodeAfterFundingLockedReceived, channelMsg) ->
                    match channelMsg with
                    | :? FundingLockedMsg as fundingLockedMsg ->
                        return Ok (peerNodeAfterFundingLockedReceived, fundingLockedMsg)
                    | _ -> return Error <| ExpectedFundingLocked channelMsg
        }

        match theirFundingLockedMsgRes with
        | Error err -> return Error err
        | Ok (peerNodeAfterFundingLockedReceived, theirFundingLockedMsg) ->
            let channelAfterFundingLockedRes =
                channelAfterFundingConfirmed.ApplyFundingLocked
                    theirFundingLockedMsg

            match channelAfterFundingLockedRes with
            | Error err ->
                let connectedChannelAfterError = 
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterFundingLockedReceived
                            Channel = 
                                {
                                    Channel = channelAfterFundingConfirmed
                                }
                    }

                let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                return Error <| InvalidFundingLocked
                    (brokenChannel, err)
            | Ok channelAfterFundingLocked ->
                let connectedChannelAfterFundingLocked = 
                    {
                        connectedChannel with
                            PeerNode = peerNodeAfterFundingLockedReceived
                            Channel = 
                                {
                                    Channel = channelAfterFundingLocked
                                }
                    }
                connectedChannelAfterFundingLocked.SaveToWallet()
                let activeChannel: ActiveChannel = {
                    ConnectedChannel = connectedChannelAfterFundingLocked
                }
                return Ok activeChannel
    }

    static member private CheckFundingConfirmed (fundedChannel: FundedChannel)
                                                   : Async<Result<ActiveChannel, LockFundingError>> = async {
        let! confirmationCount = fundedChannel.GetConfirmations()
        if confirmationCount < fundedChannel.MinimumDepth then
            let remainingConfirmations = fundedChannel.MinimumDepth - confirmationCount
            return Error <| FundingNotConfirmed(fundedChannel, remainingConfirmations)
        else
            let! locationOnChainOpt = fundedChannel.GetLocationOnChain()
            match locationOnChainOpt with
            | None ->
                return Error <| FundingOnChainLocationUnknown fundedChannel
            | Some (absoluteBlockHeight, txIndexInBlock) ->
                return!
                    ActiveChannel.LockFunding
                        fundedChannel
                        confirmationCount
                        absoluteBlockHeight
                        txIndexInBlock
    }

    static member private ConfirmFundingLocked (connectedChannel: ConnectedChannel)
                                                   : Async<Result<ActiveChannel, LockFundingError>> = async {
        let channel = connectedChannel.Channel
        let nextLocalCommitment = channel.Channel.SavedChannelState.LocalCommit.Index
        let nextRemoteCommitment = channel.Channel.SavedChannelState.RemoteCommit.Index
        if nextLocalCommitment = CommitmentNumber.FirstCommitment && nextRemoteCommitment = CommitmentNumber.FirstCommitment then
            let fundedChannel = {
                FundedChannel.ConnectedChannel = connectedChannel
                TheirFundingLockedMsgOpt = 
                    match channel.Channel.RemoteNextCommitInfo with
                    | None -> None
                    | Some remoteNextCommitInfo ->
                        let msg : FundingLockedMsg =
                            {
                                ChannelId = channel.ChannelId.DnlChannelId
                                NextPerCommitmentPoint = remoteNextCommitInfo.PerCommitmentPoint()
                            }
                        Some msg
            }
            return! ActiveChannel.CheckFundingConfirmed fundedChannel
        else
            let activeChannel = {
                ActiveChannel.ConnectedChannel = connectedChannel
            }
            return Ok activeChannel
    }

    static member internal ConnectReestablish (channelStore: ChannelStore)
                                              (nodeMasterPrivKey: NodeMasterPrivKey)
                                              (channelId: ChannelIdentifier)
                                                  : Async<Result<ActiveChannel, ReconnectActiveChannelError>> = async {
        let! connectRes =
            ConnectedChannel.ConnectFromWallet channelStore nodeMasterPrivKey channelId
        match connectRes with
        | Error reconnectError -> return Error <| Reconnect reconnectError
        | Ok connectedChannel ->
            let! activeChannelRes = ActiveChannel.ConfirmFundingLocked connectedChannel
            match activeChannelRes with
            | Error lockFundingError -> return Error <| LockFunding lockFundingError
            | Ok activeChannel -> return Ok activeChannel
    }

    static member internal AcceptReestablish (channelStore: ChannelStore)
                                    (transportListener: TransportListener)
                                    (channelId: ChannelIdentifier)
                                        : Async<Result<ActiveChannel, ReconnectActiveChannelError >> = async {
        let! connectRes =
            ConnectedChannel.AcceptFromWallet channelStore transportListener channelId
        match connectRes with
        | Error reconnectError -> return Error <| Reconnect reconnectError
        | Ok connectedChannel ->
            let! activeChannelRes = ActiveChannel.ConfirmFundingLocked connectedChannel
            match activeChannelRes with
            | Error lockFundingError -> return Error <| LockFunding lockFundingError
            | Ok activeChannel -> return Ok activeChannel
    }

    member internal self.Balance
        with get(): LNMoney =
            self.ConnectedChannel.Channel.Balance()

    member internal self.SpendableBalance
        with get(): LNMoney =
            self.ConnectedChannel.Channel.SpendableBalance()

    member self.ChannelId
        with get(): ChannelIdentifier = self.ConnectedChannel.ChannelId

    member private self.SendCommit(): Async<Result<ActiveChannel, SendCommitError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let ourCommitmentSignedMsgAndChannelRes = channel.Channel.SignCommitment ()
        let channelAfterCommitmentSigned, ourCommitmentSignedMsg = UnwrapResult ourCommitmentSignedMsgAndChannelRes "error executing sign commit command"

        let! peerNodeAfterCommitmentSignedSent = peerNode.SendMsg ourCommitmentSignedMsg

        let rec recv (peerNode: PeerNode) =
            async {
                let! recvChannelMsgRes = peerNode.RecvChannelMsg()
                match recvChannelMsgRes with
                | Error (RecvMsg recvMsgError) -> return Error <| RecvRevokeAndAck recvMsgError
                | Error (ReceivedPeerErrorMessage (peerNodeAfterRevokeAndAckReceived, errorMessage)) ->
                    let connectedChannelAfterError = 
                        {
                            connectedChannel with
                                PeerNode = peerNodeAfterRevokeAndAckReceived
                                Channel = 
                                    {
                                        Channel = channelAfterCommitmentSigned
                                    }
                        }
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| CommitmentSignedPeerErrorResponse
                        (brokenChannel, errorMessage)
                | Ok (peerNodeAfterRevokeAndAckReceived, channelMsg) ->
                    match channelMsg with
                    | :? RevokeAndACKMsg as theirRevokeAndAckMsg ->
                        let channelAferRevokeAndAckRes =
                            channelAfterCommitmentSigned.ApplyRevokeAndACK theirRevokeAndAckMsg

                        match channelAferRevokeAndAckRes with
                        | Error err ->
                            let connectedChannelAfterError = 
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterRevokeAndAckReceived
                                        Channel = 
                                            {
                                                Channel = channelAfterCommitmentSigned
                                            }
                                }
                            let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                            return Error <| InvalidRevokeAndAck
                                (brokenChannel, err)
                        | Ok channelAferRevokeAndAck ->
                            let connectedChannelAfterRevokeAndAck = 
                                {
                                    connectedChannel with
                                        PeerNode = peerNodeAfterRevokeAndAckReceived
                                        Channel = 
                                            {
                                                Channel = channelAferRevokeAndAck
                                            }
                                }
                            let channelPrivKeys = connectedChannelAfterRevokeAndAck.Channel.ChannelPrivKeys
                            let savedChannelState = self.ConnectedChannel.Channel.Channel.SavedChannelState
                            let network = connectedChannelAfterRevokeAndAck.Network
                            let account = connectedChannelAfterRevokeAndAck.Account

                            let perCommitmentSecret = theirRevokeAndAckMsg.PerCommitmentSecret

                            let breachStore = BreachDataStore account
                            let! breachData =
                                    breachStore
                                        .LoadBreachData(connectedChannelAfterRevokeAndAck.ChannelId)
                                        .InsertRevokedCommitment perCommitmentSecret
                                                                    savedChannelState
                                                                    channelPrivKeys
                                                                    network
                                                                    account
                            breachStore.SaveBreachData breachData

                            do! TowerClient.Default.CreateAndSendPunishmentTx perCommitmentSecret
                                        savedChannelState
                                        channelPrivKeys
                                        network
                                        account
                                        true

                            connectedChannelAfterRevokeAndAck.SaveToWallet()
                            let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                            return Ok activeChannel
                    | :? FundingLockedMsg as _fundingLockedMsg ->
                        return! recv(peerNodeAfterRevokeAndAckReceived);
                    | _ -> return Error <| ExpectedRevokeAndAck channelMsg
            }

        return! recv(peerNodeAfterCommitmentSignedSent);
    }

    member private self.RecvCommit(): Async<Result<ActiveChannel, RecvCommitError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) -> return Error <| RecvCommitmentSigned recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterCommitmentSignedReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterCommitmentSignedReceived
                    Channel = channel
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| PeerErrorMessageInsteadOfCommitmentSigned
                (brokenChannel, errorMessage)
        | Ok (peerNodeAfterCommitmentSignedReceived, channelMsg) ->
            match channelMsg with
            | :? CommitmentSignedMsg as theirCommitmentSignedMsg ->
                let ourRevokeAndAckMsgAndChannelRes=
                    channel.Channel.ApplyCommitmentSigned theirCommitmentSignedMsg
                match ourRevokeAndAckMsgAndChannelRes with
                | Error err ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = peerNodeAfterCommitmentSignedReceived
                            Channel = channel
                    }
                    let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                    return Error <| InvalidCommitmentSigned
                        (brokenChannel, err)
                | Ok (channelAfterCommitmentSigned, ourRevokeAndAckMsg) ->
                    let! peerNodeAfterRevokeAndAckSent = peerNodeAfterCommitmentSignedReceived.SendMsg ourRevokeAndAckMsg

                    let connectedChannelAfterRevokeAndAck = 
                        {
                            connectedChannel with
                                PeerNode = peerNodeAfterRevokeAndAckSent
                                Channel = 
                                    {
                                        Channel = channelAfterCommitmentSigned
                                    }
                        }
                    connectedChannelAfterRevokeAndAck.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                    return Ok activeChannel
            | _ -> return Error <| ExpectedCommitmentSigned channelMsg
    }

    member private self.RecvHtlcFulfill(): Async<Result<ActiveChannel, SendMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg _recvMsgError) -> return failwith "not implemetend"
        | Error (ReceivedPeerErrorMessage (peerNodeAfterHtlcFulfillReceived, _errorMessage)) ->
            let _connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterHtlcFulfillReceived
                    Channel = channel
            }
            return failwith "not implemetend"
        | Ok (peerNodeAfterHtlcFulfillReceived, channelMsg) ->
            match channelMsg with
            | :? UpdateFulfillHTLCMsg as theirFulfillMsg ->
                let channelAfterFulfillMsgRes =
                    channel.Channel.ApplyUpdateFulfillHTLC theirFulfillMsg
                match channelAfterFulfillMsgRes with
                | Error err ->
                    return failwith (SPrintF1 "not implemetend %s" err.Message)
                | Ok channelAfterFulfillMsg ->
                    let connectedChannelAfterFulfillMsg = {
                        connectedChannel with
                            PeerNode = peerNodeAfterHtlcFulfillReceived
                            Channel = 
                                {
                                    Channel = channelAfterFulfillMsg
                                }
                    }
                    connectedChannelAfterFulfillMsg.SaveToWallet()

                    let activeChannel = { ConnectedChannel = connectedChannelAfterFulfillMsg }
                    let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
                    match activeChannelAfterCommitReceivedRes with
                    | Error _err -> return failwith "not implemetend"
                    | Ok activeChannelAfterCommitReceived ->
                        let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
                        match activeChannelAfterCommitSentRes with
                        | Error _err -> return failwith "not implemetend"
                        | Ok activeChannelAfterCommitSent -> return Ok activeChannelAfterCommitSent
            | _ -> return failwith "not implemetend"
    }

    member internal self.SendMonoHopUnidirectionalPayment (amount: LNMoney)
                                                     : Async<Result<ActiveChannel, SendMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let monoHopUnidirectionalPaymentMsgAndChannelRes =
            channel.Channel.MonoHopUnidirectionalPayment amount
        match monoHopUnidirectionalPaymentMsgAndChannelRes with
        | Error (InvalidMonoHopUnidirectionalPayment err) ->
            return Error <| SendMonoHopPaymentError.InvalidMonoHopPayment (self, err)
        | Error err -> return failwith <| SPrintF1 "error executing mono hop payment command: %s" err.Message
        | Ok (channelAfterMonoHopPayment, monoHopUnidirectionalPaymentMsg) ->
            let! peerNodeAfterMonoHopPaymentSent = peerNode.SendMsg monoHopUnidirectionalPaymentMsg
            let connectedChannelAfterMonoHopPaymentSent = 
                {
                    connectedChannel with
                        PeerNode = peerNodeAfterMonoHopPaymentSent
                        Channel = 
                            {
                                Channel = channelAfterMonoHopPayment
                            }
                }
            connectedChannelAfterMonoHopPaymentSent.SaveToWallet()
            let activeChannel = { ConnectedChannel = connectedChannelAfterMonoHopPaymentSent }
            let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| SendMonoHopPaymentError.SendCommit err
            | Ok activeChannelAfterCommitSent ->
                let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                match activeChannelAfterCommitReceivedRes with
                | Error err -> return Error <| SendMonoHopPaymentError.RecvCommit err
                | Ok activeChannelAfterCommitReceived -> return Ok activeChannelAfterCommitReceived
    }

    member internal self.SendHtlcPayment (amount: LNMoney)
                                         (paymentPreimage: uint256)
                                         (paymentSecretOpt: uint256 option)
                                         (associatedData: byte[])
                                         (outgoingCLTV: BlockHeightOffset32)
                                                     : Async<Result<ActiveChannel, Exception>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel
        let currency = (connectedChannel.Account :> IAccount).Currency

        let sessionKey = new NBitcoin.Key()

        let! blockHeight = async {
            let! blockHeightResponse =
                Server.Query currency
                    (QuerySettings.Default ServerSelectionMode.Fast)
                    (ElectrumClient.SubscribeHeaders ())
                    None
            return
                (blockHeightResponse.Height |> uint32) + outgoingCLTV.Value
        }

        let tlvs =
            match paymentSecretOpt with
            | Some paymentSecret ->
                [| HopPayloadTLV.AmountToForward amount; HopPayloadTLV.OutgoingCLTV blockHeight; HopPayloadTLV.PaymentData(PaymentSecret.Create(paymentSecret), amount) |]
            | None ->
                [| HopPayloadTLV.AmountToForward amount; HopPayloadTLV.OutgoingCLTV blockHeight |]

        let realm0Data = TLVPayload(tlvs).ToBytes()
        let onionPacket = Sphinx.PacketAndSecrets.Create (sessionKey, [channel.RemoteNodeId.Value], [realm0Data], associatedData, Sphinx.PacketFiller.DeterministicPacketFiller)

        let channelAndAddHtlcMsgRes= 
            channel.Channel.AddHTLC { OperationAddHTLC.Amount = amount; PaymentHash = (PaymentHash.PaymentHash paymentPreimage); Expiry = BlockHeight blockHeight; Onion = onionPacket.Packet; Upstream = None; Origin = None; CurrentHeight = BlockHeight.Zero }

        match channelAndAddHtlcMsgRes with
        | Error err -> return failwith <| SPrintF1 "error executing htlc payment command: %s" err.Message
        | Ok (channelAfterAddHtlcPayment, addHtlcMsg) ->
            let! peerNodeAfterAddHtlcSent = peerNode.SendMsg addHtlcMsg
            let connectedChannelAfterAddHtlcSent = {
                connectedChannel with
                    PeerNode = peerNodeAfterAddHtlcSent
                    Channel = 
                        {
                            Channel = channelAfterAddHtlcPayment
                        }
            }
            connectedChannelAfterAddHtlcSent.SaveToWallet()
            let activeChannel = { ConnectedChannel = connectedChannelAfterAddHtlcSent }
            let! activeChannelAfterCommitSentRes = activeChannel.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error _err -> return failwith "not implemented"
            | Ok activeChannelAfterCommitSent ->
                let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                match activeChannelAfterCommitReceivedRes with
                | Error _err -> return failwith "not implemented"
                | Ok activeChannelAfterCommitReceived ->
                    let! activeChannelAfterNewCommitRes = activeChannelAfterCommitReceived.RecvHtlcFulfill()
                    match (activeChannelAfterNewCommitRes) with
                    |Error _err -> return failwith "not implemented"
                    | Ok activeChannelAfterNewCommit ->
                        return Ok (activeChannelAfterNewCommit)
    }

    member internal self.RecvMonoHopUnidirectionalPayment (monoHopUnidirectionalPaymentMsg: MonoHopUnidirectionalPaymentMsg)
                                                              : Async<Result<ActiveChannel, RecvMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel

        let channelAfterMonoHopPaymentReceivedRes =
            channelWrapper.Channel.ApplyMonoHopUnidirectionalPayment
                monoHopUnidirectionalPaymentMsg

        match channelAfterMonoHopPaymentReceivedRes with
        | Error err ->
            let connectedChannelAfterErrorReceived =
                { connectedChannel with
                    Channel = channelWrapper
                }
            let! connectedChannelAfterError = connectedChannelAfterErrorReceived.SendError err.Message
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| InvalidMonoHopPayment (brokenChannel, err)
        | Ok channelAfterMonoHopPaymentReceived ->
            let connectedChannelAfterMonoHopPaymentReceived =
                {
                    connectedChannel with
                        Channel =
                            {
                                Channel = channelAfterMonoHopPaymentReceived
                            }
                }

            connectedChannelAfterMonoHopPaymentReceived.SaveToWallet()
            let activeChannel = { ConnectedChannel = connectedChannelAfterMonoHopPaymentReceived }
            let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
            match activeChannelAfterCommitReceivedRes with
            | Error err -> return Error <| RecvMonoHopPaymentError.RecvCommit err
            | Ok activeChannelAfterCommitReceived ->
            let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| RecvMonoHopPaymentError.SendCommit err
            | Ok activeChannelAfterCommitSent -> return Ok activeChannelAfterCommitSent
    }

    member internal self.AcceptUpdateFee (): Async<Result<ActiveChannel, AcceptUpdateFeeError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel
        let peerNode = connectedChannel.PeerNode

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) ->
            return Error <| AcceptUpdateFeeError.RecvUpdateFee recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterUpdateFeeReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterUpdateFeeReceived
                    Channel = channelWrapper
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| AcceptUpdateFeeError.PeerErrorMessageInsteadOfUpdateFee
                (brokenChannel, errorMessage)
        | Ok (peerNodeAfterAcceptUpdateFeeReceived, channelMsg) ->
            match channelMsg with
            | :? UpdateFeeMsg as updateFeeMsg ->
                let channelWrapperAfterUpdateFeeReceivedRes =
                    channelWrapper.Channel.ApplyUpdateFee updateFeeMsg

                match channelWrapperAfterUpdateFeeReceivedRes with
                | Error err ->
                    let connectedChannelAfterUpdateFeeReceived = {
                        connectedChannel with
                            Channel = channelWrapper
                            PeerNode = peerNodeAfterAcceptUpdateFeeReceived
                    }
                    let! connectedChannelAfterError = connectedChannelAfterUpdateFeeReceived.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| AcceptUpdateFeeError.InvalidUpdateFee
                        (brokenChannel, err)
                | Ok channelWrapperAfterUpdateFeeReceived ->
                    let connectedChannelAfterUpdateFeeReceived = 
                        {
                            connectedChannel with
                                Channel = 
                                    {
                                        Channel = channelWrapperAfterUpdateFeeReceived
                                    }
                                PeerNode = peerNodeAfterAcceptUpdateFeeReceived
                        }
                    connectedChannelAfterUpdateFeeReceived.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterUpdateFeeReceived }
                    let! activeChannelAfterCommitReceivedRes = activeChannel.RecvCommit()
                    match activeChannelAfterCommitReceivedRes with
                    | Error err -> return Error <| AcceptUpdateFeeError.RecvCommit err
                    | Ok activeChannelAfterCommitReceived ->
                        let! activeChannelAfterCommitSentRes = activeChannelAfterCommitReceived.SendCommit()
                        match activeChannelAfterCommitSentRes with
                        | Error err -> return Error <| AcceptUpdateFeeError.SendCommit err
                        | Ok activeChannelAfterCommitSent -> return Ok activeChannelAfterCommitSent
            | _ -> return Error <| AcceptUpdateFeeError.ExpectedUpdateFee channelMsg
    }

    member internal self.UpdateFee (newFeeRate: FeeRatePerKw)
                                       : Async<Result<ActiveChannel, UpdateFeeError>> = async {
        let connectedChannel = self.ConnectedChannel
        let channelWrapper = connectedChannel.Channel
        let channelWrapperAndMsgAfterUpdateFeeRes =
            {
                OperationUpdateFee.FeeRatePerKw = newFeeRate
            }
            |> channelWrapper.Channel.UpdateFee

        match channelWrapperAndMsgAfterUpdateFeeRes with
        | Error (WeCannotAffordFee(localChannelReserve, requiredFee, missingAmount)) ->
            Infrastructure.LogDebug
            <| SPrintF4
                "cannot afford to update channel fee to %s: \
                    local channel reserve == %s, \
                    required fee == %s, \
                    missing amount == %s"
                (newFeeRate.ToString())
                (localChannelReserve.ToString())
                (requiredFee.ToString())
                (missingAmount.ToString())
            let activeChannelAfterUpdateFeeAttempt = {
                self with
                    ConnectedChannel = connectedChannel
            }
            return Ok activeChannelAfterUpdateFeeAttempt
        | Error err ->
            return
                SPrintF1 "error executing update fee command: %s" err.Message
                |> failwith
        | Ok (channelWrapperAfterUpdateFee, updateFeeMsg) ->
            let connectedChannelAfterUpdateFee = {
                connectedChannel with
                    Channel = 
                        {
                            Channel = channelWrapperAfterUpdateFee
                        }
            }
            let peerNode = connectedChannelAfterUpdateFee.PeerNode
            let! peerNodeAfterUpdateFeeMsgSent =
                peerNode.SendMsg updateFeeMsg
            let connectedChannelAfterUpdateFeeMsgSent = {
                connectedChannelAfterUpdateFee with
                    PeerNode = peerNodeAfterUpdateFeeMsgSent
            }
            connectedChannelAfterUpdateFeeMsgSent.SaveToWallet()
            let activeChannelAfterUpdateFeeMsgSent = {
                self with
                    ConnectedChannel = connectedChannelAfterUpdateFeeMsgSent
            }
            let! activeChannelAfterCommitSentRes = activeChannelAfterUpdateFeeMsgSent.SendCommit()
            match activeChannelAfterCommitSentRes with
            | Error err -> return Error <| UpdateFeeError.SendCommit err
            | Ok activeChannelAfterCommitSent ->
                let! activeChannelAfterCommitReceivedRes = activeChannelAfterCommitSent.RecvCommit()
                match activeChannelAfterCommitReceivedRes with
                | Error err -> return Error <| UpdateFeeError.RecvCommit err
                | Ok activeChannelAfterCommitReceived -> return Ok activeChannelAfterCommitReceived
    }

    member internal self.Commitments(): Commitments =
        self.ConnectedChannel.Channel.Channel.Commitments

