namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Serialize.Msgs
open DotNetLightning.Channel
open DotNetLightning.Utils
open ResultUtils.Portability

open GWallet.Backend
open GWallet.Backend.FSharpUtil
open GWallet.Backend.FSharpUtil.UwpHacks

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
    member internal self.PossibleBug =
        match self with
        | RecvMonoHopPayment err -> err.PossibleBug
        | RecvCommit err -> err.PossibleBug
        | SendCommit err -> err.PossibleBug
        | PeerErrorMessageInsteadOfMonoHopPayment _
        | InvalidMonoHopPayment _
        | ExpectedMonoHopPayment _ -> false

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
        let ourFundingLockedMsgRes, channelAfterFundingConfirmed =
            let channelCmd =
                ChannelCommand.ApplyFundingConfirmedOnBC(
                    absoluteBlockHeight,
                    txIndexInBlock,
                    confirmationCount
                )
            channel.ExecuteCommand channelCmd <| function
                | (FundingConfirmed _)::(WeSentFundingLocked fundingLockedMsg)::[] ->
                    Some fundingLockedMsg
                | _ -> None
        let ourFundingLockedMsg = UnwrapResult ourFundingLockedMsgRes "DNL error creating funding_locked msg"
        let! peerNodeAfterFundingLockedSent = peerNode.SendMsg ourFundingLockedMsg
        let! theirFundingLockedMsgRes = async {
            match theirFundingLockedMsgOpt with
            | Some theirFundingLockedMsg -> return Ok (peerNodeAfterFundingLockedSent, theirFundingLockedMsg)
            | None ->
                let! recvChannelMsgRes = peerNodeAfterFundingLockedSent.RecvChannelMsg()
                match recvChannelMsgRes with
                | Error (RecvMsg recvMsgError) -> return Error <| RecvFundingLocked recvMsgError
                | Error (ReceivedPeerErrorMessage (peerNodeAfterFundingLockedReceived, errorMessage)) ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = peerNodeAfterFundingLockedReceived
                            Channel = channelAfterFundingConfirmed
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
            let res, channelAfterFundingLocked =
                let channelCmd = ApplyFundingLocked theirFundingLockedMsg
                channelAfterFundingConfirmed.ExecuteCommand channelCmd <| function
                    | (BothFundingLocked _)::[] -> Some ()
                    | _ -> None
            let connectedChannelAfterFundingLocked = {
                connectedChannel with
                    PeerNode = peerNodeAfterFundingLockedReceived
                    Channel = channelAfterFundingLocked
            }
            match res with
            | Error err ->
                let! connectedChannelAfterErrorSent = connectedChannelAfterFundingLocked.SendError err.Message
                let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                return Error <| InvalidFundingLocked
                    (brokenChannel, err)
            | Ok () ->
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
        match channel.Channel.State with
        | WaitForFundingConfirmed state ->
            let fundedChannel = {
                FundedChannel.ConnectedChannel = connectedChannel
                TheirFundingLockedMsgOpt = state.Deferred
            }
            return! ActiveChannel.CheckFundingConfirmed fundedChannel
        | WaitForFundingLocked _ ->
            let fundedChannel = {
                FundedChannel.ConnectedChannel = connectedChannel
                TheirFundingLockedMsgOpt = None
            }
            return! ActiveChannel.CheckFundingConfirmed fundedChannel
        | ChannelState.Normal _ ->
            let activeChannel = {
                ActiveChannel.ConnectedChannel = connectedChannel
            }
            return Ok activeChannel
        | _ ->
            return failwith <| SPrintF1 "unexpected channel state: %A" channel.Channel.State
    }

    static member internal ConnectReestablish (channelStore: ChannelStore)
                                              (transportListener: TransportListener)
                                              (channelId: ChannelIdentifier)
                                                  : Async<Result<ActiveChannel, ReconnectActiveChannelError>> = async {
        let! connectRes =
            ConnectedChannel.ConnectFromWallet channelStore transportListener channelId
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
            UnwrapOption
                (self.ConnectedChannel.Channel.Balance())
                "The ActiveChannel type is created by establishing a channel \
                and so guarantees that the underlying channel state has a balance"

    member internal self.SpendableBalance
        with get(): LNMoney =
            UnwrapOption
                (self.ConnectedChannel.Channel.SpendableBalance())
                "The ActiveChannel type is created by establishing a channel \
                and so guarantees that the underlying channel state has a balance"

    member self.ChannelId
        with get(): ChannelIdentifier = self.ConnectedChannel.ChannelId

    member private self.SendCommit(): Async<Result<ActiveChannel, SendCommitError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let ourCommitmentSignedMsgRes, channelAfterCommitmentSigned =
            let channelCmd = ChannelCommand.SignCommitment
            channel.ExecuteCommand channelCmd <| function
                | WeAcceptedOperationSign(msg, _)::[] -> Some msg
                | _ -> None
        let ourCommitmentSignedMsg = UnwrapResult ourCommitmentSignedMsgRes "error executing sign commit command"

        let! peerNodeAfterCommitmentSignedSent = peerNode.SendMsg ourCommitmentSignedMsg

        let! recvChannelMsgRes = peerNodeAfterCommitmentSignedSent.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) -> return Error <| RecvRevokeAndAck recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterRevokeAndAckReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterRevokeAndAckReceived
                    Channel = channelAfterCommitmentSigned
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| CommitmentSignedPeerErrorResponse
                (brokenChannel, errorMessage)
        | Ok (peerNodeAfterRevokeAndAckReceived, channelMsg) ->
            match channelMsg with
            | :? RevokeAndACKMsg as theirRevokeAndAckMsg ->
                let res, channelAferRevokeAndAck =
                    let channelCmd = ChannelCommand.ApplyRevokeAndACK theirRevokeAndAckMsg
                    channelAfterCommitmentSigned.ExecuteCommand channelCmd <| function
                        | WeAcceptedRevokeAndACK(_)::[] -> Some ()
                        | _ -> None
                let connectedChannelAfterRevokeAndAck = {
                    connectedChannel with
                        PeerNode = peerNodeAfterRevokeAndAckReceived
                        Channel = channelAferRevokeAndAck
                }
                match res with
                | Error err ->
                    let! connectedChannelAfterErrorSent = connectedChannelAfterRevokeAndAck.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                    return Error <| InvalidRevokeAndAck
                        (brokenChannel, err)
                | Ok () ->
                    connectedChannelAfterRevokeAndAck.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                    return Ok activeChannel
            | _ -> return Error <| ExpectedRevokeAndAck channelMsg
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
                let ourRevokeAndAckMsgRes, channelAfterCommitmentSigned =
                    let channelCmd = ChannelCommand.ApplyCommitmentSigned theirCommitmentSignedMsg
                    channel.ExecuteCommand channelCmd <| function
                        | WeAcceptedCommitmentSigned(msg, _)::[] -> Some msg
                        | _ -> None
                match ourRevokeAndAckMsgRes with
                | Error err ->
                    let connectedChannelAfterError = {
                        connectedChannel with
                            PeerNode = peerNodeAfterCommitmentSignedReceived
                            Channel = channelAfterCommitmentSigned
                    }
                    let! connectedChannelAfterErrorSent = connectedChannelAfterError.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterErrorSent }
                    return Error <| InvalidCommitmentSigned
                        (brokenChannel, err)
                | Ok ourRevokeAndAckMsg ->
                    let! peerNodeAfterRevokeAndAckSent = peerNodeAfterCommitmentSignedReceived.SendMsg ourRevokeAndAckMsg

                    let connectedChannelAfterRevokeAndAck = {
                        connectedChannel with
                            PeerNode = peerNodeAfterRevokeAndAckSent
                            Channel = channelAfterCommitmentSigned
                    }
                    connectedChannelAfterRevokeAndAck.SaveToWallet()
                    let activeChannel = { ConnectedChannel = connectedChannelAfterRevokeAndAck }
                    return Ok activeChannel
            | _ -> return Error <| ExpectedCommitmentSigned channelMsg
    }

    member internal self.SendMonoHopUnidirectionalPayment (amount: LNMoney)
                                                     : Async<Result<ActiveChannel, SendMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let monoHopUnidirectionalPaymentMsgRes, channelAfterMonoHopPayment =
            let channelCmd = ChannelCommand.MonoHopUnidirectionalPayment { Amount = amount }
            channel.ExecuteCommand channelCmd <| function
                | WeAcceptedOperationMonoHopUnidirectionalPayment(msg, _)::[] -> Some msg
                | _ -> None
        match monoHopUnidirectionalPaymentMsgRes with
        | Error (InvalidMonoHopUnidirectionalPayment err) ->
            return Error <| SendMonoHopPaymentError.InvalidMonoHopPayment (self, err)
        | Error err -> return failwith <| SPrintF1 "error executing mono hop payment command: %s" err.Message
        | Ok monoHopUnidirectionalPaymentMsg ->
            let! peerNodeAfterMonoHopPaymentSent = peerNode.SendMsg monoHopUnidirectionalPaymentMsg
            let connectedChannelAfterMonoHopPaymentSent = {
                connectedChannel with
                    PeerNode = peerNodeAfterMonoHopPaymentSent
                    Channel = channelAfterMonoHopPayment
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

    member internal self.RecvMonoHopUnidirectionalPayment()
                             : Async<Result<ActiveChannel, RecvMonoHopPaymentError>> = async {
        let connectedChannel = self.ConnectedChannel
        let peerNode = connectedChannel.PeerNode
        let channel = connectedChannel.Channel

        let! recvChannelMsgRes = peerNode.RecvChannelMsg()
        match recvChannelMsgRes with
        | Error (RecvMsg recvMsgError) -> return Error <| RecvMonoHopPayment recvMsgError
        | Error (ReceivedPeerErrorMessage (peerNodeAfterMonoHopPaymentReceived, errorMessage)) ->
            let connectedChannelAfterError = {
                connectedChannel with
                    PeerNode = peerNodeAfterMonoHopPaymentReceived
                    Channel = channel
            }
            let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
            return Error <| PeerErrorMessageInsteadOfMonoHopPayment
                (brokenChannel, errorMessage)
        | Ok (peerNodeAfterMonoHopPaymentReceived, channelMsg) ->
            match channelMsg with
            | :? MonoHopUnidirectionalPaymentMsg as monoHopUnidirectionalPaymentMsg ->
                let res, channelAfterMonoHopPaymentReceived =
                    let channelCmd =
                        ChannelCommand.ApplyMonoHopUnidirectionalPayment
                            monoHopUnidirectionalPaymentMsg
                    channel.ExecuteCommand channelCmd <| function
                        | WeAcceptedMonoHopUnidirectionalPayment(_)::[] -> Some ()
                        | _ -> None
                let connectedChannelAfterMonoHopPaymentReceived = {
                    connectedChannel with
                        PeerNode = peerNodeAfterMonoHopPaymentReceived
                        Channel = channelAfterMonoHopPaymentReceived
                }
                match res with
                | Error err ->
                    let! connectedChannelAfterError = connectedChannelAfterMonoHopPaymentReceived.SendError err.Message
                    let brokenChannel = { BrokenChannel.ConnectedChannel = connectedChannelAfterError }
                    return Error <| InvalidMonoHopPayment
                        (brokenChannel, err)
                | Ok () ->
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
            | _ -> return Error <| ExpectedMonoHopPayment channelMsg
    }

