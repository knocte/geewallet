namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialize
open DotNetLightning.Chain
open DotNetLightning.Channel

open GWallet.Backend
open GWallet.Backend.UtxoCoin

module Settings =

    // FIXME: this should return seq<> so that we can run Lightning on Litecoin too
    let Currency = Currency.BTC

    // Only used when accepting channels.
    // It is 'high', since low values are not accepted by alternate implementations.
    // TODO: test lower values
    let internal HandshakeConfig = { ChannelHandshakeConfig.MinimumDepth = BlockHeightOffset32 6u }

    let internal PeerLimits: ChannelHandshakeLimits = {
        ForceChannelAnnouncementPreference = false
        MinFundingSatoshis = Money 100L
        MaxHTLCMinimumMSat = LNMoney 100000L
        MinMaxHTLCValueInFlightMSat = LNMoney 10000L
        MaxChannelReserveSatoshis = Money 100000L
        MinMaxAcceptedHTLCs = 1us
        MinDustLimitSatoshis = Money 100L
        MaxDustLimitSatoshis = Money 10000000L
        MaxMinimumDepth = BlockHeightOffset32 UInt32.MaxValue // TODO make optional in DotNetLightning
        MaxClosingNegotiationIterations = 10
    }

    let internal FeatureBits =
        let featureBits = FeatureBit.Zero
        featureBits.SetFeature Feature.OptionDataLossProtect FeaturesSupport.Optional true
        featureBits

    let internal GetLocalParams (isFunder: bool)
                                (fundingAmount: Money)
                                (nodeIdForResponder: NodeId)
                                (account: NormalUtxoAccount)
                                (keyRepo: DefaultKeyRepository)
                                    : ChannelKeys * LocalParams =
        let channelKeys: ChannelKeys = (keyRepo :> IKeysRepository).GetChannelKeys false
        let channelPubkeys: ChannelPubKeys = channelKeys.ToChannelPubKeys()
        channelKeys, {
            Features = FeatureBits
            NodeId = nodeIdForResponder
            ChannelPubKeys = channelPubkeys
            DustLimitSatoshis = Money 5UL
            MaxHTLCValueInFlightMSat = LNMoney 5000L
            // BOLT #2 recommends a channel reserve of 1% of the channel capacity
            ChannelReserveSatoshis = fundingAmount / 100L
            HTLCMinimumMSat = LNMoney 1000L
            ToSelfDelay = BlockHeightOffset16 6us
            MaxAcceptedHTLCs = uint16 10
            IsFunder = isFunder
            DefaultFinalScriptPubKey = account |> ScriptManager.CreatePayoutScript
        }
