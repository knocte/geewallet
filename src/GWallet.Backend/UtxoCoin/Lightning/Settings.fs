namespace GWallet.Backend.UtxoCoin.Lightning

open System

open NBitcoin
open DotNetLightning.Utils
open DotNetLightning.Serialization
open DotNetLightning.Chain
open DotNetLightning.Channel

open GWallet.Backend

module Settings =

    let Currencies = [| Currency.BTC; Currency.LTC |] :> seq<Currency>

    let internal ConfigDirName = "LN"

    let internal PeerLimits: ChannelHandshakeLimits = {
        ForceChannelAnnouncementPreference = false
        MinFundingSatoshis = Money 100L
        MaxHTLCMinimumMSat = LNMoney 100000L
        MinMaxHTLCValueInFlightMSat = LNMoney 10000L
        MaxChannelReserveSatoshis = Money 100000L
        MinMaxAcceptedHTLCs = 1us
        MinDustLimitSatoshis = Money 200L // Value used by lnd
        MaxDustLimitSatoshis = Money 10000000L
        // TODO make optional in DotNetLightning
        MaxMinimumDepth = BlockHeightOffset32 UInt32.MaxValue
        MaxClosingNegotiationIterations = 10
    }

    let private SupportedFeatures =
        let featureBits = FeatureBits.Zero
        featureBits.SetFeature Feature.OptionDataLossProtect FeaturesSupport.Optional true
        featureBits

    let internal GetLocalParams (funding: Money)
                                (defaultFinalScriptPubKey: Script)
                                (isFunder: bool)
                                (remoteNodeId: NodeId)
                                (channelPrivKeys: ChannelPrivKeys)
                                    : LocalParams =
        {
            NodeId = remoteNodeId
            ChannelPubKeys = channelPrivKeys.ToChannelPubKeys()
            DustLimitSatoshis = Money 200UL
            MaxHTLCValueInFlightMSat = LNMoney 10000L
            ChannelReserveSatoshis = funding / 100L
            HTLCMinimumMSat = LNMoney 1000L
            ToSelfDelay = BlockHeightOffset16 6us
            MaxAcceptedHTLCs = uint16 10
            IsFunder = isFunder
            DefaultFinalScriptPubKey = defaultFinalScriptPubKey
            Features = SupportedFeatures
        }
