namespace GWallet.Backend.Tests.Unit

open NUnit.Framework

open GWallet.Backend
open GWallet.Backend.UtxoCoin.Lightning

[<TestFixture>]
type ChannelMarshalling () =
    let serializedChannelJson = """
{
  "version": "{version}",
  "typeName": "GWallet.Backend.UtxoCoin.Lightning.SerializedChannel",
  "value": {
    "channelIndex": 717406643,
    "savedChannelState": {
      "staticChannelConfig": {
        "announceChannel": true,
        "remoteNodeId": {
          "case": "NodeId",
          "fields": [
            "038ce8d371e6b5f1ae8f535b28fcd9cc731a70285be3b30fe9abf365a982dc6589"
          ]
        },
        "network": "RegTest",
        "fundingTxMinimumDepth": {
          "case": "BlockHeightOffset32",
          "fields": [
            1
          ]
        },
        "localStaticShutdownScriptPubKey": {
          "case": "Some",
          "fields": [
            "a9141b2a6e5e068c6529cdd42c9d28ab48d196cf98ee87"
          ]
        },
        "remoteStaticShutdownScriptPubKey": null,
        "isFunder": false,
        "fundingScriptCoin": {
          "isP2SH": false,
          "redeemType": 1,
          "redeem": "5221021e2e1320b6b3d0717734194cdf2631509b8e6e00c73313780406e4acc9c938e62102bd286dedeb5e59f3572b30e8b841e875273814e0ccc467b994966f43ea0948ab52ae",
          "canGetScriptCode": true,
          "isMalleable": false,
          "outpoint": "c61a372afe5f55a440caf187c3a9c93e130194b9299343a901c3372f18a4374900000000",
          "txOut": {
            "scriptPubKey": "0020e5c42f28b8f5a27ffe797f3f2e4540f2d9de5f9759ddb1c6725ae805a28cc804",
            "value": 200000
          },
          "amount": 200000,
          "scriptPubKey": "0020e5c42f28b8f5a27ffe797f3f2e4540f2d9de5f9759ddb1c6725ae805a28cc804"
        },
        "localParams": {
          "dustLimitSatoshis": 200,
          "maxHTLCValueInFlightMSat": {
            "case": "LNMoney",
            "fields": [
              10000
            ]
          },
          "channelReserveSatoshis": 2000,
          "htlcMinimumMSat": {
            "case": "LNMoney",
            "fields": [
              1000
            ]
          },
          "toSelfDelay": {
            "case": "BlockHeightOffset16",
            "fields": [
              6
            ]
          },
          "maxAcceptedHTLCs": 10,
          "features": "10"
        },
        "remoteParams": {
          "dustLimitSatoshis": 573,
          "maxHTLCValueInFlightMSat": {
            "case": "LNMoney",
            "fields": [
              198000000
            ]
          },
          "channelReserveSatoshis": 2000,
          "htlcMinimumMSat": {
            "case": "LNMoney",
            "fields": [
              1
            ]
          },
          "toSelfDelay": {
            "case": "BlockHeightOffset16",
            "fields": [
              144
            ]
          },
          "maxAcceptedHTLCs": 483,
          "features": "000000101010001010100001"
        },
        "remoteChannelPubKeys": {
          "fundingPubKey": {
            "case": "FundingPubKey",
            "fields": [
              "021e2e1320b6b3d0717734194cdf2631509b8e6e00c73313780406e4acc9c938e6"
            ]
          },
          "revocationBasepoint": {
            "case": "RevocationBasepoint",
            "fields": [
              "0332016d1802d9fe07e8002443182bb1e6df73972a90b0351216eecb3a9638a877"
            ]
          },
          "paymentBasepoint": {
            "case": "PaymentBasepoint",
            "fields": [
              "0265b9b908be167bdba8bda3e810813025f9f4718def24b08649eddd42ccc4eaa5"
            ]
          },
          "delayedPaymentBasepoint": {
            "case": "DelayedPaymentBasepoint",
            "fields": [
              "033f901b31be59347ef7f3e547450fdf91889886211e6bea0b8b43aec04466330d"
            ]
          },
          "htlcBasepoint": {
            "case": "HtlcBasepoint",
            "fields": [
              "024172921dc95fd529d4e5418e2d59c8037c1d7ff03c10d9c432bbefdff49bb741"
            ]
          }
        }
      },
      "remotePerCommitmentSecrets": [],
      "shortChannelId": null,
      "localCommit": {
        "index": 0,
        "spec": {
          "outgoingHTLCs": {},
          "incomingHTLCs": {},
          "feeRatePerKw": {
            "case": "FeeRatePerKw",
            "fields": [
              12500
            ]
          },
          "toLocal": {
            "case": "LNMoney",
            "fields": [
              0
            ]
          },
          "toRemote": {
            "case": "LNMoney",
            "fields": [
              200000000
            ]
          }
        },
        "publishableTxs": {
          "commitTx": {
            "case": "FinalizedTx",
            "fields": [
              {
                "rbf": true,
                "version": 2,
                "totalOut": 190950,
                "lockTime": 543761955,
                "inputs": [
                  {
                    "sequence": 2150416108,
                    "prevOut": "c61a372afe5f55a440caf187c3a9c93e130194b9299343a901c3372f18a4374900000000",
                    "scriptSig": "",
                    "witScript": "0400483045022100b6b2332989768b7533f2e7dc3ea9c35fdb929470f0026d240c6d82760325722e02206081cdfca3ff6ecb05c6f2e672a78d59618cce5407ca4e47421418e54962b1da01473044022023e249f5a68307003189af8e73a46ff33fe707ce66616c6f51326a064f446b730220148436749c081ee0906f61ca06c2cd8c3c664152fdbe63b965c9bccceb52477d01475221021e2e1320b6b3d0717734194cdf2631509b8e6e00c73313780406e4acc9c938e62102bd286dedeb5e59f3572b30e8b841e875273814e0ccc467b994966f43ea0948ab52ae",
                    "isFinal": false
                  }
                ],
                "outputs": [
                  {
                    "scriptPubKey": "0014faee217cfe0195a4fb7cdc248c136326d8e0447d",
                    "value": 190950
                  }
                ],
                "isCoinBase": false,
                "hasWitness": true
              }
            ]
          },
          "htlcTxs": []
        },
        "pendingHTLCSuccessTxs": []
      },
      "remoteCommit": {
        "index": 0,
        "spec": {
          "outgoingHTLCs": {},
          "incomingHTLCs": {},
          "feeRatePerKw": {
            "case": "FeeRatePerKw",
            "fields": [
              12500
            ]
          },
          "toLocal": {
            "case": "LNMoney",
            "fields": [
              200000000
            ]
          },
          "toRemote": {
            "case": "LNMoney",
            "fields": [
              0
            ]
          }
        },
        "txId": {
          "case": "TxId",
          "fields": [
            "49ab06ea7eb5fab6fc7f4a6963acee408fd9b5b81c26bd668a95cefd1130656c"
          ]
        },
        "remotePerCommitmentPoint": {
          "case": "PerCommitmentPoint",
          "fields": [
            "035b7460ae4a26fc7fb3cd43562becfd40a6b0b6467dd3747abc59a3cf4277c0da"
          ]
        }
      },
      "localChanges": {
        "signed": [],
        "acKed": []
      },
      "remoteChanges": {
        "signed": [],
        "acKed": []
      }
    },
    "commitments": {
      "proposedLocalChanges": [],
      "proposedRemoteChanges": [],
      "localNextHTLCId": {
        "case": "HTLCId",
        "fields": [
          0
        ]
      },
      "remoteNextHTLCId": {
        "case": "HTLCId",
        "fields": [
          0
        ]
      },
      "originChannels": {}
    },
    "remoteNextCommitInfo": null,
    "negotiatingState": {
      "localRequestedShutdown": null,
      "remoteRequestedShutdown": null,
      "localClosingFeesProposed": [],
      "remoteClosingFeeProposed": null
    },
    "accountFileName": "035aa42da8cd09dcc3ca767474e6a2a21248990157652a226869a6d5e460681b46",
    "counterpartyIP": [
      "127.0.0.1",
      1785
    ],
    "localForceCloseSpendingTxOpt": null,
    "localChannelPubKeys": {
      "fundingPubKey": {
        "case": "FundingPubKey",
        "fields": [
          "02bd286dedeb5e59f3572b30e8b841e875273814e0ccc467b994966f43ea0948ab"
        ]
      },
      "revocationBasepoint": {
        "case": "RevocationBasepoint",
        "fields": [
          "02ead9bddfe2db38a3123d5bc80500773b9e4bd295972c83729d9d406f509ec7ca"
        ]
      },
      "paymentBasepoint": {
        "case": "PaymentBasepoint",
        "fields": [
          "02c64fc214e0e7a3d35aa95767c9e0f53f23ac69f9ceed685158760243ddb1b931"
        ]
      },
      "delayedPaymentBasepoint": {
        "case": "DelayedPaymentBasepoint",
        "fields": [
          "02c1526b00f3920d0e085508e0cfa46e3f83f04254cf9ca073a877aff98ea3e890"
        ]
      },
      "htlcBasepoint": {
        "case": "HtlcBasepoint",
        "fields": [
          "0212577b5ee0b1e11d16d6c124ef90386009622760d17895660f96d214a4c39525"
        ]
      }
    }
  }
}
"""

    [<Test>]
    member __.``deserialized and reserialize without change``() =
        let serializedChannel = 
            Marshalling.DeserializeCustom<SerializedChannel> (
                serializedChannelJson,
                SerializedChannel.LightningSerializerSettings Currency.BTC
            )
        let reserializedChannelJson =
            Marshalling.SerializeCustom (
                serializedChannel,
                SerializedChannel.LightningSerializerSettings Currency.BTC,
                Marshalling.DefaultFormatting
            )
        Assert.That(reserializedChannelJson |> MarshallingData.Sanitize,
                    Is.EqualTo (serializedChannelJson.Trim() |> MarshallingData.Sanitize))

