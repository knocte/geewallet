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
    "channelIndex": 852287589,
    "network": "RegTest",
    "chanState": {
      "case": "Normal",
      "fields": [
        {
          "shortChannelId": {
            "blockHeight": {
              "case": "BlockHeight",
              "fields": [
                109
              ]
            },
            "blockIndex": {
              "case": "TxIndexInBlock",
              "fields": [
                1
              ]
            },
            "txOutIndex": {
              "case": "TxOutIndex",
              "fields": [
                0
              ]
            },
            "asString": "109x1x0"
          },
          "channelAnnouncement": null,
          "channelUpdate": {
            "signature@": {
              "case": "LNECDSASignature",
              "fields": [
                "304402202b13d24a766dbdb566f90e7612459a669dab78040a080542211f9f527808ba1602201a97447e4ed06bc00342b996d44b57a60ea0ad90e0ce9e4617e4d06ccb7837a9"
              ]
            },
            "contents@": {
              "chainHash@": "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206",
              "shortChannelId@": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    109
                  ]
                },
                "blockIndex": {
                  "case": "TxIndexInBlock",
                  "fields": [
                    1
                  ]
                },
                "txOutIndex": {
                  "case": "TxOutIndex",
                  "fields": [
                    0
                  ]
                },
                "asString": "109x1x0"
              },
              "timestamp@": 1623757059,
              "messageFlags@": 0,
              "channelFlags@": 3,
              "cltvExpiryDelta@": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat@": {
                "case": "LNMoney",
                "fields": [
                  1000
                ]
              },
              "feeBaseMSat@": {
                "case": "LNMoney",
                "fields": [
                  4570000
                ]
              },
              "feeProportionalMillionths@": 100,
              "htlcMaximumMSat@": null,
              "chainHash": "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206",
              "shortChannelId": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    109
                  ]
                },
                "blockIndex": {
                  "case": "TxIndexInBlock",
                  "fields": [
                    1
                  ]
                },
                "txOutIndex": {
                  "case": "TxOutIndex",
                  "fields": [
                    0
                  ]
                },
                "asString": "109x1x0"
              },
              "timestamp": 1623757059,
              "messageFlags": 0,
              "channelFlags": 3,
              "cltvExpiryDelta": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat": {
                "case": "LNMoney",
                "fields": [
                  1000
                ]
              },
              "feeBaseMSat": {
                "case": "LNMoney",
                "fields": [
                  4570000
                ]
              },
              "feeProportionalMillionths": 100,
              "htlcMaximumMSat": null
            },
            "signature": {
              "case": "LNECDSASignature",
              "fields": [
                "304402202b13d24a766dbdb566f90e7612459a669dab78040a080542211f9f527808ba1602201a97447e4ed06bc00342b996d44b57a60ea0ad90e0ce9e4617e4d06ccb7837a9"
              ]
            },
            "contents": {
              "chainHash@": "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206",
              "shortChannelId@": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    109
                  ]
                },
                "blockIndex": {
                  "case": "TxIndexInBlock",
                  "fields": [
                    1
                  ]
                },
                "txOutIndex": {
                  "case": "TxOutIndex",
                  "fields": [
                    0
                  ]
                },
                "asString": "109x1x0"
              },
              "timestamp@": 1623757059,
              "messageFlags@": 0,
              "channelFlags@": 3,
              "cltvExpiryDelta@": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat@": {
                "case": "LNMoney",
                "fields": [
                  1000
                ]
              },
              "feeBaseMSat@": {
                "case": "LNMoney",
                "fields": [
                  4570000
                ]
              },
              "feeProportionalMillionths@": 100,
              "htlcMaximumMSat@": null,
              "chainHash": "0f9188f13cb7b2c71f2a335e3a4fc328bf5beb436012afca590b1a11466e2206",
              "shortChannelId": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    109
                  ]
                },
                "blockIndex": {
                  "case": "TxIndexInBlock",
                  "fields": [
                    1
                  ]
                },
                "txOutIndex": {
                  "case": "TxOutIndex",
                  "fields": [
                    0
                  ]
                },
                "asString": "109x1x0"
              },
              "timestamp": 1623757059,
              "messageFlags": 0,
              "channelFlags": 3,
              "cltvExpiryDelta": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat": {
                "case": "LNMoney",
                "fields": [
                  1000
                ]
              },
              "feeBaseMSat": {
                "case": "LNMoney",
                "fields": [
                  4570000
                ]
              },
              "feeProportionalMillionths": 100,
              "htlcMaximumMSat": null
            },
            "isNode1": false
          },
          "localShutdown": null,
          "remoteShutdown": null,
          "remoteNextCommitInfo": {
            "case": "Revoked",
            "fields": [
              {
                "case": "PerCommitmentPoint",
                "fields": [
                  "020bef9613fd67880087938da8580cca057a4ccfef1bd820321c4a108166815547"
                ]
              }
            ]
          }
        }
      ]
    },
    "commitments": {
      "isFunder": true,
      "channelFlags": 0,
      "fundingScriptCoin": {
        "isP2SH": false,
        "redeemType": 1,
        "redeem": "522102cab56300cd3322edd929fba7b490f26847c7ff2ab5346634435bd98d01ecde9f2103fe03f23b91099d1d32df3384706b71aac42910fd692cc85a3346331ea3293b3c52ae",
        "canGetScriptCode": true,
        "isMalleable": false,
        "outpoint": "cf85d68128b301ef64840fd8a341581b79538aa6e237b3e0114177f89ada6c7600000000",
        "txOut": {
          "scriptPubKey": "0020c189c076544f88f2219ff5dbb0f65023fe29f887b4c00171f41bb74880e0c403",
          "value": 10000000
        },
        "amount": 10000000,
        "scriptPubKey": "0020c189c076544f88f2219ff5dbb0f65023fe29f887b4c00171f41bb74880e0c403"
      },
      "localChanges": {
        "proposed": [],
        "signed": [],
        "acKed": []
      },
      "localCommit": {
        "index": 2,
        "spec": {
          "htlCs": {},
          "feeRatePerKw": {
            "case": "FeeRatePerKw",
            "fields": [
              5000
            ]
          },
          "toLocal": {
            "case": "LNMoney",
            "fields": [
              7500000000
            ]
          },
          "toRemote": {
            "case": "LNMoney",
            "fields": [
              2500000000
            ]
          },
          "totalFunds": {
            "case": "LNMoney",
            "fields": [
              10000000000
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
                "totalOut": 9996380,
                "lockTime": 541417295,
                "inputs": [
                  {
                    "sequence": 2149166308,
                    "prevOut": "cf85d68128b301ef64840fd8a341581b79538aa6e237b3e0114177f89ada6c7600000000",
                    "scriptSig": "",
                    "witScript": "040047304402203c780acb53905836d19606cce78e67ffbcd5f1a4f3d17054649f5dd4e11e1b8702207c179b446a101878767f545e79402ab0af6d87fb61fd9f00d048605e3d41d8e40147304402206630add1c3886d607faf41d88a3bcd19178277f98efb98518c1a846e13d62dd40220708cf8da3c11d94b7d0e9eb75ca6ec3f95e6e45995c860987ae8adc84ca26f9a0147522102cab56300cd3322edd929fba7b490f26847c7ff2ab5346634435bd98d01ecde9f2103fe03f23b91099d1d32df3384706b71aac42910fd692cc85a3346331ea3293b3c52ae",
                    "isFinal": false
                  }
                ],
                "outputs": [
                  {
                    "scriptPubKey": "0014d207066b9de8729cb0d0e69f803c8e34619dfb40",
                    "value": 2500000
                  },
                  {
                    "scriptPubKey": "0020dbfccb82c0b4c85a82adac7a871fd47ff175a96584e19d0f89b0115fb81c7b18",
                    "value": 7496380
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
      "localNextHTLCId": {
        "case": "HTLCId",
        "fields": [
          0
        ]
      },
      "localParams": {
        "dustLimitSatoshis": 200,
        "maxHTLCValueInFlightMSat": {
          "case": "LNMoney",
          "fields": [
            10000
          ]
        },
        "channelReserveSatoshis": 100000,
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
      "originChannels": {},
      "remoteChanges": {
        "proposed": [],
        "signed": [],
        "acKed": []
      },
      "remoteCommit": {
        "index": 2,
        "spec": {
          "htlCs": {},
          "feeRatePerKw": {
            "case": "FeeRatePerKw",
            "fields": [
              5000
            ]
          },
          "toLocal": {
            "case": "LNMoney",
            "fields": [
              2500000000
            ]
          },
          "toRemote": {
            "case": "LNMoney",
            "fields": [
              7500000000
            ]
          },
          "totalFunds": {
            "case": "LNMoney",
            "fields": [
              10000000000
            ]
          }
        },
        "txId": {
          "case": "TxId",
          "fields": [
            "5b8b203dd81e15a6ab767a7cdfdaac057ad3ca60128f94d719af928ffde1083f"
          ]
        },
        "remotePerCommitmentPoint": {
          "case": "PerCommitmentPoint",
          "fields": [
            "02018d56ec8b275d4e5ebc076bd50328a4013770b27a42ae10fd83f8beec7f69b5"
          ]
        }
      },
      "remoteNextHTLCId": {
        "case": "HTLCId",
        "fields": [
          0
        ]
      },
      "remoteParams": {
        "dustLimitSatoshis": 200,
        "maxHTLCValueInFlightMSat": {
          "case": "LNMoney",
          "fields": [
            10000
          ]
        },
        "channelReserveSatoshis": 100000,
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
        "features": "00000010"
      },
      "remotePerCommitmentSecrets": [
        {
          "item1": 1,
          "item2": {
            "case": "PerCommitmentSecret",
            "fields": [
              "7bc01e6f28f27a822c6e73cb4ee97d49777fe3a7f2cf5868d13d588ea59db16c"
            ]
          }
        }
      ],
      "remoteChannelPubKeys": {
        "fundingPubKey": {
          "case": "FundingPubKey",
          "fields": [
            "02cab56300cd3322edd929fba7b490f26847c7ff2ab5346634435bd98d01ecde9f"
          ]
        },
        "revocationBasepoint": {
          "case": "RevocationBasepoint",
          "fields": [
            "03511e03ff60e8e0b7ffffe1450d4049c74987431f2e8a40cc7b5422dc3ccd65aa"
          ]
        },
        "paymentBasepoint": {
          "case": "PaymentBasepoint",
          "fields": [
            "025fb5c8ac57ed542dd05f8be40c3dce4996b543a311a6bc76755496e5ec27992f"
          ]
        },
        "delayedPaymentBasepoint": {
          "case": "DelayedPaymentBasepoint",
          "fields": [
            "0393a242e5d8dfd254c60d7613c76017f5c79ddb54e33e82a36b4f0dcce2a17eb0"
          ]
        },
        "htlcBasepoint": {
          "case": "HtlcBasepoint",
          "fields": [
            "0212a15a4d7ce0778ec0ba2436422f63e420d4dcfe33636642c6cacba213e55718"
          ]
        }
      }
    },
    "accountFileName": "031e931acde055a4882df11d4e5d290b3ebdcfc498a5f658a27b2821f5f36f1786",
    "counterpartyIP": [
      "127.0.0.1",
      9735
    ],
    "remoteNodeId": {
      "case": "NodeId",
      "fields": [
        "02eb427355b87884f53761c7a610caaa00deeb4b514bb916319184d4eacd3612ec"
      ]
    },
    "localForceCloseSpendingTxOpt": {
      "case": "Some",
      "fields": [
        "02000000000101bc2d521554bf7febb4094d09d7c5913dc8fc11158d14d9cbf1a29912b497467101000000000600000001105a72000000000017a9141401bc234269bc28d9432dc6f5291ec372a9de7b870347304402201b2c59adbc661bf50e44cb5689f60ad3e54dbf38a36b8bd4c17e878db9646647022078655540ecc40aa3b0199079135241370ec05cb65163108239cb1dd410b3112a01004b632103935238f64173dc9e61f6e8edb26386d1ed426a70d492f3e3f31a55113c6840836756b2752103827a9ed27ba967f7fde822fea1c431ff0a9b80a83ac91e1271df810ea487112d68ac00000000"
      ]
    },
    "minSafeDepth": {
      "case": "BlockHeightOffset32",
      "fields": [
        1
      ]
    },
    "localChannelPubKeys": {
      "fundingPubKey": {
        "case": "FundingPubKey",
        "fields": [
          "03fe03f23b91099d1d32df3384706b71aac42910fd692cc85a3346331ea3293b3c"
        ]
      },
      "revocationBasepoint": {
        "case": "RevocationBasepoint",
        "fields": [
          "022573e69e4d76294421187293f3ea242336e2f7b4c7d1f418a1e6898bf3cc827a"
        ]
      },
      "paymentBasepoint": {
        "case": "PaymentBasepoint",
        "fields": [
          "03d47b6fb359ddfd43cb709ed31cfe45665c787f3951f231414d369851b46d2e02"
        ]
      },
      "delayedPaymentBasepoint": {
        "case": "DelayedPaymentBasepoint",
        "fields": [
          "0320b17c6b7e75660ac9849ee9d1cd1124731d621bc4e1ebe1dc06ef7dc3d20fa1"
        ]
      },
      "htlcBasepoint": {
        "case": "HtlcBasepoint",
        "fields": [
          "03afe95fe9ebb6e946996cc8341087ab04f382f0d79d95d318cbe33e3c1d372f34"
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

