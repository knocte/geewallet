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
    "channelIndex": 1023375857,
    "network": "RegTest",
    "chanState": {
      "case": "Normal",
      "fields": [
        {
          "commitments": {
            "channelId": "323435afc15f197be3c3678495d20ea8eac7ce0029724b44cf962b96edbccad5",
            "channelFlags": 1,
            "fundingScriptCoin": {
              "isP2SH": false,
              "redeemType": 1,
              "redeem": "5221024a348b198d2e05bd5e9eb8b8289e075fbaea351118efc8f0b61068110cc9ced621032b39457df58efc0213983200ecbc00e0d4b33c02b7da01399d68bb72623106fb52ae",
              "canGetScriptCode": true,
              "isMalleable": false,
              "outpoint": "d5cabced962b96cf444b722900cec7eaa80ed2958467c3e37b195fc1af35343200000000",
              "txOut": {
                "scriptPubKey": "002034057552c46edc435d8566250edcbe68ddde8cbda740beb428ea87b438073e3d",
                "value": 200000
              },
              "amount": 200000,
              "scriptPubKey": "002034057552c46edc435d8566250edcbe68ddde8cbda740beb428ea87b438073e3d"
            },
            "localChanges": {
              "proposed": [],
              "signed": [],
              "acKed": []
            },
            "localCommit": {
              "index": 0,
              "spec": {
                "htlCs": {},
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
                },
                "totalFunds": {
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
                      "lockTime": 546280525,
                      "inputs": [
                        {
                          "sequence": 2162099758,
                          "prevOut": "d5cabced962b96cf444b722900cec7eaa80ed2958467c3e37b195fc1af35343200000000",
                          "scriptSig": "",
                          "witScript": "040048304502210089c015a95f2f2c865b9b5d561884885e171be791bfc08fc52559e96b57cf184502206ff644ceefc3f1735e203448452415bd95199861db169b0b8666f98102b834920147304402200f1a7cf52c4e32766bc9d6ad28cae0a5bf7bb303c8640bdd4dc8577c3c1cd49402204155cee2281b69400c6408513948a9da51494a949ed2b6760e11c08cab93221601475221024a348b198d2e05bd5e9eb8b8289e075fbaea351118efc8f0b61068110cc9ced621032b39457df58efc0213983200ecbc00e0d4b33c02b7da01399d68bb72623106fb52ae",
                          "isFinal": false
                        }
                      ],
                      "outputs": [
                        {
                          "scriptPubKey": "0014f689a058241c8d1a8984dd52df6e94ddeccbf6a7",
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
            "localNextHTLCId": {
              "case": "HTLCId",
              "fields": [
                0
              ]
            },
            "localParams": {
              "nodeId": {
                "case": "NodeId",
                "fields": [
                  "024f384dbd20b5b44c1301874714065086e721b07afaa3d725149712ad1ef69712"
                ]
              },
              "channelPubKeys": {
                "fundingPubKey": {
                  "case": "FundingPubKey",
                  "fields": [
                    "02d142c964689b27b72ad31d555cf0c2d9efc22b43d2ab97b47f2938a4fbe8240f"
                  ]
                },
                "revocationBasepoint": {
                  "case": "RevocationBasepoint",
                  "fields": [
                    "03ea44f75d42caf09d87d833bb8412d67f37407d54d1a8c37199b883e69d5a9b67"
                  ]
                },
                "paymentBasepoint": {
                  "case": "PaymentBasepoint",
                  "fields": [
                    "02393d5176dfe01383caaab8abcf54d45b38d4bf533e7063efcbda2d26b94f700e"
                  ]
                },
                "delayedPaymentBasepoint": {
                  "case": "DelayedPaymentBasepoint",
                  "fields": [
                    "028b989ff5ff7f0659590a765efbe044909409c2b75e59995cea183d038a8f8363"
                  ]
                },
                "htlcBasepoint": {
                  "case": "HtlcBasepoint",
                  "fields": [
                    "0348822d6f7c2f5b267e9ad62d32e7ed9ea630c5db0039061e77fcb84e0b349bda"
                  ]
                }
              },
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
              "isFunder": false,
              "defaultFinalScriptPubKey": "a914b7ce6a47799d7d050f7b0095763b8aeef85b2f3e87",
              "features": "10"
            },
            "originChannels": {},
            "remoteChanges": {
              "proposed": [],
              "signed": [],
              "acKed": []
            },
            "remoteCommit": {
              "index": 0,
              "spec": {
                "htlCs": {},
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
                },
                "totalFunds": {
                  "case": "LNMoney",
                  "fields": [
                    200000000
                  ]
                }
              },
              "txId": {
                "case": "TxId",
                "fields": [
                  "c328f5b1001fd70add57d308e490e3b8713b559c64b03506c2aa6d1ad28d6c98"
                ]
              },
              "remotePerCommitmentPoint": {
                "case": "PerCommitmentPoint",
                "fields": [
                  "026555cbbf8d5e950b84cec5e5e27cb2a8588ee20d4f8f0659ced4cf9d1f63231b"
                ]
              }
            },
            "remoteNextCommitInfo": {
              "case": "Revoked",
              "fields": [
                {
                  "case": "PerCommitmentPoint",
                  "fields": [
                    "03c5fbfbd6f151ddae01210235a6d80b461b7939d666abfbc2be26564c32445f2a"
                  ]
                }
              ]
            },
            "remoteNextHTLCId": {
              "case": "HTLCId",
              "fields": [
                0
              ]
            },
            "remoteParams": {
              "nodeId": {
                "case": "NodeId",
                "fields": [
                  "024f384dbd20b5b44c1301874714065086e721b07afaa3d725149712ad1ef69712"
                ]
              },
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
              "channelPubKeys": {
                "fundingPubKey": {
                  "case": "FundingPubKey",
                  "fields": [
                    "03edc42fa55be840139ea6b94f977d428479b843a788f7ff0e51ca2551a0bad34c"
                  ]
                },
                "revocationBasepoint": {
                  "case": "RevocationBasepoint",
                  "fields": [
                    "0218a4f28bb48a3763dda09e30a46c4ceb3c91c9120d961b22ad045776e5962a21"
                  ]
                },
                "paymentBasepoint": {
                  "case": "PaymentBasepoint",
                  "fields": [
                    "03df21726bcfb682ced8584c21b7873f7213be524c5b68e8271c8e65bc66cf2367"
                  ]
                },
                "delayedPaymentBasepoint": {
                  "case": "DelayedPaymentBasepoint",
                  "fields": [
                    "02b192e4d2139df2a9301fb4e609bc043200dc24ed49b7082a19bca8e605444445"
                  ]
                },
                "htlcBasepoint": {
                  "case": "HtlcBasepoint",
                  "fields": [
                    "03b7029bf119bc3274af84c436db9c116d8821176479c556749fb3c78f345e936a"
                  ]
                }
              },
              "features": "000000101010001010100001",
              "minimumDepth": {
                "case": "BlockHeightOffset32",
                "fields": [
                  1
                ]
              }
            },
            "remotePerCommitmentSecrets": []
          },
          "shortChannelId": {
            "blockHeight": {
              "case": "BlockHeight",
              "fields": [
                102
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
            "asString": "102x1x0"
          },
          "buried": true,
          "channelAnnouncement": null,
          "channelUpdate": {
            "signature@": {
              "case": "LNECDSASignature",
              "fields": [
                "3044022040ea32c595e22f8b10e3fbf6d1c6e3d9ef927c41c7b80359afea8d9eb734a42b02206eb427c2ee2e671e1d6f6810a9d8f914de90026f1a26d89704b4f9e916f57111"
              ]
            },
            "contents@": {
              "chainHash@": "06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f",
              "shortChannelId@": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    102
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
                "asString": "102x1x0"
              },
              "timestamp@": 1612850390,
              "messageFlags@": 0,
              "channelFlags@": 2,
              "cltvExpiryDelta@": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat@": {
                "case": "LNMoney",
                "fields": [
                  1
                ]
              },
              "feeBaseMSat@": {
                "case": "LNMoney",
                "fields": [
                  9274000
                ]
              },
              "feeProportionalMillionths@": 100,
              "htlcMaximumMSat@": null,
              "chainHash": "06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f",
              "shortChannelId": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    102
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
                "asString": "102x1x0"
              },
              "timestamp": 1612850390,
              "messageFlags": 0,
              "channelFlags": 2,
              "cltvExpiryDelta": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat": {
                "case": "LNMoney",
                "fields": [
                  1
                ]
              },
              "feeBaseMSat": {
                "case": "LNMoney",
                "fields": [
                  9274000
                ]
              },
              "feeProportionalMillionths": 100,
              "htlcMaximumMSat": null
            },
            "signature": {
              "case": "LNECDSASignature",
              "fields": [
                "3044022040ea32c595e22f8b10e3fbf6d1c6e3d9ef927c41c7b80359afea8d9eb734a42b02206eb427c2ee2e671e1d6f6810a9d8f914de90026f1a26d89704b4f9e916f57111"
              ]
            },
            "contents": {
              "chainHash@": "06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f",
              "shortChannelId@": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    102
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
                "asString": "102x1x0"
              },
              "timestamp@": 1612850390,
              "messageFlags@": 0,
              "channelFlags@": 2,
              "cltvExpiryDelta@": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat@": {
                "case": "LNMoney",
                "fields": [
                  1
                ]
              },
              "feeBaseMSat@": {
                "case": "LNMoney",
                "fields": [
                  9274000
                ]
              },
              "feeProportionalMillionths@": 100,
              "htlcMaximumMSat@": null,
              "chainHash": "06226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f",
              "shortChannelId": {
                "blockHeight": {
                  "case": "BlockHeight",
                  "fields": [
                    102
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
                "asString": "102x1x0"
              },
              "timestamp": 1612850390,
              "messageFlags": 0,
              "channelFlags": 2,
              "cltvExpiryDelta": {
                "case": "BlockHeightOffset16",
                "fields": [
                  6
                ]
              },
              "htlcMinimumMSat": {
                "case": "LNMoney",
                "fields": [
                  1
                ]
              },
              "feeBaseMSat": {
                "case": "LNMoney",
                "fields": [
                  9274000
                ]
              },
              "feeProportionalMillionths": 100,
              "htlcMaximumMSat": null
            },
            "isNode1": true
          },
          "localShutdown": null,
          "remoteShutdown": null,
          "channelId": {
            "case": "ChannelId",
            "fields": [
              "323435afc15f197be3c3678495d20ea8eac7ce0029724b44cf962b96edbccad5"
            ]
          }
        }
      ]
    },
    "accountFileName": "03df5dd272af2dc4e9cd1b944a804762b26df7f73a8b961416152d724d7bc6a2ab",
    "counterpartyIP": [
      "127.0.0.1",
      59938
    ],
    "remoteNodeId": {
      "case": "NodeId",
      "fields": [
        "024f384dbd20b5b44c1301874714065086e721b07afaa3d725149712ad1ef69712"
      ]
    },
    "minSafeDepth": {
      "case": "BlockHeightOffset32",
      "fields": [
        1
      ]
    },
    "isFunder": false,
    "channelId": "323435afc15f197be3c3678495d20ea8eac7ce0029724b44cf962b96edbccad5"
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

