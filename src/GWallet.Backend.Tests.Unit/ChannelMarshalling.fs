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
              "acKed": [],
              "all": []
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
                "fundingPubKey": "032b39457df58efc0213983200ecbc00e0d4b33c02b7da01399d68bb72623106fb",
                "revocationBasePubKey": "037b61f5a47b938380351ea81d5c276e296d1d59d52eae3a7bdf0b8d9ea94b7ea5",
                "paymentBasePubKey": "03c5533a33fbc9e823f41f7406aaae09b2df834af4a92112b9823669a88fbb993f",
                "delayedPaymentBasePubKey": "021298412b55d119375dd01b225da6937f45de7dd5f6f34e5f72c2b8e77f53cc38",
                "htlcBasePubKey": "0369e1187de0e4b833a385997de900b31ba5ead0326c055f8e6a2ec480145f71e0",
                "commitmentSeed": "bb58d7d9528d716119a29bae146a42a96e0e55b6ba2e53bfca5de0a12f051424"
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
              "remotePerCommitmentPoint": "02fac9f29f746835957c0e0b5869a41af20ab9f3e2521096385d67aba99c8590f3"
            },
            "remoteNextCommitInfo": {
              "case": "Revoked",
              "fields": [
                "038e3d78afbb59129d67a1ef3aad13278c3a493423c3baa60257f8a362eadffd37"
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
              "paymentBasePoint": "02fea40401439678a564073c0257d20e664c07326608dfcbc941244a9af88e797b",
              "fundingPubKey": "024a348b198d2e05bd5e9eb8b8289e075fbaea351118efc8f0b61068110cc9ced6",
              "revocationBasePoint": "031667797aeba9dd667d9d8075bac89ee1839242e4184e134c819e033693a7a9f4",
              "delayedPaymentBasePoint": "03fb3934d023a81e36e7cdef2113e2d38051c476433c5cfbc71a4995a2b1ce9b82",
              "htlcBasePoint": "0366e8c287c8f8a4ea91b826a813d7913393dadce6cded3c0a4a24051cdd3c6962",
              "features": "000000101010001010100001",
              "minimumDepth": {
                "case": "BlockHeightOffset32",
                "fields": [
                  1
                ]
              }
            },
            "remotePerCommitmentSecrets": {
              "knownHashes": {},
              "lastIndex": null
            }
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
                SerializedChannel.LightningSerializerSettings
            )
        let reserializedChannelJson =
            Marshalling.SerializeCustom (
                serializedChannel,
                SerializedChannel.LightningSerializerSettings,
                Marshalling.DefaultFormatting
            )
        Assert.That(reserializedChannelJson |> MarshallingData.Sanitize,
                    Is.EqualTo (serializedChannelJson.Trim() |> MarshallingData.Sanitize))

