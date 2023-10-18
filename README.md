# PlutusV2 Hello World

This is a bare bones PlutusV2 smart-contract template. The goal is to provide the minimum expression of a PlutusV2 project to be used as starting point to build more complex contracts. **Demostrating one of the new features of PlutusV2** ([Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)).

## Manual Installation of Dependencies

### Cardano Haskell Dependencies

1. Clone the `libsodium` repository and build it:

```sh
git clone https://github.com/input-output-hk/libsodium
cd libsodium
git checkout 66f017f1
./autogen.sh
./configure
make
sudo make install
```

```sh
export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH"
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:$PKG_CONFIG_PATH"
```

```sh
git clone https://github.com/bitcoin-core/secp256k1
cd secp256k1
git checkout ac83be33
./autogen.sh
./configure --enable-module-schnorrsig --enable-experimental
make
make check
sudo make install
```

```sh
git clone https://github.com/supranational/blst
cd blst
./build.sh
sudo cp libblst.a /usr/local/lib/
```

```sh
echo 'prefix=/usr/local/lib
libdir=${prefix}
includedir=${prefix}

Name: libblst
Description: The BLST library
Version: 0.3.11  # adjust this to the correct version
Libs: -L${libdir} -lblst
Cflags: -I${includedir}' | sudo tee /usr/local/lib/pkgconfig/libblst.pc
```

```sh
sudo mv /path/to/blst/bindings/blst.h /usr/local/include/
sudo mv /path/to/blst/bindings/blst_aux.h /usr/local/include/
```
```sh
sudo apt-get install libpq-dev
```

## Dev Environment

If you don't want to install the required components yourself and setup a fully synchronized `cardano-node`, you can use [Demeter.run](https://demeter.run) platform to create a cloud environment with access to common Cardano infrastrcuture. The following command will open this repo in a private, web-based VSCode IDE with all of the required Haskell toolchain, access to a fully synchronized shared Cardano Node and a pre-installed binary of the `cardano-cli`.

[![Code in Cardano Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/ADAPhilippines/plutus-v2-starter-kit.git&template=plutus)

## Quick Start

> **Note**
> This guide assumes that you're using a Cardano Workspace as detailed above. 

### Compile the Validator

The source code for the Plutus contract lives in the `src/Hello` folder. The `Contracts.hs` contains a minimalistic validator logic and the required boilerplate code to serialize the Plutus-Tx code as UPLC that can be submitted on-chain.

The entry point for the Cabal project lives in `Main.hs` and can be used to trigger the serialization. Run the following command from the workspace terminal:

```sh
cabal run plutus-starter-kit -- assets/alwaysTrueV2.plutus
```

> **Note**
> The _Cardano Workspace_ provides a cached version of the Cardano api dependencies. This greatly accelerates the build process.

When the command finishes, you should get a `assets/alwaysTrueV2.plutus` file that contains a JSON envelope of the UPLC code. This file can be used to submit transactions on-chain.

```json
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "5907c05907bd0100003232323232323232323..."
}
```

# Testnet Demo via cardano-cli

## Setup

First we make sure our `protocol-parameters` are updated by executing the following command:

```sh
cardano-cli query protocol-parameters --testnet-magic ${CARDANO_NODE_MAGIC} > ./assets/pp.json
```

To construct on-chain transactions, we'll need the address of the  `plutus` script we've just compiled. For this, run the following command:

```sh
cardano-cli address build \
  --payment-script-file ./assets/alwaysTrueV2.plutus \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file ./assets/alwaysTrueV2.addr
```

> **Note**
> The `CARDANO_NODE_MAGIC` env variable is set automatically by the Cardano Workspace.

Next we will need to generate a `payment` key-pair that will serve as our `Cardano Wallet Key`:

```sh
cardano-cli address key-gen \
--verification-key-file ./assets/payment.vkey \
--signing-key-file ./assets/payment.skey
```

Using the key-pair we will then generate our wallet address:

```sh
cardano-cli address build \
--payment-verification-key-file ./assets/payment.vkey \
--out-file ./assets/payment.addr \
--testnet-magic ${CARDANO_NODE_MAGIC}
```

We can view the actual wallet address value with the following command:

```sh
cat ./assets/payment.addr
> addr_test1vp0l8elw4c5zr224869vvw2qldwpekym72q529nj4gzlhfgmaan79
```

Assuming you just generated the `payment` keys and wallet address as instructed above, then there should be no balance available.

> **Note**
> If you have an existing `payment` key-pair feel free to use that instead.

We can check the `balance` or `utxos` inside the wallet address with the following command:

```sh
cardano-cli query utxo --address $(cat ./assets/payment.addr) --testnet-magic ${CARDANO_NODE_MAGIC}

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```
You can obtain some `test ADA` or `tADA` via the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet).

> **Note**
> If you are using `--testnet-magic ${CARDANO_NODE_MAGIC}` then you should choose `preview` network in the faucet options.

Once you have requested some funds via the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet). then querying your wallet address `utxos` should looke like this:


```sh
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------

5a6b938debc6b2c9bc425c7fa35b59fc50e1e1654fa97f009b21dc6742925332     1        10000000000 lovelace + TxOutDatumNone
```

## Testing the AlwaysTrue Plutus contract

To test the contract we can try to send `5 ADA` to the contract address and try to unlock it and send it back to your wallet address.

We can send `ADA` to the contract with the following command:

```sh
# Build the transaction
cardano-cli transaction build --babbage-era --testnet-magic ${CARDANO_NODE_MAGIC} \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/alwaysTrueV2.addr)+5000000 \
--tx-out-datum-hash-file ./assets/myDatum.json \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw
```
> **Note**
> Make sure you put the proper `TxHash` and `TxIndex` with your available wallet `utxos`.

Since the contract will always allow any asset to be unlocked from it and ignores whatever `datum` and `redeemer` you pass into it. We attach a arbitrary `datum` to the transaction `--tx-out-datum-hash-file ./assets/myDatum.json`

```json
{
	"constructor": 0,
	"fields": [{
		"int": 42
	}]
}
```

Next we sign and submit the transaction:

```sh
# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic ${CARDANO_NODE_MAGIC} --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file ./assets/tx.signed 
```

We can query the `utxos` of the contract address:

```sh
cardano-cli query utxo --address $(cat ./assets/alwaysTrueV2.addr) --testnet-magic ${CARDANO_NODE_MAGIC}

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9b70ea82b9fad0a811372637a0753c94d62e1c69c9006b5e1fccc6e471a760db     1        5000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "fcaa61fb85676101d9e3398a484674e71c45c3fd41b492682f3b0054f4cf3273"
```

We should be able to see the `5 ADA` we just sent to the contract.

> **Note**
> It might take 20 seconds or more for the transaction to propagate throughout the network depending on the network health, so you will have to be patient.

### Reference Scripts

One important feature that the recent [Cardano Vasil Hard Fork]() has enabled is something called [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033). It essentially allows transactions to reference a plutus script rather than requiring it to be attached to every transaction, which was the case with `PlutusV1`. Ultimately saving room for more transaction size and fees to allocate elsewhere if needed.

The following command will upload a `plutus` script to the `cardano` blockchain and use it as a reference script.

```sh
# Build Tx wih a plutus reference script attached to it
cardano-cli transaction build --babbage-era --testnet-magic ${CARDANO_NODE_MAGIC} \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/payment.addr)+15000000 \
--tx-out-reference-script-file ./assets/alwaysTrueV2.plutus \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw

# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic ${CARDANO_NODE_MAGIC} --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file ./assets/tx.signed
```

> **Note**
> Make sure you put the proper `TxHash` and `TxIndex` with your available wallet `utxos`.

Before interacting with `Cardano Smart-Contracts`, a user needs to have a `utxo` with a minimum amount of `ADA` that can be used as [collateral](https://docs.cardano.org/plutus/collateral-mechanism).  (e.g `5 ADA`)

> **Note**
> Although the **Vasil** hardfork has improved the concept of collateral in cardano, We will not cover it in this document so we will use the (old) `Alonzo era` usage of collateral.

```sh
# Send 5 ADA to yourself to be used as collateral input
cardano-cli transaction build --babbage-era --testnet-magic ${CARDANO_NODE_MAGIC} \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/payment.addr)+50000000 \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw

# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic ${CARDANO_NODE_MAGIC} --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file ./assets/tx.signed 

```

Finally we can execute the script logic to **unlock** the `5 ADA` that we have **locked** previously at the contract address.

```sh
# Unlock the 5 ADA from the contract address and send it to your wallet address
cardano-cli transaction build --babbage-era --testnet-magic ${CARDANO_NODE_MAGIC} \
--tx-in LockedUTXOTxHash#LockedUTXOTxIndex \
--tx-in-collateral CollateralTxHash#CollateralTxIndex \
--spending-tx-in-reference RefScriptTxHash#RefScriptTxIndex \
--spending-plutus-script-v2 \
--spending-reference-tx-in-datum-file ./assets/myDatum.json \
--spending-reference-tx-in-redeemer-file ./assets/myDatum.json \
--change-address $(cat ./assets/payment.addr) \
--protocol-params-file ./assets/pp.json \
--out-file ./assets/tx.raw

# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic ${CARDANO_NODE_MAGIC} --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file ./assets/tx.signed 
```

> **Note**
> We did not have to attach a `.plutus` script in this transaction and instead used a reference of the `.plutus` script we wanted to execute!

> **Breakdown**
> 
> - `LockedUTXOTxHash#LockedUTXOTxIndex` is the UTXO of the `ADA` you locked at the script address.
> - `CollateralTxHash#CollateralTxIndex` is the UTXO of the `5 ADA` we sent to our wallet address that we will use as collateral.
> - `RefScriptTxHash#RefScriptTxIndex` is the UTXO of the [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) we uploaded above to be used as the `validation logic` for this transaction.

Congratulations 🎊🎊🎊, you have compiled and interacted with a `Cardano on-chain PlutusV2 script` using the [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) feature!
