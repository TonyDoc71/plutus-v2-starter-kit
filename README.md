# PlutusV2 Hello World

This is a bare bones PlutusV2 smart-contract template. The goal is to provide the minimum expression of a PlutusV2 project to be used as starting point to build more complex contracts. **Demostrating one of the new features in PlutusV2** ([Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)).

## Dev Environment

To build the script you'll need the Haskell toolchain (GCH, Cabal, etc) and several dependencies from IOHK repositories. There's also a requirement on the [secp256k1](https://github.com/bitcoin-core/secp256k1.git) library. Once you've compiled the source code into a Plutus script, you'll need a fully-synced Cardano Node and the `cardano-cli` binary in order to submit example transactions to the Blockchain.

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
cardano-cli query protocol-parameters --testnet-magic 2 > ./assets/pp.json
```

To construct on-chain transactions, we'll need the address of the  `plutus` script we've just compiled. For this, run the following command:

```sh
cardano-cli address build \
  --payment-script-file ./assets/alwaysTrueV2.plutus \
  --testnet-magic 2 \
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
--testnet-magic 2
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
cardano-cli query utxo --address $(cat ./assets/payment.addr) --testnet-magic 2

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```
You can obtain some `test ADA` or `tADA` via the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet).

> **Note**
> If you are using `--testnet-magic 2` then you should choose `preview` network in the faucet options.

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
cardano-cli transaction build --babbage-era --testnet-magic 2 \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/typedAlwaysSucceeds.addr)+5000000 \
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
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic 2 --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic 2 --tx-file ./assets/tx.signed 
```

We can query the `utxos` of the contract address:

```sh
cardano-cli query utxo --address $(cat ./assets/typedAlwaysSucceeds.addr) --testnet-magic 2

                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
9b70ea82b9fad0a811372637a0753c94d62e1c69c9006b5e1fccc6e471a760db     1        5000000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "fcaa61fb85676101d9e3398a484674e71c45c3fd41b492682f3b0054f4cf3273"
```

We should be able to see the `5 ADA` we just sent to the contract.

> **Note**
> It might take 20 seconds or more for the transaction to propagate throughout the network depending on the network health, so you will have to be patient.

### Reference Scripts

One important feature that the recent [Cardano Vasil Hard Fork]() has enabled is something called [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033). It essentially allows transactions to reference a plutus script rather than requiring it to be attached to the transaction, which was the case with `PlutusV1`. Ultimately saving room for more transaction size and fees to allocate elsewhere if needed.

The following command will upload a `plutus` script to the `cardano` blockchain and use it as a reference script.

```sh
# Build Tx wih a plutus reference script attached to it
cardano-cli transaction build --babbage-era --testnet-magic 2 \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/payment.addr)+15000000 \
--tx-out-reference-script-file ./assets/alwaysTrueV2.plutus \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw

# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic 2 --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic 2 --tx-file ./assets/tx.signed
```

> **Note**
> Make sure you put the proper `TxHash` and `TxIndex` with your available wallet `utxos`.

Before interacting with `Cardano Smart-Contracts`, a user needs to have a `utxo` with a minimum amount of `ADA` that can be used as [collateral](https://docs.cardano.org/plutus/collateral-mechanism).  (e.g `5 ADA`)

> **Note**
> Although the **Vasil** hardfork has improved the concept of collateral in cardano, We will not cover it in this document so we will use the (old) `Alonzo era` usage of collateral.

```sh
# Send 5 ADA to yourself to be used as collateral input
cardano-cli transaction build --babbage-era --testnet-magic 2 \
--tx-in 21dc43bf8fe518c34aee4992b72350fa4e42436fa269cbbf995569663c4af254#0 \
--tx-out $(cat ./assets/payment.addr)+50000000 \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw

# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic 2 --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic 2 --tx-file ./assets/tx.signed 

```

Finally we can execute the script logic to **unlock** the `5 ADA` that we have **locked** previously at the contract address.

```sh
# Unlock the 5 ADA from the contract address and send it to your wallet address
cardano-cli transaction build --babbage-era --testnet-magic 2 \
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
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic 2 --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic 2 --tx-file ./assets/tx.signed 
```

> **Note**
> We did not have to attach a `.plutus` script in this transaction and instead used a reference of the `.plutus` script we wanted to execute!

> **Breakdown**
> 
> - `LockedUTXOTxHash#LockedUTXOTxIndex` is the UTXO of the `ADA` you locked at the script address.
> - `CollateralTxHash#CollateralTxIndex` is the UTXO of the `5 ADA` we sent to our wallet address that we will use as collateral.
> - `RefScriptTxHash#RefScriptTxIndex` is the UTXO of the [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) we uploaded above to be used as the `validation logic` for this transaction.

Congratulations 🎊🎊🎊, you have compiled and interacted with a `Cardano on-chain PlutusV2 script` using the [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033) feature!

# System Requirements

```sh
./scripts/contract-balance.sh
```

> **Note**
> Querying the node for UTxO is not part of the scope for this Starter Kit. If you want to learn more about common operations through the CLI, try out the [Cardano-CLI Starter Kit](https://github.com/txpipe/cardano-cli-starter-kit)

### Prepare a Dev Wallet

To interact with our contract on-chain, we'll need a wallet with some tAda (test ADA). Open your Cardano Workspace terminal and excute the following command:

```sh
./scripts/new-dev-wallet.sh
```

> **Note**
> Creating wallet keys is not part of the scope for this Starter Kit. If you want to learn more about common operations through the CLI, try out the [Cardano-CLI Starter Kit](https://github.com/txpipe/cardano-cli-starter-kit)

When done, you should see new files inside the `assets` directory that contain the key pairs and address for your newly created wallet.

If you want to query the balance of your wallet address, you can use a helper script that queries the UTxO for the address generated in the previous step. Open your Cardano Workspace terminal and run the following command:

```sh
./scripts/dev-wallet-balance.sh
```

> **Note**
> Querying the node for UTxO is not part of the scope for this Starter Kit. If you want to learn more about common operations through the CLI, try out the [Cardano-CLI Starter Kit](https://github.com/txpipe/cardano-cli-starter-kit)

Your wallet we'll need some funds to interact with the contract. You can use the corresponding [faucet](https://docs.cardano.org/cardano-testnet/tools/faucet) to obtain some.

### Lock Funds

Now that we have a validator script ready, we'll lock funds on the script address. Locking funds is just a fancy way of saying that we'll send some assests (ADA in this case) to the script by submitting a transaction to the corresponding address. It is called "locking" because the funds can only be retrieved if the validator script allows it.

Our very basic validator has one simple task: to ensure that the integer value in the datum is greater or equal than the integer value in the redeemer (quite dumb and useless).

Our datum is defined as a Haskell newtype that wraps a single integer. The `src/Hello/Contract.hs` contains the correponding code:

```haskell
newtype HelloDatum = HelloDatum Integer
```

When locking the funds, the submitting party is the one in control of the datum and needs to specify the hash of the value. For that, we need a JSON representation of the datum to be passed to the cardano-cli in order to obtain the hash. The file `assets/lock.datum` contains an example of the JSON representation for a datum that holds the value `42`:

```json
{"constructor":0,"fields":[{"int":42}]}
```

From inside your Cardano Workspace, open a terminal and execute the following command to generate a hash for data contained in the `assets/lock.datum` file. The result of the cardano-cli command will be stored in the `scriptdatumhash` variable.

```sh
scriptdatumhash=$(cardano-cli transaction hash-script-data --script-data-file assets/lock.datum)
```

The locking transaction needs a reference to a UTxO in your dev wallet to be used as source for the funds we'll be locking in the script. Since this step is specific to the state of your wallet, you'll need to manually assign the value in a shell variable for the next step to succeed.

Use the `dev-wallet-balance.sh` to check your available UTxO and select the one to be used in our test transaction. Assign the `{TxHash}#{TxIn}` to the `locktxin` shell variable. We'll be locking a small amount of tADA (1230000 lovelace), make sure that your UTxO has enough. The following is just an example, replace accordingly:

```sh
$ ./scripts/dev-wallet-balance.sh

                           TxHash                                 TxIx   Amount
---------------------------------------------------------------------------------
0939be18d8583bbdd7309b4cfefd419c8900df0f84142149066ec2755c94a322     0   9980637126 lovelace
9805cc2d7c08f8b99acd2d60d9cf1e3eb14b281e7f3f430f26a26f0927ff5fde     0   1060942 lovelace
9ec2a9a546d8a9c7221be452e26278d2128cb39429d57a58b420598c0e9c2591     0   1060678 lovelace

$ locktxin=0939be18d8583bbdd7309b4cfefd419c8900df0f84142149066ec2755c94a322#0
```

We also need to retrieve some Protocol Parameters before building the transaction, in order to do so, execute the following script helper:

```sh
$ ./scripts/download-params.sh
```

Now we are finally ready to build the locking transaction. From inside your Cardano Workspace, open a terminal and execute the following command to build the unsigned Tx payload. 

```sh
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --change-address $(cat assets/wallet1.addr) \
  --tx-in ${locktxin} \
  --tx-out $(cat assets/contract.addr)+1230000 \
  --tx-out-datum-hash ${scriptdatumhash} \
  --protocol-params-file assets/params.json \
  --out-file assets/lock.tx
```

> **Note**
> The `CARDANO_NODE_MAGIC` env variable is set automatically by the Cardano Workspace.

The next step consists of signing the transaction with our dev wallet key. From inside your Cardano Workspace, open a terminal and execute the following command:

```sh
cardano-cli transaction sign \
  --tx-body-file assets/lock.tx \
  --signing-key-file assets/wallet1.skey \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file assets/lock.tx-signed
```

The only remaining task is to submit the signed transaction on-chain. From inside your Cardano Workspace, open a terminal and execute the following command:

```sh
cardano-cli transaction submit \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --tx-file assets/lock.tx-signed
```

After a few seconds (might be longer depending on chain activity), the balance for the script address should show our locked funds. Check the UTxO of the script address using our helper script:

```sh
./scripts/contract-balance.sh
```

The output of the script should show something similar to the following:

```sh
 TxHash    TxIx        Amount
--------------------------------------------------------------------------------------
b00...313     1        1230000 lovelace + TxOutDatumHash ScriptDataInBabbageEra "923...4ec"
```

# Unlock Funds

```sh
cardano-cli transaction build \
  --babbage-era \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --tx-in ${lockedtxin} \
  --tx-in-script-file assets/contract.plutus \
  --tx-in-datum-file assets/lock.datum \
  --tx-in-redeemer-file assets/unlock.redeemer \
  --tx-in-collateral ${collateraltxin} \
  --change-address $(cat assets/wallet1.addr) \
  --protocol-params-file assets/params.json \
  --out-file assets/unlock.tx
```

```sh
cardano-cli transaction sign \
  --tx-body-file assets/unlock.tx \
  --signing-key-file assets/wallet1.skey \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file assets/unlock.tx-signed
```

```sh
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file assets/unlock.tx-signed
```
