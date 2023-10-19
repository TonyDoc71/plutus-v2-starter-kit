# PlutusV2 Hello World

This is a bare bones PlutusV2 smart-contract template. The goal is to provide the minimum expression of a PlutusV2 project to be used as a starting point to build more complex contracts. **Demonstrating one of the new features of PlutusV2**, [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033), which allow for more efficient transaction validation by referencing scripts on-chain.

## Manual Installation of Dependencies

Before diving into the smart contract, ensure you have the necessary dependencies installed. Here's a guide on how to manually install the required Cardano and Haskell dependencies:

### Cardano Haskell Dependencies

1. **Libsodium** - a modern, easy-to-use software library for encryption, decryption, signatures, password hashing, and more.
   
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

Bitcoin Core Secp256k1 - a library for optimized ECDSA signature verification, a crucial component for blockchain applications.

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

Supranational BLST - a library for BLS Signatures, a cryptographic primitive used for validating transactions on many blockchain platforms.

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

Each of these libraries is crucial for working with Cardano smart contracts and the Haskell programming environment. Ensure that you have properly installed and configured them before proceeding.

## Development Environment

Setting up a local environment can be quite tedious. If you prefer a quicker setup or are not keen on managing dependencies manually, consider utilizing the [Demeter.run](https://demeter.run) platform. This platform provides a cloud-based environment equipped with the necessary Cardano infrastructure and the Haskell toolchain. It grants you access to a fully synchronized shared Cardano Node and a pre-installed binary of the `cardano-cli`. Here's how to get started:

1. Click on the link below to launch a private, web-based VSCode IDE that is pre-configured with the required Haskell toolchain and a synchronized shared Cardano Node:
   
   [![Code in Cardano Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/ADAPhilippines/plutus-v2-starter-kit.git&template=plutus)

2. This will open the repository in a VSCode instance within your browser, ready for you to explore, modify, and run the PlutusV2 smart contract template.

> **Note**: 
> - The provided link will direct you to a version-controlled workspace allowing you to jump-start your PlutusV2 project effortlessly.
> - Ensure your browser allows pop-ups, as the IDE will open in a new tab or window.

This cloud-based setup significantly simplifies the initial setup process, enabling you to focus more on building and testing your smart contracts on the Cardano blockchain.

## Quick Start

This section will guide you through the process of compiling the validator script for a minimalistic Plutus smart contract. It's assumed that you are using a Cardano Workspace as detailed in the [Development Environment](#development-environment) section.

### Compile the Validator

The source code for the Plutus contract resides in the `src/Hello` folder. Inside, the `Contracts.hs` file contains a minimalistic validator logic along with the required boilerplate code to serialize the Plutus-Tx code into UPLC format, suitable for on-chain submission.

To compile the validator script, use the following command from the workspace terminal. This command triggers the serialization process defined in `Main.hs` and generates a file named `assets/alwaysTrueV2.plutus`:

```sh
cabal run plutus-starter-kit -- assets/alwaysTrueV2.plutus
```

> Note:
>
> The assets/alwaysTrueV2.plutus file contains a JSON envelope of the UPLC code which is ready for on-chain submission.
The Cardano Workspace provides cached versions of the Cardano API dependencies, which significantly accelerates the build process.

Once the command executes successfully, you'll find the generated assets/alwaysTrueV2.plutus file with content resembling the following:

```json
{
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "5907c05907bd0100003232323232323232323..."
}
```
This file is now ready for use in on-chain transactions. The subsequent sections will guide you through interacting with the Cardano blockchain using this compiled Plutus script.

# Testnet Demo via cardano-cli

This section outlines the steps to interact with a Plutus smart contract on the Cardano testnet using the `cardano-cli`. It's essential to have your `protocol-parameters` updated to construct on-chain transactions. 

## Setup

1. **Update Protocol Parameters**:
   Execute the following command to ensure your `protocol-parameters` are updated:

```sh
cardano-cli query protocol-parameters --testnet-magic ${CARDANO_NODE_MAGIC} > ./assets/pp.json
```

2. Generate Script Address:
- To interact with the smart contract, generate the address of the plutus script using the following command:

```sh
cardano-cli address build \
  --payment-script-file ./assets/alwaysTrueV2.plutus \
  --testnet-magic ${CARDANO_NODE_MAGIC} \
  --out-file ./assets/alwaysTrueV2.addr
```

> Note:
> 
> The CARDANO_NODE_MAGIC environment variable is automatically set by the Cardano Workspace.
Ensure the alwaysTrueV2.plutus file has been successfully generated as outlined in the Quick Start section.

3. Generate Payment Key-Pair:
- Create a payment key-pair that will serve as your Cardano Wallet Key using the following command:

```sh
cardano-cli address key-gen \
--verification-key-file ./assets/payment.vkey \
--signing-key-file ./assets/payment.skey
```

4. Generate Wallet Address:
- With the key-pair generated, create your wallet address:

```sh
cardano-cli address build \
--payment-verification-key-file ./assets/payment.vkey \
--out-file ./assets/payment.addr \
--testnet-magic ${CARDANO_NODE_MAGIC}
```

To view the actual wallet address value, use the following command:

```sh
cat ./assets/payment.addr
> addr_test1vp0l8elw4c5zr224869vvw2qldwpekym72q529nj4gzlhfgmaan79
```

5. Check Wallet Balance:
- Initially, your wallet balance will be zero. To check the balance or utxos inside the wallet address, use the following command:

```sh
cardano-cli query utxo --address $(cat ./assets/payment.addr) --testnet-magic ${CARDANO_NODE_MAGIC}
```

6. Obtain Test ADA:
- Acquire some test ADA (tADA) from the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnet/tools/faucet).

> Note:
> 
> If using --testnet-magic ${CARDANO_NODE_MAGIC}, select the preview network in the faucet options.
Once you've obtained test ADA, querying your wallet address utxos should display your updated balance.

The above steps prepare your environment for interacting with the smart contract on the Cardano testnet. The next section, Testing the AlwaysTrue Plutus contract, will guide you through sending ADA to the contract address and attempting to unlock it.

## Testing the AlwaysTrue Plutus contract

This section demonstrates how to send ADA to the contract address, attempt to unlock it, and send it back to your wallet address. 

1. **Sending ADA to the Contract**:
- Build the transaction to send `5 ADA` to the contract address:
   
```sh
cardano-cli transaction build --babbage-era --testnet-magic ${CARDANO_NODE_MAGIC} \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/alwaysTrueV2.addr)+5000000 \
--tx-out-datum-hash-file ./assets/myDatum.json \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw
```
> Note:
> 
> Replace TxHash and TxIndex with your available wallet utxos.
Since the contract always unlocks any asset and ignores the datum and redeemer values, an arbitrary datum is attached to the transaction using --tx-out-datum-hash-file ./assets/myDatum.json.

```json
{
	"constructor": 0,
	"fields": [{
		"int": 42
	}]
}
```

2. Sign and Submit the Transaction:
  - Sign the transaction with your payment signing key and submit it to the Cardano Network:

```sh
# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic ${CARDANO_NODE_MAGIC} --out-file ./assets/tx.signed

# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file ./assets/tx.signed
```

3. Query Contract UTXOs:
  - Check the utxos of the contract address to see the 5 ADA you sent:

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

The [Cardano Vasil Hard Fork](link-to-vasil-hard-fork-details) introduced the concept of [Reference Scripts](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033), allowing transactions to reference a Plutus script rather than requiring the script to be attached to every transaction. This innovation optimizes transaction size and fee allocation. This section demonstrates how to upload a Plutus script to the Cardano blockchain and use it as a reference script.

1. **Uploading Plutus Script**:
  - Build a transaction with a Plutus reference script attached to it using the following command:

```sh
# Build Tx with a Plutus reference script attached to it
cardano-cli transaction build --babbage-era --testnet-magic ${CARDANO_NODE_MAGIC} \
--tx-in TxHash#TxIndex \
--tx-out $(cat ./assets/payment.addr)+15000000 \
--tx-out-reference-script-file ./assets/alwaysTrueV2.plutus \
--change-address $(cat ./assets/payment.addr) \
--out-file ./assets/tx.raw
```

> Note:
>
> Replace TxHash and TxIndex with your available wallet utxos.
>
> You are sending this ADA to yourself, so you don't lose the ADA. You are just attaching the Plutus script to the output.

1. Signing and Submitting the Transaction:
  - Sign the transaction with your payment signing key and submit it to the Cardano Network:

```sh
# Sign with your payment signing key
cardano-cli transaction sign --tx-body-file ./assets/tx.raw --signing-key-file ./assets/payment.skey --testnet-magic ${CARDANO_NODE_MAGIC} --out-file ./assets/tx.signed
# Submit the transaction to the Cardano Network
cardano-cli transaction submit --testnet-magic ${CARDANO_NODE_MAGIC} --tx-file ./assets/tx.signed
```

2. Setting Up Collateral:
- Before interacting with Cardano Smart Contracts, ensure you have a UTXO with a minimum amount of ADA to be used as collateral. For instance, send 5 ADA to yourself to be used as collateral input:

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

3. Executing Script Logic to Unlock ADA:
- Execute the script logic to unlock the 5 ADA that was previously locked at the contract address:

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

> Note:
> 
> Unlike previous transactions, there’s no need to attach a .plutus script in this transaction. Instead, a reference of the .plutus script uploaded earlier is used to execute the script logic.

Through this process, you've compiled and interacted with a Cardano on-chain PlutusV2 script using the Reference Scripts feature, significantly optimizing the transaction process on the Cardano blockchain.

Congratulations 🎉, you have explored an essential feature of PlutusV2 smart contracts on the Cardano network!