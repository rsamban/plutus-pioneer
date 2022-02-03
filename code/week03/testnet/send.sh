cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat keys/01.addr) \
    --tx-in 16ad0527543a3b54d65589e0a606b2aae2c118d4e0dbdb2810262a4b60e698aa#0 \
    --tx-out "$(cat keys/02.addr) 10000000 lovelace" \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file keys/01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
