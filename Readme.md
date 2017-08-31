# Amplified Developer Challenge

Implementation by Jyotsna Prakash


## Build and Run from Source
   ``` bash
   git clone https://github.com/moonlightdrive/amplified-challenge.git
   cd amplified-challenge
   cabal sandbox init
   cabal install --only-dependencies
   cabal configure
   cabal build
   ./dist/build/amplified-challenge/amplified-challenge
   ```

The webserver should now be running on `localhost:3000`. Try visiting [localhost:3000/phone?input=25](localhost:3000/phone?input=25). Or with `curl`,
   ``` bash
   curl -v -X GET localhost:3000/phone?input=25
   ```