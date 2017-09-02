# Amplified Developer Challenge

Implementation by Jyotsna Prakash

## Documentation
Documentation can be viewed at [moonlightdrive.github.io/amplified-challenge/](https://moonlightdrive.github.io/amplified-challenge).

### Building docs
The documentation can be built with
``` bash
cabal haddock --executables
```
You will find the generted docs in the folder `dist/doc/html/amplified-challenge/amplified-challenge/`.

## Building and Running
This project can be built and run from source or with docker.
### From Source
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
### With Docker
``` bash
    # or get the Dockerfile onto your system any other way you please
    wget https://raw.githubusercontent.com/moonlightdrive/amplified-challenge/master/Dockerfile
    # Build container
    docker build -t amplified-challenge .
    # Run container. $PORT is a port of your choice. 3000 should be okay but 8080 isn't a bad choice either
    docker run --rm -p $PORT:3000 amplified-challenge
```
From another terminal,
``` bash
    curl -v -X GET localhost:$PORT/?input=25
```    