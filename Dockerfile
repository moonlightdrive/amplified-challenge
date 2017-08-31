FROM haskell:8

RUN git clone --depth=50 --branch=master https://github.com/moonlightdrive/amplified-challenge.git amplified-challenge
WORKDIR /amplified-challenge
RUN cabal update
RUN ls
RUN cabal install --dependencies-only && cabal configure && cabal build
CMD ./dist/build/amplified-challenge/amplified-challenge
