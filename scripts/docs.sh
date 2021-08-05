#!/bin/sh
dune build @default @doc
cp -r _build/default/_doc/_html/ ./docs/
mkdir -p ./docs/simple-data-channel
cp _build/default/examples/simple-data-channel/index.bc.js ./docs/simple-data-channel/
cp _build/default/examples/simple-data-channel/index.html ./docs/simple-data-channel/
mkdir -p ./docs/simple-video-call
cp _build/default/examples/simple-video-call/index.bc.js ./docs/simple-video-call/
cp _build/default/examples/simple-video-call/index.html ./docs/simple-video-call/
chmod -R +w ./docs
