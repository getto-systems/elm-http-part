#!/bin/bash

publish_main(){
  npm install
  npm test
  npm run elm publish
}

publish_main
