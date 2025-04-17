#!/bin/sh
gnatmake -ITJa -D obj -o bin/$1 src/$1.adb
