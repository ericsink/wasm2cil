#!/bin/sh
hexdump -v -e '/1 "%02x\n"' $1
