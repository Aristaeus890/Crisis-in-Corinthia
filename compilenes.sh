#!/bin/bash

ca65 cic.asm -o cic.o -t nes
ld65 cic.o -o cic.nes -t nes
