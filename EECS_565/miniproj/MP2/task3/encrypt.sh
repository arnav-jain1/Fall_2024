#! /bin/bash

openssl enc -aes-128-ecb -nosalt -in f2.txt -out encrypted_ecb.txt -k test123
openssl enc -aes-128-cbc -nosalt -in f2.txt -out encrypted_cbc.txt -k test123
openssl enc -aes-128-cfb -nosalt -in f2.txt -out encrypted_cfb.txt -k test123
openssl enc -aes-128-ofb -nosalt -in f2.txt -out encrypted_ofb.txt -k test123
