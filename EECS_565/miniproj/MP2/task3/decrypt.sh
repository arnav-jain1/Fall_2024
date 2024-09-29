#!/bin/bash

openssl enc -d -aes-128-ecb -nosalt -in encrypted_ecb.txt -out decrypted_ecb.txt -k test123
openssl enc -d -aes-128-cbc -nosalt -in encrypted_cbc.txt -out decrypted_cbc.txt -k test123
openssl enc -d -aes-128-cfb -nosalt -in encrypted_cfb.txt -out decrypted_cfb.txt -k test123
openssl enc -d -aes-128-ofb -nosalt -in encrypted_ofb.txt -out decrypted_ofb.txt -k test123
