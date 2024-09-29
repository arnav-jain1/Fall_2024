#! /bin/bash

openssl enc -aes-256-ecb -nosalt -in pic_original.bmp -out ecb_enc.txt -k test123
head -c 54 pic_original.bmp > header 
tail -c +55 ecb_enc.txt > data
cat header data > ecb.bmp
rm header data



openssl enc -aes-256-cbc -nosalt -in pic_original.bmp -out cbc_enc.txt -k test123
head -c 54 pic_original.bmp > header 
tail -c +55 cbc_enc.txt > data
cat header data > cbc.bmp
rm header data
