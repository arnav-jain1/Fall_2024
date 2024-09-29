#!/bin/bash


openssl enc -aes-128-cbc -k test123 -iv "0000000000000000" -nosalt -a -in text.txt -out iv1.txt 
openssl enc -aes-128-cbc -k test123 -iv "1111111111111111" -nosalt -a -in text.txt -out iv2.txt 
openssl enc -aes-128-cbc -k test123 -iv "0000000000000000" -nosalt -a -in text.txt -out ivsame.txt 

diff iv1.txt iv2.txt
diff iv1.txt ivsame.txt

