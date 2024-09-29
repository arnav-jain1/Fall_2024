#!/usr/bin/python3

# XOR two bytearrays
def xor(first, second):
   return bytearray(x^y for x,y in zip(first, second))

known_message = "This is a known message!"
ciphertext1 = "a469b1c502c1cab966965e50425438e1bb1b5f9037a4c159"
ciphertext2 = "bf73bcd3509299d566c35b5d450337e1bb175f903fafc159"

# Convert ascii string to bytearray
D1 = bytes(known_message, 'utf-8')

# Convert hex string to bytearray
D2 = bytearray.fromhex(ciphertext1)
D3 = bytearray.fromhex(ciphertext2)


key = xor(D1, D2)
message = xor(key, D3).decode('ascii')


print(f"Decrypted message: \n{message}")
