# Vigenere cipher
import itertools

dictionary = set()
with open("MP1_dict.txt", 'r') as file:
    for line in file:
        dictionary.add(line.strip().lower())

def num_to_letter(num, shift=0):
    number_to_letter = {
        0: 'a', 1: 'b', 2: 'c', 3: 'd', 4: 'e', 5: 'f', 6: 'g', 7: 'h', 8: 'i',
        9: 'j', 10: 'k', 11: 'l', 12: 'm', 13: 'n', 14: 'o', 15: 'p', 16: 'q',
        17: 'r', 18: 's', 19: 't', 20: 'u', 21: 'v', 22: 'w', 23: 'x', 24: 'y', 25: 'z'
    }
    return number_to_letter[(num + shift) % 26]

def shift_backwards(letter, shift):
    return num_to_letter((letter_to_num(letter) - letter_to_num(shift) ) % 26)

def letter_to_num(letter):
    letter_to_number = {
        'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5, 'g': 6, 'h': 7, 'i': 8,
        'j': 9, 'k': 10, 'l': 11, 'm': 12, 'n': 13, 'o': 14, 'p': 15, 'q': 16,
        'r': 17, 's': 18, 't': 19, 'u': 20, 'v': 21, 'w': 22, 'x': 23, 'y': 24, 'z': 25
    }
    return letter_to_number[letter]


def vig(input_string, cipher):
    input_string = input_string.replace(" ", "")
    output = ""    
    cipher_shift = [letter_to_num(letter) for letter in cipher]
    cipher_length = len(cipher)
    
    counter = 0
    for letter in input_string:
        output += num_to_letter(letter_to_num(letter), cipher_shift[counter])
        counter += 1
        if counter == (cipher_length):
            counter = 0

    return output


def cracker(ciphertext, keyLength, firstWordLength):
    letters = 'abcdefghijklmnopqrstuvwxyz'
    possibilities = []
    for key_comb in itertools.product(letters, repeat=keyLength):
        word = ""
        potential = True
        counter = 0
        for letter in ciphertext:
            word += shift_backwards(letter, key_comb[counter])
            counter += 1
            if counter == keyLength:
                counter = 0
            if len(word) == firstWordLength and word not in dictionary:
                potential = False
                break
        if potential == True:
            possibilities.append((word, "".join(key_comb)))
    return possibilities

text = "MSOKKJCOSXOEEKDTOSLGFWCMCHSUSGX"
text = text.lower()
print("Question 1:")
print(cracker(text, 2, 6))


text = "PSPDYLOAFSGFREQKKPOERNIYVSDZSUOVGXSRRIPWERDIPCFSDIQZIASEJVCGXAYBGYXFPSREKFMEXEBIYDGFKREOWGXEQSXSKXGYRRRVMEKFFIPIWJSKFDJMBGCC"
text = text.lower()
print("Question 2:")
print(cracker(text, 3, 7))

text = "MTZHZEOQKASVBDOWMWMKMNYIIHVWPEXJA"
text = text.lower()
print("Question 3:")
print(cracker(text, 4, 10))

text = "SQLIMXEEKSXMDOSBITOTYVECRDXSCRURZYPOHRG"
text = text.lower()
print("Question 4:")
print(cracker(text, 5, 11))


text = "LDWMEKPOPSWNOAVBIDHIPCEWAETYRVOAUPSINOVDIEDHCDSELHCCPVHRPOHZUSERSFS"
text = text.lower()
print("Question 5:")
print(cracker(text, 6, 9))

text = "VVVLZWWPBWHZDKBTXLDCGOTGTGRWAQWZSDHEMXLBELUMO"
text = text.lower()
print("Question 6:")
print(cracker(text, 7, 13))


