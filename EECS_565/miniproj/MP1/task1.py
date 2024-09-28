number_to_letter = {
    0: 'a', 1: 'b', 2: 'c', 3: 'd', 4: 'e', 5: 'f', 6: 'g', 7: 'h', 8: 'i',
    9: 'j', 10: 'k', 11: 'l', 12: 'm', 13: 'n', 14: 'o', 15: 'p', 16: 'q',
    17: 'r', 18: 's', 19: 't', 20: 'u', 21: 'v', 22: 'w', 23: 'x', 24: 'y', 25: 'z'
}

letter_to_number = {
    'a': 0, 'b': 1, 'c': 2, 'd': 3, 'e': 4, 'f': 5, 'g': 6, 'h': 7, 'i': 8,
    'j': 9, 'k': 10, 'l': 11, 'm': 12, 'n': 13, 'o': 14, 'p': 15, 'q': 16,
    'r': 17, 's': 18, 't': 19, 'u': 20, 'v': 21, 'w': 22, 'x': 23, 'y': 24, 'z': 25
}

# Convert number to letter with a shift
def num_to_letter(num, shift=0):
    return number_to_letter[(num + shift) % 26]


# Convert letter to number
def letter_to_num(letter):
    return letter_to_number[letter]


def vig(input_string, cipher):
    # remove spaces and make lowercase
    input_string = input_string.replace(" ", "").lower()
    output = ""    
    # Convert the cipher letters into the numbers that need to be shifted
    cipher_shift = [letter_to_num(letter) for letter in cipher]
    cipher_length = len(cipher)
    
    counter = 0
    # Go through each letter and append the shifted value to the output
    for letter in input_string:
        output += num_to_letter(letter_to_num(letter), cipher_shift[counter])
        # Reset the counter if it reaches the cipherlength
        counter += 1
        if counter == (cipher_length):
            counter = 0

    return output



print(vig("Hello world", "hi"))

