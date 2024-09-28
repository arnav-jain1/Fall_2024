import itertools
import multiprocessing
from functools import partial
import time

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


# Load all of the elements in the dictionary file to a set for faster lookup
dictionary = set()
with open("MP1_dict.txt", 'r') as file:
    for line in file:
        dictionary.add(line.strip().lower())


# Same as the num_to_letter but minus and more importantly does the lookup itself so it is mildly faster
def shift_backwards(letter, shift):
    return number_to_letter[(letter_to_number[letter] - letter_to_number[shift]) % 26]

# Takes in key combination, ciphertext, key length, and first word length for cracking, helper function to be parallelized
def process_key_combination(key_comb, ciphertext, keyLength, firstWordLength):
    word = ""
    counter = 0
    # go through each letter in the ciphertext and add the shiftbackwards to the current word
    for letter in ciphertext:
        word += shift_backwards(letter, key_comb[counter])
        counter += 1
        # Put the counter back to 0 if we reach the end of the key
        if counter == keyLength:
            counter = 0
        # Check if the length of the word and if it is == first word length, check if its in the dictionary. If it isnt then return None, if not continue
        if len(word) == firstWordLength and word not in dictionary:
            return None
    # Join the key and return it with the decrypted message
    return (word, "".join(key_comb))

# The ciphertext cracker function
def cracker(ciphertext, keyLength, firstWordLength):
    # Get all the key combinations from the key length 
    letters = 'abcdefghijklmnopqrstuvwxyz'
    if keyLength != 7:
        key_combinations = itertools.product(letters, repeat=keyLength)

        # Create a partial function with fixed arguments for each thread to run, the first arg is the function, the rest are args to the function itself (without key_comb which comes from later)
        process_func = partial(process_key_combination,
                               ciphertext=ciphertext, 
                               keyLength=keyLength, 
                               firstWordLength=firstWordLength)
        
        # use all of the cpu threads to try each key comb
        with multiprocessing.Pool() as pool:
            results = pool.map(process_func, key_combinations)
        
        # Return all the results that are not none
        return [result for result in results if result is not None]
    # Key length being 7 makes an issue of running out of ram, so try appending each letter to all combinations of 6 seperately
    elif keyLength == 7:
        key_combinations = itertools.product(letters, repeat=keyLength-1)
        all_results = []
        process_func = partial(process_key_combination,
                               ciphertext=ciphertext, 
                               keyLength=keyLength, 
                               firstWordLength=firstWordLength)

        for letter in letters:
            new_combinations = (tuple(letter) + comb for comb in key_combinations)
            
            with multiprocessing.Pool() as pool:
                results = pool.map(process_func, new_combinations)
            all_results += [result for result in results if result is not None]

        
        return all_results



if __name__ == "__main__":
    test_cases = [
        ("MSOKKJCOSXOEEKDTOSLGFWCMCHSUSGX", 2, 6),
        ("PSPDYLOAFSGFREQKKPOERNIYVSDZSUOVGXSRRIPWERDIPCFSDIQZIASEJVCGXAYBGYXFPSREKFMEXEBIYDGFKREOWGXEQSXSKXGYRRRVMEKFFIPIWJSKFDJMBGCC", 3, 7),
        ("MTZHZEOQKASVBDOWMWMKMNYIIHVWPEXJA", 4, 10),
        ("SQLIMXEEKSXMDOSBITOTYVECRDXSCRURZYPOHRG", 5, 11),
        ("LDWMEKPOPSWNOAVBIDHIPCEWAETYRVOAUPSINOVDIEDHCDSELHCCPVHRPOHZUSERSFS", 6, 9),
        ("VVVLZWWPBWHZDKBTXLDCGOTGTGRWAQWZSDHEMXLBELUMO", 7, 13)
    ]

    for i, (text, key_length, first_word_length) in enumerate(test_cases, 1):
        print(f"Question {i}:")
        start = time.time()
        result = cracker(text.lower(), key_length, first_word_length)
        end = time.time()
        print(result)
        print(f"Time taken: {end - start} seconds")
        print()
