import itertools
import multiprocessing
from functools import partial
import time

# Load all of the elements in the dictionary file to a set for faster lookup
dictionary = set()
with open("MP1_dict.txt", 'r') as file:
    for line in file:
        dictionary.add(line.strip().lower())

# Same as the num_to_letter but minus and more importantly does the lookup itself so it is mildly faster
def shift_backwards(letter, shift):
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
    return number_to_letter[(letter_to_number[letter] - letter_to_number[shift]) % 26]

# Takes in key combination, ciphertext, key length, and first word length for cracking, helper function to be parallelized
def process_key_combination(key_comb, ciphertext, keyLength, firstWordLength):
    word = ""
    counter = 0
    # go through each letter in the ciphertext and add the shiftbackwards to the current word
    for letter in ciphertext:
        word += shift_backwards(letter, key_comb[counter])
        counter += 1
        # Update the counter if need be
        if counter == keyLength:
            counter = 0
        # Check if the length of the word and if it is == first word length, check if its in the dictionary. If it isnt then return None, if not continue
        if len(word) == firstWordLength and word not in dictionary:
            return None
    # Join the key and return it with the decrypted message
    return (word, "".join(key_comb))

def cracker(ciphertext, keyLength, firstWordLength):
    if keyLength >= 8:
        raise ValueError("Key length 8 or greater is not supported")

    letters = 'abcdefghijklmnopqrstuvwxyz'
    
    if keyLength == 7:
        # Generate combinations of length 6
        base_combinations = list(itertools.product(letters, repeat=6))
        
        # Function to process each base combination
        def process_base_comb(base_comb):
            results = []
            for letter in letters:
                full_comb = base_comb + (letter,)
                result = process_key_combination(full_comb, ciphertext, keyLength, firstWordLength)
                if result is not None:
                    results.append(result)
            return results
        
        # Use multiprocessing to process base combinations
        with multiprocessing.Pool() as pool:
            all_results = pool.map(process_base_comb, base_combinations)
        
        # Flatten the list of results
        return [item for sublist in all_results for item in sublist]
    
    else:
        # Original logic for key lengths < 7
        key_combinations = itertools.product(letters, repeat=keyLength)
        process_func = partial(process_key_combination,
                               ciphertext=ciphertext, 
                               keyLength=keyLength, 
                               firstWordLength=firstWordLength)
        
        with multiprocessing.Pool() as pool:
            results = pool.map(process_func, key_combinations)
        
        return [result for result in results if result is not None]

# Test the cracker and time it
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
