class Parser:
    def __init__(self):
        self.pieces = {'R', 'N', 'B', 'Q', 'K'}
        self.files = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'}
        self.ranks = {'1', '2', '3', '4', '5', '6', '7', '8'}
        self.check = {'#', '+'}
        self.takes = {'x'}

    def parse(self, inp):
        inp = inp.strip()

        # Check to make sure that it isn't castling
        if inp == 'O-O':
            return True, "Kingside castle"
        elif inp == 'O-O-O':
            return True, "Queenside castle"

        # Remove the final check
        # if '#' in inp or '+' in inp:
            # if inp[-1] in self.check:
                # inp = inp[:-1]
            # else:
                # # +/# is not the last char, return False
                # return False, "+/# is not the final char"

        length = len(inp)

        # The first piece letter to be in self.pieces or its a pawn move. If its a pawn move check to make sure the first letter is a file
        if inp[0] in self.pieces:
            inp = inp[1::]
        elif inp[0] in self.files:
            pass
        else:
            return False, "Not a valid piece"
        

        # After parsing the piece, check for rank/file or takes (make sure that the string is greater than 3 first because we want to skip over just basic moves for now)
        if length > 3:
            if inp[0] in self.files or inp[0] in self.ranks or inp[0] in self.takes:
                inp = inp[1::]
            else:
                return False, "Not valid rank/file"

        # Parse the takes (if there)
        if inp[0] in self.takes:
            inp = inp[1::]

    
        if inp[0] in self.files:
            inp = inp[1::]
        else:
            return False, "Fails at initial location (file)"


        if inp[0] in self.ranks:
            inp = inp[1::]
            if len(inp) == 0:
                return True, "Pass"
        else:
            return False, "Fails at initial location (rank)"

        if inp[0] in self.check:
            inp = inp[1::]

        if len(inp) == 0:
            return True, "Pass"




        return False, "Fails, either not in rank or too many chars"

        

def test_parser():
    parser = Parser()
    test_cases = [
        "e4",           # Valid, pawn move
        "Nf3",          # valid, piece move
        "Bxe5",         # valid, piece capture
        "O-O",          # Kingside castle
        "O-O-O",        # Queenside castle
        "Rde1+",        # move with check
        "Qxf7#",        # capture with mate
        "Kxe5",         # king capture 
        "Nbd7",         # knight move  (on file b)
        "R1e2",         # Rook move (on rank 1)
        "exe4",         # Pawn on e file takes e4

        "hi",           # Should error, invalid
        "Nd9",          # d9 is not a valid move
        "Nd54+",         # 54 not valid location
        "Ndee3",         # ee invalid
        "y8",           # y is invalid
        "Kp3"           # p is invalid
    ]
    
    for test in test_cases:
        # reason was for debugging
        result, reason = parser.parse(test)
        print(f"Testing '{test}': {'Valid' if result else 'Invalid'}")

test_parser()

