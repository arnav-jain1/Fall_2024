#include "Tests/test1.h"
#include "Tests/test2.h"
#include "Tests/test3.h"
#include "Tests/test4.h"
#include "Tests/test5.h"
#include "Tests/test6.h"
#include "Tests/test7.h"
#include "Tests/test8.h"
#include "Tests/test9.h"
#include "Tests/test10.h"

int main() {
    int totalScore = 0;

    totalScore += test1();
    totalScore += test2();
    totalScore += test3();
    totalScore += test4();
    totalScore += test5();
    totalScore += test6();
    totalScore += test7();
    totalScore += test8();
    totalScore += test9();
    totalScore += test10();

    // Print the total score
    printf("Total Score: %d/100\n", totalScore);

    return 0;
}

