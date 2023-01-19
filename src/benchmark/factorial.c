#include <stdio.h>
#include <stdlib.h>

int recursiveFactorial(int n) {
    if (n == 0) {
        return 1;
    } else {
        return n * recursiveFactorial(n - 1);
    }
}

int iterativeFactorial(int n) {
    int result = 1;
    int i;
    for (i = 1; i <= n; i++) {
        result = result * i;
    }
    return result;
}

int memoizedFactorial(int n) {
    int memo[100];
    memo[0] = 1;
    int i;
    for (i = 1; i <= n; i++) {
        memo[i] = i * memo[i - 1];
    }
    return memo[n];
}

int main() {
    printf("%d\n", recursiveFactorial(19));
    printf("%d\n", iterativeFactorial(19));
    printf("%d\n", memoizedFactorial(19));
    return 0;
}