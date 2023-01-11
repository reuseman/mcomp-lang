#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool isPrime(int n)
{
    int i = 2;
    for (; i < n; i++)
    {
        if (n % i == 0)
        {
            return false;
        }
    }
    return true;
}

void printPrimes(int n)
{
    int i = 2;
    for (; i < n; i++)
    {
        if (isPrime(i))
        {
            printf("%d\n", i);
        }
    }
}


int main(int argc, char const *argv[])
{
    printPrimes(200000);
    
    return 0;
}
