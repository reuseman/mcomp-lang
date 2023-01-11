component MyComponent provides App {

    def printPrimes(n : int) : void {
        var i : int = 2;
        for (; i < n; i++) {
            if (isPrime(i)) {
                print(i);
            }
        }
    }

    def isPrime(n : int) : bool {
        var i : int = 2;
        for (; i < n; i++) {
            if (n % i == 0) {
                return false;
            }
        }
        return true;
    }

    def main() : int {
        printPrimes(200000);

        return 0;
    }
}