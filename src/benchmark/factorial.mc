component MyComponent provides App {

    def recursiveFactorial(n : int) : int {
        if (n == 0) {
            return 1;
        } else {
            return n * recursiveFactorial(n - 1);
        }
    }


    def iterativeFactorial(n : int) : int {
        var result : int = 1;
        var i : int = 1;
        for (; i <= n; i++) {
            result = result * i;
        }
        return result;
    }

    def memoizedFactorial(n : int) : int {
        // var memo : int[n];
        var memo : int[100];
        memo[0] = 1;
        var i : int = 1;
        for (; i <= n; i++) {
            memo[i] = i * memo[i - 1];
        }
        return memo[n];
    }


    def main() : int {
        print(recursiveFactorial(19));
        print(iterativeFactorial(19));
        print(memoizedFactorial(19));

        return 0;
    }
}