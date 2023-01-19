component EntryPoint provides App { 
  def main() : int {
    var arr : int[5];

    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 3;
    arr[3] = 4;
    arr[4] = 5;

    var index : int;
    index = 0;

    arr[++index] += 10;             // 12 
    print(arr[1]);

    // if this would have been implemented in the wrong way, then it would have been unrolled to the following expression
    // arr[++index] = arr[++index] + 10;
    // arr[1] = arr[2] + 10;       -> arr[1] = 13

    return 0;
  }
}