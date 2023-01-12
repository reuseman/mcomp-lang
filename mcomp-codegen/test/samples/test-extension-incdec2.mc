component EntryPoint provides App { 
  def main() : int {
    var arr : int[5];
    var i : int;
    for (i = 0; i < 5; i++) {
      arr[i] = i;
    }

    var index : int;
    index = 0;
    print(arr[index]++);
    print(arr[index]);

    index = 3;
    print(arr[++index]);

    print(arr[index--]);
    print(arr[index]);
  
    return 0;
  }
}