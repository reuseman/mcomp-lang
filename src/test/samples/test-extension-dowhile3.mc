component Program provides App {  
  def main() : int {
    var i : int;
    i = 42;
    while (i < 10) {
      print(i);
      i = i + 1;
    }

    var j : int;
    j = 42;
    do {
      print(j);
      j = j + 1;
    } while (j < 10);

    return 0;
  }
}