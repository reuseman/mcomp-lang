component Program provides App {
  def foo(a : int) : int {
    var j : int;
    j = 0;
    do {
      j = j + 2;
      a = a - 1;
    } while (a > 0);
    return j;
  }
  
  def main() : int {
    print(foo(7));
    return 0;
  }
}