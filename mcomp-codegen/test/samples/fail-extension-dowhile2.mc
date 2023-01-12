component EntryPoint provides App {
  def main() : int {
    var i : int;
  
    do {
      i = i + 1;
    } while (true);
  
    do {
      foo(); /* foo undefined */
    } while (true);

    return 0;
  }
}