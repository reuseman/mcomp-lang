component EntryPoint provides App { 
  def main() : int {
    var i : int;

    i = 0;
    print(i);
    
    i += 3;
    print(i);

    i *= 3;
    print(i);

    i -= 2;
    print(i);

    i /= 2;
    print(i);

    i %= 2;
    print(i);

    return 0;
  }
}