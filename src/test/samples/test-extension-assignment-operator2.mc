component EntryPoint provides App { 
  def main() : int {
    var i : float;

    i = 0.0;
    print(i);
    
    i += 3.0;
    print(i);

    i *= 3.0;
    print(i);

    i -= 2.0;
    print(i);

    i /= 2.0;
    print(i);

    return 0;
  }
}