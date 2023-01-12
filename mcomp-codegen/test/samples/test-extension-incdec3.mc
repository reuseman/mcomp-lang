component EntryPoint provides App { 
  def main() : int {
    var i : float;
    i = 0.0;

    print(i++);
    print(++i);
    print(--i);
    print(i--);
    print(i);
  
    return 0;
  }
}