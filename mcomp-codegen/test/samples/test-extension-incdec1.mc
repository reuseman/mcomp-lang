component EntryPoint provides App { 
  def main() : int {
    var i : int;
    i = 0;

    print(i++);
    print(++i);
    print(--i);
    print(i--);
    print(i);
  
    return 0;
  }
}