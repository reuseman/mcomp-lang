component EntryPoint provides App {

  def multiply_by_two(a : int) : int {
    return a * 2;
  }

  def multiply_by_two(b : int) : int {
    return b * 4;
  }

  def main() : int {
    var x_int : int;
    var x_float : float; 

    x_int = 42;

    print(multiply_by_two(x_int));
    print(multiply_by_two(x_int));

    return 0;
  }
}   
