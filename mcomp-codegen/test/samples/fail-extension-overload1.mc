component EntryPoint provides App {

  def multiply_by_two(a : int) : int {
    return a * 2;
  }

  def multiply_by_two(a : int) : float {
    return int_to_float(a) * 2.0;
  }

  def main() : int {
    var x_int : int;
    var x_float : float; 

    x_int = 42;
    x_float = 42.0;

    print(multiply_by_two(x_int));
    print(multiply_by_two(x_float));

    return 0;
  }
}   
