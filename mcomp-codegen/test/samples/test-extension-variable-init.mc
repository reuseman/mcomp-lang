component Entrypoint provides App {
  var my_int : int = 42;
  var my_float : float = 3.14;
  var my_char : char = 'a';
  var my_bool : bool = true;

  var my_int_sum : int = 42 + 1;
  var my_int_diff : int = 42 - 1;
  var my_int_prod : int = 42 * 2;
  var my_int_div : int = 42 / 2;
  var my_int_neg : int = -42;

  var my_bool_and1 : bool = false && true;
  var my_bool_and2 : bool = true && true;
  var my_bool_or1 : bool = false || false;
  var my_bool_or2 : bool = true || false;
  var my_bool_not : bool = !true;


  def main() : int {
    print(my_int);
    print(my_float);
    print(my_char);
    print(my_bool);

    print(my_int_sum);
    print(my_int_diff);
    print(my_int_prod);
    print(my_int_div);
    print(my_int_neg);

    print(my_bool_and1);
    print(my_bool_and2);
    print(my_bool_or1);
    print(my_bool_or2);
    print(my_bool_not);

    return 0;
  }
}
