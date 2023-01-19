interface Foo { 
  def foo(a : float) : float;
}

component CFoo provides Foo {
  def foo(a : float) : float {
    return a;
  }
}

component EntryPoint 
     provides App 
     uses Foo       
{
  def main() : int {
    var a : float;
    a = 42.0;
    a = a + 5.0;
    print(foo(a));
    return 0;
  }
}   

connect EntryPoint.Foo <- CFoo.Foo

