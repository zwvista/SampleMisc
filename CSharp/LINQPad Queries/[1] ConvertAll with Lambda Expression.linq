<Query Kind="Program" />

// Title: List<T>.ConvertAll<>() with Lambda Expression
// Link: http://www.devcurry.com/2011/06/list-with-lambda-expression.html
void Main()
{
	Example1();
  Example2();
}

private void Example1()
{
  var l1 = new List<int> { 1, 2, 3, 4 };
  var l2 = l1.ConvertAll<string>(delegate(int x)
  {
    return x.ToString();
  });
  
  l2.Dump();
}

private void Example2()
{
  var l1 = new List<int> { 1, 2, 3, 4 };
  
  l1.ConvertAll<string>(r => r.ToString()).Dump();
}
