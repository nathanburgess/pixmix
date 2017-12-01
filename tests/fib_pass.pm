fun fib(int x)
{
  if (x is 0)
  return 0;
  if (x is 1)
  return 1;
}

int main()
{
  console.read(int x);    //user input
  console.log(fib(x - 1) + fib(x - 2));   //print
}
