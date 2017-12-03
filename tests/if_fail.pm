fun iftest (num x)
{
  num a;
  num b;

  if (x is 0)
  a = 2;
  b = 3;

  if (x is 1)
  a = 4;
  b = 5
}

num main()
{
  console.read(num x);    #user input
  console.log(iftest(x));   #print
}
