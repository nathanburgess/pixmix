num iftest (num x)
{
  num a;
  num b;
  num c;

  if (x is 0)
  a = 2;
  b = 3;
  c = a + b; 

  if (x is 1)
  a = 4;
  b = 5;
  c = a + b;

}

num main()
{
  console.read(num x);    #user input
  console.log(iftest(c));   #print
}
