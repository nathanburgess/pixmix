num gcd (num x, num y)
{
  while ( x != y) {
  if ( x > y) { x = x - y;
  else {
  y = y - x; }
  }
  return x;
}

}


  
 
  print(gcd(2, 3));
  print("finished");
  
