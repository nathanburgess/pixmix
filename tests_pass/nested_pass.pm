num nested (num x, num y)
{
  while ( x != y) {
  if ( x > y) { x = x + y;}
  if ( y < x) { x = x + y;}
  else {
  x = y - x; }
  
  return x;
}

}


  
 
  print(nested(4, 3));
  print("finished");
  
