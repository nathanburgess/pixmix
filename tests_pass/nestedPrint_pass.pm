num nested (num x, num y)
{
  while ( x != y) { print("past while");
  if ( x > y) { print("past outer if"); x = x + y;}
  if ( y < x) { print("past inner if"); x = x - y;}
  else {
  print("past else");
  x = y - x; }
  
  return x;
}

}


  
 
  print(nested(4, 3));
  print("finished");
  
