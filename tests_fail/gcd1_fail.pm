num gcd (string x, num y)
{
    if(x == 0) {
        return 0;
    }

    while (y != 0) {
        if ( x > y) { 
            x = x - y;
        }
        else {
            y = y - x; 
        }
    }
      
    return x;
}
 
print("GCD Result: ", gcd(48, 18));

print("finished");
