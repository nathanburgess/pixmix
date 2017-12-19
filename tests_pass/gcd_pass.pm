num gcd (num x, num y)
{
    if(x == 0) {
        return y;
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
 
print("GCD Result: ", gcd(240, 150));

print("finished");