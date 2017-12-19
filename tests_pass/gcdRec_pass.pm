num gcd (num x, num y)
{
    if(y == 0) {
        return x;
    }

    return gcd(y, x % y);
}

print("GCD Result: ", gcd(a, b));

print("finished");