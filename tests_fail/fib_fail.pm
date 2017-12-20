num a = 0;
num b = 1;
num c;
bool i;
num fib;

fib = 5;
print(a, b);
for(i = 2; i< fib; i = i +1)
{
    c = a + b;
    print(c);
    a = b;
}
print("finished");
