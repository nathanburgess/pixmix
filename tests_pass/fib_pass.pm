num num0 = 0;
num num1 = 1;
num num2;
num i;
num fib;

fib = 5;
print(num0, num1);
for(i = 2; i< fib; i = i +1)
{
    num2 = num0 + num1;
    print(num2);
    num0 = num1;
}
print("finished");
