int num = 0;
int num1 = 1;
bool num2;
int i;
int fib;

fib = 5;
print(num, num1);
for(i = 2; i< fib; i = i +1)
{
    num2 = num + num1;
    print(num2);
    num = num1;
}
print("finished");
