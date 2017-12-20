Object one = {
    string oneA = "first string in object one";
    num oneB = 32.5;
    num oneC;

    num oneFunc() {
        print("printing from one.oneFunc()");
    }
};

num global = 5;

print(global);

one.oneFunc();
num a = one.oneA;