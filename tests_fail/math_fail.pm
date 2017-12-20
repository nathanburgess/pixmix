Object Math = {
	num add(num a, num b) {
		return a + b;
	}

	string mod(num a, num b) {
		return a % b;
	}

	num pow(num a, num b) {
		num base = a;
		while(b > 1) {
			a = a * base;
			b = b - 1;
		}
		return 0;
	}
};

printf("4 + 1 = %f\n", Math.add(4, 1));
printf("10 % 6 = %f\n", Math.mod(10, 6));
printf("2^5 = %f\n", Math.pow(2, 5));
print("finished");
