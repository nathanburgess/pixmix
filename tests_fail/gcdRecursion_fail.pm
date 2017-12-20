num gcd (num x, num y)

{   
     
    if (y == 0) 
     {
        return x;
        
     }
      
    return gcd(y, x%y);
    print("gcd");
    
}

print("GCD OUTPUT:", gcd(60, 36));


    

