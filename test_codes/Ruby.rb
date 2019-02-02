#Given two numbers a and b, write a fast method to multiply both of them.
#Algorithm: Russian Peasant
=begin
1. Initialize ans=0
2. if b is odd ,add a to ans
3. double a,half b
4. repeat steps 2 and 3 until b>0
=end

#Using arithmatic operations
def russian_peasant(a,b)
    ans=0
    while b>0
      ans+=a if (b%2==1)  # b&1 == 1 only when b is odd
      a*=2                #a<<1 is same as a*2
      b/=2                #b>>1 is same as b/2
    end
    return ans
end
russian_peasant(10,51) # => 510

#Using Bit-operations
def russian_peasant(a,b)
    ans=0
    while b>0
      ans+=a if (b&1==1)  # b&1 == 1 only when b is odd
      a<<=1               #a<<1 is same as a*2
      b>>=1               #b>>1 is same as b/2
    end
    return ans
end
russian_peasant(10,51) # => 510