// Core Part 1 about the 3n+1 conjecture
//============================================

object C1 {

//(1) Complete the collatz function below. It should
//    recursively calculate the number of steps needed 
//    until the collatz series reaches the number 1.
//    If needed, you can use an auxiliary function that
//    performs the recursion. The function should expect
//    arguments in the range of 1 to 1 Million.

def collatz(n: Long) : Long = rec(n,0)
    
def rec(n: Long, steps: Long) : Long = {
    if (n==1) steps
    else if((n%2)==0) rec(n/2, steps+1)
    else rec(3*n + 1, steps + 1)
}    
    

//(2) Complete the collatz_max function below. It should
//    calculate how many steps are needed for each number 
//    from 1 up to a bound and then calculate the maximum number of
//    steps and the corresponding number that needs that many 
//    steps. Again, you should expect bounds in the range of 1
//    up to 1 Million. The first component of the pair is
//    the maximum number of steps and the second is the 
//    corresponding number.

def collatz_max(bnd: Long) : (Long, Long) = {
   val collatz_list =  for(i <- 1L to bnd) yield collatz(i)
   val maxVal = collatz_list.max
   (maxVal,collatz_list.indexOf(maxVal)+1)
}

//(3) Implement a function that calculates the last_odd
//    number in a collatz series.  For this implement an
//    is_pow_of_two function which tests whether a number 
//    is a power of two. The function is_hard calculates 
//    whether 3n + 1 is a power of two. Again you can
//    assume the input ranges between 1 and 1 Million,
//    and also assume that the input of last_odd will not 
//    be a power of 2.

def is_pow_of_two(n: Long) : Boolean = if (n==1) true else if (n%2!=0) false else if (n==0) false else is_pow_of_two(n/2)  

def is_hard(n: Long) : Boolean = is_pow_of_two(3*n + 1)

def last_odd(n: Long) : Long ={
    if (is_hard(n) & n%2==1) n 
    else if(is_pow_of_two(n) & n%2==0) n 
    else if(n%2==0) last_odd(n/2) 
    else last_odd(3*n +1)
    }
}
