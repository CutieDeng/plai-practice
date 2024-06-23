def recurialize(f): 
    # return (lambda v: ((lambda b: b(b))((lambda w: w((lambda b: b(b)), v))(f)))) 
    # recursive call it 
    # return lambda *args: f(recurialize(f), *args)
    
    # return lambda v: f(lambda v2: f(lambda v3: f(f, v3), v2), v)
    # o = (lambda f: lambda b: lambda v: f(b(b), v))(f)
    # o = lambda b: lambda v: f(b(b), v) 
    # w = o(o) 
    # return lambda v: f(o(o), v)
    # return o(o)(v)
    return (lambda b: b(b))(lambda b: lambda v: f(b(b), v))
    # return (lambda b: b(b)) ((lambda w: lambda b: w(b(b)))(f))
    # omega = lambda f: lambda g: f(g(g)) 
    # rst = lambda f: omega(omega) 
    # omega(omega) 
    # lambda a: f(f, a) 

# def call(f) : 
#     f(f) 

# def call2(f): 
#     f(f) 

# call(call2) 

# call(call2) ; (call call2) 
# call2(call2) ; (call2 call2) 
# call2(call2) ; (call2 call2)

# Now expect recursion ... 
# (call call)
# (special (call call)) 

def fact(f, n): 
    if n == 1: 
        return 1 
    else: 
        return f(n-1) * n 

def recursive(f): 
    return (lambda b: b(b))(lambda b: lambda *args: f(b(b), *args))

f = recursive(fact) 
print (f(6)) # what is it? 

