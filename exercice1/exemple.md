# 1. Arithmetic 
{
    a = 5;
    b = 7;
    c = a + b;
    d = a + a;
}
echo "{a = 5;b = 7;c = a + b;d = a + a}" | ./tinyc

# 2. Conditional Statement
{
    x = 10;
    if (x < 20)
        y = 1;
    else
        y = 2;
}
echo "{x = 10;if (x < 20) y = 1;else y = 2;}" | ./tinyc

# 3. While Loop
{
    i = 1;
    r = 2;
    while (i < 5){ 
        i = i + 1;
    r = r + i 
    }
}
echo "{ i = 1; r = 2; while (i<5){i=i+1;r=r+i}}" | ./tinyc

# 4. Do statement
{
j = 1;
do {
    j = j + 2;
} while (j < 6);
}

echo "{ j=1; do j=j+2; while (j < 6); }" | ./tinyc

# 5. Nested Conditionals
{
    p = 15;
    if (p < 30) {
        if (p < 20)
            q = 1;
        else
            q = 2;
    } else {
        q = 3;
    }
}

echo "{ p=15; if (p < 30) if (p < 20) q = 1; else q = 2; else q = 3;}" | ./tinyc

# 6. Multiple Statements in Braces:
{
    m = 5;
    n = 10;
}

echo "{ m=5; n=10;}" | ./tinyc

# 7. Incrementing variable
c = 1;
c = c + 1;

echo "{c = 1;c = c + 1;}" | ./tinyc

# 8. Random spacing
{
    p = 15;
           if (p < 30) {
        if (p < 20)
            q    = 1;
        else
            q    = 2;
    }      else {
        q     = 3;
    }
}
echo "{ p=15;       if (p < 30) if (p < 20) q   = 1; else q =   2; else   q   = 3;}" | ./tinyc


# 9. Arithmetic with declaration
a = 3;
b = 12;
a = a + b - 2;

echo "{a = 3;b = 12;a = a + b - 2;}" | ./tinyc

# 10. Empty line
x = 5;
;
y = 10;

echo "{x = 5;;y = 10;}" | ./tinyc