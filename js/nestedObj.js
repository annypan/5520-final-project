var x = {y: "hello", z: 1, w: {a: 1, b: 2, c: 3}}

if (x.z + x.w.a > 0) {
    x.y = "world";
} else {
    x.y = "nooooo";
}

while (x.w.b < 10) {
    x.w.b = x.w.b + 1;
}

function foo(i: number, j: string): number{
    return i + j;
}

foo(x.w.a, x.w.b);