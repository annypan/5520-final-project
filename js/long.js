function f(x: number, y: number): number {
    return x + y - 3;
}
function g(z: number, w: number): number {
    return z - w + f(1, 2);
}

var x1 = g(3, 4);
var x2 = f(x1, 3);

if (x1 + x2 == 5) {
    x1 = 0;
} else {
    x1 = false;
}

g(1, 3);
g(x1, x2);

function h(q: string) : void {
    return undefined;
}

g(h("hello"), h("world"))