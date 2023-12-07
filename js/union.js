function f(x: number, y: number): number | string {
    return x + y - 3;
}
var x1 = f(3, 4);
if (x1 + 3 == 10) {
    x1 = "str";
} else {
    x1 = "str2";
}