function f(x: number, y: number): number {
    return 3;
}
function g(z: number, w: number): number {
    return 4 + f(1, 2);
}
function h(h: string): string {
    return g(3, 4);
}
function wrong_args(str: string): string {
    return g(1, 2, 3);
}