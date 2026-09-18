void main() {
    int n = add(2, 3);
    if (n != 5) {
        throw new RuntimeException("expected 5, got " + n);
    }
    IO.println("All compact source tests passed!");
}

int add(int a, int b) {
    return a + b;
}
