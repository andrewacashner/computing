class Test {
    public static void main(String[] args) {
        double diff = 100.0 - 99.01; 
        double doubleValue = diff * 100.0;
        int intValue = (int)doubleValue;

        System.out.printf("%f\n", doubleValue);    // prints 99.000000
        System.out.printf("%d\n", intValue);       // prints 98
        System.out.printf("%d\n", (int)99.000000); // prints 99
    }
}
