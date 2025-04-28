/* 2025/04/9
 * CSC 103 exercise on recursion
 */
public class Cheers {
    public static void main(String[] args) {
        int n = Integer.parseInt(args[0]);
        cheers(n);
    }

    public static void cheers(int n) {
        cheers(n, n / 2);
    }

    public static void cheers(int n, int total) {
        if (n > total) {
            System.out.println("Hip");
            cheers(n - 1, total);
            System.out.println("Hip");
        } else {
            System.out.println("Hurrah");
        }
    }
}
