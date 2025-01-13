import java.util.Deque;
import java.util.ArrayDeque;

class ReverseStack {
    public static void main(String[] args) {
        if (args.length != 1) {
            return;
        }
        String input = args[0];

        Deque<Character> stack = new ArrayDeque<Character>();
        for (int i = 0; i < input.length(); ++i) {
            stack.push(input.charAt(i));
        }
        // OR: input.chars().forEach(c -> stack.push((char) c));

        StringBuilder output = new StringBuilder();
        while (stack.peekFirst() != null) {
            output.append(stack.pop());
        }

        System.out.println(output);
    }
}
