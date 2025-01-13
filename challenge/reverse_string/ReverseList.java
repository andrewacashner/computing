import java.util.stream.*;

class ReverseList {
    public static void main(String[] args) {
        if (args.length != 1) {
            return;
        }
        String input = args[0];

        String output = input.chars()
                    .mapToObj(Character::toString)
                    .collect(Collectors.toList())
                    .reversed()
                    .stream()
                    .collect(Collectors.joining());

        System.out.println(output);
    }
}
