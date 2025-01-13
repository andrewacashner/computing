class ReverseSimple {
    public static void main(String[] args) {
        if (args.length != 1) {
            return;
        }

        String input = args[0];
        StringBuilder output = new StringBuilder();
        for (int i = input.length() - 1; i >= 0; --i) {
            output.append(input.charAt(i));
        }
        System.out.println(output);
    }
}
