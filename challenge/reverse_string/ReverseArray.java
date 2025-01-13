class ReverseArray {
    public static void main(String[] args) {
        if (args.length != 1) {
            return;
        }
        String input = args[0];

        int inputLength = input.length();
        char[] chars = new char[inputLength];

        for (int i = 0; i < inputLength; ++i) {
            chars[inputLength - 1 - i] = input.charAt(i);
        }

        StringBuilder output = new StringBuilder();
        for (int i = 0; i < chars.length; ++i) {
            output.append(chars[i]);
        }

        System.out.println(output);
    }
}
