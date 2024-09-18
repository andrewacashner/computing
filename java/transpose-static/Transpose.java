import java.util.List;
import java.util.ArrayList;

class Transpose {
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Usage: java transpose PITCHNAME");
            System.exit(1);
        }

        String input = args[0];

        String inputPname = input.substring(0, 1);
        String inputAccid = input.substring(1, input.length() - 1);
        int inputOctave = Integer.parseInt(
                input.substring(input.length() - 1));

        String pnameString = Gamut.pname.get(inputPname);
        int pnameValue = Gamut.pname.valueOf(inputPname);
        System.out.format("Pitch: %s (%d)\n", pnameString, pnameValue);

        String accidString = Gamut.accid.get(inputAccid);
        int accidValue = Gamut.accid.valueOf(inputAccid);
        System.out.format("Accid: %s (%d)\n", accidString, accidValue);

        Pitch pitch = Pitch.parse(input);
        System.out.println(pitch);

    }
}
