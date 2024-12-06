import java.nio.file.*;
import java.io.IOException;

import java.util.stream.*;
import java.util.Collections;
import java.util.List;

public class ReverseLines {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("Usage: java ReverseLines INFILE OUTFILE");
            return;
        }

        String infileName = args[0];
        String outfileName = args[1];

        Path infile, outfile;
        try {
            infile = Path.of(infileName);
            outfile = Path.of(outfileName);
            readWriteReversed(infile, outfile);
        }
        catch (InvalidPathException e) {
            System.err.println(e);
        }
    }

    // NB: In Java 21, you could do reverseLines.reversed();
    private static void readWriteReversed(Path infile, Path outfile) {
        try (Stream<String> lines = Files.lines(infile);) {
            List<String> reverseLines = lines.collect(Collectors.toList()); 
            Collections.reverse(reverseLines); 
            Files.write(outfile, reverseLines);
        }
        catch (IOException e) {
            System.err.println(e);
        }
    }
}
