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

        try {
            Path infile = Paths.get(infileName);
            Path outfile = Paths.get(outfileName);
           
            Stream<String> lines = Files.lines(infile);

            // NB: In Java 21, you could do reverseLines.reversed();
            List<String> reverseLines = lines.collect(Collectors.toList()); 
            Collections.reverse(reverseLines); 

            Files.write(outfile, reverseLines);
        }
        catch (InvalidPathException | IOException e) {
            System.err.println(e.getMessage());
        }

    }
}
