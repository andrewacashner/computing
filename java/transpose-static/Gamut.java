class Gamut {
    private static String[] pnames = 
    {"C", "D", "E", "F", "G", "A", "B"};

    public static SymbolList pname = new SymbolList(Gamut.pnames);

    enum PitchClass {
        pcC (pname.get(0)),
        pcD (pname.get(1))
    }
           

    private static String[] accidInput =
    {"bb", "b", "", "#", "##"};

    private static String[] accidOutput =
    {"𝄫", "♭", "♮", "♯", "𝄪"};

    public static SymbolList accid = new SymbolList(Gamut.accidInput, 
            Gamut.accidOutput, -2);
}
