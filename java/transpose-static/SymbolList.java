import java.util.List;
import java.util.ArrayList;

public class SymbolList {

    private ArrayList<String> inputNames;
    private ArrayList<String> outputNames;
    private int length;
    private int initialValue = 0;
    
    public SymbolList(String[] inputNames, String[] outputNames,
            int initialValue) {

        this.length = inputNames.length;

        if (this.length == outputNames.length) {
            this.inputNames = new ArrayList<String>(List.of(inputNames));
            this.outputNames = new ArrayList<String>(List.of(outputNames));
            this.initialValue = initialValue;
            
        } else {
            throw new Error("Bad input to SymbolList");
        }
    }

    public SymbolList(String[] inputNames, String[] outputNames) {
        this(inputNames, outputNames, 0);
    }
    
    public SymbolList(String[] names) {
        this(names, names, 0);
    }

    private int offsetValue(int index) {
        return index + this.initialValue;
    }

    private int deOffsetValue(int offset) {
        return offset - this.initialValue;
    }

    public int valueOf(String input) {
        int index = this.inputNames.indexOf(input);
        if (index != -1) {
            return this.offsetValue(index);
        } else {
            throw new Error(
                    String.format("Symbol not found from string '%s'", input));
        }
    }

    public String get(int value) {
        value = this.deOffsetValue(value);

        if (value < this.length) {
            return this.outputNames.get(value);
        } else {
            throw new Error("Bad value input");
        } 
    }

    public String get(String input) {
        int index = this.deOffsetValue(this.valueOf(input));
        return this.outputNames.get(index);
    }
}
