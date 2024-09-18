
/*
Symbol pcC = new Symbol("C", "C", 0);
Symbol pcA = new Symbol("A", "A", 5);
SymbolList pnames = new SymbolList(pcC, pcA);
SymbolList pnames = newSymbolList(
        new String[]{"c", "d", "e"}, 
        new String[]{"C", "D", "E"},
        new int[]{0, 1, 2});
String pcA = pnames.getFromValue(5);
String pcA = pnames.getFromString("a");
*/


import java.util.List;
import java.util.Arrays;

public class SymbolList {

    class Symbol {
        private String inputName;
        private String outputName;
        private int value;

        public Symbol(String inputName, String outputName, int value) {
            this.inputName = inputName;
            this.outputName = outputName;
            this.value = value;
        }

        String toString() {
            return this.outputName;
        }

        int toValue() {
            return this.value;
        }
    }

    private List<Symbol> symbols;
    
    public SymbolList(Symbol... symbols) {
        this.symbols = Arrays.asList(symbols);
    }

    public SymbolList(String[] inputNames, String[] outputNames, 
            int[] values) {

        if (inputNames.length == outputNames.length 
                && outputNames.length == values.length) {

            for (int i = 0; i < inputNames; ++i) {
                Symbol newSym = new Symbol(
                        inputNames.get(i),
                        outputNames.get(i), 
                        values.get(i));
                this.symbols.add(newSym);
            }
        } else {
            throw new Error("Bad input to SymbolList\n");
        }
    }

    Symbol getFromValue(int value) {
        Symbol result = null;
        for (Symbol symbol : this.symbols) {
            if (symbol.toValue() == value) {
                result = symbol;
                break;
            }
        }
        if (!result) {
            throw new Error("Symbol not found from value\n");
        } 
        return result;
    }

    Symbol getFromString(String input) {
        Symbol result = null;
        for (Symbol symbol : this.symbols) {
            if (symbol.inputString == input) {
                result = symbol;
                break;
            }
        }
        if (!result) {
            throw new Error("Symbol not found from string\n");
        } 
        return result;
    }

    String getFromValue(int value) {
        Symbol sym = getFromValue(value);
        return sym.toString();
    }

    String getFromString(String input) {
        Symbol sym = getFromString(input);
        return sym.toString();
    }
}
