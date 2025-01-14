package com.andrewcashner.musarithmetic;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class IntervalCalcGUI extends JFrame implements ActionListener {
    private Pname pname         = Pname.DEFAULT;
    private Accid accid         = Accid.DEFAULT;
    private Octave octave       = Octave.DEFAULT;
    private Pitch pitch         = Pitch.DEFAULT;

    private Sign sign           = Sign.DEFAULT;

    private Quality quality     = Quality.DEFAULT;
    private int degree          = Interval.DEFAULT_DEGREE;
    private Interval interval   = Interval.DEFAULT;

    private JList pnameSelect;
    private JList accidSelect;
    private JSpinner octaveSelect;
    private JList signSelect;
    private JList qualitySelect;
    private JSpinner degreeSelect;

    private String result;
    private JLabel resultLabel;

    public IntervalCalcGUI() {
        super("Interval Calculator");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new FlowLayout());
        setSize(600, 400);

        pnameSelect = new JList<Pname>(Pname.values());
        add(pnameSelect);

        accidSelect = new JList<Accid>(new Accid[] {
            Accid.DBL_FLAT,
            Accid.FLAT,
            Accid.NATURAL,
            Accid.SHARP,
            Accid.DBL_SHARP
        });
        add(accidSelect);

        SpinnerModel octaveModel = new SpinnerNumberModel(0, 0, null, 1);
        octaveSelect = new JSpinner(octaveModel);
        add(octaveSelect);

        signSelect = new JList<Sign>(Sign.values());
        add(signSelect);

        // But these need to be validated pairs with degree
        // also full text not abbrev (or tooltips?)
        qualitySelect = new JList<Quality>(Quality.values());
        add(qualitySelect);

        SpinnerModel degreeModel = new SpinnerNumberModel(1, 1, null, 1);
        degreeSelect = new JSpinner(degreeModel);
        add(degreeSelect);

        // add labels for current selected pitch, sign, and interval
        // sub-buttons to create pitch and interval first?
        // button to check expression first?
        resultLabel = new JLabel("", JLabel.CENTER);
        add(resultLabel);

        JButton goButton = new JButton("Calculate");
        goButton.addActionListener(this);
        add(goButton);
    }

    public String getResult() {
        return this.result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    private void updateResultLabel() {
        resultLabel.setText(" = " + getResult());
    }

    public void actionPerformed(ActionEvent e) {
        String newResult = "+"; 
        // IntervalCalculator.evaluate(getExpression());
        setResult(newResult);
        updateResultLabel();
    }

    public static void main(String[] args) {
        IntervalCalcGUI gui = new IntervalCalcGUI();
        gui.setVisible(true);
    }
}
