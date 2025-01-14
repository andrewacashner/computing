package com.andrewcashner.musarithmetic;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * First try at Java GUI
 * @author Andrew Cashner
 * @version Started 2025/01/14
 */
public class IntervalCalcGUI extends JFrame implements ActionListener {
    private Pitch startPitch    = Pitch.DEFAULT;
    private Sign sign           = Sign.DEFAULT;
    private Interval interval   = Interval.DEFAULT;
    private Pitch endPitch      = Pitch.DEFAULT;

    private JList pnameSelect, accidSelect, signSelect, qualitySelect;
    private JSpinner octaveSelect, degreeSelect;
    private JLabel startPitchLabel, signLabel, intervalLabel, endPitchLabel;
    private JPanel selectorPanel, rendererPanel;

    public IntervalCalcGUI() {
        super("Interval Calculator");
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLayout(new FlowLayout());
        setSize(300, 200);

        selectorPanel = new JPanel(); // TODO layout manager (GridBag?)
        rendererPanel = new JPanel();
        add(selectorPanel);
        add(rendererPanel);

        pnameSelect = new JList<Pname>(Pname.values());
        pnameSelect.addListSelectionListener(e -> 
            setPname((Pname)pnameSelect.getSelectedValue()));
        selectorPanel.add(pnameSelect);

        accidSelect = new JList<Accid>(new Accid[] {
            Accid.DBL_FLAT,
            Accid.FLAT,
            Accid.NATURAL,
            Accid.SHARP,
            Accid.DBL_SHARP
        });
        accidSelect.addListSelectionListener(e ->
            setAccid((Accid)accidSelect.getSelectedValue()));
        selectorPanel.add(accidSelect);

        SpinnerModel octaveModel = new SpinnerNumberModel(
                Octave.DEFAULT_VALUE, 
                Octave.MIN_VALUE, 
                Octave.MAX_VALUE, 
                1);
        octaveSelect = new JSpinner(octaveModel);
        octaveSelect.addChangeListener(e ->
                setOctave((int)octaveSelect.getValue()));
        selectorPanel.add(octaveSelect);

        signSelect = new JList<Sign>(Sign.values());
        signSelect.addListSelectionListener(e ->
                setSign((Sign)signSelect.getSelectedValue()));
        selectorPanel.add(signSelect);

        // TODO these need to be validated pairs with degree
        // also full text not abbrev (or tooltips?)
        // hierarchical menu? e.g., { minor -> 2, 3, 6, 7 }, 
        //                          { perfect -> ... }
        qualitySelect = new JList<Quality>(Quality.values());
        selectorPanel.add(qualitySelect);

        SpinnerModel degreeModel = new SpinnerNumberModel(1, 1, null, 1);
        degreeSelect = new JSpinner(degreeModel);
        selectorPanel.add(degreeSelect);

        startPitchLabel = new JLabel(this.getStartPitch().toString(), 
                JLabel.CENTER);
        rendererPanel.add(startPitchLabel);

        signLabel = new JLabel(this.getSign().toString(), 
                JLabel.CENTER);
        rendererPanel.add(signLabel);

        intervalLabel = new JLabel(this.getInterval().toString(), 
                JLabel.CENTER);
        rendererPanel.add(intervalLabel);

        // add labels for current selected pitch, sign, and interval
        // sub-buttons to create pitch and interval first?
        // button to check expression first?
        endPitchLabel = new JLabel(END_PITCH_LABEL_DEFAULT, JLabel.CENTER);
        rendererPanel.add(endPitchLabel);

        JButton goButton = new JButton("Calculate");
        goButton.addActionListener(this);
        add(goButton);
    }

    private final String END_PITCH_LABEL_DEFAULT = "= ?";

    private Pitch getStartPitch() {
        return this.startPitch;
    }

    private Sign getSign() {
        return this.sign;
    }

    private Interval getInterval() {
        return this.interval;
    }

    private Pitch getEndPitch() {
        return this.endPitch;
    }

    private void setStartPitch(Pitch pitch) {
        this.startPitch = pitch;
        updateExpression();
    }

    private void setPname(Pname pname) {
        setStartPitch(getStartPitch().copyWith(pname));
    }

    private void setAccid(Accid accid) {
        setStartPitch(getStartPitch().copyWith(accid));
    }

    private void setOctave(int value) {
        Octave newOctave = Octave.of(value);
        setStartPitch(getStartPitch().copyWith(newOctave));
    }

    private void setSign(Sign sign) {
        this.sign = sign;
        updateExpression();
    }

    private void setEndPitch(Pitch pitch) {
        this.endPitch = pitch;
        updateResult();
    }

    private void updateExpression() {
        startPitchLabel.setText(this.getStartPitch().toString());
        signLabel.setText(this.getSign().toString());
        endPitchLabel.setText(END_PITCH_LABEL_DEFAULT);
    }

    private void updateResult() {
        endPitchLabel.setText(" = " + this.getEndPitch().toString());
    }


    public void actionPerformed(ActionEvent e) {
        Pitch result = Pitch.inc(this.getStartPitch(), this.getInterval());
        setEndPitch(result);
        updateResult();
    }

    public static void main(String[] args) {
        IntervalCalcGUI gui = new IntervalCalcGUI();
        gui.setVisible(true);
    }
}
