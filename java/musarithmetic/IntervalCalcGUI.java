package com.andrewcashner.musarithmetic;

// TODO
// instead of quality (reset degrees) ->  degrees -> octaveInc
// make a tree menu of degree -> quality with sep octaveInc
// so there is never an invalid combination;
// or just a list minor2, major2, aug2, dim3, etc.
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

    private JList pnameSelect, accidSelect, signSelect;
    private JList qualitySelect, degreeSelect;
    private JSpinner octaveSelect, octaveIncSelect;
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

        // SELECTORS
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

        // Quality, degree, and octave inc are linked 
        qualitySelect = new JList<Quality>(Quality.values());
        qualitySelect.addListSelectionListener(e -> 
                setQualityLimitDegree(
                    (Quality)qualitySelect.getSelectedValue()));
        selectorPanel.add(qualitySelect);

        degreeSelect = new JList<Integer>(
                legalDegrees(getInterval().quality()));

        SpinnerModel octaveIncModel = new SpinnerNumberModel(0, 0, null, 1);
        octaveIncSelect = new JSpinner(octaveIncModel);
        
        degreeSelect.addListSelectionListener(e ->
                setDegree((int)degreeSelect.getSelectedValue(),
                    (int)octaveIncSelect.getValue()));
        selectorPanel.add(degreeSelect);
        
        octaveIncSelect.addChangeListener(e ->
                setDegree((int)degreeSelect.getSelectedValue(),
                    (int)octaveIncSelect.getValue()));
        selectorPanel.add(octaveIncSelect);

        // LABELS
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

    // NB 1-indexed for user
    private Integer[] legalDegrees(Quality quality) {
        return switch (quality) {
            case PERFECT        -> new Integer[] { 1, 4, 5 };
            case MINOR, MAJOR   -> new Integer[] { 2, 3, 6, 7 };
            case DIMINISHED, 
                 AUGMENTED      -> new Integer[] { 1, 2, 3, 4, 5, 6, 7 };
        };
    }

    private void setQualityLimitDegree(Quality quality) {

        // TODO unchecked warning (is JList<Integer> type being erased?)
        Integer[] degrees = legalDegrees(quality);
        degreeSelect.setListData(degrees);
        
        if (Interval.isValid(quality, getInterval().degree())) {
            setInterval(getInterval().copyWith(quality));
        } else {
            // The displayed degrees are 1-indexed
            setInterval(new Interval(quality, degrees[0] - 1));
        } 

        updateExpression();
    }

    private void setDegree(int degreeOneIndexed, int octaveInc) {
        int newDegree = octaveInc * 7 + degreeOneIndexed - 1;
        setInterval(getInterval().copyWith(newDegree));
    }

    private void setInterval(Interval interval) {
        this.interval = interval;
        updateExpression();
    }

    private void setEndPitch(Pitch pitch) {
        this.endPitch = pitch;
        updateResult();
    }

    private void updateExpression() {
        startPitchLabel.setText(getStartPitch().toString());
        signLabel.setText(getSign().toString());
        intervalLabel.setText(getInterval().toString());
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
