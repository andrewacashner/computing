package com.andrewcashner.musarithmetic;

// TODO
// instead of quality (reset degrees) ->  degrees -> octaveInc
// make a tree menu of degree -> quality with sep octaveInc
// so there is never an invalid combination;
// or just a list minor2, major2, aug2, dim3, etc.
import javax.swing.*;
import javax.swing.tree.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import java.util.Hashtable;
import java.util.Arrays;

/**
 * First try at Java GUI
 * @author Andrew Cashner
 * @version Started 2025/01/14
 */
public class IntervalCalcGUI extends JFrame implements ActionListener {
    private Pitch startPitch    = new Pitch();
    private Sign sign           = Sign.DEFAULT;
    private Interval interval   = new Interval();
    private Pitch endPitch      = new Pitch();

    private JPanel selectorPanel, rendererPanel;
    private JList pnameSelect, accidSelect, signSelect;
    private JSpinner octaveSelect, octaveIncSelect;
    private JTree intervalSelect;
    private JLabel startPitchLabel, signLabel, intervalLabel, endPitchLabel;

    // NB 1-indexed for user
    private static final Integer[] legalDegrees(Quality quality) {
        return switch (quality) {
            case PERFECT        -> new Integer[] { 1, 4, 5 };
            case MINOR, MAJOR   -> new Integer[] { 2, 3, 6, 7 };
            case DIMINISHED, 
                 AUGMENTED      -> new Integer[] { 1, 2, 3, 4, 5, 6, 7 };
        };
    }

    private static final Hashtable<Quality, Integer[]> legalIntervals = 
        new Hashtable<>();

    static {
        legalIntervals.put(
                Quality.PERFECT, legalDegrees(Quality.PERFECT));
        legalIntervals.put(
                Quality.MINOR, legalDegrees(Quality.MINOR));
        legalIntervals.put(
                Quality.MAJOR, legalDegrees(Quality.MAJOR));
        legalIntervals.put(
                Quality.DIMINISHED, legalDegrees(Quality.DIMINISHED));
        legalIntervals.put(
                Quality.AUGMENTED, legalDegrees(Quality.AUGMENTED));
    }

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
        class PitchComponentCellRenderer extends JLabel 
                implements ListCellRenderer<Object> {

            public Component getListCellRendererComponent(
                    JList<?> list, 
                    Object value, 
                    int index,
                    boolean isSelected, 
                    boolean cellHasFocus) {
                setText(((Describable)value).description());
                return this;
            }
        }

        pnameSelect = new JList<Pname>(Pname.values());
        pnameSelect.addListSelectionListener(e -> 
            setPname((Pname)pnameSelect.getSelectedValue()));
        pnameSelect.setCellRenderer(new PitchComponentCellRenderer());
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
        accidSelect.setCellRenderer(new PitchComponentCellRenderer());
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

        // TODO continue setting this up
        intervalSelect = new JTree(legalIntervals);
        intervalSelect.addTreeSelectionListener(e ->
                setInterval(intervalSelect.getSelectionPath(),
                            (int)octaveIncSelect.getValue()));
        selectorPanel.add(intervalSelect);
        
        SpinnerModel octaveIncModel = new SpinnerNumberModel(0, 0, null, 1);
        octaveIncSelect = new JSpinner(octaveIncModel);
        octaveIncSelect.addChangeListener(e ->
                setDegree((int)octaveIncSelect.getValue()));
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
        setStartPitch(getStartPitch().copyWith(new Octave(value)));
    }

    private void setSign(Sign sign) {
        this.sign = sign;
        updateExpression();
    }

    private void setDegree(int octaveInc) {
        int newDegree = octaveInc * 7 + this.interval.degree();
        setInterval(getInterval().copyWith(newDegree));
    }

    private void setInterval(Interval interval) {
        this.interval = interval;
        updateExpression();
    }

    private void setInterval(TreePath path, int octaveInc) {
        Object[] selections = path.getPath();
        if (selections.length == 3) {
            Object qualityObj = 
                ((DefaultMutableTreeNode)selections[1]).getUserObject();
            Object degreeObj = 
                ((DefaultMutableTreeNode)selections[2]).getUserObject();
            Quality quality = (Quality)qualityObj;
            // Return to 0 index
            int degree = octaveInc * 7 + (int)degreeObj - 1;
            Interval interval = new Interval(quality, degree);
            setInterval(interval);
        }
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
        Pitch result = this.getStartPitch().inc(this.getInterval());
        setEndPitch(result);
        updateResult();
    }

    public static void main(String[] args) {
        IntervalCalcGUI gui = new IntervalCalcGUI();
        gui.setVisible(true);
    }
}
