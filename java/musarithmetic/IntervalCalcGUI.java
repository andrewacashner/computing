package com.andrewcashner.musarithmetic;

// TODO
// DEBUG
// interaction between sign, interval degree, octaveInc
// - not working for negative intervals
//      - (REALLY should include sign with interval anyway)
// - octaveInc doesn't ever decrement
//
// LAYOUT
// Layout two full-width panels
// Interval label-tree layout
// Interval menu style instead of tree
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
    private JComboBox pnameSelect, accidSelect, signSelect;
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
        setSize(700, 400);
        
        // TODO layout manager (GridBag?)
        selectorPanel = new JPanel();
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

        pnameSelect = new JComboBox<Pname>(Pname.values());
        pnameSelect.addItemListener(e -> 
            setPname((Pname)pnameSelect.getSelectedItem()));
        pnameSelect.setRenderer(new PitchComponentCellRenderer());
        Box pnameBox = new Box(BoxLayout.Y_AXIS);
        pnameBox.add(new JLabel("Pitch Name"));
        pnameBox.add(pnameSelect);
        selectorPanel.add(pnameBox);

        accidSelect = new JComboBox<Accid>(new Accid[] {
            Accid.NATURAL,
            Accid.FLAT,
            Accid.DBL_FLAT,
            Accid.SHARP,
            Accid.DBL_SHARP
        });
        accidSelect.addItemListener(e ->
            setAccid((Accid)accidSelect.getSelectedItem()));
        accidSelect.setRenderer(new PitchComponentCellRenderer());
        Box accidBox = new Box(BoxLayout.Y_AXIS);
        accidBox.add(new JLabel("Accidental"));
        accidBox.add(accidSelect);
        selectorPanel.add(accidBox);

        SpinnerModel octaveModel = new SpinnerNumberModel(
                Octave.DEFAULT_VALUE, 
                Octave.MIN_VALUE, 
                Octave.MAX_VALUE, 
                1);
        octaveSelect = new JSpinner(octaveModel);
        octaveSelect.addChangeListener(e ->
                setOctave((int)octaveSelect.getValue()));
        Box octaveBox = new Box(BoxLayout.Y_AXIS);
        octaveBox.add(new JLabel("Octave"));
        octaveBox.add(octaveSelect);
        selectorPanel.add(octaveBox);

        signSelect = new JComboBox<Sign>(Sign.values());
        signSelect.addItemListener(e ->
                setSign((Sign)signSelect.getSelectedItem()));
        selectorPanel.add(signSelect);

        // TODO menu style instead of tree?
        // TODO sort the hashtable or build the tree in desired order
        intervalSelect = new JTree(legalIntervals);
        intervalSelect.addTreeSelectionListener(e ->
                setInterval(intervalSelect.getSelectionPath(),
                            (int)octaveIncSelect.getValue(),
                            (Sign)signSelect.getSelectedItem()));
        // TODO can't put in box without clipping display of tree
        JPanel intervalPanel = new JPanel();
        intervalPanel.add(new JLabel("Interval Quality and Base Degree"));
        intervalPanel.add(intervalSelect);
        selectorPanel.add(intervalPanel);
        
        SpinnerModel octaveIncModel = new SpinnerNumberModel(0, 0, null, 1);
        octaveIncSelect = new JSpinner(octaveIncModel);
        octaveIncSelect.addChangeListener(e ->
                setDegree((int)octaveIncSelect.getValue(),
                    (Sign)signSelect.getSelectedItem()));
        Box octaveIncBox = new Box(BoxLayout.Y_AXIS);
        octaveIncBox.add(new JLabel("Add Octaves"));
        octaveIncBox.add(octaveIncSelect);
        selectorPanel.add(octaveIncBox);

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
        setInterval(getInterval().signed(sign));
        updateExpression();
    }

    private void setDegree(int octaveInc, Sign sign) {
        int newDegree = octaveInc * 7 + this.interval.degree();
        setInterval(getInterval().copyWith(newDegree).signed(sign));
    }

    private void setInterval(Interval interval) {
        this.interval = interval;
        System.out.format("Interval set to %s\n", interval);
        updateExpression();
    }

    private void setInterval(TreePath path, int octaveInc, Sign sign) {
        if (path != null) {
            Object[] selections = path.getPath();
            if (selections.length == 3) {
                Object qualityObj = ((DefaultMutableTreeNode)selections[1])
                                    .getUserObject();
                Object degreeObj = ((DefaultMutableTreeNode)selections[2])
                                    .getUserObject();

                Quality quality = (Quality)qualityObj;
                // Return to 0 index and apply sign
                int degree = sign.apply(octaveInc * 7 + (int)degreeObj - 1);

                setInterval(new Interval(quality, degree));
            }
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
