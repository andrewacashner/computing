package com.andrewcashner.musarithmetic;

/**
 * Every component of a pitch must report its own chromatic offset and
 * represent itself as a standard string and as Lilypond code.
 */
public interface PitchComponent extends Describable {
   
    /** 
     * Chromatic offset of this component 
     *
     * @return Integer value, zero-indexed offset
     */
    int offset12();

    /** 
     * String representation
     *
     * @return String
     */
    String toString();

    /** 
     * Lilypond code representation
     *
     * @return Lilypond string
     */
    String toLy();
}

