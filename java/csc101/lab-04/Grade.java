/* GRADE
 *
 * Given a score out of 100, report a letter grade (pure letters, not minus
 * or plus). We are assuming the input is an integer.
 *
 * We calculate the score two different ways: first with an if/else change,
 * second with a switch statement.
 * 
 * Scale: 90-100 A
 *        80-89  B
 *        70-79  C
 *        60-69  D
 *         0-59  F
 *
 * CSC 101, Lab 4
 * Andrew Cashner, acashner@student.monroecc.edu
 * 2024/09/26
 */

import java.util.Scanner;

class Grade {
   public static void main(String[] args) {

      // Welcome and get user input
      System.out.println("GRADE");
      System.out.print("Enter a score out of 100 to see your letter grade (no decimals): ");

      Scanner kbScan = new Scanner(System.in);
      int score = kbScan.nextInt();

      /* Calculate grade: Method 1 with if/else chain
       *      By starting at the low end of the scores we can skip checking
       *      for the lower end of the range in subsequent comparisons.
       */
      char letterGrade;
      if (score < 60) {
         letterGrade = 'F';
      } else if (score < 70) {
         letterGrade = 'D';
      } else if (score < 80) {
         letterGrade = 'C';
      } else if (score < 90) {
         letterGrade = 'B';
      } else {
         letterGrade = 'A';
      }

      System.out.printf("For a score of %d you earned %c.\n", 
            score, letterGrade);

      /* Method 2 with switch statement
       *      This time it makes more sense to go in descending order so
       *      that we can leave all the cases where score < 60 as the
       *      default case for 'F'.
       */
      int scoreRange = score / 10; // E.g., all scores between 80 and 89
                                   // become 8

      switch (scoreRange) {
         case 10:
         case 9:
            letterGrade = 'A';
            break;
         case 8:
            letterGrade = 'B';
            break;
         case 7:
            letterGrade = 'C';
            break;
         case 6: 
            letterGrade = 'D';
            break;
         default:
            letterGrade = 'F';
            break;
      }

      System.out.printf("For a score of %d you earned %c.\n", 
            score, letterGrade);

   }
}


