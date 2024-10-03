/* # Rock
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC 101, Project 2
 * | 2024/10/02
 *
 * ## Description
 *
 * Simulate a game of Rock, Paper, Scissors.
 *
 * The user specifies how many winning rounds are needed to win the game.
 * The program plays against the user and tracks number of wins, losses, and
 * ties.
 * The program ends when one player wins.
 * 
 * 
 * ### Scoring
 *
 * If both choose the same item, the result is a tie.
 * Rock beats Scissors, Scissors beats Paper, Paper beats Rock.
 *
 * ### Limitations
 *
 * This program would be much better if I could use an enum with values ROCK,
 * PAPER, and SCISSORS, for both comparison values and output strings.
 *
 * As with the other programs, repeated code should instead be made into
 * function calls.
 */

import java.util.Scanner;
import java.util.Random;

class Rock {
   public static void main(String[] args) {
      enum Weapon {
          ROCK      ("Rock"), 
          PAPER     ("Paper"),
          SCISSORS  ("Scissors");

          private String label;

          private Weapon(String label) {
              this.label = label;
          }

          static Weapon fromAbbrev(char abbrev) {
              return switch (abbrev) {
                  case 'r' -> ROCK;
                  case 'p' -> PAPER;
                  case 's' -> SCISSORS;
                  default -> 
                      throw new Error("Unrecognized weapon abbreviation");
              };
          }

          public String toString() {
              return this.label;
          }

          public boolean beats(Weapon opponent) {
              return 
                  (this == ROCK && opponent == SCISSORS) ||
                  (this == PAPER && opponent == ROCK)    ||
                  (this == SCISSORS && opponent == PAPER);
          }
      }

      // Welcome, Get user input
      System.out.print("ROCK PAPER SCISSORS\n");

      Scanner kbScan = new Scanner(System.in);

      System.out.print("How many winning rounds are needed to win the game? ");
      int maxRound = kbScan.nextInt();

      System.out.printf("Okay, first to win %d rounds wins!\n", maxRound);
      System.out.print("Each round, choose Rock, Paper, or Scissors: Press 'r', 'p', or 's'\n\n");

      Random randomizer = new Random();
      int userWins = 0;
      int computerWins = 0;
      int ties = 0;
      int round = 0;

      String userInput = new String();

      // Game loop: Keep playing until someone wins or user presses q; 
      // count rounds
      while (!userInput.equals("q") &&
            userWins < maxRound 
            && computerWins < maxRound) {

         ++round; // Count rounds from 1
         System.out.printf(" ~~~ ROUND %d ~~~\n\n", round);

         // Get user choice
         System.out.print("Select r, p, or s (Press q to quit): ");
         userInput = kbScan.next().toLowerCase();
         
         Weapon userWeapon;
         try {
             userWeapon = Weapon.fromAbbrev(userInput.charAt(0));
         } catch (Error e) {
             System.out.printf("%s: Try again!\n\n", e.getMessage());
             --round; // Redo this round
             continue;
         }

         // Make computer choice (random)
         int weaponIndex = randomizer.nextInt(Weapon.values().length);
         Weapon computerWeapon = Weapon.values()[weaponIndex];

         System.out.printf("\nYou chose %s, I chose %s\n", 
              userWeapon, computerWeapon);

         // Evaluate contest and report results
         if (userWeapon == computerWeapon) {
            ++ties;
            System.out.print("Tie!\n\n");
         } else if (userWeapon.beats(computerWeapon)) {
            ++userWins;
            System.out.printf("%s beats %s: You win!\n\n", 
                  userWeapon, computerWeapon);
         } else {
            ++computerWins;
            System.out.printf("%s beats %s: You lose!\n\n",
                  computerWeapon, userWeapon);
         }

         // Update game stats
         System.out.printf(
               "After round %d: You have %d wins, %d losses, %d ties\n\n", 
               round, userWins, computerWins, ties);
      }

      // Evaluate overall winner
      if (userWins > computerWins) {
         System.out.print("YOU WON THE GAME!\n\n");
      } else {
         System.out.print("YOU LOST THE GAME!\n\n");
      }

      System.out.print("Thank you for playing Rock, Paper, Scissors!\n");
   }
}


