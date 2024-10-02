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
 */

import java.util.Scanner;
import java.util.Random;

class Rock {
   // We will use the default values of this enum to compare weapon
   // choices; we have to add custom labels.
   enum Weapon {
      ROCK     ("Rock"), 
      PAPER    ("Paper"), 
      SCISSORS ("Scissors");

      private String label;

      private Weapon(String label) {
         this.label = label;
      }

      public String toString() {
         return this.label;
      }
   }

   static Weapon randomWeapon(Random randomizer) {
      int value = randomizer.nextInt(Weapon.values().length);
      return Weapon.values()[value];
   }

   static Weapon weaponFromInitial(char initial) {
      return switch (initial) {
         case 'r' -> Weapon.ROCK;
         case 'p' -> Weapon.PAPER;
         case 's' -> Weapon.SCISSORS;
         default -> throw new Error("Invalid Weapon input");
      };
   }

   public static void main(String[] args) {

      // Welcome, Get user input
      System.out.println("ROCK PAPER SCISSORS");

      Scanner kbScan = new Scanner(System.in);

      System.out.print("How many winning rounds are needed to win the game? ");
      int maxRound = kbScan.nextInt() + 1;

      System.out.printf("Okay, first to win %d rounds wins!\n", maxRound);
      System.out.println("Each round, choose Rock, Paper, or Scissors: Press 'r', 'p', or 's'");

      Random randomizer = new Random();
      int userWins = 0;
      int computerWins = 0;
      int ties = 0;
      int round = 0;

      // Game loop: Keep playing until someone wins; count rounds
      while (userWins < maxRound && computerWins < maxRound) {
         ++round; // Count rounds from 1
         System.out.printf("\n~~~ ROUND %d ~~~\n\n", round);

         // Get user choice
         System.out.print("Select r, p, or s: ");
         String userInputStr = kbScan.next().toLowerCase();

         Weapon userWeapon;
         try {
            userWeapon = Rock.weaponFromInitial(userInputStr.charAt(0));
         } catch (Error e) {
            System.err.printf("%s: Try again\n", e.getMessage());
            --round;
            continue;
         }

         // Make computer choice
         Weapon computerWeapon = Rock.randomWeapon(randomizer);

         System.out.printf("\nYou chose %s, I chose %s\n", 
               userWeapon, computerWeapon);

         // Evaluate contest and report results
         if (userWeapon == computerWeapon) {
            ++ties;
            System.out.println("Tie!");
         } else if (
               (userWeapon == Weapon.ROCK
                && computerWeapon == Weapon.SCISSORS) ||
               (userWeapon == Weapon.PAPER
                && computerWeapon == Weapon.ROCK)     ||
               (userWeapon == Weapon.SCISSORS
                && computerWeapon == Weapon.PAPER)
               ) {
            ++userWins;
            System.out.printf("%s beats %s: You win!\n", 
                  userWeapon, computerWeapon);
         } else {
            ++computerWins;
            System.out.printf("%s beats %s: You lose!\n",
                  computerWeapon, userWeapon);
         }

         // Update game stats
         System.out.printf(
               "\nAfter round %d: You have %d wins, %d losses, %d ties\n", 
               round, userWins, computerWins, ties);
      }

      // Evaluate overall winner
      if (userWins > computerWins) {
         System.out.println("\nYOU WIN THE GAME!");
      } else {
         System.out.println("\nYOU LOST THE GAME!");
      }

      System.out.println("\nThank you for playing Rock, Paper, Scissors!");
   }
}


