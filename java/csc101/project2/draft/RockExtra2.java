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
   public static void main(String[] args) {
      // Instead of an enum, which would be better, we use integer constants
      // for the weapons; we select weapons from character input by finding
      // the index in INITIALS; and we select weapons randomly using a
      // randomly chosen index between 0 and INITIALS.length().
      final int ROCK     = 0;
      final int PAPER    = 1;
      final int SCISSORS = 2;
      final String INITIALS = new String("rps");

      // Welcome, Get user input
      System.out.println("ROCK PAPER SCISSORS");

      Scanner kbScan = new Scanner(System.in);

      System.out.print("How many winning rounds are needed to win the game? ");
      int maxRound = kbScan.nextInt();

      System.out.printf("Okay, first to win %d rounds wins!\n", maxRound);
      System.out.println("Each round, choose Rock, Paper, or Scissors: Press 'r', 'p', or 's'");

      Random randomizer = new Random();
      int userWins = 0;
      int computerWins = 0;
      int ties = 0;
      int round = 0;

      // Game loop: Keep playing until someone wins; count rounds
      while (userWins <= maxRound && computerWins <= maxRound) {
         ++round; // Count rounds from 1
         System.out.printf("\n~~~ ROUND %d ~~~\n\n", round);

         // Get user choice
         System.out.print("Select r, p, or s: ");
         String userInput = kbScan.next().toLowerCase();

         if (!(INITIALS.contains(userInput))) {
            System.err.printf("Invalid input '%s': Try again!\n", userInput);
            --round; // Redo
            continue;
         }
         
         int userWeapon = INITIALS.indexOf(userInput.charAt(0));

         // Make computer choice (random)
         int computerWeapon = randomizer.nextInt(INITIALS.length());

         String userWeaponName = switch (userWeapon) {
            case ROCK  -> "Rock";
            case PAPER -> "Paper";
            default    -> "Scissors";
         };

         String computerWeaponName = switch (computerWeapon) {
            case ROCK  -> "Rock";
            case PAPER -> "Paper";
            default    -> "Scissors";
         };

         System.out.printf("\nYou chose %s, I chose %s\n", 
              userWeaponName, computerWeaponName);

         // Evaluate contest and report results
         if (userWeapon == computerWeapon) {
            ++ties;
            System.out.println("Tie!");
         } else if ((userWeapon == ROCK && computerWeapon == SCISSORS) ||
                    (userWeapon == PAPER && computerWeapon == ROCK)    ||
                    (userWeapon == SCISSORS && computerWeapon == PAPER)) {
            ++userWins;
            System.out.printf("%s beats %s: You win!\n", 
                  userWeaponName, computerWeaponName);
         } else {
            ++computerWins;
            System.out.printf("%s beats %s: You lose!\n",
                  computerWeaponName, userWeaponName);
         }

         // Update game stats
         System.out.printf(
               "\nAfter round %d: You have %d wins, %d losses, %d ties\n", 
               round, userWins, computerWins, ties);
      }

      // Evaluate overall winner
      if (userWins > computerWins) {
         System.out.println("\nYOU WON THE GAME!");
      } else {
         System.out.println("\nYOU LOST THE GAME!");
      }

      System.out.println("\nThank you for playing Rock, Paper, Scissors!");
   }
}


