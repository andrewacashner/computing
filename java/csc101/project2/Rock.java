/* # Rock
 *
 * | Andrew Cashner, `acashner@student.monroecc.edu`
 * | CSC 101, Project 2
 * | 2024/10/08
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

      // Welcome, Get user input
      System.out.print("ROCK PAPER SCISSORS\n");

      Scanner kbScan = new Scanner(System.in);
      int maxRound = getMaxRounds(kbScan);
      System.out.print(instructions(maxRound));

      Random randomizer = new Random();
      int userWins = 0;
      int computerWins = 0;
      int ties = 0;
      String userInput = new String();

      // Game loop: Keep playing until someone wins or user presses q; 
      // count rounds
      for (int round = 1;
            !userInput.equals("q") &&
            userWins < maxRound && computerWins < maxRound;
            ++round) {

         // Get and validate user's choice
         System.out.printf(roundHeader(round));
         userInput = getUserCommand(kbScan);

         Weapon userWeapon;
         try {
            userWeapon = Weapon.fromAbbrev(userInput.charAt(0));
         } catch (Error e) { 
            // Redo this round if invalid input
            System.out.printf("%s: Try again!\n\n", e.getMessage());
            --round; 
            continue;
         }

         // Make and report computer's choice 
         Weapon computerWeapon = Weapon.random(randomizer);
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

      // Evaluate and report overall winner
      System.out.print(gameResult(userWins, computerWins));
      System.out.print("Thank you for playing Rock, Paper, Scissors!\n");
   }

   // The Weapon enum makes it possible to create Weapon choices from user
   // input, compare them, and to convert them to strings for output
   public static enum Weapon {
      ROCK      ("Rock"), 
      PAPER     ("Paper"),
      SCISSORS  ("Scissors");

      private String label;

      private Weapon(String label) {
         this.label = label;
      }

      // Select Weapon from user input
      static Weapon fromAbbrev(char abbrev) {
         return switch (abbrev) {
            case 'r' -> ROCK;
            case 'p' -> PAPER;
            case 's' -> SCISSORS;
            default -> 
               throw new Error("Unrecognized weapon abbreviation");
         };
      }

      // Select random Weapon
      static Weapon random(Random randomizer) {
         int weaponIndex = randomizer.nextInt(Weapon.values().length);
         return Weapon.values()[weaponIndex];
      }

      public String toString() {
         return this.label;
      }

      // Does this Weapon win versus the given one?
      public boolean beats(Weapon opponent) {
         return 
            (this == ROCK     && opponent == SCISSORS) ||
            (this == PAPER    && opponent == ROCK)     ||
            (this == SCISSORS && opponent == PAPER);
      }
   }

   // Get user input for number of rounds to play
   public static int getMaxRounds(Scanner kbScan) {
      System.out.print("How many winning rounds are needed to win the game? ");
      return kbScan.nextInt();
   }

   // Print game instructions including number of rounds from user
   public static String instructions(int rounds) {
      return String.format("Okay, first to win %d rounds wins!\n", rounds)
         + "Each round, choose Rock, Paper, or Scissors: "
         + "Press 'r', 'p', or 's'\n\n";
   }

   public static String roundHeader(int round) {
      return String.format(" ~~~ ROUND %d ~~~\n\n", round);
   }
   
   // Get user command for rock, paper, or scissors (or quit)
   public static String getUserCommand(Scanner kbScan) {
      System.out.print("Select r, p, or s (Press q to quit): ");
      return kbScan.next().toLowerCase();
   }
  
   // Concluding message reporting who won
   public static String gameResult(int userWins, int computerWins) {
      String outcome = userWins > computerWins ? "WON" : "LOST";
      return String.format("YOU %s THE GAME!\n\n", outcome);
   }


}


