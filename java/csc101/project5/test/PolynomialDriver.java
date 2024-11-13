//Test Driver for polynomial project
//As you build your class comment out parts of the switch
//so that you can check each case

//Build the constructors and the toString methods first
//Build stub methods for others not working yet

import java.util.Scanner;
import java.util.InputMismatchException;

class PolynomialDriver {
   public static void main(String args []) {
      Scanner input = new Scanner(System.in);
      System.out.println("##Constructor Tests##");
      //Test default constructor
      Polynomial testValue1 = new Polynomial();
      //Test toString method
      System.out.println("Your test polynomial is " + testValue1); //should be f(x) = 0.0
      //Test constructor
      Polynomial testValue2 = new Polynomial("0 1 -3 0 1 2 0 0");
      System.out.println("Your 2nd test polynomial is " + testValue2);
      System.out.println("It's degree is : " + testValue2.getDegree());  // should be 5
      System.out.println();
      //Loop to repeat menu until quit is chosen
      int choice = 0;
      while (choice != 7) {
         choice = menu();
         switch(choice) {
            case 1:
               System.out.println("Display Polynomials");
               System.out.println("#1  :  " + testValue1);
               System.out.println("#2  :  " + testValue2);
               System.out.println();
               break;
            case 2: //input values with single spaces between values
               System.out.println("Input New Polynomials");
               String coeff;
               System.out.print("Enter values for first Polynomial :");
               coeff = input.nextLine();
               testValue1.setCoeff(coeff);
               System.out.println();
               System.out.print("Enter values for second Polynomial :");
               coeff = input.nextLine();
               testValue2.setCoeff(coeff);
               System.out.println();
               break;
            case 3: //call add method
               System.out.println("Add Polynomials");
               System.out.println("The Sum of \t" + testValue1);
               System.out.println("    and    \t" + testValue2);
               System.out.println("    is :   \t" + testValue1.add(testValue2));
               System.out.println();
               break;
            case 4:
               System.out.println("Evaluate Polynomial #1");
               System.out.println("Enter a value for x to evaluate f(x) for Polynomial #1");
               double value;
               value = input.nextDouble();
               input.nextLine(); //empty the buffer
               System.out.println("When x = " + value + " for " + testValue1);
               System.out.println("f(x) = " + testValue1.evaluate(value));
               System.out.println();
               break;
            case 5:
               System.out.println("Derivative of Polynomial #1");
               System.out.println("The derivative of " + testValue1);
               System.out.println("is  : " + testValue1.derivative());
               System.out.println();
               break;
            case 6:
               System.out.println("Find Root of Polynomial #1");
               System.out.println("Enter a value to be the initial guess for a root as");
               System.out.println("needed to perform the Newton-Raphson method : ");
               value = input.nextDouble();
               input.nextLine(); //empty the buffer
               System.out.println("Root of  " + testValue1);
               //Note : my findRoot method throws an IllegalStateException in the case that 
               // 1000 iterations failed to converge to a root value
               try {
                  double root = testValue1.findRoot(value);
                  System.out.println("found at x = " + root);
               }
               catch (IllegalStateException e) {
                  System.out.println("was unable to be determined.");
               }
               System.out.println();               
               break;
            case 7:
               System.out.println("I hope that you enjoyed this program!");
          }
      }// end while
   
   }
   
   //Method to call menu
   //Exceptions not set up here.  Only integers will work
   public static int menu() {
      int choice = 0;
      Scanner input = new Scanner(System.in);
      while(choice < 1 || choice > 7) {
         System.out.println("Please choose from the following: ");
         System.out.println("\t#1 Display two polynomials stored");
         System.out.println("\t#2 Input two new polynomials");
         System.out.println("\t#3 Add polynomials");
         System.out.println("\t#4 Evaluate polynomial #1 with a specific value");
         System.out.println("\t#5 Show Derivative of polynomial #1");
         System.out.println("\t#6 Find root of polynomial #1 with initial guess");
         System.out.println("\t#7 End program");
         System.out.print("Choice : ");
         try {
            choice = input.nextInt();
         }
         catch(InputMismatchException e) {
            System.out.println("Please enter a number!");
         }
         input.nextLine(); //clear buffer
         System.out.println();
      }
      return choice;
   }
} //end class