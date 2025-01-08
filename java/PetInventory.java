import java.util.*;
import java.util.stream.*;
import java.util.function.*;

class PetInventory {

    public static void main(String[] args) {

        String[] dogNames = new String[] {
            "Bowser",
            "Rex",
            "Professor McMann"
        };

        String[] catNames = new String[] {
            "Fluffy",
            "Miss Thang",
            "Monique"
        };


        Stream<Dog> dogs = Arrays.stream(dogNames).map(Dog::new);
        Stream<Cat> cats = Arrays.stream(catNames).map(Cat::new);

        //// Another approach
        //   enum PetType { DOG, CAT };
        //
        //   Map<PetType, String[]> petInventory = Map.of(
        //           PetType.DOG, 
        //           new String[] {"Bowser", "Rex", "Professor McMann"},
        //
        //           PetType.CAT, 
        //           new String[] { "Fluffy", "Miss Thang", "Monique"});
        //
        //   Stream<Dog> dogs = Arrays.stream(petInventory.get(PetType.DOG))
        //       .map(Dog::new);
        //
        //   Stream<Cat> cats = Arrays.stream(petInventory.get(PetType.CAT))
        //       .map(Cat::new);

        List<Pet> pets = Stream.concat(dogs, cats)
            .collect(Collectors.toList());

        pets.add(new Pet("Baron von Fiend"));

        Collections.shuffle(pets);

        String petRollCall = pets.stream()
            .map(p -> String.format("%s says \"%s\"!", p, p.speak()))
            .collect(Collectors.joining("\n"));

        System.out.println(petRollCall);
    }
}


class Pet {
    private String name;
    protected String says = "?";

    public Pet(String name) {
        this.name = name;
    }

    public Pet(String name, String says) {
        this(name);
        this.says = says;
    }

    public String speak() {
        return this.says;
    }

    public String toString() {
        return this.name;
    }
}

class Dog extends Pet {
    public Dog(String name) {
        super(name, "Woof");
    }
}

class Cat extends Pet {
    public Cat(String name) {
        super(name, "Meow");
    }
}



