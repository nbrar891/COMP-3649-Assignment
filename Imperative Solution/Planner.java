package dailyplanner;
import java.io.File;  // Import the File class
import java.io.FileNotFoundException;  // Import this class to handle errors
import java.util.Scanner; // Import the Scanner class to read text files

public class Planner {

    public static void main(String[] args) {
        try {
            Scanner input = new Scanner (new File("Test.txt"));
            input.useDelimiter("-|\n|hrs|min|V");
            Activity[] activities = new Activity[0]; //set to 0 initially
            double currentTime = 8.30;



            while(input.hasNext()) { //begin parsing input file for data

                int id = input.nextInt();
                String name = input.next();
                double startRange = input.nextDouble();
                double endRange = input.nextDouble();
                double duration = input.nextDouble();
                boolean allowed = true;
                input.nextLine();

                Activity newActivity = new Activity(name,duration,id,startRange,endRange, allowed);
                activities = addActivity(activities, newActivity); //update array with new activty
            }


            System.out.print("\n");

               for(int i =0; i <= activities.length-1; i++) {
                   System.out.print(activities[i].name);
                   System.out.print("\n");
                   System.out.print(activities[i].duration);
                   System.out.print("\n");
                   System.out.print(activities[i].id);
                   System.out.print("\n");
                   System.out.print(activities[i].startRange);
                   System.out.print("\n");
                   System.out.print(activities[i].endRange);
                   System.out.print("\n");
                   System.out.print("\n");


               }

               checkAllowance(activities, currentTime);


            input.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    private static Activity[] addActivity(Activity[] activities, Activity activityToAdd) {
        Activity[] newActivities = new Activity[activities.length + 1];
        System.arraycopy(activities, 0, newActivities, 0, activities.length);
        newActivities[newActivities.length - 1] = activityToAdd;

        return newActivities;
    }

    private static int solution (Activity[] activities, double currentTime) {

        return 0;
    }

    public static void checkAllowance(Activity[] activities, double currentTime) {
        /* first check if the activity starts before the selected start-time within planner, 8:30 am*/
        for (int i = 0; i <= activities.length-1; i++) {
            if (activities[i].startRange < currentTime) {
                activities[i].allowed = false;
            }
        }
    }
}
