import java.io.*;
import java.util.Scanner;

public class Planner {
    public static void main(String[] args) {

        try {
            Scanner input = new Scanner(new File("test.txt"));
            input.useDelimiter("-|\n|hrs|mins|V");
            Activity[] activities = new Activity[0]; // set to 0 initially
            double currentTime = 8.50;
            double endTime = 22.00; /* millitary time */

            while (input.hasNext()) { // begin parsing input file for data

                int id = input.nextInt();
                String name = input.next();
                double startRange = input.nextDouble();
                double endRange = input.nextDouble();
                double duration = input.nextDouble();
                boolean allowed = true;
                input.nextLine();

                Activity newActivity = new Activity(name, duration, id, startRange, endRange, allowed, 0, 0);
                activities = addActivity(activities, newActivity); // update array with new activty
            }

            System.out.print("\n");

            /*
            for (int i = 0; i <= activities.length - 1; i++) {
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
            */

            generateSchedule(activities, currentTime);
            checkAllowance(activities, currentTime);

            input.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    private static void checkAllowance(Activity[] activities, double currentTime) {
        /*
         * first check if the activity starts before the selected start-time within
         * planner, 8:30 am
         */
        for (int i = 0; i <= activities.length - 1; i++) {
            if (activities[i].startRange < currentTime) {
                activities[i].allowed = false; /* non valid activity, starts before allowed time */
            }
        }
    }

    private static Activity[] addActivity(Activity[] activities, Activity activityToAdd) {
        Activity[] newActivities = new Activity[activities.length + 1];
        System.arraycopy(activities, 0, newActivities, 0, activities.length);
        newActivities[newActivities.length - 1] = activityToAdd;

        return newActivities;
    }

    public static void generateSchedule(Activity[] activities, double currentTime) {

        for (int i = 0; i < activities.length; i++) {
            if (activities[i].duration > (activities[i].endRange - activities[i].startRange)) {
                System.out.println("Activity " + activities[i].name
                        + " not included in the schedule because the duration is greater than the time range.");
                continue;
            }
            for (int j = i + 1; j < activities.length; j++) {
                Activity a1 = activities[i];
                Activity a2 = activities[j];
                if (a1.startRange == a2.startRange) {
                    double a1End = a1.startRange + a1.duration;
                    double a2End = a2.startRange + a2.duration;
                    if (a1End > a2.endRange) {
                        Activity temp = activities[i];
                        activities[i] = activities[j];
                        activities[j] = temp;
                    } else if (a2End > a1.endRange) {
                        Activity temp = activities[j];
                        activities[j] = activities[i];
                        activities[i] = temp;
                    }
                }
            }

            outputSchedule(activities);
        }
    }

    private static void outputSchedule(Activity[] activities) {
        for (int i = 0; i < activities.length; i++) {
            if (activities[i].startRange < 8.5 || activities[i].endRange > 22.0) {
                System.out.println("Activity " + activities[i].name
                        + " not included in the schedule because it starts before 8.5 or ends after 22.0");
                continue;
            }
            if (activities[i].duration > (activities[i].endRange - activities[i].startRange)) {
                System.out.println("Activity " + activities[i].name
                        + " not included in the schedule because the duration is greater than the time range");
                continue;
            }
            double current = activities[i].startRange;
            for (int j = i + 1; j < activities.length; j++) {
                if (activities[i].startRange == activities[j].startRange) {
                    if (activities[i].duration > activities[j].duration) {
                        activities[j].startRange = activities[i].startRange + activities[i].duration;
                    }
                }
            }
            while (current < activities[i].endRange) {
                System.out.println(current + " - " + activities[i].endRange + ": " + activities[i].name);
                current += activities[i].duration;
                if (current > activities[i].endRange) {
                    break;
                }
            }
        }
    }

    public static String convertStandard(double time) {
        String timeString = "";
        int hours = (int) time;
        int minutes = (int) ((time - hours) * 60);
        if (hours > 12) {
            hours -= 12;
            timeString = hours + ":" + minutes + "pm";
        } else if (hours == 12) {
            timeString = hours + ":" + minutes + "pm";
        } else if (hours == 0) {
            hours = 12;
            timeString = hours + ":" + minutes + "am";
        } else {
            timeString = hours + ":" + minutes + "am";
        }
        return timeString;
    }

    // convert standard time to military time
    public static double convertMilitary(String time) {
        double timeDouble = 0;
        int hours = Integer.parseInt(time.substring(0, 2));
        int minutes = Integer.parseInt(time.substring(3, 5));
        if (time.substring(6, 8).equals("pm")) {
            hours += 12;
        }
        timeDouble = hours + (minutes / 60.0);
        return timeDouble;
    }

    /*
     * NIMRAT'S CODE
     * public static void printSchedule(Activity[] activities) // this method takes
     * in a array of activities and outputs it to the console.
     * {
     * for(Activity actvity : activities) // iterates each activity in the act
     * {
     * outputFormatter(actvity);
     * }
     * }
     * public static void outputFormatter(Activity activity) // prints out the
     * activity for that time slot
     * {
     * 
     * system.out.println("    " +
     * "-------------------------------------------------------");
     * system.out.println("    " + "|");
     * system.out.println("    " + "|");
     * system.out.println("    " + "|"+ " Time: " +
     * convertStandard(activity.actualStart) + "  -  " +
     * convertStandard(activity.actualEnd) );
     * system.out.println("    " + "|");
     * system.out.println("    " + "|" + " Task: " + actvity.name);
     * system.out.println("    " + "|");
     * system.out.println("    " + "|");
     * system.out.println("    " +
     * "--------------------------------------------------------");
     * 
     * 
     * }
     */
}
