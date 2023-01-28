import java.io.*;
import java.util.Scanner;

public class Planner {
    public static void main(String[] args) {

        try {
            Scanner input = new Scanner(new File("test.txt"));
            input.useDelimiter("-|\n");
            Activity[] activities = new Activity[0]; // set to 0 initially
            double minimumTime = 8.30;
            double maximumTime = 22.00; /* millitary time */

            while (input.hasNext()) { // begin parsing input file for data
                String name = input.next();
                double startRange = input.nextDouble();
                double endRange = input.nextDouble();
                String durationInStr = input.next();
                boolean allowed = true;
                input.nextLine();

                startRange = convertTimeRange((int) startRange); // convert start time to decimal
                endRange = convertTimeRange((int) endRange); // convert end time to decimal
                double duration = getDurationInHours(durationInStr); // convert duration to hours

                Activity newActivity = new Activity(name, duration, startRange, endRange, allowed, 0, 0);
                activities = addActivity(activities, newActivity); // update array with new activty
            }

            System.out.print("\n");

            generateSchedule(activities, minimumTime, maximumTime);

            /*
             * for (int i = 0; i <= activities.length - 1; i++) {
             * System.out.print(activities[i].name);
             * System.out.print("\n");
             * System.out.print(activities[i].duration);
             * System.out.print("\n");
             * System.out.print(activities[i].id);
             * System.out.print("\n");
             * System.out.print(activities[i].startRange);
             * System.out.print("\n");
             * System.out.print(activities[i].endRange);
             * System.out.print("\n");
             * System.out.print("\n");
             * 
             * }
             */

            input.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    public static void generateSchedule(Activity[] activities, double minimumTime, double maximumTime) {
        // Sort the activities by their start time
        java.util.Arrays.sort(activities, (a1, a2) -> {
            if (a1.getStartRange() == a2.getStartRange()) {
                return Double.compare(a1.getEndRange(), a2.getEndRange());
            } else {
                return Double.compare(a1.getStartRange(), a2.getStartRange());
            }
        });

        // Check the activities for any issues
        checkAllowance(activities, minimumTime, maximumTime);

        // Initialize the schedule
        Activity[] schedule = new Activity[activities.length];
        double scheduledTime = activities[0].getStartRange();
        int index = 0;
        schedule[index] = activities[0];
        schedule[index].actualStart = scheduledTime;
        schedule[index].actualEnd = scheduledTime + activities[0].getDuration();
        scheduledTime += activities[0].getDuration();
        index++;

        // Iterate through the activities
        for (int i = 1; i < activities.length; i++) {
            Activity a = activities[i];
            if (scheduledTime < a.getStartRange()) {
                scheduledTime = a.getStartRange();
            }
            double start = scheduledTime;

            if (scheduledTime + a.getDuration() > a.getEndRange()) {
                activities[i].allowed = false;
            } else {
                double end = scheduledTime + a.getDuration();
                if (start < end) {
                    if (scheduledTime > a.getEndRange()) {
                        a.actualStart = a.getStartRange();
                        a.actualEnd = a.getEndRange();
                    } else {
                        a.actualStart = start;
                        a.actualEnd = end;
                        scheduledTime = end;
                        schedule[index] = a;
                        index++;
                    }                    
                }
            }
        }

        // Print the schedule
        printSchedule(activities);

    }

    public static void printSchedule(Activity[] activities) {
        for (int i = 0; i < activities.length; i++) {
            if (activities[i].allowed) {
                System.out.println(activities[i].getName() + ": " + activities[i].actualStart + " - "
                        + activities[i].actualEnd);
            }
        }
    }

    // convert the values of the time ranges from military time to decimal
    public static double convertMilitaryToDecimal(int time) {
        int hours = time / 100;
        int mins = time % 100;
        return hours + (double) mins / 60;
    }

    // convert the values of the time ranges from decimal to military time
    public static int convertDecimalToMilitary(int time) {
        int hours = time / 100;
        int mins = time % 100;
        return hours * 100 + mins;
    }

    // convert the duration into hours if it is in minutes
    public static double getDurationInHours(String duration) {
        String[] arr = duration.split(" ");
        double durationInHours;
        String mins = "mins";

        if (arr[1].trim().equals(mins)) {
            durationInHours = Double.parseDouble(arr[0]) / 60;
        } else {
            durationInHours = Double.parseDouble(arr[0]);
        }
        return durationInHours;
    }

    /*
     * check if the activity starts before the selected start-time within planner,
     * or after the selected end-time within planner. Also check if the duration of
     * the activity is greater than the time range. If so, the activity is not
     * allowed to be added to the schedule.
     */
    private static void checkAllowance(Activity[] activities, double minimumTime, double maximumTime) {
        for (int i = 0; i <= activities.length - 1; i++) {
            if (activities[i].startRange < minimumTime || activities[i].endRange > maximumTime) {
                activities[i].allowed = false;
            }
            if (activities[i].duration > (activities[i].endRange - activities[i].startRange)) {
                activities[i].allowed = false;
            }
        }
    }

    // add new activity to the array of activities
    private static Activity[] addActivity(Activity[] activities, Activity activityToAdd) {
        Activity[] newActivities = new Activity[activities.length + 1];
        System.arraycopy(activities, 0, newActivities, 0, activities.length);
        newActivities[newActivities.length - 1] = activityToAdd;

        return newActivities;
    }

    // convert military time to standard time
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
}
