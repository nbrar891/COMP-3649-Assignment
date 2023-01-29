import java.io.*;
import java.util.Scanner;
import java.util.Arrays;
import java.util.Comparator;

public class Planner {
    public static void main(String[] args) {

        try {
            Scanner input = new Scanner(new File("test.txt"));
            input.useDelimiter("-|\n");
            Activity[] activities = new Activity[0]; // set to 0 initially
            double minimumTime = 8.30;
            double maximumTime = 22.00;

            while (input.hasNext()) { // begin parsing input file for data
                String name = input.next();
                double startRange = input.nextDouble();
                double endRange = input.nextDouble();
                String durationInStr = input.next();
                boolean allowed = true;
                String[] tokens = input.nextLine().trim().split("-");
                int priority = Integer.parseInt(tokens[tokens.length-1]);

                startRange = convertMilitaryToDecimal((int) startRange); // convert start time to decimal
                endRange = convertMilitaryToDecimal((int) endRange); // convert end time to decimal
                double duration = getDurationInHours(durationInStr); // convert duration to hours

                Activity newActivity = new Activity(name, duration, startRange, endRange, allowed, 0, 0, priority);
                activities = addActivity(activities, newActivity); // update array with new activty
            }

            System.out.print("\n");

            // create a new array of activities that are allowed to be scheduled
            Activity[] allowedActivities = createAllowedActivitiesOnlyArray(activities, minimumTime, maximumTime);

            System.out.print("\n");

            generateSchedule(allowedActivities);

            input.close();

        } catch (FileNotFoundException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    /*
     * createAllowedActivitiesOnlyArray()
     * 
     * This method creates a new array of activities that are allowed to be
     * scheduled
     * and removes the activities that are not allowed to be scheduled.
     * It will also sort the activities by their start time.
     */
    public static Activity[] createAllowedActivitiesOnlyArray(Activity[] activities, double minimumTime,
            double maximumTime) {
        // check if the activities are allowed to be scheduled
        checkAllowance(activities, minimumTime, maximumTime);

        Activity[] allowedActivities = new Activity[0];
        for (int i = 0; i < activities.length; i++) {
            if (activities[i].allowed) {
                allowedActivities = addActivity(allowedActivities, activities[i]);
            } else {
                System.out.println(activities[i].getName() + " is not allowed to be scheduled.");
            }
        }

        /*
         * Organize the activities by their end time. The lower end time goes first.
         * If the end times are the same, then organize based on start time.
         */
        Arrays.sort(allowedActivities, new Comparator<Activity>() {
            @Override
            public int compare(Activity a1, Activity a2) {
                if (a1.getEndRange() == a2.getEndRange()) {
                    return Double.compare(a1.getStartRange(), a2.getStartRange());
                } else {
                    return Double.compare(a1.getEndRange(), a2.getEndRange());
                }
            }
        });

        return allowedActivities;
    }

    public static void generateSchedule(Activity[] activities) {
        // Initialize the schedule
        Activity[] schedule = new Activity[activities.length];
        double scheduledTime = activities[0].getStartRange();
        int index = 0;
        schedule[index] = activities[0];
        schedule[index].actualStart = scheduledTime;
        schedule[index].actualEnd = scheduledTime + activities[0].getDuration();
        schedule[index].priority = activities[0].getPriority();
        scheduledTime += activities[0].getDuration();
        index++;

        // Iterate through the activities
        for (int i = 1; i < activities.length; i++) {
            Activity a = activities[i];

            /*
             * If the current activity's start range is higher than the previous activity,
             * then set the current activity's start time to the current activity's start
             * range.
             */
            if (scheduledTime < a.getStartRange()) {
                scheduledTime = a.getStartRange();
            }
            // set the start time of current activity to scheduledTime, and change later if
            // needed.
            double start = scheduledTime;

            /*
             * Check for activities that conflict and will need to be removed.
             * Case 1: the current activity will go past it's end range.
             */
            if (scheduledTime + a.getDuration() > a.getEndRange()) {
                activities[i].allowed = false;
            } // Case 2: the current activity will cause the next activity to not be able to
              // be scheduled, and the next activity has a higher priority.
            else if (i + 1 < activities.length && 
                    scheduledTime + a.getDuration() + activities[i + 1].getDuration() > activities[i + 1].getEndRange()
                    && a.getPriority() < activities[i + 1].getPriority()) {
                activities[i].allowed = false;
            } else {
                
                // set the end time of current activity to scheduledTime + duration.
                double end = scheduledTime + a.getDuration();

                a.actualStart = start;
                a.actualEnd = end;
                scheduledTime = end;
                schedule[index] = a;
                index++;
            }
        }

        // Print the schedule
        printSchedule(activities);
    }

    public static void printSchedule(Activity[] activities) {
        for (int i = 0; i < activities.length; i++) {
            if (activities[i].allowed) {
                int start = convertDecimalToMilitary(activities[i].actualStart);
                String startStr = convertMilitaryToStandard(start);
                int end = convertDecimalToMilitary(activities[i].actualEnd);
                String endStr = convertMilitaryToStandard(end);
                System.out.println(activities[i].getName() + ": " + startStr + " - " + endStr);
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
    public static int convertDecimalToMilitary(double time) {
        int hours = (int) time;
        int minutes = (int) Math.round((time - hours) * 60);
        if (hours <= 12) {
            if (hours == 0) {
                hours = 12;
            }
            return (hours * 100) + minutes;
        } else {
            hours -= 12;
            if (hours == 0) {
                hours = 12;
            }
            return (hours * 100) + minutes + 1200;
        }
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
            /*
             * if the activity starts before the selected start-time within planner, or
             * after the selected end-time within planner, then it is not allowed to be
             * added to the schedule
             */
            if (activities[i].startRange < minimumTime || activities[i].endRange > maximumTime) {
                activities[i].allowed = false;
            }
            /*
             * if the duration of the activity is greater than the time range, then it is
             * not allowed to be added to the schedule
             */
            if (activities[i].duration > (activities[i].endRange - activities[i].startRange)) {
                activities[i].allowed = false;
            }
            /*
             * if start range and end range are the same, then it is not allowed to be added
             */
            if (activities[i].startRange == activities[i].endRange) {
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
    public static String convertMilitaryToStandard(double time) {
        String timeString = "";
        int hours = (int) time / 100;
        int minutes = (int) time % 100;
        String am_pm = hours >= 12 ? "pm" : "am";
        hours = hours % 12;
        hours = hours == 0 ? 12 : hours;
        String formattedMinutes = String.format("%02d", minutes);
        timeString = hours + ":" + formattedMinutes + am_pm;
        return timeString;
    }

    // convert standard time to military time
    public static double convertStandardToMilitary(String time) {
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
