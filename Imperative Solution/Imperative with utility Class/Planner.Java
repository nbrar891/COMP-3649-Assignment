import java.io.*;
import java.util.Scanner;
import java.util.Arrays;
import java.util.Comparator;

public class Planner {
    public static void main(String[] args) {
        /*
         * File folder = new File("."); // current directory
         * File[] listOfFiles = folder.listFiles((dir, name) -> name.endsWith(".txt"));
         * String[] filenames = new String[listOfFiles.length];
         * for (int i = 0; i < listOfFiles.length; i++) {
         * filenames[i] = listOfFiles[i].getName();
         * }
         */

        // for (String filename : filenames) {
        try {
            Scanner input = new Scanner(new File("test.txt"));
            input.useDelimiter("-|\n");
            Activity[] activities = new Activity[0]; // set to 0 initially
            int minimumTime = 830; // 830 am
            int maximumTime = 2330; // 1130 pm

            while (input.hasNext()) { // begin parsing input file for data
                String name = input.next();
                int startRange = input.nextInt();
                int endRange = input.nextInt();
                String durationInStr = input.next();
                boolean allowed = true;

                int duration = Utility.getDurationInMinutes(durationInStr); // convert duration to hours

                Activity newActivity = new Activity(name, duration, startRange, endRange, allowed, 0, 0);
                activities = addActivity(activities, newActivity); // update array with new activty
            }

            int index = 0, extraTime = 0;
            // print the current file name
            // System.out.println("Current file being ran: " + filename + "\n");

            // create a new array of activities that are allowed to be scheduled
            Activity[] allowedActivities = createAllowedActivitiesOnlyArray(activities, minimumTime, maximumTime);
            // sortSchedule(allowedActivities);
            // schedule the allowed activities
            if (scheduleActivities(allowedActivities, index, minimumTime, minimumTime, extraTime) && allowedActivities.length > 0) {
                System.out.println("Schedule is possible. Here is the result:\n\n");
                sortFinalSchedule(allowedActivities);
                Utility.printSchedule(allowedActivities);
            } else {
                System.out.println("Schedule is not possible.");
            }

            System.out.println("\n");

            // generateSchedule(allowedActivities, minimumTime, maximumTime);

            // System.out.print("\n\n\n\n");

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
    public static Activity[] createAllowedActivitiesOnlyArray(Activity[] activities, int minimumTime,
            int maximumTime) {
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
         * If the start times are the same, then compare based on start time (lower goes
         * first).
         * If the duration and start times are the same, the compare based on duration
         * (lower goes first).
         */
        return allowedActivities;
    }

    public static void sortFinalSchedule(Activity[] activities) {
        Arrays.sort(activities, new Comparator<Activity>() {
            @Override
            public int compare(Activity a1, Activity a2) {
                if (a1.actualStart == a2.actualStart) {
                    if (a1.duration == a2.duration) {
                        return a1.actualEnd - a2.actualEnd;
                    } else {
                        return a1.duration - a2.duration;
                    }
                } else {
                    return a1.actualStart - a2.actualStart;
                }
            }
        });
    }

    public static boolean scheduleActivities(Activity[] activities, int index, int previousActivityStartTime, int previousActivityEndTime, int extraTime) {
        // Base case:
        if (index >= activities.length || activities[index] == null) {
            return true;
        } 
        // Recursive case:
        int startTime = Utility.addMinutesToTime(activities[index].startRange, extraTime);
        int endTime = Utility.addMinutesToTime(startTime, activities[index].duration);    
        while (endTime <= activities[index].endRange) {
            boolean conflict = false;
            for (int prevActivity = 0; prevActivity < index; prevActivity++) { // does current activity conflict with any of the previous activities
                if (endTime > activities[prevActivity].actualStart && startTime < activities[prevActivity].actualEnd) {
                    conflict = true;
                    break;
                }
            }
            if (!conflict) { // if no conflict, then schedule the activity and recurse to the next activity
                activities[index].actualStart = startTime;
                activities[index].actualEnd = endTime;
                if (scheduleActivities(activities, index + 1, startTime, endTime, 0)) {
                    return true;
                }
            } // if there is a conflict, then try the next time slot
            startTime = Utility.addMinutesToTime(startTime, 1);
            endTime = Utility.addMinutesToTime(startTime, activities[index].duration);
        } 
        return false;
    }
    

    /*
     * check if the activity starts before the selected start-time within planner,
     * or after the selected end-time within planner. Also check if the duration of
     * the activity is greater than the time range. If so, the activity is not
     * allowed to be added to the schedule.
     */
    private static void checkAllowance(Activity[] activities, int minimumTime, int maximumTime) {
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
}
