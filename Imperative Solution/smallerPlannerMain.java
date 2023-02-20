import java.util.Scanner;

public class Planner {
    public static void main(String[] args) {

        Scanner scan = null;
        try {
             scan = new Scanner(new File("normalNoProblems.txt"));
             double minimumTime = 8.50;
             double maximumTime = 23.50;
             Activity[] activities = parseInput(scan);
             Activity[] allowedActivities = createAllowedActivitiesOnlyArray(activities, minimumTime, maximumTime);
             System.out.print("\n");
             generateSchedule(allowedActivities, minimumTime, maximumTime);

        }catch (IOException e) {
            System.out.println("You entered bad data." );
            System.out.println("Run the program again." );

        } finally {
            scan.close();
        }

    }

    
    /*parseInput reads from text and creates the initial array of activities
     *
     *
     *
     */
    public static Activity[] parseInput(Scanner input) {
        Activity[] activities = new Activity[0]; // set to 0 initially
        input.useDelimiter("-|\n");

        while (input.hasNext()) { // begin parsing input file for data
            String name = input.next();
            double startRange = input.nextDouble();
            double endRange = input.nextDouble();
            String durationInStr = input.next();
            boolean allowed = true;
            String[] tokens = input.nextLine().trim().split("-");
            int priority = Integer.parseInt(tokens[tokens.length-1]);

            startRange = Utility.convertMilitaryToDecimal((int) startRange); // convert start time to decimal
            endRange = Utility.convertMilitaryToDecimal((int) endRange); // convert end time to decimal
            double duration = Utility.getDurationInHours(durationInStr); // convert duration to hours

            Activity newActivity = new Activity(name, duration, startRange, endRange, allowed, 0, 0, priority);
            activities = addActivity(activities, newActivity); // update array with new activty
        }
        return activities;
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

    public static void generateSchedule(Activity[] activities, double minimumTime, double maximumTime) {
        // Initialize the schedule
        Activity[] schedule = new Activity[activities.length];
        double scheduledTime = minimumTime;
        int index = 0;

        // Iterate through the activities
        for (int i = 0; i < activities.length; i++) {
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
        Utility.printSchedule(activities);
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
}
