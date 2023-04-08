public class Utility {

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

    // convert the duration from hours into mins
    // ex: 2 hrs should be turned into 120 mins (in int)
    public static int getDurationInMinutes(String duration) {
        String[] arr = duration.split(" ");
        int durationInMinutes;
        String hours = "hrs";

        if (arr[1].trim().equals(hours)) {
            durationInMinutes = Integer.parseInt(arr[0]) * 60;
        } else {
            durationInMinutes = Integer.parseInt(arr[0]);
        }

        return durationInMinutes;
    }

    public static void printSchedule(Activity[] activities) {
        for (int i = 0; i < activities.length; i++) {
            if (activities[i].allowed) {
                String startStr = Utility.convertMilitaryToStandard(activities[i].actualStart);
                String endStr = Utility.convertMilitaryToStandard(activities[i].actualEnd);
                System.out.println(activities[i].getName() + ": " + startStr + " - " + endStr);
            }
        }
    }

    public static int addMinutesToTime(int currentTime, int minutesToAdd) {
        // Extract the hour and minute components from the current time
        int currentHour = currentTime / 100;
        int currentMinute = currentTime % 100;

        // Add the minutes to the current minute value
        currentMinute += minutesToAdd;

        // If the minute value exceeds 60, adjust the hour and minute accordingly
        if (currentMinute >= 60) {
            currentHour += currentMinute / 60;
            currentMinute %= 60;
        }

        // If the hour value exceeds 23 (i.e. midnight), wrap around to 0
        currentHour %= 24;

        // Return the result as an integer in the format of HHmm
        return currentHour * 100 + currentMinute;
    }


    // might need these in the future?

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

    // convert the values of the time ranges from military time to decimal
    public static double convertMilitaryToDecimal(int time) {
        int hours = time / 100;
        int mins = time % 100;
        return hours + (double) mins / 60;
    }

}