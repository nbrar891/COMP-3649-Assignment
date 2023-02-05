
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

    
    public static void printSchedule(Activity[] activities) {
        for (int i = 0; i < activities.length; i++) {
            if (activities[i].allowed) {
                int start = Utility.convertDecimalToMilitary(activities[i].actualStart);
                String startStr = Utility.convertMilitaryToStandard(start);
                int end = Utility.convertDecimalToMilitary(activities[i].actualEnd);
                String endStr = Utility.convertMilitaryToStandard(end);
                System.out.println(activities[i].getName() + ": " + startStr + " - " + endStr);
            }
        }
    }
    
}
