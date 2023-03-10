package dailyplanner;
public class Activity {

    public String name; // name of the activity
    public double duration; // duration of the activity
    public double startRange; // range of time the activity can start
    public double endRange; // range of time the activity can end
    public boolean allowed; // if the activity is allowed to be scheduled
    public double actualStart; // the actual start time of the activity
    public double actualEnd; // the actual end time of the activity
    public int priority; // the priority of the activity. 1 = low, 2 = medium, 3 = high

    public Activity(String name, double duration, double startRange, double endRange, boolean allowed,
            double actualStart, double actualEnd, int priority) {
        this.name = name;
        this.duration = duration;
        this.startRange = startRange;
        this.endRange = endRange;
        this.allowed = allowed;
        this.actualStart = actualStart;
        this.actualEnd = actualEnd;
        this.priority = priority;
    }

    public double getStartRange() {
        return startRange;
    }

    public void setStartRange(double startRange) {
        this.startRange = startRange;
    }

    public double getEndRange() {
        return endRange;
    }

    public void setEndRange(double endRange) {
        this.endRange = endRange;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public double getDuration() {
        return duration;
    }

    public void setDuration(double duration) {
        this.duration = duration;
    }

    public boolean isAllowed() {
        return allowed;
    }

    public void setAllowed(boolean allowed) {
        this.allowed = allowed;
    }

    public int getPriority() {
        return priority;
    }

    public void setPriority(int priority) {
        this.priority = priority;
    }
}
