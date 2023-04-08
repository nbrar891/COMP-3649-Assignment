public class Activity {

    public String name; // name of the activity
    public int duration; // duration of the activity
    public int startRange; // range of time the activity can start
    public int endRange; // range of time the activity can end
    public boolean allowed; // if the activity is allowed to be scheduled
    public int actualStart; // the actual start time of the activity
    public int actualEnd; // the actual end time of the activity

    public Activity(String name, int duration, int startRange, int endRange, boolean allowed,
            int actualStart, int actualEnd) {
        this.name = name;
        this.duration = duration;
        this.startRange = startRange;
        this.endRange = endRange;
        this.allowed = allowed;
        this.actualStart = actualStart;
        this.actualEnd = actualEnd;
    }

    public int getStartRange() {
        return startRange;
    }

    public void setStartRange(int startRange) {
        this.startRange = startRange;
    }

    public int getEndRange() {
        return endRange;
    }

    public void setEndRange(int endRange) {
        this.endRange = endRange;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public boolean isAllowed() {
        return allowed;
    }

    public void setAllowed(boolean allowed) {
        this.allowed = allowed;
    }

    public int getActualStart() {
        return actualStart;
    }

    public void setActualStart(int actualStart) {
        this.actualStart = actualStart;
    }

    public int getActualEnd() {
        return actualEnd;
    }

    public void setActualEnd(int actualEnd) {
        this.actualEnd = actualEnd;
    }
}
