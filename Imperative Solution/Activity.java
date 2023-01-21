package dailyplanner;

public class Activity {

    public String name;
    public double duration;
    public int id;
    public double startRange;
    public double endRange;
    public boolean allowed;
    public double actualStart;
    public double actualEnd;

    public Activity(String name, double duration, int id,double startRange,double endRange, boolean allowed
            ,double actualStart, double actualEnd) {

        this.name = name;
        this.duration = duration;
        this.id = id;
        this.startRange = startRange;
        this.endRange = endRange;
        this.allowed = allowed;
        this.actualStart = actualStart;
        this.actualEnd = actualEnd;

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
    public int getID() {
        return id;
    }
    public void setValue(int id) {
        this.id = id;
    }
}
