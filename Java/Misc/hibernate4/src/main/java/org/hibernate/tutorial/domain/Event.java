package org.hibernate.tutorial.domain;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

public class Event {
    private Long id;

    private String title;
    private Date date;

    public Event() {}
    private Set participants = new HashSet();

    public Set getParticipants() {
        return participants;
    }

    public void setParticipants(Set participants) {
        this.participants = participants;
    }
    public void addToParticipant(Person participant) {
        this.getParticipants().add(participant);
        participant.getEvents().add(this);
    }

    public void removeFromParticipant(Person participant) {
        this.getParticipants().remove(participant);
        participant.getEvents().remove(this);
    }

    public Long getId() {
        return id;
    }

    private void setId(Long id) {
        this.id = id;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}