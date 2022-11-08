# HCal

## Overview

We plan to implement a calendar viewer that can view calendars in iCalendar (`.ics`) format. The viewer will present the calendar using a terminal user interface similar to common calendar applications such as Google Calendar.

## Features

### Input & Output

Our viewer will support reading a calendar from a file or from `stdin`. The command line interface will look like:

``` bash
# view a calendar file
calender some_calendar.ics

# view a calendar from stdin
curl https://some_calendar.ics | calendar
```

The user can save the modified calendar by specifying a file name to save the new calendar as.

### Event View
Similar to Google Calendar, our app will allow users to switch between views with different granularity (e.g., day, 4 days, week, and month) and display relevant events in a grid.

Besides, it could show the current time, using a horizontal line. Based on that line, the viewer could inform the user which events are passed and which happens soon.

In the case that there are too many events (larger than our pre-defined window), then the viewer will only show the event title instead of full information.

### Navigation

The viewer allows the user to navigate to a specific date by using page-down and page-up keys or by entering the date he or she wants to navigate to. Additionally, the viewer provides a key combination to jump to "today".

The viewer will also support search an event by name.

### Editing

Our viewer also supports features related to editing, users could change their events on specific time mannually by selecting that event and changing it. Also, users could deleting or adding their events by selecting the corresponding time slots and typing: time of starts, title of events.
