# UNC Baseball Blast App
Bradley Buchner, Senior Analyst for UNC Baseball

## Description
The UNC Baseball Blast App is an R Shiny web application created for UNC hitters and coaches to view and interact with Blast and batting practice data. Users can view Blast and Trackman data on individual swings or as a practice session summary. Individual sessions can be compared with season-long averages, and the best/worst swings of a session are displayed as well (with links to corresponding video coming soon). The app also serves as a shared hitting journal between the hitters and hitting coach by allowing them to leave comments on individual swings, full practice sessions, log a focus, or log a swing adjustment. Our coaching staff use the app to evaluate hitters, communicate with them, and organize their evaluations. Our hitters use the app to communicate with the coaching staff, self-evaluate, and record their swing transformations. Within the UNC Baseball Analytics team is a Blast research group tasked with supplying additional analysis to the Blast app.

## Database
The app's data consists of Blast data, Trackman batting practice data, and information recorded by the user. All data is stored in and loaded from a MySQL database hosted in AWS. After each batting practice session, the Blast swings and Trackman data are joined by their timestamps and written to the database. This querying process is located in the file "Write to Blast DB.R". As the user records information within the app, tables in the database are updated to store this information. These queries are located within functions in the file "app functions.R". 

## User Guide
Below I have listed each tab in the app and details on its corresponding user interface. Here is the link to the condensed version of the app: https://tinyurl.com/UNCBaseball-BlastAppDemo

### Home
- Table that compares Blast and Trackman averages from the selected hitter's most recent session with their season averages.
- Table of the selected hitter's recent notes
- Table of the selected hitter's recent focuses (A focus can be thought of as what the hitter is working on. Hitter's will log any swing adjustments as a focus)

### Analysis
- Table identical to home tab table however the user is able to select the session. 
- Tables of best and worst swings based on the metric chosen by the user. Video Link is not yet functional. 

### Notebook
Here is where the user can leave a note on individual swings, entire practice sessions, and edit/delete these notes. To leave a swing note, the user clicks the button, then clicks the row of the swing to leave a note on, then the user types their note and clicks submit. You must refresh your browser to see any edits. To leave a session note is much less complicated. Session notes can be in reference to the data or can be totally unrelated, such as a "hitter X looked good in BP today". Session notes are used to summarize practice sessions. 

### Swing Mechanics
Here is where the user can log a focus on a practice session. What was that hitter working on with their swing in that session? Or... What adjustments did that hitter make in that session?

### Logs
- View all notes and focuses for the selected hitter. 


Since the version of the app linked here has been created for users unaffiliated with UNC Baseball, feel free to test the interactivity of the app by leaving notes and logging focuses. Remember to refresh your browser to see edits that you make.  
