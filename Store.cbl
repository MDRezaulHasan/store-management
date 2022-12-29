       IDENTIFICATION DIVISION.
       PROGRAM-ID. STORE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PASSWARDFILE ASSIGN TO "passward.txt"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS RECORDID.

       DATA DIVISION.
       FILE SECTION.
       FD PASSWARDFILE.
       01 RECORDDATA.
          02 RECORDID         PIC 99.
          02 SYSTEMNAME       PIC X(15).
          02 PASSWARDLIST     PIC X(15).

       WORKING-STORAGE SECTION.
       01 CHOICE              PIC 9.
       01 STAYOPEN            PIC X     VALUE 'Y'.
       01 PASSWARDEXISTS      PIC X.
       01 WS-PASSWARDS.
          02 WS-RECORDID      PIC 99.
          02 WS-SYSTEMNAME    PIC X(15).
          02 WS-PASSWARDLIST  PIC X(15).        

       PROCEDURE DIVISION.
       STARTPARA.
           OPEN I-O PASSWARDFILE.
           PERFORM UNTIL STAYOPEN = 'N'
                   DISPLAY " "
                   DISPLAY "PASSWARD RECORDS"
                   DISPLAY "1 : Add Passward"
                   DISPLAY "2 : Delete Passward"
                   DISPLAY "3 : Update Passward"
                   DISPLAY "4 : Get Passward Record"
                   DISPLAY "5 : Get All Records"
                   DISPLAY "0 : Quit"
                   DISPLAY ": " WITH NO ADVANCING
                   ACCEPT CHOICE
                   EVALUATE CHOICE
                   WHEN 1
                        PERFORM ADDPASS
                   WHEN 2
                        PERFORM DELETEPASS
                   WHEN 3
                        PERFORM UPDATEPASS
                   WHEN 4
                        PERFORM GETPASS
                   WHEN OTHER
                        MOVE 'N' TO STAYOPEN
                   END-EVALUATE
          
           END-PERFORM.
           CLOSE PASSWARDFILE
           STOP RUN.

       ADDPASS.
           DISPLAY " ".
           DISPLAY "Enter Record ID : " WITH NO ADVANCING.
           ACCEPT RECORDID.
           DISPLAY "Enter SYSTEM NAME : " WITH NO ADVANCING.
           ACCEPT SYSTEMNAME.
           DISPLAY "Enter PASSWARD : " WITH NO ADVANCING.
           ACCEPT PASSWARDLIST.
           DISPLAY " ".
           WRITE RECORDDATA
           INVALID KEY
                   DISPLAY "ID is Taken"
           END-WRITE.


       DELETEPASS.
           DISPLAY " ".
           DISPLAY "Enter Record ID to Delete : " WITH NO ADVANCING.
           ACCEPT RECORDID.
           DELETE PASSWARDFILE
           INVALID KEY
                   DISPLAY "Key Doesn't Exist"
           END-DELETE.

       UPDATEPASS.
           MOVE 'Y' TO PASSWARDEXISTS.
           DISPLAY " ".
           DISPLAY "Enter ID to Update : " WITH NO ADVANCING.
           ACCEPT RECORDID.
           READ PASSWARDFILE
           INVALID KEY
                   MOVE 'N' TO PASSWARDEXISTS
           END-READ.
           IF PASSWARDEXISTS = 'N'
              DISPLAY "Record Doesn't Exist"
           ELSE
              DISPLAY "Enter the New SYSTEM NAME : " WITH NO ADVANCING
              ACCEPT SYSTEMNAME
              DISPLAY "Enter the New PASSWARD : " WITH NO ADVANCING
              ACCEPT PASSWARDLIST
           END-IF.
           REWRITE RECORDDATA
           INVALID KEY
                   DISPLAY "Record Not Updated"
           END-REWRITE.


       GETPASS.
           MOVE 'Y' TO PASSWARDEXISTS.
           DISPLAY " ".
           DISPLAY "Enter record ID to Find : " WITH NO ADVANCING.
           ACCEPT RECORDID.
           READ PASSWARDFILE
           INVALID KEY
                   MOVE 'N' TO PASSWARDEXISTS
           END-READ.
           IF PASSWARDEXISTS = 'N'
              DISPLAY "Record Doesn't Exist"
           ELSE
              DISPLAY "ID : " RECORDID
              DISPLAY "SYSTEM NAME : " SYSTEMNAME
              DISPLAY "PASSWARD : " PASSWARDLIST
           END-IF.
