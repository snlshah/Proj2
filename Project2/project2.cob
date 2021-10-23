       IDENTIFICATION DIVISION.
       PROGRAM-ID.  project2.
       AUTHOR. Sonali Shah.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT INPUT-FILE   ASSIGN TO 'NEWEMP'.

       SELECT PRNT-FILE    ASSIGN TO 'UR22S-PRNT'.


       DATA DIVISION.

       FILE SECTION.
       FD INPUT-FILE
       BLOCK CONTAINS 0 RECORDS
       LABEL RECORDS ARE STANDARD.


       01 INPUT-REC   PIC X(107).

       FD  PRNT-FILE
       LABEL RECORDS ARE OMITTED.
       01  PRNT-REC                   PIC X(135).
           
       WORKING-STORAGE SECTION.
       01 S-P PIC 99 VALUE 00.
       01 H-P PIC 99 VALUE 00.    
       01 S-DIVI PIC 99999 VALUE 00.
       01 S-PLACE PIC 99999 VALUE 00.
       
       01 H-DIVI PIC 99999 VALUE 00.    
       01 H-PLACE PIC 99999 VALUE 00.    
       01  H-AVE PIC 99999 VALUE 00.
       01  S-AVE PIC 99999 VALUE 00.    
       01  REPT-NUM   PIC 99 VALUE 00.
       01  H-NUM PIC 99 VALUE 00.
       01  S-NUM PIC 99 VALUE 00.
       01  T-ONE PIC 99 VALUE 00.
       01  T-TWO PIC 99 VALUE 00.
       01  T-THREE PIC 99 VALUE 00.
       01  T-FOUR PIC 99 VALUE 00.
       01  T-FIVE PIC 99 VALUE 00.
       01  T-SIX PIC 99 VALUE 00.
       01  T-SEVEN PIC 99 VALUE 00.
       01  T-EIGHT PIC 99 VALUE 00.
       01  T-NINE PIC 99 VALUE 00.
       01  T-TEN PIC 99 VALUE 00.
           
       01  LINE-KOUNT PIC 99 VALUE 00.
       01  PAGE-KOUNT PIC 9999 VALUE 0001.
       01  LINE-INPUT PIC 9999 VALUE 0000.
       01  INPUT-DATA.
                03  I-EMPID                  PIC X(7).
                03  I-LAST                   PIC X(15).
                03  I-FIRST                  PIC X(15).
                03  I-TYPE                   PIC 9(2).
                03  I-TITLE                  PIC X(17).
                03  I-SSN.
                        10 SSN_NUM.
                                15 a PIC 9(3).
                                15 b PIC 9(2).
                                15 c PIC 9(4).
                03  FILLER   PIC X(24)     VALUE SPACES.
                03  I-DATE.
                        10 START_DATE.
                                15 MONTH  PIC 9(2).
                                15 DY PIC 9(2).
                                15 YEAR    PIC 9(4).
                03  FILLER  PIC X(2)       VALUE SPACES.
                03  I-RATE  PIC 9(4)V99.
                03  I-ST    PIC X(1).
           



       01  PRNT-DATA1.
                
                03  FILLER PIC X(2) VALUE SPACES.
                03  a1 PIC 9(3).
                03 FILLER PIC X VALUE '-'.
                03  b1 PIC 9(2).
                03 FILLER PIC X VALUE '-'.
                03  c1 PIC 9(4).

                03  FILLER PIC X(3) VALUE SPACES.

                03  L-EMPID1              PIC X(7).
                03  FILLER PIC X(3) VALUE SPACES.

                03  L-LAST                 PIC X(15).
                03  FILLER PIC X(2) VALUE SPACES.

                03  L-FIRST PIC X(15).
                03  FILLER PIC X(10) VALUE SPACES.

                03  L-TITLE PIC X(17).
                03  FILLER PIC X(5) VALUE SPACES.

                03  L-TYPE PIC 9(2).
                03  FILLER PIC X(5) VALUE SPACES.

                03  MONTH1 PIC 9(2).
                03 FILLER PIC X VALUE '/'.
                03 DY1 PIC 9(2).
                03 FILLER PIC X VALUE '/'.
                03 YEAR1 PIC 9(4).
                03 FILLER PIC X(2) VALUE SPACES.
                03 L-RATE PIC 9999.99.
                03 FILLER PIC X(3) VALUE SPACES.
                03 L-ST PIC X(1). 
      * 01 PRNT-SAVE.
      *         03 FILLER PIC X(2) VALUE SPACES.
      *         03 FILLER PIC X(21) VALUE 'AVERAGE SALARY RATE: '.
      *         03 FILLER PIC X(2) VALUE SPACES.
      *         03 SALARY PICTURE $$9999.99.
      
      * 01 PRNT-HAVE.
      *          03 FILLER PIC X(2) VALUE SPACES.
      *         03 FILLER PIC X(21) VALUE 'AVERAGE HOURLY RATE: '.
      *         03 FILLER PIC X(2) VALUE SPACES.
      *         03 HOURLY PICTURE $$9999.99.
       01 PRNT-CAL.
          03 FILLER PIC X(2) VALUE SPACES.
          03 FILLER PIC X(33) VALUE 'NUMBER OF EMPLOYEE RECORDS READ: '.
          03 FILLER PIC X(2) VALUE SPACES.
          03 NUMRECORDS PICTURE ZZ99.

       01 PRNT-HOURLY.
          03 FILLER PIC X(2) VALUE SPACES.
          03 FILLER PIC X(28) VALUE 'NUMBER OF HOURLY EMPLOYEES: '.
          03 FILLER PIC X(7) VALUE SPACES.
          03 NUMHOUREMP PIC ZZZ9.
          03 FILLER PIC X(30) VALUE SPACES.
          03 FILLER PIC X(21) VALUE 'AVERAGE HOURLY RATE: '.
          03 HOURLY PICTURE $$9999.99.
        
          

          
          

       01 PRNT-SALARY.
          03 FILLER PIC X(2) VALUE SPACES.
          03 FILLER PIC X(30) VALUE 'NUMBER OF SALARIED EMPLOYEES: '.
          03 FILLER PIC X(5) VALUE SPACES.
           
          03 NUMSALEMP PIC ZZZ9.
          03 FILLER PIC X(30) VALUE SPACES.
          03 FILLER PIC X(21) VALUE 'AVERAGE SALARY RATE: '.
          03 SALARY PICTURE $$9999.99.
          

       01 PRNT-ONE.
          03 FILLER PIC X(2) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 1:'.
        
          03 TYPEONE PIC ZZ9.

       
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 2:'.
       
          03 TYPETWO PIC ZZ9.
        
      * 01 PRNT-THREE.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 3:'.
      
          03 TYPETHREE PIC ZZ9.

      * 01 PRNT-FOUR.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 4:'.
     
          03 TYPEFOUR PIC ZZ9.

      * 01 PRNT-FIVE.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(8) VALUE 'TYPE 5 :'.
    
          03 TYPEFIVE PIC ZZ9.

       01 PRNT-SIX.
         03 FILLER PIC X(2) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 6:'.
   
          03 TYPESIX PIC ZZ9.   

      * 01 PRNT-SEVEN.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 7:'.
  
          03 TYPESEVEN PIC ZZ9.

      * 01 PRNT-EIGHT.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 8:'.
 
          03 TYPEEIGHT PIC ZZ9.
      * 01 PRNT-NINE.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(7) VALUE 'TYPE 9:'.

          03 TYPENINE PIC ZZ9.
      * 01 PRNT-TEN.
          03 FILLER PIC X(13) VALUE SPACES.
          03 FILLER PIC X(8) VALUE 'TYPE 10:'.    

          03 TYPETEN PIC ZZ9.
           

       01  PRNT-HEADING1.
        03 FILLER PIC X(2) VALUE SPACES.
        03 DATE-MON PICTURE Z9.
        03 FILLER PIC X VALUE '/'.
        03 DATE-DAY PICTURE Z9.
        03 FILLER PIC X VALUE '/'.
        03 DATE-YR PIC Z9.

        03 FILLER PIC X(36) VALUE SPACES.       
       
        03 FILLER PIC X(28) VALUE 'THE BEST IS YET TO COME, INC'.
        03 FILLER PIC X(34) VALUE SPACES.
        03 FILLER PIC X(10) VALUE ' PAGE:'.
       
       03 ERROR-PAGE PIC ZZZZ.
      *  03 PAGE-KOUNT PIC 99 VALUE 00.
       
       01 PRNT-HEADING2.
       03 FILLER PIC X(45) VALUE SPACES.
       03 FILLER PIC X(31) VALUE 'EMPLOYEE CLASSIFICATION AND PAY'.
       01 PRNT-HEADING3.
       03 FILLER PIC X(2) VALUE SPACES.
       03 FILLER PIC X(3) VALUE 'SSN'.

       03 FILLER PIC X(11) VALUE SPACES.
       03 FILLER PIC X(6) VALUE 'EMP ID'.

       03 FILLER PIC X(4) VALUE SPACES.
       03 FILLER PIC X(4) VALUE 'LAST'.

       03 FILLER PIC X(13) VALUE SPACES.
       03 FILLER PIC X(5) VALUE 'FIRST'.

       03 FILLER PIC X(20) VALUE SPACES.
       03 FILLER PIC X(5) VALUE 'TITLE'.

       03 FILLER PIC X(17) VALUE SPACES.
       03 FILLER PIC X(4) VALUE 'TYPE'.

       03 FILLER PIC X(3) VALUE SPACES.
       03 FILLER PIC X(4) VALUE 'DATE'.
       03 FILLER PIC X(8) VALUE SPACES.
       03 FILLER PIC X(4) VALUE 'RATE'.
       03 FILLER PIC X(6) VALUE SPACES.
       03 FILLER PIC X(2) VALUE 'ST'.    
         
       01 MISC.
       03 CURRENT-DATE.
                        05 CUR-YR PIC X(2).
                        05 CUR-MON PIC X(2).
                        05 CUR-DAY PIC X(2).
       03 FORMAT-DATE.
                        05 FILLER PIC X(2) VALUE SPACES.
                        05 FOR-MON PIC X(2).
                        05 FILLER PIC X(1) VALUE '/'.
                        05 FOR-DAY PIC X(2).
                        05 FILLER PIC X(1) VALUE '/'.
                        05 FOR-YR PIC X(2).    
       03 PAGE-NUMBER.
                        
                        05 FILLER PIC X(110) VALUE SPACES.
                        05 FILLER PIC X(11) VALUE 'PAGE'. 

       03  EOF-I                  PIC 9         VALUE 0.

       PROCEDURE DIVISION.
       000-MAINLINE.
       ACCEPT CURRENT-DATE FROM DATE.
       OPEN INPUT INPUT-FILE
       OUTPUT PRNT-FILE.
           PERFORM 2000-READ-INPUT.
             
           PERFORM 1400-PRINT-HEAD.
          
           PERFORM 1500-LOOP
          
           
           UNTIL EOF-I = 1.
           IF EOF-I = 1
      *              ADD 1 TO PAGE-KOUNT
                   MOVE PAGE-KOUNT TO ERROR-PAGE
                   PERFORM 1700-RECNUM
                   PERFORM 700-HEMP
                   PERFORM 770-SEMP
                   PERFORM 775-ONE
      *             PERFORM 780-TWO
      *             PERFORM 781-THREE
      *             PERFORM 782-FOUR
      *             PERFORM 783-FIVE
                   PERFORM 784-SIX
      *             PERFORM 785-SEVEN
      *             PERFORM 786-EIGHT
      *             PERFORM 787-NINE
      *             PERFORM 788-TEN
      *            PERFORM 789-HOURLY
      *             PERFORM 790-SALARY
           END-IF.

           CLOSE INPUT-FILE
            PRNT-FILE.
          
               
           STOP RUN.
       
       1100-PHEAD.
           
           MOVE PAGE-KOUNT TO ERROR-PAGE.
           MOVE CUR-DAY TO DATE-DAY.
           MOVE CUR-MON TO DATE-MON.
           MOVE CUR-YR TO DATE-YR.
       
     
           WRITE PRNT-REC FROM PRNT-HEADING1
           AFTER ADVANCING 2 LINES.
           WRITE PRNT-REC FROM PRNT-HEADING2
           AFTER ADVANCING 1 LINE.

           WRITE PRNT-REC FROM PRNT-HEADING3
           AFTER ADVANCING 2 LINES.
           ADD 1 TO REPT-NUM.
           ADD 1 TO PAGE-KOUNT.

          
       1400-PRINT-HEAD.
                 PERFORM 1100-PHEAD.
           
      
       1500-LOOP.
           
           PERFORM 1600-PRINT-NAMES.
           PERFORM 2000-READ-INPUT.
        
       1700-RECNUM.
           
           WRITE PRNT-REC FROM PRNT-HEADING1
           AFTER ADVANCING 4 LINE.
           WRITE PRNT-REC FROM PRNT-HEADING2
           AFTER ADVANCING 1 LINE.
           WRITE PRNT-REC FROM PRNT-CAL
           AFTER ADVANCING 4 LINES.
          
       700-HEMP.
           WRITE PRNT-REC FROM PRNT-HOURLY
           AFTER ADVANCING 2 LINES.

       770-SEMP.
           WRITE PRNT-REC FROM PRNT-SALARY
           AFTER ADVANCING 2 LINES.

        775-ONE.
           WRITE PRNT-REC FROM PRNT-ONE
           AFTER ADVANCING 2 LINES.

      * 780-TWO.
      *     WRITE PRNT-REC FROM PRNT-TWO
      *    AFTER ADVANCING 2 LINES.

      * 781-THREE.
      *     WRITE PRNT-REC FROM PRNT-THREE
      *     AFTER ADVANCING 2 LINES.
      * 782-FOUR.
      *    WRITE PRNT-REC FROM PRNT-FOUR
      *     AFTER ADVANCING 2 LINES.
      *  783-FIVE.
      *     WRITE PRNT-REC FROM PRNT-FIVE
      *     AFTER ADVANCING 2 LINES.
       784-SIX.
          WRITE PRNT-REC FROM PRNT-SIX
           AFTER ADVANCING 2 LINES.
      * 785-SEVEN.
      *    WRITE PRNT-REC FROM PRNT-SEVEN
      *     AFTER ADVANCING 2 LINES.
      * 786-EIGHT.
      *    WRITE PRNT-REC FROM PRNT-EIGHT
      *    AFTER ADVANCING 2 LINES.
      * 787-NINE.
      *    WRITE PRNT-REC FROM PRNT-NINE
      *     AFTER ADVANCING 2 LINES.
      * 788-TEN.
      *     WRITE PRNT-REC FROM PRNT-TEN
      *    AFTER ADVANCING 2 LINES.

      * 789-HOURLY.
      *    WRITE PRNT-REC FROM PRNT-HAVE
      *     AFTER ADVANCING 2 LINES.
           

      * 790-SALARY.
           
      *     WRITE PRNT-REC FROM PRNT-SAVE
      *     AFTER ADVANCING 2 LINES.

       1600-PRINT-NAMES.
            
           MOVE I-EMPID           TO L-EMPID1.
           MOVE I-LAST            TO L-LAST.
           MOVE I-FIRST           TO L-FIRST.
           MOVE I-TYPE            TO L-TYPE.
           MOVE I-TITLE           TO L-TITLE.
           MOVE a                 TO a1.
           MOVE b                 TO b1.
           MOVE c                 TO c1.
           MOVE MONTH            TO MONTH1.
           MOVE DY               TO DY1.
           MOVE YEAR             TO YEAR1.
           MOVE I-RATE           TO L-RATE.
           MOVE I-ST             TO L-ST.
           MOVE CUR-DAY TO DATE-DAY.
           MOVE CUR-MON TO DATE-MON.
           MOVE CUR-YR TO DATE-YR.
           
           IF L-RATE > 0
                   IF L-ST = 'S'
                           MOVE L-RATE TO S-PLACE
                           COMPUTE S-AVE = S-AVE + S-PLACE
                           COMPUTE S-DIVI = S-AVE / S-NUM
                           MOVE S-DIVI TO SALARY
                   END-IF
           IF L-RATE > 0
                   IF L-ST = 'H'
                   MOVE L-RATE TO H-PLACE
                   COMPUTE H-AVE = H-AVE + H-PLACE
                   COMPUTE H-DIVI =  H-AVE/ H-NUM
                   MOVE H-DIVI TO HOURLY
        
                   END-IF.        


                           
           IF LINE-KOUNT >= 7 
                   PERFORM 1800-HEADING-RTN
           END-IF.
           IF REPT-NUM =5 
                   DISPLAY REPT-NUM
                   PERFORM 1100-PHEAD
                   ADD 1 TO REPT-NUM
                   EXIT
           END-IF.
           IF L-ST = 'H' 
                   ADD 1 TO H-NUM
                   MOVE H-NUM TO NUMHOUREMP
                   MOVE NUMHOUREMP TO H-P

           ELSE
                   ADD 1 TO S-NUM
                   
                   MOVE S-NUM TO NUMSALEMP
                   MOVE NUMSALEMP TO S-P

           END-IF.        
           EVALUATE TRUE
                   WHEN L-TYPE = 01
                   ADD 1 TO T-ONE
                   MOVE T-ONE TO TYPEONE
                   WHEN L-TYPE = 02
                   ADD 1 TO T-TWO
                   MOVE T-TWO TO TYPETWO
                   WHEN L-TYPE = 03
                   ADD 1 TO T-THREE
                   MOVE T-THREE TO TYPETHREE
                   WHEN L-TYPE = 04
                   ADD 1 TO T-FOUR
                   MOVE T-FOUR TO TYPEFOUR

                   WHEN L-TYPE = 05 
                   ADD 1 TO T-FIVE
                   MOVE T-FIVE TO TYPEFIVE
                   WHEN L-TYPE = 06
                   ADD 1 TO T-SIX
                   MOVE T-SIX TO TYPESIX
                   WHEN L-TYPE = 07
                           ADD 1 TO T-SEVEN
                           MOVE T-SEVEN TO TYPESEVEN
                   WHEN L-TYPE = 08 
                           ADD 1 TO T-EIGHT
                           MOVE T-EIGHT TO TYPEEIGHT
                   WHEN L-TYPE =09
                           ADD 1 TO T-NINE
                           MOVE T-NINE TO TYPENINE



                   WHEN OTHER
                     ADD 1 TO T-TEN
                    MOVE T-TEN TO  TYPETEN

           END-EVALUATE.        

          
       WRITE PRNT-REC FROM PRNT-DATA1
       AFTER ADVANCING 1 LINES.
           ADD 1 TO LINE-KOUNT.
           ADD 1 TO LINE-INPUT.
           MOVE LINE-INPUT TO NUMRECORDS.
      *WRITE PRNT-REC FROM PRNT-CAL.
       
       1800-HEADING-RTN.          
           PERFORM 1400-PRINT-HEAD.
                  
           MOVE ZEROS TO LINE-KOUNT.

       2000-READ-INPUT.
           READ INPUT-FILE INTO INPUT-DATA     
           AT END MOVE 1 TO EOF-I.
      *   PERFORM 1700-RECNUM. 


