*          DATA SET SPVPAPTSJ  AT LEVEL 001 AS OF 07/26/07                      
*PHASE PADDUSJA                                                                 
         TITLE 'ADD AUTHORIZATION PARMS TO JOB STATEMENT IN JCL DECK'           
ADDUSERJ CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,ADDUSERJ,=A(SAVEAREA)                                          
*                                                                               
* COPY THE SYSUT1 DATASET TO SYSUT2, INSERTING THIS CARD AFTER THE JOB          
*  AND/OR NJB CARD:                                                             
*  "//      USER=JCLCNGMN,PASSWORD=<SEE BELOW>,"                                
*                                                                               
*                                                                               
* FIRST, DO A SIXUNPK ON THE PASSWORD FIELD AND PLACE IT INTO                   
* THE ADDITIONAL CARD WITH THE NEW JOB/NJB PARAMETERS.                          
*                                                                               
         LA    R2,PASSWORD         A(COMPRESSED DATA)                           
         LA    R3,SECPSWD          A(OUTPUT AREA)                               
         LHI   R4,6                L'INPUT                                      
*                                                                               
         SR    R7,R7               CLEAR COUNTER                                
         SR    R6,R6               CLEAR INNER LOOP COUNTER                     
         LR    R5,R3               R5=OUTPUT FIELD                              
*                                                                               
OUTLP    SR    RE,RE               RE=HOLDS 3 BYTES OF INPUT                    
         ICM   RE,7,0(R2)          GET 3 LOW BYTES->4 CHARS                     
*                                                                               
LP       SRDL  RE,6                SHIFT LOWER 6 BITS INTO RF                   
         SRL   RF,26               RT JUSTIFY BITS                              
         O     RF,=X'000000C0'     TURN ON 2 HOBS (TOSSED BY SIXPACK)           
*                                                                               
         STC   RF,3(R5)            STORE EBCDIC CHAR IN OUTPUT                  
         LA    R6,1(R6)            BUMP LOOP COUNTER                            
         BCTR  R5,0                BACK UP 1 LOCATION                           
         CHI   R6,4                HAVE WE OUTPUT 4 CHARS?                      
         BL    LP                  NOT YET                                      
         LA    R7,3(R7)            BUMP OUTER LOOP COUNTER                      
         SR    R6,R6               RESET INNER LOOP COUNTER                     
         LA    R5,8(R5)            BUMP OUTPUT POINTER                          
         CR    R4,R7               HAVE WE PROCESSED ALL CHARS?                 
         BNH   *+12                YES                                          
         LA    R2,3(R2)            BUMP INPUT POINTER                           
         B     OUTLP               LOOP BACK TO TOP                             
         EJECT                                                                  
* NOW READ THE INPUT JCL, LOOKING FOR A JOB OR NJB CARD. WRITE THE              
* OUTPUT JCL AS WE GO ALONG.                                                    
*                                                                               
         OPEN  SYSUT1                                                           
         OPEN  (SYSUT2,OUTPUT)                                                  
*                                                                               
NEXTCARD DS    0H                                                               
         GET   SYSUT1,RECORD       READ AN INPUT JCL RECORD                     
*                                                                               
         CLC   =C'//*',RECORD      COMMENT STATEMENT?                           
         BNE   *+16                                                             
         LA    R2,RECORD                                                        
         BAS   RE,PUTCARD          YES: OUTPUT CARD JUST AS IT IS               
         B     NEXTCARD                                                         
*                                                                               
         CLC   =C'//',RECORD       JCL STATEMENT?                               
         BE    *+16                                                             
         LA    R2,RECORD                                                        
         BAS   RE,PUTCARD          NO: OUTPUT CARD JUST AS IT IS                
         B     NEXTCARD                                                         
*                                                                               
         LA    R3,RECORD+2         POINT PAST "//"                              
         CLI   0(R3),C' '          SCAN TO THE FIRST BLANK                      
         BE    *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         CLI   0(R3),C' '          SCAN TO THE FIRST NON-BLANK                  
         BNE   *+12                                                             
         LA    R3,1(R3)                                                         
         B     *-12                                                             
*                                                                               
         CLC   =C'JOB ',0(R3)      IS THIS A JOB OR NJB CARD?                   
         BE    CHKCARD                                                          
         CLC   =C'NJB ',0(R3)                                                   
         BE    CHKCARD                                                          
*                                                                               
         LA    R2,RECORD           NO: OUTPUT CARD JUST AS IT IS                
         BAS   RE,PUTCARD                                                       
         B     NEXTCARD                                                         
*                                                                               
* HOPEFULLY, AN UNCONTINUED JOB/NJB CARD DOESN'T GO ALL THE WAY UP              
* TO COLUMN 71, WHICH WOULD LEAVE US WITHOUT ANY ROOM TO INSERT A               
* CONTINUATION COMMA. RATHER THAN TRY TO SPLIT THE CARD UP IN THIS              
* PROGRAM, WE SURRENDER BY INSERTING THE SPECIAL CARD ANYWAY, WHICH WE          
* KNOW WILL RESULT IN A JCL ERROR. THIS WILL HOPEFULLY INDICATE TO              
* THE PROGRAMMER THAT HE/SHE NEEDS TO SPLIT UP THE JOB/NJB CARD                 
* BEFORE INVOKING THE ISPF PANEL.                                               
*                                                                               
CHKCARD  DS    0H                                                               
         MVI   SECCOMMA,C','       POSIT: THIS CARD IS CONTINUED                
*                                                                               
         LA    R3,RECORD+70        POINT R3 TO COLUMN 71 (LAST COLUMN)          
         CLI   0(R3),C','          CONTINUED CARD GOES TO LAST COLUMN?          
         BE    OKAY                YES: THAT'S OKAY                             
         CLI   0(R3),C' '          ANY WIGGLE ROOM IN THE CARD?                 
         BE    CHKCONT             YES: SEE IF IT'S CONTINUED                   
*                                                                               
* AN UNCONTINUED JOB/NJB CARD GOES ALL THE WAY TO COLUMN 71. GENERATE           
* COMMENT CARDS DESCRIBING THE PROBLEM, THEN FORCE A JCL ERROR.                 
*                                                                               
         LA    R2,COMMCARD                                                      
         BAS   RE,PUTCARD                                                       
         LA    R2,SPLITMS1                                                      
         BAS   RE,PUTCARD                                                       
         LA    R2,SPLITMS2                                                      
         BAS   RE,PUTCARD                                                       
         LA    R2,COMMCARD                                                      
         BAS   RE,PUTCARD                                                       
         B     OKAY                CONTINUE WITH DELIBERATE JCL ERROR           
*                                                                               
CHKCONT  DS    0H                                                               
         BCTR  R3,0                BACK UP TO LAST SIGNIFICANT CHAR.            
         CLI   0(R3),C' '                                                       
         BE    *-6                                                              
*                                                                               
         CLI   0(R3),C','          IS THE CARD CONTINUED?                       
         BE    OKAY                YES                                          
         MVI   1(R3),C','          NO: FORCE A CONTINUATION HERE...             
         MVI   SECCOMMA,C' '       ...BUT DON'T CONTINUE THE NEXT CARD          
*                                                                               
OKAY     DS    0H                                                               
         LA    R2,RECORD                                                        
         BAS   RE,PUTCARD          WRITE JOB/NJB CARD TO OUTPUT JCL             
         LA    R2,SECINFO                                                       
         BAS   RE,PUTCARD          WRITE SPECIAL CARD TO OUTPUT JCL             
*                                                                               
         B     NEXTCARD                                                         
         SPACE 3                                                                
CLOSE    CLOSE SYSUT1                                                           
         CLOSE SYSUT2                                                           
*                                                                               
         XBASE                                                                  
         SPACE 3                                                                
PUTCARD  NTR1                                                                   
*                                                                               
* R2 = A(CARD TO PUT TO SYSUT2)                                                 
*                                                                               
         PUT   SYSUT2,(R2)                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         SPACE 3                                                                
SYSUT1   DCB   DDNAME=SYSUT1,DSORG=PS,MACRF=GM,EODAD=CLOSE                      
SYSUT2   DCB   DDNAME=SYSUT2,DSORG=PS,MACRF=PM,LRECL=80,RECFM=FB                
*                                                                               
SECINFO  DS    0CL80                                                            
         DC    C'//          USER=JCLCNGMN,PASSWORD='                           
SECPSWD  DC    C'XXXXXXXX'                                                      
SECCOMMA DS    C             COMMA OR BLANK (DEPENDING UPON NEXT CARD)          
         DC    CL(L'SECINFO-(*-SECINFO))' '                                     
*                                                                               
COMMCARD DC    CL80'//*************************************************+        
               ***************'                                                 
SPLITMS1 DC    CL80'//*** THE FOLLOWING CARD IS CODED ALL THE WAY UP TO+        
                COLUMN 71. ***'                                                 
SPLITMS2 DC    CL80'//*** IT MUST BE SPLIT UP INTO AT LEAST TWO CARDS. +        
                           ***'                                                 
*                                                                               
* PASSWORD IN SIXPACK FORMAT (SO IT DOESN'T APPEAR AS CLEAR TEXT IN             
*  THIS LOAD MODULE)                                                            
*                                                                               
PASSWORD DC    X'105262C37CB4'     DEIS0724                                     
*                                                                               
RECORD   DS    CL80                                                             
*                                                                               
         DS    0D                                                               
SAVEAREA DC    5000X'00'                                                        
         SPACE 3                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPVPAPTSJ 07/26/07'                                      
         END                                                                    
