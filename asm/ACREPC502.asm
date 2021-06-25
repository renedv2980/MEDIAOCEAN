*          DATA SET ACREPC502  AT LEVEL 010 AS OF 05/23/16                      
*PHASE ACC502A,+0                                                               
*INCLUDE SQUASHER                                                               
         TITLE 'AYER DAILY TIME CARD'                                           
ACC502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACC5**                                                       
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
*                                                                               
         USING ACC5D,RC                                                         
         LA    RC,SPACEND                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACC502+4096,R9                                                   
         EJECT                                                                  
*-----------------------------------------------------------------*             
*              ROUTINE FOR RUN FIRST                                            
*-----------------------------------------------------------------*             
C502     CLI   MODE,RUNFRST                                                     
         BNE   C506                                                             
*                                                                               
         LA    RE,RELOTAB          RELOCATE MY A TYPES                          
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
*                                                                               
         MVC   C5PWK1(C5PLNQ),SPACES    INITIALIZE PRINT LINE 1                 
         LA    R1,DETMAX                                                        
         STC   R1,MAXLINES              SET MAX LINES PER PAGE                  
         STC   R1,PGEMAX                                                        
         MVC   PAGE,=H'1'               START AT PAGE 1                         
*&&DO                                                                           
         LA    R0,2                                                             
C504     GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1                   PRINT LINE UP                           
         MVI   P,C'X'                                                           
         MVC   P+1(34),P                BEGINNING TO END OF DEPT LINE           
         MVC   P+36(13),P               BEGINNING TO END OF DATE FIELD          
         GOTO1 ACREPORT                                                         
         BCT   R0,C504                                                          
*&&                                                                             
         SR    R0,R0                    GET MAIN STORAGE                        
         LA    R4,MAINTAB                                                       
*                                                                               
RNF03    CLI   0(R4),X'FF'              END OF TABLE                            
         BE    RNF05                                                            
         A     R0,0(R4)                 ADD THE LENGTH OF EACH TABLE            
         LA    R4,L'MAINTAB(R4)                                                 
         B     RNF03                                                            
*                                                                               
RNF05    ST    R0,MAINLEN               SAVE LENGTH OF TABLE                    
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN               START OF AREA                           
         LA    R4,MAINTAB                                                       
*                                                                               
RNF07    L     R3,4(R4)                                                         
         ST    R1,0(R3)                 A(START OF THIS TABLE)                  
         L     R0,0(R4)                 LENGTH OF THIS TABLE                    
         AR    R1,R0                    R1 TO NEXT AREA                         
         LA    R4,L'MAINTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   RNF07                                                            
*                                                                               
         BAS   RE,RDSJ                  BUILD TABLE OF SJ ACCOUNTS              
         B     C5XIT                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*              ROUTINE FOR REQUEST FIRST                                        
*-----------------------------------------------------------------*             
C506     CLI   MODE,REQFRST                                                     
         BNE   C522                                                             
         MVI   RCSUBPRG,0                                                       
*                                                                               
*                              *** BUILD TIME CARDS DATE TABLE***               
         LA    R7,DATETAB          DATE TABLE                                   
         MVI   0(R7),X'FF'         MARK END OF TABLE                            
         LA    R3,1                NUMBER OF DAYS FOR ADDAY                     
*                                                                               
         SR    R2,R2                                                            
         LA    R2,5                DEFAULT IS 5 DAILY TIME CARDS                
         CLI   QOPT1,C' '          IS IT OVERIDDEN BY OPTION 1                  
         BE    C508                NO - GO WITH 5                               
         PACK  DUB,QOPT1(1)                                                     
         CVB   R2,DUB              USE THE NUMBER FROM OPTION 1                 
*                                                                               
C508     MVC   WORK(6),QSTART                                                   
C508A    GOTO1 DATCON,DMCB,(0,WORK),(8,0(R7))       MMMDD/YY INTO TAB           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)          BUMP TO NEXT DATE           
         MVC   WORK(6),WORK+6                                                   
         LA    R7,8(R7)                             NEXT SLOT IN TABLE          
         MVI   0(R7),X'FF'                          MARK END OF TABLE           
         BCT   R2,C508A                                                         
*                                                                               
*                              *** DAYS BACK FOR CODE LOOKUP***                 
C510     GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)      RUN DATE INTO WORK          
         MVC   OLDATE,=3X'FF'                                                   
         USING PROFD,R5                                                         
         LA    R5,PROGPROF                          REPORT PROFILES             
         ZIC   R3,PROFDAYS                          DAYS IN PROFILE             
         LTR   R3,R3                                                            
         BZ    C512                                                             
         LNR   R3,R3                                SET TO NEGATIVE             
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,WORK+6,(1,OLDATE)                                    
C512     B     C5XIT                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*                                                                               
*-----------------------------------------------------------------*             
C522     CLI   MODE,PROCACC                                                     
         BNE   C530                                                             
         L     R5,ATSCODE          TABLE OF EMPLOYEE'S CLIENTS WORKD            
         USING BIND,R5                                                          
         XC    BININ,BININ         CLEAR TABLE                                  
         MVI   FCRDTRNS,C'N'                                                    
         USING PROFD,R5                                                         
         LA    R5,PROGPROF                          REPORT PROFILES             
         CLI   PROFSAL,C'Y'       PRINT CARDS FOR EVERYONE?                     
         BE    C524               YES - SKIP SALARY EL REQUIREMENT              
         MVI   ELCODE,X'52'                                                     
         BAS   RE,GETEL                                                         
         BNE   C5XIT               IGNORE IF NO RATE ELEMENT                    
*                                                                               
C524     L     R4,ADACC                                                         
         CLI   3(R4),C'9'          OR IF AN OVERHEAD ACCOUNT                    
         BE    C5XIT                                                            
         CLC   4(2,R4),=C'999'                                                  
         BE    C5XIT                                                            
         CLC   8(3,R4),=C'999'                                                  
         BE    C5XIT                                                            
*                                                                               
         L     R4,ADACC                                                         
         MVI   ELCODE,X'30'        STATUS ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   C5XIT                                                            
         USING ACSTATD,R4                                                       
         TM    ACSTSTAT,X'20'                                                   
         BO    C5XIT               SKIP LOCKED                                  
*                                                                               
*                                 *** SAVE EMPL NAME CODE AND DEPT***           
         L     R4,ADHEIRB          LOOK UP THIS EMPLOYEES OFF/DEPT              
         MVC   DEPT,3(R4)          SAVE HIS OFF/DEPT CODE                       
         L     R4,ADLVBNAM         ADDR OF DEPT NAME                            
         LA    R5,DPTNAME                                                       
         BAS   RE,NAMOUT                                                        
*                                                                               
         L     R4,ADACC                                                         
         MVC   EMPL(7),8(R4)       SAVE EMPLOYEE CODE                           
         L     R4,ADACCNAM         ADDR OF EMPLOYEES NAME                       
         LA    R5,EMPNAME          ADDR OF EMPLOYEE NAME SAVE AREA              
         BAS   RE,NAMOUT                                                        
         MVI   FCRDTRNS,C'Y'                                                    
         B     C5XIT                                                            
*                                                                               
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R5),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R5),ACNMNAME                                                 
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        BUILD TABLE OF CLIENTS THIS EMPLOYEE                                   
*        WORKED ON NOT OLDER THAN "OLDATE"                                      
*-----------------------------------------------------------------*             
C530     CLI   MODE,PROCTRNS                                                    
         BNE   C540                                                             
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                JUST TRANSACTIONS                    
         BNE   C5XIT                                                            
         CLC   TRNSDATE,OLDATE                                                  
         BL    C5XIT                       IGNORE OLD ONES                      
         MVI   ELCODE,X'51'                PROJECT CONTROL ELEMENT              
         BAS   RE,NEXTEL                                                        
         BNE   C5XIT                       IGNORE IF NO 51 ELEMENT              
*                                                                               
         USING ACPCD,R4                                                         
         USING TSCDE,R5                                                         
         LA    R5,WORK                                                          
         MVC   WORK,SPACES                                                      
         CLC   ACPCCLI+1(2),=C'SJ'                                              
         BNE   C5XIT                       SKIP NON CLIENT                      
         MVC   TSKEY,ACPCCLI                                                    
         MVC   TSKEY(1),RCCOMPFL                                                
         CLI   ACPCLEN,X'22'                                                    
         BL    C532                        NO PROJECT/TASK ON THIS ACC          
         MVC   TSPJT,ACPCPRJT+9                                                 
         MVC   TSTSK,ACPCTSK                                                    
*                                                                               
C532     GOTO1 BINADD,DMCB,(R5),ATSCODE    ADD TO TABLE                         
         B     C5XIT                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*                                                                               
*-----------------------------------------------------------------*             
C540     CLI   MODE,ACCLAST                                                     
         BNE   RUNL10                                                           
         L     R5,ANONCLI          ADD NON-CLIENTS OR CLIENTS                   
         ZIC   R3,NUMNON           TO PRINT ON ALL TIMESHEETS                   
         LTR   R3,R3                                                            
         BZ    C546                                                             
         MVC   WORK,SPACES                                                      
C544     MVC   WORK(15),0(R5)                                                   
         GOTO1 BINADD,DMCB,WORK,ATSCODE                                         
         LA    R5,15(R5)                                                        
         BCT   R3,C544                                                          
*                                                                               
C546     MVI   ACTIVITY,C'N'       SET ACTIVITY SWITCH                          
         BAS   RE,PGEPRNT          PRINT THE PAGE                               
         L     R5,ATSCODE          TABLE OF EMPLOYEE'S CLIENTS WORKD            
         B     C5XIT                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*        FREE MAIN STORAGE                                                      
*-----------------------------------------------------------------*             
RUNL10   CLI   MODE,RUNLAST                                                     
         BNE   C5XIT                                                            
         LM    R0,R1,MAINLEN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
         B     C5XIT                                                            
         EJECT                                                                  
*-----------------------------------------------------------------*             
*              PRINT A PAGE                                                     
*-----------------------------------------------------------------*             
PGEPRNT  NTR1                                                                   
         LA    R7,DATETAB          TIMESHEET DATE TABLE                         
         LA    R2,5                DEFAULT IS 5 DAILY TIME CARDS                
         CLI   QOPT1,C' '          IS IT OVERIDDEN BY OPTION 1                  
         BE    C547                NOT OVERIDDEN GO WITH 5                      
         PACK  DUB,QOPT1(1)                                                     
         CVB   R2,DUB              USE THE NUMBER FROM OPTION 1                 
*                                                                               
C547     MVC   PAGE,=H'1'                                                       
         CLI   0(R7),X'FF'         END OF DATE TABLE                            
         BNE   *+6                                                              
         DC    H'0'                DATE TABLE EMPTY                             
         MVC   CARDDATE,0(R7)      DATE FOR THIS TIMESHEET                      
         L     R5,ATSCODE          TABLE OF EMPLOYEE'S CLIENTS WORKD            
         USING BIND,R5                                                          
         L     R3,BININ            NUMBER IN TABLE                              
         LTR   R3,R3                                                            
         BNZ   C547A               CLIENT IN TABLE                              
         BAS   RE,HEADUP           JUST PRINT HEADINGS                          
         B     C555                                                             
*                                                                               
C547A    L     R5,BINTABLE                                                      
*                                                                               
         USING TSCDE,R5                                                         
         USING C5PD,R4                                                          
C548     LA    R4,C5PWK1                                                        
         MVC   C5PNAME(C5PLNQ),SPACES   CLEAR PRINT WORK LINE                   
         MVC   C5PNUM(3),TSKEY+3       MOVE IN CLIENT CODE                      
         LA    R1,C5PNUM+2                                                      
         CLI   0(R1),C' '              IS CLIENT CODE 2 DIGITS?                 
         BE    *+8                     YES- PUT IN DASH                         
         LA    R1,1(R1)                MUST BE 3 DIGITS                         
         MVI   0(R1),C'-'                                                       
         MVC   1(3,R1),TSKEY+6         MOVE IN PRODUCT CODE                     
*                                                                               
         LR    R6,R5                   GET NAME FROM CODELIST                   
         L     R5,ACODLST              ADDR OF CODELIST                         
         USING BIND,R5                                                          
         MVC   DMCB+8(16),BININ        MOVE IN PARMS 3,4,5,6                    
         L     R8,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(R6),(R8)                                           
         CLI   DMCB,0                                                           
         BNE   C550                    NOT FOUND                                
         L     R5,DMCB                                                          
         USING CLICDE,R5                                                        
         MVC   C5PNAME,CLINAME         MOVE IN NAME                             
*                                                                               
C550     CLI   ACTIVITY,C'Y'           ANY ACTIVITY                             
         BE    C552                    YES- GO TO PRINT                         
         MVI   ACTIVITY,C'Y'           SET ACTIVITY SWITCH TO YES               
         BAS   RE,HEADUP                                                        
*                                                                               
C552     MVI   PGEMAX,DETMAX           SET MAX FOR DETAIL                       
         BAS   RE,PRNTDET                                                       
         LR    R5,R6                                                            
         LA    R5,TSLEN(R5)            SET TABLE TO NEXT CLIENT                 
         BCT   R3,C548                                                          
*                                                                               
C555     MVI   ACTIVITY,C'N'           RESET ACTIVITY SWITCH                    
         LA    R7,8(R7)                RESET FOR NEXT TIME SHEET DATE           
         BCT   R2,C547                 PRINT ANOTHER TS FOR THIS GUY            
         B     C5XIT                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT DETAIL LINES                                    
*                                                                               
PRNTDET  NTR1                                                                   
         ZIC   R2,PGEMAX           MAX  ALLOWED FOR PAGE                        
         ZIC   R1,LINE             CURRENT LINE                                 
         AH    R1,=H'1'            PLUS NUMBER I WNAT TO PRINT                  
         CR    R1,R2               MUST HAVE ENOUGH LINES                       
         BNH   *+8                                                              
         BAS   RE,HEADUP           OR ELSE HEADUP NEW                           
*                                                                               
         LA    R1,P                TO PRINT LINE                                
         LA    R2,C5PWK1                                                        
         MVC   0(C5PLNQ,R1),0(R2)                                               
         MVC   0(C5PLNQ,R2),SPACES     CLEAR MY WORK LINES                      
         GOTO1 ACREPORT                AND PRINT IT                             
         ZIC   R1,LINE                 ADD 1 TO CURRENT LINE                    
         AH    R1,=H'1'                SO DETAIL PORTION OF REPORT              
         STC   R1,GETLNE               SKIPS LINES                              
         BAS   RE,SKIPLNE                                                       
C5XIT    XMOD1 1                                                                
         EJECT                                                                  
*              ROUTINE TO SETUP AND PRINT HEADLINES                             
*                                                                               
HEADUP   NTR1                                                                   
         GOTO1 PRINT,DMCB,P,=C'BC01'    SKIP TO CHANNEL 1                       
         MVI   LINE,1              SET LINE NUMBER                              
*                                                                               
         MVI   GETLNE,DTELNE       LINE NUMBER FOR DATE                         
         BAS   RE,SKIPLNE                                                       
         MVC   P+39(8),CARDDATE    THIS TS DATE INTO P LINE                     
*                                                                               
         MVI   GETLNE,DEPTLNE      LINE NUMBER FOR DEPT CODE                    
         BAS   RE,SKIPLNE                                                       
         MVC   P(3),DEPT           MOVE IN OFF/DEPT CODE                        
         MVC   P+9(28),DPTNAME     AD DEPT NAME                                 
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   GETLNE,ACTLNE       LINE NUMBER FOR EMPLOYEE CODE                
         BAS   RE,SKIPLNE                                                       
         MVC   P(7),EMPL           MOVE IN EMPLOYEE CODE                        
         MVC   P+9(28),EMPNAME     MOVE IN EMPLOYEE NAME                        
*                                                                               
         MVI   GETLNE,PGELNE       LINE NUMBER FOR PAGE                         
         BAS   RE,SKIPLNE                                                       
         MVC   P+39(4),=C'PAGE'                                                 
         EDIT  (B2,PAGE),(3,P+44),3,ZERO=BLANK                                  
         GOTO1 ACREPORT                                                         
         MVC   HALF,PAGE           UPDATE PAGE NUMBER                           
         LH    R3,HALF                                                          
         AH    R3,=H'1'                                                         
         STH   R3,HALF                                                          
         MVC   PAGE,HALF                                                        
*                                                                               
         MVI   GETLNE,DETLNE                                                    
         BAS   RE,SKIPLNE          SKIP TO DETAIL LINE                          
         B     XIT                                                              
         EJECT 1                                                                
*              ROUTINE TO SKIP TO NEW LINE                                      
*                                                                               
SKIPLNE  NTR1                                                                   
         ZIC   R1,GETLNE           LINE NUMBER I WANT TO SKIP TO                
         ZIC   R2,LINE             CURRENT LINE                                 
         SR    R1,R2                                                            
         BZ    XIT                 ALREADY AT THE LINE I WANT                   
         BP    *+6                                                              
         DC    H'0'                LOST TRACK                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKIP+1(3),DUB+6(2)                                               
         MVC   SKIP(2),=C'BL'                                                   
         AR    R2,R1               ADD SKIP COUNT  TO LINE                      
         STC   R2,LINE                                                          
         GOTO1 PRINT,DMCB,P,SKIP   AND SKIP                                     
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO PUT SJ ACCOUNTS INTO CODELIST                         
*                                                                               
         USING ACKEYD,R4                                                        
RDSJ     NTR1                                                                   
         MVI   NUMNON,0                                                         
         L     R4,ACREC                                                         
         MVC   ACKEYACC(ACRECORD-ACKEYACC),SPACES   CLEAR KEY                   
         MVC   ACKEYACC(1),RCCOMPFL                 MOVE IN COMPANY             
         MVC   ACKEYACC+1(2),=C'SJ'                         U/L SJ              
         MVI   ACKEYACC+3,X'41'                                                 
RDSJ1    BAS   RE,HIGH                                                          
         CLC   ACKEYACC(3),SAVEKEY                  SAME KEY                    
         BNE   C5XIT                                NO -  FINISHED SJ           
         CLI   ACKEYACC+9,C' '     IS THERE A JOB                               
         BNE   RDSJ3               YES - FIX KEY TO LOOK FOR NEXT PROD          
         BAS   RE,ADDSJ            ADD TO CODE LIST                             
         L     R4,ACREC                                                         
         MVI   ELCODE,X'30'        STATUS ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   RDSJ3                                                            
         USING ACSTATD,R4                                                       
         CLI   ACSTFILT+1,C'T'     ACCOUNT FILTER 2 = T                         
         BNE   RDSJ3               MEANS PRINT ON ALL TIMESHEETS                
         L     R4,ACREC                                                         
         ZIC   R1,NUMNON                                                        
         MH    R1,=H'15'                                                        
         L     R5,ANONCLI          ADDR OF PRINT ON ALL TS TABLE                
         AR    R5,R1                                                            
         MVC   0(15,R5),0(R4)                                                   
         ZIC   R0,NUMNON                                                        
         AH    R0,=H'1'                                                         
         STC   R0,NUMNON                                                        
         USING ACKEYD,R4                                                        
RDSJ3    L     R4,ACREC                                                         
         ZIC   R1,ACKEYACC+8      FIX KEY TO SKIP JOBS                          
         AH    R1,=H'1'                                                         
         STC   R1,ACKEYACC+8                                                    
         B     RDSJ1                                                            
         EJECT                                                                  
*              ROUTINE TO ADD CLIENT TO CODE LIST TABLE                         
*                                                                               
         USING ACKEYD,R4                                                        
         USING CLICDE,R5                                                        
ADDSJ    NTR1                                                                   
         L     R4,ACREC                                                         
         LA    R5,WORK                                                          
         MVC   CLIKEY(CLILEN),SPACES        CLEAR KEY                           
         MVC   CLIKEY,ACKEYACC              MOVE IN KEY                         
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   ADDSJ2                                                           
         USING ACNAMED,R4                                                       
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   CLINAME(0),ACNMNAME          MOVE IN NAME                        
ADDSJ2   GOTO1 BINADD,DMCB,WORK,ACODLST                                         
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         B     C5XIT                                                            
         EJECT                                                                  
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
*                                                                               
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         L     R6,BINTABLE         A(TABLE)                                     
         L     R4,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         B     C5XIT                                                            
         EJECT                                                                  
*              DATAMGR INTERFACE                                                
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   SAVEKEY,0(R4)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
GTREC    NTR1                                                                   
         L     R4,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R4),(R4)                       
         B     C5XIT                                                            
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              CONSTANTS                                                        
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(CODLIST)                                                       
         DC    A(TSCODE)                                                        
         DC    A(NONCLI)                                                        
         DC    V(SQUASHER)                                                      
         DC    X'FF'                                                            
*                                                                               
CODNUM   EQU   30000                                                            
TSNUM    EQU   300                                                              
*                                                                               
MAINTAB  DS    0D                                                               
         DC    AL4((((CODNUM*CLILEN)+7)/8)*8),A(CODTAB)                         
         DC    AL4((((TSNUM*TSLEN)+7)/8)*8),A(TSTAB)                            
         DC    X'FF'                                                            
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
DATETAB  DS    CL60                                                             
*                                                                               
NONCLI   DS    0D                                                               
         DS    100CL15                                                          
*                                                                               
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL1000                                                           
*                                                                               
CODLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CLILEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'CLIKEY)       KEY LENGTH                                   
         DC    AL4(CODNUM)         MAX IN TABLE                                 
CODTAB   DS    AL4(0)              THE TABLE                                    
*                                                                               
TSCODE   DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(TSLEN)          RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'TSKEY)        KEY LENGTH                                   
         DC    AL4(TSNUM)          MAX IN TABLE                                 
TSTAB    DS    AL4(0)              THE TABLE                                    
*                                                                               
         EJECT                                                                  
*                                                                               
*              DSECT FOR STORAGE AREA                                           
*                                                                               
ACC5D    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
ACODLST  DS    A                                                                
ATSCODE  DS    A                                                                
ANONCLI  DS    A                                                                
SQUASHER DS    V                                                                
*                                                                               
MAINLEN  DS    F                                                                
MAINBGN  DS    F                                                                
*                                                                               
NUMNON   DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    CL1                                                              
GETLNE   DS    CL1                                                              
OLDATE   DS    CL3                                                              
SAVEKEY  DS    CL42                                                             
DEPT     DS    CL3                 OFFICE/DEPT CODE                             
DPTNAME  DS    CL36                DEPT NAME                                    
EMPL     DS    CL7                 EMPLOYEE CODE                                
EMPNAME  DS    CL36                EMPLOYEE NAME                                
CARDDATE DS    CL8                 TIME SHEET DATE                              
PGEMAX   DS    CL1                                                              
*                                                                               
PROFS    DS    CL16                PROGRAM PROFILES                             
*                                                                               
C5PWK1   DS    (C5PLNQ)C           PRINT LINE 1                                 
*                                                                               
ACTIVITY DS    CL1                 HAS ACCOUNT HAD ACTIVITY                     
SKIP     DS    CL4                                                              
*                                                                               
DEPTLNE  EQU   1                   LINE NUMBER FOR OFF/DPT CODE +NAME           
DTELNE   EQU   1                   LINE NUMBER TO PRINT DATE                    
ACTLNE   EQU   4                   LINE NUMBER TO PRINT ACCT NUMBER             
PGELNE   EQU   4                   LINE NUMBER TO PRINT PAGE                    
DETLNE   EQU   28                  LINE NUMBER FOR DETAIL                       
XXXLNE   EQU   28                  LINE NUMBER FOR DETAIL LINEUP                
DETMAX   EQU   57                  LAST DETAIL LINE                             
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
*                                                                               
C5PD     DSECT                                                                  
C5PNAME  DS    CL25                CLIENT PRODUCT NAME                          
         DS    CL3                                                              
C5PNUM   DS    CL7                 CLIENT PRODUCT CODE                          
C5PLNQ   EQU   *-C5PNAME           LENGTH OF LINE                               
         EJECT                                                                  
*              DSECT FOR PROFILES                                               
PROFD    DSECT                                                                  
PROFDAYS DS    CL1                 DAYS FOR PREVIOUS CODES 0-90                 
PROFSAL  DS    CL1                 SKIP SALARY REQUIREMENT FOR TS               
*                                                                               
*              DSECT FOR BINSRCH PARAMETERS                                     
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
*                                                                               
*              DSECT FOR CODE LIST TABLE                                        
CLICDE   DSECT                                                                  
CLIKEY   DS    CL15                                                             
CLINAME  DS    CL36                                                             
CLILEN   EQU   *-CLIKEY                                                         
*                                                                               
*              DSECT FOR TIME SHEET CODES                                       
TSCDE    DSECT                                                                  
TSKEY    DS    CL15                                                             
TSPJT    DS    CL6                                                              
TSTSK    DS    CL2                                                              
TSLEN    EQU   *-TSKEY                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
*        DDLOGOD                                                                
*        ACGENBOTH                                                              
*        ACGENPOST                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
*        DDCNTRL                                                                
*        DDREPXTRAD                                                             
*        DDREPMASTD                                                             
*        DDREMOTED                                                              
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENPOST                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE DDCNTRL                                                        
       ++INCLUDE DDREPXTRAD                                                     
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACREPC502 05/23/16'                                      
         END                                                                    
