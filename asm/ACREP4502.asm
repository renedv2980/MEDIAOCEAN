*          DATA SET ACREP4502  AT LEVEL 007 AS OF 05/01/02                      
*PHASE AC4502A,+0                                                               
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE SQUASHER                                                               
         TITLE 'DISTRIBUTION SCHEME LISTING'                                    
AC4502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC45**,RR=R9                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC4502D,RC                                                       
         LA    R7,SORTREC                                                       
         USING SORTD,R7                                                         
*              NOW WHERE DO WE GO                                               
         SPACE 2                                                                
         CLI   MODE,RUNFRST                                                     
         BE    RUNFT                                                            
         CLI   MODE,REQFRST                                                     
         BE    REQFT                                                            
         CLI   EIJ,X'00'                    ERROR IN JOB                        
         BNE   EOJ                          BRANCH TO END OF JOB                
         CLI   MODE,LEDGFRST                                                    
         BE    LEDGFT                                                           
         CLI   MODE,REQLAST                                                     
         BE    REQLT                                                            
XIT      XMOD1 1                                                                
         EJECT                                                                  
RUNFT    EQU   *                                                                
         LA    R1,SRTKEYQ                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         SPACE 1                                                                
         LA    R1,SRTLENQ                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         SPACE 1                                                                
         LA    R1,BXHOOK                                                        
         ST    R1,HEADHOOK                                                      
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
REQFT    EQU   *                                                                
         MVI   EIJ,X'00'                  SET ERROR TO NONE                     
         MVI   FTIME,C'Y'                 FIRST TIME THROUGH                    
         MVI   CLEARHED,C'N'              MANUALLY CLEAR HEADINGS AND           
         MVI   CLEARMID,C'N'                             MIDLINES               
         MVI   FORCEHED,C'Y'                                                    
         MVI   FORCEMID,C'Y'                                                    
         MVC   ROWLTNAM,SPACES          CLEAR FROM LAST REQUEST                 
         MVC   HEAD1,SPACES                                                     
         MVC   HEAD2,SPACES                                                     
         MVC   HEAD3,SPACES                                                     
         MVC   HEAD4,SPACES                                                     
         MVC   HEAD5,SPACES                                                     
         MVC   HEAD6,SPACES                                                     
         MVC   HEAD7,SPACES                                                     
         MVC   HEAD8,SPACES                                                     
         MVC   HEAD9,SPACES                                                     
         MVC   HEAD10,SPACES                                                    
         MVC   HEAD11,SPACES                                                    
         MVC   MID1,SPACES                                                      
         MVC   PREVACCT,SPACES                                                  
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,0                                   
         MVC   DUMPON,QOPT7                                                     
         MVC   SCHMCODE,QOPT1                                                   
         MVI   STAT,C'N'                                                        
         B     XIT                                                              
         EJECT                                                                  
         USING ACANALD,R2                                                       
LEDGFT   EQU   *                                                                
         L     R2,ADLEDGER         LEDGFRST                                     
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL            FIND THE SCHEME (WORK CODE)                  
         BE    LEDGFT20                                                         
         MVI   EIJ,X'01'           NO SCHEMES AT ALL                            
         B     EOJ                                                              
LEDGFT20 EQU   *                                                                
         CLC   ACANCODE,SCHMCODE   SEE IF IT IS THE RIGHT SCHEME                
         BE    LEDGFT30            FOUND THE SCHEME                             
         BAS   RE,NEXTEL           GET NEXT SCHEME                              
         BE    LEDGFT20                                                         
         MVI   EIJ,X'02'           NO SUCH SCHEME FOUND                         
         B     EOJ                                                              
         SPACE 1                                                                
LEDGFT30 EQU   *                                                                
         MVC   SCHNAME,ACANDESC                                                 
         L     R2,ADLEDGER         LEDGFRST                                     
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    4(R2),X'01'                                                      
         BNE   LEDGFT40                                                         
         MVI   STAT,C'Y'                                                        
LEDGFT40 EQU   *                                                                
         L     R2,ADLEDGER         LEDGFRST                                     
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,0                                                             
         LA    R2,2(R2)                                                         
         MVI   ROWLEN,X'00'                                                     
         SPACE 1                                                                
         LA    R5,ROWTABLE                                                      
         LA    R6,ROWLEN+1                                                      
         SPACE 1                                                                
LEDGFT50 EQU   *                                                                
         MVC   0(1,R6),0(R2)                                                    
         MVC   0(15,R5),1(R2)                                                   
         LA    R2,16(R2)                                                        
         LA    R5,15(R5)                                                        
         LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
         STC   R3,NOL           NUMBER OF LEVELS                                
         CLI   NOL,X'04'                                                        
         BE    LEDGFT90                                                         
         CLI   0(R2),X'00'                                                      
         BE    LEDGFT90                                                         
         B     LEDGFT50                                                         
         SPACE 1                                                                
LEDGFT90 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
         USING ACKEYD,R4                                                        
REQLT    EQU   *                                                                
         L     R4,ADIO          LOAD ADDRESS OF IO AREA FOR RECORD              
         LA    R6,PREVACCT                                                      
         MVC   PREVACCT,ACKEYACC                                                
         MVI   SRTLV,X'00'                                                      
         SPACE 1                                                                
REQLT10  EQU   *                                                                
         GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'ACCOUNT',KEY,(R4)                     
         CLC   ACKEYACC(3),QCOMPANY                                             
         BNE   REQLT50                                                          
         MVC   SRTCANAM,SPACES         CLEAR CONTRA ACCT NAME IN SORTD          
         SPACE 1                                                                
         LR    R2,R4                                                            
         MVI   ELCODE,X'20'            GET LEVEL ACCOUNT NAME                   
         BAS   RE,GETEL                                                         
         BNE   REQLT30                                                          
         SPACE 1                                                                
         MVC   SRTLVNAM,SPACES         BLANK OUT LV NAME IN SORT DSECT          
         ZIC   R1,1(R2)                FOR A PATICULAR LEVEL                    
         SH    R1,=H'3'                THEN MOVE IN ACCTUAL NAME                
         LA    R5,SRTLVNAM                                                      
         EX    R1,LEVNAME              FOR THAT LEVEL                           
         STC   R1,SRTNAML                                                       
         SPACE 1                                                                
         BAS   RE,GETLEV                                                        
         B     REQLT40                                                          
         SPACE 1                                                                
REQLT30  EQU   *                                                                
         LR    R2,R4                                                            
         MVI   ELCODE,X'43'            NO LEVEL NAME SO USE THE FIRST'S         
         BAS   RE,GETEL                RECORDS NAME AND GET CONTRA ACCT         
         BE    *+6                     NAME                                     
         DC    H'0'                                                             
         SPACE 1                                                                
         ZIC   R1,1(R2)                                                         
         SH    R1,=H'18'               MOVE IN CONTRA ACCT NAME                 
         LA    R5,SRTCANAM                                                      
         EX    R1,CANAME               FOR THAT LEVEL                           
         STC   R1,SRTCAL                                                        
         SPACE 1                                                                
         USING ACDISTD,R2                                                       
REQLT40  EQU   *                                                                
         LR    R2,R4                                                            
         AH    R2,DATADISP                                                      
         BAS   RE,GETSCHM                                                       
         BNE   REQLT10                                                          
         SPACE 1                                                                
         ZAP   SRTLVTOT,=P'0'                                                   
         ZAP   SRTLVTOT,ACDIVAL        MOVE IN TOTAL FOR THAT LEVEL             
         MVC   SRTCNTRA,ACKEYCON       MOVE IN THE SORT KEY                     
         MVC   SRTACCT,ACKEYACC                                                 
         SPACE 1                                                                
         GOTO1 SORTER,DMCB,=C'PUT',(R7)                                         
         B     REQLT10                                                          
         SPACE 1                                                                
REQLT50  EQU   *                                                                
         USING PRINTD,R8            INITIALIZE FOR REPORT                       
         BAS   RE,LOADPRT           LOAD IN HEADING TITLES, PRT. ADDRS.         
         SPACE 1                                                                
REQLT60  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'GET'                                              
         ICM   R7,15,DMCB+4           EOF SORTER RECORDS?                       
         BZ    REQLT90                                                          
         SPACE 1                                                                
         CLI   DUMPON,X'40'                                                     
         BE    REQLT62                                                          
         MVC   SORTREC,0(R7)                                                    
         GOTO1 PRNTBL,DMCB,(3,=C'PUT'),SORTREC,C'DUMP',SRTLENQ,=C'2D'           
         SPACE 1                                                                
REQLT62  EQU   *                                                                
         CLC   18(1,R7),=C'*'                                                   
         BNE   REQLT65                IF NOT A ****... ACCT. BRANCH             
         ZAP   JOBTOTAL,SRTLVTOT      ELSE PUT IN ACCT JOB TOTAL                
         B     REQLT60                                                          
         SPACE 1                                                                
REQLT65  EQU   *                                                                
         CLC   SRTCNTRA,PREVACCT      SEE IF JOB (CA) HAS CHANGED               
         BE    REQLT70                IF NOT THEN BRANCH ELSE                   
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'          NEW JOB SO PAGE                           
         ZIC   R2,NOL                                                           
         GOTO1 PRINTTOT,DMCB,(R2),2                                             
         LA    R2,HEAD5               PUT JOB NAME IN CORRECT HEADING           
         MVC   HEAD5,SPACES           CLEAR PREVIOUS JOB                        
         CLC   SRTCNTRA,SPACES                                                  
         BE    *+10                                                             
         MVC   1(3,R2),=C'JOB'                                                  
         ZIC   R3,INDENT              TO LINE UP WITH OTHER HEADINGS            
         LA    R2,2(R3,R2)                                                      
         MVC   0(14,R2),SRTCNTRA+1                                              
         MVC   16(L'SRTCANAM,R2),SRTCANAM   PUT IN JOB NAME                     
         MVC   PREVACCT,SRTCNTRA            REPLACE PREVIOUS ACCT.              
         SPACE 1                                                                
REQLT70  EQU   *                                                                
         BAS   RE,GETLEN            LEN-> R1,SRTLV-> R3,LEN TABLE-> R5          
         BCTR  R5,R0                BUMP BACKWARDS IN ACCT. LEN TABLE           
         SR    R4,R4                                                            
         ICM   R4,1,0(R5)           PUT HIGHER LV. LEN IN R4                    
         SR    R1,R4                LEN- HIGHER LV. LEN = NUM OF CHAR           
         LA    R6,SRTACCT                                                       
         LA    R6,3(R4,R6)          STARTING ADDR. OF LEV ACCT. CODE            
         SPACE 1                                                                
         BCTR  R3,R0                CALCULATE DISPLACMENT INTO TABLE            
         MH    R3,=H'04'                                                        
         LA    R4,PRTADDR           LOAD ADDR TABLES OF PRINTLINE ADDRS         
         L     R4,0(R4,R3)          LOAD CORRECT ADDR.                          
         MVC   0(14,R4),SPACES      CLEAR CURRENT LINE                          
         SPACE 1                                                                
         SH    R1,=H'03'            ADJUST FOR U/L                              
         EX    R1,PUTNUM            PUT ACCT FOR LEN (R1) TO REPORT             
         LA    R4,2(R1,R4)          MOVE 2 SPACES + ACCT LEN. IN RPT            
         MVC   0(36,R4),SPACES      CLEAR CURRENT LINE                          
         ZIC   R1,SRTNAML           GET LENGTH ON SRT NAME                      
         EX    R1,PUTNAM            MOVE IT TO CURRENT LINE                     
         LA    R8,38(R4)            LOAD ADDR. FOR PRTLINE. DSECT               
         SPACE 1                                                                
         LA    R4,LVTOTALS          LOAD ADDR TABLES OF TOTALS                  
         ZIC   R3,SRTLV             GET CURRENT RECORD LEVEL                    
         SH    R3,=H'01'            CALCULATE DISPLACMENT INTO TABLE            
         MH    R3,=H'06'            MOVE LEN. OF TABLE ENTRY                    
         LA    R4,0(R3,R4)                                                      
         ZAP   0(6,R4),SRTLVTOT     PUT LV TOTAL IN TABLE                       
         SPACE 1                                                                
         CLC   SRTLV,NOL            IF LOWEST LV. THEN IT IS A PRTLINE          
         BE    REQLT80              NOT AT LOWEST LV. SO BRANCH                 
         CLI   NOL,X'01'            IF ONLY ONE LEVEL SKIP THIS                 
         BNE   REQLT85                                                          
         B     REQLT60              GET NEXT SORT RECORD                        
         SPACE 1                                                                
REQLT80  EQU   *                                                                
         BAS   RE,PRINTPER                                                      
         MVI   FTIME,C'N'                                                       
         SPACE 1                                                                
         B     REQLT60                 GET NEXT SORT RECORD                     
         SPACE 1                                                                
REQLT85  EQU   *                                                                
         ZIC   R3,NOL                  PUT NUMBER OF LEVELS IN R3               
         LR    R2,R3                                                            
         ZIC   R4,SRTLV                GET CURRENT LEVEL                        
         SR    R2,R4                                                            
         GOTO1 PRINTTOT,DMCB,(R2),2                                             
         BCTR  R3,R0                   SUBTRACT ONE FOR LOGIC                   
         SPACE 1                                                                
         CLI   NOL,X'02'                                                        
         BE    *+10                                                             
         CR    R3,R4                   COMPARE TO DETERMINE IF                  
         BE    *+8                     WE SHOULD PAGE OR PRT. MIDLINE           
         MVI   FORCEHED,C'Y'           HEADLINE CHANGED SO PAGE                 
         MVI   FORCEMID,C'Y'           NEW MID LINE SO FORCE MID                
         B     REQLT60                                                          
         SPACE 1                                                                
REQLT90  EQU   *                                                                
         ZIC   R2,NOL                                                           
         GOTO1 PRINTTOT,DMCB,(R2),2                                             
REQLT99  EQU   *                                                                
         GOTO1 SORTER,DMCB,=C'END'                                              
         B     XIT                                                              
         SPACE 1                                                                
LEVNAME  MVC   0(0,R5),2(R2)        PUT IN ACCOUNT RECORD NAME IN SRT           
CANAME   MVC   0(0,R5),17(R2)       PUT IN CONTRA NAME IN SRT RECORD            
PUTNUM   MVC   0(0,R4),0(R6)                                                    
PUTNAM   MVC   0(0,R4),SRTLVNAM                                                 
         EJECT                                                                  
*        END OF JOB BECAUSE OF ERROR                                            
         SPACE 1                                                                
EOJ      EQU   *                                                                
         CLI   EIJ,X'01'                                                        
         BE    EOJ01                                                            
         CLI   EIJ,X'02'                                                        
         BE    EOJ02                                                            
         B     EOJ90            ERROR FLAGGED BUT ERROR NOT IN TABLE            
         SPACE 1                                                                
EOJ01    MVC   PSECOND+34(28),=CL28'SCHEME CODES ARE NOT SET UP'                
         B     EOJ99                                                            
         SPACE 1                                                                
EOJ02    MVC   PSECOND+34(15),=CL15'NO SCHEME FOUND'                            
         B     EOJ99                                                            
         SPACE 1                                                                
EOJ90    MVC   PSECOND+34(29),=CL29'ERROR FLAGGED REASON UNKNOWN'               
EOJ99    MVC   P+34(15),=CL15'**** ERROR ****'                                  
         CLI   MODE,RUNLAST                                                     
         BNE   EOJ9X                                                            
         GOTO1 SORTER,DMCB,=C'END'                                              
EOJ9X    EQU   *                                                                
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
*              GET THE CORRECT SCHEME CODE                                      
         SPACE 2                                                                
         USING ACDISTD,R2                                                       
GETSCHM  EQU   *                                                                
         ST    RE,RESAVE                                                        
         MVI   ELCODE,X'62'                                                     
GETSCHM1 BAS   RE,NEXTEL                                                        
         BNE   GETSCHM9                                                         
         CLC   ACDICODE,SCHMCODE                                                
         BNE   GETSCHM1                                                         
GETSCHM9 L     RE,RESAVE                                                        
         BR    RE                                                               
         SPACE 3                                                                
         EJECT                                                                  
*        GET LEVEL FOR THE THE RECORD                                           
         SPACE 3                                                                
GETLEV   NTR1                                                                   
GETLEV1  BAS   RE,GETLEN            LEN-> R1,SRTLV-> R3,LEN TABLE-> R5          
         EX    R1,RECCOMP           BUMP BACKWARDS IN ACCT. LEN TABLE           
         BE    GETLEV4                                                          
         CLI   SRTLV,X'00'                                                      
         BNE   GETLEV8                                                          
         SPACE 1                                                                
GETLEV4  LA    R3,1(R3)                                                         
         STC   R3,SRTLV                                                         
         BAS   RE,GETLEN            LEN-> R1,SRTLV-> R3,LEN TABLE-> R5          
         EX    R1,MOVEACC           BUMP BACKWARDS IN ACCT. LEN TABLE           
         B     GETLEV9                                                          
         SPACE 1                                                                
GETLEV8  SH    R3,=H'01'                                                        
         STC   R3,SRTLV                                                         
         BAS   RE,GETLEN            LEN-> R1,SRTLV-> R3,LEN TABLE-> R5          
         B     GETLEV1              BUMP BACKWARDS IN ACCT. LEN TABLE           
         SPACE 1                                                                
GETLEV9  B     XIT                                                              
         SPACE 1                                                                
RECCOMP  CLC   0(0,R6),ACKEYACC                                                 
MOVEACC  MVC   0(0,R6),ACKEYACC                                                 
         SPACE 1                                                                
         EJECT                                                                  
*        GET LENGTH OF ACCOUNT FOR A LEVEL                                      
         SPACE 1                                                                
GETLEN   ZIC   R3,SRTLV                                                         
         LA    R5,ROWLEN                                                        
         LA    R5,0(R3,R5)                                                      
         SR    R1,R1                                                            
         ICM   R1,1,0(R5)       PUT LEN. FOR THAT LEVEL IN R1                   
         LA    R1,2(R1)         LEN.= LEN.+ 2 (COMPANY,UNIT,LEDGER)             
         BR    RE                                                               
         EJECT                                                                  
*        LOAD PRINT LINE ADDRESSES INTO TABLE                                   
         SPACE 1                                                                
LOADPRT  NTR1                                                                   
         MVC   ROWLTNAM(15),ROWTABLE                                            
         MVC   ROWLTNAM+16(8),=C'TO TOTAL'                                      
         BAS   RE,REVERSE                                                       
         SPACE 1                                                                
         MVC   RCSUBPRG,NOL                                                     
         ZIC   R2,NOL                                                           
         LA    R6,8                                                             
         LA    R2,1(R2)                  1 MORE COL.THAN THE NUM OF LV.         
         LA    R3,1(R2)                  ADD A OVERFLOW LINE TO TABLE           
         SPACE 1                                                                
         GOTO1 PROLLER,DMCB,0,TOTLINES,(R3),(R2)         CREATE TABLE           
         SPACE 1                                                                
         LA    R2,P+6                    LOAD ADDR. TO BE PUT IN TABLE          
         LA    R3,MID1                                                          
         LA    R4,HEAD6                                                         
         LA    R5,HEAD7                                                         
         SPACE 1                                                                
         CLI   NOL,X'01'                 ONE LEVEL REPORT                       
         BNE   LOADPRT2                                                         
         ST    R2,PRTADDR                                                       
         SPACE 1                                                                
LOADPRT2 EQU   *                                                                
         CLI   NOL,X'02'                      TWO LEVEL REPORT                  
         BNE   LOADPRT3                                                         
         MVC   1(15,R4),ROWTABLE+15           HEADLINE NAME                     
         GOTO1 SQUASHER,DMCB,ROWTABLE+15,15                                     
         CLI   DMCB+7,8                       GET LENGTH OF STRING              
         BL    *+8                                                              
         L     R6,DMCB+4                                                        
         LA    R4,2(R6,R4)                                                      
         ST    R4,PRTADDR                     HEADLINE PRINT ADDR.              
         ST    R2,PRTADDR+4                   PRINTLINE ADDR.                   
         SPACE 1                                                                
LOADPRT3 EQU   *                                                                
         CLI   NOL,X'03'                      THREE LEVEL REPORT                
         BNE   LOADPRT4                                                         
         MVC   1(15,R4),ROWTABLE+30           HEADLINE NAME                     
         MVC   4(15,R3),ROWTABLE+15           MIDLINE NAME                      
         GOTO1 SQUASHER,DMCB,ROWTABLE+15,15   GET LEN. OF MID NAME              
         L     R6,DMCB+4                                                        
         LA    R3,5(R6,R3)                                                      
         ST    R3,PRTADDR+4                   MIDLINE PRINT ADDR.               
         SPACE 1                                                                
         LA    R6,8                           RELOAD FOR INDENT                 
         GOTO1 SQUASHER,DMCB,ROWTABLE+30,15   GET LEN. OF HEADNAME              
         CLI   DMCB+7,8                       GET LENGTH OF STRING              
         BL    *+8                                                              
         L     R6,DMCB+4                                                        
         LA    R4,2(R6,R4)                                                      
         ST    R4,PRTADDR                      HEADLINE PRINT ADDR.             
         ST    R2,PRTADDR+8 PRINTLINE ADDR.                                     
         SPACE 1                                                                
LOADPRT4 EQU   *                                                                
         CLI   NOL,X'04'                           FOUR LEVEL REPORT            
         BNE   LOADPRT9                                                         
         MVC   1(15,R4),ROWTABLE+45                HEADLINE NAME                
         MVC   1(15,R5),ROWTABLE+30                SECOND HEADLINE NAME         
         MVC   4(15,R3),ROWTABLE+15                MIDLINE NAME                 
         GOTO1 SQUASHER,DMCB,ROWTABLE+15,15        GET LEN. OF MID NAME         
         L     R6,DMCB+4                                                        
         LA    R3,5(R6,R3)                       NAME LEN + ADDR. + 5           
         ST    R3,PRTADDR+8                      MIDLINE PRINT ADDR.            
         SPACE 1                                                                
         LA    R6,8                                                             
         GOTO1 SQUASHER,DMCB,ROWTABLE+30,15      GET LEN. OF HEAD2 NAME         
         CLI   DMCB+7,8                          GET LENGTH OF STRING           
         BL    *+8                                                              
         L     R6,DMCB+4                                                        
         GOTO1 SQUASHER,DMCB,ROWTABLE+45,15      GET LEN. OF HEAD1 NAME         
         C     R6,DMCB+4                                                        
         BH    *+8                               TAKE HIGHER NUMBER             
         L     R6,DMCB+4                                                        
         LA    R4,2(R6,R4)                                                      
         LA    R5,2(R6,R5)                                                      
         ST    R4,PRTADDR                   HEADLINE1 PRINT ADDR.               
         ST    R5,PRTADDR+4                 HEADLINE2 PRINT ADDR.               
         ST    R2,PRTADDR+12                PRINTLINE ADDR.                     
         SPACE 1                                                                
LOADPRT9 EQU   *                                                                
         MVC   HEAD11+8(15),ROWTABLE      PRINTLINE NAME                        
         STC   R6,INDENT                  JUSTIFICATION FOR HEADLINES           
         MVC   SRTLV,NOL                  FORCE FOR GETLEN                      
         BAS   RE,GETLEN            LEN-> R1,SRTLV-> R3,LEN TABLE-> R5          
         BCTR  R5,R0                BUMP BACKWARDS IN ACCT. LEN TABLE           
         SR    R4,R4                                                            
         ICM   R4,1,0(R5)           PUT PREV. ACCT. LEN IN R4                   
         SR    R1,R4                LEN OF ACCT CODE FOR THAT LEVEL             
         SH    R1,=H'03'            ADJUST FOR U/L                              
         LA    R4,HEAD11                                                        
         LA    R4,51(R1,R4)         DISPLACEMNET TO PRINT 'UNITS'               
         MVC   0(5,R4),=C'UNITS'                                                
         LA    R4,11(R4)                                                        
         LA    R2,HEAD10                                                        
         LA    R2,47(R1,R2)         DISPLACEMENT TO PRINT ROWNAME               
         LA    R5,ROWTABLE+15                                                   
         SPACE 1                                                                
LOADPRTY EQU   *                                                                
         LA    R2,15(R2)                   SETUP COLUMN NAMES                   
         CH    R3,=H'01'                   LAST COLUMN ?                        
         BE    LOADPRTX                                                         
         MVC   0(10,R2),=C'PERCENT OF'                                          
         MVC   0(15,R4),0(R5)              MOVE ROWNAME TO HEAD11               
         LA    R4,15(R4)                                                        
         LA    R5,15(R5)                                                        
         SPACE 1                                                                
LOADPRTX EQU   *                                                                
         BCT   R3,LOADPRTY                                                      
         SPACE 1                                                                
         AH    R1,=H'46'            DISPLACEMENT FOR CENTER LINE                
         STC   R1,CENTBX                                                        
         SPACE 1                                                                
         MVC   0(15,R2),=C'PERCENT OF THIS'                                     
         GOTO1 SQUASHER,DMCB,ROWLTNAM,24                                        
         MVC   0(24,R4),ROWLTNAM                                                
         SPACE 1                                                                
         LA    R4,15                                                            
         C     R4,DMCB+4            LEN. OF LAST COL. NAME                      
         BH    *+8                                                              
         L     R4,DMCB+4                                                        
         LA    R2,2(R4,R2)          MIN. 17 PLUS LAST THING ON PRTLINE          
         LA    R3,HEAD10            FOR DISP. OF RIGHT SIDE OF BOX              
         SR    R2,R3                THE DIFFERENCE OF BEGINING AND              
         STC   R2,RSIDEBX           THE RIGHT SIDE OF BOX                       
         SPACE 1                                                                
         LA    R6,HEAD3+105                                                     
         MVC   2(L'SCHMCODE,R6),SCHMCODE                                        
         MVC   L'SCHMCODE+3(L'SCHNAME,R6),SCHNAME                               
         SPACE 1                                                                
         B     XIT                                                              
         EJECT                                                                  
*        PUT PERCENTS INTO COLUMNS FOR PRINTING                                 
         SPACE 2                                                                
PRINTPER NTR1                                                                   
         ST    R8,DSECADDR                                                      
         ZIC   R2,NOL                                                           
         LA    R4,TOTTABLE                                                      
         LA    R5,LV1PER                                                        
         LR    R3,R2                                                            
         MH    R3,=H'15'                                                        
         LA    R5,0(R3,R5)          STARTING ADDR. ON P LINE FOR NUMS.          
         CLI   STAT,C'Y'                                                        
         BE    PRTPER05                                                         
         EDIT  (P6,SRTLVTOT),(10,UNITS),4                                       
         B     PRTPER06                                                         
         SPACE 1                                                                
PRTPER05 EQU   *                                                                
         EDIT  (P6,SRTLVTOT),(10,UNITS),2                                       
         SPACE 1                                                                
PRTPER06 EQU   *                                                                
         GOTO1 PROLLER,DMCB,3,TOTLINES,SRTLVTOT,1,1                             
         ZIC   R3,NOL                                                           
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
PRTPER10 EQU   *                                                                
         SH    R5,=H'15'                   BUMP BACKWARDS IN PRTLINE            
         ZAP   TEMPPACK,SRTLVTOT                                                
         MP    TEMPPACK,=P'1000000'                                             
         CP    TEMPPACK,=P'0'      AVOID DIVISION BY ZERO ERROR                 
         BE    PRTPER12                                                         
         OC    0(6,R4),0(R4)                                                    
         BZ    PRTPER12                                                         
         DP    TEMPPACK,0(6,R4)                                                 
         ZAP   ANSWER,TEMPPACK(6)                                               
         GOTO1 PROLLER,DMCB,3,TOTLINES,ANSWER,1,(R3)                            
         BCTR  R3,R0                                                            
         SRP   ANSWER,64-2,5                                                    
         EDIT  (P6,ANSWER),(10,(R5)),2                                          
         SPACE 1                                                                
PRTPER12 LA    R4,6(R4)              BUMP FORWARDS IN TOTAL TABLE               
         BCT   R2,PRTPER10                                                      
         GOTO1 PROLLER,DMCB,4,TOTLINES,1,2                                      
         GOTO1 PROLLER,DMCB,2,TOTLINES,1                                        
         GOTO1 ACREPORT                                                         
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         B     XIT                                                              
         EJECT                                                                  
*        PRINT TOTAL LINES FOR REPORT                                           
         SPACE 1                                                                
PRINTTOT NTR1                                                                   
         CLI   FTIME,C'Y'           NOT FIRST TIME THROUGH                      
         BE    PRTTOT99                                                         
         MVC   TPRTLINE,SPACES                                                  
         XC    MID1,TPRTLINE                                                    
         XC    TPRTLINE,MID1                                                    
         XC    MID1,TPRTLINE                                                    
         MVC   FHTEMP,FORCEHED                                                  
         MVC   FMTEMP,FORCEMID                                                  
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         L     R3,DMCB               NUMBER OF TOTALS TO PRINT                  
         L     R2,DMCB+4             START AT LINE                              
         GOTO1 ACREPORT              PRINT A BLANK LINE                         
PRTTOT10 EQU   *                                                                
         BAS   RE,TOTNAME                                                       
         LA    R4,1(R2)                                                         
         GOTO1 PROLLER,DMCB,4,TOTLINES,(R2),(R4)                                
         GOTO1 PROLLER,DMCB,1,TOTLINES,(R2)                                     
         L     R4,DMCB                                                          
         ZAP   TPACK,0(6,R4)                                                    
         L     R8,DSECADDR           STARTING ADDR. ON P TO PRINT TOTS.         
         LA    R5,LV1PER                                                        
         ZIC   R6,NOL                                                           
         CLI   STAT,C'Y'                                                        
         BE    PRTTOT15                                                         
         EDIT  (P6,TPACK),(10,UNITS),4                                          
         B     PRTTOT20                                                         
         SPACE 1                                                                
PRTTOT15 EQU   *                                                                
         EDIT  (P6,TPACK),(10,UNITS),2                                          
         SPACE 1                                                                
PRTTOT20 EQU   *                                                                
         LA    R4,6(R4)                                                         
         ZAP   TPACK,0(6,R4)                                                    
         SRP   TPACK,64-2,5                                                     
         EDIT  (P6,TPACK),(10,(R5)),2                                           
         LA    R5,15(R5)                                                        
         BCT   R6,PRTTOT20                                                      
         GOTO1 PROLLER,DMCB,2,TOTLINES,(R2)                                     
         GOTO1 ACREPORT                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,PRTTOT10                                                      
         CLC   LINE,MAXLINES                                                    
         BE    PRTTOT22                                                         
         GOTO1 ACREPORT                PRINT BLANK LINE                         
PRTTOT22 EQU   *                                                                
         XC    MID1,TPRTLINE                                                    
         XC    TPRTLINE,MID1                                                    
         XC    MID1,TPRTLINE                                                    
         MVC   FORCEHED,FHTEMP                                                  
         MVC   FORCEMID,FMTEMP                                                  
         MVI   FTIME,C'Y'                                                       
PRTTOT99 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
TOTNAME  NTR1                                                                   
         MVC   P(41),SPACES                                                     
         MVC   P+4(10),=C'TOTAL FOR '                                           
         SPACE 1                                                                
         ZIC   R3,NOL                                                           
         BCTR  R2,R0                                                            
         CR    R2,R3                                                            
         BNE   TOTNAME5                                                         
         MVC   P+14(6),=C'REPORT'                                               
         B     TOTNAME9                                                         
         SPACE 1                                                                
TOTNAME5 EQU   *                                                                
         MH    R2,=H'15'                                                        
         LA    R4,ROWTABLE                                                      
         LA    R4,0(R2,R4)                                                      
         MVC   P+14(15),0(R4)             MOVE IN ROW TITLE                     
TOTNAME9 EQU   *                                                                
         B     XIT                                                              
         EJECT                                                                  
*        REVERSE NAMES IN TABLE FOR EASIER BUMPING                              
         SPACE 1                                                                
REVERSE  NTR1                                                                   
         ZIC   R3,NOL           SWITCH ROWNAMES LEVELS GOING                    
         BCTR  R3,R0            FROM HIGH THROUGH LOW ORDER TO                  
         MH    R3,=H'15'        LOW THROUGH HIGH ORDER                          
         LA    R5,ROWTABLE                                                      
         LA    R6,0(R3,R5)                                                      
REVERSE5 CR    R6,R5                                                            
         BNH   REVERSE9              COMPARE ADDR. ENTRY IN TABLE               
         XC    0(15,R5),0(R6)        TO SEE IF WE SWAPED ALL ENTRIES            
         XC    0(15,R6),0(R5)        SWAP                                       
         XC    0(15,R5),0(R6)        SWAP                                       
         LA    R5,15(R5)             BUMP UP AND                                
         SH    R6,=H'15'             BUMP DOWN TILL ADDR. CROSS OVER            
         B     REVERSE5                                                         
REVERSE9 XIT                                                                    
         EJECT                                                                  
*        DATA CONSTANTS, LTORG                                                  
         SPACE 1                                                                
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
PRNTBL   DC    V(PRNTBL)                                                        
SORTER   DC    V(SORTER)                                                        
SQUASHER DC    V(SQUASHER)                                                      
         SPACE 1                                                                
         GETEL R2,DATADISP,ELCODE                                               
         LTORG                                                                  
         EJECT                                                                  
*        BOX HOOK FOR REPORT                                                    
         SPACE 1                                                                
BXHOOK   DS    0D                                                               
         NMOD1 0,*BXHK*                                                         
         L     R4,ADBXAREA                                                      
         LTR   R4,R4                                                            
         BZ    BXHOOK99                                                         
         LA    RC,SPACEND                                                       
         USING BOXD,R4                                                          
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXCOLS,SPACES                                                   
         LA    R2,BOXROWS                                                       
         MVI   8(R2),C'T'                                                       
         MVI   11(R2),C'M'                                                      
         MVI   59(R2),C'B'                                                      
         LA    R2,BOXCOLS                                                       
         MVI   2(R2),C'L'                                                       
         ZIC   R3,CENTBX                                                        
         LA    R3,0(R3,R2)                                                      
         MVI   0(R3),C'C'                                                       
         ZIC   R3,RSIDEBX                                                       
         LA    R3,0(R3,R2)                                                      
         MVI   0(R3),C'R'                                                       
         SPACE 1                                                                
         MVI   BOXWT,1                                                          
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,X'00'                                                    
BXHOOK99 XMOD1 1                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE DDBOXEQUS                                                      
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
         EJECT                                                                  
*              DSECT FOR PRINT LINE                                             
         SPACE 2                                                                
PRINTD   DSECT                                                                  
UNITS    DS    CL10                                                             
         DS    CL5              SPACES                                          
LV1PER   DS    CL10                                                             
         DS    CL5              SPACES                                          
LV2PER   DS    CL10                                                             
         DS    CL5              SPACES                                          
LV3PER   DS    CL10                                                             
         DS    CL5              SPACES                                          
LV4PER   DS    CL10                                                             
         EJECT                                                                  
*              DSECT FOR REGION TABLE ENTRY                                     
         SPACE 2                                                                
SORTD    DSECT                                                                  
SRTCNTRA DS    CL15             CONTRA ACCOUNT (JOB)                            
SRTACCT  DS    CL15             REAL ACCOUNT                                    
SRTKEYQ  EQU   *-SORTD                                                          
         ORG   SORTD                                                            
SRTKEY   DS    CL(SRTKEYQ)      COVERS KEY                                      
SRTNAML  DS    XL1                                                              
SRTLVNAM DS    CL36             LEVEL NAME                                      
SRTCAL   DS    XL1                                                              
SRTCANAM DS    CL36             CONTRA NAME (JOB)                               
SRTLVTOT DS    PL6              LEVEL TOTAL                                     
SRTLV    DS    XL1              LEVEL NUMBER                                    
SRTLENQ  EQU   *-SORTD                                                          
         EJECT                                                                  
*              A DSECT FOR PROGRAM                                              
         SPACE 2                                                                
AC4502D  DSECT                                                                  
ELCODE   DS    XL1                                                              
EIJ      DS    XL1              ERROR IN JOB FLAG                               
NOL      DS    XL1              NUMBER OF LEVELS                                
INDENT   DS    XL1              INDENT LENGTH FOR HEADINGS                      
STAT     DS    XL1              IS UNITS IN PERCENTS OR JUST UNITS              
RSIDEBX  DS    XL1              RIGHT SIDE LINE OF BOX                          
CENTBX   DS    XL1              CENTER LINE OF BOX                              
DUMPON   DS    CL1              DUMP EACH SORT RECORD                           
FTIME    DS    CL1              FIRST TIME THOUGH A PIECE OF CODE               
FHTEMP   DS    CL1              FORCE HEAD TEMP                                 
FMTEMP   DS    CL1              FORCE MID TEMP                                  
DSECADDR DS    A                STARTING ADDR. FOR THE PRINT DSECT              
RESAVE   DS    A                SAVE REG. RE                                    
PREVACCT DS    CL15             PREVIEOUS ACCOUNT                               
SCHMCODE DS    CL2              TWO LETER SCHEME CODE (WORK CODE)               
SCHNAME  DS    CL15             NAME OF WORK CODE                               
SORTREC  DS    CL(SRTLENQ)                                                      
PRTADDR  DS    4A               ADDR. OF HEAD, MID, PRINT LINES                 
ROWTABLE DS    4CL15            NAMES OF LEVELS FROM 1 UP TO 4                  
ROWLTNAM DS    CL24             LAST COLUMN NAME ON REPORT                      
ROWLEN   DS    5XL1             ACCOUNT LENGTH, LEVELS 4 DOWN TO 1              
TPACK    DS    PL6              TEMPORARY PACKED                                
TEMPPACK DS    PL12             TEMPORARY PACKED                                
ANSWER   DS    PL6              ANSWER TO BE PRINTED                            
TOTTABLE DS    0PL6             TABLE OF CURRENT TOTALS FOR EACH LEVEL          
JOBTOTAL DS    PL6              CURRENT JOB TOTAL                               
LVTOTALS DS    4PL6             POSSIBLE LEVELS 4 DOWN TO 1                     
TOTLINES DS    CL188            MAX AMOUNT NEED FOR TOTAL LINES                 
TPRTLINE DS    CL132            TEMPORARY PRINT LINE                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREP4502 05/01/02'                                      
         END                                                                    
