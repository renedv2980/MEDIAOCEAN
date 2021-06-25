*          DATA SET DELDNECK   AT LEVEL 003 AS OF 05/01/02                      
*PHASE DELDNECK,+0                                                              
*INCLUDE HEXOUT                                                                 
*INCLUDE XSORT                                                                  
         TITLE 'DELDNECK - LOAD/DUMP EXTERN TO CHECK NTI RECORDS'               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
         ST    R5,RELO             SAVE RELOCATION FACTOR                       
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM            KEEP RECORD EXIT                             
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         EJECT                                                                  
*****************************************************************               
* INITIALIZE LOGIC                                              *               
*****************************************************************               
         SPACE 1                                                                
DMXINIT  DS    0H                                                               
         B     DMXIT                                                            
         EJECT                                                                  
**************************************************************                  
* PROCESS RECORD LOGIC                                       *                  
* MODULE LOOKS AT OUTPUT TAPE FROM NTI CONVERSION            *                  
* MAINTAIN OF NETWORK/WEEK/PROGRAM NUMBER FOR TRACKING       *                  
* ANY DISCREPANCIES BETWEEN 'P' AND 'Q' RECORDS AND 'N'      *                  
* PASSIVE POINTERS                                           *                  
**************************************************************                  
         SPACE 1                                                                
DMXREC   DS    0H                                                               
         L     R3,AREC             POINT TO RECORD                              
         L     R1,RECIN                                                         
         LA    R1,1(R1)                                                         
         ST    R1,RECIN                                                         
         CLI   2(R3),C'N'          TEST FOR NETWORK                             
         BE    DMXREC1                                                          
         L     R1,BADREC                                                        
         LA    R1,1(R1)                                                         
         ST    R1,BADREC                                                        
         B     DMXKEEP                                                          
         SPACE 1                                                                
DMXREC1  DS    0H                                                               
         LA    R6,TABLE            POINT TO TABLE                               
         USING NETENTD,R6                                                       
         CLI   0(R3),C'P'          TEST FOR 'P' RECORD                          
         BE    DMXREC2                                                          
         CLI   0(R3),C'Q'          TEST FOR 'Q' RECORD                          
         BE    DMXREC3                                                          
         CLI   0(R3),C'N'          TEST FOR 'N' RECORD                          
         BNE   DMXKEEP                                                          
         SPACE 2                                                                
* 'N' - PASSIVE POINTER PROCESSING                                              
*                                                                               
         USING PNKEY,R3                                                         
         MVC   WORK(5),PNSTAT      SET SEARCH VALUES                            
         MVC   WORK+5(2),PNBOOK                                                 
         MVC   WORK+7(2),PNPNUM                                                 
         BAS   RE,GETENTRY         FIND TABLE ENTRY                             
         CLI   NETNLOSQ,0                                                       
         BE    *+14                                                             
         CLC   NETNLOSQ,PNSTIM     TABLE LOW SQH VS RECORD                      
         BL    *+10                                                             
         MVC   NETNLOSQ,PNSTIM                                                  
         CLI   NETNHISQ,0                                                       
         BE    *+14                                                             
         CLC   NETNHISQ,PNSTIM     TABLE HIGH SQH VS RECORD                     
         BH    *+10                TABLE IS HIGHER-NO UPDATE                    
         MVC   NETNHISQ,PNSTIM                                                  
         MVC   NETNDURM,PNACTDUR   SET DURATION (MINUTES)                       
         ZIC   R1,NETNCNT                                                       
         LA    R1,1(R1)                                                         
         STC   R1,NETNCNT                                                       
         L     R1,NRECS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,NRECS                                                         
         B     DMXKEEP                                                          
         SPACE 2                                                                
* 'P' - TIME PERIOD PROCESSING                                                  
*                                                                               
DMXREC2  DS    0H                                                               
         USING PRKEY,R3                                                         
         CLC   PRSTAT(3),=C'HUT'   TEST FOR HUT RECORD                          
         BE    DMXREC2H                                                         
         MVC   WORK(5),PRSTAT      SET SEARCH VALUES                            
         MVC   WORK+5(2),PRBOOK                                                 
         MVI   ELCODE,X'20'                                                     
         LR    R5,R3               RECORD START                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PHELEM,R5                                                        
         MVC   WORK+7(2),PHPNUM    PROGRAM NUMBER                               
         BAS   RE,GETENTRY                                                      
         CLI   NETPLOSQ,0          TEST FOR FIRST RECD                          
         BE    *+14                                                             
         CLC   NETPLOSQ,PRSTIM                                                  
         BL    *+16                                                             
         MVC   NETPLOSQ,PRSTIM                                                  
         MVC   NETPLODU,PHDUR      MOVE IN DURATION                             
         CLI   NETPHISQ,0          TEST FOR FIRST RECD                          
         BE    *+14                                                             
         CLC   NETPHISQ,PRSTIM                                                  
         BH    *+16                                                             
         MVC   NETPHISQ,PRSTIM                                                  
         MVC   NETPHIDU,PHDUR      MOVE IN DURATION                             
         B     DMXREC2X                                                         
         SPACE 1                                                                
DMXREC2H DS    0H                  HUT RECORDS                                  
         L     R1,HUTRECS                                                       
         LA    R1,1(R1)                                                         
         ST    R1,HUTRECS                                                       
         SPACE 1                                                                
DMXREC2X DS    0H                                                               
         L     R1,PRECS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,PRECS                                                         
         B     DMXKEEP                                                          
         SPACE 2                                                                
* 'Q' - PROGRAM RECORD PROCESSING                                               
*                                                                               
DMXREC3  DS    0H                                                               
         USING PMKEY,R3                                                         
         CLC   PMSTAT(3),=C'HUT'                                                
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(5),PMSTAT                                                   
         MVC   WORK+5(2),PMBOOK                                                 
         MVC   WORK+7(2),PMPNUM                                                 
         BAS   RE,GETENTRY                                                      
         OC    NETQSQH(4),NETQSQH  TEST FOR DOUBLE RECS                         
         BZ    *+6                                                              
         DC    H'0'                                                             
         MVI   ELCODE,X'20'                                                     
         LR    R5,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PHELEM,R5                                                        
         MVC   NETQDURQ,PHDUR      DURATION-QUARTER HOUR                        
         MVI   ELCODE,X'22'                                                     
         LR    R5,R3                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING NTELEM,R5                                                        
         MVC   NETQSQH,NTSQH                                                    
         MVC   NETQEQH,NTEQH                                                    
         MVC   NETQDURM,NTDUR                                                   
         L     R1,QRECS                                                         
         LA    R1,1(R1)                                                         
         ST    R1,QRECS                                                         
         B     DMXKEEP                                                          
         EJECT                                                                  
***************************************************************                 
* END-OF-FILE LOGIC                                           *                 
* SHOW TOTALS FOR RECORDS IN AND BAD RECORDS.  THEN PRINT     *                 
* THE TABLE ENTRIES OUT.                                      *                 
***************************************************************                 
         SPACE 1                                                                
DMXEOF   DS    0H                                                               
         XC    LASTKEY,LASTKEY     CLEAR LAST KEY CONTROL                       
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         GOTO1 VPRINTER                                                         
         MVC   P+10(26),=C'END OF FILE SUMMARY TOTALS'                          
         GOTO1 VPRINTER                                                         
         MVI   P+10,C'-'                                                        
         MVC   P+11(25),P+10                                                    
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
         LA    R4,BUCKETS          PRINT THE BUCKETS AND DESCRIPTIONS           
         LA    R3,BUCKTAB          IN A LOOP                                    
DMXEOFA  MVC   P+10(20),4(R3)      DESCRIPTION                                  
         MVI   P+30,C'='                                                        
         L     R2,0(R3)            BUCKET VALUE                                 
         EDIT  (R2),(10,P+32)                                                   
         GOTO1 VPRINTER                                                         
         LA    R3,L'BUCKTAB(R3)    POINT TO NEXT BUCKET                         
         BCT   R4,DMXEOFA                                                       
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
* SORT TABLE - THEN PRINT HEADLINES                                             
*                                                                               
DMXEOF1  DS    0H                                                               
         L     R0,TABENT           NUMBER OF TABLE ENTRIES FOR SORTING          
         GOTO1 XSORT,DMCB,TABLE,(R0),L'NETENT,L'NETKEY,0,RR=RELO                
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         LA    R6,TABLE            R6 POINTS TO TABLE                           
         L     R7,TABENT           COUNTER                                      
*                                                                               
         MVC   P+10(8),=C'NET/BOOK'                                             
         MVC   P+22(4),=C'PNUM'                                                 
         MVC   P+30(15),=C'PROGRAM RECORDS'                                     
         MVC   P+52(19),=C'TIME PERIOD RECORDS'                                 
         MVC   P+75(15),=C'PASSIVE RECORDS'                                     
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         MVC   P+10(8),=8C'-'                                                   
         MVC   P+22(4),=4C'-'                                                   
         MVC   P+30(15),=C'SQH EQH QHD MID'                                     
         MVC   P+52(11),=C'LO DU HI DU'                                         
         MVC   P+75(13),=C'LO HI DUR CNT'                                       
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         SPACE 1                                                                
* PRINT REPORT                                                                  
*                                                                               
DMXEOF2  DS    0H                                                               
         USING NETENTD,R6                                                       
         OC    NETENT,NETENT       TEST FOR END OF TABLE                        
         BZ    DMXEOFN                                                          
*                                                                               
         MVC   P+10(5),NETSTA                                                   
         GOTO1 HEXOUT,DMCB,NETWEEK,P+16,2,0,RR=RELO                             
         SR    R4,R4                                                            
         ICM   R4,3,NETPNUM                                                     
         EDIT  (R4),(4,P+22),FILL=0                                             
         SPACE 1                                                                
DMXEOF2A DS    0H                  PROGRAM DATA                                 
         LA    R2,NETQSQH                                                       
         LA    R4,P+30             POINT R4 AT OUTPUT                           
         LA    R5,3                R5 IS COUNTER                                
         SPACE 1                                                                
DMXEOF3  DS    0H                                                               
         ZIC   R3,0(R2)                                                         
         EDIT  (R3),(2,(R4))                                                    
         LA    R2,1(R2)            NEXT FIELD IN TABLE ENTRY                    
         LA    R4,4(R4)                                                         
         BCT   R5,DMXEOF3                                                       
         ZIC   R3,NETQDURM         DURATION IN MINUTES                          
         EDIT  (R3),(3,(R4))                                                    
         SPACE 1                                                                
DMXEOF4  DS    0H                  TIME PERIOD DATA                             
         LA    R2,NETPLOSQ         POINT R2 AT DATA                             
         LA    R4,P+52             R4 POINTS TO OUTPUT                          
         LA    R5,4                COUNTER                                      
         SPACE 1                                                                
DMXEOF5  DS    0H                                                               
         ZIC   R3,0(R2)                                                         
         EDIT  (R3),(2,(R4))                                                    
         LA    R2,1(R2)            NEXT DATA FIELD                              
         LA    R4,3(R4)            NEXT OUTPUT POSITION                         
         BCT   R5,DMXEOF5                                                       
         SPACE 1                                                                
DMXEOF6  DS    0H                  PASSIVE RECORD DATA                          
         LA    R2,NETNLOSQ         R2 POINTS TO DATA                            
         LA    R4,P+75                                                          
         LA    R5,2                                                             
         SPACE 1                                                                
DMXEOF7  DS    0H                                                               
         ZIC   R3,0(R2)                                                         
         EDIT  (R3),(2,(R4))                                                    
         LA    R2,1(R2)                                                         
         LA    R4,3(R4)                                                         
         BCT   R5,DMXEOF7                                                       
         IC    R3,NETNDURM         DURATION IN MINUTES                          
         EDIT  (R3),(3,(R4))                                                    
         LA    R4,4(R4)            BUMP OUTPUT POINTER                          
         IC    R3,NETNCNT          POINTER COUNT                                
         EDIT  (R3),(2,(R4))                                                    
         SPACE 1                                                                
* FLAG UNMATCHED OR DISCREPANT DATA                                             
*                                                                               
DMXEOF8  DS    0H                                                               
         OC    NETQSQH(4),NETQSQH  TEST FOR PROGRAM DATA                        
         BZ    DMXEOF9             NONE-FLAG ENTRY                              
         CLI   NETSTA+3,C'S'       TEST FOR SEASON-TO-DATE                      
         BE    *+14                YES-IT DOES NOT HAVE TIME PER RECS           
         OC    NETPLOSQ(4),NETPLOSQ TEST FOR TIME PERIOD DATA                   
         BZ    DMXEOF9                                                          
         OC    NETNLOSQ(4),NETNLOSQ    TEST FOR PASSIVE DATA                    
         BZ    DMXEOF9                                                          
         CLI   NETSTA+3,C'S'       TEST FOR SEASON-TO-DATE                      
         BE    *+14                YES-SKIP CHECK AGAINST TIME PERIOD           
         CLC   NETQSQH,NETPLOSQ    TEST PROGRAM SQH VS TIME PER SQH             
         BNE   DMXEOF10            FLAG IF NOT THE SAME                         
         CLC   NETQSQH,NETNLOSQ    PROGRAM VS PASSIVE RECD                      
         BNE   DMXEOF10                                                         
         B     DMXEOF12            RECORD IS OK                                 
         SPACE 1                                                                
DMXEOF9  DS    0H                                                               
         MVC   P+95(12),=C'MISSING DATA'                                        
         B     DMXEOF11                                                         
         SPACE 1                                                                
DMXEOF10 DS    0H                                                               
         MVC   P+95(15),=C'DISCREPANT DATA'                                     
         SPACE 1                                                                
DMXEOF11 DS    0H                                                               
         L     R1,FLAGGED                                                       
         LA    R1,1(R1)                                                         
         ST    R1,FLAGGED                                                       
         SPACE 1                                                                
* PRINT LINE - THEN RESET TO PRINT NEXT ENTRY                                   
*                                                                               
DMXEOF12 DS    0H                                                               
         OC    LASTKEY,LASTKEY     TEST FOR FIRST TIME                          
         BZ    *+20                                                             
         CLC   LASTKEY,NETKEY      TEST FOR CHANGE IN KEY                       
         BE    *+10                                                             
         ZAP   LINE,=PL2'99'       FORCE PAGE BREAK                             
         MVC   LASTKEY,NETKEY      UPDATE LAST KEY                              
         GOTO1 VPRINTER                                                         
         GOTO1 VPRINTER                                                         
         LA    R6,L'NETENT(R6)     BUMP TO NEXT TABLE ENTRY                     
         BCT   R7,DMXEOF2                                                       
         SPACE 1                                                                
* END OF TABLE - PRINT TOTAL RECORDS FLAGGED                                    
*                                                                               
DMXEOFN  DS    0H                                                               
         MVC   P+10(15),=C'RECORDS FLAGGED'                                     
         MVI   P+30,C'='                                                        
         L     R4,FLAGGED                                                       
         EDIT  (R4),(6,P+32)                                                    
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
         EJECT                                                                  
* SUB-ROUTINE TO FIND TABLE ENTRY (R6 POINTS TO TABLE)                          
*                                                                               
GETENTRY DS    0H                                                               
         LA    R0,MAXENT           COUNT OF MAXIMUM ENTRIES                     
         SPACE 1                                                                
GETENTR1 DS    0H                                                               
         OC    NETENT,NETENT       TEST FOR ZERO ENTRY                          
         BZ    GETENTR2            ADD NEW ENTRY                                
         CLC   NETKEY,WORK         ENTRY KEY VERSUS SEARCH VALUES               
         BER   RE                                                               
         LA    R6,L'NETENT(R6)                                                  
         BCT   R0,GETENTR1                                                      
         DC    H'0'                BLOW UP IF NOT FOUND                         
         SPACE 1                                                                
GETENTR2 DS    0H                                                               
         MVC   NETKEY,WORK         INITIALIZE TABLE ENTRY KEY                   
         L     R1,TABENT           UPDATE TABLE ENTRY COUNT                     
         LA    R1,1(R1)                                                         
         ST    R1,TABENT                                                        
         BR    RE                                                               
         EJECT                                                                  
         GETEL R5,23,ELCODE                                                     
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         SPACE 2                                                                
* BUCKET TABLE                                                                  
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL24                                                            
RECIN    DC    F'0',CL20'RECORDS IN'                                            
BADREC   DC    F'0',CL20'NON-NETWORK RECORDS'                                   
NRECS    DC    F'0',CL20'PASSIVE RECORDS'                                       
PRECS    DC    F'0',CL20'TIME PERIOD RECORDS'                                   
QRECS    DC    F'0',CL20'PROGRAM RECORDS'                                       
HUTRECS  DC    F'0',CL20'HUT RECORDS'                                           
BUCKETS  EQU   (*-BUCKTAB)/L'BUCKTAB                                            
         SPACE 2                                                                
* ROUTINE ADDRESSES                                                             
*                                                                               
HEXOUT   DC    V(HEXOUT)                                                        
XSORT    DC    V(XSORT)                                                         
         SPACE 2                                                                
* TABLE OF NTI PROGRAM DATA                                                     
*                                                                               
TABENT   DC    F'0'                NUMBER OF TABLE ENTRIES                      
FLAGGED  DC    F'0'                                                             
         SPACE 1                                                                
TABLE    DC    2000XL21'00'                                                     
MAXENT   EQU   (*-TABLE)/L'TABLE                                                
         SPACE 2                                                                
* WORKING STORAGE DSECT                                                         
*                                                                               
WORKD    DSECT                                                                  
RELO     DS    A                                                                
DUB      DS    D                                                                
DMCB     DS    6F                                                               
APARM    DS    A                                                                
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
HALF     DS    H                                                                
BYTE     DS    C                                                                
THREE    DS    CL3                                                              
FULL     DS    F                                                                
TODAY    DS    CL6                                                              
WORK     DS    CL64                                                             
ELCODE   DS    C                                                                
LASTKEY  DS    CL7                 STATION/BOOK                                 
WORKX    EQU   *                                                                
         SPACE 2                                                                
* TABLE ENTRY DSECT                                                             
*                                                                               
NETENTD  DSECT                                                                  
NETENT   DS    0CL21                                                            
NETKEY   DS    0CL9                                                             
NETSTA   DS    CL5                                                              
NETWEEK  DS    XL2                                                              
NETPNUM  DS    XL2                                                              
NETQSQH  DS    X                                                                
NETQEQH  DS    X                                                                
NETQDURQ DS    X                                                                
NETQDURM DS    X                                                                
NETPLOSQ DS    X                                                                
NETPLODU DS    X                   LOW SQH DURATION                             
NETPHISQ DS    X                   HIGH SQH DURATION                            
NETPHIDU DS    X                   HIGH SQH DURATION                            
NETNLOSQ DS    X                   LOW SQH                                      
NETNHISQ DS    X                   HIGH SQH                                     
NETNDURM DS    X                   DURATION-MINUTES                             
NETNCNT  DS    X                                                                
         EJECT                                                                  
* DMLDDEFN                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMLDDEFN                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DDDPRINT                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
         SPACE 2                                                                
* DEDEMFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003DELDNECK  05/01/02'                                      
         END                                                                    
