*          DATA SET RERIS02S   AT LEVEL 039 AS OF 05/01/02                      
*PHASE T80D02A,+0                                                               
RIS2     TITLE 'RERIS02 - T80D02 - BUCKET CONTRACT DATA'                        
*                                                                               
*- RERIS02 -- PHASE T80D02 -- READ AND BUCKET CONTRACTS                         
***********************************************************************         
*  HISTORY OF CHANGES                                                 *         
***********************************************************************         
*  08/03/89 (PJS) ---  REMOVE PENNIES FROM CONTRACT AMOUNTS           *         
*                                                                     *         
*  02/21/92 (BU ) ---  OPTIONAL BOOKED VS BEST $ FIGURES.  CLEAN UP   *         
*                      LISTING.  DROP COMMENTED OUT CODE.             *         
*                                                                     *         
*  12/03/92 (BU ) ---  IMPLEMENT COMBO STATION REPORTING              *         
*                                                                     *         
*  05/21/93 (BU ) ---  SET/RESET 'ACMBOSTA' TO SAVE DISPLACEMENT      *         
*                                                                     *         
* OCT24/97 (BU ) --- RERISWRKB --> RERISWRKC                          *         
*                    RGENEROL INCLUDED EXPLICITLY                     *         
*                                                                     *         
* APR07/98 (BU ) --- IF ALT CAL REQUEST, IGNORE BIT-11 TESTING        *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                      **  END TOMBSTONE  **                          *         
***********************************************************************         
T80D02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0D02*                                                        
         L     RC,0(R1)                                                         
         USING T80DFFD,RA                                                       
         USING GENOLD,RC                                                        
         LA    RE,RECORD                                                        
         ST    RE,AIOAREA                                                       
         XC    LINK,LINK           FIRST TIME - SET TO ZERO                     
         SPACE 1                                                                
*&&DO                                                                           
         GETIME TU                                                              
         ST    R1,SAVETIME         TU IS 1/300 SECOND                           
*&&                                                                             
         SPACE 1                                                                
*&&OS                                                                           
         TIME  TU                                                               
         ST    R0,SAVETIME         TU IS 26 MICROSECONDS                        
*&&                                                                             
         EJECT                                                                  
READREC  GOTO1 VREAD,DMCB,(RC),LINK                                             
         CLI   DMCB+1,X'FF'        OVER MAX IO'S                                
         BE    MAXOUT                                                           
         CLI   DMCB,X'FF'          ANY MORE RECORDS?                            
         BNE   REAR0010            YES - CONTINUE                               
         CLI   CMBOREP,C'Y'        NO  - IS THIS A COMBO STATION RUN?           
         BNE   EXXMOD              NO  - FINISHED                               
         L     RF,ACMBOSTA         YES - CHECK FOR NEXT STATION                 
         AR    RF,RA               RE-ADDRESS                                   
         LA    RF,5(RF)            A(NEXT STATION IN LIST)                      
         LR    RE,RF               DON'T SAVE ADDRESS                           
*                                     SAVE DISPLACEMENT                         
         SR    RE,RA                                                            
         ST    RE,ACMBOSTA         STORE A(NEW STATION) BACK                    
         CLI   0(RF),0             END OF LIST?                                 
         BE    EXXMOD              YES - FINISHED                               
         MVC   TBLSTA,0(RF)        LOAD IN NEXT STATION                         
         XC    LINK,LINK           SET 'FIRST TIME FLAG' AGAIN                  
         B     READREC             GO BACK AND READ AGAIN                       
REAR0010 EQU   *                                                                
         CLI   ERRAREA,0                                                        
         BNE   EXXMOD                                                           
*                                                                               
         MVC   LINK,4(R1)                                                       
                                                                                
         BAS   RE,FILTERS          CHK REC AGAINST NEW RIS FILTERS              
         BNE   READREC                                                          
*                                                                               
         XC    ACTLIST,ACTLIST                                                  
         XC    CONGRID(156),CONGRID                                             
         XC    INVGRID(156),INVGRID ACTUAL BUCKETS                              
         SPACE 2                                                                
*                                  CHECK TIME                                   
         TM    12(RA),X'C0'         TEST IF TERMINAL ON UNLIMITED ID            
         BNZ   GETELEM0                                                         
         CLI   1(RA),C'*'         TEST FOR DDS TERMINAL                         
         BE    GETELEM0                                                         
         SPACE 1                                                                
*&&DO                                                                           
         GETIME TU                                                              
         S     R1,SAVETIME                                                      
         C     R1,=F'13500'        ALLOW 450 SECONDS                            
         BL    GETELEM0                                                         
*&&                                                                             
*&&OS                                                                           
         TIME  TU                                                               
         S     R0,SAVETIME                                                      
         C     R0,MAXOSTIM         OS EQUIVALENT OF 450 SECS                    
         BL    GETELEM0                                                         
*&&                                                                             
         LA    R3,60                                                            
         LA    R2,RISOFFH                                                       
         B     ERROR                                                            
*                                  FIND PROPER ELEMENTS                         
MAXOUT   DS    0H                                                               
         LA    R2,RISMESSH                                                      
         MVC   8(40,R2),=C'*IO TIMEOUT - REDUCE REQUEST PARAMETERS*'            
         FOUT  (R2)                                                             
         MVI   ERRAREA,X'FF'       LET RERIS00 KNOW ERROR                       
         B     EXXMOD    THAT'S ALL                                             
         EJECT                                                                  
         LTORG                                                                  
GETELEM0 EQU   *                                                                
         LA    R5,RCONELEM         A(01 ELEMENT OF CONTRACT REC)                
GETELEM  CLI   0(R5),0             TEST END OF RECORD                           
         BE    ADDIN                                                            
*                                                                               
         CLC   0(1,R5),ESTBUCKT    CONTRACT BUCKET ELEMENT                      
         BE    TABCON              (ORDERED DOLLARS)                            
*                                                                               
         CLC   0(1,R5),INVBUCKT    INVOICE BUCKET ELEMENT                       
         BE    INVCON              (INVOICED DOLLARS)                           
*                                                                               
GETELEM2 SR    R6,R6               FIND NEXT ELEMENT:                           
         IC    R6,1(R5)            A(CURRENT) + L(CURRENT) = A(NEXT)            
         LA    R5,0(R5,R6)                                                      
         B     GETELEM                                                          
         SPACE 2                                                                
ADDIN    LA    R8,CONGRID          ADD INTO OVERALL BUCKETS                     
         LA    R4,GRID+2                                                        
         LA    R6,13                                                            
*                                                                               
         CLI   RISALTD,C'I'        INVOICE ONLY ?                               
         BNE   *+12                                                             
         BAS   RE,MERGRID3                                                      
         B     ADDIN004                                                         
                                                                                
**->     TM    SVPGPBIT,X'10'      ALTERNATE $ DISPLAY?                         
         CLI   RISALTD,C'E'        ESTIMATE $ ?                                 
         BNE   ADDIN002            YES                                          
         BAS   RE,MERGRID          MERGE ACT & EST GRIDS                        
         B     ADDIN004                                                         
*                                                                               
ADDIN002 EQU   *                                                                
         BAS   RE,MERGRID2         MERGE ACT & EST GRIDS                        
*                                  ACTUALS ONLY                                 
ADDIN004 EQU   *                                                                
*                                                                               
         BAS   RE,NOPENNY          STRIP OUT PENNIES                            
*                                                                               
ADDIN008 LM    R1,R3,0(R8)                                                      
         A     R1,0(R4)                                                         
         A     R2,4(R4)                                                         
         A     R3,8(R4)                                                         
         STM   R1,R3,0(R4)                                                      
         LA    R8,12(R8)                                                        
         LA    R4,14(R4)                                                        
         BCT   R6,ADDIN008                                                      
         B     READREC                                                          
         EJECT                                                                  
*                                                                               
*    'GRID' CONSISTS OF 14-BYTE ENTRIES                                         
*        BYTES   1 - 2   =   DATE (FROM REQUEST DATES)                          
*        BYTES   3 - 13  =   NOT USED IN THIS MODULE                            
*    'CONGRID' (BOOKED) AND 'INVGRID' (INVOICED) CONSIST OF 36                  
*      FULL-WORD ENTRIES, CORRESPONDING TO 12 MONTHS.  EACH MONTH               
*      HAS THREE FULL-WORD BUCKETS ASSIGNED TO IT:                              
*        WORD 1 (DISPLACE 0)= PRIOR $ W/IN ACTIV DATES/< AS AT DATES            
*        WORD 2 (DISPLACE 4)= PRIOR $                                           
*        WORD 3 (DISPLACE 8)= CURR  $ W/IN ACTIV DATES/< AS AT DATES            
*                                                                               
TABCON   LA    R4,GRID                                                          
         LA    R7,ACTLIST                                                       
         LA    R8,CONGRID                                                       
         USING RCONBKEL,R5                                                      
TABCON10 MVC   HALF,0(R4)          DECREMENT YEAR FOR PREVIOUS EST.             
         IC    RE,HALF             TAKE YEAR FROM YYMM                          
         BCTR  RE,0                DECREMENT YEAR                               
         STC   RE,HALF             REINSERT YEAR                                
*                                                                               
TABCON20 CLI   0(R4),0             TEST END OF GRID                             
         BE    GETELEM2                                                         
         CLC   HALF,RCONBKYR       TEST YEAR/MONTH OF SERVICE PREVIOUS          
         BE    TABCON30                                                         
         CLC   0(2,R4),RCONBKYR    TEST YEAR/MONTH OF SERVICE CURRENT           
         BE    TABCON60                                                         
*                                  LOOK AT NEXT MONTH IN GRID                   
         LA    R4,14(R4)           BUMP TO NEXT GRID ENTRY                      
         LA    R7,3(R7)            TRY TO APPLY ELEMENT TO                      
         LA    R8,12(R8)           EACH GRID ENTRY                              
         B     TABCON10                                                         
         SPACE 2                                                                
TABCON30 MVC   FULL,RCONBKAM       ACTUAL BUCKET/USE BEST ESTIMATE              
         L     R1,4(R8)            PRIOR BOOKED                                 
         A     R1,FULL                                                          
         ST    R1,4(R8)                                                         
*                                                                               
*                                  PRIOR BUCKET - TEST AS AT DATE               
         SPACE 1                                                                
TABCON40 CLC   RCONBKWK,START1     TEST OPTIONAL ACTIVITY START DATE            
         BL    GETELEM2                                                         
         CLC   RCONBKWK,END1       TEST OPTIONAL AS AT DATE                     
         BH    GETELEM2                                                         
*                                                                               
TABCON42 L     R1,0(R8)            PRIOR BOOKED W/IN ACTIVITY DATES             
         A     R1,FULL             OR BEFORE AS-AT DATE                         
         ST    R1,0(R8)                                                         
*                                                                               
         B     GETELEM2                                                         
         SPACE 2                                                                
*                                  CURRENT BUCKET - TEST AS AT DATE             
         SPACE 1                                                                
TABCON60 CLC   RCONBKWK,START2     TEST OPTIONAL ACTIVITY START DATE            
         BL    GETELEM2                                                         
         CLC   RCONBKWK,END2       TEST OPTIONAL AS AT DATE                     
         BH    GETELEM2                                                         
*                                                                               
TABCON62 MVC   FULL,RCONBKAM       CURRENT BOOKED W/IN ACTIVITY DATES           
         L     R1,8(R8)            OR BEFORE AS-AT DATE                         
         A     R1,FULL                                                          
         ST    R1,8(R8)                                                         
*                                                                               
         B     GETELEM2                                                         
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*- BUCKET INVOICE (ACTUAL) AMOUNTS                                              
*                                                                               
INVCON   LA    R4,GRID                                                          
         LA    R7,ACTLIST                                                       
         LA    R8,INVGRID                                                       
         USING RCONSTEL,R5                                                      
INVCON10 MVC   HALF,0(R4)          DECREMENT YEAR FOR PREV. ACTUAL              
         IC    RE,HALF                                                          
         BCTR  RE,0                                                             
         STC   RE,HALF                                                          
*                                                                               
INVCON20 CLI   0(R4),0             TEST END OF GRID                             
         BE    GETELEM2                                                         
         CLC   HALF,RCONSTYR       TEST YEAR/MONTH OF SERVICE                   
         BE    INVCON30                                                         
         CLC   0(2,R4),RCONSTYR                                                 
         BE    INVCON60                                                         
*                                                                               
         LA    R4,14(R4)           TEST NEXT MONTH FOR MATCH                    
         LA    R7,3(R7)            BUMP TO NEXT GRID ENTRY                      
         LA    R8,12(R8)           TRY TO APPLY ELEMENT TO                      
         B     INVCON10            EACH GRID ENTRY                              
         SPACE 2                                                                
INVCON30 EQU   *                                                                
         MVC   FULL,RCONSTAM                                                    
         L     R1,4(R8)                                                         
         A     R1,FULL                                                          
         ST    R1,4(R8)                                                         
*                                                                               
         MVI   1(R7),1             INVOICE AMOUNT FOUND                         
*                                                                               
INVCON40 CLC   RCONSTWK,START1     TEST ACTIVITY                                
         BL    GETELEM2                                                         
         CLC   RCONSTWK,END1                                                    
         BH    GETELEM2                                                         
         L     R1,0(R8)            ADD AMT INTO CONTRACT TOTALS                 
         A     R1,FULL             PRIOR INVOICED W/IN ACTIVITY DATES           
         ST    R1,0(R8)              OR BEFORE AS-AT DATE                       
         MVI   0(R7),1             SET INV. SWITCH ON                           
         B     GETELEM2                                                         
*                                                                               
INVCON60 CLC   RCONSTWK,START2                                                  
         BL    GETELEM2                                                         
         CLC   RCONSTWK,END2                                                    
         BH    GETELEM2                                                         
*                                                                               
         MVI   2(R7),1                                                          
         MVC   FULL,RCONSTAM                                                    
         L     R1,8(R8)            CURRENT INVOICED W/IN ACTIVITY DATES         
         A     R1,FULL               OR BEFORE AS-AT DATE                       
         ST    R1,8(R8)                                                         
*                                                                               
         B     GETELEM2                                                         
         DROP  R5                                                               
         SPACE 3                                                                
*&&OS                                                                           
         DS    0F                                                               
MAXOSTIM DC    AL4(450000000/26)                                                
*&&                                                                             
         EJECT                                                                  
* CHECK CONTRACT REC AGAINST NEW RIS FILTERS                                    
FILTERS  NTR1                                                                   
         LA    R2,REPIOTBL                                                      
         USING REPIOD,R2                                                        
         OC    RIPDEMO,RIPDEMO     ARE WE FILTERING ON DEMO ?                   
         BZ    FLT20                                                            
* - GET DEMO FROM BOP OR SAR ELEMENT AND SAVE IN WORK+50                        
         XC    WORK(3),WORK                                                     
         LA    R4,RCONELEM                                                      
PPC8DLP  CLI   0(R4),0                                                          
         BE    PPC8D00                                                          
         CLI   0(R4),X'12'         SAR ELEMENT                                  
         BE    PPC8DDD                                                          
         CLI   0(R4),X'10'         BOP ELEMENT                                  
         BE    PPC8DEE                                                          
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    PPC8D00                                                          
         AR    R4,R1                                                            
         B     PPC8DLP             GO TO LOOP                                   
         USING RSARCO,R4                                                        
PPC8DDD  MVC   WORK(3),RSARDEM    DEMO                                          
         NI    WORK,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR             
         B     PPC8D00                                                          
         USING RCONBPEL,R4                                                      
PPC8DEE  MVC   WORK(3),RCONBPDM+1                                               
         NI    WORK,X'FF'-X'40'           CLEAR MAIN DEMO INDICATOR             
         B     PPC8D00                                                          
         DROP R4                                                                
PPC8D00  EQU   *                                                                
         CLC   RIPDEMO,WORK                                                     
         BNE   FILTNO                                                           
*                                                                               
FLT20    OC    RIPCDATS,RIPCDATS         CREATION DATE                          
         BZ    FLT40                                                            
         GOTO1 VDATCON,DMCB,(3,RCONHDRD),(2,WORK)                               
         CLC   WORK(2),RIPCDATS                                                 
         BL    FILTNO                                                           
         CLC   WORK(2),RIPCDATE                                                 
         BH    FILTNO                                                           
         B     FILTYES                                                          
                                                                                
FLT40    EQU   *                                                                
                                                                                
FILTYES  SR    RE,RE                                                            
FILTNO   LTR   RE,RE                                                            
FLTX     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*- MERGRID -- MERGE ESTIMATE & ACTUAL AMOUNTS INTO CONGRID                      
*             ALL  BUCKETS                                                      
*                                                                               
MERGRID  NTR1                                                                   
*                                                                               
         BAS   RE,INVSTAT          CHECK INVOICE STATUS                         
*                                                                               
         LA    R1,CONGRID          EST BUCKETS                                  
         LA    R2,INVGRID          INVOICE BUCKETS                              
         LA    R3,ACTLIST          ACTUAL/ESTIMATE SWITCHES                     
         LA    R0,36               12 MONTHS / 3 BKTS PER MONTH                 
MG20     CLI   0(R3),0             ANY INVOICE AMOUNT?                          
         BE    MG40                NO.                                          
         MVC   0(4,R1),0(R2)       USE INVOICED INSTEAD OF EST                  
MG40     EQU   *                                                                
         LA    R1,4(R1)            NEXT EST                                     
         LA    R2,4(R2)            NEXT INVOICE                                 
         LA    R3,1(R3)            NEXT ACTLIST                                 
         BCT   R0,MG20                                                          
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
*- MERGRID2 -- MERGE ACTUAL AMOUNTS INTO CONGRID - SKIP ESTIMATE                
*        TO PRODUCE 'BOOKED'                                                    
*                                                                               
MERGRID2 NTR1                                                                   
*                                                                               
***->    BAS   RE,INVSTAT          CHECK INVOICE STATUS                         
                                                                                
         LA    R1,CONGRID+4        EST BUCKETS (PRIOR ACTUAL)                   
         LA    R2,INVGRID+4        INV BUCKETS (PRIOR ACTUAL)                   
         LA    R3,ACTLIST+1        ACTUAL SWITCHES                              
         LA    R0,12               12 MONTHS                                    
MGRID004 EQU   *                                                                
         CLI   0(R3),0             ANY INVOICE AMOUNT?                          
         BE    MGRID008            NO                                           
         MVC   0(4,R1),0(R2)       USE INVOICED INSTEAD OF EST                  
MGRID008 EQU   *                                                                
         LA    R1,12(R1)           NEXT EST                                     
         LA    R2,12(R2)           NEXT INVOICE                                 
         LA    R3,3(R3)            NEXT ACTLIST                                 
         BCT   R0,MGRID004                                                      
         XIT1                                                                   
         SPACE 2                                                                
*                                                                               
* INVOICE $ ONLY                                                                
MERGRID3 NTR1                                                                   
*                                                                               
         LA    R1,CONGRID          EST BUCKETS                                  
         LA    R2,INVGRID          INVOICE BUCKETS                              
         LA    R0,36               12 MONTHS / 3 BKTS PER MONTH                 
MG320    MVC   0(4,R1),0(R2)       USE INVOICED INSTEAD OF EST                  
         LA    R1,4(R1)            NEXT EST                                     
         LA    R2,4(R2)            NEXT INVOICE                                 
         LA    R3,1(R3)            NEXT ACTLIST                                 
         BCT   R0,MG320                                                         
         XIT1                                                                   
         EJECT                                                                  
******************************************************                          
* THIS ROUTINE CHECKS TO SEE IF REPPROF+11 SET TO C'Y'                          
* IF =Y, THEN IF NO INVOICE DOLLARS                                             
*        AND IF BUCKET IS BEFORE STATION CLOSE DATE                             
*        USE 0 INVOICE $ INSTEAD OF BOOKED FOR BEST $                           
*******************************************************                         
INVSTAT  NTR1                                                                   
         CLI   ESTBUCKT,X'53'      ALT CALENDAR BUCKETS USED?                   
         BE    INVX                YES - SKIP REPPROF+11 TESTING                
*                                                                               
         CLI   CFLAG,C'Y'          USE 0 INVOICE $ ?                            
         BNE   INVX                                                             
         CLC   RCONKSTA,CSTATION   DO WE HAVE STATION ALREADY ?                 
         BE    INV05                                                            
         BAS   RE,GETSTAT                                                       
INV05    LA    R4,GRID                                                          
         LA    R1,CONGRID                                                       
         LA    R2,INVGRID                                                       
         LA    R3,ACTLIST                                                       
         LA    R0,36               12 MONTHS/3 BKTS PER MONTH                   
INV10    MVC   HALF,0(R4)          DECREMENT YEAR FOR PRIOR CHECK               
                                                                                
         IC    RE,HALF             GET YR FROM YYMM                             
         BCTR  RE,0                DECREASE YEAR - 1                            
         STC   RE,HALF             STORE PRIOR BKT DATE                         
* ??PRIOR YEAR MUST BE PROPORTIONAL WITH CURRENT YEAR CLOSE DATES               
                                                                                
         CLC   CDATE,HALF       IS STATION CLOSE DATE < PRIOR BKT DAT?          
         BL    INV40               YES-SO JUMP TO NEXT BUCKET DATE              
* TEST FIRST PRIOR BUCKET                                                       
         OC    0(4,R2),0(R2)       ANY INVOICE $ ?                              
         BNZ   INV10A              YES                                          
         CLI   1(R3),1             ANY INVOICE FOUND AT ALL?                    
         BE    INV10A              YES                                          
         XC    0(4,R1),0(R1)       NO - CLEAR PRIOR ACTUAL $                    
INV10A   EQU   *                                                                
* TEST NEXT PRIOR BUCKET                                                        
         OC    4(4,R2),4(R2)       ANY INVOICE $ ?                              
         BNZ   INV10B              YES                                          
         CLI   4(R3),1             ANY INVOICE FOUND AT ALL?                    
         BE    INV10B              YES                                          
         XC    4(4,R1),4(R1)       NO - CLEAR PRIOR ACTUAL $                    
INV10B   EQU   *                                                                
*                                                                               
         CLC   CDATE,0(R4)       IS STATION CLOSE DATE < CURRENT DAT?           
         BL    INV40               YES-SO JUMP TO NEXT BUCKET DATE              
         OC    8(4,R2),8(R2)       NO - ARE THERE INVOICE $ ?                   
         BNZ   INV40                    NO                                      
         XC    8(4,R1),8(R1)            YES-CLEAR CURRENT ACTUAL $              
*                                                                               
INV40    LA    R4,14(R4)           BUMP GRID                                    
         LA    R1,12(R1)           BUMP BOOKED                                  
         LA    R2,12(R2)           BUMP INVOICED                                
         LA    R3,3(R3)            BUMP ACTLIST                                 
         CLI   0(R4),0             END OF GRID                                  
         BNE   INV10                                                            
*                                                                               
INVX     XIT1                                                                   
         EJECT                                                                  
**************************************************                              
* GET STATION RECORD AND SET CDATE AND CSTATION                                 
* FOR USE IN INVOICE 0$ ROUTINE                                                 
***************************************************                             
GETSTAT  NTR1                                                                   
         MVC   SAVEKEY,KEY    SAVE KEY                                          
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RSTAREC,R5                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,RCONKREP                                                
         MVC   RSTAKSTA,RCONKSTA                                                
         BAS   RE,HIGH                                                          
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                SHOULD NEVER GET HERE                        
         BAS   RE,GETREC                                                        
         L     R5,AIOAREA                                                       
         MVC   CDATE,RSTACLDT      STATION CLOSING DATE  (YM)                   
         MVC   CSTATION,RSTAKSTA                                                
                                                                                
**************************************************************                  
* ADJUST CLOSE DATE IF NECESSARY                                                
                                                                                
         GOTO1 VDATCON,DMCB,(2,END2),(3,SCLASATD)   AS AT DATE                  
                                                                                
*          DATA SET RESETVAL   AT LEVEL 009 AS OF 06/02/97                      
         CLI   SCLASATD+1,3        AS AT DATE'S MONTH MARCH OR LATER?           
         BNL   SEVA0080            YES                                          
         ZIC   RF,SCLASATD+1       NO  - ADJUST MONTH, AND YEAR                 
         LA    RF,12(RF)           ADD 12 TO MONTH                              
         SH    RF,=H'2'            BACK UP TWO MONTHS                           
         STC   RF,SCLASATD+1       PUT MONTH BACK                               
         ZIC   RF,SCLASATD         BACK YEAR UP 1                               
         BCTR  RF,0                                                             
         STC   RF,SCLASATD         PUT YEAR BACK                                
         B     SEVA0100                                                         
SEVA0080 EQU   *                                                                
         ZIC   RF,SCLASATD+1       ADJUST MONTH BACK 2 MONTHS                   
         SH    RF,=H'2'            BACK UP TWO MONTHS                           
         STC   RF,SCLASATD+1       PUT MONTH BACK                               
SEVA0100 EQU   *                                                                
         CLC   SCLASATD(2),CDATE                                                
*                                  'ADJUSTED' CLOSE VS ACTL STN CLOSE           
         BNL   SEVA0120            USE STATION'S ACTUAL CLOSE                   
         MVC   CDATE,SCLASATD                                                   
*                                  CALC'D EARLIER:  USE IT                      
SEVA0120 EQU   *                                                                
*                                                                               
************************************************************                    
         MVC   KEY,SAVEKEY         RESTORE KEY                                  
         BAS   RE,HIGH             RESTORE SEQ READ                             
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
*- NOPENNY --- REMOVE PENNIES CONGRID AMOUNTS                                   
*                                                                               
*  CONGRID = 12 MONTH BUCKETS, 3 DOLLAR CELLS PER.                              
*                                                                               
*  AFTER STRIPPING PENNIES, CALCULATE CONTRACT TOTALS                           
*  (CONGRIDX).  INSURE SCREEN COLUMNS SUM TO TOTAL                              
*                                                                               
NOPENNY  NTR1                                                                   
         LA    R0,12               13 BUCKETS                                   
         LA    R1,100              DIVIDE BY 100                                
         LA    RF,CONGRID          BUCKETS                                      
*                                                                               
         LA    R5,CONGRIDX         TOTAL ROW                                    
         XC    0(12,R5),0(R5)      START CLEAN                                  
*                                                                               
NPEN20   LA    RE,0                1ST CELL DISPLACEMENT                        
         LA    R4,3                3 CELLS PER BUCKET                           
*                                                                               
NPEN30   EQU   *                   LOOP FOR 1 BUCKET                            
*                                                                               
         L     R2,0(RE,RF)         $'S WITH PENNIES                             
         LA    R3,50                                                            
         AR    R2,R3               ADD 50 CENTS                                 
         LTR   R2,R2                                                            
         BNM   NPEN40              NOT A NEGATIVE.                              
         SR    R2,R1               THE 50 WE ADDED & 50 MORE (-50)              
NPEN40   EQU   *                                                                
         SRDA  R2,32               PROPAGATE SIGN BIT                           
*                                                                               
         DR    R2,R1               /100                                         
         ST    R3,0(RE,RF)         $'S WITHOUT PENNIES                          
*                                                                               
         L     R6,0(RE,R5)         ADD TO TOTAL                                 
         AR    R6,R3                                                            
         ST    R6,0(RE,R5)                                                      
*                                                                               
         LA    RE,4(RE)            NEXT CELL IN BUCKET                          
         BCT   R4,NPEN30                                                        
*                                                                               
         LA    RF,12(RF)           NEXT MONTH BUCKET                            
         BCT   R0,NPEN20                                                        
*                                                                               
         XIT1                                                                   
         SPACE 2                                                                
INVGRID  DS    36F                 INVOICE BUCKETS, 3F/MONTH                    
TEMPIO   DS    CL1000                                                           
         EJECT                                                                  
       ++INCLUDE RGENEROL                                                       
       ++INCLUDE RERISWRK                                                       
       ++INCLUDE REPIOBLK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'039RERIS02S  05/01/02'                                      
         END                                                                    
