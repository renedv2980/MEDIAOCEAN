*          DATA SET DESORTBBMZ AT LEVEL 179 AS OF 05/05/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE SORTBBMA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DATCON                                                                 
*INCLUDE DDINFO                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE KHDUMMY                                                                
SORTBBM  TITLE 'MERGE BBM POCKETPIECE AND NSS TAPE INTO ONE '                   
SORTBBM  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         NBASE 0,*SORTBBM,=V(REGSAVE),RA                                        
*                                                                               
         GOTO1 =V(STXITER),DMCB,STXTAB                                          
         L     R9,=V(CPRINT)                                                    
         USING DPRINT,R9                                                        
         B     INIT                                                             
*                                                                               
STXTAB   DS    0H                                                               
         DC    A(SORTBBM)                                                       
         DC    V(DUMMY)                                                         
         ORG   *-4                                                              
         DC    X'80'                                                            
         ORG                                                                    
         EJECT                                                                  
INIT     OPEN  (BBMIN1,(INPUT))                                                 
         GOTO1 =V(SORTER),DMCB,SORTCRD,(X'80',RECCRD),(X'80',0)                 
*                                                                               
SORT2    GET   BBMIN1,IOAREA                                                    
         CLI   IOAREA,C'3'                                                      
         BE    SORT2                                                            
         CLI   IOAREA,C'1'                                                      
         BNE   *+12                                                             
         BAS   RE,CALCBOOK         DERIVE OUR MONTHLY BOOK                      
         B     SORT5                                                            
*                                                                               
         CLI   IOAREA,C'4'                                                      
         BNE   SORT5                                                            
*                                  SWAP STATION AND DAY FIELDS                  
         MVC   DUB(L'QTRSTACD),(QTRSTACD-QTRHOURD)+IOAREA                       
         MVC   (QSORTDAY-QTRHOURD)+IOAREA,(QTRDAY-QTRHOURD)+IOAREA              
         MVC   (QSORTSTA-QTRHOURD)+IOAREA,DUB                                   
*                                                                               
SORT5    GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT2                                                            
*                                                                               
SORT10   CLOSE (BBMIN1,)                                                        
*                                                                               
         OPEN  (BBMOUT1,(OUTPUT))                                               
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,DMCB+4                                                     
         BZ    SORT20                                                           
*                                                                               
         CLI   0(R3),C'4'                                                       
         BNE   SORT18                                                           
*                                  RESTORE STATION AND DAY FIELDS               
         MVC   DUB(L'QSORTSTA),(QSORTSTA-QTRHOURD)(R3)                          
         MVC   (QTRDAY-QTRHOURD)(,R3),(QSORTDAY-QTRHOURD)(R3)                   
         MVC   (QTRSTACD-QTRHOURD)(,R3),DUB                                     
*                                                                               
SORT18   DS    0H                                                               
         PUT   BBMOUT1,(R3)                                                     
         B     SORT15                                                           
*                                                                               
SORT20   DS    0H                                                               
         CLOSE BBMOUT1                                                          
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*******************************************************                         
*THIS IS THE PART WHEN WE HAVE TO CONVERT FILE INTO QTR CHUNKS                  
*******************************************************                         
         GOTO1 =V(SORTER),DMCB,(X'80',SORTCRD2),RECCRD,(X'80',0)                
*                                                                               
         LA    R6,0          R6 IS COUNTER FOR INPUT DEMO DISPLACEMENT          
         LA    R8,96         96 QTR HOURS NEED TO BE PROCESSED                  
SORT30   OPEN  (BBMOUT1,(INPUT))                                                
         LA    R5,0          R5 IS COUNTER FOR OUT DEMO DISPLACEMENT            
*                                                                               
         MVI   FIRSTREC,C'Y'                                                    
*                                                                               
SORT40   GET   BBMOUT1,IOAREA                                                   
SORT40A  CLI   IOAREA,C'4' QTR HOUR PROCEDURES                                  
         BE    SORT60                                                           
*                                                                               
         CLI   IOAREA,C'5' END OF FILE REACHED?                                 
         BNE   SORT50      NOT "4" OR "5"?                                      
*                                                                               
SORT45   LA    RE,96                                                            
         LR    RF,R8       GET THE NEGATIVE BYTE FOR QTR HOURS                  
         SR    RE,RF       SINCE WE R COUNTING BACKWARDS                        
         STC   RE,SVIOAREA+22  QTRHOUR BYTE WHERE THE SPACE IS                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',SVIOAREA                                 
*                                                                               
SORT50   CHI   R8,1        ANYTHING BUT "4" GETS PRINTED ONCE AT LAST           
         BNE   SORT40      LOOP COUNTER                                         
         GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT40                                                           
*                                                                               
SORT60   CLI   FIRSTREC,C'Y'                                                    
         LA    RE,IOAREA                                                        
         BE    SORT70                                                           
         CLC   IOAREA(12),SVIOAREA                                              
         BNE   SORT90                 IF NEW RECORD IS DIF THAN RELEASE         
SORT70   DS    0H                                                               
         MVC   OUTAREA(23),IOAREA     THE 23 BYTE KEY                           
*************************************************************                   
* LOOK UP CATEGORY IN TABLE TO SEE WHICHDEMO POSITION THE DEMO GO IN            
*************************************************************                   
         LA    RE,INCATTAB                                                      
         LA    RF,IOAREA                                                        
         USING INRECD,RF                                                        
SORT80   CLC   =X'FFFF',0(RE)                                                   
         BNE   *+6                                                              
         DC    H'0'             ---DIE IF DONT FIND CAT CODE---                 
         CLC   INCAT,0(RE)                                                      
         BE    *+12                                                             
         LA    RE,L'INCATTAB(RE)                                                
         B     SORT80                                                           
         DROP   RF                                                              
*                                                                               
         LA    R3,OUTAREA                                                       
         USING OUTRECD,R3                                                       
         LLC   RF,L'INCAT(RE)   GET THE DISPLACEMENT OF DEMO FROM TABLE         
         MHI   RF,8             MULTIPLY BY LENGTH OF DEMOS                     
         LA    RE,OUTDEMOS                                                      
         AR    RE,RF            EXACT OUTPUT POSITION                           
*                                                                               
         LA    RF,IOAREA+24                                                     
         AR    RF,R6           INPUT DEMO SLOT                                  
         MVC   0(L'OUTDEMOS,RE),0(RF)                                           
*************************************************7/6/99 NEW SLOT                
*                                                                               
***                                                                             
         LA    R5,L'OUTDEMOS(R5)                                                
         DROP  R3                                                               
*                                                                               
         LA    RF,L'SVIOAREA                                                    
         LR    R1,RF                                                            
         LA    R0,SVIOAREA    VARIABLE LENGTH                                   
         LA    RE,OUTAREA                                                       
         MVCL  R0,RE                                                            
         BNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         MVI   FIRSTREC,C'N'                                                    
*                                                                               
         B     SORT40                                                           
*                                                                               
SORT90   DS    0H                                                               
         LA    RE,96                                                            
         LR    RF,R8       GET THE LOOP COUNTER AND SUBTRACT FOR QH             
         SR    RE,RF       SINCE WE R COUNTING BACKWARDS                        
         STC   RE,SVIOAREA+22  QTRHOUR BYTE WHERE THE SPACE IS                  
START6A  GOTO1 =V(SORTER),DMCB,=C'PUT',SVIOAREA                                 
*                                                                               
START2A  LA    R5,0           REST DEMO COUNTER AFTER WE OUPUT REC              
*                                                                               
* SINCE WE READ THE NEW RECORD W CHANGED KEY ALREADY WE NEED TO SAVE IT         
* AS THE NEW SVIOAREA                                                           
*                                                                               
*                                                                               
         MVC   OUTAREA(23),IOAREA     THE 23 BYTE KEY                           
         LA    R3,OUTAREA                                                       
         USING OUTRECD,R3                                                       
         LA    R4,OUTDEMOS                                                      
         AR    R4,R5           OUPUT DEMO SLOT                                  
         LA    RE,IOAREA+24                                                     
         AR    RE,R6           INPUT DEMO SLOT                                  
         MVC   0(L'OUTDEMOS,R4),0(RE)                                           
         LA    R5,L'OUTDEMOS(R5)                                                
         DROP  R3                                                               
*                                                                               
         LA    RF,L'OUTAREA                                                     
         LR    R1,RF                                                            
         LA    R0,SVIOAREA    VARIABLE LENGTH                                   
         LA    RE,OUTAREA                                                       
         MVCL  R0,RE                                                            
         BNO   *+6                 DESTRUCTIVE MOVE?                            
         DC    H'0'                YES!                                         
         MVI   FIRSTREC,C'N'                                                    
         B     SORT40A                                                          
*                                                                               
SORT100  DS    0H                                                               
         CLOSE BBMOUT1                                                          
         LA    R6,L'OUTDEMOS(R6)                                                
         MVI   FIRSTREC,C'Y'   RESET FIRSTREC FLAG FOR REREAD OF FILE           
         BCT   R8,SORT30       WILL SCAN FILE ONCE FOR EACH QH                  
*******************************************************                         
* NOW RESORT THE RECORDS BY QTR HOURS                                           
*******************************************************                         
         OPEN  (BBMOUT2,(OUTPUT))                                               
SORT120  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R3,15,DMCB+4                                                     
         BZ    SORT130                                                          
*                                                                               
         CLI   0(R3),C'4'                                                       
         BNE   SORT125                                                          
         CLI   22(R3),15           SKIPPING 2-6AM AS OF AUG17/04(SEAN)          
         BNH   SORT120                                                          
         LLC   RE,22(R3)           SET 6 O'CLOCK AS HOUR 0                      
         SHI   RE,16                                                            
         STC   RE,22(R3)                                                        
SORT125  PUT   BBMOUT2,(R3)                                                     
         B     SORT120                                                          
*                                                                               
SORT130  DS    0H                                                               
         CLOSE BBMOUT2                                                          
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*******************************************************                         
DONE     XBASE                                                                  
         SPACE 2                                                                
*                                                                               
* I/O ERROR HANDLER                                                             
*                                                                               
IOERR    DS    0H                                                               
*                                                                               
* EXTRACT SYSTEM ERROR MESSAGES INTO FIELD WORK. SEE IBM MANUAL                 
* "Z/OS DFSMS MACRO INSTRUCTIONS FOR DATA SETS", SECTION ON SYNADAF             
* MACRO, FOR DETAILS. IF WE HAVE A DDNAME, TRY TO EXTRACT A DSN OR              
* PATHNAME AND DISPLAY IT ALONG WITH THE SYNAD MESSAGES.                        
*                                                                               
         SYNADAF ACSMETH=QSAM                                                   
         MVC   WORK,50(R1)         MESSAGE AREA FROM SYNADAF                    
         SYNADRLS                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'I/O ERROR: FORCING USER AB+        
               END.'                                                            
*                                                                               
         CLC   WORK+25(8),SPACES   DO WE HAVE A DDNAME?                         
         BE    IOERR20             NO                                           
*                                  YES: TRY TO EXTRACT DSN                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRTDSN)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO DSN AVAILABLE                             
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
         CLC   =C'...PATH=.SPECIFIED...',0(RE)                                  
         BNE   IOERR10             IT'S NOT A PATHNAME                          
*                                                                               
*                                  TRY TO EXTRACT PATHNAME                      
         GOTO1 =V(DDINFO),DMCB,(8,WORK+25),(0,=AL2(DINRPATH)),0                 
         LTR   RF,RF                                                            
         BNZ   IOERR20             BAD RETURN FROM DDINFO                       
         SR    R2,R2                                                            
         ICM   R2,1,DMCB+8                                                      
         BZ    IOERR20             NO PATHNAME RETURNED                         
         L     RE,DMCB+8           A(RETURNED DSN OR PATHNAME)                  
*                                                                               
IOERR10  DS    0H                                                               
         MVC   OPERMSG(21),=C'FAILURE READING FROM '                            
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   OPERMSG+21(0),0(RE) PUT PATHNAME INTO CONSOLE MESSAGE            
         AHI   R2,1+21             R2 = L'MESSAGE                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',((R2),OPERMSG)                        
*                                                                               
IOERR20  DS    0H                                                               
         GOTO1 =V(LOGIO),DMCB,X'FF000001',=C'SYNAD ERROR MESSAGES FOLLO+        
               W:'                                                              
         MVC   OPERMSG,SPACES      BUILD FIRST MESSAGE LINE...                  
         MVC   OPERMSG(59),WORK+18 STARTING AFTER <JOBNAME,STEPNAME,>           
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(59,OPERMSG)                          
         CLI   WORK+77,C'S'        IS THERE A 2ND MESSAGE?                      
         BNE   IOERRXIT                                                         
         MVC   OPERMSG,SPACES      YES: BUILD IT                                
         MVC   OPERMSG,WORK+94                                                  
         GOTO1 =V(LOGIO),DMCB,X'FF000001',(L'OPERMSG,OPERMSG)                   
*                                                                               
IOERRXIT DS    0H                                                               
         ABEND 925                                                              
         EJECT                                                                  
CALCBOOK NTR1                                                                   
*                                                                               
* IOAREA CONTAINS THE BBM '1' (HEADER) RECORD                                   
*                                                                               
         LA    R4,IOAREA                                                        
         PACK  DUB,HDWEEKNO-HEADERD(,R4)                                        
         CVB   R4,DUB              R4 = WEEK NUMBER FROM HEADER RECORD          
*                                                                               
         OPEN  BBMWEEK                                                          
*                                                                               
CB10     DS    0H                                                               
         GET   BBMWEEK,WORK                                                     
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,WORK,(2,0) FIRST FIELD IS WEEK NUMBER            
         CLI   DMCB,X'FF'                                                       
         BNE   *+6                                                              
         DC    H'0'                MISSING WEEK NO. IN WEEK.TXT FILE!           
         L     R2,DMCB+4           WEEK NUMBER FROM WEEK.TXT FILE               
         CR    R2,R4               MATCH ON WEEK NUMBER?                        
         BNE   CB10                NO                                           
*                                                                               
         L     R4,DMCB+8           L'WEEK NUMBER STRING                         
         CLOSE (BBMWEEK,)                                                       
*                                                                               
         LA    R4,WORK+1(R4)       POINT BEYOND WEEK NUMBER TO SURVEY           
         LA    R3,OUTAREA          BUILD SORT RECORD HERE                       
         XC    0(256,R3),0(R3)     FORCE LOW KEY, SO IT SORTS FIRST             
         USING PHEADERD,R3                                                      
         MVC   PHDBKMM,=C'03'      POSIT SPRING BOOK --> MAR                    
         CLC   =C'SP',0(R4)                                                     
         BE    CB20                CORRECT                                      
         MVC   PHDBKMM,=C'07'      POSIT SUMMER BOOK --> JUL                    
         CLC   =C'SU',0(R4)                                                     
         BE    CB20                CORRECT                                      
         MVC   PHDBKMM,=C'11'      POSIT FALL BOOK --> NOV                      
         CLC   =C'FA',0(R4)                                                     
         BE    CB20                CORRECT                                      
         DC    H'0'                UNKNOWN SEASON IN WEEK.TXT !                 
*                                                                               
CB20     DS    0H                                                               
         MVC   DUB(2),2(R4)        YY                                           
         MVC   DUB+2(4),=C'0101'   FOR DATCON                                   
         GOTO1 =V(DATCON),DMCB,DUB,(20,DUB1)                                    
         MVC   PHDBKYR,DUB1        YYYY                                         
         DROP  R3                                                               
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',OUTAREA                                  
*                                                                               
         XIT1                                                                   
*                                                                               
* THIS IS THE EODAD FOR THE WEEK.TXT FILE. IF WE GET HERE, SOMETHING            
* IS VERY WRONG, BECAUSE IT MEANS WE'VE READ THE ENTIRE FILE AND                
* HAVEN'T FOUND A MATCH ON THE WEEK NUMBER!                                     
DEATH    DC    H'0'                BBMWEEK FILE EODAD                           
*                                                                               
         EJECT                                                                  
*                                                                               
* CATEGORY TABLE FOR DEMOS                                                      
INCATTAB DS    0CL7                                                             
         DC    CL6'000001',AL1(0)                                               
         DC    CL6'000002',AL1(1)                                               
         DC    CL6'000003',AL1(2)                                               
         DC    CL6'000004',AL1(3)                                               
         DC    CL6'000005',AL1(4)                                               
         DC    CL6'000006',AL1(5)                                               
         DC    CL6'000007',AL1(6)                                               
         DC    CL6'000008',AL1(7)                                               
         DC    CL6'000009',AL1(8)                                               
         DC    CL6'000010',AL1(9)                                               
         DC    CL6'000011',AL1(10)                                              
         DC    CL6'000012',AL1(11)                                              
         DC    CL6'000013',AL1(12)                                              
         DC    CL6'000014',AL1(13)                                              
         DC    CL6'000015',AL1(14)                                              
         DC    CL6'000016',AL1(15)                                              
         DC    CL6'000017',AL1(16)                                              
         DC    CL6'000018',AL1(17)                                              
         DC    XL2'FFFF'                                                        
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,22,A),FORMAT=BI,EQUALS'                      
SORTCRD2 DC    CL80'SORT FIELDS=(2,7,A,1,23,A),FORMAT=BI,EQUALS'                
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=793'                                   
         EJECT                                                                  
BBMWEEK  DCB   DDNAME=BBMWEEK,DSORG=PS,RECFM=FB,MACRF=GM,              X        
               EODAD=DEATH,LRECL=80,BLKSIZE=0,SYNAD=IOERR                       
BBMIN1   DCB   DDNAME=BBMIN1,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=SORT10,LRECL=792,BLKSIZE=0,SYNAD=IOERR                     
BBMOUT1  DCB   DDNAME=BBMOUT1,DSORG=PS,RECFM=FB,MACRF=(GM,PM),         X        
               EODAD=SORT100,LRECL=792,BLKSIZE=0,SYNAD=IOERR                    
BBMOUT2  DCB   DDNAME=BBMOUT2,DSORG=PS,RECFM=FB,MACRF=(PM),            X        
               LRECL=200,BLKSIZE=2000                                           
*                                                                               
DMCB     DS    6F                                                               
DUB      DS    D                                                                
DUB1     DS    D                                                                
OPERMSG  DS    CL100               MAXIMUM LENGTH FOR LOGIO                     
WORK     DS    CL256                                                            
FIRSTREC DS    C                                                                
         DS    0D                                                               
         DC    C'***I/O**'                                                      
IOAREA   DS    CL793                                                            
SVIOAREA DS    CL200                                                            
OUTAREA  DS    CL793                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDDPRINT                                                       
         PRINT ON                                                               
*                                                                               
INRECD   DSECT                                                                  
         DS   CL14                                                              
INCAT    DS   CL6                                                               
*                                                                               
OUTRECD  DSECT                                                                  
OUTKEY   DS    CL24                                                             
OUTDEMOS DS    18CL8                                                            
*                                                                               
*  AUDIENCE RECORD                                                              
QTRHOURD DSECT                                                                  
QTRRECTY DS    CL1                 ALWAYS C'4'                                  
QTRPANCD DS    CL3                                                              
QTRAGBWK DS    CL4                                                              
QTRDAY   DS    CL1                                                              
QTRSTACD DS    CL3                                                              
         ORG   QTRDAY                                                           
QSORTSTA DS    CL3                 FOR SORTING                                  
QSORTDAY DS    CL1                 FOR SORTING                                  
         ORG                                                                    
         DS    CL1                                                              
QTRACTV  DS    CL1                                                              
QTRCATCD DS    CL6                                                              
QTRSTHR  DS    CL2                                                              
QTRQHR   DS    CL1                                                              
         DS    CL1                                                              
QTRAUDS  DS    0CL294                                                           
QTRAUD   DS    49CL6                                                            
*                                                                               
*  DSECT OF HEADER REC                                                          
HEADERD  DSECT                                                                  
HDRECTY  DS    CL1                 ALWAYS C'1'                                  
         DS    CL3                                                              
HDWEEKNO DS    CL4                 WEEK ID FROM WEEK.TXT FILE                   
         DS    CL22                                                             
HEADSTRT DS    0CL8                START DAY DATE                               
HEADDD   DS    CL2                                                              
HEADMM   DS    CL2                                                              
HEADYR   DS    CL4                                                              
HDFILENM DS    CL11                FILENAME OF THE QHR FILE                     
HDFILLER DS    0C                                                               
*                                                                               
*  PRE-HEADER RECORD (GENERATED BY THIS PROGRAM)                                
PHEADERD DSECT                                                                  
PHDRECTY DS    XL30                ALWAYS NULLS (SO IT SORTS FIRST)             
PHDBK    DS    0CL6                OUR DERIVED MONTHLY BOOK: YYMM               
*                                   FALL   --> NOV                              
*                                   SPRING --> MAR                              
*                                   SUMMER --> JUL                              
PHDBKYR  DS    CL4                 YEAR                                         
PHDBKMM  DS    CL2                 MONTH                                        
*                                                                               
         IEFZB4D2                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'179DESORTBBMZ05/05/15'                                      
         END                                                                    
