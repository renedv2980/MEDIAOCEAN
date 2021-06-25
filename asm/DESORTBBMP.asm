*          DATA SET DESORTBBMP AT LEVEL 004 AS OF 01/28/13                      
*PROCESS USING(WARN(15))                                                        
*PHASE SORTPPMA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE DDINFO                                                                 
*INCLUDE LOGIO                                                                  
*INCLUDE SORTER                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
*INCLUDE PDUMPER                                                                
*INCLUDE STXITER                                                                
*INCLUDE KHDUMMY                                                                
SORTBBM  TITLE 'SORT BBM DATA BEFORE THE CONVERSION'                            
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
         GOTO1 =V(SORTER),DMCB,SORTCRD,RECCRD,0                                 
*                                                                               
SORT2    GET   BBMIN1,IOAREA                                                    
         CLI   IOAREA,C'3'                                                      
         BE    SORT2                                                            
         CLI   IOAREA,C'2'                                                      
         BE    *+8                                                              
         CLI   IOAREA,C'4'                                                      
         BNE   SORT5                                                            
         MVC   STATION,IOAREA+9    SWAP STATION AND DAY                         
         MVC   IOAREA+11(1),IOAREA+8                                            
         MVC   IOAREA+8(L'STATION),STATION                                      
SORT5    GOTO1 =V(SORTER),DMCB,=C'PUT',IOAREA                                   
         B     SORT2                                                            
*                                                                               
SORT10   CLOSE (BBMIN1,)                                                        
*                                                                               
         OPEN  (BBMOUT,(OUTPUT))                                                
*                                                                               
SORT15   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BE    SORT20                                                           
         L     R3,DMCB+4                                                        
         MVC   STATION,8(R3)       SWAP STATION AND DAY                         
         MVC   8(1,R3),11(R3)                                                   
         MVC   9(L'STATION,R3),STATION                                          
         MVC   P(80),0(R3)                                                      
         PUT   BBMOUT,(R3)                                                      
         B     SORT15                                                           
*                                                                               
SORT20   DS    0H                                                               
         CLOSE BBMOUT                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
*******************************************************                         
*THIS IS THE PART WHEN WE HAVE TO CONVERT FILE INTO QTR CHUNKS                  
*******************************************************                         
         GOTO1 =V(SORTER),DMCB,SORTCRD2,RECCRD,0                                
*                                                                               
         LA    R6,0          R6 IS COUNTER FOR INPUT DEMO DISPLACEMENT          
         LA    R8,96         96 QTR HOURS NEED TO BE PROCESSED                  
SORT30   OPEN  (BBMIN2,(INPUT))                                                 
         LA    R5,0          R5 IS COUNTER FOR OUT DEMO DISPLACEMENT            
*                                                                               
         MVI   FIRSTREC,C'Y'                                                    
*                                                                               
SORT40   GET   BBMIN2,IOAREA                                                    
SORT40A  CLI   IOAREA,C'4'                                                      
         BE    SORT60      QTR HOUR PROCEDURES                                  
         CLI   IOAREA,C'5'                                                      
         BNE   SORT50      END OF FILE REACHED?                                 
*                                                                               
         LA    RE,96                                                            
         LR    RF,R8       GET THE NEGATIVE BYTE FOR QTR HOURS                  
         SR    RE,RF       SINCE WE ARE COUNTING BACKWARDS                      
*                                                                               
         CHI   RE,16       2A-545A?                                             
         BNL   *+12                                                             
         AHI   RE,80                                                            
         B     *+8                                                              
         SHI   RE,16       6A-145A                                              
*                                                                               
         STC   RE,SVIOAREA+22                                                   
         GOTO1 =V(SORTER),DMCB,=C'PUT',SVIOAREA                                 
*                                                                               
SORT50   CHI   R8,1        LOOP COUNTER                                         
         BNE   SORT40                                                           
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
*                                                                               
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
         SR    RE,RF       SINCE WE ARE COUNTING BACKWARDS                      
*                                                                               
         CHI   RE,16       2A-545A?                                             
         BNL   *+12                                                             
         AHI   RE,80                                                            
         B     *+8                                                              
         SHI   RE,16       6A-145A                                              
*                                                                               
         STC   RE,SVIOAREA+22                                                   
START6A  GOTO1 =V(SORTER),DMCB,=C'PUT',SVIOAREA                                 
*                                                                               
START2A  LA    R5,0           REST DEMO COUNTER AFTER WE OUPUT REC              
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
*                                                                               
         B     SORT40A                                                          
SORT100  DS    0H                                                               
         CLOSE BBMIN2                                                           
         LA    R6,L'OUTDEMOS(R6)                                                
         MVI   FIRSTREC,C'Y'   RESET FIRSTREC FLAG FOR REREAD OF FILE           
         BCT   R8,SORT30       WILL SCAN FILE ONCE FOR EACH QH                  
*******************************************************                         
* NOW RESORT THE RECORDS BY QTR HOURS                                           
*******************************************************                         
         OPEN  (BBMOUT2,(OUTPUT))                                               
SORT120  GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BE    SORT130                                                          
         L     R3,DMCB+4                                                        
         CLI   0(R3),C'4'                                                       
         BNE   SORT125                                                          
*                                                                               
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
*                                                                               
STATION  DS    CL3                                                              
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
SORTCRD  DC    CL80'SORT FIELDS=(1,22,A),FORMAT=BI'                             
SORTCRD2 DC    CL80'SORT FIELDS=(2,7,A,1,23,A),FORMAT=BI'                       
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=793'                                   
         EJECT                                                                  
BBMIN1   DCB   DDNAME=BBMIN1,DSORG=PS,RECFM=FB,MACRF=(GM),             X        
               EODAD=SORT10,LRECL=792,BLKSIZE=0,SYNAD=IOERR                     
BBMIN2   DCB   DDNAME=BBMOUT1,DSORG=PS,RECFM=FB,MACRF=(GM),            X        
               EODAD=SORT100,LRECL=792,BLKSIZE=0,SYNAD=IOERR                    
BBMOUT   DCB   DDNAME=BBMOUT1,DSORG=PS,RECFM=FB,MACRF=(PM),            X        
               LRECL=792,BLKSIZE=7920                                           
BBMOUT2  DCB   DDNAME=BBMOUT2,DSORG=PS,RECFM=FB,MACRF=(PM),            X        
               LRECL=200,BLKSIZE=2000                                           
*                                                                               
DMCB     DS    6F                                                               
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
         IEFZB4D2                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004DESORTBBMP01/28/13'                                      
         END                                                                    
