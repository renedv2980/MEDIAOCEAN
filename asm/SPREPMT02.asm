*          DATA SET SPREPMT02  AT LEVEL 101 AS OF 05/01/02                      
*PHASE SPMT02A                                                                  
*INCLUDE BINSRCH2                                                               
         TITLE 'SPMT02 - SPOT MARKET NUMBER CHANGE'                             
SPMT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPMT02                                                         
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    MT10                                                             
         CLI   MODE,REQFRST                                                     
         BE    CHMKT                                                            
         B     EXIT                                                             
*                                                                               
NEQXIT   LTR   RB,RB               SET CC NOT EQUAL                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                  SORT THE TABLE, STUPID..                     
*                                                                               
MT10     DS    0H                                                               
         LA    R7,CHGTAB                                                        
         L     R5,BINNUM                                                        
         GOTO1 XSORT,DMCB,(R7),(R5),4,2,0                                       
*                                                                               
*                                                                               
         LA    R0,3                                                             
         LA    R1,VSORTER                                                       
FJ20     LA    RE,RELO                                                          
         S     RE,RELO                                                          
         A     RE,0(R1)                                                         
         ST    RE,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,FJ20                                                          
         SPACE 1                                                                
         B     EXIT                                                             
*********************************************************                       
*                                                                               
RELO     DC    A(*)                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
CHMKT    DS    0H                                                               
COPY     EQU   1                                                                
*                                                                               
         OPEN  (FILEIN,(INPUT),FILEOUT,(OUTPUT))                                
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'00'                                                            
*                                                                               
CHMKT5   LA    R1,FILEIN                                                        
         L     R0,ADBUY                                                         
         SH    R0,=H'4'            VARBLK TAPE COMPENSATION                     
         GET   (R1),(R0)                                                        
*                                                                               
*                                                                               
* CHGMKT PROCESSSING                                                            
         SPACE 1                                                                
*                                                                               
*                            CHANGE MARKET# GOAL RECORDS                        
*                                                                               
         MVI   DMINBTS,X'08'       READ DELETED RECS                            
         MVI   DMOUTBTS,X'FD'      MEL SAID SO                                  
         XC    P1,P1                                                            
         MVI   ELCNT,0                                                          
*                                                                               
         L     R6,ADBUY                                                         
         SR    R1,R1                                                            
         LH    R1,13(R6)           INSERT REC LEN                               
         AR    R1,R6               INDEX INTO RECORD TO END                     
         XC    0(4,R1),0(R1)       CLEAR A FEW BYTES                            
         USING GKEYD,R6                                                         
*                                                                               
CHMKT10  CLI   GKEYTYPE,X'02'      TEST GOAL RECORDS                            
         BNE   CHMKT100                                                         
*                                                                               
         TM    15(R6),X'80'                                                     
         BO    CHMKT999                                                         
*                                                                               
CHMKT20  CLI   GKEYAM,X'92'      TEST AGY MH/RADIO                              
         BE    CHMKT30                                                          
         B     CHMKT999                                                         
*                                                                               
CHMKT30  CLC   GKEYMKT,SAVMKT                                                   
         BNE   *+12                                                             
         L     R7,SAVADD           ADDRESS OF LAST MARKET                       
         B     CHMKT40                                                          
*                                                                               
         LA    R0,GKEYMKT          FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT45                                                          
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT40  DS    0H                                                               
*                                                                               
         MVC   GKEYMKT,NEWMKT                                                   
*                                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT                                                         
*                                                                               
         CH    R0,=H'500'          PRINT EVERY 500TH                            
         BL    CHMKT999                                                         
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(L'GKEY,R5),0(R6)         MOVE CHANGED REC TO PRINT             
         MVC   20(L'OLDMKT,R5),OLDMKT                                           
         MVC   25(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
CHMKT45  DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         XC    P1,P1                                                            
         LA    R5,P1                                                            
         MVC   0(L'GKEY,R5),0(R6)                                               
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
         DROP  R7,R6                                                            
         EJECT                                                                  
*              CHANGE MARKET# FOR MKT ASSIGNMENT REC                            
*                                                                               
*                                                                               
CHMKT100 DS    0H                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING MKARECD,R6                                                       
*                                                                               
CHMKT120 CLC   MKAKTYP,=X'0D03'   TEST MARKET ASSIGNMENT RECS                   
         BNE   CHMKT150                                                         
*                                                                               
         TM    15(R6),X'80'                                                     
         BO    CHMKT999                                                         
*                                                                               
         CLI   MKAKAGMD,X'92'      TEST AGY MH/RADIO                            
         BE    CHMKT130                                                         
         B     CHMKT999                                                         
*                                                                               
CHMKT130 CLC   MKAKMKT,SAVMKT                                                   
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT140                                                         
*                                                                               
         LA    R0,MKAKMKT          FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT145                                                         
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT140 DS    0H                                                               
*                                                                               
         MVC   MKAKMKT,NEWMKT                                                   
*                                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
         STH   R0,COUNT                                                         
*                                                                               
         CH    R0,=H'500'       PRINT EVERY 500TH                               
         BL    CHMKT999                                                         
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(L'MKAKEY,R5),0(R6)      MOVE CHANGED REC TO PRINT              
         MVC   20(L'OLDMKT,R5),OLDMKT                                           
         MVC   25(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
CHMKT145 DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         LA    R5,P1                                                            
         MVC   0(L'MKAKEY,R5),0(R6)                                             
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
         DROP  R7,R6                                                            
         EJECT                                                                  
*                                                                               
*              CHANGE MARKET# FOR STATION BILL REC                              
*                                                                               
*                                                                               
CHMKT150 DS    0H                                                               
*                                                                               
         L     R6,ADBUY                                                         
         USING STABUCKD,R6                                                      
*                                                                               
CHMKT170 CLC   STABKCOD,=X'0E01'   TEST STATION BUCKET CODES                    
         BNE   CHMKT200                                                         
*                                                                               
         TM    15(R6),X'80'                                                     
         BO    CHMKT999                                                         
*                                                                               
         CLI   STABKAM,X'92'      TEST AGY/MEDIA                                
         BE    CHMKT180                                                         
         B     CHMKT999                                                         
*                                                                               
CHMKT180 CLC   STABKMKT,SAVMKT                                                  
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT190                                                         
*                                                                               
         LA    R0,STABKMKT         FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT195                                                         
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT190 DS    0H                                                               
*                                                                               
         MVC   STABKMKT,NEWMKT                                                  
*                                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
*                                                                               
         STH   R0,COUNT                                                         
         CH    R0,=H'500'       PRINT EVERY 500TH                               
         BL    CHMKT999                                                         
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(12,R5),0(R6)      MOVE CHANGED REC TO PRINT                    
         MVC   20(L'OLDMKT,R5),OLDMKT                                           
         MVC   25(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
CHMKT195 DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         XC    P1,P1                                                            
         LA    R5,P1                                                            
         MVC   0(12,R5),0(R6)                                                   
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
         DROP  R7,R6                                                            
         EJECT                                                                  
*                                                                               
*              CHANGE MARKET# FOR CANADIAN SPILL                                
*                                                                               
*                                                                               
CHMKT200 DS    0H                                                               
         L     R6,ADBUY                                                         
         USING SDEFRECD,R6                                                      
*                                                                               
CHMKT220 CLC   SDEFKTYP,=X'0D13'   TEST CANADIAN SPILL RECS                     
         BNE   CHMKT250                                                         
*                                                                               
         TM    15(R6),X'80'                                                     
         BO    CHMKT999                                                         
*                                                                               
         CLC   SDEFKAGY,=C'MH'     TEST AGY                                     
         BE    CHMKT230                                                         
         B     CHMKT999                                                         
*                                                                               
CHMKT230 L     R2,ADBUY                                                         
*                                                                               
         USING SDEFEL05,R2                                                      
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHMKT999                                                         
         B     *+12                                                             
*                                                                               
CHMKT235 BAS   RE,NEXTEL                                                        
         BNE   CHMKT240                                                         
*                                                                               
         CLC   SDEFAMKT,SAVMKT                                                  
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT237                                                         
*                                                                               
         LA    R0,SDEFAMKT         FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT235                                                         
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT237 MVC   SDEFAMKT,NEWMKT      MOVE IN NEW MKT#                            
         AI    ELCNT,1                                                          
         B     CHMKT235                                                         
*                                                                               
CHMKT240 DS    0H                                                               
         OC    ELCNT,ELCNT                                                      
         BZ    CHMKT245                                                         
*                                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
*                                                                               
         STH   R0,COUNT                                                         
         CH    R0,=H'500'       PRINT EVERY 500TH                               
         BL    CHMKT999                                                         
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(L'SDEFKEY,R5),0(R6)      MOVE CHANGED REC TO PRINT             
         MVC   20(1,R5),ELCNT                                                   
         MVC   25(L'OLDMKT,R5),OLDMKT                                           
         MVC   30(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
CHMKT245 DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         XC    P1,P1                                                            
         LA    R5,P1                                                            
         MVC   0(L'SDEFKEY,R5),0(R6)                                            
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
         DROP  R6,R7,R2                                                         
         EJECT                                                                  
*                                                                               
*              CHANGE MARKET# FOR DEMO OVERRIDE                                 
*                                                                               
*                                                                               
CHMKT250 DS    0H                                                               
         L     R6,ADBUY                                                         
         USING DOVRECD,R6                                                       
*                                                                               
CHMKT270 CLC   DOVKTYP,=X'0D14'   TEST DEMO OVERRIDE RECS                       
         BNE   CHMKT300                                                         
*                                                                               
         TM    15(R6),X'80'                                                     
         BO    CHMKT999                                                         
*                                                                               
         CLI   DOVKAGMD,X'92'      TEST AGY/MEDIA                               
         BE    CHMKT280                                                         
         B     CHMKT999                                                         
*                                                                               
CHMKT280 DS    0H                                                               
         L     R2,ADBUY                                                         
         USING DOVEL05,R2                                                       
*                                                                               
         MVI   ELCODE,X'05'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHMKT999                                                         
         B     *+12                                                             
*                                                                               
CHMKT285 BAS   RE,NEXTEL                                                        
         BNE   CHMKT292                                                         
*                                                                               
         CLI   DOVSTA,0            TEST FOR SPILL MKT                           
         BNZ   CHMKT285                                                         
         CLC   DOVMKT,SAVMKT                                                    
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT290                                                         
*                                                                               
         LA    R0,DOVMKT           FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT285                                                         
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT290 MVC   DOVMKT,NEWMKT      MOVE IN NEW MKT#                              
         AI    ELCNT,1                                                          
         B     CHMKT285                                                         
*                                                                               
CHMKT292 DS    0H                                                               
         OC    ELCNT,ELCNT         TEST ANY ELEMENTS CHANGED                    
         BZ    CHMKT295                                                         
*                                                                               
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
*                                                                               
         STH   R0,COUNT                                                         
         CH    R0,=H'500'       PRINT EVERY 500TH                               
         BL    CHMKT999                                                         
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(L'DOVKEY,R5),0(R6)      MOVE CHANGED REC TO PRINT              
         MVC   20(1,R5),ELCNT                                                   
         MVC   25(L'OLDMKT,R5),OLDMKT                                           
         MVC   30(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
CHMKT295 DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         XC    P1,P1                                                            
         LA    R5,P1                                                            
         MVC   0(L'DOVKEY,R5),0(R6)                                             
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT999                                                         
*                                                                               
         DROP  R6,R7,R2                                                         
         EJECT                                                                  
*                                                                               
CHMKT300 DS    0H                  CHANGE BUY RECORDS                           
         MVI   KEYSAVE,X'92'                                                    
         BAS   RE,BUYGET                                                        
         B     CHMKT999                                                         
         SPACE 6                                                                
BUYGET   NTR1                                                                   
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
CHMKT310 CLC   BUYKAM(1),KEYSAVE          TEST BUY RECORDS                      
         BNE   NEQXIT              SET CC NOT EQUAL                             
*                                                                               
         TM    15(R6),X'80'        TEST FOR DELETE                              
         BO    EXIT                                                             
*                                                                               
         CLC   4(2,R6),SAVMKT                                                   
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT320                                                         
*                                                                               
         LA    R0,4(R6)            FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT325            NOT IN KEY-TRY ELMENTS                       
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT320 DS    0H                                                               
*                                                                               
         MVC   4(2,R6),NEWMKT      CHANGE IN FILE KEY                           
         DROP  R7                                                               
*                                                                               
CHMKT325 L     R2,ADBUY            WHERE RECORD IS TO BE                        
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
         BNE   CHMKT340                                                         
         B     *+12                                                             
*                                                                               
CHMKT330 BAS   RE,NEXTEL                                                        
         BNE   CHMKT340                                                         
*                                                                               
         CLC   4(2,R2),SAVMKT                                                   
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT335                                                         
*                                                                               
         LA    R0,4(R2)            FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT330                                                         
*                                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT335 MVC   4(2,R2),NEWMKT      CHANGE IN ELEMENT                            
         AI    ELCNT,1                                                          
         B     CHMKT330                                                         
*                                                                               
CHMKT340 DS    0H                                                               
         OC    ELCNT,ELCNT                                                      
         BZ    CHMKT345                                                         
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
*                                                                               
         STH   R0,COUNT                                                         
         CH    R0,=H'500'       PRINT EVERY 500TH                               
         BL    CHMKT350                                                         
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(17,R5),0(R6)         MOVE CHANGED REC TO PRINT                 
         MVC   28(1,R5),ELCNT      # ELEMENTS CHANGED                           
         MVC   30(L'OLDMKT,R5),OLDMKT                                           
         MVC   35(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT350                                                         
*                                                                               
CHMKT345 DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         XC    P1,P1                                                            
         LA    R5,P1                                                            
         MVC   0(17,R5),0(R6)                                                   
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     CHMKT350                                                         
*                                                                               
CHMKT350 DS    0H                                                               
         MVI   ELCNT,0                                                          
         CLI   0(R6),X'83'         TEST FOR NETWORK BUYS                        
         BNE   EXIT                                                             
         L     R2,ADBUY                                                         
         MVI   ELCODE,X'68'        OOPS...I FORGOT THESE                        
         BAS   RE,GETEL                                                         
         BNE   CHMKT370                                                         
         B     *+12                                                             
*                                                                               
CHMKT360 BAS   RE,NEXTEL                                                        
         BNE   CHMKT370                                                         
*                                                                               
         CLI   1(R2),X'0B'                                                      
         BNE   EXIT                                                             
*                                                                               
         CLC   2(2,R2),SAVMKT                                                   
         BNE   *+12                                                             
         L     R7,SAVADD                                                        
         B     CHMKT365                                                         
*                                                                               
         LA    R0,2(R2)            FIND OLD MKT IN TABLE                        
         ST    R0,BINAREC          A(RECORD TO BE FOUND)                        
         MVI   BINCMND,0           READ HIGH                                    
*                                                                               
         GOTO1 BINSRCH,BINPARMS                                                 
         CLI   0(R1),0             TEST ERROR                                   
         BNE   CHMKT360                                                         
*                                                                               
         DROP  R7                                                               
         L     R7,0(R1)            ADDRESS OF RECORD THAT MATCHES               
         USING MKTCHG,R7                                                        
*                                                                               
         MVC   SAVADD,0(R1)                                                     
         MVC   SAVMKT,0(R7)                                                     
*                                                                               
CHMKT365 MVC   2(2,R2),NEWMKT      CHANGE IN ELEMENT                            
         AI    ELCNT,1                                                          
         B     CHMKT360                                                         
*                                                                               
CHMKT370 DS    0H                                                               
         OC    ELCNT,ELCNT                                                      
         BZ    CHMKT375                                                         
         LH    R0,COUNT                                                         
         AH    R0,=H'1'                                                         
*                                                                               
         STH   R0,COUNT                                                         
         CH    R0,=H'500'       PRINT EVERY 500TH                               
         BL    EXIT                                                             
         XC    COUNT,COUNT                                                      
         LA    R5,P1                                                            
*                                                                               
         MVC   0(17,R5),0(R6)         MOVE CHANGED REC TO PRINT                 
         MVI   20(R5),X'68'                                                     
         MVC   28(1,R5),ELCNT      # ELEMENTS CHANGED                           
         MVC   30(L'OLDMKT,R5),OLDMKT                                           
         MVC   35(L'NEWMKT,R5),NEWMKT                                           
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'CHANGE',(R5),C'DUMP',40,=C'1D'                    
         B     EXIT                                                             
*                                                                               
CHMKT375 DS    0H                  UNCHANGED RECORD NOT IN TABLE                
         XC    P1,P1                                                            
         LA    R5,P1                                                            
         MVC   0(17,R5),0(R6)                                                   
         MVI   20(R5),X'68'                                                     
*                                                                               
         GOTO1 PRNTBL,DMCB,=C'OUTREC',(R5),C'DUMP',40,=C'1D'                    
         B     EXIT                                                             
         DROP  R7,R6                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
CHMKT999 DS    0H                                                               
         BAS   RE,OUTFILE                                                       
         B     CHMKT5                                                           
*                                                                               
         EJECT                                                                  
*                                                                               
*              ROUTINE TO MOVE RECORD TO OUTFILE                                
*                                                                               
*                                                                               
OUTFILE  NTR1                                                                   
*                                                                               
*                                                                               
OUT200   DS    0H                                                               
*                                                                               
OUT250   DS    0H                                                               
         L     R0,ADBUY            GET A(RECORD)                                
         SH    R0,=H'4'            COMPENSATE FOR REC LEN ON VARBLK             
         LA    R1,FILEOUT                                                       
         PUT   (R1),(R0)                                                        
*                                                                               
OUTX     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
CHGEND   DS    0H                                                               
         CLOSE (FILEIN,,FILEOUT)                                                
         B     EXIT                                                             
*                                                                               
*                                                                               
*                                                                               
         GETEL R2,24,ELCODE                                                     
*                                                                               
*                                                                               
BINPARMS DS    0F                                                               
BINCMND  DS    0X                                                               
BINAREC  DS    A                   A(RECORD)                                    
BINATAB  DC    A(CHGTAB)           A(TABLE)                                     
BINNUM   DC    AL4((CHGTABX-CHGTAB)/L'CHGENT)                                   
BINLREC  DC    AL4(L'CHGENT)                                                    
BINDKEY  DC    AL1(0)                                                           
BINLKEY  DC    AL3(L'OLDMKT)                                                    
BINMAXN  DC    AL4((CHGTABX-CHGTAB)/L'CHGENT)                                   
ELCODE   DS    CL1                                                              
SAVMKT   DS    XL2                 PREVIOUS MARKET                              
SAVADD   DS    A                   ADDRESS OF PREVIOUS MKT                      
ELCNT    DS    CL1                                                              
COUNT    DS    H                                                                
COUNTER  DS    H                                                                
VSORTER  DS    V(SORTER)                                                        
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         SPACE 4                                                                
CHGTAB   DS    0C                                                               
         DC    AL2(0001),AL2(0110)                                              
         DC    AL2(0010),AL2(0100)                                              
         DC    AL2(0020),AL2(0120)                                              
         DC    AL2(0030),AL2(0125)                                              
         DC    AL2(0040),AL2(0130)                                              
         DC    AL2(0050),AL2(0140)                                              
         DC    AL2(0060),AL2(0145)                                              
         DC    AL2(0070),AL2(0155)                                              
         DC    AL2(0080),AL2(0150)                                              
         DC    AL2(0090),AL2(0220)                                              
         DC    AL2(0091),AL2(0192)                                              
         DC    AL2(0094),AL2(0194)                                              
         DC    AL2(0098),AL2(0196)                                              
         DC    AL2(0140),AL2(0200)                                              
         DC    AL2(0150),AL2(0250)                                              
         DC    AL2(0160),AL2(0198)                                              
         DC    AL2(0170),AL2(0202)                                              
         DC    AL2(0180),AL2(0230)                                              
         DC    AL2(1200),AL2(0270)                                              
         DC    AL2(1210),AL2(0260)                                              
         DC    AL2(1300),AL2(0330)                                              
         DC    AL2(1350),AL2(0360)                                              
         DC    AL2(1355),AL2(0370)                                              
         DC    AL2(1500),AL2(0605)                                              
         DC    AL2(2200),AL2(0210)                                              
         DC    AL2(2300),AL2(0280)                                              
         DC    AL2(2301),AL2(0290)                                              
         DC    AL2(2310),AL2(0340)                                              
         DC    AL2(2400),AL2(0460)                                              
         DC    AL2(2410),AL2(0560)                                              
         DC    AL2(2420),AL2(0580)                                              
CHGTABX  EQU   *                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=VB,MACRF=GM,               X        
               EODAD=CHGEND                                                     
FILEOUT  DCB   DDNAME=FILEOUT,DSORG=PS,RECFM=VB,MACRF=PM,              X        
               LRECL=4004,BUFNO=2,BLKSIZE=32760                                 
         SPACE 4                                                                
MKTCHG   DSECT                         DSECT FOR MKT# CHANGES                   
         ORG                                                                    
CHGENT   DS    0AL4                                                             
OLDMKT   DS    AL2                                                              
NEWMKT   DS    AL2                                                              
*                                                                               
         SPACE 6                                                                
*                                                                               
         SPACE 6                                                                
*                                                                               
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
GKEYD    DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
       ++INCLUDE SPGENMKA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE SPGENSDEF                                                      
         EJECT                                                                  
       ++INCLUDE SPGENDOV                                                       
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'101SPREPMT02 05/01/02'                                      
         END                                                                    
