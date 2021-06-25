*          DATA SET DEBBMAVG   AT LEVEL 009 AS OF 03/19/15                      
*PROCESS USING(WARN(15))                                                        
*PHASE DEBBMAVA                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE SORTER                                                                 
*INCLUDE CARDS                                                                  
         TITLE 'MERGE/AVERAGE WEEKLY BBM DATA TO PRODUCE MONTHLY AVG'           
DEBBMAVG CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 0,DEBBMAVG,=V(REGSAVE),RA,R9                                     
         OPEN  (FILIN1,(INPUT))                                                 
         OPEN  (FILOUT,(OUTPUT))                                                
         GOTO1 =V(SORTER),DMCB,(X'80',SORTCRD),RECCRD,(X'80',0)                 
         XC    INCNT,INCNT                                                      
         XC    OUTCNT,OUTCNT                                                    
         LA    RE,IOAREA                                                        
         LH    R1,=Y(RECLENQ)                                                   
         AR    RE,R1                                                            
         ST    RE,AREC             A(SAVED RECORD)                              
         SR    R3,R3                                                            
************************************************************                    
         BAS   RE,VALPARM                                                       
************************************************************                    
SORT1    LA    R2,IOAREA                                                        
         LA    R2,4(R2)                                                         
*                                                                               
SORT2    CLI   INFLAG,X'08'                                                     
         BE    GET1                                                             
         CLI   INFLAG,X'04'                                                     
         BE    GET2                                                             
         CLI   INFLAG,X'02'                                                     
         BE    GET3                                                             
         CLI   INFLAG,X'01'                                                     
         BE    GET4                                                             
*        CLI   INFLAG,X'5'                                                      
*        BE    GET5                                                             
         DC    H'0'                                                             
*                                                                               
GET1     GET   FILIN1,IOAREA                                                    
         B     SORT5                                                            
GET2     GET   FILIN2,IOAREA                                                    
         B     SORT5                                                            
GET3     GET   FILIN3,IOAREA                                                    
         B     SORT5                                                            
GET4     GET   FILIN4,IOAREA                                                    
         B     SORT5                                                            
*ET5     GET   FILIN5,IOAREA                                                    
*        B     SORT5                                                            
*                                                                               
SORT5    CLI   0(R2),C'R'                                                       
         BE    RREC                                                             
         CLI   INFLAG,X'08'       FOR NOW ONLY USE PTRS FROM 1ST REC            
         BNE   SORT2                                                            
         BAS   RE,SETPTR                                                        
         PUT   FILOUT,IOAREA                                                    
         B     SORT2                                                            
************************************************************                    
* R2: STARTING PTR                                                              
* R3: STARTING OF THE ELEMENTS                                                  
* RE: TEMP CURRENT PTR                                                          
* R1: UTILITY                                                                   
************************************************************                    
RREC     DS    0H                                                               
*                                                                               
         USING DRKEY,R2                                                         
         MVC   SORTSTA,DRSTAT                                                   
         MVI   DRBTYP,0                                                         
         LA    R3,DRFRSTEL                                                      
RREC10   CLI   0(R3),X'20'         READ UP TO THE FIRST QHELEM                  
         BE    RREC20                                                           
         CLI   0(R3),X'00'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R3),X'01'         MARKET TYPE ELEMENT                          
         BNE   *+10                                                             
         MVC   SORTMKT(2),2(R3)    SAVE THE MARKET FOR SORT                     
         ZIC   R4,1(R3)                                                         
         AR    R3,R4                                                            
         B     RREC10                                                           
         DROP  R2                                                               
*                                                                               
RREC20   LR    R4,R3               SAVE EVERYTHING UP TO QHELEM                 
         SR    R4,R2               FROM HERE TO START OF RECORD                 
         SHI   R4,1                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   SAVEKEY(0),0(R2)    SAVE AS KEY                                  
         AHI   R4,1                                                             
         STH   R4,KEYLEN         SAVE AS LENGTH OF KEY                          
*                                                                               
         LR    R4,R3               R4 IS WHERE WE ARE NOW                       
RREC25   ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         CLI   0(R4),X'00'         END OF RECORD                                
         BE    RREC30                                                           
         CLI   0(R4),X'20'         ANOTHER QHELEM                               
         BNE   RREC25                                                           
*                                                                               
RREC30   DS    0H                                                               
         USING QHELEM,R3                                                        
         MVC   SORTDT,QHDAY        DAY/SQH/EQH                                  
         MVC   SORTWKS,INFLAG                                                   
*                                                                               
         LH    R1,KEYLEN           MOVE KEY INTO SAVEREC                        
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SAVEREC(0),SAVEKEY                                               
         LA    R1,SAVEREC+1(R1)    BUMP R1 TO END OF SAVEKEY                    
         SR    R4,R3               LENGTH OF SEGMENT                            
         SHI   R4,1                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),0(R3)       VOILA                                        
         LH    R1,KEYLEN           KEYLEN + SEGMENT LENGTH                      
         AR    R1,R4                                                            
         AHI   R1,6                +1 FOR EX +1 FOR 00 +4 FOR RECLEN            
         STCM  R1,3,SAVELEN                                                     
*                                                                               
         BAS   RE,EXPAND                                                        
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'PUT',SORTMKT                                  
         LA    RE,SAVELEN                                                       
         XCEF  (RE),1004                                                        
         LA    R3,1(R4,R3)         PT IT TO NEXT SEGMENT                        
         CLI   0(R3),X'00'         WAS THAT THE LAST SEGMENT?                   
         BE    SORT2                                                            
         LR    R4,R3                                                            
         B     RREC25                                                           
*                                                                               
************************************************************                    
         DS    0H                                                               
WEEK2    CLOSE (FILIN1,)                                                        
         OPEN  (FILIN2,(INPUT))                                                 
         MVI   INFLAG,X'04'                                                     
         B     SORT1                                                            
WEEK3    CLOSE (FILIN2,)                                                        
         CLI   FILENUM,3                                                        
         BL    GET01                                                            
         OPEN  (FILIN3,(INPUT))                                                 
         MVI   INFLAG,X'02'                                                     
         B     SORT1                                                            
WEEK4    CLOSE (FILIN3,)                                                        
         CLI   FILENUM,4                                                        
         BL    GET01                                                            
         OPEN  (FILIN4,(INPUT))                                                 
         MVI   INFLAG,X'01'                                                     
         B     SORT1                                                            
*EEK5    CLOSE (FILIN4,)                                                        
*        CLI   FILENUM,5                                                        
*        BL    GET01                                                            
*        OPEN  (FILIN5,(INPUT))                                                 
*        MVI   INFLAG,5                                                         
*        B     SORT1                                                            
ENDCPY   CLOSE (FILIN4,)                                                        
GET01    XC    SAVEKEY,SAVEKEY                                                  
*                                                                               
GET10    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   GET15                                                            
         MVI   ENDGET,1                                                         
         B     GET60                                                            
*********BE    GET90                                                            
*                                                                               
GET15    L     R3,DMCB+4                                                        
         MVC   WEEKNUM,SORTWKS-SORTMKT(R3)                                      
*                                                                               
         OC    SAVESRT,SAVESRT     FIRST TIME                                   
         BZ    GET20                                                            
*                                                                               
         CLC   SAVEKEY(L'DRKMAJOR),SAVEREC-SORTMKT(R3)                          
         BNE   GET60               KEY CHANGED                                  
*                                                                               
         CLC   SAVESRT,0(R3)       SAME MARKET/STATION/DAY+TIME?                
         BNE   GET60                                                            
*                                                                               
         CLC   SAVEPGM3,SORTPGM-SORTMKT(R3)    PROGRAM CHANGED                  
         BNE   GET40                                                            
*                                                                               
         B     GET30                                                            
************************************************************                    
GET20    XC    DEMTAB1(TABLENQ),DEMTAB1                                         
         MVI   TAB1WK,X'0'                                                      
         XC    DEMTAB2(TABLENQ),DEMTAB2                                         
         MVI   TAB2WK,X'0'                                                      
*********XC    IUVSIZE(2),IUVSIZE                                               
         MVC   SAVESRT,0(R3)                                                    
         MVC   SAVEPGM3,SORTPGM-SORTMKT(R3)                                     
         LA    R4,SORTLNQ(R3)                                                   
         LA    R5,RECLENQ                                                       
         LA    RE,SAVELEN          SAVING THE FIRST RECORD                      
         LA    RF,RECLENQ                                                       
         MVCL  RE,R4                                                            
*                                                                               
GET30    LA    R3,SAVEREC-SORTMKT(R3)                                           
         USING DRKEY,R3                                                         
         OC    TEMPWKS,WEEKNUM                                                  
         MVC   SAVEKEY(L'DRKMAJOR),0(R3)                                        
         LA    R3,DRFRSTEL                                                      
         BAS   RE,EXTRACT                                                       
*********OC    DEMTAB2(TABLENQ),DEMTAB2     2 SETS OF DEMOS?                    
*********BZ    GET10                                                            
         CLI   TAB2WK,X'1'                                                      
         BNE   GET10                                                            
         LA    R1,TAB1WK                                                        
         ST    R1,DMCB                                                          
         LA    R1,TAB2WK                                                        
         ST    R1,DMCB+4                                                        
         BAS   RE,AVERAGE                                                       
*********OC    TEMPWKS,WEEKNUM                                                  
         B     GET10                                                            
         DROP  R3                                                               
************************************************************                    
GET40    DS    0H                  PROGRAM CHANGE                               
         BAS   RE,BLDELEM                                                       
         BAS   RE,BLDREC                                                        
         MVI   TEMPWKS,0                                                        
         LA    R1,TAB3WK                                                        
         ST    R1,DMCB                                                          
         LA    R1,TAB1WK                                                        
         ST    R1,DMCB+4                                                        
         BAS   RE,AVERAGE          STORE THE OVERALL AVERAGE IN 3               
         BAS   RE,CONDENSE                                                      
         MVI   VARIOUS,C'Y'                                                     
         B     GET20                                                            
************************************************************                    
GET60    DS    0H                  QTR HOUR CHANGE                              
         BAS   RE,BLDELEM                                                       
         BAS   RE,BLDREC                                                        
         MVI   TEMPWKS,0                                                        
         BAS   RE,CONDENSE                                                      
         CLI   LOOPFLAG,1          ALREADY OUTPUTTED INDIVIDUAL PGM             
         BNE   GET70                                                            
         MVI   VARIOUS,0           NEW QTR HOUR, RESET VARIOUS FLAG             
         MVI   LOOPFLAG,0                                                       
*                                                                               
         CLI   ENDGET,1                                                         
         BE    GET90                                                            
*                                                                               
         CLC   SAVEKEY(L'DRKMAJOR),SAVEREC-SORTMKT(R3)                          
         BNE   GET80               KEY CHANGED                                  
         B     GET20                                                            
*                                                                               
GET70    LA    R1,TAB3WK                                                        
         ST    R1,DMCB                                                          
         LA    R1,TAB1WK                                                        
         ST    R1,DMCB+4                                                        
         BAS   RE,AVERAGE          STORE THE OVERALL AVERAGE IN 3               
         CLI   VARIOUS,C'Y'                                                     
         BNE   *+8                                                              
         MVI   VARIOUS,1                                                        
         MVC   DEMTAB1(TABLENQ),DEMTAB3                                         
         MVI   TAB3WK,0                                                         
         XC    DEMTAB3(TABLENQ),DEMTAB3                                         
         MVI   TEMPWKS,X'2F'                                                    
         MVI   LOOPFLAG,1                                                       
         B     GET60                                                            
************************************************************                    
GET80    OC    OUTLEN,OUTLEN       KEY CHANGED,SAVE KEY,OUTPUT LASTREC          
         BZ    GET20                                                            
         LA    RE,OUTREC                                                        
         USING DRKEY,RE                                                         
         MVC   DRHIGHD,SAVEDAY2                                                 
         MVC   DRHIQHR,SAVEQHR2                                                 
         LH    R1,OUTLEN                                                        
         SHI   R1,4                                                             
         STCM  R1,3,DRRLEN                                                      
         DROP  RE                                                               
         PUT   FILOUT,OUTLEN                                                    
         LA    RE,OUTLEN                                                        
         XCEF  (RE),1004                                                        
         B     GET20                                                            
*                                                                               
GET90    OC    OUTLEN,OUTLEN       DONE                                         
         BZ    GET100                                                           
         LA    RE,OUTREC                                                        
         USING DRKEY,RE                                                         
         MVC   DRHIGHD,SAVEDAY2                                                 
         MVC   DRHIQHR,SAVEQHR2                                                 
         LH    R1,OUTLEN                                                        
         SHI   R1,4                                                             
         STCM  R1,3,DRRLEN                                                      
         DROP  RE                                                               
         PUT   FILOUT,OUTLEN                                                    
GET100   DS    0H                                                               
         CLOSE FILOUT                                                           
         GOTO1 =V(SORTER),DMCB,=C'END'                                          
DONE     XBASE                                                                  
*                                                                               
************************************************************                    
*VALPARM                                                   *                    
************************************************************                    
VALPARM  NTR1                                                                   
VAL10    GOTO1 =V(CARDS),DMCB,PARMS,=C'RE00'                                    
         LA    R1,PARMS                                                         
         CLC   =C'/*',0(R1)                                                     
         BE    VALPX                                                            
         CLI   0(R1),C'*'          ACCEPT COMMENT CARDS                         
         BE    VAL10                                                            
*                                                                               
         CLC   =C'FILES',0(R1)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,6(R1)                                                         
         PACK  DUB,0(1,R2)                                                      
         CVB   R0,DUB                                                           
         STC   R0,FILENUM                                                       
         B     VAL10                                                            
*                                                                               
VALPX    DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
* ROUTINE IS TO EXPAND THE RECORD FROM THE 4 BYTE CONDENSED ELEMENT             
* TO ORIGINAL ELEMENTS                                                          
***********************************************************************         
EXPAND   NTR1                                                                   
         LA    R1,SAVEREC          RECORD WE ABOUT TO SORT                      
         LA    R3,TEMPREC          EXPANDED VERSION OF THE RECORD               
         LA    R4,IOAREA+4         ORIGINAL LONG RECORD                         
*                                                                               
         USING DRKEY,R1                                                         
         MVC   TEMPREC(DRFRSTEL-DRKEY),0(R1)    SAVE THE KEY FIRST              
         LA    R3,TEMPREC+DRFRSTEL-DRKEY                                        
         LA    R1,DRFRSTEL                                                      
*                                                                               
EXP10    CLI   0(R1),0             EOL                                          
         BE    EXP40                                                            
         CLI   1(R1),4             4 BYTE AND X'80'                             
         BNE   *+12                                                             
         TM    2(R1),X'80'         CONDENSED ELEMENT                            
         BO    EXP20                                                            
         CLI   0(R1),X'20'                                                      
         BE    EXP30                                                            
EXP15    ZIC   RE,1(R1)            STORE ELEMENT THAT IS NOT SHORTENED          
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R1)                                                    
         LA    R1,1(RE,R1)                                                      
         LA    R3,1(RE,R3)                                                      
         B     EXP10                                                            
*                                                                               
EXP20    MVC   HALF,2(R1)          GET THE DISPLACE OF ORIGINAL ELEM            
         NI    HALF,255-X'80'                                                   
         ICM   RE,3,HALF                                                        
         AR    R4,RE               POINT TO IT IN THE ORIGINAL RECORD           
         CLC   0(1,R4),0(R1)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RE,1(R4)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R4)       SAVE IT                                      
         LA    R1,4(R1)            BUMP SAVEREC POINTER                         
         LA    R3,1(RE,R3)         BUMP TEMPREC POINTER                         
         LA    R4,IOAREA+4         RESET R4                                     
         B     EXP10                                                            
*                                                                               
EXP30    CLI   1(R1),6             SHORTENED QHELEM?                            
         BE    EXP35                                                            
         XC    SVPGMLN(15),SVPGMLN                                              
         ZIC   RE,1(R1)                                                         
         SHI   RE,6                                                             
         STC   RE,SVPGMLN                                                       
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEPGM(0),6(R1)  SAVE THE PROGRAM NAME                          
         MVC   SORTPGM,SAVEPGM                                                  
         B     EXP15                                                            
*                                                                               
EXP35    MVC   0(6,R3),0(R1)                                                    
         ZIC   RE,SVPGMLN                                                       
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R3),SAVEPGM                                                  
         MVC   SORTPGM,SAVEPGM                                                  
         AHI   RE,7                PGMNAME+ELEM HEADER+1 FOR EX                 
         STC   RE,1(R3)                                                         
         AR    R3,RE               TEMPREC PTR UPDATE                           
         LA    R1,6(R1)            IOAREA PTR UPDATE                            
         B     EXP10                                                            
*                                                                               
EXP40    LA    RE,TEMPREC                                                       
         SR    R3,RE                                                            
         AHI   R3,5                                                             
         STCM  R3,3,SAVELEN                                                     
*                                                                               
         LA    R2,TEMPREC                                                       
         LA    RE,SAVEREC          SAVING THE FIRST RECORD                      
         LR    RF,R3                                                            
         MVCL  RE,R2                                                            
*                                                                               
         LA    RE,TEMPREC                                                       
         XCEF  (RE),1000                                                        
         XIT1                                                                   
***********************************************************************         
* EXTRACTING THE NUMBERS FROM A RECORD, AND STORING IT TO A TABLE               
***********************************************************************         
EXTRACT  NTR1                                                                   
         LA    R1,DEMTAB1                                                       
         CLI   TAB1WK,0                                                         
         BNE   *+12                                                             
         MVI   TAB1WK,1                                                         
         B     EXTR10                                                           
         LA    R1,DEMTAB2                                                       
         MVI   TAB2WK,X'1'                                                      
*                                                                               
EXTR10   CLI   0(R3),X'31'         STORE THE UNIVERSE                           
         BE    EXTR20                                                           
         CLI   0(R3),X'33'                                                      
         BNE   EXTR15                                                           
         LA    R0,DEMTAB2          WHICH TABLE ARE WE ON                        
         CR    R1,R0                                                            
         BNL   *+12                                                             
         LA    R1,DEMTAB1+X31LENQ  BUMP TO IMPRESSIONS                          
         B     EXTR20                                                           
         LA    R1,DEMTAB2+X31LENQ  DEMTAB2 IMPRESSIONS                          
         B     EXTR20                                                           
EXTR15   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'00'                                                      
         BE    EXTRX                                                            
         B     EXTR10                                                           
*                                                                               
EXTR20   DS    0H                                                               
         ZIC   RE,2(R3)            ISOLATE THE CELL LENGTH                      
*                                                                               
         MVC   DUB(1),2(R3)        NO - EXTRACT CONTROL/LENGTH                  
         NI    DUB,X'F8'                                                        
         MVC   DUB+1(1),2(R3)                                                   
         NI    DUB+1,X'07'                                                      
*                                                                               
         CLI   0(R3),X'31'                                                      
         BNE   *+12                                                             
         STC   RE,IUVSIZE          SAVE THE CELL SIZE                           
         B     *+8                                                              
         STC   RE,IQVSIZE                                                       
         SLL   RE,28                                                            
         SRL   RE,28                                                            
*                                                                               
         LA    R2,4                R2: ARITHMETIC                               
         SR    R2,RE                                                            
         BCTR  RE,0                FOR EXECUTE                                  
         ZIC   R5,1(R3)                                                         
         AR    R5,R3                                                            
         LA    R6,3(R3)            R6: ELEMENT PTR                              
EXTR30   LA    R4,FULL1            MOVING EACH CELL INTO FULL1 FIRST            
         AR    R4,R2                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R6)                                                    
*                                                                               
         TM    DUB,X'40'           IF NOT IMPLIED DECIMAL                       
         BO    EXTR40                                                           
         L     R4,FULL1                                                         
         MHI   R4,10               MULTIPLY BY 10                               
         ST    R4,FULL1                                                         
*                                                                               
EXTR40   MVC   0(4,R1),FULL1       STORE IT INTO TABLE                          
         XC    FULL1,FULL1                                                      
         LA    R6,1(RE,R6)         BUMP THE ELEM PTR                            
         CR    R6,R5                                                            
         BNL   EXTR90                                                           
         LA    R1,4(R1)            BUMP THE TABLE PTR                           
         B     EXTR30                                                           
*                                                                               
EXTR90   ZIC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R3),X'33'                                                      
         BH    EXTRX                                                            
         B     EXTR10              PROCESS THE IMPRESSIONS                      
EXTRX    XIT1                                                                   
***********************************************************************         
SETPTR   NTR1                                                                   
         CLI   0(R2),C'M'                                                       
         BNE   SETPTR20                                                         
*                                                                               
         USING BSKEY,R2                                                         
         CLI   BSIND,BSINDEQU                                                   
         BNE   SETPTR10                                                         
         MVI   BSBTYP,X'00'                                                     
         B     SETPTRX                                                          
         DROP  R2                                                               
*                                                                               
         USING MLKEY,R2                                                         
SETPTR10 DS    0H                                                               
         MVI   MLBTYP,X'00'                                                     
         B     SETPTRX                                                          
         DROP  R2                                                               
*                                                                               
         USING SBKEY,R2                                                         
SETPTR20 DS    0H                                                               
         MVI   SBBTYP,X'00'                                                     
         DROP  R2                                                               
SETPTRX  XIT1                                                                   
***********************************************************************         
* "AVERAGE" TAKES 2 TABLES OF NUMBERS.  EACH TABLE IS PRECEDED BY A             
* MULTIPLIER.  STATING HOW MANY WEEKS WERE CONTRIBUTED INTO THE AVG'S           
* THAT ARE IN THE TABLE. FINAL AVERAGE BETWEEN THE TWO TABLES ARE               
* CALCULATED BY :                                                               
*               TAB1 = (TAB1 * X + TAB2 * Y)/(X+Y)                              
* THEN X+Y IS STORED INTO FINAL RESULT AS THE MULTIPLIER FOR FUTURE             
* CALCULATIONS                                                                  
***********************************************************************         
AVERAGE  NTR1                                                                   
         L     R1,DMCB                                                          
         L     R3,DMCB+4                                                        
         ZIC   RE,0(R1)                                                         
         ST    RE,DIVISOR1         NUMBER OF WEEKS OF 1ST PGM                   
         ZIC   RE,0(R3)                                                         
         ST    RE,DIVISOR2         NUMBER OF WEEKS OF 2ND PGM                   
         A     RE,DIVISOR1                                                      
         ST    RE,DIVISOR3         OVERALL WEEKS                                
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
*                                                                               
AVG10    MVC   FULL1,0(R1)                                                      
         MVC   FULL2,0(R3)                                                      
         L     RF,FULL1                                                         
         SR    RE,RE               HOW MANY CONTRIBUTED TO THE AVERAGE          
         M     RE,DIVISOR1         RESTORE TO THE SUM                           
*                                                                               
         L     R5,FULL2                                                         
         SR    R4,R4                                                            
         M     R4,DIVISOR2                                                      
*                                                                               
         AR    RF,R5               ADD THE NEW NUMBER                           
         SR    RE,RE                                                            
         D     RE,DIVISOR3         AVERAGE                                      
         ST    RF,FULL1                                                         
         MVC   0(4,R1),FULL1                                                    
         LA    R1,4(R1)                                                         
         LA    R3,4(R3)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   AVG10                                                            
*                                                                               
         XC    DEMTAB2(TABLENQ),DEMTAB2                                         
         L     R1,DMCB                                                          
         L     RE,DIVISOR3                                                      
         STC   RE,0(R1)                                                         
*                                                                               
AVGX     XIT1                                                                   
***********************************************************************         
SETCNTRL NTR1                                                                   
         OI    IUVSIZE,X'40'                                                    
         OI    IQVSIZE,X'40'                                                    
         LA    R2,IUVSIZE                                                       
         SR    RE,RE                                                            
         LA    R1,IUVM2O                                                        
SETC05   LA    R7,0(R1)                                                         
         AHI   R7,X31LENQ                                                       
         OC    0(X31LENQ,R1),0(R1) MAKE SURE UNIV TABLE'S NOT EMPTY             
         BNZ   SETC10                                                           
SETC07   AHI   R1,X31LENQ                                                       
SETC08   CLI   0(R1),X'FF'                                                      
         BE    SETCX                                                            
         LA    R2,IQVSIZE                                                       
         SR    RE,RE                                                            
         B     SETC05                                                           
*                                                                               
SETC10   L     R4,0(R1)                                                         
         CR    R4,RE                                                            
         BNH   SETC20                                                           
         LR    RE,R4                                                            
*                                                                               
         XR    RF,RF                                                            
         LA    R3,1                                                             
SETC15   SRDL  RE,8                                                             
         LTR   RE,RE                                                            
         BZ    *+12                                                             
         AHI   R3,1                                                             
         B     SETC15                                                           
         ZIC   RE,0(R2)                                                         
         SRL   RE,4                                                             
         SLL   RE,4                                                             
         OR    RE,R3                                                            
         STC   RE,0(R2)                                                         
         LR    RE,R4                                                            
*                                                                               
SETC20   LA    R1,4(R1)                                                         
         CR    R1,R7                                                            
         BL    SETC10                                                           
         B     SETC08                                                           
*                                                                               
SETCX    XIT1                                                                   
***********************************************************************         
* "BLDELEM" BUILDS TWO DEMO ELEMENTS USING THE NUMBERS STORED INSIDE            
*  OF DEMTAB1.                                                                  
***********************************************************************         
BLDELEM NTR1                                                                    
         BAS   RE,SETCNTRL         SET THE ELEM CONTROL BYTE                    
*                                                                               
         LA    R1,IUVM2O            UNIVERSE FIRST                              
         LA    R3,ELEM                                                          
         XC    ELEM,ELEM                                                        
*                                                                               
         LA    R7,IQVM2O                                                        
         OC    IUVM2O(X31LENQ),IUVM2O                                           
         BZ    BLDEL15                                                          
BLDEL10  MVI   0(R3),X'31'                                                      
         LA    RE,3                 ELEMENT LENGTH COUNTER                      
         ZIC   R6,IUVSIZE                                                       
         STC   R6,2(R3)                                                         
         LA    R2,3(R3)                                                         
         B     BLDEL20                                                          
BLDEL15  OC    IQVM2O(X31LENQ),IQVM2O                                           
         BZ    BLDELX                                                           
         AHI   R7,X31LENQ           IMPRESSION MARKER                           
         LA    R1,IQVM2O                                                        
         MVI   0(R3),X'33'                                                      
         LA    RE,3                                                             
         ZIC   R6,IQVSIZE                                                       
         STC   R6,2(R3)                                                         
         LA    R2,3(R3)                                                         
*                                                                               
BLDEL20  SLL   R6,28                                                            
         SRL   R6,28                                                            
*                                                                               
BLDEL30  L     R4,0(R1)                                                         
*                                                                               
         CHI   R6,1                                                             
         BNE   *+8                                                              
         STCM  R4,1,0(R2)                                                       
         CHI   R6,2                                                             
         BNE   *+8                                                              
         STCM  R4,3,0(R2)                                                       
         CHI   R6,3                                                             
         BNE   *+8                                                              
         STCM  R4,7,0(R2)                                                       
         CHI   R6,4                                                             
         BNE   *+8                                                              
         STCM  R4,15,0(R2)                                                      
*                                                                               
         LA    R1,4(R1)                                                         
         AR    RE,R6                LENGTH + CELLSIZE                           
         CR    R1,R7                END OF UNIVERSE/IMPRESSION?                 
         BE    BLDEL40                                                          
         AR    R2,R6                HARD-CODED PRECISION                        
         B     BLDEL30                                                          
*                                                                               
BLDEL40  SHI   R6,1                 CUT THE SLACK                               
         EX    R6,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),0(R2)        UNTIL THERE IS SUBSTANCE                    
         BNZ   BLDEL50                                                          
         AHI   R6,1                                                             
         SR    RE,R6                UPDATE SHORTENED LENGTH                     
         SR    R2,R6                                                            
         B     BLDEL40                                                          
*                                                                               
BLDEL50  STC   RE,1(R3)                                                         
         AR    R3,RE                                                            
         CLI   0(R7),X'FF'         END OF TABLE?                                
         BNE   BLDEL15                                                          
*                                                                               
BLDELX   MVC   0(L'X5EELEM,R3),X5EELEM                                          
         XIT1                                                                   
***********************************************************************         
BLDREC   NTR1                                                                   
         LA    R4,SAVELEN                                                       
         LA    R5,RECLENQ                                                       
         LA    RE,IOAREA                                                        
         LA    RF,RECLENQ                                                       
         MVCL  RE,R4                                                            
         LA    RE,SAVELEN                                                       
         XCEF  (RE),1004                                                        
         LA    R2,SAVEREC                                                       
*                                                                               
         LA    R1,IOAREA+4                                                      
         USING DRKEY,R1                                                         
         MVI   DRBTYP,X'00'                                                     
         MVC   0(DRFRSTEL-DRKEY,R2),DRKEY                                       
         AHI   R2,DRFRSTEL-DRKEY                                                
         LA    R1,DRFRSTEL                                                      
BLDR20   ZIC   RE,1(R1)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         LA    R2,1(RE,R2)                                                      
BLDR23   LA    R1,1(RE,R1)                                                      
         CLI   0(R1),0                                                          
         BNE   BLDR23A                                                          
         CLC   X5EELEM,ELEM                                                     
         BE    BLDRX                                                            
         B     BLDR25                                                           
************************************************************                    
BLDR23A  CLI   0(R1),X'20'                                                      
         BNE   BLDR25                                                           
*                                                                               
         CLI   FILENUM,3                                                        
         BE    BLDR24A                                                          
         CLI   FILENUM,2                                                        
         BE    BLDR24B                                                          
*                                                                               
         CLI   TEMPWKS,X'07'      0111                                          
         BE    *+8                                                              
         CLI   TEMPWKS,X'0B'      1011                                          
         BE    *+8                                                              
         CLI   TEMPWKS,X'0D'      1101                                          
         BE    *+8                                                              
         CLI   TEMPWKS,X'0E'      1110                                          
         BNE   *+8                                                              
         OI    TEMPWKS,X'20'                                                    
         B     BLDR24X                                                          
*                                                                               
BLDR24A  CLI   TEMPWKS,X'0C'      1100                                          
         BE    *+8                                                              
         CLI   TEMPWKS,X'06'      0110                                          
         BE    *+8                                                              
         CLI   TEMPWKS,X'0A'      1010                                          
         BNE   *+8                                                              
         OI    TEMPWKS,X'20'                                                    
         B     BLDR24X                                                          
*                                                                               
BLDR24B  CLI   TEMPWKS,X'0C'      1100                                          
         BNE   *+8                                                              
         OI    TEMPWKS,X'20'                                                    
*                                                                               
BLDR24X  MVC   5(1,R1),TEMPWKS                                                  
         CLI   VARIOUS,1           DO WE NEED TO CHANGE PROGRAM NAME?           
         BNE   BLDR20                                                           
         MVC   0(6,R2),0(R1)                                                    
         MVI   1(R2),13            HARDCODED LENGTH                             
         MVC   6(7,R2),=C'VARIOUS'                                              
         MVI   VARIOUS,C'Y'                                                     
         LA    R2,13(R2)                                                        
         ZIC   RE,1(R1)                                                         
         SHI   RE,1                                                             
         B     BLDR23                                                           
************************************************************                    
*                                                                               
BLDR25   LA    R1,ELEM                                                          
BLDR30   ZIC   RE,1(R1)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R1)                                                    
         LA    R2,1(RE,R2)                                                      
         LA    R1,1(RE,R1)                                                      
         CLI   0(R1),0                                                          
         BNE   BLDR30                                                           
         XC    ELEM,ELEM                                                        
*                                                                               
BLDRX    LA    RE,SAVELEN                                                       
         SR    R2,RE                                                            
         AHI   R2,1                                                             
         STCM  R2,3,SAVELEN                                                     
         XIT1                                                                   
***********************************************************************         
* "CONDENSE" WILL KEEP ON CONDENSING SHORT RECORDS INTO THE LONG OUTREC         
*  AS LONG AS THE MAJOR KEY REMAINS THE SAME AND THE RECORD LENGTH IS           
*  UNDER THE MAXIMUM LENGTH ALLOWED.                                            
***********************************************************************         
CONDENSE NTR1                                                                   
COND01   LA    R1,OUTREC                                                        
OR       USING DRKEY,R1                                                         
         LA    R2,SAVEREC                                                       
SR       USING DRKEY,R2                                                         
*                                                                               
         OC    OUTLEN,OUTLEN       FIRST RECORD IN THE OUTREC?                  
         BNZ   COND10                                                           
         MVI   PUTSW,C'Y'                                                       
         LA    R4,SAVELEN          SAVE THE ENTIRE RECORD                       
         LA    R5,RECLENQ+4                                                     
         LA    RE,OUTLEN                                                        
         LA    RF,RECLENQ+4                                                     
         MVCL  RE,R4                                                            
*                                                                               
         LA    R4,OR.DRFRSTEL      SAVE ADDRESS OF FRSTEL                       
         ST    R4,AFRSTEL                                                       
*                                                                               
COND05   ZIC   RE,1(R4)            SAVE PROGRAM NAME                            
         AR    R4,RE                                                            
         CLI   0(R4),X'20'                                                      
         BNE   COND05                                                           
         USING QHELEM,R4                                                        
         XC    SAVEPGM,SAVEPGM                                                  
         ZIC   RE,QHELN                                                         
         SHI   RE,6+1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEPGM(0),QHPNAME                                               
         B     CONDX                                                            
         DROP  R4                                                               
*                                                                               
COND10   LA    R4,SR.DRFRSTEL                                                   
         USING QHELEM,R4                                                        
COND15   CLI   0(R4),X'20'                                                      
         BE    COND20                                                           
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         B     COND15                                                           
*                                                                               
COND20   MVC   SAVEDAY,QHDAY                                                    
         MVC   SAVEQHR,QHEQH                                                    
         XC    SAVEPGM2,SAVEPGM2                                                
         ZIC   RE,QHELN                                                         
         SHI   RE,6+1                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SAVEPGM2(0),QHPNAME                                              
         CLC   SAVEPGM2,SAVEPGM                                                 
         BNE   *+12                 IS IT A NEW PGM                             
         LA    RE,6                 NO: SHORTEN LENGTH TO 6                     
         B     COND40                                                           
         MVC   SAVEPGM,SAVEPGM2     NEW PROGRAM NAME                            
COND30   ZIC   RE,1(R4)             NEW LENGTH                                  
*                                                                               
COND40   SHI   RE,1                                                             
         XC    ELEM,ELEM                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ELEM(0),0(R4)                                                    
         AHI   RE,1                UPDATED ELEMENT LENGTH                       
         STC   RE,ELEM+1                                                        
*                                                                               
         CLI   ELEM,X'20'                                                       
         BE    *+8                                                              
         BAS   RE,CHKDUP           CHECK FOR DUPLICATE AND UPDATE ELEM          
*                                                                               
         LA    RE,ELEM             FROM: ELEM                                   
         ST    RE,DMCB                                                          
         LA    RE,TEMPREC          TO: TEMPREC                                  
         ST    RE,DMCB+4                                                        
         BAS   RE,PUTELEM                                                       
         ZIC   RE,1(R4)                                                         
         AR    R4,RE                                                            
         CLI   0(R4),0             END OF RECORD?                               
         BNE   COND30                                                           
*                                                                               
COND50   CLI   PUTSW,C'N'          MAX LENGTH REACHED?                          
         BNE   COND60                                                           
*                                                                               
         LA    RE,OUTREC                                                        
         MVC   OR.DRHIGHD,SAVEDAY2                                              
         MVC   OR.DRHIQHR,SAVEQHR2                                              
         LH    R6,OUTLEN                                                        
         SHI   R6,4                                                             
         STCM  R6,3,OR.DRRLEN                                                   
*********MVC   OR.DRRLEN,OUTLEN                                                 
         PUT   FILOUT,OUTLEN       MAXLENGTH OF RECORD REACHED                  
         MVC   SAVEKEY(L'DRKMAJOR),OUTREC                                       
         LA    RE,OUTLEN                                                        
         XCEF  (RE),1004                                                        
         LA    RE,TEMPLEN                                                       
         XCEF  (RE),1004                                                        
         B     COND01              RE-DO THIS ONE                               
*                                                                               
COND60   LA    RE,TEMPREC          FROM : TEMPREC                               
         ST    RE,DMCB                                                          
         LH    RE,OUTLEN           TO : UPDATED PTR IN OUTREC                   
         LA    RE,OUTLEN(RE)                                                    
         SHI   RE,1                                                             
         ST    RE,DMCB+4                                                        
         BAS   RE,PUTELEM                                                       
         LH    RE,TEMPLEN                                                       
         STCM  RE,3,OUTLEN                                                      
         LA    RE,TEMPLEN                                                       
         XCEF  (RE),1004                                                        
         MVC   SAVEDAY2,SAVEDAY                                                 
         MVC   SAVEQHR2,SAVEQHR                                                 
*                                                                               
CONDX    XIT1                                                                   
***********************************************************************         
CHKDUP   NTR1                                                                   
         LA    R1,ELEM                                                          
         L     R2,AFRSTEL                                                       
CHKD10   CLI   0(R2),0                                                          
         BE    CHKDX                                                            
         ZIC   RE,1(R2)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R2),0(R1)                                                    
         BE    *+12                                                             
         LA    R2,1(R2,RE)                                                      
         B     CHKD10                                                           
*                                                                               
         XC    ELEM+1(L'ELEM-1),ELEM+1                                          
         MVI   1(R1),4                                                          
         S     R2,=A(OUTREC)                                                    
         STCM  R2,3,2(R1)                                                       
         OI    2(R1),X'80'                                                      
*                                                                               
CHKDX    XIT1                                                                   
***********************************************************************         
PUTELEM  NTR1                                                                   
         L     R1,DMCB+4                                                        
         L     R3,DMCB                                                          
         LR    R2,R1                                                            
PUTEL10  CLI   0(R2),0                                                          
         BE    PUTEL20                                                          
         ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     PUTEL10                                                          
*                                                                               
PUTEL20  ZIC   RE,1(R3)                                                         
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
         LA    R2,1(R2,RE)                                                      
         LA    R3,1(R3,RE)                                                      
         CLI   0(R3),0                                                          
         BNE   PUTEL20                                                          
*                                                                               
PUTEL30  MVI   0(R2),0                                                          
*                                                                               
         LA    RE,0(R1)        ONLY MATTERS IF IT'S PUT INTO TEMPREC            
         SR    R2,RE           CHECK TO SEE IF IT WILL FIT INTO OUTREC          
         AH    R2,OUTLEN                                                        
         STCM  R2,3,TEMPLEN                                                     
         CH    R2,MAXLEN                                                        
         BNH   PUTELX                                                           
         MVI   PUTSW,C'N'                                                       
*                                                                               
PUTELX   XC    ELEM,ELEM                                                        
         XIT1                                                                   
***********************************************************************         
         EJECT                                                                  
FILIN1   DCB   DDNAME=FILIN1,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=WEEK2,LRECL=2000,BLKSIZE=0                                 
FILIN2   DCB   DDNAME=FILIN2,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=WEEK3,LRECL=2000,BLKSIZE=0                                 
FILIN3   DCB   DDNAME=FILIN3,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=WEEK4,LRECL=2000,BLKSIZE=0                                 
FILIN4   DCB   DDNAME=FILIN4,DSORG=PS,RECFM=VB,MACRF=(GM),             X        
               EODAD=ENDCPY,LRECL=2000,BLKSIZE=0                                
*ILIN5   DCB   DDNAME=FILIN5,DSORG=PS,RECFM=VB,MACRF=(GM),                      
*              EODAD=ENDCPY,LRECL=2000,BLKSIZE=0                                
FILOUT   DCB   DDNAME=FILOUT,DSORG=PS,RECFM=VB,MACRF=(PM),             X        
               LRECL=2000,BLKSIZE=8200                                          
*                                                                               
SORTCRD  DC    CL80'SORT FIELDS=(1,25,A),FORMAT=BI'                             
RECCRD   DC    CL80'RECORD TYPE=F,LENGTH=401'                                   
*                                                                               
IUVSIZE  DS    XL1                 X'43'                                        
IQVSIZE  DS    XL1                 X'42'                                        
TAB1WK   DC    X'0'                                                             
DEMTAB1  DS    0XL4                                                             
* X'31' ELEMENT                                                                 
IUVM2O   DS    XL4                                                              
IUVM12O  DS    XL4                                                              
IUVM18O  DS    XL4                                                              
IUVM25O  DS    XL4                                                              
IUVM35O  DS    XL4                                                              
IUVM50O  DS    XL4                                                              
IUVM55O  DS    XL4                                                              
IUVM60O  DS    XL4                                                              
IUVM65O  DS    XL4                                                              
IUVW2O   DS    XL4                                                              
IUVW12O  DS    XL4                                                              
IUVW18O  DS    XL4                                                              
IUVW25O  DS    XL4                                                              
IUVW35O  DS    XL4                                                              
IUVW50O  DS    XL4                                                              
IUVW55O  DS    XL4                                                              
IUVW60O  DS    XL4                                                              
IUVW65O  DS    XL4                                                              
X31LENQ  EQU   *-DEMTAB1                                                        
* X'33' ELEMENT                                                                 
IQVM2O   DS    XL4                                                              
IQVM12O  DS    XL4                                                              
IQVM18O  DS    XL4                                                              
IQVM25O  DS    XL4                                                              
IQVM35O  DS    XL4                                                              
IQVM50O  DS    XL4                                                              
IQVM55O  DS    XL4                                                              
IQVM60O  DS    XL4                                                              
IQVM65O  DS    XL4                                                              
IQVW2O   DS    XL4                                                              
IQVW12O  DS    XL4                                                              
IQVW18O  DS    XL4                                                              
IQVW25O  DS    XL4                                                              
IQVW35O  DS    XL4                                                              
IQVW50O  DS    XL4                                                              
IQVW55O  DS    XL4                                                              
IQVW60O  DS    XL4                                                              
IQVW65O  DS    XL4                                                              
TABLENQ  EQU   *-DEMTAB1                                                        
         DC    X'FF'                                                            
TAB2WK   DC    X'0'                                                             
DEMTAB2  DS    0XL4                                                             
         DS    XL(X31LENQ)                                                      
         DS    XL(X31LENQ)                                                      
         DC    X'FF'                                                            
TAB3WK   DC    X'0'                                                             
DEMTAB3  DS    0XL4                                                             
         DS    XL(X31LENQ)                                                      
         DS    XL(X31LENQ)                                                      
         DC    X'FF'                                                            
*                                                                               
SVPGMLN  DS    XL1                                                              
SAVEPGM  DS    CL14                                                             
SAVEPGM2 DS    CL14                                                             
SAVEPGM3 DS    CL14                                                             
SAVEDAY  DS    XL1                                                              
SAVEDAY2 DS    XL1                                                              
SAVEQHR  DS    XL1                                                              
SAVEQHR2 DS    XL1                                                              
AFRSTEL  DS    A                                                                
SAVEKEY  DS    CL50                                                             
TEMPKEY  DS    XL18                                                             
SAVESRT  DS    XL10                                                             
ELEM     DS    CL256                                                            
KEYLEN   DS    H                                                                
DIVISOR1 DS    F                                                                
DIVISOR2 DS    F                                                                
DIVISOR3 DS    F                                                                
ENDGET   DC    X'0'                                                             
INFLAG   DC    X'08'               WEEK 1                                       
LOOPFLAG DS    X                                                                
VARIOUS  DC    X'0'                                                             
PUTSW    DC    C'Y'                                                             
MAXLEN   DC    H'1004'                                                          
PARMS    DS    CL80                FOR CARDS                                    
FILENUM  DS    XL1                                                              
WEEKNUM  DC    X'0'                                                             
TEMPWKS  DC    X'0'                                                             
X5EELEM  DC    X'5E07E3E6C16823'                                                
*                                                                               
INCNT    DC    F'0'                                                             
OUTCNT   DC    F'0'                                                             
DUB      DS    D                                                                
HALF     DS    H                                                                
FULL1    DS    F                                                                
FULL2    DS    F                                                                
DMCB     DS    6F                                                               
WORK     DS    CL80                                                             
AREC     DS    F                                                                
         SPACE 2                                                                
         LTORG                                                                  
         DS    F                                                                
RECLENQ  EQU   1000                                                             
IOAREA   DS    XL4                                                              
         DS    CL(RECLENQ)                                                      
SORTMKT  DS    XL2                                                              
SORTSTA  DS    CL5                                                              
SORTDT   DS    XL3                 START DATE, START QH, END QH                 
SORTPGM  DS    CL14                PROGRAM NAME                                 
SORTWKS  DS    XL1                                                              
SORTLNQ  EQU   *-SORTMKT                                                        
SAVELEN  DS    XL4                                                              
SAVEREC  DS    CL(RECLENQ)                                                      
OUTLEN   DS    XL4                                                              
OUTREC   DS    CL(RECLENQ)                                                      
TEMPLEN  DS    XL4                                                              
TEMPREC  DS    CL(RECLENQ)                                                      
         EJECT                                                                  
* DEDBLOCK                                                                      
* DEDEMFILE                                                                     
         PRINT OFF                                                              
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* DEDEMFILE                                                                     
       ++INCLUDE DEDEMFILE                                                      
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009DEBBMAVG  03/19/15'                                      
         END                                                                    
