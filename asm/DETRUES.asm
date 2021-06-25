*          DATA SET DETRUES    AT LEVEL 079 AS OF 07/24/08                      
*PHASE DETRUESA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE DEMTIME                                                                
*INCLUDE TIMVAL                                                                 
*INCLUDE UNTIME                                                                 
*INCLUDE REGSAVE                                                                
*INCLUDE PRINTER                                                                
*INCLUDE NETWEEK                                                                
*INCLUDE GETDAY                                                                 
*INCLUDE ADDAY                                                                  
TAPECOPY CSECT                                                                  
         PRINT NOGEN                                                            
         NBASE 500,TAPECOPY,=V(REGSAVE),R6,R7                                   
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         L     RE,=A(PROGBUFF)                                                  
         L     RF,=F'120000'                                                    
         XCEF                                                                   
         L     RE,=A(PROGBUF2)                                                  
         L     RF,=F'200000'                                                    
         XCEF                                                                   
*                                                                               
         OPEN  (INA1,(INPUT))                                                   
         MVI   GETSW,1                                                          
         OPEN  (OUT,(OUTPUT))                                                   
         GET   INA1,INREC          BYPASSING TWO RECORDS                        
         GET   INA1,INREC                                                       
GET      MVI   CLEARS,C' '                                                      
         MVC   CLEARS+1(CLEARE-CLEARS-1),CLEARS                                 
         LA    RE,INREC                                                         
         LA    RF,1204                                                          
         XCEF                                                                   
         GET   INA1,INREC                                                       
*                                                                               
         XC    TVNNAME(TVNQ),TVNNAME                                            
*                                                                               
         LA    R1,INREC+4                                                       
         MVC   TVNYEAR,0(R1)                                                    
         LA    R1,L'TVNYEAR+1(R1)                                               
         MVC   TVNNET,=CL4' '                                                   
*                                                                               
NET15    LA    R8,TVNNET                                                        
         MVI   TEMPCHAR,C','                                                    
*                                                                               
NET20    CLC   0(1,R1),TEMPCHAR    LOOKING FOR EITHER , OR "                    
         BE    NET30                                                            
         MVC   0(1,R8),0(R1)       MOVE NAME IN ONE CHAR AT A TIME              
         LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         B     NET20                                                            
*                                                                               
NET30    LA    R1,1(R1)                                                         
*                                                                               
*        MVC   TVNNET,0(R1)                                                     
*        LA    R1,L'TVNNET+1(R1)                                                
*        MVC   TVNNUM,0(R1)                                                     
*        LA    R1,L'TVNNUM+1(R1)                                                
         MVC   TVNNUM,=CL6' '                                                   
*                                                                               
NUM15    LA    R8,TVNNUM                                                        
         MVI   TEMPCHAR,C','                                                    
*                                                                               
NUM20    CLC   0(1,R1),TEMPCHAR    LOOKING FOR EITHER , OR "                    
         BE    NUM30                                                            
         MVC   0(1,R8),0(R1)       MOVE NAME IN ONE CHAR AT A TIME              
         LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         B     NUM20                                                            
*                                                                               
NUM30    LA    R1,1(R1)                                                         
*                                                                               
NAME10   MVI   TEMPCHAR,C','                                                    
         CLI   0(R1),C'"'                                                       
         BNE   NAME15                                                           
         MVI   TEMPCHAR,C'"'                                                    
         LA    R1,1(R1)                                                         
*                                                                               
NAME15   LA    R8,TVNNAME+1        FIRST BYTE OF TVNNAME IS LENGTH              
*                                                                               
NAME20   CLC   0(1,R1),TEMPCHAR    LOOKING FOR EITHER , OR "                    
         BE    NAME30                                                           
         MVC   0(1,R8),0(R1)       MOVE NAME IN ONE CHAR AT A TIME              
         LA    R1,1(R1)                                                         
         LA    R8,1(R8)                                                         
         B     NAME20                                                           
*                                                                               
NAME30   LA    RE,TVNNAME+1                                                     
         SR    R8,RE                                                            
         STC   R8,TVNNAME                                                       
         CLI   0(R1),C'"'          BYPASS THIS - STILL A COMMA                  
         BNE   *+8                  SEPARATING FIELDS                           
         LA    R1,1(R1)                                                         
*                                                                               
FILLTAB  DS    0H                                                               
****************************************************************                
* R1: POINT AT THE DEMO NUMBERS IN INREC                       *                
* R2: POINT AT DEMO DISPLACEMENT TABLE                         *                
* R3: TEMPORARY POINTER ON-MOVING WITH LOOP.                   *                
* R4: TEMPORARY BINARY STORAGE BEFORE ENTERED INTO TABLE       *                
* RE: TEMPORARY CALCULATION REGISTER                           *                
****************************************************************                
FILL10   LA    R1,1(R1)            POINT TO THE NUMBERS                         
         LR    R3,R1                                                            
         LA    R2,TABDEF           POINT TO DISPLACEMENT TABLE                  
         SR    RE,RE               CLEAR CELL LENGTH COUNTER                    
FILL20   CLI   0(R3),C','                                                       
         BE    FILL30                                                           
         CLI   0(R3),0                                                          
         BE    FILL30                                                           
         LA    RE,1(RE)            BUMP COUNTER                                 
         LA    R3,1(R3)                                                         
         B     FILL20                                                           
*                                                                               
FILL30   DS    0H                  EX PACK                                      
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R4,DUB                                                           
         AR    R1,RE               AFTER PACK, POINT TO NEXT CELL               
         LA    R1,2(R1)            RE WAS SUBTRACTED BY 1                       
         LA    R3,DNUMS            POINT TO STORAGE                             
         ZIC   R0,0(R2)                                                         
         AR    R3,R0               ADD DISPLACEMENT                             
         SHI   R3,1                                                             
         STC   R4,0(R3)                                                         
         LA    R2,1(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BE    FILLX                                                            
         SR    RE,RE                                                            
         LA    R3,0(R1)                                                         
         B     FILL20                                                           
*                                                                               
FILLX    DS    0C                                                               
         MVI   TVNQTR,0                                                         
*                                                                               
         EJECT                                                                  
         SPACE 1                                                                
OUTREC1  DS    0H                                                               
         ZIC   R1,TVNQTR                                                        
         LA    R1,1(R1)                                                         
         STC   R1,TVNQTR                                                        
         CLI   TVNQTR,4                                                         
         BH    OUTRECX                                                          
*                                                                               
         L     RE,=A(OUTREC)                                                    
         L     RF,=F'1004'                                                      
         XCEF                                                                   
*                                                                               
         L     RE,=A(OUTDATA)                                                   
         USING PMKEY,RE                                                         
         LA    R9,PMPNUM                                                        
         ST    R9,APMPNUM                                                       
         MVC   PMKEY(3),=C'QNN'                                                 
         PACK  DUB,TVNYEAR                                                      
         CVB   R1,DUB                                                           
         SH    R1,=H'1900'                                                      
         CLI   TVNQTR,4            4TH QTR USE PREVIOUS YEAR                    
         BNE   *+8                                                              
         SHI   R1,1                                                             
         STC   R1,PMBOOK                                                        
         MVC   PMBOOK+1,TVNQTR                                                  
         MVC   PMSTAT(4),TVNNET                                                 
         MVI   PMSTAT+4,C'M'                                                    
         MVI   PMSTYP,X'01'                                                     
         MVI   PMBTYP,C'U'                                                      
         XC    PMBTYP+1(6),PMBTYP+1                                             
         MVC   PMBTYP+1(6),TVNNUM                                               
         MVC   PMPNUM,=X'0001'                                                  
         MVC   PMRLEN,=H'23'                                                    
         LA    RF,PMDATA                                                        
         USING MARELEM,RF                                                       
         MVC   0(8,RF),=X'010800C800C452'                                       
         ICM   R4,3,PMRLEN                                                      
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING PHTELEM,RF                                                       
         LA    R9,PHTDTYP                                                       
         ST    R9,APHTDTYP                                                      
         MVI   PHTCODE,PHTCODEQ                                                 
         MVI   PHTLEN,PHTLNEQ                                                   
         MVI   PHTDTYP,X'09'                                                    
         CLI   MULTISW,C'Y'                                                     
         BNE   *+8                                                              
         MVI   PHTDTYP,X'31'                                                    
*        MVC   PHTPTYP(2),NTIFILT                                               
         MVC   PHTPTYP(1),DPT3                                                  
         ZIC   R5,1(RF)                                                         
         AR    RF,R5                                                            
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         DROP  RF                                                               
         USING PHELEM,RF                                                        
         LA    R9,PHPNUM                                                        
         ST    R9,APHPNUM                                                       
         MVC   PHPNUM,PNUM                                                      
         MVI   PHCODE,PHCODEQ                                                   
         MVI   PHELN,PHELNEQ                                                    
         MVC   PHDWKS,X'0F'                                                     
         MVC   PHDBOOK,PMBOOK                                                   
         ICM   R4,3,PMRLEN                                                      
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING PPNELEM,RF                                                       
         MVI   PPNCODE,PPNCODEQ                                                 
         ZIC   R1,TVNNAME                                                       
         AHI   R1,2                                                             
         STC   R1,PPNELN                                                        
         ZIC   R1,TVNNAME                                                       
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PPNNME(0),TVNNAME+1                                              
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING NTELEM,RF                                                        
         MVC   0(10,RF),=X'221F24283C05DC06407C'                                
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING SLELEM,RF                                                        
         MVC   SLCODE(3),=X'230300'                                             
         MVI   SLSECT,172                                                       
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         USING PPEELEM,RF                                                       
         MVI   0(RF),X'41'                                                      
         MVI   1(RF),3+DNQLN                                                    
         MVI   2(RF),X'41'                                                      
         ZIC   R1,TVNQTR                                                        
         SHI   R1,1                                                             
         MHI   R1,DNQLN                                                         
         LA    R2,DNUMS                                                         
         AR    R2,R1                                                            
         MVC   3(DNQLN,RF),0(R2)                                                
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         AR    RF,R5                                                            
         DROP  RF                                                               
         MVC   0(7,RF),=X'5E07D5D5D56601'                                       
         MVC   2(3,RF),=C'OPI'                                                  
         ZIC   R5,1(RF)                                                         
         AR    R4,R5                                                            
         STCM  R4,3,PMRLEN                                                      
         LA    R4,4(R4)                                                         
         STCM  R4,3,OUTREC                                                      
         PUT   OUT,OUTREC                                                       
         B     OUTREC1                                                          
OUTRECX  B     GET                                                              
*                                                                               
OUTPROGX DS    0C                                                               
ENDJOB   DS    0C                                                               
         CLOSE (INA1)                                                           
         CLOSE (OUT)                                                            
         XBASE                                                                  
         EJECT                                                                  
*                                                                               
LNBUFF   EQU   L'PNAME+L'HN+L'GS+L'ONUMS                                        
LNBUF2   EQU   CLEARE-CLEARS                                                    
         DC    CL8'**WORK**'                                                    
SAVER8   DC    A(0)                                                             
ARDEF    DC    A(0)                                                             
NOMATCH  DC    X'0'                                                             
GETSW    DS    C                                                                
MATCH    DS    C                                                                
ALPHA    DS    C                                                                
NUMSW    DS    C                                                                
RFIELD   DS    C                                                                
HX       DS    C                                                                
HA       DS    C                                                                
HN       DS    C                                                                
GS       DS    C                                                                
MULTISW  DS    C                                                                
DPT3     DS    CL3                                                              
DUB      DS    D                                                                
FULL     DS    F                                                                
HALF     DS    H                                                                
PNUM     DS    H                                                                
ADPTCUME DS    F                                                                
WORK     DS    CL20                                                             
WORK2    DS    CL80                                                             
PNAME    DS    CL50                                                             
*                                                                               
TVNNAME  DS    CL60                                                             
TVNNET   DS    CL4                                                              
TVNNUM   DS    CL6                                                              
TVNYEAR  DS    CL4                                                              
TVNTEMP  DS    H                                                                
TVNQTR   DS    CL1                                                              
TEMPCHAR DS    CL1                                                              
TVNQ     EQU   *-TVNNAME                                                        
DMCB     DS    6F                                                               
         DC    CL8'*CLEARS*'                                                    
CLEARS   DS    0C                                                               
*                                                                               
NTINET   DS    CL6                                                              
NTINTI   DS    XL5                                                              
NTIFILT  DS    XL2                                                              
NTILP    DS    CL24                                                             
CLEARE   DS    0C                                                               
TBSTART  DS    0C                                                               
TBPNAME  DS    CL50                                                             
TBNET    DS    CL1                                                              
TBDPT    DS    CL1                                                              
TBIQ     DS    XL50                                                             
TBTVQ    DS    XL50                                                             
TBFT     DS    XL50                                                             
TBEND    DS    0C                                                               
P2       DS    CL132                                                            
         DS    200C                                                             
         DC    CL8'**IREC**'                                                    
INREC    DS    CL1204                                                           
         DC    CL8'**ONUM**'                                                    
ONUMS    DS    XL200                                                            
         DC    CL8'**DNUM**'                                                    
DNUMS    DS    XL200                                                            
DNQ1     EQU   0                                                                
DNQ2     EQU   32                                                               
DNQ3     EQU   64                                                               
DNQ4     EQU   96                                                               
DNQLN    EQU   32                                                               
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
INA1     DCB   DDNAME=INA1,                                            X        
               DSORG=PS,                                               X        
               EODAD=ENDJOB,                                           X        
               RECFM=VB,                                               X        
               LRECL=01200,                                            X        
               BLKSIZE=03600,                                          X        
               MACRF=GM                                                         
*                                                                               
OUT      DCB   DDNAME=OUT,                                             X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=01004,                                            X        
               BLKSIZE=05000,                                          X        
               MACRF=PM                                                         
*                                                                               
         EJECT                                                                  
TABDEF   DC    AL1(Q4HH,Q1HH,Q2HH,Q3HH)                                         
         DC    AL1(Q4W1824,Q1W1824,Q2W1824,Q3W1824)                             
         DC    AL1(Q4W1834,Q1W1834,Q2W1834,Q3W1834)                             
         DC    AL1(Q4W1849,Q1W1849,Q2W1849,Q3W1849)                             
         DC    AL1(Q4W2534,Q1W2534,Q2W2534,Q3W2534)                             
         DC    AL1(Q4W2549,Q1W2549,Q2W2549,Q3W2549)                             
         DC    AL1(Q4W2554,Q1W2554,Q2W2554,Q3W2554)                             
         DC    AL1(Q4W3564,Q1W3564,Q2W3564,Q3W3564)                             
         DC    AL1(Q4W5099,Q1W5099,Q2W5099,Q3W5099)                             
         DC    AL1(Q4M1824,Q1M1824,Q2M1824,Q3M1824)                             
         DC    AL1(Q4M1834,Q1M1834,Q2M1834,Q3M1834)                             
         DC    AL1(Q4M1849,Q1M1849,Q2M1849,Q3M1849)                             
         DC    AL1(Q4M2534,Q1M2534,Q2M2534,Q3M2534)                             
         DC    AL1(Q4M2549,Q1M2549,Q2M2549,Q3M2549)                             
         DC    AL1(Q4M2554,Q1M2554,Q2M2554,Q3M2554)                             
         DC    AL1(Q4M3564,Q1M3564,Q2M3564,Q3M3564)                             
         DC    AL1(Q4M5099,Q1M5099,Q2M5099,Q3M5099)                             
         DC    AL1(Q4A1824,Q1A1824,Q2A1824,Q3A1824)                             
         DC    AL1(Q4A1834,Q1A1834,Q2A1834,Q3A1834)                             
         DC    AL1(Q4A1849,Q1A1849,Q2A1849,Q3A1849)                             
         DC    AL1(Q4A2534,Q1A2534,Q2A2534,Q3A2534)                             
         DC    AL1(Q4A2549,Q1A2549,Q2A2549,Q3A2549)                             
         DC    AL1(Q4A2554,Q1A2554,Q2A2554,Q3A2554)                             
         DC    AL1(Q4A3599,Q1A3599,Q2A3599,Q3A3599)                             
         DC    AL1(Q4A3564,Q1A3564,Q2A3564,Q3A3564)                             
         DC    AL1(Q4A5099,Q1A5099,Q2A5099,Q3A5099)                             
         DC    AL1(Q4A1234,Q1A1234,Q2A1234,Q3A1234)                             
         DC    AL1(Q4A1217,Q1A1217,Q2A1217,Q3A1217)                             
         DC    AL1(Q4A0211,Q1A0211,Q2A0211,Q3A0211)                             
         DC    AL1(Q4A0611,Q1A0611,Q2A0611,Q3A0611)                             
         DC    AL1(Q4A0205,Q1A0205,Q2A0205,Q3A0205)                             
         DC    AL1(Q4A0914,Q1A0914,Q2A0914,Q3A0914)                             
         DC    X'FF'                                                            
R13E     DS    0C                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
TNYEAR   EQU   200                                                              
TNNET    EQU   201                                                              
TNNUM    EQU   202                                                              
TNNAME   EQU   203                                                              
NULL     EQU   0                                                                
****Q1 DISPLACEMENTS                                                            
Q1HH     EQU   1                                                                
Q1W1824  EQU   2                                                                
Q1W1834  EQU   3                                                                
Q1W1849  EQU   4                                                                
Q1W2534  EQU   5                                                                
Q1W2549  EQU   6                                                                
Q1W2554  EQU   7                                                                
Q1W3564  EQU   8                                                                
Q1W5099  EQU   9                                                                
Q1M1824  EQU   10                                                               
Q1M1834  EQU   11                                                               
Q1M1849  EQU   12                                                               
Q1M2534  EQU   13                                                               
Q1M2549  EQU   14                                                               
Q1M2554  EQU   15                                                               
Q1M3564  EQU   16                                                               
Q1M5099  EQU   17                                                               
Q1A1824  EQU   18                                                               
Q1A1834  EQU   19                                                               
Q1A1849  EQU   20                                                               
Q1A2534  EQU   21                                                               
Q1A2549  EQU   22                                                               
Q1A2554  EQU   23                                                               
Q1A3599  EQU   24                                                               
Q1A3564  EQU   25                                                               
Q1A5099  EQU   26                                                               
Q1A1234  EQU   27                                                               
Q1A1217  EQU   28                                                               
Q1A0211  EQU   29                                                               
Q1A0611  EQU   30                                                               
Q1A0205  EQU   31                                                               
Q1A0914  EQU   32                                                               
****Q2 DISPLACEMENTS                                                            
Q2HH     EQU   33                                                               
Q2W1824  EQU   34                                                               
Q2W1834  EQU   35                                                               
Q2W1849  EQU   36                                                               
Q2W2534  EQU   37                                                               
Q2W2549  EQU   38                                                               
Q2W2554  EQU   39                                                               
Q2W3564  EQU   40                                                               
Q2W5099  EQU   41                                                               
Q2M1824  EQU   42                                                               
Q2M1834  EQU   43                                                               
Q2M1849  EQU   44                                                               
Q2M2534  EQU   45                                                               
Q2M2549  EQU   46                                                               
Q2M2554  EQU   47                                                               
Q2M3564  EQU   48                                                               
Q2M5099  EQU   49                                                               
Q2A1824  EQU   50                                                               
Q2A1834  EQU   51                                                               
Q2A1849  EQU   52                                                               
Q2A2534  EQU   53                                                               
Q2A2549  EQU   54                                                               
Q2A2554  EQU   55                                                               
Q2A3599  EQU   56                                                               
Q2A3564  EQU   57                                                               
Q2A5099  EQU   58                                                               
Q2A1234  EQU   59                                                               
Q2A1217  EQU   60                                                               
Q2A0211  EQU   61                                                               
Q2A0611  EQU   62                                                               
Q2A0205  EQU   63                                                               
Q2A0914  EQU   64                                                               
****Q3 DISPLACEMENTS                                                            
Q3HH     EQU   65                                                               
Q3W1824  EQU   66                                                               
Q3W1834  EQU   67                                                               
Q3W1849  EQU   68                                                               
Q3W2534  EQU   69                                                               
Q3W2549  EQU   70                                                               
Q3W2554  EQU   71                                                               
Q3W3564  EQU   72                                                               
Q3W5099  EQU   73                                                               
Q3M1824  EQU   74                                                               
Q3M1834  EQU   75                                                               
Q3M1849  EQU   76                                                               
Q3M2534  EQU   77                                                               
Q3M2549  EQU   78                                                               
Q3M2554  EQU   79                                                               
Q3M3564  EQU   80                                                               
Q3M5099  EQU   81                                                               
Q3A1824  EQU   82                                                               
Q3A1834  EQU   83                                                               
Q3A1849  EQU   84                                                               
Q3A2534  EQU   85                                                               
Q3A2549  EQU   86                                                               
Q3A2554  EQU   87                                                               
Q3A3599  EQU   88                                                               
Q3A3564  EQU   89                                                               
Q3A5099  EQU   90                                                               
Q3A1234  EQU   91                                                               
Q3A1217  EQU   92                                                               
Q3A0211  EQU   93                                                               
Q3A0611  EQU   94                                                               
Q3A0205  EQU   95                                                               
Q3A0914  EQU   96                                                               
****Q4 DISPLACEMENTS                                                            
Q4HH     EQU   97                                                               
Q4W1824  EQU   98                                                               
Q4W1834  EQU   99                                                               
Q4W1849  EQU   100                                                              
Q4W2534  EQU   101                                                              
Q4W2549  EQU   102                                                              
Q4W2554  EQU   103                                                              
Q4W3564  EQU   104                                                              
Q4W5099  EQU   105                                                              
Q4M1824  EQU   106                                                              
Q4M1834  EQU   107                                                              
Q4M1849  EQU   108                                                              
Q4M2534  EQU   109                                                              
Q4M2549  EQU   110                                                              
Q4M2554  EQU   111                                                              
Q4M3564  EQU   112                                                              
Q4M5099  EQU   113                                                              
Q4A1824  EQU   114                                                              
Q4A1834  EQU   115                                                              
Q4A1849  EQU   116                                                              
Q4A2534  EQU   117                                                              
Q4A2549  EQU   118                                                              
Q4A2554  EQU   119                                                              
Q4A3599  EQU   120                                                              
Q4A3564  EQU   121                                                              
Q4A5099  EQU   122                                                              
Q4A1234  EQU   123                                                              
Q4A1217  EQU   124                                                              
Q4A0211  EQU   125                                                              
Q4A0611  EQU   126                                                              
Q4A0205  EQU   127                                                              
Q4A0914  EQU   128                                                              
*                                                                               
APMPNUM  DS    A                                                                
APHTDTYP DS    A                                                                
APHPNUM  DS    A                                                                
         DS    0F                                                               
         DC    CL8'*OUTREC*'                                                    
OUTREC   DS    XL4                                                              
OUTDATA  DS    XL1000                                                           
         EJECT                                                                  
         DS    C'PROGBUFF'                                                      
PROGBUFF DS    120000C                                                          
         DS    C'PROGBUF2'                                                      
PROGBUF2 DS    200000C                                                          
       ++INCLUDE DDDPRINT                                                       
       ++INCLUDE DEDEMFILE                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079DETRUES   07/24/08'                                      
         END                                                                    
