*          DATA SET SPCSO03    AT LEVEL 045 AS OF 05/01/02                      
*PHASE T21803A,*                                                                
         TITLE 'T21803 - CHILD SPOT ALLOCATION STATUS'                          
T21803   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21803                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
*                                                                               
         CLI   MYOVNUM,X'03'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    ALLMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'03'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE AND DISPLAY REQUESTS                
         BE    VK                                                               
         DC    H'0'                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,ALLMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    ALLCLTH+4,X'DF'                                                  
         NI    ALLSTAH+4,X'DF'                                                  
         NI    ALLESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,ALLCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKMKT                                                            
         NI    ALLSTAH+4,X'DF'                                                  
         NI    ALLESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKMKT    LA    R2,ALLSTAH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    ALLESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         IF NUMERIC IT'S A MARKET NUMBER              
         BZ    VKSTA                                                            
         GOTO1 VALIMKT                                                          
         B     VKSTAX                                                           
VKSTA    GOTO1 VALISTA             ELSE IT'S A STATION NAME                     
VKSTAX   OI    4(R2),X'20'                                                      
         B     VKEST                                                            
*                                                                               
VKEST    LA    R2,ALLESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKESTX                                                           
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
VKESTX   GOTO1 CMPNTP                                                           
         GOTO1 CLRACC                                                           
         EJECT                                                                  
VKDATA   LA    R2,ALLDATAH         VALIDATE DATA FIELD                          
         GOTO1 ANY                                                              
         GOTO1 SCANNER,DMCB,(R2),SCANOUT,0                                      
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         CLI   4(R1),2                                                          
         BH    INVERR              NO MORE THAN TWO DATA TYPES                  
         ZIC   R3,4(R1)                                                         
         ST    R3,FULL             SAVE NUMBER OF DATA TYPES                    
         LA    R3,1                                                             
         LA    R4,DATA             WILL HOLD VALID DATA TYPES 1,2               
         MVC   DATA,MYSPACES                                                    
         LA    R5,SCANOUT                                                       
*                                                                               
VKDATA10 C     R3,FULL             TEST NO MORE DATA TYPES TO VALIDATE          
         BH    VKX                                                              
         CLI   0(R5),0             TEST ABSENT DATA TYPE                        
         BE    VKDATA50                                                         
         CLI   1(R5),0             NON-EMPTY SECOND HALF NOT VALID              
         BNE   FLDERR                                                           
         LA    R6,DATATYPS         TABLE OF VALID DATA TYPES                    
         USING DATATYPD,R6                                                      
*                                                                               
VKDATA20 CLI   0(R6),0             TEST END OF TABLE                            
         BE    FLDERR                                                           
         CLC   12(4,R5),DATAVAL    TEST MATCH ON DATA TYPE                      
         BE    VKDATA30                                                         
         LA    R6,DATATYPL(R6)     BUMP TO NEXT TABLE ENTRY                     
         B     VKDATA20                                                         
*                                                                               
VKDATA30 MVC   0(DATATYPL,R4),0(R6)     SAVE SELECTED DATA TYPE                 
*                                                                               
VKDATA50 LA    R4,DATATYPL(R4)     BUMP POINTER TO NEXT DATA TYPE               
         LA    R3,1(R3)            INCREMENT DATA TYPE NUMBER                   
         LA    R5,32(R5)           BUMP TO NEXT SCAN FIELD                      
         B     VKDATA10                                                         
*                                                                               
VKX      CLC   DATA(DATATYPL),MYSPACES                                          
         BE    INVERR              BLANK FIRST DATA TYPE NOT VALID              
         CLC   DATA(DATATYPL),DATA+DATATYPL                                     
         BE    INVERR              DUPLICATE DATA TYPES NOT VALID               
         DROP  R6                                                               
         EJECT                                                                  
DR       XC    ACCNUM,ACCNUM       PROCESS FIRST THEN SECOND DATA TYPE          
*                                                                               
DR10     LA    R3,DATATYPL         POINT R3 TO REQUESTED DATA TYPE              
         M     R2,ACCNUM                                                        
         LA    R3,DATA(R3)                                                      
         USING DATATYPD,R3                                                      
         CLC   DATAVAL,=C'G   '    TEST GOAL DOLLARS                            
         BE    DR20                                                             
         CLC   DATAVAL,=C'CG  '    TEST CASH GOAL                               
         BE    DR30                                                             
         CLC   DATAVAL,=C'    '    TEST BLANK                                   
         BE    DR900                                                            
         B     DR500               ELSE DATA FOUND IN PROGRAM RECORDS           
*                                                                               
DR20     LA    R2,ALLMEDH          POINT TO MEDIA FIELD FOR ERROR               
         GOTO1 CMPGOL              COMPUTE GOAL DOLLARS INTO ACCTAB             
         B     DR900                                                            
*                                                                               
DR30     LA    R2,ALLMEDH          POINT TO MEDIA FIELD FOR ERROR               
         GOTO1 CMPCGOL             COMPUTE CASH GOAL INTO ACCTAB                
         B     DR900                                                            
         EJECT                                                                  
DR500    XC    KEY,KEY             COMPUTE CASH/TRADE/TOTAL                     
         LA    R4,KEY                                                           
         USING CSORECD,R4          FILL KEY WITH AGYMD/CLT/MKT                  
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT,BMKT                                                     
*                                                                               
         OC    BSTA,BSTA           TEST IF STATION GIVEN                        
         BE    DR510                                                            
         MVC   CSOKSTA,BSTA        USE THIS STATION ONLY AND SUM OVER           
         MVC   CSOKEST,BMEST           REFERENCE NUMBERS                        
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE                                                  
         BNE   ERRPNF              MUST BE AT LEAST ONE REFERENCE NO.           
         B     DR550                                                            
*                                                                               
DR510    MVC   CSOKSTA,=X'000001'  SUM OVER ALL STATIONS FOR THIS MKT           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
DR520    CLC   KEY(7),KEYSAVE      MUST BE AT LEAST ONE STATION FOR             
         BNE   ERRPNF                  THIS ESTIMATE                            
         CLC   CSOKEST,BMEST       TEST MATCH ON ESTIMATE                       
         BE    DR550                                                            
         MVI   RDUPDATE,C'N'       IF NOT TRY NEXT RECORD                       
         GOTO1 SEQ                                                              
         B     DR520                                                            
         EJECT                                                                  
DR550    MVI   RDUPDATE,C'N'       READ IN RECORD                               
         GOTO1 GETREC                                                           
         L     R6,AIO              POINT TO FIRST WEEK ELEMENT                  
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
*                                                                               
         LA    R5,ACCTAB           POINT TO FIRST ACCTAB ENTRY                  
         USING ACCTABD,R5                                                       
         L     RF,ACCNUM           POINT TO CORRECT ACCUMULATOR                 
         SLL   RF,2                                                             
         LA    R5,ACCONE(RF)                                                    
*                                                                               
DR560    ICM   RE,15,0(R5)         RE = ACCUMULATOR VALUE                       
         ICM   RF,15,WKCOST        RF = WEEKLY RATE                             
         SR    R7,R7                                                            
         ICM   R7,3,WKCSPOTS       R7 = CASH SPOTS                              
         SR    R8,R8                                                            
         ICM   R8,3,WKTSPOTS       R8 = TRADE SPOTS                             
*                                                                               
         CLC   DATAVAL,=C'C   '    TEST CASH DOLLARS                            
         BE    DR600                                                            
         CLC   DATAVAL,=C'T   '    TEST TRADE DOLLARS                           
         BE    DR610                                                            
         CLC   DATAVAL,=C'TOT '    TEST TOTAL DOLLARS                           
         BE    DR620                                                            
         CLC   DATAVAL,=C'CS  '    TEST CASH SPOTS                              
         BE    DR630                                                            
         CLC   DATAVAL,=C'TS  '    TEST TRADE SPOTS                             
         BE    DR640                                                            
         CLC   DATAVAL,=C'TOTS'    TEST TOTAL SPOTS                             
         BE    DR650                                                            
         DC    H'0'                                                             
         EJECT                                                                  
DR600    LR    R1,R7               ADD CASH DOLLARS TO ACCUMULATOR              
         MR    R0,RF                                                            
         AR    R1,RE                                                            
         B     DR660                                                            
DR610    LR    R1,R8               ADD TRADE DOLLARS TO ACCUMULATOR             
         MR    R0,RF                   (APPLY NTP% LATER)                       
         AR    R1,RE                                                            
         B     DR660                                                            
DR620    LR    R1,R7               ADD TOTAL DOLLARS TO ACCUMULATOR             
         MR    R0,RF                                                            
         LR    R2,R1                                                            
         LR    R1,R8                                                            
         MR    R0,RF                                                            
         M     R0,NTPPERC                                                       
         A     R1,=F'5000'                                                      
         D     R0,=A(10*1000)                                                   
         AR    R1,R2                                                            
         AR    R1,RE                                                            
         B     DR660                                                            
DR630    LR    R1,R7               ADD CASH SPOTS TO ACCUMULATOR                
         AR    R1,RE                                                            
         B     DR660                                                            
DR640    LR    R1,R8               ADD TRADE SPOTS TO ACCUMULATOR               
         AR    R1,RE                                                            
         B     DR660                                                            
DR650    LR    R1,R7               ADD TOTAL SPOTS TO ACCUMULATOR               
         AR    R1,R8                                                            
         AR    R1,RE                                                            
DR660    STCM  R1,15,0(R5)         STORE SUM BACK INTO ACCUMULATOR              
         EJECT                                                                  
DR670    BAS   RE,NEXTEL           REPEAT UNTIL WEEKS EXAUSTED                  
         BNE   DR700                                                            
         LA    R5,ACCTABL(R5)      BUMP TO NEXT ACCUMULATOR                     
         B     DR560                                                            
*                                                                               
DR700    OC    BSTA,BSTA           IF STATION GIVEN REPEAT UNTIL                
         BE    DR710                   STATION EXAUSTED                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
         CLC   KEY(11),KEYSAVE                                                  
         BNE   DR900                                                            
         B     DR550                                                            
*                                                                               
DR710    MVI   RDUPDATE,C'N'       ELSE REPEAT UNTIL MARKET EXAUSTED            
         GOTO1 SEQ                                                              
         CLC   KEY(7),KEYSAVE      TEST MARKET EXAUSTED                         
         BNE   DR900                                                            
         CLC   CSOKEST,BMEST       TEST CORRECT ESTIMATE NUMBER                 
         BNE   DR710                                                            
         B     DR550                                                            
*                                                                               
DR900    AF    ACCNUM,=F'1'        REPEAT UNTIL DATA TYPES EXAUSTED             
         CLC   ACCNUM,=F'2'                                                     
         BL    DR10                                                             
         EJECT                                                                  
DR950    LA    R5,ACCTAB           R5 = A(ACCUMULATOR TABLE)                    
         LA    R4,DATA             R4 = A(DATA TYPE TABLE)                      
         USING DATATYPD,R4                                                      
*                                                                               
         XC    ALLQUA1,ALLQUA1     PRE-CLEAR TOTAL FIELDS                       
         OI    ALLQUA1H+6,X'80'                                                 
         XC    ALLQUA2,ALLQUA2                                                  
         OI    ALLQUA2H+6,X'80'                                                 
         XC    ALLTOTS,ALLTOTS                                                  
         OI    ALLTOTSH+6,X'80'                                                 
*                                                                               
         LA    R2,ALLTOTS+14       IF THERE IS NO SECOND DATA TYPE              
         LA    R4,DATATYPL(R4)         THEN CENTER ALIGN FIRST TOTAL            
         CLC   DATAVAL,MYSPACES                                                 
         BH    *+8                                                              
         LA    R2,ALLTOTS+30                                                    
*                                                                               
         LA    R4,DATA             DISPLAY FIRST TOTALS                         
         GOTO1 DRTOTALS,DMCB,ACCONE,ALLQUA1,(R2)                                
*                                                                               
         LA    R4,DATATYPL(R4)     IF THERE IS A SECOND DATA TYPE               
         CLC   DATAVAL,MYSPACES                                                 
         BNH   DR980               THEN DISPLAY SECOND TOTALS                   
         GOTO1 DRTOTALS,DMCB,ACCTWO,ALLQUA2+6,ALLTOTS+42                        
*                                                                               
DR980    LA    R4,DATA             DISPLAY ACCUMULATORS TO SCREEN               
         MVC   CALHEADS(4),DATAHEAD                                             
         LA    R4,DATATYPL(R4)                                                  
         MVC   CALHEADS+4(4),DATAHEAD                                           
         LA    R2,ALLL1H                                                        
         ST    R2,CALSPTR                                                       
         GOTO1 BLDCAL                                                           
*                                                                               
DR990    B     EXIT                                                             
         EJECT                                                                  
DRTOTALS NTR1                      DISPLAY QUARTERS/YEAR TOTALS                 
         USING DATATYPD,R4                                                      
         LM    R2,R3,0(R1)         R2 = A(FIRST ACCUMULATOR)                    
*                                  R3 = A(PLACE TO DISPLAY QUARTER 1)           
         L     R5,8(R1)            R5 = A(PLACE TO DISPLAY TOTAL)               
*                                                                               
*                                  TOTAL ACCUMULATORS                           
         GOTO1 TOTFULL,DMCB,(R2),ACCTABL                                        
         MVC   TOTAL,0(R1)                                                      
*                                                                               
         CLC   DATAVAL,=C'T   '    IF DOING TRADE DOLLARS                       
         BNE   DRT10               THEN APPLY NTP TO ACCS                       
         GOTO1 APPNTP,DMCB,(R2),TOTAL                                           
*                                                                               
DRT10    LA    R2,4                DISPLAY 4 QUARTERS                           
         LA    R6,QUARTAB                                                       
         USING QUARTABD,R6                                                      
*                                                                               
*                                  DISPLAY QUARTER TOTAL                        
DRT20    EDIT  (4,QUARACC),(12,0(R3)),ALIGN=RIGHT,COMMAS=YES,ZERO=NOBLAX        
               NK,MINUS=YES                                                     
*                                                                               
         LA    R6,QUARTABL(R6)     BUMP TO NEXT QUARTER                         
         LA    R3,19(R3)                                                        
         BCT   R2,DRT20                                                         
         DROP  R6                                                               
*                                                                               
*        DISPLAY TOTAL FOR THE YEAR                                             
*                                                                               
         CLI   DATAPRNT,C'Y'       IF DOLLAR DATA TYPE DISPLAY DOLLAR           
         BNE   DRT50                   HEADING                                  
         MVC   0(15,R5),=C'TOTAL DOLLARS $'                                     
         LA    R5,15(R5)                                                        
         B     DRT60                                                            
*                                  ELSE DISPLAY SPOT HEADING                    
DRT50    MVC   0(12,R5),=C'TOTAL SPOTS '                                        
         LA    R5,12(R5)                                                        
*                                  DISPLAY DOLLAR FIGURE                        
DRT60    EDIT  (4,TOTAL),(10,0(R5)),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK,X        
               MINUS=YES                                                        
*                                                                               
DRTX     B     XIT                                                              
         EJECT                                                                  
APPNTP   NTR1                      APPLY NTP% TO ACCUMULATORS                   
*                                                                               
         L     R3,0(R1)            R3 = A(FIRST ACCUMULATOR)                    
         L     R4,4(R1)            R4 = A(TOTAL)                                
*                                                                               
         MVC   THISDATE,QSTART                                                  
*                                                                               
APP10    CLC   THISDATE,QEND       TEST END OF TABLE                            
         BH    APP20                                                            
         ICM   RF,15,0(R3)                                                      
         M     RE,NTPPERC          APPLY TRADE PERCENTAGE                       
         A     RF,=F'5000'                                                      
         D     RE,=A(10*1000)                                                   
         STCM  RF,15,0(R3)                                                      
         LA    R3,ACCTABL(R3)      NEXT ACCUMULATOR                             
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         B     APP10                                                            
*                                                                               
APP20    ICM   RF,15,0(R4)         APPLY PERCENTAGE TO YEAR TOTAL               
         M     RE,NTPPERC                                                       
         A     RF,=F'5000'                                                      
         D     RE,=A(10*1000)                                                   
         STCM  RF,15,0(R4)                                                      
*                                                                               
         LA    R2,4                APPLY TO FOUR QUARTERS                       
         LA    R6,QUARTAB                                                       
         USING QUARTABD,R6                                                      
*                                                                               
APP30    ICM   RF,15,QUARACC       APPLY PERCENTAGE TO QUARTER TOTAL            
         M     RE,NTPPERC                                                       
         A     RF,=F'5000'                                                      
         D     RE,=A(10*1000)                                                   
         STCM  RF,15,QUARACC                                                    
*                                                                               
         LA    R6,QUARTABL(R6)     BUMP TO NEXT QUARTER                         
         BCT   R2,APP30                                                         
         DROP  R6                                                               
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
FLDERR   OI    6(R2),X'40'         SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(23,R2),=C'** ERROR IN FIELD    **'                             
         EDIT  (R3),(2,26(R2)),ALIGN=LEFT                                       
         GOTO1 ERREX2                                                           
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ERRPNF   OI    ALLMEDH+6,X'40'     SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(34,R2),=C'** ERROR PROGRAM RECORDS NOT FOUND'                  
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     OI    ALLDATAH+6,X'40'    SET CURSOR                                   
         LA    R2,CONHEADH                                                      
         XC    CONHEAD,CONHEAD                                                  
         MVC   8(14,R2),=C'DATA DISPLAYED'                                      
         GOTO1 ERREX2                                                           
*                                                                               
DATATYPS DC    C'C   ',C'C$  ',C'Y'                                             
         DC    C'T   ',C'T$  ',C'Y'                                             
         DC    C'G   ',C'G$  ',C'Y'                                             
         DC    C'CG  ',C'CG$ ',C'Y'                                             
         DC    C'TOT ',C'TOT$',C'Y'                                             
         DC    C'CS  ',C'CS  ',C'N'                                             
         DC    C'TS  ',C'TS  ',C'N'                                             
         DC    C'TOTS',C'TOTS',C'N'                                             
         DC    X'00'                                                            
*                                                                               
DASHES   DC    C'------------------'                                            
MYSPACES DC    CL80' '                                                          
         EJECT                                                                  
         LTORG                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE SPCSOFFD                                                       
         PRINT ON                                                               
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPCSOF3D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
DATA     DS    CL(2*DATATYPL)                                                   
TOTAL    DS    F                                                                
TOTFLAG  DS    C                                                                
*                                                                               
DATATYPD DSECT                                                                  
DATAVAL  DS    CL4                 VALID DATA TYPE                              
DATAHEAD DS    CL4                 DISPLAY LINE HEADING                         
DATAPRNT DS    C                   FLAGS WHETHER OR NOT TO PRINT TOTALS         
DATATYPL EQU   *-DATATYPD                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGENDMN                                                       
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045SPCSO03   05/01/02'                                      
         END                                                                    
