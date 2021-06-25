*          DATA SET SPCSO02    AT LEVEL 037 AS OF 05/01/02                      
*PHASE T21802A,*                                                                
         TITLE 'T21802 - CHILD SPOT STATION PERCENTAGE MAINTENANCE'             
T21802   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21802                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MYOVNUM,X'02'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    PERMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'02'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
         CLI   MODE,XRECPUT                                                     
         BE    DR                                                               
         CLI   MODE,XRECADD                                                     
         BE    DR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       LA    R2,PERMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    PERCLTH+4,X'DF'                                                  
         NI    PERMKTH+4,X'DF'                                                  
         NI    PERESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,PERCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKMKT                                                            
         NI    PERMKTH+4,X'DF'                                                  
         NI    PERESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKMKT    LA    R2,PERMKTH          VALIDATE MARKET FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    PERESTH+4,X'DF'                                                  
         CLI   ACTNUM,ACTLIST      IF ACTION LIST                               
         BNE   VKMKT10                                                          
         XC    BMKT,BMKT           THEN DEFAULT TO ZERO                         
         CLI   5(R2),0                                                          
         BE    VKMKTX                                                           
         B     VKMKT20                                                          
VKMKT10  GOTO1 ANY                 ELSE MARKET REQUIRED                         
VKMKT20  GOTO1 VALIMKT                                                          
VKMKTX   OI    4(R2),X'20'                                                      
         B     VKEST                                                            
*                                                                               
VKEST    LA    R2,PERESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
*                                                                               
VKX      LA    R6,KEY              BUILD KEY                                    
         USING CSOKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVI   CSOKTYPE,CSOKTYPQ   CSO RECORD TYPE                              
         MVI   CSOKSTYP,CSOKSTPQ   CSO RECORD SUB-TYPE                          
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT,BMKT        STATION ZEROS MEANS STATION RECORD           
         MVC   CSOKEST,BMEST                                                    
*                                                                               
         CLI   ACTNUM,ACTLIST      IF NOT LIST ACTION                           
         BE    XIT                                                              
         GOTO1 DATCON,DMCB,(0,QSTART),(5,PERL1)    DISPLAY START DATE           
         OI    PERL1H+6,X'80'                                                   
         B     XIT                                                              
         EJECT                                                                  
* VALIDATE RECORD                                                               
*                                                                               
VR       L     R6,AIO              BUILD RECORD FROM SCRATCH                    
         USING CSORECD,R6                                                       
         XC    0(256,R6),0(R6)                                                  
         MVC   CSOKEY,KEY                                                       
         MVC   CSOLEN,DATADISP     INSERT RECORD LENGTH                         
         MVC   CSOAGYA,AGENCY      AND ALPHA AGENCY CODE                        
         DROP  R6                                                               
*                                                                               
VR10     LA    R6,ELEM             FOR FIRST EFFECTIVE DATE                     
         XC    ELEM,ELEM               BUILD STATION ELEMENTS                   
         USING CSOSTAEL,R6                                                      
         MVI   STACODE,STACODEQ                                                 
*                                                                               
         LA    R2,PERL1H           CALCULATE NUMBER OF DRTAB ENTRIES            
         LA    R3,1                                                             
*                                                                               
VR12     ZIC   R0,0(R2)            BUMP TO NEXT LINE                            
         AR    R2,R0                                                            
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0             IF END OF SCREEN THEN DONE                   
         BE    VR15                                                             
         CLI   5(R2),0             IF BLANK LINE THEN DONE                      
         BE    VR15                                                             
         LA    R3,1(R3)            BUMP COUNTER                                 
         B     VR12                LOOP BACK                                    
*                                                                               
VR15     MH    R3,=Y(DPTABL)       SET STATION PERC ELEMENT LENGTH              
         LA    R3,7(R3)                                                         
         STC   R3,STALEN                                                        
*                                                                               
         LA    R7,STADPTAB         POINT R7 TO DATES AND PERCENTAGES            
         USING DPTABD,R7                                                        
         DROP  R6                                                               
*                                  SET FIRST DATE TO EST START DATE             
VR20     GOTO1 DATCON,DMCB,(0,QSTART),(3,DPDATE)                                
*                                                                               
         LA    R2,PERL1H           POINT TO FIRST STATIONS FIELD                
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 ANY                 SCAN FOR STATIONS = PERCENTAGES              
         GOTO1 SCANNER,DMCB,(R2),SCANOUT                                        
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         LA    R5,SCANOUT                                                       
         ZIC   R4,4(R1)            R4 = NUMBER OF STATIONS                      
         LA    R3,1                R3 = STATION COUNTER                         
*                                                                               
VR30     LA    R6,SAVEKEY          TEST STATION VALID FOR THIS MARKET           
         USING STARECD,R6              BY READING STATION MASTER RECORD         
         MVI   SAVEKEY,C'0'                                                     
         MVC   SAVEKEY+1(16),SAVEKEY                                            
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
         MVC   STAKCALL,12(R5)                                                  
         CLI   STAKCALL+4,C' '                                                  
         BH    *+8                                                              
         MVI   STAKCALL+4,C'T'                                                  
         MVC   STAKAGY,AGENCY                                                   
         MVC   STAKCLT,QCLT                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',SAVEKEY,AIO2             
         CLI   8(R1),0                                                          
         BNE   FLDERR                                                           
         L     R6,AIO2             COMPARE MARKET FOR THIS STATION              
         CLC   QMKT,SMKT               WITH MARKET FOR THIS RECORD              
         BNE   FLDERR                                                           
         DROP  R6                                                               
*                                                                               
         LA    R6,ELEM             POINT BACK TO ELEMENT                        
         USING CSOSTAEL,R6                                                      
*                                                                               
         LA    R8,SCANOUT          DON'T ALLOW DUPLICATE STATIONS               
*                                                                               
VR40     CR    R8,R5               CHECK ALL SCANNER BLOCK ENTRIES              
         BNL   VR50                    PRECEEDING THIS ONE                      
         CLC   12(4,R5),12(R8)                                                  
         BE    FLDERR                                                           
         LA    R8,32(R8)                                                        
         B     VR40                                                             
*                                                                               
VR50     MVC   STANAME,12(R5)                                                   
         ZIC   R8,1(R5)            VALIDATE PERCENTAGE                          
         GOTO1 CASHVAL,DMCB,(3,22(R5)),(R8)                                     
         CLI   0(R1),0                                                          
         BNE   FLDERR                                                           
         MVC   DPPCT,4(R1)         MOVE PERCENTAGE INTO ELEMENT                 
         DROP  R7                                                               
*                                                                               
         ZIC   R1,STALEN           R1 = LENGTH OF ELEMENT                       
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              ADD ELEMENT TO END UNSORTED                  
         USING CSORECD,R6                                                       
         SR    RF,RF                                                            
         ICM   RF,3,CSOLEN                                                      
         LA    RE,0(RF,R6)                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),ELEM                                                     
*                                                                               
         LA    RF,1(R1,RF)         ADJUST RECORD LENGTH                         
         STCM  RF,3,CSOLEN                                                      
         DROP  R6                                                               
*                                                                               
         LA    R5,32(R5)           LOOP THROUGH SCANNER BLOCK UNTIL             
         LA    R3,1(R3)                NO MORE STATIONS LEFT                    
         CR    R3,R4                                                            
         BNH   VR30                                                             
         EJECT                                                                  
*    FOR SUBSEQUENT EFFECTIVE DATES, LOOP THROUGH ALREADY BUILT STATION         
*    ELEMENTS AND INSERT DATES AND PERCENTAGES                                  
*                                                                               
         LA    R7,1                START WITH SECOND EFFECTIVE DATE             
         MVC   LASTDATE,QSTART                                                  
*                                                                               
VR100    ZIC   R0,0(R2)            BUMP TO NEXT EFFECTIVE DATE                  
         AR    R2,R0                                                            
         CLI   0(R2),0             TEST END OF SCREEN                           
         BE    VRX                                                              
*                                                                               
         CLI   5(R2),0             CHECK NO MORE EFFECTIVE DATES                
         BNE   VR110                                                            
         ZIC   R0,0(R2)            OTHER FIELD MUST BE EMPTY                    
         AR    R2,R0                                                            
         CLI   5(R2),0                                                          
         BNE   INVERR                                                           
         B     VRX                                                              
*                                  VALIDATE EFFECTIVE DATE                      
VR110    GOTO1 DATVAL,DMCB,(0,8(R2)),THISDATE                                   
         OC    DMCB(4),DMCB                                                     
         BZ    INVERR                                                           
         GOTO1 ADDAY,DMCB,LASTDATE,LASTDATE,F'7'                                
*                                                                               
VR111    CLC   LASTDATE,THISDATE                                                
         BE    VR112                                                            
         GOTO1 ADDAY,DMCB,LASTDATE,LASTDATE,F'7'                                
         CLC   LASTDATE,QEND                                                    
         BNH   VR111                                                            
         B     INVERR                                                           
*                                                                               
VR112    ZIC   R0,0(R2)            BUMP TO STATIONS FIELD                       
         AR    R2,R0                                                            
*                                                                               
         GOTO1 ANY                 SCAN FOR STATIONS = PERCENTAGES              
         GOTO1 SCANNER,DMCB,(R2),SCANOUT                                        
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         LA    R5,SCANOUT                                                       
         ZIC   R4,4(R1)            R4 = NUMBER OF STATIONS                      
         LA    R3,1                R3 = STATION COUNTER                         
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,STACODEQ                                                  
         BAS   RE,GETEL            R6 = A(FIRST STATION ELEMENT)                
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOSTAEL,R6                                                      
*                                                                               
VR120    CLC   STANAME,12(R5)      MUST BE SAME STATION                         
         BNE   FLDERR                                                           
*                                  CONVERT DATE AND VALIDATE PERCENTAGE         
         GOTO1 DATCON,DMCB,(0,THISDATE),(3,NEXTDATE)                            
         ZIC   R8,1(R5)                                                         
         GOTO1 CASHVAL,DMCB,(3,22(R5)),(R8)                                     
         CLI   0(R1),0                                                          
         BNE   FLDERR                                                           
*                                                                               
         LA    RF,DPTABL           POINT RF TO DATES AND PERCENTAGES            
         MR    RE,R7                                                            
         LA    RF,STADPTAB(RF)                                                  
         USING DPTABD,RF                                                        
         MVC   DPDATE,NEXTDATE     MOVE IN DATE AND PERCENTAGE                  
         MVC   DPPCT,4(R1)                                                      
         DROP  RF                                                               
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT STATION ELEMENT                 
         BNE   VR150                                                            
         LA    R5,32(R5)           LOOP THROUGH SCANNER BLOCK UNTIL             
         LA    R3,1(R3)                NO MORE STATIONS LEFT                    
         CR    R3,R4                                                            
         BNH   VR120                                                            
         B     INVERR              SHOULD NOT BE ANY ELEMENTS LEFT              
*                                                                               
VR150    CR    R3,R4               SHOULD NOT BE ANY STATIONS LEFT              
         BNE   INVERR                                                           
         LA    R7,1(R7)            BUMP TO NEXT EFFECTIVE DATE                  
         B     VR100                                                            
*                                                                               
VRX      B     XIT                                                              
         EJECT                                                                  
* DISPLAY RECORD                                                                
*                                                                               
DR       GOTO1 CLEARF,DMCB,(0,PERL1H),PERLAST                                   
         XC    PERL1,PERL1                                                      
         OI    PERL1H+6,X'80'                                                   
*                                                                               
         LA    R2,PERL1H           POINT TO FIRST EFFECTIVE DATE                
*                                                                               
         LA    R5,1                INITIALIZE EFFECTIVE DATE COUNTER            
*                                                                               
DR5      L     R6,AIO              POINT TO FIRST STATION ELEMENT               
         MVI   ELCODE,STACODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOSTAEL,R6                                                      
*                                                                               
DR10     LR    RF,R5               POINT R4 TO CORRECT DPTAB ENTRY              
         BCTR  RF,0                                                             
         LA    R1,DPTABL                                                        
         MR    RE,R1                                                            
         LR    R7,RF               HOLD DISPLACEMENT IN R7                      
         LA    R4,STADPTAB(R7)                                                  
         USING DPTABD,R4                                                        
*                                                                               
         ZIC   RF,STALEN           TEST NO MORE DATES                           
         LA    RF,0(RF,R6)                                                      
         CR    R4,RF                                                            
         BNL   DRX                                                              
*                                                                               
         OC    DPDATE,DPDATE       TEST NO MORE DATES (OLD RECORDS)             
         BZ    DRX                                                              
*                                  DISPLAY DATE                                 
         GOTO1 DATCON,DMCB,(3,DPDATE),(5,8(R2))                                 
*                                                                               
         ZIC   R0,0(R2)            BUMP TO STATIONS FIELD                       
         AR    R2,R0                                                            
         LA    R3,8(R2)            POINT R3 TO STATIONS FIELD DATA              
*                                                                               
DR20     LA    R4,STADPTAB(R7)     POINT R4 TO CORRECT DPTAB ENTRY              
*                                                                               
         MVC   0(4,R3),STANAME     DISPLAY STATION = PERCENTAGE                 
         LA    R3,4(R3)                                                         
         CLI   STANAME+4,C' '                                                   
         BNH   *+14                                                             
         MVC   0(1,R3),STANAME+4                                                
         LA    R3,1(R3)                                                         
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
         EDIT  (4,DPPCT),(7,0(R3)),3,ALIGN=LEFT,ZERO=NOBLANK                    
         AR    R3,R0                                                            
*                                                                               
         BAS   RE,NEXTEL           BUMP TO NEXT STATION ELEMENT                 
         BNE   DR30                                                             
         MVI   0(R3),C','          INSERT COMMA BETWEEN STATIONS                
         LA    R3,1(R3)                                                         
         B     DR20                DISPLAY NEXT STATION                         
*                                                                               
DR30     ZIC   R0,0(R2)            BUMP TO NEXT EFFECTIVE DATE                  
         AR    R2,R0                                                            
         LA    R5,1(R5)                                                         
         B     DR5                                                              
*                                                                               
DRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY KEY                                                                   
*                                                                               
DK       L     R6,AIO              RECORD SELECTED                              
         USING CSOKEY,R6                                                        
*                                                                               
         GOTO1 CLEARF,DMCB,(0,PERMEDH),PEROPTH                                  
         MVC   PERMED(L'QMED),QMED                                              
         MVC   PERCLT(L'QCLT),QCLT                                              
         GOTO1 MSUNPK,DMCB,CSOKMKT,QMKT,QSTA                                    
         MVC   PERMKT(L'QMKT),QMKT                                              
         MVC   PEREST(L'QMEST),QMEST                                            
*                                                                               
DKX      B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* LIST RECORDS                                                                  
*                                                                               
LR       LA    R4,KEY                                                           
         USING CSOKEY,R4                                                        
*                                                                               
         OC    KEY(13),KEY         TEST FIRST TIME THROUGH                      
         BNZ   LR10                                                             
         MVI   CSOKTYPE,CSOKTYPQ   RECORD TYPE                                  
         MVI   CSOKSTYP,CSOKSTPQ   RECORD SUB-TYPE                              
         MVC   CSOKAM,BAGYMD       AGY/MED                                      
         MVC   CSOKCLT,BCLT        CLIENT                                       
         MVC   CSOKMKT,BMKT        STARTING MARKET NUMBER                       
*                                                                               
LR10     MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                FIRST RECORD                                 
         B     LR30                                                             
*                                                                               
LR20     LA    R4,KEY              NEXT RECORD                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 SEQ                                                              
*                                                                               
LR30     CLC   KEY(5),KEYSAVE      TEST SAME TYPE/AGY/MED/CLT                   
         BNE   LRX                                                              
         OC    CSOKSTA,CSOKSTA     TEST STATION PERCENTAGE RECORD               
         BNZ   LR20                                                             
         CLC   CSOKEST,BMEST       TEST SAME ESTIMATE NUMBER                    
         BNE   LR20                                                             
         MVI   RDUPDATE,C'N'       READ IN RECORD                               
         GOTO1 GETREC                                                           
*                                  FILL IN LIST LINE                            
         XC    LISTAR,LISTAR                                                    
*                                                                               
         SR    R0,R0               MARKET NUMBER                                
         ICM   R0,3,CSOKMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  LSTMKT(4),DUB                                                    
*                                                                               
         LA    R6,SAVEKEY          GET MARKET NAME FROM STATION FILE            
         USING MKTRECD,R6                                                       
         MVI   SAVEKEY,C'0'                                                     
         MVC   SAVEKEY+1(16),SAVEKEY                                            
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
         MVC   MKTKMKT,LSTMKT                                                   
         MVC   MKTKAGY,AGENCY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'STATION',SAVEKEY,AIO2             
         CLI   8(R1),0                                                          
         BE    LR40                                                             
         MVC   LSTNAME(13),=C'** UNKNOWN **'     MARKET NAME NOT FOUND          
         B     LR45                                                             
*                                                                               
LR40     L     R6,AIO2                                                          
         USING MKTRECD,R6                                                       
         MVC   LSTNAME,MKTNAME     MOVE MARKET NAME INTO LIST LINE              
         DROP  R6                                                               
*                                                                               
LR45     L     R6,AIO              INSPECT STATION ELEMENTS                     
         MVI   ELCODE,STACODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST BE AT LEAST ONE                         
         USING CSOSTAEL,R6                                                      
         LA    R2,LSTSTA                                                        
*                                                                               
LR50     MVC   0(4,R2),STANAME     MOVE IN STATION NAMES                        
         LA    R2,5(R2)                                                         
*                                                                               
LR60     BAS   RE,NEXTEL           SKIP TO NEXT STATION                         
         BE    LR50                                                             
*                                                                               
LR70     GOTO1 LISTMON             DISPLAY LINE                                 
         B     LR20                                                             
*                                                                               
LRX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* PRINT SCHEME REPORT                                                           
*                                                                               
PR       B     XIT                                                              
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
DASHES   DC    C'------------------'                                            
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
       ++INCLUDE SPCSOF2D                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSTMKT   DS    CL5                                                              
         DS    CL8                                                              
LSTNAME  DS    CL24                                                             
         DS    CL6                                                              
LSTSTA   DS    CL30                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'037SPCSO02   05/01/02'                                      
         END                                                                    
