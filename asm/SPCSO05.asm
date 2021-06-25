*          DATA SET SPCSO05    AT LEVEL 083 AS OF 05/01/02                      
*PHASE T21805A,*                                                                
         TITLE 'T21805 - CHILD SPOT ALLOCATION GENERATION'                      
T21805   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 30*REFTABL,T21805                                                
         LR    R7,RC                                                            
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R7,AREFTAB                                                       
*                                                                               
         CLI   MYOVNUM,X'05'       CHECK FIRST TIME FOR THIS OVERLAY            
         BE    GOAHEAD                                                          
         NI    GENMEDH+4,X'DF'     FORCE VALIDATION OF KEY FIELDS               
GOAHEAD  MVI   MYOVNUM,X'05'       STORE OVERLAY NUMBER                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,PRINTREP       PROCEED WITH ALLOCATION                      
         BE    VR                                                               
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
* VALIDATE KEY ROUTINE                                                          
*                                                                               
VK       MVI   PQSW,1              SUPPRESS OPENING OF PRINT QUEUE              
*                                                                               
         LA    R2,GENMEDH          VALIDATE MEDIA FIELD                         
         TM    4(R2),X'20'                                                      
         BO    VKCLT                                                            
         NI    GENCLTH+4,X'DF'                                                  
         NI    GENSTAH+4,X'DF'                                                  
         NI    GENESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALIMED                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,GENCLTH          VALIDATE CLIENT FIELD                        
         TM    4(R2),X'20'                                                      
         BO    VKSTA                                                            
         NI    GENSTAH+4,X'DF'                                                  
         NI    GENESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALICLT                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKSTA    LA    R2,GENSTAH          VALIDATE STATION FIELD                       
         TM    4(R2),X'20'                                                      
         BO    VKEST                                                            
         NI    GENESTH+4,X'DF'                                                  
         GOTO1 ANY                                                              
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VKEST    LA    R2,GENESTH          VALIDATE ESTIMATE FIELD                      
         TM    4(R2),X'20'                                                      
         BO    VKREAL                                                           
         GOTO1 ANY                                                              
         GOTO1 VALIMEST                                                         
         OI    4(R2),X'20'                                                      
         EJECT                                                                  
VKREAL   LA    R2,GENREALH         VALIDATE REALLOCATION FIELD                  
*                                                                               
         MVI   REALFLAG,C'N'       IF EMPTY THEN REALFLAG = 'N'                 
         CLI   5(R2),0                                                          
         BE    VKOPT                                                            
*                                                                               
         CLC   8(3,R2),=C'YES'     ELSE USER MUST ENTER 'YES' OR 'OK'           
         BE    VKREAL10                                                         
         CLC   8(2,R2),=C'OK'                                                   
         BNE   INVERR                                                           
*                                                                               
VKREAL10 MVI   REALFLAG,C'Y'       REALFLAG = 'Y'                               
*                                                                               
VKOPT    LA    R2,GENOPTH          VALIDATE OPTION FIELD                        
         XC    TRACEOPT,TRACEOPT                                                
*                                                                               
         CLI   REALFLAG,C'Y'       IF RELOCATION REQUESTED                      
         BNE   VKOPT5                                                           
         GOTO1 ANY                 THEN FIELD IS MANDATORY                      
         B     VKOPT10                                                          
*                                                                               
VKOPT5   CLI   5(R2),0             ELSE FIELD IS OPTIONAL                       
         BE    VKX                                                              
*                                  SCAN FIELD FOR START DATE & TRACE            
VKOPT10  GOTO1 SCANNER,DMCB,(R2),(1,SCANOUT)                                    
         CLI   4(R1),1             MUST HAVE ONE OR TWO FIELDS                  
         BL    INVERR                                                           
         CLI   4(R1),2                                                          
         BH    INVERR                                                           
         LA    R6,SCANOUT                                                       
*                                                                               
         CLC   12(6,R6),=C'START ' IF FIRST FIELD STARTS WITH 'START'           
         BNE   VKOPT20             SECOND HALF MUST BE VALID MMMDD/YY           
         GOTO1 DATVAL,DMCB,22(R6),ALSTART                                       
         OC    0(4,R1),0(R1)       SAVE DATE IN ALSTART                         
         BZ    INVERR                                                           
*                                                                               
         CLC   ALSTART,QSTART      DATE MUST BE WITHIN THE YEAR                 
         BL    INVERR                                                           
         CLC   ALSTART,QEND                                                     
         BNL   INVERR                                                           
*                                                                               
         CLI   4(R1),2             IF THERE IS A SECOND SCANNER FIELD           
         BNE   VKX                                                              
         LA    R6,32(R6)           BUMP R6 TO NEXT SCANNER FIELD                
*                                                                               
VKOPT20  CLC   12(6,R6),=C'TRACE ' FIRST HALF MUST BE 'TRACE'                   
         BNE   INVERR                                                           
         CLC   22(5,R6),=C'WEEK '  SECOND HALF MUST BE 'WEEK' OR 'SPOT'         
         BE    VKOPT30                                                          
         CLC   22(5,R6),=C'SPOT '                                               
         BNE   INVERR                                                           
*                                                                               
VKOPT30  MVC   TRACEOPT,22(R6)     TRACEOPT = SECOND HALF OF SCANNER            
         GOTO1 OPENPQ              OPEN PRINT QUEUE FOR WRITING                 
*                                                                               
VKX      B     XIT                                                              
         EJECT                                                                  
* BUILD REFERENCE TABLE AND COMPUTE CASH GOALS                                  
*                                                                               
VR       GOTO1 CMPNTP              COMPUTE NTP PERCENTAGE                       
         GOTO1 CLRACC              CLEAR ACCUMULATOR TABLE                      
         XC    ACCNUM,ACCNUM                                                    
         GOTO1 CMPCGOL             COMPUTE CASH GOALS INTO ACCTAB               
*                                                                               
         LA    R6,ACCTAB           TOTAL CASH GOALS INTO TOTDOLLS               
         USING ACCTABD,R6                                                       
         GOTO1 TOTFULL,DMCB,ACCONE,ACCTABL                                      
         MVC   TOTDOLLS,0(R1)                                                   
         DROP  R6                                                               
*                                                                               
         L     R5,AREFTAB          INITIALIZE REFTAB                            
         USING REFTABD,R5                                                       
         LR    RE,R5                                                            
         L     RF,=A(30*REFTABL)                                                
         XCEF                                                                   
         SR    R8,R8               R8 = NUMBER OF REFERENCES                    
*                                                                               
         XC    KEY,KEY             READ PROGRAM RECORDS AND FILL REFTAB         
         LA    R4,KEY                  WITH INFO CONTAINED THEREIN              
         USING CSOKEY,R4                                                        
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VR10     CLC   KEY(11),KEYSAVE     CHECK NO MORE REFERNCE NUMBERS               
         BNE   VR50                                                             
*                                                                               
         MVI   RDUPDATE,C'N'       READ RECORD                                  
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO              POINT TO DESCRIPTION ELEMENT                 
         MVI   ELCODE,DSCCODEQ                                                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSODSCEL,R6                                                      
         CLI   DSCWGT,0            SKIP PROGRAMS WITH ZERO WEIGHTS              
         BE    VR40                                                             
*                                                                               
         MVC   REFNUM,CSOKREF      MOVE IN REFERENCE NUMBER                     
         MVC   REFWGT,DSCWGT       MOVE IN WEIGHT                               
         MVC   REFMIN,DSCMIN       MOVE IN MINIMUM SPOTS                        
         MVC   REFMAX,DSCMAX       MOVE IN MAXIMUM SPOTS                        
         CLI   REFMAX,0            IF NO MAX SPECIFIED                          
         BNE   *+8                                                              
         MVI   REFMAX,99           THEN SET TO 99                               
*                                                                               
         L     R6,AIO              USE WEEKLY ELEMENTS TO FILL DRTAB            
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         MVC   REFRATE,WKCOST      SET CURRENT RATE TO FIRST WEEK'S             
         MVC   FULL,WKCOST                                                      
         LA    R3,REFDRTAB         POINT TO DATE/RATE TABLE                     
         USING DRTABD,R3                                                        
         XC    REFDRDSP,REFDRDSP   ZERO DISPLACEMENT TO DRTAB ENTRY             
*                                                                               
VR20     CLI   REALFLAG,C'N'       IF REALLOCATION NOT PERMITTED                
         BNE   VR25                                                             
         OC    WKCSPOTS,WKCSPOTS   AND SPOTS ALREADY ALLOCATED                  
         BNZ   ERRREAL             THEN ERROR                                   
*                                                                               
VR25     BAS   RE,NEXTEL           BUMP TO NEXT WEEK ELEMENT                    
         BNE   VR30                                                             
         CLC   WKCOST,FULL         TEST CHANGE IN RATE                          
         BE    VR20                                                             
         MVC   DRDATE,WKDATE       CREATE DRTAB ENTRY                           
         MVC   DRRATE,WKCOST                                                    
         MVC   FULL,WKCOST                                                      
         LA    R3,DRTABL(R3)       BUMP TO NEXT ENTRY                           
         B     VR20                                                             
*                                                                               
VR30     MVC   DRDATE,=X'FFFF'     SET LAST DATE TO INFINITY                    
*                                                                               
         LA    R5,REFTABL(R5)      POINT TO NEXT REFTAB ENTRY                   
         LA    R8,1(R8)            INCREMENT NUMBER OF REFERENCES               
*                                                                               
VR40     MVI   RDUPDATE,C'N'       READ NEXT KEY                                
         GOTO1 SEQ                                                              
         B     VR10                                                             
*                                                                               
VR50     ST    R8,NUMREFS          SAVE NUMBER OF REFERENCES                    
         C     R8,=F'0'                                                         
         BE    ERRPNF              ERROR PROGRAM RECORDS NOT FOUND              
         B     VR100                                                            
         DROP  R3                                                               
         EJECT                                                                  
* OUTSIDE LOOP GOES THROUGH EACH WEEK IN THE YEAR                               
*                                                                               
VR100    L     RF,TOTDOLLS         ALLOCATE ZERO SPOTS IF GOAL FOR              
         LTR   RF,RF                   ENTIRE YEAR IS NOT POSITIVE              
         BNP   VR500                                                            
*                                                                               
         LA    R6,ACCTAB           ACCUMULATE TOTALS FOR EACH WEEK              
         USING ACCTABD,R6              IN ACCTAB                                
*                                                                               
         XC    DOLLARS,DOLLARS     CLEAR DOLLARS LEFT TO ALLOCATE               
         SR    R8,R8               R8 = WEEK NUMBER                             
         MVC   THISDATE,QSTART     CURRENT DATE = START DATE                    
*                                                                               
VR110    XC    ALLOCED,ALLOCED     CLEAR DOLLARS ALLOCATED THIS WEEK            
*                                                                               
         MVC   EXAUSTED,C'Y'       STAYS 'Y' IF TOTAL DOLLARS RUN OUT           
*                                                                               
         ICM   RF,15,ACCONE        DOLLARS LEFT = DOLLARS LEFT FROM             
         A     RF,DOLLARS              LAST WEEK + GOAL FOR THIS WEEK           
         ST    RF,DOLLARS                                                       
*                                                                               
         LTR   RF,RF               SKIP THIS WEEK IF NO DOLLARS LEFT            
         BNP   VR400                                                            
*                                                                               
         ICM   RF,15,ACCONE        SKIP THIS WEEK IF NO GOAL DOLLARS            
         LTR   RF,RF                                                            
         BNP   VR400                                                            
*                                  CONVERT THIS DATE TO 2-BYTE FORMAT           
         GOTO1 DATCON,DMCB,(0,THISDATE),(2,NEXTDATE)                            
*                                                                               
         MVI   RATCHANG,C'N'       SET RATE CHANGE TO 'N'                       
         L     R5,AREFTAB          BRING RATES FOR THIS WEEK UP TO DATE         
*                                                                               
VR120    CLI   REFNUM,0            WHILE NOT END OF REFERENCE TABLE             
         BE    VR135                                                            
*                                                                               
         ZIC   R4,REFDRDSP         POINT TO DATE/RATE TABLE                     
         LA    R4,REFDRTAB(R4)                                                  
         USING DRTABD,R4                                                        
*                                                                               
         CLC   DRDATE,NEXTDATE     IF RATE CHANGES THIS WEEK                    
         BH    VR130                                                            
         MVC   REFRATE,DRRATE      THEN CHANGE RATE                             
         ZIC   R4,REFDRDSP                                                      
         LA    R4,DRTABL(R4)                                                    
         STC   R4,REFDRDSP         POINT TO NEXT DATE                           
         DROP  R4                                                               
*                                                                               
         MVI   RATCHANG,C'Y'       SET RATE CHANGE TO 'Y'                       
*                                                                               
VR130    LA    R5,REFTABL(R5)      NEXT PROGRAM                                 
         B     VR120                                                            
*                                                                               
VR135    CLI   RATCHANG,C'Y'       IF A RATE HAS CHANGED                        
         BNE   VR150                                                            
         L     R5,AREFTAB          THEN LOOP THROUGH REFERENCE TABLE            
*                                                                               
VR140    CLI   REFNUM,0                                                         
         BE    VR150                                                            
         XC    REFACT,REFACT       AND CLEAR ACTUAL SPOTS                       
         XC    REFRATIO,REFRATIO   AND RATIO                                    
*                                                                               
         LA    R5,REFTABL(R5)                                                   
         B     VR140               NEXT PROGRAM                                 
*                                                                               
VR150    B     VR200                                                            
         EJECT                                                                  
* ALLOCATE MINIMUM SPOTS TO EACH PROGRAM                                        
*                                                                               
VR200    L     R5,AREFTAB                                                       
*                                                                               
VR210    CLI   REFNUM,0            LOOP THROUGH EACH PROGRAM                    
         BE    VR290                                                            
         OC    REFMIN,REFMIN                                                    
         BZ    VR230                                                            
         ZIC   R4,REFMIN                                                        
*                                                                               
VR220    CLC   REFRATE,TOTDOLLS    IF HAVEN'T RUN OUT OF DOLLARS FOR            
         BH    VR230                   THE YEAR THEN EXAUSTED = 'N'             
         MVI   EXAUSTED,C'N'                                                    
*                                                                               
         CLC   REFRATE,DOLLARS     IF ENOUGH DOLLARS LEFT THIS WEEK             
         BH    VR230                                                            
         BAS   RE,ADDSPOT          ADD A SPOT TO THIS PROGRAM                   
         BCT   R4,VR220                                                         
*                                                                               
VR230    LA    R5,REFTABL(R5)      BUMP TO NEXT PROGRAM                         
         B     VR210                                                            
*                                                                               
VR290    B     VR300                                                            
         EJECT                                                                  
* ALLOCATE SPOTS ACCORDING TO NEED UNTIL GOAL REACHED                           
*                                                                               
VR300    XC    HISCORE,HISCORE     INITITALIZE SCORING VARIABLES                
         XC    REFPTR,REFPTR                                                    
         L     R5,AREFTAB                                                       
*                                                                               
VR310    CLI   REFNUM,0            ASSIGN SPOT TO PROGRAM WITH HIGHEST          
         BE    VR330                   SCORE                                    
*                                                                               
         CLC   REFRATE,TOTDOLLS    IF HAVEN'T RUN OUT OF DOLLARS FOR            
         BH    VR320                   THE YEAR THEN EXAUSTED = 'N'             
         MVI   EXAUSTED,C'N'                                                    
*                                                                               
         OC    REFRATE,REFRATE     DON'T ASSIGN SPOTS TO PROGRAMS WITH          
         BZ    VR320                   ZERO RATES OR RATES THAT EXCEED          
         CLC   REFRATE,DOLLARS         DOLLARS LEFT                             
         BH    VR320                                                            
*                                                                               
         OC    REFMAX,REFMAX       DON'T ASSIGN SPOTS TO PROGRAMS THAT          
         BZ    VR315                   HAVE ALREADY RECEIVED THE                
         LR    RF,R8                   MAXIMUM ALLOWED                          
         SLL   RF,1                                                             
         LA    RF,REFSPOTS+1(RF)                                                
         CLC   0(1,RF),REFMAX                                                   
         BNL   VR320                                                            
*                                                                               
VR315    LA    R4,SCORE            SCORE PROGRAMS BY:                           
         USING SCORED,R4               1)  LOWEST RATIO                         
         SR    RF,RF                   2)  HIGHEST WEIGHT                       
         ICM   RF,15,REFRATIO          3)  HIGHEST RATE                         
         LA    RF,1(RF)                                                         
         LNR   RF,RF                                                            
         STCM  RF,15,SCRRATIO                                                   
         MVC   SCRWGT,REFWGT                                                    
         MVC   SCRRATE,REFRATE                                                  
*                                                                               
         CLC   SCORE,HISCORE       IF SCORE IS HIGHER THAN CURRENT              
         BNH   VR320                   HIGHEST THAN SET CURRENT HIGHEST         
         MVC   HISCORE,SCORE           TO THIS SCORE                            
         ST    R5,REFPTR                                                        
         DROP  R4                                                               
*                                                                               
VR320    LA    R5,REFTABL(R5)      NEXT PROGRAM                                 
         B     VR310                                                            
*                                                                               
VR330    OC    HISCORE,HISCORE     TEST SOME PROGRAM HAS BEEN CHOSEN            
         BZ    VR400                                                            
         L     R5,REFPTR           POINT TO PROGRAM TO BE GIVEN SPOT            
         BAS   RE,ADDSPOT                                                       
         B     VR300                                                            
         EJECT                                                                  
VR400    CLC   TRACEOPT(4),=C'WEEK'    TEST TRACE BY WEEK OPTION                
         BNE   VR410                                                            
         BAS   RE,TRACE                                                         
         B     VR420                                                            
*                                                                               
VR410    XC    TRACEOPT,TRACEOPT   CLEAR TRACE BY SPOT OPTION                   
*                                                                               
VR420    MVC   ACCTWO,ALLOCED      SAVE ALLOCATED DOLLARS IN ACCTAB             
*                                                                               
         CLI   EXAUSTED,C'Y'       IF NO MORE DOLLARS THEN DONE                 
         BE    VR490                                                            
*                                                                               
         LA    R6,ACCTABL(R6)      BUMP TO NEXT WEEK                            
         LA    R8,1(R8)                                                         
         GOTO1 ADDAY,DMCB,THISDATE,THISDATE,F'7'                                
         CLC   THISDATE,QEND       REPEAT UNTIL END OF YEAR                     
         BNH   VR110                                                            
*                                                                               
VR490    B     VR500                                                            
         EJECT                                                                  
* UPDATE PROGRAM RECORDS WITH NEWLY ALLOCATED SPOTS                             
*                                                                               
VR500    L     R2,NUMREFS          SORT TABLE BY REFERENCE NUMBER               
         GOTO1 XSORT,DMCB,(X'00',AREFTAB),(R2),REFTABL,L'REFNUM,       X        
               A(REFNUM-REFTABD)                                                
*                                                                               
         L     R5,AREFTAB          UPDATE PROGRAMS IN REFERENCE TABLE           
         USING REFTABD,R5                                                       
*                                                                               
         XC    KEY,KEY             FILL KEY WITH AGYMED/CLT/MKTSTA              
         LA    R4,KEY                                                           
         USING CSOKEY,R4                                                        
         MVI   CSOKTYPE,CSOKTYPQ                                                
         MVI   CSOKSTYP,CSOKSTPQ                                                
         MVC   CSOKAM,BAGYMD                                                    
         MVC   CSOKCLT,BCLT                                                     
         MVC   CSOKMKT(5),BMKTSTA                                               
         MVC   CSOKEST,BMEST                                                    
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
VR510    CLC   KEY(11),KEYSAVE     CHECK NO MORE REFERNCE NUMBERS               
         BNE   VR600                                                            
*                                                                               
         CLC   CSOKREF,REFNUM      SKIP REFERENCES NOT IN THE TABLE             
         BNE   VR540                                                            
*                                                                               
         GOTO1 GETREC              READ RECORD                                  
*                                                                               
         L     R6,AIO              POINT TO FIRST WEEK ELEMENT                  
         MVI   ELCODE,WKCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CSOWKEL,R6                                                       
         LA    R3,REFSPOTS         POINT TO FIRST WEEK IN TABLE                 
*                                                                               
VR520    CLI   REALFLAG,C'Y'       IF REALLOCATING                              
         BNE   VR522                                                            
         GOTO1 DATCON,DMCB,(2,WKDATE),(0,THISDATE)                              
         CLC   THISDATE,ALSTART    THEN SKIP WEEKS BEFORE START WEEK            
         BL    VR525                                                            
*                                                                               
VR522    MVC   WKCSPOTS,0(R3)      MOVE CASH SPOTS INTO RECORD                  
*                                                                               
VR525    BAS   RE,NEXTEL           LOOP UNTIL NO MORE WEEKS                     
         BNE   VR530                                                            
         LA    R3,2(R3)                                                         
         B     VR520                                                            
*                                                                               
VR530    GOTO1 PUTREC              WRITE RECORD BACK                            
*                                                                               
         LA    R5,REFTABL(R5)      POINT TO NEXT REFTAB ENTRY                   
*                                                                               
VR540    MVI   RDUPDATE,C'N'       READ NEXT KEY                                
         GOTO1 SEQ                                                              
         B     VR510                                                            
         EJECT                                                                  
* DISPLAY ACCUMULATORS AND DOLLAR TOTALS TO SCREEN                              
*                                                                               
VR600    LA    R2,GENL1H           DISPLAY ACCUMULATORS TO SCREEN               
         ST    R2,CALSPTR                                                       
         MVC   CALHEADS,=C'CG$ C$  '                                            
         GOTO1 BLDCAL                                                           
*                                  DISPLAY DOLLAR TOTALS AT THE BOTTOM          
         OI    GENTOTSH+6,X'80'                                                 
         XC    GENTOTS,GENTOTS                                                  
         MVC   GENTOTS+20(16),=C'DOLLAR TOTALS  $'                              
         LA    R6,ACCTAB                                                        
         USING ACCTABD,R6                                                       
*                                  DISPLAY FIRST ACCUMULATOR TOTAL              
         LA    R2,GENTOTS+20+16                                                 
         GOTO1 TOTFULL,DMCB,ACCONE,ACCTABL                                      
         EDIT  (4,0(R1)),(10,0(R2)),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK          
         AR    R2,R0                                                            
         LA    R2,3(R2)                                                         
*                                  DISPLAY SECOND ACCUMULATOR TOTAL             
         MVI   0(R2),C'$'                                                       
         LA    R2,1(R2)                                                         
         GOTO1 TOTFULL,DMCB,ACCTWO,ACCTABL                                      
         EDIT  (4,0(R1)),(10,0(R2)),ALIGN=LEFT,COMMAS=YES,ZERO=NOBLANK          
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
ADDSPOT  LR    RF,R8               POINT RF TO THIS WEEK'S SPOTS                
         SLL   RF,1                                                             
         LA    RF,REFSPOTS(RF)                                                  
*                                                                               
         SR    R1,R1               INCREMENT THIS WEEK'S SPOTS                  
         ICM   R1,3,0(RF)                                                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,0(RF)                                                       
*                                                                               
         ICM   R1,15,REFACT        INCREMENT SPOTS FOR THE YEAR                 
         LA    R1,1(R1)                                                         
         STCM  R1,15,REFACT                                                     
*                                                                               
         M     R0,=F'100'          COMPUTE NEW RATIO SPOTS/WEIGHT               
         ZIC   RF,REFWGT                                                        
         DR    R0,RF                                                            
         STCM  R1,15,REFRATIO                                                   
*                                                                               
         ICM   RF,15,REFRATE       LOAD RATE                                    
*                                                                               
         L     R0,ALLOCED          ADD RATE TO DOLLARS ALLOCATED                
         AR    R0,RF                                                            
         ST    R0,ALLOCED                                                       
*                                                                               
         L     R0,DOLLARS          SUBTRACT RATE FROM DOLLARS TO BE             
         SR    R0,RF                   ALLOCATED FOR THE WEEK                   
         ST    R0,DOLLARS                                                       
*                                                                               
         L     R0,TOTDOLLS         SUBTRACT RATE FROM DOLLARS TO BE             
         SR    R0,RF                   ALLOCATED FOR THE YEAR                   
         ST    R0,TOTDOLLS                                                      
*                                                                               
         CLC   TRACEOPT(4),=C'SPOT'    TEST TRACE BY SPOT OPTION                
         BE    TRACE                                                            
         BR    RE                                                               
         EJECT                                                                  
TRACE    NTR1                                                                   
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R5,AREFTAB                                                       
*                                                                               
TR10     CLI   REFNUM,0                                                         
         BE    TRX                                                              
         MVC   P1+30(10),=C'REFERNCE #'                                         
         EDIT  (1,REFNUM),(3,P1+40)                                             
         GOTO1 HEXOUT,DMCB,0(R5),P2,64,0                                        
         GOTO1 HEXOUT,DMCB,64(R5),P3,64,0                                       
         GOTO1 HEXOUT,DMCB,128(R5),P4,64,0                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,REFTABL(R5)                                                   
         B     TR10                                                             
*                                                                               
TRX      MVI   LINE,99                                                          
         B     XIT                                                              
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     TRAPERR                                                          
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
ERRREAL  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(33),=C'ENTER ''YES'' OR ''OK'' TO REALLOCATE'            
         LA    R2,GENREALH                                                      
         GOTO1 ERREX2                                                           
*                                                                               
ERRPNF   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(34),=C'** ERROR PROGRAM RECORDS NOT FOUND'               
         LA    R2,GENMEDH                                                       
         GOTO1 ERREX2                                                           
*                                                                               
EXIT     XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(14),=C'DATA DISPLAYED'                                   
         LA    R2,GENMEDH                                                       
         GOTO1 ERREX2                                                           
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
       ++INCLUDE SPCSOF5D                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCSO                                                       
         EJECT                                                                  
       ++INCLUDE SPCSOWORKD                                                     
         EJECT                                                                  
* WORK AREA                                                                     
*                                                                               
         ORG   SYSSPARE                                                         
AREFTAB  DS    A                   ADDRESS OF PROGRAMS TABLE                    
TRACEOPT DS    CL10                TRACE OPTION                                 
REALFLAG DS    C                   FLAGS REALLOCATION OF SPOTS ALLOWED          
ALSTART  DS    CL6                 START DATE FOR REALLOCATION                  
NUMREFS  DS    F                   NUMBER OF ENTRIES IN REFTAB                  
TOTDOLLS DS    F                   DOLLARS LEFT TO BE ALLOCED THIS YEAR         
EXAUSTED DS    C                   FLAGS IF NO MORE DOLLARS THIS YEAR           
DOLLARS  DS    F                   DOLLARS LEFT TO BE ALLOCED THIS WEEK         
ALLOCED  DS    F                   AMOUNT ALLOCATED THIS WEEK SO FAR            
SCORE    DS    CL(SCOREL)          SCORE FOR CURRENT REFERENCE NUMBER           
HISCORE  DS    CL(SCOREL)          HIGHEST SCORE SO FAR                         
REFPTR   DS    A                   POINTER TO REFERENCE WITH HIGH SCORE         
RATCHANG DS    C                   INDICATES IF A RATE HAS CHANGED              
         SPACE 1                                                                
REFTABD  DSECT                     TABLE OF PROGRAMS TO BE GIVEN SPOTS          
REFNUM   DS    X                   REFERENCE NUMBER                             
REFMIN   DS    X                   MINIMUM SPOTS                                
REFMAX   DS    X                   MAXIMUM SPOTS                                
REFWGT   DS    X                   PROGRAM WEIGHT                               
REFRATE  DS    XL4                 CURRENT RATE                                 
REFDRDSP DS    X                   DISPLACEMENT WITHIN DATE/RATE TABLE          
REFDRTAB DS    CL(10*DRTABL)       DATE/RATE TABLE                              
REFSPOTS DS    CL(53*2)            ALLOCATED SPOTS FOR EACH WEEK                
REFACT   DS    XL4                 TOTAL SPOTS ALLOCATED SO FAR                 
REFRATIO DS    XL4                 RATIO OF TOTAL SPOTS/WEIGHT                  
REFTABL  EQU   *-REFTABD                                                        
*                                                                               
DRTABD   DSECT                     TABLE OF EFFECTIVE DATES AND RATES           
DRDATE   DS    XL2                 EFFECTIVE DATE                               
DRRATE   DS    XL4                 RATE                                         
DRTABL   EQU   *-DRTABD                                                         
*                                                                               
SCORED   DSECT                     ARRANGES COMPARISONS INTO HEIRARCHY          
SCRRATIO DS    XL4                 RATIO                                        
SCRWGT   DS    XL1                 WEIGHT                                       
SCRRATE  DS    XL4                 RATE                                         
SCOREL   EQU   *-SCORED                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'083SPCSO05   05/01/02'                                      
         END                                                                    
