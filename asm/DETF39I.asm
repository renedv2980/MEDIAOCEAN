*          DATA SET DETF39I    AT LEVEL 031 AS OF 04/22/19                      
*PROCESS USING(WARN(15))                                                        
*PHASE DETF39IA                                                                 
*INCLUDE NUMVAL                                                                 
         TITLE 'NCC FUSION DATA CONVERSION (INPUT PHASE)'                       
DETF39I  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,DETF39I,RA                                                     
         USING DEMCOND,R8                                                       
         L     R7,VCPRINT                                                       
         USING DPRINT,R7           R7=A(PRINTER DSECT)                          
         L     RC,ARREC                                                         
         L     R2,AIREC                                                         
         USING INTERD,R2                                                        
*                                                                               
         B     *+4(R1)                                                          
         B     READ                GET INPUT (ARREC - INT)                      
         B     CNVWR               CONVERT TO OUTPUT (INT - WORK)               
         B     MORET               CLOSE INPUT                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*===================== GET INPUT (RREC --> IREC) =====================*         
*                                                                               
READ     CLI   INTAPESW,1          INPUT FILE OPEN YET?                         
         BE    OPENOK              YES                                          
*                                                                               
         OPEN  IN1                                                              
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         MVC   ADEMTABS,CDEMTABS                                                
         DROP  RF                                                               
*                                                                               
         LHI   R0,NUMDEMOS                                                      
         CHI   R0,19                                                            
         BE    *+6                                                              
         DC    H'0'                WE ONLY KNOW OF 19 DEMO CATEGORIES!          
*                                                                               
* GET FIRST RECORD (SURVEY NAME)                                                
*                                                                               
         L     R4,ARREC                                                         
         XCEF  (R4),2000                                                        
         GET   IN1,(R4)                                                         
         LA    R4,4(R4)            BUMP PAST RDW: R4 POINTS TO 1ST C'"'         
         OC    0(L'SPACES,R4),SPACES                                            
*                                                                               
FINDBOOK DS    0H                                                               
         MVC   P(25),=C'READING RELATIVE MARKET #'                              
         EDIT  NUMMKTS,(5,P+26),ALIGN=LEFT,COMMAS=YES                           
         GOTO1 VPRINTER                                                         
*                                                                               
FB10     DS    0H                                                               
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *-8                 FIND THE START OF THE NEXT TOKEN             
         ST    R4,FULL             SAVE A(SPACE BEFORE TOKEN)                   
         MVC   WORK,SPACES                                                      
         LA    RF,WORK+6           PUT THE TOKEN INTO WORK+6                    
FB20     MVC   0(1,RF),1(R4)                                                    
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         CLI   1(R4),C'"'          EOR?                                         
         BE    *+12                                                             
         CLI   1(R4),C' '                                                       
         BNE   FB20                                                             
         SHI   RF,2                POINT TO LAST 2 BYTES OF THE TOKEN           
         MVC   HALF,=C'00'                                                      
         MVZ   HALF,0(RF)                                                       
         CLC   HALF,=C'00'                                                      
         BNE   FB30                MUST BE NUMERIC                              
         MVC   2(1,RF),1(RF)                                                    
         MVC   1(1,RF),0(RF)                                                    
         MVI   0(RF),C'/'                                                       
         GOTO1 VDATVAL,DMCB,(2,WORK+6),WORK                                     
         CLC   =C'000000',WORK     TOKEN IS A VALID M/Y?                        
         BNE   FB40                YES: WORK CONTAINS 'FUNNY' YYMMDD            
FB30     CLI   1(R4),C'"'          EOR?                                         
         BNE   FB10                                                             
         DC    H'0'                NO VALID DATE IN SURVEY NAME RECORD          
*                                                                               
FB40     GOTO1 VDATCON,DMCB,WORK,(3,THREE)                                      
         OC    BOOKYM,BOOKYM       FIRST MARKET FOR THIS BOOK?                  
         BZ    *+16                YES: SAVE BOOK                               
         CLC   BOOKYM,THREE        THIS MARKET BETTER BE FOR SAME BOOK!         
         BE    FB44                                                             
         DC    H'0'                                                             
*                                                                               
         MVC   BOOKYM,THREE        BOOK BINARY Y/M                              
*                                                                               
         GOTO1 ADEMTABS,DMCB,NSICABLE  GET A(NSI CABLE CALL LETTER TBL)         
         MVC   ANSICABT,0(R1)      A(TABLE) RETURNED IN P1                      
         OC    ANSICABT,ANSICABT                                                
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         MVC   LNSICABT,6(R1)      SAVE L'TABLE ENTRY                           
*                                                                               
         GOTO1 ADEMTABS,DMCB,SETMETER  GET A(NIELSEN SET METERED MKTS)          
         MVC   ASETMETR,0(R1)      A(TABLE) RETURNED IN P1                      
         OC    ASETMETR,ASETMETR                                                
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         MVC   LSETMETR,6(R1)      SAVE L'TABLE ENTRY                           
*                                                                               
* NCC SOMETIMES POSTS FUSION DATA FOR MARKETS EVEN AFTER THEY ARE               
* LPM. WE NEED TO CATCH THOSE MARKETS AND SKIP THEM.                            
*                                                                               
         GOTO1 ADEMTABS,DMCB,FUSNENDP  GET A(FUSION END DATE TABLE)             
         ICM   RE,15,0(R1)         A(TABLE) RETURNED IN P1                      
         BNZ   *+6                                                              
         DC    H'0'                BAD TABLEID PASSED                           
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
*                                                                               
         USING FUSENDPD,RE                                                      
FB41     DS    0H                                                               
         CLC   BOOKYM,FUSENDDT     IS THIS NOW AN LPM MARKET?                   
         BL    FB43                NO: WE CAN PROCESS IT                        
*                                                                               
         LARL  R3,FUSMKTS          YES: FIND IT IN OUR MARKET TABLE             
         USING FUSMKTD,R3                                                       
FB42     DS    0H                                                               
         CLI   0(R3),X'FF'         EOT?                                         
         BE    FB43                MARKET NOT IN TABLE (E.G., ATLANTA)          
         CLC   FUSAMKTF,FUSMKTC    IS THIS THE MARKET?                          
         BE    *+12                YES                                          
         AHI   R3,FUSMKTLQ                                                      
         B     FB42                                                             
         OI    FUSFLAGS,FUSLPM     FLAG THIS AS LPM MARKET                      
         DROP  R3                                                               
*                                                                               
FB43     DS    0H                                                               
         AR    RE,R0               BUMP TO NEXT LPM MARKET                      
         CLI   0(RE),X'FF'         EOT?                                         
         BNE   FB41                NO                                           
         DROP  RE                                                               
*                                                                               
FB44     DS    0H                                                               
         MVI   BOOKTYP,0           DEFAULT TO STANDARD BOOKTYPE                 
         MVC   BOOKTYPA,=C'ST'                                                  
         CLC   =C' C-DMA NIELSEN CBL #"',1(R4)  (FOR BACKWARD COMPAT.)          
         BE    FB45                                                             
         CLC   =C' C-DMA NIELSEN CBL LIVE+7"',1(R4)                             
         BE    FB45                                                             
         CLC   =C' C-DMA NIELSEN CBL-NDM LIVE+7"',1(R4)                         
         BE    FB45                                                             
         MVI   BOOKTYP,BOOKTYPE_WS                                              
         MVC   BOOKTYPA,=C'WS'                                                  
         CLC   =C' C-DMA NIELSEN CBL LIVE+SD"',1(R4)                            
         BE    FB45                                                             
         CLC   =C' C-DMA NIELSEN CBL-NDM LIVE+SD"',1(R4)                        
         BE    FB45                                                             
         MVI   BOOKTYP,C'C'                                                     
         MVC   BOOKTYPA,=C'C '                                                  
         CLC   =C' DMA NIELSEN CBL LIVE+7"',1(R4)                               
         BE    FB45                                                             
         CLC   =C' DMA NIELSEN CBL-NDM LIVE+7"',1(R4)                           
         BE    FB45                                                             
         MVI   BOOKTYP,BOOKTYPE_LS                                              
         MVC   BOOKTYPA,=C'LS'                                                  
         CLC   =C' DMA NIELSEN CBL LIVE+SD"',1(R4)                              
         BE    FB45                                                             
         CLC   =C' DMA NIELSEN CBL-NDM LIVE+SD"',1(R4)                          
         BE    FB45                                                             
         DC    H'0'                INVALID FILE HEADER RECORD                   
*                                                                               
FB45     DS    0H                                                               
         XC    MYSVMKT,MYSVMKT     IN CASE DMA # IS UNKNOWN                     
         MVC   WORK,SPACES                                                      
         L     R1,ARREC                                                         
         LA    R1,5(R1)            BUMP PAST RDW AND 1ST C'"'                   
         L     RF,FULL                                                          
         SR    RF,R1               RF = L'DMA NAME                              
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R1)       WORK CONTAINS DMA NAME                       
         MVC   MKTMSGNM,WORK       SAVE DMA NAME                                
         MVC   LPMMSGNM,WORK                                                    
*                                                                               
         SR    R0,R0               INITIALIZE DMA# TO ZERO                      
         L     R4,ARREC                                                         
         LA    R4,5(R4)            BUMP PAST RDW AND 1ST C'"'                   
         CLI   0(R4),C'"'          FIND END-QUOTE                               
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
         CLI   1(R4),C','          IF A COMMA DOESN'T FOLLOW QUOTE...           
         BE    *+6                 ...THEN THERE'S NO DMA#                      
         DC    H'0'                DMA# IS MANDATORY AS OF SEP/08               
*                                                                               
         GOTO1 =V(NUMVAL),DMCB,2(R4),(2,0)    DMA# FOLLOWS QUOTE-COMMA          
         CLI   DMCB,0              DMA# IS VALID?                               
         BE    *+6                                                              
         DC    H'0'                NO                                           
         L     R0,4(R1)            YES: GET DMA#                                
*                                                                               
         CVD   R0,DUB              SAVE DMA# IN ERROR MESSAGE                   
         UNPK  MKTMSG#,DUB                                                      
         OI    MKTMSG#+2,X'F0'                                                  
*                                                                               
         LARL  R6,FUSMKTS          FIND DMA# IN OUR MARKET TABLE                
         USING FUSMKTD,R6                                                       
FB46     DS    0H                                                               
         CLI   0(R6),X'FF'                                                      
         BE    FB51                UNKNOWN DMA #                                
         CH    R0,FUSMKT#          IS THIS THE MARKET?                          
         BE    *+12                YES                                          
         AHI   R6,FUSMKTLQ                                                      
         B     FB46                                                             
*                                                                               
         TM    FUSFLAGS,FUSLPM     LPM MARKET?                                  
         BO    *+16                YES                                          
         MVC   MYSVMKT,FUSMKT#     SAVE DMA #                                   
         MVC   MYSVMKTC,FUSMKTC    SAVE DMA ALPHA CODE                          
*                                                                               
         CLC   BOOKYM,=AL2(SEP_12) EFFECTIVE WITH SEP/12 SURVEY...              
         BL    FB50                ...MIGHT HAVE TO ADJUST THE BOOKTYPE         
         CLI   BOOKTYP,BOOKTYPE_WS C-DMA LIVE+SD ?                              
         BNE   FB50                NO: LEAVE BOOKTYPE ALONE                     
*                                                                               
         L     RE,ASETMETR         A(SET METERED MARKET TABLE)                  
         USING SETMETRD,RE                                                      
FB47     CLI   0(RE),X'FF'         IF EOT REACHED, THEN...                      
         BE    FB48                ...IT'S DIARY: USE STANDARD BOOKTYPE         
         CLC   SETMKNUM,MYSVMKT    MATCH ON DMA# ?                              
         BE    FB50                YES: IT'S A SET METERED MARKET               
         AH    RE,LSETMETR         BUMP TO NEXT ENTRY                           
         B     FB47                                                             
*                                                                               
FB48     DS    0H                                                               
         MVI   BOOKTYP,0           OVERRIDE METERED MARKETS TO STANDARD         
         MVC   BOOKTYPA,=C'ST'                                                  
         DROP  RE                                                               
*                                                                               
FB50     DS    0H                                                               
         MVC   P(32),=C'BOOK MMM/YY, BTYPE XX, DMA# NNN:'                       
         GOTO1 VDATCON,DMCB,(3,THREE),(6,P+5) BOOK                              
         MVC   P+19(L'BOOKTYPA),BOOKTYPA    ALPHA BOOKTYPE                      
         OC    MYSVMKT,MYSVMKT     WAS DMA # FOUND?                             
         BZ    *+10                NO                                           
         MVC   P+33(L'FUSMKTC),FUSMKTC      ALPHA MARKET CODE                   
         MVC   P+37(L'MKTMSGNM),MKTMSGNM    DMA NAME                            
         EDIT  MYSVMKT,(3,P+28),ZERO=NOBLANK  DMA #                             
         GOTO1 VPRINTER                                                         
         OC    MYSVMKT,MYSVMKT     WAS DMA # FOUND?                             
         BNZ   FB60                YES                                          
*                                                                               
         TM    FUSFLAGS,FUSLPM     LPM MARKET?                                  
         BO    FB52                YES                                          
*                                                                               
FB51     DS    0H                                                               
         MVC   P(MKTMSGLQ),MKTMSG  ERROR MESSAGE                                
         MVC   TEMP(L'ANPREFIX),ANPREFIX  AUTONOTE MESSAGE PREFIX               
         MVC   TEMP+L'ANPREFIX(L'P),P     MESSAGE CONTENT                       
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('ANMAXLEN',TEMP) SEND E-MAIL            
         B     FB55                                                             
*                                                                               
FB52     DS    0H                                                               
         MVC   P(LPMMSGLQ),LPMMSG  ERROR MESSAGE                                
         MVC   TEMP(L'ANPREFIX),ANPREFIX  AUTONOTE MESSAGE PREFIX               
         MVC   TEMP+L'ANPREFIX(L'P),P     MESSAGE CONTENT                       
         GOTO1 VDATAMGR,DMCB,=C'OPMSG',('ANMAXLEN',TEMP) SEND E-MAIL            
*                                                                               
FB55     DS    0H                                                               
         GOTO1 VPRINTER                                                         
         B     READPOP                                                          
         EJECT                                                                  
FB60     DS    0H                  CHECK FOR MARKET FILTER(S)                   
         SR    RF,RF                                                            
         ICM   RF,1,FILTMRKT                                                    
         BZ    READPOP             NO FILTERS PRESENT                           
         LA    R1,FILTMRKT+1                                                    
*                                                                               
         TM    FLAGS1,NEGATIVE_FILTMRKT    NEGATIVE MARKET FILTER LIST?         
         BZ    FB70                                                             
         CLC   MYSVMKT,0(R1)                                                    
         BE    FB80                FILTER OUT THIS MARKET                       
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,*-14                                                          
         B     READPOP                                                          
*                                                                               
FB70     CLC   MYSVMKT,0(R1)                                                    
         BE    READPOP                                                          
         LA    R1,L'INTMRKT(R1)                                                 
         BCT   RF,FB70                                                          
*                                                                               
FB80     XC    MYSVMKT,MYSVMKT     FILTER FAILED: IGNORE THIS MARKET            
*                                                                               
* DEMO/POPULATION RECORDS                                                       
*                                                                               
READPOP  DS    0H                                                               
         LHI   R3,NUMDEMOS                                                      
         LA    R5,UNIVERS                                                       
RP10     L     R4,ARREC                                                         
         XCEF  (R4),2000                                                        
         GET   IN1,(R4)                                                         
         LA    R4,4(R4)            BUMP PAST RDW                                
         OC    0(L'SPACES,R4),SPACES                                            
         CLI   0(R4),C'"'          FIRST CHARACTER MUST BE DOUBLE-QUOTE         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,1(R4)            BUMP PAST FIRST DOUBLE-QUOTE                 
         MVC   DUB,SPACES                                                       
         LA    RF,DUB                                                           
         MVC   0(1,RF),0(R4)       CONSTRUCT DEMO DESCRIPTION IN DUB            
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),C'"'                                                       
         BNE   *-18                                                             
         LA    RE,DUB                                                           
         SR    RF,RE                                                            
         LR    R1,RF               R1 = L'DEMO                                  
         CLM   R1,1,7(R5)          IS L'DEMO WHAT'S EXPECTED?                   
         BE    *+6                                                              
         DC    H'0'                NO                                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),0(R5)        IS DEMO NAME WHAT'S EXPECTED?                
         BE    *+6                                                              
         DC    H'0'                NO                                           
         GOTO1 =V(NUMVAL),DMCB,2(R4),(2,0)                                      
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   8(4,R5),4(R1)                                                    
         LA    R5,L'UNIVERS(R5)                                                 
         BCT   R3,RP10                                                          
*                                                                               
         OC    MYSVMKT,MYSVMKT     IF MARKET IS VALID...                        
         BZ    *+10                ...STORE UNIVERSE VALUE IN TABLE             
         MVC   FUSUNVRS,UNIVERS+8  WIRED UNIVERSE                               
         DROP  R6                                                               
*                                                                               
* STATION AND DAYPART COUNTS                                                    
*                                                                               
         L     R4,ARREC                                                         
         XCEF  (R4),2000                                                        
         GET   IN1,(R4)                                                         
         LA    R4,4(R4)            BUMP PAST RDW                                
         OC    0(L'SPACES,R4),SPACES                                            
         GOTO1 =V(NUMVAL),DMCB,0(R4),(3,X'0000006B')  COMMA-DELIMITED           
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   XNUMSTNS,6(R1)      SAVE NUMBER OF EXPECTED STATIONS             
         L     R0,8(R1)            LENGTH OF FIRST TOKEN                        
         AR    R4,R0               R4 SHOULD NOW POINT TO COMMA                 
         GOTO1 =V(NUMVAL),DMCB,1(R4),(2,0)                                      
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =F'672',4(R1)                                                    
         BE    *+6                                                              
         DC    H'0'                MUST BE 672 DAYPARTS                         
         MVC   QTRHRPWK,6(R1)                                                   
*                                                                               
         MVC   P(18),=C'EXPECTED STATIONS:'                                     
         EDIT  XNUMSTNS,(5,P+19),ALIGN=LEFT                                     
         GOTO1 VPRINTER                                                         
*                                                                               
* HUT/PUT STATION RECORD                                                        
*                                                                               
         L     R4,ARREC                                                         
         XCEF  (R4),2000                                                        
         GET   IN1,(R4)                                                         
         LA    R4,4(R4)            BUMP PAST RDW                                
         OC    0(L'SPACES,R4),SPACES                                            
         GOTO1 =V(NUMVAL),DMCB,0(R4),(3,X'0000006B')  COMMA-DELIMITED           
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   =F'-1',4(R1)        -1 DENOTES HUT/PUT                           
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,8(R1)            LENGTH OF FIRST TOKEN                        
         AR    R4,RF               R4 SHOULD NOW POINT TO COMMA                 
         LA    R4,1(R4)                                                         
         CLC   =C'"HUT/PVT"',0(R4)                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   MYSVSTA,=C'HUT T'                                                
*                                                                               
         MVC   STACOUNT,=H'1'                                                   
*                                                                               
         XC    0(INTRKYLQ,R2),0(R2)                                             
         XC    INTRECLN,INTRECLN                                                
         MVC   INTRECLN(2),=Y(L'INTKEY+L'INTVALS) RECORD LENGTH                 
*                                                                               
*                                  FILTER FIELDS                                
         MVI   INTRTYP,C'M'        RECORD TYPE                                  
         MVC   INTMRKT,MYSVMKT     MARKET NUMBER                                
         MVC   INTSTA,MYSVSTA      STATION                                      
         MVC   INTBOOK,BOOKYM      BOOK                                         
         MVC   INTBTYP,BOOKTYP     BOOK TYPE                                    
         MVI   INTSPILL,C'Y'       SPILL MARKET                                 
*                                                                               
*                                  SORT KEY FIELDS                              
         MVC   INTSRTYP,INTRTYP    RECORD TYPE                                  
         MVI   INTSMED,C'T'        MEDIA 'T'                                    
         MVI   INTSSRC,C'F'        SOURCE 'F' (FUSION)                          
         MVC   INTSSTA,INTSTA      STATION                                      
         MVC   INTSBOOK,INTBOOK    BOOK                                         
         MVC   INTSBTYP,BOOKTYP    BOOK TYPE                                    
         MVC   INTSMKT,INTMRKT     MARKET NUMBER                                
*                                                                               
         OC    MYSVMKT,MYSVMKT     WAS DMA # FOUND PREVIOUSLY?                  
         BZ    OPENOK              NO                                           
         BAS   RE,ADJACCS          YES: RETURN ADJUSTED REC TO DEMCNV           
         B     EXIT                                                             
         EJECT                                                                  
OPENOK   DS    0H                                                               
         L     R4,ARREC                                                         
         XCEF  (R4),2000                                                        
         GET   IN1,(R4)                                                         
         LA    R4,4(R4)            BUMP PAST RDW                                
         OC    0(L'SPACES,R4),SPACES                                            
*                                                                               
* AT THE COMPLETION OF THE LAST STATION, READ THE NEXT MARKET                   
*                                                                               
         CLC   STACOUNT,XNUMSTNS   LAST STATION?                                
         BNE   CHKREC              NO                                           
         CLC   DPTCOUNT,QTRHRPWK   ALL 1/4-HOURS READ?                          
         BNE   CHKREC              NO                                           
*                                                                               
* NEXT RECORD IS EITHER THE START OF A NEW MARKET, OR IT'S A DEMO               
* RECORD (BECAUSE THERE'S A SPLIT FOR THE FINAL 1/4-HOUR OF THE FINAL           
* DAYPART). IF THE STRING "DMA NIELSEN" IS FOUND IN THE RECORD, IT'S            
* THE START OF A NEW MARKET.                                                    
*                                                                               
         LR    RE,R4                                                            
         LHI   R0,80               NO NEED TO LOOK PAST 80 BYTES IN             
         CLC   =C'DMA NIELSEN',0(RE)                                            
         BE    *+16                IT'S A NEW MARKET                            
         LA    RE,1(RE)                                                         
         BCT   R0,*-14                                                          
         B     CHKREC              DEMO RECORD FROM THE FINAL 1/4-HOUR          
*                                                                               
         XC    DPTCOUNT,DPTCOUNT   RESET DAYPART COUNTER                        
         XC    LASTDYQH,LASTDYQH   CLEAR 'LAST DAY/QH SEEN'                     
*                                                                               
         BAS   RE,MKTDONE          PRINT END-OF-MARKET MESSAGE                  
         B     FINDBOOK            READ THE NEXT MARKET                         
*                                                                               
CHKREC   DS    0H                                                               
         CLI   0(R4),C'"'          IF RECORD BEGINS W/ DOUBLE-QUOTE...          
         BE    DAYDEMOS            ...THEN IT'S A DEMO RECORD                   
*                                                                               
* STATION ID AND NAME                                                           
*                                                                               
         CLC   DPTCOUNT,QTRHRPWK   DID WE SEE EXACTLY 672 DAYPARTS?             
         BE    *+6                                                              
         DC    H'0'                NO                                           
*                                                                               
         XC    DPTCOUNT,DPTCOUNT   RESET DAYPART COUNTER                        
         XC    LASTDYQH,LASTDYQH   CLEAR 'LAST DAY/QH SEEN'                     
*                                                                               
* FORMAT OF STATION RECORD IS AS FOLLOWS:                                       
*  1. COLUMNS 1 THROUGH 4 (OR 5) MUST BE NUMERIC CHARACTERS                     
*  2. THIS MUST BE FOLLOWED BY COMMA/DOUBLE-QUOTE                               
*  3. CALL LETTERS FOLLOW THIS AND MUST END WITH "-TV" AND DOUBLE-QUOTE         
*                                                                               
         MVI   LOADSTA,C'Y'        ASSUME WE WILL LOAD THE STATION              
*                                                                               
         XC    MYSVSTA,MYSVSTA     IN CASE STATION RECORD IS INVALID            
         GOTO1 =V(NUMVAL),DMCB,0(R4),(3,X'0000006B')  COMMA-DELIMITED           
         CLI   DMCB,0                                                           
         BE    STN03               FIRST FIELD IS NUMERIC                       
         MVC   DUB,=CL8'NOT NUM.'                                               
         MVI   LOADSTA,C'N'                                                     
         B     STNBAD              INVALID: FIRST FIELD NOT NUMERIC             
*                                                                               
STN03    DS    0H                                                               
         CLC   =F'4',DMCB+8                                                     
         BE    STN05               MUST BE PRECISELY 4 OR 5 DIGITS              
         CLC   =F'5',DMCB+8                                                     
         BE    STN05                                                            
         MVC   DUB,=CL8'INV. #'                                                 
         MVI   LOADSTA,C'N'                                                     
         B     STNBAD                                                           
*                                                                               
STN05    DS    0H                                                               
         LR    RF,R4                                                            
         A     RF,DMCB+8           RF = A(NEXT CHARACTER)                       
         CLC   =C',"',0(RF)        THIS MUST BE FOLLOWED BY ',"'                
         BE    STN07                                                            
         MVC   DUB,=CL8'BAD FMT'                                                
         MVI   LOADSTA,C'N'                                                     
         B     STNBAD                                                           
*                                                                               
STN07    DS    0H                                                               
         MVC   MYSVSTAB,DMCB+4     BINARY STATION (DISTRIBUTOR) CODE            
         MVC   MYSVSTAA,SPACES                                                  
         SR    R0,R0               COUNT CHARACTERS IN CALL LETTERS             
         LA    RE,2(RF)            START OF ALPHA CALL LETTERS IN IREC          
         LA    RF,MYSVSTAA         SAVE CALL LETTERS HERE                       
STN10    DS    0H                                                               
         CLI   0(RE),C'A'                                                       
         BL    STN20               END OF CALL LETTERS                          
         AHI   R0,1                                                             
         CHI   R0,4                                                             
         BNH   STN15                                                            
         MVC   DUB,=CL8'BAD LTRS'                                               
         MVI   LOADSTA,C'N'                                                     
         B     STNBAD              TOO MANY CHARACTERS IN CALL LETTERS          
*                                                                               
STN15    DS    0H                                                               
         MVC   0(1,RF),0(RE)                                                    
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         B     STN10                                                            
*                                                                               
STN20    DS    0H                                                               
         CLC   =C'-TV',0(RE)       MUST END WITH "-TV"                          
         BE    STN30                                                            
         CLC   =C' -TV',0(RE)                                                   
         BE    STN30                                                            
         CLC   =C'  -TV',0(RE)                                                  
         BE    STN30                                                            
         MVI   LOADSTA,C'N'                                                     
         MVC   DUB,=CL8'NO "-TV"'                                               
         B     STNBAD                                                           
*                                                                               
STN30    DS    0H                                                               
         L     RE,ANSICABT         A(NSICABLE) TABLE FROM DEMTABS               
         LH    R0,LNSICABT         L'TABLE ENTRY                                
STN40    DS    0H                                                               
         USING NSICBLD,RE                                                       
         CLC   NSICBNML,MYSVSTAB+1 MATCH ON DISTRIBUTOR (NUMERIC) CODE?         
         BE    STNOK               YES                                          
         AR    RE,R0                                                            
         CLI   0(RE),X'FF'         EOT?                                         
         BNE   STN40               NO: TRY NEXT STATION                         
         MVC   DUB,=CL8'BAD CODE'  NO TABLE ENTRY FOUND                         
         DROP  RE                                                               
*                                                                               
STNBAD   DS    0H                                                               
         MVC   STAMSGRC,0(R4)      FIRST 20 BYTES OF BAD RECORD                 
         MVC   STAMSGRS,DUB        "DUB" CONTAINS ERROR REASON                  
         LH    R0,MYSVMKT          DMA#                                         
         CVD   R0,DUB                                                           
         UNPK  STAMSGM#,DUB                                                     
         OI    STAMSGM#+2,X'F0'                                                 
         MVC   STAMSGMC,MYSVMKTC   ALPHA DMA CODE                               
         MVC   STAMSGBT,BOOKTYPA   BOOKTYPE                                     
         GOTO1 VDATCON,DMCB,(3,BOOKYM),(6,STAMSGBK)   BOOK                      
         MVC   P(STAMSGLQ),STAMSG  ERROR MESSAGE                                
         MVC   TEMP(L'ANPREFIX),ANPREFIX  AUTONOTE MESSAGE PREFIX               
         MVC   TEMP+L'ANPREFIX(L'P),P     MESSAGE CONTENT                       
         GOTO1 VPRINTER                                                         
*********GOTO1 VDATAMGR,DMCB,=C'OPMSG',('ANMAXLEN',TEMP) SEND E-MAIL            
*                                                                               
         CLI   LOADSTA,C'Y'        SHOULD WE LOAD THE STATION ANYWAY?           
         BNE   BUMPSTA             NO: IGNORE THIS STATION                      
*                                                                               
STNOK    DS    0H                                                               
         L     R0,MYSVSTAB         STATION NUMBER (DISTRIBUTOR CODE)            
         CVD   R0,DUB                                                           
         CHI   R0,9999             CODE > 9999 ?                                
         BNH   *+12                                                             
         STCM  R0,15,MYSVSTA       YES: STORE IN BINARY                         
         B     *+14                                                             
         UNPK  MYSVSTA(4),DUB      STATION NAME IS NNNNT                        
         OI    MYSVSTA+3,X'F0'                                                  
         MVI   MYSVSTA+4,C'T'      ALWAYS SUFFIX WITH "T"                       
*                                                                               
*&&DO                                                                           
* FOR DEBUGGING                                                                 
         MVC   P(20),=C'CONVERTING MKT/STA: '                                   
         LH    R0,MYSVMKT                                                       
         CVD   R0,DUB                                                           
         UNPK  P+20(4),DUB                                                      
         OI    P+23,X'F0'                                                       
         MVC   P+25(5),MYSVSTA                                                  
         MVC   P+31(4),MYSVSTAA                                                 
         GOTO1 VPRINTER                                                         
*&&                                                                             
         XC    0(INTRKYLQ,R2),0(R2)                                             
         XC    INTRECLN,INTRECLN                                                
         MVC   INTRECLN(2),=Y(L'INTKEY+L'INTVALS) RECORD LENGTH                 
*                                                                               
*                                  FILTER FIELDS                                
         MVI   INTRTYP,C'M'        RECORD TYPE                                  
         MVC   INTMRKT,MYSVMKT     MARKET NUMBER                                
         MVC   INTSTA,MYSVSTA      STATION                                      
         MVC   INTBOOK,BOOKYM      BOOK                                         
         MVC   INTBTYP,BOOKTYP     BOOK TYPE                                    
         MVI   INTSPILL,C'Y'       SPILL MARKET                                 
*                                                                               
*                                  SORT KEY FIELDS                              
         MVC   INTSRTYP,INTRTYP    RECORD TYPE                                  
         MVI   INTSMED,C'T'        MEDIA 'T'                                    
         MVI   INTSSRC,C'F'        SOURCE 'F' (FUSION)                          
         MVC   INTSSTA,INTSTA      STATION                                      
         MVC   INTSBOOK,INTBOOK    BOOK                                         
         MVC   INTSBTYP,BOOKTYP    BOOK TYPE                                    
         MVC   INTSMKT,INTMRKT     MARKET NUMBER                                
*                                                                               
BUMPSTA  DS    0H                                                               
         LH    R0,STACOUNT                                                      
         AHI   R0,1                                                             
         STH   R0,STACOUNT                                                      
*                                                                               
         OC    MYSVSTA,MYSVSTA     IS THIS STATION VALID?                       
         BZ    OPENOK              NO: GET THE NEXT INPUT RECORD                
         OC    MYSVMKT,MYSVMKT     WAS DMA # FOUND PREVIOUSLY?                  
         BZ    OPENOK              NO: GET THE NEXT INPUT RECORD                
         BAS   RE,ADJACCS          YES: RETURN ADJUSTED REC TO DEMCNV           
         B     EXIT                                                             
         EJECT                                                                  
* DAYPART NAME, DEMO IMPRESSIONS                                                
*                                                                               
DAYDEMOS DS    0H                                                               
         XC    0(INTRKYLQ,R2),0(R2)                                             
         XC    INTRECLN,INTRECLN                                                
         MVC   INTRECLN(2),=Y(INTRECLQ)                                         
*                                                                               
*                                  FILTER FIELDS                                
         MVI   INTRTYP,C'R'        RECORD TYPE                                  
         MVC   INTMRKT,MYSVMKT     MARKET NUMBER                                
         MVC   INTSTA,MYSVSTA      STATION                                      
         MVC   INTBOOK,BOOKYM      BOOK                                         
         MVC   INTBTYP,BOOKTYP     BOOKTYPE                                     
         MVI   INTSPILL,C'Y'       SPILL MARKET                                 
*                                                                               
*                                  SORT KEY FIELDS                              
         MVC   INTSRTYP,INTRTYP    RECORD TYPE                                  
         MVI   INTSMED,C'T'        MEDIA 'T'                                    
         MVI   INTSSRC,C'F'        SOURCE 'F' (FUSION)                          
         MVC   INTSSTA,INTSTA      STATION                                      
         MVC   INTSBOOK,INTBOOK    BOOK                                         
         MVC   INTSBTYP,BOOKTYP    BOOK TYPE                                    
         MVC   INTSMKT,INTMRKT     MARKET NUMBER                                
*                                                                               
         LA    R4,1(R4)            1ST DOUBLE-QUOTE                             
         MVC   DUB,SPACES                                                       
         LA    RF,DUB                                                           
         MVC   0(1,RF),0(R4)       CONSTRUCT DAY EXPRESSION IN DUB              
         LA    RF,1(RF)                                                         
         LA    R4,1(R4)                                                         
         CLI   0(R4),C' '                                                       
         BNE   *-18                                                             
         LA    RE,DUB                                                           
         SR    RF,RE                                                            
*                                                                               
         LA    RE,DAYTAB           VALIDATE DAY EXPRESSION                      
DAY10    LLC   R1,9(RE)            L'DAY NAME                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DUB(0),0(RE)                                                     
         BE    *+18                GOT IT                                       
         LA    RE,L'DAYTAB(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   DAY10                                                            
         DC    H'0'                INVALID DAY EXPRESSION                       
         MVC   INTDAYWK,10(RE)     DAY CODE                                     
         MVC   INTSDAY,INTDAYWK                                                 
*                                                                               
         LA    R4,1(R4)            BUMP TO START OF TIME EXPRESSION             
         MVC   WORK,SPACES                                                      
         LA    RF,WORK+3           BUILD IN WORK+3 (LEAVE ROOM FOR DAY)         
*                                                                               
TIM10    DS    0H                                                               
         MVC   0(1,RF),0(R4)                                                    
         LA    RF,1(RF)                                                         
TIM20    LA    R4,1(R4)                                                         
         CLI   0(R4),C'"'          END OF EXPRESSION?                           
         BE    *+16                                                             
         CLI   0(R4),C':'          TIMVAL CAN'T HANDLE COLONS                   
         BE    TIM20                                                            
         B     TIM10                                                            
*                                                                               
         LA    RE,WORK+3                                                        
         SR    RF,RE                                                            
         LR    R0,RF                                                            
         GOTO1 VTIMVAL,DMCB,((R0),WORK+3),FULL                                  
         CLI   0(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHRTOQH,DMCB,FULL,INTSQH                                         
         MVC   INTEQH,INTSQH       END 1/4 HOUR IS SAME AS START                
         MVC   INTSSQH,INTSQH                                                   
         MVC   INTSCALL,MYSVSTAA   ALPHA STATION CALL LETTERS                   
*                                                                               
         CLC   INTSDYQH,LASTDYQH   SAME DAY/QH?                                 
         BE    TIM30                                                            
         MVC   LASTDYQH,INTSDYQH   NO: REMEMBER THE NEW ONE                     
         LH    RF,DPTCOUNT         BUMP THE DAYPART COUNTER                     
         AHI   RF,1                                                             
         STH   RF,DPTCOUNT                                                      
*                                                                               
TIM30    DS    0H                                                               
         LA    R4,3(R4)            BUMP PAST DOUBLE-QUOTE/COMMA/BLANK           
*                                                                               
         MVI   INTSWKS,0           SET UP ACTIVE WEEKS                          
         SR    R0,R0               COUNT THE NUMBER OF WEEKS                    
         OC    0(8,R4),=C'00000000'                                             
*                                                                               
         CLC   =C'00',0(R4)        WEEK 1 INCLUDED?                             
         BE    *+12                                                             
         OI    INTSWKS,X'08'       YES                                          
         AHI   R0,1                                                             
         CLC   =C'00',2(R4)        WEEK 2 INCLUDED?                             
         BE    *+12                                                             
         OI    INTSWKS,X'04'       YES                                          
         AHI   R0,1                                                             
         CLC   =C'00',4(R4)        WEEK 3 INCLUDED?                             
         BE    *+12                                                             
         OI    INTSWKS,X'02'       YES                                          
         AHI   R0,1                                                             
         CLC   =C'00',6(R4)        WEEK 4 INCLUDED?                             
         BE    *+12                                                             
         OI    INTSWKS,X'01'       YES                                          
         AHI   R0,1                                                             
         STC   R0,INTS#WKS         NUMBER OF WEEKS FOR THIS PROGRAM             
*                                                                               
         LHI   R0,L'INTPNAM        PREVENT OVERFLOW OF PROGRAM NAME FLD         
         LA    RE,INTPNAM                                                       
         MVC   INTPNAM,SPACES      INITIALIZE PROGRAM NAME TO BLANKS            
         CLI   0(R4),C'"'          FIND PROGRAM NAME                            
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         CLI   1(R4),C'"'          IS A PROGRAM NAME PRESENT?                   
         BNE   PGMNAM10                                                         
         MVC   INTPNAM,=C'NOT AVAILABLE '                                       
         LA    R4,1(R4)            POINT TO FINAL QUOTE                         
         B     PGMNAM20                                                         
*                                                                               
PGMNAM10 DS    0H                                                               
         LA    R4,1(R4)            BUMP PAST DOUBLE-QUOTE                       
         CLI   0(R4),C'"'          END OF PROGRAM NAME?                         
         BE    PGMNAM20                                                         
         MVC   0(1,RE),0(R4)       MOVE IN ONE BYTE AT A TIME                   
         LA    RE,1(RE)                                                         
         BCT   R0,PGMNAM10         PREVENT OVERFLOW OF PROGRAM NAME FLD         
*                                                                               
PGMNAM20 DS    0H                                                               
         CLI   0(R4),C'"'          MAKE SURE WE POINT TO FINAL QUOTE            
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         B     *-12                                                             
*                                                                               
         LA    R4,2(R4)            BUMP PAST DOUBLE-QUOTE AND COMMA             
         LHI   R0,NUMDEMOS                                                      
         LA    R3,INTACCS                                                       
IMPS     CLI   0(R4),C' '          BUMP PAST BLANK(S)                           
         BNE   *+12                                                             
         LA    R4,1(R4)                                                         
         B     IMPS                                                             
         MVC   DMCB+4(4),=X'0300006B' ALL IMPRESSIONS EXCEPT LAST...            
         CHI   R0,1                   ... ARE COMMA-DELIMITED                   
         BNE   *+10                                                             
         MVC   DMCB+4(4),=X'02000000' LAST ONE IS BLANK-DELIMITED               
         GOTO1 =V(NUMVAL),DMCB,0(R4)                                            
         CLI   DMCB,0                                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R3),4(R1)       SAVE IMPRESSIONS                             
         L     RF,8(R1)            LENGTH OF FIRST TOKEN                        
         AR    R4,RF               R4 SHOULD NOW POINT TO COMMA                 
         LA    R4,1(R4)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,IMPS                                                          
*                                                                               
         MVC   INTSHIMP,INTACCS    THE HOMES VALUE (TO BREAK TIES ON            
*                                   A 2-2 SPLIT)                                
*                                                                               
         CLC   STACOUNT,=H'1'      ARE WE READING THE HUTS?                     
         BNE   HUTS                NO                                           
         LH    RE,DPTCOUNT         INDEX INTO HUTTABLE                          
         BCTR  RE,0                TABLE IS ZERO-BASED                          
         MHI   RE,NUMDEMOS*4       DISPLACEMENT TO CORRECT ROW                  
         LA    RE,HUTTABLE(RE)     A(ROW)                                       
         LA    R3,INTACCS                                                       
         LHI   R0,NUMDEMOS                                                      
         MVC   0(4,RE),0(R3)       COPY IMPRESSIONS INTO HUTTABLE               
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,*-14                                                          
*                                                                               
HUTS     DS    0H                                                               
         LH    RE,DPTCOUNT         INDEX INTO HUTTABLE                          
         BCTR  RE,0                TABLE IS ZERO-BASED                          
         MHI   RE,NUMDEMOS*4       DISPLACEMENT TO CORRECT ROW                  
         LA    RE,HUTTABLE(RE)     A(ROW)                                       
         LA    R3,INTACCS+(NUMDEMOS*8)  POINT TO HUT COLUMNS                    
         LHI   R0,NUMDEMOS                                                      
         MVC   0(4,R3),0(RE)       COPY IMPRESSIONS INTO ACCUMS                 
         LA    RE,4(RE)                                                         
         LA    R3,4(R3)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         LA    RE,UNIVERS          A(UNIVERSE TABLE)                            
         LA    R3,INTACCS+(NUMDEMOS*4)  POINT TO UNIVERSE COLUMNS               
         LHI   R0,NUMDEMOS                                                      
         MVC   0(4,R3),8(RE)       COPY UNIVERSES INTO ACCUMS                   
         LA    RE,L'UNIVERS(RE)                                                 
         LA    R3,4(R3)                                                         
         BCT   R0,*-14                                                          
*                                                                               
         OC    MYSVSTA,MYSVSTA     IS THIS STATION VALID?                       
         BZ    OPENOK              NO: GET THE NEXT INPUT RECORD                
         OC    MYSVMKT,MYSVMKT     WAS DMA # FOUND PREVIOUSLY?                  
         BZ    OPENOK              NO: GET THE NEXT INPUT RECORD                
         BAS   RE,ADJACCS          YES: RETURN ADJUSTED REC TO DEMCNV           
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*CNVWR  - SORT IRECS TO CREATE SREC FOR OPHASE                                  
***********************************************************************         
CNVWR    B     EXIT                                                             
*                                                                               
***********************************************************************         
*END OF TAPE HANDLING                                                           
***********************************************************************         
MORET    DS    0H                                                               
         BAS   RE,MKTDONE          PRINT END-OF-MARKET MESSAGE                  
*                                                                               
         CLOSE (IN1,REWIND)                                                     
         MVI   INTAPESW,X'02'                                                   
*                                                                               
EXIT     XMOD1                                                                  
         SPACE 2                                                                
***********************************************************************         
*PRINT MESSAGE AT END OF MARKET                                                 
***********************************************************************         
MKTDONE  NTR1                                                                   
*                                                                               
         MVC   P(31),=C'READ COMPLETE: MARKET PROCESSED'                        
         OC    MYSVMKT,MYSVMKT                                                  
         BNZ   *+10                                                             
         MVC   P(31),=C'READ COMPLETE: MARKET IGNORED  '                        
         GOTO1 VPRINTER                                                         
         MVI   P,0                 SKIP A LINE                                  
         GOTO1 VPRINTER                                                         
*                                                                               
         CP    NUMMKTS,=P'1'       IS THIS THE FIRST MARKET?                    
         BNE   MKTDONEX                                                         
*                                                                               
         OPEN  (ETAPE,OUTPUT)                                                   
*                                                                               
         GOTO1 VDATCON,DMCB,(3,BOOKYM),(6,MAILBOOK)   BOOK                      
         PUT   ETAPE,MAILSUBJ      E-MAIL SUBJECT                               
*                                                                               
* AT LEAST ONE LINE OF BODY TEXT IN THE E-MAIL IS REQUIRED. WHEN DEIS           
* INVESTIGATED THIS IN NOV/11, HE FOUND THAT THERE WERE AT LEAST TWO            
* PROBLEMS IF THERE IS A SUBJECT WITH NO BODY LINES:                            
*  1. DMPRTQXFR/DMPRTQXMQM. FOR SOME REASON, NO REPORT IS WRITTEN TO            
*     THE PRINT QUEUE FOR EDICT TO FIND.                                        
*  2. EVEN IF A REPORT WERE WRITTEN TO THE PQ, EDICT KICKS OUT REPORTS          
*     THAT HAVE NO BODY LINES.                                                  
* SO IN THE INTEREST OF MOVING THINGS ALONG, WE JUST WRITE A SINGLE             
* LINE OF TEXT THAT IS IDENTICAL TO THE SUBJECT.                                
*                                                                               
         XC    WORK,WORK           ONE LINE OF E-MAIL BODY IS REQUIRED          
         MVC   WORK(2),=AL2(L'MAILTEXT+L'MAILBOOK+4)                            
         MVC   WORK+4(L'MAILTEXT),MAILTEXT                                      
         MVC   WORK+4+L'MAILTEXT(L'MAILBOOK),MAILBOOK                           
         PUT   ETAPE,WORK                                                       
*                                                                               
         CLOSE ETAPE                                                            
*                                                                               
MKTDONEX DS    0H                                                               
         AP    NUMMKTS,=P'1'       INCREMENT RELATIVE MARKET NUMBER             
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* ADJUST DEMO SEQUENCE IN INTACCS                                               
***********************************************************************         
ADJACCS  NTR1                                                                   
*                                                                               
* IN THE ORIGINAL VERSION OF THE INPUT FILE, THE TWO CHILD DEMOS                
* FOLLOWED IMMEDIATELY AFTER THE HH VALUE. IN JAN/06, STRATA CHANGED            
* THIS FORMAT TO PUT THE TWO CHILD DEMOS AFTER THE WOMEN DEMOS. THE             
* DEMDISP TABLES, HOWEVER, STILL HAVE THE DEMOS IN THE ORIGINAL                 
* SEQUENCE (BECAUSE WE DIDN'T WANT OR NEED TO FIX THE PRE-EXISTING              
* RECORDS). SO THIS PROGRAM RESEQUENCES THE DEMOS THAT ARE READ FROM            
* THE INPUT FILE INTO THE OLD FORMAT BEFORE PASSING THEM BACK TO                
* DEMCNV.                                                                       
*                                                                               
*    OLD         NEW                                                            
*    ---         ---                                                            
*    HH          HH                                                             
*    C 2-5       M 12-17                                                        
*    C 6-11      M 18-20                                                        
*    M 12-17     M 21-24                                                        
*    M 18-20     M 25-34                                                        
*    M 21-24     M 35-49                                                        
*    M 25-34     M 50-54                                                        
*    M 35-49     M 55-64                                                        
*    M 50-54     M 65+                                                          
*    M 55-64     W 12-17                                                        
*    M 65+       W 18-20                                                        
*    W 12-17     W 21-24                                                        
*    W 18-20     W 25-34                                                        
*    W 21-24     W 35-49                                                        
*    W 25-34     W 50-54                                                        
*    W 35-49     W 55-64                                                        
*    W 50-54     W 65+                                                          
*    W 55-64     C 2-5                                                          
*    W 65+       C 6-11                                                         
*                                                                               
         LA    R5,INTACCS+4             IMPRESSIONS                             
         BAS   RE,ADJUST                                                        
         LA    R5,INTACCS+4+(19*4)      UNIVERSES                               
         BAS   RE,ADJUST                                                        
         LA    R5,INTACCS+4+(19*4*2)    PUTS                                    
         BAS   RE,ADJUST                                                        
         B     ADJX                                                             
*                                                                               
ADJUST   MVC   DUB,(4*16)(R5)           SAVE C2-5 AND C6-11 DEMO VALUES         
         LA    R3,(4*16)-1(R5)          A(LAST BYTE OF W65+ VALUE)              
         LHI   R0,4*16                  16 DEMOS TO SHIFT                       
         MVC   8(1,R3),0(R3)            SHIFT RIGHT, ONE BYTE AT A TIME         
         BCTR  R3,0                                                             
         BCT   R0,*-8                                                           
         MVC   0(8,R5),DUB              PUT C2-5 AND C6-11 BACK IN              
         BR    RE                                                               
*                                                                               
ADJX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IN1      DCB   DDNAME=IN1,DSORG=PS,MACRF=GM,EODAD=MORET,LRECL=4004,    +        
               RECFM=VB                                                         
         SPACE 2                                                                
ETAPE    DCB   DDNAME=ETAPE,DSORG=PS,RECFM=VB,LRECL=250,MACRF=PM                
         SPACE 2                                                                
ADEMTABS DS    A                   A(DEMTABS)                                   
NUMMKTS  DC    PL3'1'              CURRENT RELATIVE MARKET NUMBER               
MYSVSTAB DS    F                   BINARY STATION (DISTRIBUTOR CODE)            
ANSICABT DS    A                   A(NSICABLE) TABLE FROM DEMTABS               
ASETMETR DS    A                   A(SET METERED MKTS) FROM DEMTABS             
LNSICABT DS    H                   L'(NSICABLE TABLE ENTRY)                     
LSETMETR DS    H                   L'(SETMETER TABLE ENTRY)                     
QTRHRPWK DS    H                   QUARTER-HOURS PER WEEK (672)                 
XNUMSTNS DS    H                   EXPECTED # OF STATIONS IN INPUT FILE         
STACOUNT DS    H                   ACTUAL # OF STATIONS                         
DPTCOUNT DS    H                   CURRENT DAYPART NUMBER (1-BASED)             
MYSVMKT  DS    H                   DMA NUMBER (OR NULLS IF INVALID)             
MYSVMKTC DS    CL3                 ALPHA DMA CODE                               
MYSVSTA  DS    CL5                 NUMERIC STATION (DISTRIBUTOR CODE)           
MYSVSTAA DS    CL4                 ALPHA CALL LETTERS                           
BOOKYM   DC    XL2'0000'           RATING BOOK (BINARY Y/M)                     
BOOKTYP  DS    X                   BOOK TYPE                                    
BOOKTYPA DS    CL2                 ALPHA BOOK TYPE                              
LASTDYQH DS    XL2                 LAST DAY/QH SEEN                             
THREE    DS    XL3                                                              
LOADSTA  DS    C                                                                
         EJECT                                                                  
ANPREFIX DC    C'AUTONOTE*US-DEMOSTEAM:'                                        
ANMAXLEN EQU   105                 AUTONOTE MAX LENGTH                          
*                                                                               
MAILSUBJ DC    H'84'               L'RECORD + L'RDW                             
         DC    H'0'                                                             
         DC    CL80' '                                                          
         ORG   *-80                                                             
         DC    C'SUBJECT='                                                      
MAILTEXT DC    C'NCC FUSION DATA LOADED FOR '                                   
MAILBOOK DC    C'MMM/YY'                                                        
         ORG                                                                    
*                                                                               
MKTMSG   DC    C'** FUSION UNKNOWN MARKET '                                     
MKTMSG#  DC    CL3' '              MARKET NUMBER IS INSERTED HERE               
         DC    C' '                                                             
MKTMSGNM DS    CL30                MARKET NAME                                  
MKTMSGLQ EQU   *-MKTMSG                                                         
*                                                                               
LPMMSG   DC    C'** FUSION LPM MARKET: '                                        
LPMMSGNM DS    CL30                MARKET NAME                                  
LPMMSGLQ EQU   *-LPMMSG                                                         
*                                                                               
STAMSG   DC    C'** FUSION INVALID STATION: '                                   
STAMSGRC DS    CL20                FIRST 20 CHARS OF STATION RECORD             
         DC    C' IN DMA# '                                                     
STAMSGM# DS    CL3                 DMA NUMBER                                   
         DC    C' '                                                             
         DC    C'('                                                             
STAMSGMC DS    CL3                 DMA ALPHA CODE                               
         DC    C') '                                                            
STAMSGBK DS    CL6                 BOOK                                         
         DC    C' BT='                                                          
STAMSGBT DS    CL2                 BOOKTYPE                                     
         DC    C' '                                                             
STAMSGRS DS    CL8                 REASON FOR ERROR                             
STAMSGLQ EQU   *-STAMSG                                                         
         EJECT                                                                  
DAYTAB   DS    0XL11                                                            
* BYTES 1-9: FULL DAY NAME                                                      
* BYTE  10:  L'NAME (FOR EXECUTED CLC)                                          
* BYTE  11:  DEMO FILE ENCODED DAY CODE                                         
         DC    C'MONDAY   ',AL1(6),X'10'                                        
         DC    C'TUESDAY  ',AL1(7),X'20'                                        
         DC    C'WEDNESDAY',AL1(9),X'30'                                        
         DC    C'THURSDAY ',AL1(8),X'40'                                        
         DC    C'FRIDAY   ',AL1(6),X'50'                                        
         DC    C'SATURDAY ',AL1(8),X'60'                                        
         DC    C'SUNDAY   ',AL1(6),X'70'                                        
         DC    X'FF'                                                            
         EJECT                                                                  
* TABLE OF EXPECTED DEMO CATEGORIES AND UNIVERSES FROM INPUT FILE.              
*  CL7: DEMO                                                                    
*  AL1: L'DEMO CATEGORY DESCRIPTION                                             
*  F:   UNIVERSE                                                                
*                                                                               
* NOTE: SEE COMMENT IN ROUTINE ADJACCS                                          
*                                                                               
         DS    0D                                                               
UNIVERS  DS    0CL12                                                            
         DC    C'HH     ',AL1(2),F'0'                                           
         DC    C'M 12-17',AL1(7),F'0'                                           
         DC    C'M 18-20',AL1(7),F'0'                                           
         DC    C'M 21-24',AL1(7),F'0'                                           
         DC    C'M 25-34',AL1(7),F'0'                                           
         DC    C'M 35-49',AL1(7),F'0'                                           
         DC    C'M 50-54',AL1(7),F'0'                                           
         DC    C'M 55-64',AL1(7),F'0'                                           
         DC    C'M 65+  ',AL1(5),F'0'                                           
         DC    C'W 12-17',AL1(7),F'0'                                           
         DC    C'W 18-20',AL1(7),F'0'                                           
         DC    C'W 21-24',AL1(7),F'0'                                           
         DC    C'W 25-34',AL1(7),F'0'                                           
         DC    C'W 35-49',AL1(7),F'0'                                           
         DC    C'W 50-54',AL1(7),F'0'                                           
         DC    C'W 55-64',AL1(7),F'0'                                           
         DC    C'W 65+  ',AL1(5),F'0'                                           
         DC    C'C 2-5  ',AL1(5),F'0'                                           
         DC    C'C 6-11 ',AL1(6),F'0'                                           
NUMDEMOS EQU   (*-UNIVERS)/L'UNIVERS                                            
         SPACE 3                                                                
         DS    0D                                                               
         DC    C'HUTTABLE'                                                      
HUTTABLE DS    (24*7*4*NUMDEMOS)F  672 QUARTER-HOURS                            
         EJECT                                                                  
* ATLANTA HAS BEEN DELIBERATELY REMOVED FROM THIS TABLE, BECAUSE OF THE         
* CONFUSION WITH ITS DMA NUMBER HAVING CHANGED. SINCE IT'S BEEN LPM             
* SINCE JUL/06, WE DON'T NEED IT HERE. OTHER LPM MARKETS ARE HERE, BUT          
* WE IDENTIFY THOSE IN THE CODE ABOVE, AND THEY ARE IGNORED. THEY ARE           
* KEPT IN THIS TABLE ONLY IN CASE WE NEED TO BACKLOAD OLD DATA FOR ANY          
* MONTHS PRIOR TO THEIR GOING LPM.                                              
*                                                                               
         DS    0D                                                               
         DC    C'*FUSMKTS'                                                      
FUSMKTS  DC    0C                  FUSION MKTS WITH NIELSEN DMA NUMBERS         
         DC    HL2'100',C'POP',X'00',FL4'0'                                     
         DC    HL2'101',C'NYK',X'00',FL4'0'                                     
         DC    HL2'102',C'BIN',X'00',FL4'0'                                     
         DC    HL2'103',C'MAC',X'00',FL4'0'                                     
         DC    HL2'104',C'PHI',X'00',FL4'0'                                     
         DC    HL2'105',C'DET',X'00',FL4'0'                                     
         DC    HL2'106',C'BOS',X'00',FL4'0'                                     
         DC    HL2'107',C'SAV',X'00',FL4'0'                                     
         DC    HL2'108',C'PIT',X'00',FL4'0'                                     
         DC    HL2'109',C'FTW',X'00',FL4'0'                                     
         DC    HL2'110',C'CLE',X'00',FL4'0'                                     
         DC    HL2'111',C'WAS',X'00',FL4'0'                                     
         DC    HL2'112',C'BAL',X'00',FL4'0'                                     
         DC    HL2'113',C'FLI',X'00',FL4'0'                                     
         DC    HL2'114',C'BUF',X'00',FL4'0'                                     
         DC    HL2'115',C'CIN',X'00',FL4'0'                                     
         DC    HL2'116',C'ERI',X'00',FL4'0'                                     
         DC    HL2'117',C'CHL',X'00',FL4'0'                                     
         DC    HL2'118',C'GWH',X'00',FL4'0'                                     
         DC    HL2'119',C'CHS',X'00',FL4'0'                                     
         DC    HL2'120',C'AUG',X'00',FL4'0'                                     
         DC    HL2'121',C'PRO',X'00',FL4'0'                                     
         DC    HL2'122',C'COG',X'00',FL4'0'                                     
         DC    HL2'123',C'BUR',X'00',FL4'0'                                     
         DC    HL2'125',C'ALG',X'00',FL4'0'                                     
         DC    HL2'126',C'UTI',X'00',FL4'0'                                     
         DC    HL2'127',C'IND',X'00',FL4'0'                                     
         DC    HL2'128',C'MFH',X'00',FL4'0'                                     
         DC    HL2'129',C'LOU',X'00',FL4'0'                                     
         DC    HL2'130',C'TAL',X'00',FL4'0'                                     
         DC    HL2'131',C'JCK',X'00',FL4'0'                                     
         DC    HL2'132',C'AST',X'00',FL4'0'                                     
         DC    HL2'133',C'HNH',X'00',FL4'0'                                     
         DC    HL2'134',C'ORL',X'00',FL4'0'                                     
         DC    HL2'135',C'COO',X'00',FL4'0'                                     
         DC    HL2'136',C'YOU',X'00',FL4'0'                                     
         DC    HL2'137',C'BAN',X'00',FL4'0'                                     
         DC    HL2'138',C'RON',X'00',FL4'0'                                     
         DC    HL2'139',C'TAM',X'00',FL4'0'                                     
         DC    HL2'140',C'TRA',X'00',FL4'0'                                     
         DC    HL2'141',C'LEX',X'00',FL4'0'                                     
         DC    HL2'142',C'DAY',X'00',FL4'0'                                     
         DC    HL2'143',C'SPR',X'00',FL4'0'                                     
         DC    HL2'144',C'NPN',X'00',FL4'0'                                     
         DC    HL2'145',C'GRN',X'00',FL4'0'                                     
         DC    HL2'146',C'COS',X'00',FL4'0'                                     
         DC    HL2'147',C'TOL',X'00',FL4'0'                                     
         DC    HL2'148',C'WPB',X'00',FL4'0'                                     
         DC    HL2'149',C'WTN',X'00',FL4'0'                                     
         DC    HL2'150',C'WNC',X'00',FL4'0'                                     
         DC    HL2'151',C'LAN',X'00',FL4'0'                                     
         DC    HL2'152',C'PRI',X'00',FL4'0'                                     
         DC    HL2'153',C'MAR',X'00',FL4'0'                                     
         DC    HL2'154',C'WHE',X'00',FL4'0'                                     
         DC    HL2'155',C'SYR',X'00',FL4'0'                                     
         DC    HL2'156',C'RIC',X'00',FL4'0'                                     
         DC    HL2'157',C'KNO',X'00',FL4'0'                                     
         DC    HL2'158',C'LIM',X'00',FL4'0'                                     
         DC    HL2'159',C'BLF',X'00',FL4'0'                                     
         DC    HL2'160',C'RAL',X'00',FL4'0'                                     
         DC    HL2'161',C'JAX',X'00',FL4'0'                                     
         DC    HL2'163',C'GRR',X'00',FL4'0'                                     
         DC    HL2'164',C'CHW',X'00',FL4'0'                                     
         DC    HL2'165',C'ELM',X'00',FL4'0'                                     
         DC    HL2'166',C'HAR',X'00',FL4'0'                                     
         DC    HL2'167',C'GRS',X'00',FL4'0'                                     
         DC    HL2'169',C'HSB',X'00',FL4'0'                                     
         DC    HL2'170',C'FLO',X'00',FL4'0'                                     
         DC    HL2'171',C'FTM',X'00',FL4'0'                                     
         DC    HL2'173',C'ROA',X'00',FL4'0'                                     
         DC    HL2'174',C'JTN',X'00',FL4'0'                                     
         DC    HL2'175',C'CHT',X'00',FL4'0'                                     
         DC    HL2'176',C'SAL',X'00',FL4'0'                                     
         DC    HL2'177',C'WBS',X'00',FL4'0'                                     
         DC    HL2'181',C'TER',X'00',FL4'0'                                     
         DC    HL2'182',C'LAI',X'00',FL4'0'                                     
         DC    HL2'183',C'ALP',X'00',FL4'0'                                     
         DC    HL2'184',C'CHV',X'00',FL4'0'                                     
         DC    HL2'188',C'SOU',X'00',FL4'0'                                     
         DC    HL2'192',C'GAI',X'00',FL4'0'                                     
         DC    HL2'196',C'ZNV',X'00',FL4'0'                                     
         DC    HL2'197',C'PKM',X'00',FL4'0'                                     
         DC    HL2'198',C'CLB',X'00',FL4'0'                                     
         DC    HL2'200',C'COR',X'00',FL4'0'                                     
         DC    HL2'202',C'CHI',X'00',FL4'0'                                     
         DC    HL2'203',C'JOP',X'00',FL4'0'                                     
         DC    HL2'204',C'COM',X'00',FL4'0'                                     
         DC    HL2'205',C'TOP',X'00',FL4'0'                                     
         DC    HL2'206',C'DOT',X'00',FL4'0'                                     
         DC    HL2'209',C'STL',X'00',FL4'0'                                     
         DC    HL2'210',C'RKF',X'00',FL4'0'                                     
         DC    HL2'211',C'ROM',X'00',FL4'0'                                     
         DC    HL2'212',C'SHR',X'00',FL4'0'                                     
         DC    HL2'213',C'MSP',X'00',FL4'0'                                     
         DC    HL2'216',C'KAN',X'00',FL4'0'                                     
         DC    HL2'217',C'MIL',X'00',FL4'0'                                     
         DC    HL2'218',C'HOU',X'00',FL4'0'                                     
         DC    HL2'219',C'SPM',X'00',FL4'0'                                     
         DC    HL2'222',C'NWO',X'00',FL4'0'                                     
         DC    HL2'223',C'DAL',X'00',FL4'0'                                     
         DC    HL2'224',C'SCT',X'00',FL4'0'                                     
         DC    HL2'225',C'WAC',X'00',FL4'0'                                     
         DC    HL2'226',C'VIC',X'00',FL4'0'                                     
         DC    HL2'227',C'WTF',X'00',FL4'0'                                     
         DC    HL2'228',C'MON',X'00',FL4'0'                                     
         DC    HL2'230',C'BIR',X'00',FL4'0'                                     
         DC    HL2'231',C'OTT',X'00',FL4'0'                                     
         DC    HL2'232',C'PAD',X'00',FL4'0'                                     
         DC    HL2'233',C'ODM',X'00',FL4'0'                                     
         DC    HL2'234',C'AMA',X'00',FL4'0'                                     
         DC    HL2'235',C'AUS',X'00',FL4'0'                                     
         DC    HL2'236',C'HRL',X'00',FL4'0'                                     
         DC    HL2'237',C'CED',X'00',FL4'0'                                     
         DC    HL2'238',C'SJM',X'00',FL4'0'                                     
         DC    HL2'239',C'JKT',X'00',FL4'0'                                     
         DC    HL2'240',C'MEM',X'00',FL4'0'                                     
         DC    HL2'241',C'SNT',X'00',FL4'0'                                     
         DC    HL2'242',C'LAF',X'00',FL4'0'                                     
         DC    HL2'243',C'LKC',X'00',FL4'0'                                     
         DC    HL2'244',C'ALX',X'00',FL4'0'                                     
         DC    HL2'247',C'GRW',X'00',FL4'0'                                     
         DC    HL2'248',C'CSD',X'00',FL4'0'                                     
         DC    HL2'249',C'EVA',X'00',FL4'0'                                     
         DC    HL2'250',C'OKL',X'00',FL4'0'                                     
         DC    HL2'251',C'LUB',X'00',FL4'0'                                     
         DC    HL2'252',C'OMA',X'00',FL4'0'                                     
         DC    HL2'256',C'PMC',X'00',FL4'0'                                     
         DC    HL2'257',C'ARD',X'00',FL4'0'                                     
         DC    HL2'258',C'GRB',X'00',FL4'0'                                     
         DC    HL2'259',C'NAS',X'00',FL4'0'                                     
         DC    HL2'261',C'SNA',X'00',FL4'0'                                     
         DC    HL2'262',C'ABI',X'00',FL4'0'                                     
         DC    HL2'269',C'MAD',X'00',FL4'0'                                     
         DC    HL2'270',C'FTS',X'00',FL4'0'                                     
         DC    HL2'271',C'TUL',X'00',FL4'0'                                     
         DC    HL2'273',C'COT',X'00',FL4'0'                                     
         DC    HL2'275',C'PEO',X'00',FL4'0'                                     
         DC    HL2'276',C'DUL',X'00',FL4'0'                                     
         DC    HL2'278',C'WTA',X'00',FL4'0'                                     
         DC    HL2'279',C'DES',X'00',FL4'0'                                     
         DC    HL2'282',C'DAV',X'00',FL4'0'                                     
         DC    HL2'286',C'MOB',X'00',FL4'0'                                     
         DC    HL2'287',C'MIN',X'00',FL4'0'                                     
         DC    HL2'291',C'HUN',X'00',FL4'0'                                     
         DC    HL2'292',C'BPA',X'00',FL4'0'                                     
         DC    HL2'293',C'LIT',X'00',FL4'0'                                     
         DC    HL2'298',C'MGY',X'00',FL4'0'                                     
         DC    HL2'302',C'LAK',X'00',FL4'0'                                     
         DC    HL2'305',C'WAU',X'00',FL4'0'                                     
         DC    HL2'309',C'TYL',X'00',FL4'0'                                     
         DC    HL2'310',C'LHB',X'00',FL4'0'                                     
         DC    HL2'311',C'MER',X'00',FL4'0'                                     
         DC    HL2'316',C'BAT',X'00',FL4'0'                                     
         DC    HL2'317',C'QUI',X'00',FL4'0'                                     
         DC    HL2'318',C'JAK',X'00',FL4'0'                                     
         DC    HL2'322',C'LHK',X'00',FL4'0'                                     
         DC    HL2'324',C'FGO',X'00',FL4'0'                                     
         DC    HL2'325',C'SFL',X'00',FL4'0'                                     
         DC    HL2'334',C'JON',X'00',FL4'0'                                     
         DC    HL2'336',C'BOW',X'00',FL4'0'                                     
         DC    HL2'337',C'MNK',X'00',FL4'0'                                     
         DC    HL2'340',C'NPO',X'00',FL4'0'                                     
         DC    HL2'343',C'ANC',X'00',FL4'0'                                     
         DC    HL2'344',C'HON',X'00',FL4'0'                                     
         DC    HL2'345',C'FAR',X'00',FL4'0'                                     
         DC    HL2'346',C'BIG',X'00',FL4'0'                                     
         DC    HL2'347',C'JUN',X'00',FL4'0'                                     
         DC    HL2'349',C'LAR',X'00',FL4'0'                                     
         DC    HL2'351',C'DEN',X'00',FL4'0'                                     
         DC    HL2'352',C'CSP',X'00',FL4'0'                                     
         DC    HL2'353',C'PHO',X'00',FL4'0'                                     
         DC    HL2'354',C'BUT',X'00',FL4'0'                                     
         DC    HL2'355',C'GFL',X'00',FL4'0'                                     
         DC    HL2'356',C'BLG',X'00',FL4'0'                                     
         DC    HL2'357',C'BOI',X'00',FL4'0'                                     
         DC    HL2'358',C'IDF',X'00',FL4'0'                                     
         DC    HL2'359',C'CHE',X'00',FL4'0'                                     
         DC    HL2'360',C'TWF',X'00',FL4'0'                                     
         DC    HL2'362',C'MSL',X'00',FL4'0'                                     
         DC    HL2'364',C'RPC',X'00',FL4'0'                                     
         DC    HL2'365',C'ELP',X'00',FL4'0'                                     
         DC    HL2'366',C'HEL',X'00',FL4'0'                                     
         DC    HL2'367',C'CAS',X'00',FL4'0'                                     
         DC    HL2'370',C'SLC',X'00',FL4'0'                                     
         DC    HL2'371',C'YUM',X'00',FL4'0'                                     
         DC    HL2'373',C'GRJ',X'00',FL4'0'                                     
         DC    HL2'389',C'TUC',X'00',FL4'0'                                     
         DC    HL2'390',C'ALQ',X'00',FL4'0'                                     
         DC    HL2'398',C'GLD',X'00',FL4'0'                                     
         DC    HL2'400',C'BAK',X'00',FL4'0'                                     
         DC    HL2'401',C'EUG',X'00',FL4'0'                                     
         DC    HL2'402',C'EUR',X'00',FL4'0'                                     
         DC    HL2'403',C'LAX',X'00',FL4'0'                                     
         DC    HL2'404',C'PSP',X'00',FL4'0'                                     
         DC    HL2'407',C'SNF',X'00',FL4'0'                                     
         DC    HL2'410',C'YAK',X'00',FL4'0'                                     
         DC    HL2'411',C'REN',X'00',FL4'0'                                     
         DC    HL2'413',C'MED',X'00',FL4'0'                                     
         DC    HL2'419',C'SEA',X'00',FL4'0'                                     
         DC    HL2'420',C'POO',X'00',FL4'0'                                     
         DC    HL2'421',C'BRO',X'00',FL4'0'                                     
         DC    HL2'425',C'SND',X'00',FL4'0'                                     
         DC    HL2'428',C'SAM',X'00',FL4'0'                                     
         DC    HL2'439',C'LAS',X'00',FL4'0'                                     
         DC    HL2'455',C'STB',X'00',FL4'0'                                     
         DC    HL2'462',C'SAC',X'00',FL4'0'                                     
         DC    HL2'466',C'FRE',X'00',FL4'0'                                     
         DC    HL2'468',C'CHO',X'00',FL4'0'                                     
         DC    HL2'481',C'SPO',X'00',FL4'0'                                     
         DC    X'FF'               EOT                                          
         EJECT                                                                  
FUSMKTD  DSECT                                                                  
FUSMKT#  DS    HL2                 NIELSEN DMA NUMBER                           
FUSMKTC  DS    CL3                 DMA CODE                                     
FUSFLAGS DS    X                   VARIOUS FLAGS                                
FUSLPM   EQU   X'80'                LPM MARKET                                  
FUSUNVRS DS    FL4                 UNIVERSE (ONLY USED PRIOR TO 2008)           
FUSMKTLQ EQU   *-FUSMKTD                                                        
         EJECT                                                                  
* INTERIM RECORD                                                                
       ++INCLUDE DEINTD                                                         
INTRKYLQ EQU   *-INTERD                                                         
       ++INCLUDE DEINTFUSD                                                      
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DEDEMFILE                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMCNVD                                                      
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
       ++INCLUDE DDMONYREQU                                                     
         PRINT ON                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031DETF39I   04/22/19'                                      
         END                                                                    
