*          DATA SET CTMAD05    AT LEVEL 024 AS OF 09/11/03                      
*PHASE TA0C05A,*                                                                
         TITLE 'TA0C05 - $MAD DOWNLOAD GOALS'                                   
TA0C05   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENFREE,TA0C05,RA                                                
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
***********************************************************************         
* INITIALIZE OVERLAY WIDE REGISTERS                                             
***********************************************************************         
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMEORY)              
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
*                                                                               
         ST    RF,AGUIDES          SAVE A(CPP GUIDES)                           
         EJECT                                                                  
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
*                                                                               
         CLI   FATALERR,C'Y'                                                    
         BE    MX                                                               
*                                                                               
         CLI   OVERMODE,C'S'       IF MODE IS START                             
         BNE   M10                                                              
         BAS   RE,PROCSTRT         THEN CALL PROCSTRT                           
         B     MX                                                               
*                                                                               
M10      CLI   OVERMODE,C'M'       ELSE IF MODE IS MIDDLE                       
         BNE   M20                                                              
         BAS   RE,PROCMID          THEN CALL PROCMID                            
         B     MX                                                               
*                                                                               
M20      BAS   RE,PROCEND          ELSE CALL PROCEND                            
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
***********************************************************************         
INIT     NTR1                                                                   
*                                  INITIALIZE SYSTEM                            
         GOTO1 SETSYS,DMCB,=C'SPOT',=CL8'SPTDIR',=CL8'SPTFIL'                   
         BNE   EXIT                                                             
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE REQUEST              
* OBJECT PASSED BY ALINK AND RETURNS FRAME(S) FULL OF GOAL OBJECTS.             
***********************************************************************         
PROCSTRT NTR1                                                                   
         XC    NUMESTB,NUMESTB     NO ENTRIES IN ESTIMATE TABLE YET             
*                                                                               
         MVI   BITFLAG1,0          CLEAR THE BIT FLAGS                          
         BAS   RE,GETGOAL          GET GOAL REQUEST OBJECT                      
*                                                                               
PROCST2  BAS   RE,TSTALL           TEST FOR 'ALL' IN THE FIELDS                 
*                                                                               
         BAS   RE,VALGOAL          VALIDATE GOAL REQUEST OBJECT                 
*                                                                               
         TM    BITFLAG1,BF1PLPRD   IF POL PRODUCT                               
         BZ    *+12                                                             
         BAS   RE,DOPOLPRD         THEN DO POL PRODUCT PROCESSING               
         B     PSX                                                              
*                                                                               
         BAS   RE,SETTMP           SET UP TEMP FILE FOR PUTTING KEYS            
*                                                                               
         BAS   RE,GETGKEY          FILL FRAME WITH GOAL OBJECTS                 
*                                                                               
PSX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT RETURNS FRAME(S)            
* FULL OF GOAL OBJECTS.                                                         
***********************************************************************         
PROCMID  NTR1                                                                   
*                                                                               
PROCMD2  MVI   POLGIND,0           SET OFF INDICATOR                            
         BAS   RE,GETGKEY          FILL FRAME WITH GOAL OBJECTS                 
*                                                                               
PMX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT DOES NOTHING CURRENTLY.              
***********************************************************************         
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE GOAL REQUEST OBJECT.                                    
***********************************************************************         
GETGOAL  NTR1                                                                   
         GOTO1 GETITEM             GET THE REQUEST OBJECT                       
         BNE   EXIT                                                             
*                                  ERROR IF REQUEST NOT A GOAL REQUEST          
         CLC   TYPENUM,=A(ITDNLGOL)                                             
         BE    GETG10                                                           
         CLC   TYPENUM,=A(ITNWDGOL)                                             
         BNE   INVLRQST                                                         
         OI    BITFLAG1,BF1NWGOL                                                
*                                  ERROR IF LENGTH > MAX(L'(REQUEST))           
GETG10   CLC   DATALEN,=A(REQLEN)                                               
         BH    INVLRQST                                                         
*                                                                               
         L     R4,ADATA            SAVE REQUEST OBJ TO LOCAL STORAGE            
         MVC   REQUEST(REQLEN),0(R4)                                            
*                                                                               
         OC    REQCLT,=C'   '      BLANK PAD CLIENT                             
         OC    REQPR1,=C'   '      BLANK PAD PRIMARY PRODUCT                    
         OC    REQPR2,=C'   '      BLANK PAD PIGGYBACK PRODUCT                  
*                                                                               
GETGX    B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE TESTS FOR 'ALL' IN THE FIELDS.                                   
***********************************************************************         
TSTALL   NTR1                                                                   
         MVI   ALLFLAGS,0          CLEAR THE ALL FLAGS                          
*                                                                               
         CLC   =C'   ',REQPR1      IF ALL PRODUCTS REQUESTED                    
         BNE   *+8                                                              
         OI    ALLFLAGS,X'80'      THEN FLAG ALL PRODUCTS                       
*                                                                               
         CLC   =C'  ',REQEST       IF ALL ESTIMATES REQUESTED                   
         BNE   TSTA10                                                           
         OI    ALLFLAGS,X'20'      THEN FLAG ALL ESTIMATES                      
*                                                                               
TSTA10   CLC   =C' ',REQDYPT       IF ALL DAYPARTS REQUESTED                    
         BNE   *+8                                                              
         OI    ALLFLAGS,X'10'      THEN FLAG ALL DAYPARTS                       
*                                                                               
TSTA20   CLC   =C'00',REQSPLN      IF ALL SPOT LENGTHS REQUESTED                
         BNE   *+8                                                              
         OI    ALLFLAGS,X'08'      THEN FLAG ALL SPOT LENGTHS                   
*                                                                               
*                                  IF ALL MARKETS NUMBERS REQUESTED             
TSTA30   CLC   DATALEN,=A(REQMKT-REQUEST)                                       
         BH    TSTA32                                                           
         OI    ALLFLAGS,X'40'      THEN FLAG ALL MARKET NUMBERS                 
         B     TSTAX                                                            
*                                                                               
TSTA32   CLC   =C'**',REQMKT        TEST FOR PURPOSE CODE                       
         BNE   TSTAX                                                            
         OC    REQMKT+2(6),=6C' '   MAKE PURPOSE CODE UPPERCASE                 
         CLC   DATALEN,=A(REQMKT-REQUEST+8)                                     
         BH    *+8                                                              
         OI    ALLFLAGS,X'40'                                                   
*                                                                               
TSTAX    B     XIT                 RETURN TO THE CALLER                         
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE VALIDATES THE MEDIA, CLIENT AND PERIOD USED FOR GOALS.           
* IT ALSO SETS UP THE CODES FOR PRODUCT, ESTIMATE, DAYPART, SPOT LENGTH         
* AND MARKET NUMBER.                                                            
***********************************************************************         
VALGOAL  NTR1                                                                   
         MVI   POLGIND,0           SET OFF INDICATOR                            
*                                                                               
         CLI   REQMED,C'T'                                                      
         BNE   VGOAL2                                                           
         CLC   REQPR1,=C'POL'                                                   
         BE    VGOAL60                                                          
*                                                                               
VGOAL2   XC    PARTKEY,PARTKEY     CLEAR THE PARTIAL KEY                        
         MVI   TYPECODE,TYPECODQ   SET UP RECORD TYPE X'02'                     
*                                                                               
         GOTO1 VALIMED,DMCB,REQMED VALIDATE MEDIA                               
         BNE   ERROBJ                                                           
*                                                                               
         MVC   AGMDCODE,BAGYMED    COPY THE AGENCY MEDIA CODE TO LOCAL          
*                                                                               
         GOTO1 VALICLT,DMCB,REQCLT VALIDATE THE CLIENT                          
         BNE   ERROBJ                                                           
*                                                                               
         MVC   CLTCODE,BCLT        COPY THE CLIENT CODE TO LOCAL                
* READ B0 PROFILE FOR PURPOSE CODE                                              
         MVC   WORK(4),=C'S0B0'                                                 
         MVC   WORK+4(2),SIGNON2C                                               
         MVC   WORK+6(1),REQMED                                                 
         MVC   WORK+7(3),REQCLT                                                 
         MVI   WORK+10,C'*'                                                     
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVC   WORK+11(1),COFFICE                                               
         MVC   SVOFFC,COFFICE                                                   
         MVC   SVCACCS,CACCESS                                                  
         DROP  R6                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETPROF-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,WORK,SVB0PROF,DATAMGR                                  
*                                                                               
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         CLI   TWAACCS,C'+'        TEST LIMIT ACCESS BY MKT                     
         BE    VGOAL4                                                           
         DROP  RE                                                               
*                                                                               
         BRAS  RE,CALLOFCR         CHECK LIMIT ACCESS                           
         BNE   NOACCESS                                                         
*                                                                               
VGOAL4   TM    ALLFLAGS,X'20'      IF ALL ESTIMATES                             
         BZ    VGOAL10                                                          
*                                  IF PERIOD NOT VALID DECIMALS                 
         GOTO1 DECIN,DMCB,REQSTDT,L'REQSTDT                                     
         BNE   INVLPERD                                                         
         GOTO1 DECIN,DMCB,REQNDDT,L'REQNDDT                                     
         BNE   INVLPERD            THEN ERROR                                   
*                                                                               
*                                  ELSE COPY DATES TO BE USED                   
         MVC   DATESTRT(L'REQPERD),REQPERD                                      
*                                                                               
         CLC   DATESTRT,DATEEND    IF START DATE > END DATE                     
         BH    INVLPERD            THEN ERROR                                   
*                                                                               
VGOAL10  TM    ALLFLAGS,X'80'      IF NOT ALL PRODUCTS REQUESTED                
         BNZ   VGOAL20                                                          
*                                                                               
         CLC   =C'POL',REQPR1      IF POL PRODUCT                               
         BNE   VGOAL12                                                          
*                                                                               
         TM    ALLFLAGS,X'FF'      IF ANY ALL IS REQUESTED                      
         BNZ   INVLPOLR            THEN ERROR, NO ALLS FOR POL                  
*                                                                               
         OI    BITFLAG1,BF1PLPRD   ELSE FLAG POL                                
         MVI   PRDCDE,1            CHECK FIRST PRODUCT                          
         MVI   BPRD,1                                                           
         B     VGOAL20                                                          
*                                                                               
VGOAL12  CLC   =C'   ',REQPR2      IF PIGGY BACK                                
         BE    VGOAL15                                                          
         OI    BITFLAG1,BF1PIGGY   THEN FLAG IT                                 
*                                                                               
         GOTO1 VALIPRD,DMCB,REQPR2 VALIDATE THE 2ND PRODUCT                     
         BNE   ERROBJ                                                           
*                                                                               
         MVC   QPRD2,QPRD          COPY THE PRODUCT TO LOCAL                    
         MVC   PRDCDE2,BPRD                 PRODUCT CODE                        
*                                                                               
VGOAL15  GOTO1 VALIPRD,DMCB,REQPR1 ELSE VALIDATE THE 1ST PRODUCT                
         BNE   ERROBJ                                                           
*                                                                               
         MVC   PRDCDE,BPRD         COPY THE PRODUCT CODE                        
*                                                                               
*                                                                               
VGOAL20  TM    ALLFLAGS,X'20'      IF ONE ESTIMATE REQUESTED                    
         BNZ   VGOAL30                                                          
*                                  IF ESTIMATE NOT VALID HEX                    
         GOTO1 HEXIN,DMCB,REQEST,ESTCODE,L'GKEYEST*2                            
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLQEST            THEN ERROR                                   
*                                                                               
*                                                                               
VGOAL30  TM    ALLFLAGS,X'10'      IF ONE DAYPART REQUESTED                     
         BNZ   VGOAL40                                                          
         MVC   DPTCODE,REQDYPT     THEN COPY DAYPART CODE                       
*                                                                               
*                                                                               
VGOAL40  TM    ALLFLAGS,X'08'      IF ONE SPOT LENGTH REQUESTED                 
         BNZ   VGOAL50                                                          
*                                      IF SPOT LENGTH NOT VALID HEX             
         GOTO1 HEXIN,DMCB,REQSPLN,SLNCODE,L'GKEYSLN*2                           
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLSLN                 THEN ERROR                               
*                                                                               
*                                                                               
VGOAL50  TM    ALLFLAGS,X'40'      IF NOT ALL MARKET NUMBER REQUESTED           
         BNZ   VGOAL60                                                          
*                                  THEN GET THE MARKET NUMBER(S)                
*                                                                               
         XC    MKTLIST,MKTLIST     CLEAR THE MARKET LIST                        
         SR    R0,R0               R0 = # OF MARKET NUMBERS IN LIST             
         LA    R4,MKTLIST          R4 = A(1ST ENTRY IN MARKET LIST)             
         LA    R3,REQMKT           R3 = A(FIRST MKTNUM IN REQ)                  
         L     R2,DATALEN          R2 = LEN OF MKTS IN REQ                      
         LA    R1,REQMKT-REQUEST   R1 = DSPL TO FIRST MKT IN REQ                
*                                                                               
         CLC   =C'**',REQMKT       TEST PURPOSE CODE PRESENT                    
         BNE   *+12                                                             
         LA    R3,8(R3)            ADJUST MKT POINTER                           
         LA    R1,8(R1)              AND DISPLACEMENT                           
*                                                                               
         SR    R2,R1               GIVES LENGTH OF MARKETS                      
*                                  IF MARKET # NOT NUMERIC                      
VGOAL55  GOTO1 DECIN,DMCB,(R3),L'REQMKT                                         
         BNE   INVLMKT             THEN ERROR                                   
*                                                                               
*                                  SAVE BINARY EQUIV. TO LOCAL STORAGE          
         MVC   0(L'MKTCODE,R4),(4-L'MKTCODE)(R1)                                
         AHI   R0,1                BUMP # OF MARKET NUMBERS COUNTER             
         LA    R4,L'MKTCODE(R4)    R4 = A(NEXT ENTRY IN MARKET LIST)            
         LA    R3,L'REQMKT(R3)     R3 = A(NEXT NUMERIC MARKET IN LIST)          
*                                                                               
         AHI   R2,-4               IF NO MORE MARKET NUMBERS                    
         BZ    *+8                 THEN DONE WITH MARKET NUMBER LIST            
         B     VGOAL55             ELSE LOOP BACK FOR MORE                      
*                                                                               
         STC   R0,NUMMKTS          STORE # OF MARKET NUMBERS IN LIST            
         LR    R2,R0                                                            
*                                                                               
         GOTO1 QSORT,DMCB,MKTLIST,(R2),L'MKTCODE,L'MKTCODE,0                    
*                                                                               
VGOAL60  CLI   SVB0PROF+9,C'Y'     TEST PURPOSE CODES REQ'D                     
         BE    VGOAL62                                                          
         CLC   =C'**',REQMKT       TEST PURPOSE CODE PRESENT                    
         BNE   VGOALX              NO                                           
         B     NOPURPS             SHOULDN'T HAVE PURPOSE CODE                  
*                                                                               
VGOAL62  CLC   =C'**',REQMKT       TEST PURPOSE CODE PRESENT                    
         BNE   MSSGPURP                                                         
*                                                                               
VGOALX   B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DOES ALL THE POL PRODUCT PROCESSING.                             
***********************************************************************         
DOPOLPRD NTR1                                                                   
         CLI   NUMMKTS,1           IF MORE THAN 1 MARKET                        
         BH    INVLPOLR            THEN ERROR                                   
*                                                                               
         MVC   QPRD,=C'POL'                                                     
         MVC   MKTCODE,MKTLIST                                                  
         MVC   CURRKEY(L'PARTKEY),PARTKEY                                       
*                                                                               
         BAS   RE,CHKESTM          GET PERIOD FOR ESTIMATE                      
         BAS   RE,CNVDATES         GET BINARY EQUIVALENTS OF DATES              
         BAS   RE,SETACC           SETUP THE ACCUMULATOR TABLE                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING GOALRECD,R4                                                      
         MVC   KEY(L'PARTKEY),PARTKEY                                           
*                                                                               
DPOLHI   GOTO1 HIGH                                                             
*                                  IF SAME CLIENT                               
         CLC   GKEY(GKEYPRD-GKEY),KEYSAVE                                       
         BNE   DPOLPUT                                                          
*                                                                               
         CLI   GKEYPRD,X'FF'       NO POL GOAL RECORD POSSIBLE                  
         BE    DPOLPUT                                                          
*                                  IF WE HAVE A PRD UNDER THE FILTERS           
         CLC   GKEY(GKEYSEC-GKEY),KEYSAVE                                       
         BE    DPOL20                                                           
*                                  IF SAME PRODUCT                              
         CLC   GKEYPRD,KEYSAVE+GKEYPRD-GKEY                                     
         BNE   DPOL15                                                           
*                                                                               
DPOL10   IC    R1,GKEYPRD          INCREMENT TO NEXT PRODUCT                    
         LA    R1,1(R1)                                                         
         STC   R1,GKEYPRD                                                       
*                                  SET UP FILTERS                               
DPOL15   XC    GKEYMKT(GKEYPRD2+L'GKEYPRD2-GKEYMKT),GKEYMKT                     
         MVC   GKEYMKT(GKEYSEC-GKEYMKT),MKTCODE                                 
         B     DPOLHI                                                           
*                                                                               
DPOL20   TM    GKEYAGY,X'80'       IF PASSIVE PIGGYBACK POINTER                 
         BNZ   DPOL10              THEN BUMP TO NEXT PRODUCT                    
*                                                                               
         BAS   RE,CALCGOAL         ADD GOALS TO THE POL                         
*                                                                               
         B     DPOL10              AND BUMP TO NEXT PRODUCT                     
*                                                                               
DPOLPUT  MVC   QPRD,=C'POL'                                                     
         MVC   QPRD2,=C'   '                                                    
         MVC   CURRKEY(L'PARTKEY),PARTKEY                                       
         LA    R4,CURRKEY                                                       
         MVI   GKEYSEC,0           NO MEANING FOR TOTAL SECONDS IN POL          
*                                                                               
         BAS   RE,FILLFRM          SHOW THE POL GOAL                            
*                                                                               
*                                  PUT END OF DATA OBJECT OUT                   
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         LAST DATA FRAME                              
*                                                                               
DPOLX    B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP THE TEMPFILE SO WE CAN PUT GOAL KEYS IN IT.              
***********************************************************************         
SETTMP   NTR1                                                                   
         MVC   DUB(2),SIGNON2C     AGENCY CODE                                  
         MVC   DUB+2(1),REQMED     MEDIA                                        
         MVC   DUB+3(2),BCLT       BINARY CLIENT CODE                           
         L     R5,DATAMGR          GET A(DATAMGR)                               
*                                                                               
*                                  GET EQUIVALENCE HEADER                       
         GOTO1 EQVRD,DMCB,DUB,EQUDPT,EQUSECT1,(R5)                              
*                                                                               
         CLI   DMCB+12,0           IF THERE IS ANY PROBLEMS                     
         BE    *+6                                                              
         DC    H'0'                THEN DIE, SERIOUS BUG                        
*                                                                               
*                                  OPEN TMP FILE FOR PUTS                       
         GOTO1 TMPOPEN,DMCB,=C'PUT'                                             
*                                                                               
         XC    NUMGOALS,NUMGOALS   NUMBER OF GOALS READ                         
*                                                                               
         MVC   MKTCODE,MKTLIST     GET FIRST MARKET NUMBER FROM LIST            
*                                                                               
         BAS   RE,PUTGKEY          PUT GOAL KEYS INTO ATMPBUF                   
*                                                                               
         GOTO1 TMPCLOSE            CLOSE TMP FILE AFTER PUTS                    
*                                                                               
         B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS IF GOAL FULFILLS OUR FILTERS (IF ANY).  IF THE            
* GOAL DOES, THEN ITS KEY PUT INTO ATMPBUF.                                     
***********************************************************************         
PUTGKEY  NTR1                                                                   
         XC    CURRKEY,CURRKEY     LOAD KEY INFORMATION TO SAVED KEY            
         XC    KEY,KEY                                                          
         MVC   CURRKEY(L'PARTKEY),PARTKEY                                       
*                                                                               
*                                  USE THE COPY OF THE KEY WE MADE              
PGKHI    MVC   KEY(L'CURRKEY),CURRKEY                                           
         GOTO1 HIGH                GET 1ST RECORD THAT >= KEY                   
*                                                                               
PGKTST   MVC   CURRKEY,KEY         MAKE A COPY OF THE GOAL KEY                  
*                                                                               
*                                  IF NON-VARIANT DATA DON'T MATCH              
         CLC   KEY(L'GKEYTYPE+L'GKEYAM+L'GKEYCLT),PARTKEY                       
         BE    PGK10                                                            
         OC    NUMGOALS,NUMGOALS   THEN IF NO GOALS BEFORE                      
         BZ    INVLGOAL                   THEN ERROR                            
         B     PGKX                       ELSE DONE                             
*                                                                               
PGK10    BAS   RE,TSTKPRTS         ELSE TEST KEY PARTS                          
         BNE   PGKHI               GET ANOTHER KEY IF NO MATCH                  
*                                                                               
PGK20    BAS   RE,GETQPRD          GET EBCDIC REP OF PRIMARY PRODUCT            
*                                                                               
         BAS   RE,CHKESTM          CK IF PERIOD INTERSECTS ESTIMATE'S           
         BNE   PGKHI               GET ANOTHER KEY IF NOT                       
*                                                                               
         MVC   TMPGKEY,CURRKEY                                                  
         GOTO1 PUTTMP,DMCB,TMPKEY,L'TMPKEY                                      
         BNE   EXIT                                                             
*                                                                               
         ICM   R1,3,NUMGOALS       INCREMENT NUMBER OF GOALS READ               
         LA    R1,1(R1)                                                         
         STCM  R1,3,NUMGOALS                                                    
*                                  RESET FILE TO POINT TO GOAL                  
         XC    KEY,KEY                                                          
         MVC   KEY(L'CURRKEY),CURRKEY                                           
         GOTO1 HIGH                                                             
*                                                                               
         DS    0H                                                               
         GOTO1 SEQ                 GET NEXT GOAL RECORD                         
         B     PGKTST              TEST IF STILL GOOD GOAL                      
*                                                                               
PGKX     B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS TESTS IF PARTS OF THE KEY STILL SATISFIES THE CRITERIA SET BY            
* THE USER.                                                                     
***********************************************************************         
TSTKPRTS NTR1                                                                   
         LA    R4,CURRKEY                                                       
         USING GOALRECD,R4                                                      
*                                                                               
*** PRD ***                                                                     
TSTK00   TM    ALLFLAGS,X'80'      IF ALL PRODUCTS                              
         BZ    TSTK02                                                           
*                                                                               
         CLI   GKEYPRD,X'FF'       IF WE HAVE CPP GUIDE                         
         BE    TSTK04              THEN FORCE NO MORE TO BE READ                
*                                                                               
         MVC   PRDCDE,GKEYPRD      ELSE EXTRACT PRODUCT CODE                    
         B     TSTK10                   AND TEST MARKET NUMBER                  
*                                                                               
TSTK02   CLC   GKEYPRD,PRDCDE      IF PRODUCT CODE SAME                         
         BE    TSTK10              THEN TEST MARKET                             
         BL    TSTK06                                                           
*                                                                               
TSTK04   MVI   GKEYPRD,X'FF'       FORCE NO MORE TO BE READ                     
         BAS   RE,BMPKPRD                                                       
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
TSTK06   MVC   GKEYPRD,PRDCDE      USE THE PRODUCT WE NEED                      
*                                  CLEAR REST OF KEY                            
         XC    GKEYMKT(GKEYPRD2-GKEYMKT+L'GKEYPRD2),GKEYMKT                     
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
*** MKT ***                                                                     
TSTK10   TM    ALLFLAGS,X'40'      IF ALL MARKETS                               
         BZ    TSTK10A             NO                                           
         MVC   MKTCODE,GKEYMKT     THEN EXTRACT MARKET CODE                     
         B     TSTK20                   AND TEST ESTIMATE                       
*                                                                               
TSTK10A  CLC   GKEYMKT,MKTCODE     IF MARKET CODE SAME                          
         BE    TSTK20              THEN TEST ESTIMATE                           
         BL    TSTK16                                                           
*                                                                               
         LA    R6,MKTLIST          R6 = A(MARKET NUMBERS LIST)                  
*                                                                               
TSTK10LP CLC   GKEYMKT,0(R6)       IF MARKET IN KEY > ONE IN LIST               
         BNH   TSTK13                                                           
         LA    R6,L'MKTCODE(R6)    R6 = A(NEXT MARKET IN LIST)                  
*                                                                               
*                                  IF THERE ARE MORE MARKETS                    
         OC    0(L'MKTCODE,R6),0(R6)                                            
         BNZ   TSTK10LP            THEN LOOP BACK                               
*                                                                               
         BAS   RE,BMPKPRD          ELSE BUMP PRODUCT AND RESET MARKET           
         B     TSTK16                                                           
*                                                                               
TSTK13   MVC   MKTCODE,0(R6)       USE MARKET IN LIST                           
TSTK16   MVC   GKEYMKT,MKTCODE     SET MARKET IN KEY TO MARKET IN USE           
*                                  CLEAR REST OF KEY                            
         XC    GKEYEST(GKEYPRD2-GKEYEST+L'GKEYPRD2),GKEYEST                     
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
*** EST ***                                                                     
TSTK20   TM    ALLFLAGS,X'20'      IF ALL ESTIMATES                             
         BZ    *+14                                                             
         MVC   ESTCODE,GKEYEST     THEN EXTRACT ESTIMATE CODE                   
         B     TSTK30                   AND TEST DAYPARTS                       
*                                                                               
         CLC   GKEYEST,ESTCODE     IF ESTIMATE SAME                             
         BE    TSTK30              THEN TEST DAYPARTS                           
         BL    TSTK25                                                           
         BAS   RE,BMPKMKT          BUMP MARKET IN KEY IF GREATER                
*                                                                               
TSTK25   MVC   GKEYEST,ESTCODE     USE THE ESTIMATE WE NEED                     
*                                  CLEAR REST OF KEY                            
         XC    GKEYDPT(GKEYPRD2-GKEYDPT+L'GKEYPRD2),GKEYDPT                     
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
*** DPT ***                                                                     
TSTK30   CLC   MDACTION,=Y(ACCBLGOL) CABLE DOWNLOAD GOALS?                      
         BNE   TSTK34                                                           
*                                                                               
         TM    ALLFLAGS,X'10'      IF ALL DAYPARTS                              
         BZ    TSTK32              NO, SHOULD STILL MATCH ONE OF THE 3          
         CLI   GKEYDPT,C'G'        IF GOALS ARE FOR ONE OF THESE DAYPTS         
         BE    TSTK35              THEN TEST SPOT LENGTH                        
         BH    *+12                                                             
         MVI   GKEYDPT,C'G'                                                     
         B     TSTK39A                                                          
*                                                                               
         CLI   GKEYDPT,C'X'                                                     
         BE    TSTK35                                                           
         BH    *+12                                                             
         MVI   GKEYDPT,C'X'                                                     
         B     TSTK39A                                                          
*                                                                               
         CLI   GKEYDPT,C'1'                                                     
         BE    TSTK35                                                           
         BH    TSTK37              HIGHER THAN OUR LAST DPT, BUMP EST           
         MVI   GKEYDPT,C'1'                                                     
         B     TSTK39A                                                          
*                                                                               
TSTK32   DS    0H                                                               
         CLI   DPTCODE,C'G'        IF GOALS ARE FOR ONE OF THESE DAYPTS         
         BE    TSTK36              THEN TEST SPOT LENGTH                        
         CLI   DPTCODE,C'X'                                                     
         BE    TSTK36                                                           
         CLI   DPTCODE,C'1'                                                     
         BE    TSTK36                                                           
         B     TSTK37              ELSE BUMP ESTIMATE T 1 OF 3                  
*******                                                                         
TSTK34   TM    ALLFLAGS,X'10'      IF ALL DAYPARTS                              
         BZ    *+14                                                             
TSTK35   MVC   DPTCODE,GKEYDPT     THEN EXTRACT DAYPART CODE                    
         B     TSTK40              AND TEST SPOT LENGTH                         
*                                                                               
TSTK36   CLC   GKEYDPT,DPTCODE     IF DAYPART SAME                              
         BE    TSTK40              THEN TEST SPOT LENGTH                        
         BL    TSTK39                                                           
TSTK37   BAS   RE,BMPKEST          BUMP ESTIMATE IN KEY IF GREATER              
*                                                                               
TSTK39   MVC   GKEYDPT,DPTCODE     USE THE DAYPART WE NEED                      
*                                  CLEAR REST OF KEY                            
TSTK39A  XC    GKEYSLN(GKEYPRD2-GKEYSLN+L'GKEYPRD2),GKEYSLN                     
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
*** SLN ***                                                                     
TSTK40   TM    ALLFLAGS,X'08'      IF ALL SPOT LENGTHS                          
         BZ    *+14                                                             
         MVC   SLNCODE,GKEYSLN     THEN EXTRACT SPOT LENGTH                     
         B     TSTK50              AND TEST PIGGYBACK                           
*                                                                               
         CLC   GKEYSLN,SLNCODE     IF SPOT LENGTH SAME                          
         BE    TSTK50              THEN TEST IF PIGGYBACK                       
         BL    TSTK45                                                           
         BAS   RE,BMPKDPT          BUMP DAYPART IN KEY IF GREATER               
*                                                                               
TSTK45   MVC   GKEYSLN,SLNCODE     USE THE SPOT LENGTH WE NEED                  
*                                  CLEAR REST OF KEY                            
         XC    GKEYSEC(GKEYPRD2-GKEYSEC+L'GKEYPRD2),GKEYSEC                     
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
*** PB'S ***                                                                    
TSTK50   TM    ALLFLAGS,X'80'      IF ALL PRODUCTS                              
         BZ    TSTK52                                                           
*                                                                               
         TM    GKEYAGY,X'80'       IF PASSIVE                                   
         BZ    TSTKYES                                                          
         B     TSTK60              THEN BUMP PIGGYBACK                          
*                                                                               
TSTK52   CLI   GKEYPRD2,0          IF NO PIGGYBACK FOUND IN KEY                 
         BNE   TSTK54                                                           
*                                                                               
         TM    BITFLAG1,BF1PIGGY   IF PB WAS NOT REQUESTED                      
         BZ    TSTKYES             THEN RETURN 'YES' TO CALLER                  
*                                                                               
         MVC   GKEYPRD2,PRDCDE2    ELSE USE THE PIGGYBACK WE NEED               
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
TSTK54   CLC   =C'**',REQMKT       TEST PURPOSE CODES                           
         BE    TSTKYES             YES - NO PIGGYBACKS AROUND                   
         CLI   SVB0PROF+9,C'Y'                                                  
         BE    TSTKYES                                                          
*                                                                               
         TM    GKEYAGY,X'80'       IF KEY IS PASSIVE                            
         BNZ   TSTK60              THEN BUMP PIGGYBACK IN KEY                   
*                                                                               
         TM    BITFLAG1,BF1PIGGY   IF PIGGYBACK WASN'T REQUESTED                
         BZ    TSTK60              THEN BUMP PIGGYBACK IN KEY                   
*                                                                               
         CLC   PRDCDE2,GKEYPRD2    IF PIGGYBACKS MATCH                          
         BE    TSTKYES             THEN RETURN 'YES'                            
         BH    TSTK60                                                           
         MVC   GKEYPRD2,PRDCDE2    USE THE PIGGYBACK WE NEED IF LESS            
         B     TSTKNO              RETURN 'NO' TO CALLER                        
*                                                                               
TSTK60   BAS   RE,BMPKPRD2         BUMP PARTNER PRODUCT IN KEY                  
         B     TSTKNO                                                           
*                                                                               
TSTKYES  MVC   PRDCDE2,GKEYPRD2    EXTRACT PIGGYBACK PRODUCT CODE               
         B     YES                 RETURN A 'YES' TO CALLER                     
*                                                                               
TSTKNO   B     NO                  RETURN A 'NO' TO CALLER                      
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INCREMENTS THE PRODUCT CODE IN THE CURRKEY.                      
***********************************************************************         
BMPKPRD  NTR1                                                                   
         LA    R4,CURRKEY          R4 = A(CURRKEY)                              
         USING GOALRECD,R4                                                      
         CLI   GKEYPRD,X'FF'       IF LOOKING AT LAST ONE                       
         BNE   BMPPRD10                                                         
         ICM   R1,15,GKEYTYPE      THEN INCREMENT FIRST 4 BYTES OF KEY          
         A     R1,=F'1'                TO FORCE KEY TO CONFLICT                 
         STCM  R1,15,GKEYTYPE                                                   
*                                                                               
BMPPRD10 IC    R1,GKEYPRD          ELSE INCREMENT PRODUCT CODE BY ONE           
         LA    R1,1(R1)                                                         
         STC   R1,GKEYPRD                                                       
*                                                                               
         MVC   MKTCODE,MKTLIST     RESET MARKET TO FIRST IN LIST                
*                                                                               
         B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
***********************************************************************         
* THIS ROUTINE INCREMENTS THE MARKET NUMBER IN THE CURRKEY.                     
***********************************************************************         
BMPKMKT  NTR1                                                                   
         LA    R4,CURRKEY          R4 = A(CURRKEY)                              
         USING GOALRECD,R4                                                      
         CLI   GKEYMKT+1,X'FF'     IF LOOKING AT LAST ONE                       
         BNE   BMPMKT10                                                         
         CLI   GKEYMKT,X'FF'                                                    
         BNE   BMPMKT10                                                         
         BAS   RE,BMPKPRD          THEN BUMP THE PRODUCT                        
*                                                                               
BMPMKT10 ZICM  R1,GKEYMKT,(3)      ELSE INCREMENT MARKET NUMBER BY ONE          
         LA    R1,1(R1)                                                         
         STCM  R1,3,GKEYMKT                                                     
         B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
***********************************************************************         
* THIS ROUTINE INCREMENTS THE ESTIMATE IN THE CURRKEY.                          
***********************************************************************         
BMPKEST  NTR1                                                                   
         LA    R4,CURRKEY          R4 = A(CURRKEY)                              
         USING GOALRECD,R4                                                      
         CLI   GKEYEST,X'FF'       IF LOOKING AT LAST ONE                       
         BNE   BMPEST10                                                         
         BAS   RE,BMPKMKT          THEN BUMP THE MARKET                         
*                                                                               
BMPEST10 IC    R1,GKEYEST          ELSE INCREMENT ESTIMATE BY ONE               
         LA    R1,1(R1)                                                         
         STC   R1,GKEYEST                                                       
         B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE INCREMENTS THE DAYPART IN THE CURRKEY.                           
***********************************************************************         
BMPKDPT  NTR1                                                                   
         LA    R4,CURRKEY          R4 = A(CURRKEY)                              
         USING GOALRECD,R4                                                      
         CLI   GKEYDPT,X'FF'       IF LOOKING AT LAST ONE                       
         BNE   BMPDPT10                                                         
         BAS   RE,BMPKEST          THEN BUMP THE ESTIMATE                       
*                                                                               
BMPDPT10 IC    R1,GKEYDPT          ELSE INCREMENT DAYPART BY ONE                
         LA    R1,1(R1)                                                         
         STC   R1,GKEYDPT                                                       
         B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
***********************************************************************         
* THIS ROUTINE INCREMENTS THE SPOT LENGTH IN THE CURRKEY.                       
***********************************************************************         
BMPKSLN  NTR1                                                                   
         LA    R4,CURRKEY          R4 = A(CURRKEY)                              
         USING GOALRECD,R4                                                      
         CLI   GKEYSLN,X'FF'       IF LOOKING AT LAST ONE                       
         BNE   BMPSLN10                                                         
         BAS   RE,BMPKDPT          THEN BUMP THE DAYPART                        
*                                                                               
BMPSLN10 IC    R1,GKEYSLN          ELSE INCREMENT SPOT LENGTH BY ONE            
         LA    R1,1(R1)                                                         
         STC   R1,GKEYSLN                                                       
         B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
***********************************************************************         
* THIS ROUTINE INCREMENTS THE PARTNER PRODUCT IN THE CURRKEY.                   
***********************************************************************         
BMPKPRD2 NTR1                                                                   
         LA    R4,CURRKEY          R4 = A(CURRKEY)                              
         USING GOALRECD,R4                                                      
         ICM   R1,15,GKEYSLN       INCREMENT LAST 4 BYTES OF KEY                
         A     R1,=F'1'                                                         
         STCM  R1,15,GKEYSLN                                                    
         B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE EBCDIC EQUIVALENT FOR THE PRODUCT CODE.                 
***********************************************************************         
GETQPRD  NTR1                                                                   
         LA    R6,PRDLIST          R6 = A(1ST PRD IN PRD CODE LIST)             
*                                                                               
GPRD10   CLI   0(R6),0             IF END OF TABLE                              
         BE    INVLPLST            THEN ERROR                                   
*                                                                               
         CLC   PRDCDE,3(R6)        IF PRODUCT CODE MATCHES                      
         BE    GPRD20              THEN DONE                                    
*                                                                               
         LA    R6,4(R6)            BUMP R6 TO NEXT PRODUCT                      
         B     GPRD10              LOOP BACK                                    
*                                                                               
GPRD20   MVC   QPRD,0(R6)          SAVE PRODUCT IN BOTH FORMS                   
         MVC   BPRD,PRDCDE                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS THE ESTIMATE DATES TO MAKE SURE THE PERIOD                
* INTERSECTS WITH THE ESTIMATE'S PERIOD.                                        
***********************************************************************         
CHKESTM  NTR1                                                                   
         LA    R4,CURRKEY                                                       
         USING GOALRECD,R4                                                      
*                                                                               
         BAS   RE,FINDESTM         IF WE HAVEN'T CHECK THIS ESTIMATE            
         BE    CKE30                                                            
*                                  CONVERT GOAL ESTIMATE CODE TO EBCDIC         
         GOTO1 HEXOUT,DMCB,ESTCODE,FULL,L'ESTCODE                               
*                                                                               
         DS    0H                                                               
         GOTO1 VALIEST,DMCB,FULL   EXTRACT DATA FROM ESTIMATE HEADER            
         BNE   INVLGEST                                                         
*                                                                               
         TM    ALLFLAGS,X'20'      IF ALL ESTIMATES                             
         BZ    CKE20                                                            
*                                                                               
         CLC   ESTSTRT,DATESTRT    IF NO INTERSECTION ON LOWER END              
         BL    CKE10                                                            
         CLC   ESTSTRT,DATEEND                                                  
         BNH   CKE20                                                            
CKEBMP   BAS   RE,BMPKEST          THEN BUMP ESTIMATE IN KEY                    
         XC    GKEYDPT(GKEYPRD2-GKEYDPT+L'GKEYPRD2),GKEYDPT                     
         B     CKENO                   PERIOD INTERSECT WITH ESTIMATE'S         
*                                                                               
CKE10    CLC   ESTEND,DATESTRT     IF NO INTERSECTION ON HIGHER END             
         BL    CKEBMP              THEN BUMP ESTIMATE IN KEY                    
*                                                                               
CKE20    LA    R3,TMPENTRY                                                      
         USING ETBENTRY,R3                                                      
         MVC   ETBEST,ESTCODE      SET UP KEY FOR TABLE ENTRY                   
         MVC   ETBQPRD,QPRD                                                     
         MVC   ETBDPT,DPTCODE                                                   
*                                                                               
         MVI   ETDAILY,C'N'        NOT DAILY ESTIMATE                           
         TM    BITFLAG1,BF1NWGOL   DO WE CHECK FOR DAILY GOALS?                 
         BZ    CKE25                                                            
*                                                                               
         L     R6,AIO              YES, GET DAILY FLAG FROM EST RECORD          
         USING ESTHDRD,R6                                                       
         MVC   ETDAILY,EDAILY                                                   
         MVC   SVC2FCTR,ECOST2     SAVE COS2 FACTOR                             
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,14,EPWPCT                                                     
         BZ    CKE25                                                            
         SRA   R0,8                                                             
         ST    R0,SVPWPCT                                                       
* GET ADDRESS OF PWCALC                                                         
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QPWCALC                                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VPWCALC,0(R1)                                                    
         DROP  R6                                                               
*                                                                               
CKE25    GOTO1 DATCON,DMCB,(0,ESTSTRT),(2,ETBPERDS)                             
         DS    0H                                                               
         GOTO1 DATCON,DMCB,(0,ESTEND),(2,ETBPERDE)                              
*                                                                               
         MVC   ETBOWROT,ESTOWKSD                                                
*                                  GET THE DAYPART NUMBER                       
         GOTO1 VALIDPT,DMCB,DPTCODE                                             
         BNE   ERROBJ                                                           
         MVC   ETBDPTNM,BDPTNUM                                                 
         DROP  R3                                                               
*                                                                               
         L     R0,NUMESTB                                                       
         GOTO1 BINSRCH,DMCB,(X'01',TMPENTRY),ESTTABLE,(R0),ETBRECL,    X        
               (0,ETBKEYL),MAXESTB                                              
*                                                                               
CKE30    L     R3,DMCB                                                          
         OC    DMCB+1(3),DMCB+1                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         USING ETBENTRY,R3                                                      
         MVC   TMPPERDS,ETBPERDS                                                
         MVC   TMPPERDE,ETBPERDE                                                
         MVC   TMPOWROT,ETBOWROT                                                
         MVC   TMPDPTNM,ETBDPTNM                                                
         MVC   TMPDAILY,ETDAILY                                                 
         MVC   NUMESTB,DMCB+8      NUMBER OF ESTIMATE ENTRIES                   
         DROP  R3                                                               
*                                                                               
CKEYES   B     YES                 RETURN TO CALLER                             
*                                                                               
CKENO    B     NO                                                               
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CHECKS THE ESTIMATE TABLE TO SEE IF WE ALREADY READ THE          
* ESTIMATE INFORMATION FOR THE ESTIMATE, PRODUCT AND DAYPART.                   
***********************************************************************         
FINDESTM NTR1                                                                   
         OC    NUMESTB,NUMESTB     IF NO RECORD                                 
         BZ    FNDNO               THEN NOTHING TO FIND                         
*                                                                               
         LA    R1,TMPENTRY                                                      
         USING ETBENTRY,R1                                                      
         MVC   ETBEST,ESTCODE      SET UP KEY FOR TABLE ENTRY                   
         MVC   ETBQPRD,QPRD                                                     
         MVC   ETBDPT,DPTCODE                                                   
         DROP  R1                                                               
*                                                                               
         L     R0,NUMESTB                                                       
         GOTO1 BINSRCH,DMCB,(X'00',TMPENTRY),ESTTABLE,(R0),ETBRECL,    X        
               (0,ETBKEYL),MAXESTB                                              
*                                                                               
         CLI   DMCB,X'01'          IF RECORD NOT FOUND                          
         BE    FNDNO               THEN RETURN 'NO'                             
*                                                                               
FNDYES   B     YES                 RETURN TO CALLER                             
*                                                                               
FNDNO    B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE GOAL KEY STORED IN ATMPBUF AND THEN PUTS OUT            
* INFORMATION FROM THE GOAL RECORD THAT HAS BEEN CALCULATED FOR THE             
* DOLLARS.                                                                      
***********************************************************************         
GETGKEY  NTR1                                                                   
         LA    R4,CURRKEY          POINT TO THE KEY FOR REFERENCE               
         USING GOALRECD,R4                                                      
*                                                                               
         CLI   REQMED,C'R'         TEST RADIO                                   
         BE    GGK00               YES                                          
         CLC   MDACTION,=Y(ACCBLGOL)  CABLE DOWNLOAD GOAL?                      
         BNE   EXIT                   NO, EXIT LIKE IT USE TO                   
         CLC   =C'SJ',SIGNON2C                                                  
         BE    GGK00                                                            
         CLC   =C'WI',SIGNON2C                                                  
         BE    GGK00                                                            
         CLC   =C'WJ',SIGNON2C                                                  
         BE    GGK00                                                            
         B     EXIT                NO - DO NOT PROCESS GOALS                    
*                                                                               
GGK00    CLI   OVERMODE,C'S'       IF WE DIDN'T READ OBJECT BEFORE              
         BNE   GGK30                                                            
*                                  THEN PREPARE TO GET GOAL KEYS                
         GOTO1 TMPOPEN,DMCB,=C'GET',L'TMPKEY                                    
         BNE   EXIT                                                             
*                                                                               
GGKLP    XC    KEY,KEY             GET A GOAL KEY FROM ATMPBUF                  
         GOTO1 GETTMP,DMCB,TMPKEY                                               
         OC    4(4,R1),4(R1)       IF NO MORE KEYS                              
         BZ    GGKDONE                 THEN DONE                                
*                                  ELSE GET THE RECORD                          
         MVC   CURRKEY,TMPGKEY                                                  
         MVC   KEY(L'CURRKEY),CURRKEY                                           
         GOTO1 READ                                                             
         BNE   EXIT                                                             
*                                                                               
GGK10    MVC   PRDCDE,GKEYPRD      SAVE THE PRIMARY PRODUCT CODE                
         XC    PRDCDE2,PRDCDE2     CLEAR PIGGYBACK PRODUCT                      
         MVC   QPRD2,=C'   '                                                    
*                                                                               
         CLI   GKEYPRD2,0          IF THERE IS A PIGGYBACK PRODUCT              
         BE    GGK20                                                            
         CLC   =C'**',REQMKT       OR IF THERE ARE PURPOSE CODES                
         BE    GGK20                                                            
*                                                                               
         MVC   PRDCDE2,GKEYPRD2    THEN SAVE PIGGYBACK PRODUCT CODE             
         MVC   QPRD2(L'PRDCDE),PRDCDE                                           
*                                                                               
         MVC   PRDCDE,PRDCDE2      GET THE EBCDIC EQUIVALENT                    
         BAS   RE,GETQPRD                                                       
         MVC   PRDCDE,QPRD2            ** RESTORE PRIMARY PRODUCT CODE          
         MVC   QPRD2,QPRD                                                       
*                                                                               
GGK20    BAS   RE,GETQPRD          GET EBCDIC REP OF PRIMARY PRODUCT            
*                                                                               
         BAS   RE,CNVDATES         GET BINARY EQUIVALENTS OF DATES              
*                                                                               
         BAS   RE,SETACC           SETUP THE ACCUMULATOR TABLE                  
*                                                                               
         BAS   RE,CALCGOAL         PUT GOALS INTO ACCUMULATOR TABLE             
         CLI   PURPFLAG,C'N'       TEST PURPOSE CODE MISMATCH                   
         BE    GCK32               YES - NO GOAL OBJECT REQD                    
*                                                                               
GGK30    BAS   RE,FILLFRM          PUT THE GOAL OBJECT IN FRAME                 
*                                                                               
GCK32    CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET                     
         BE    GGKX                THEN FINISHED WITH THIS FRAME                
*                                                                               
         B     GGKLP               GET ANOTHER GOAL KEY                         
*                                                                               
*                                  PUT END OF DATA OBJECT OUT                   
GGKDONE  GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         LAST DATA FRAME                              
*                                                                               
         GOTO1 TMPCLOSE            NO NEED FOR TEMP FILE ANYMORE                
*                                                                               
GGKX     B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONVERTS THE DATES IN THE PERIOD TO MONDAY DATES OR              
* TO ITS OUT OF WEEK ROTATOR EQUIVALENT                                         
*                                                                               
* RETURNS:     BINSTRT             START DATE (2-BYTE COMPRESSED)               
*              BINEND              END BOUNDARY DATE FOR RANGE TEST             
***********************************************************************         
CNVDATES NTR1                                                                   
         ZIC   R0,TMPOWROT         R0 = OUT OF WEEK ROTATOR                     
         CLI   TMPOWROT,0                                                       
         BNE   *+8                                                              
         LA    R0,1                IF NO ROTATOR, CONVERT TO MONDAY             
*                                                                               
         GOTO1 DATCON,DMCB,(2,TMPPERDS),(0,DATESTRT)                            
*                                                                               
         CLI   TMPDAILY,C'Y'       OUT OF WEEK ROTATOR MAKES NO SENSE?          
         BE    CNVD10              YES, NOT FOR DAILY                           
*                                                                               
         GOTO1 GETDAY,DMCB,DATESTRT,WORK                                        
         ZIC   R2,0(R1)                                                         
         SR    R2,R0               R2 = -(# OF DAYS FROM ROTATOR DAY)           
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,DATESTRT,DATESTRT,(R2)                                
         DS    0H                                                               
CNVD10   GOTO1 DATCON,DMCB,(0,DATESTRT),(2,BINSTRT)                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,TMPPERDE),(0,DATEEND)                             
*                                                                               
         CLI   TMPDAILY,C'Y'       OUT OF WEEK ROTATOR MAKES NO SENSE?          
         BNE   *+14                                                             
         MVC   WORK(L'DATEEND),DATEEND                                          
         B     CNVD20              YES, NOT FOR DAILY                           
*                                                                               
         GOTO1 GETDAY,DMCB,DATEEND,WORK                                         
         ZIC   R2,0(R1)                                                         
         SR    R2,R0               R2 = # OF DAYS FROM ROTATOR DAY              
         BP    *+14                PAST ESTIMATE END                            
         MVC   BINEND,TMPPERDE                                                  
         B     CNVDX                                                            
*                                                                               
* FOLLOWING STATEMENT SEEMS TO FUCK THE WEEKS UP GIVING US THE WRONG            
* GOALS.  COMMENTED OUT.  WHOA                                                  
*                                                                               
*        LA    R2,1(R2)            ONE DAY BEFORE OUT OF WEEK ROTATOR           
*                                                                               
         LNR   R2,R2                                                            
         GOTO1 ADDAY,DMCB,DATEEND,DATEEND,(R2)                                  
*                                                                               
*                                  COMPUTE 2-BYTE END DATE BOUNDARY             
         DS    0H                                                               
         GOTO1 ADDAY,DMCB,DATEEND,WORK,7                                        
         DS    0H                                                               
CNVD20   GOTO1 DATCON,DMCB,(0,WORK),(2,BINEND)                                  
*                                                                               
CNVDX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE SETS UP THE ACCUMULATOR AND THE PERIOD TABLES.                   
*                                                                               
* RETURNS:     NUMWEEKS            NUMBER OF WEEKS IN PERIOD                    
***********************************************************************         
SETACC   NTR1                                                                   
         LA    R5,PRDWEEKS         R5 = A(PERIOD TABLE OF WEEKS)                
         LA    R6,ACCTAB           R6 = A(1ST WEEK IN ACCUMULATOR TAB)          
         USING ACCTABD,R6                                                       
*                                  WORK = DATE TO BE CONVERTED                  
         MVC   WORK(L'DATESTRT),DATESTRT                                        
         SR    R3,R3               R3 = # OF WEEKS IN PERIOD                    
*                                                                               
*                                  STORE DATE (2-BYTES) IN THE TABLES           
SETA10   GOTO1 DATCON,DMCB,(0,WORK),(2,ACCWEEK)                                 
         MVC   0(L'ACCWEEK,R5),ACCWEEK                                          
*                                                                               
         XC    ACCGRP,ACCGRP       NO GOAL PTS FOR THIS WEEK                    
         XC    ACCBUDGT,ACCBUDGT   NO GOAL DOLLARS FOR THIS WEEK                
*                                                                               
         CLI   TMPDAILY,C'Y'                DAILY GOALS?                        
         BNE   SETA20                                                           
         GOTO1 ADDAY,DMCB,WORK,WORK,F'1'    YES                                 
         B     SETA30                                                           
*                                  BUMP TO THE NEXT WEEK                        
SETA20   GOTO1 ADDAY,DMCB,WORK,WORK,F'7'                                        
*                                                                               
SETA30   LA    R5,L'ACCWEEK(R5)                                                 
         LA    R6,ACCTABL(R6)                                                   
         LA    R3,1(R3)                                                         
*                                  IF DATE IS STILL WITHIN THE PERIOD           
         CLC   WORK(L'DATEEND),DATEEND                                          
         BNH   SETA10              THEN LOOP BACK                               
*                                                                               
         STC   R3,NUMWEEKS         ELSE SAVE NUMBER OF WEEKS                    
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,0(R5))                                   
         MVC   ACCWEEK,0(R5)                                                    
         MVI   L'ACCWEEK(R5),X'FF'                                              
*                                                                               
         B     XIT                 DONE SETTING THE TABLE                       
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DOES THE GOAL CALCULATIONS TO THE ACCUMULATOR TABLE.             
***********************************************************************         
CALCGOAL NTR1                                                                   
         MVI   PURPFLAG,0          CLEAR FLAG                                   
         GOTO1 GETREC              GET GOAL RECORD                              
*                                                                               
         DS    0H                                                               
         GOTO1 GETELEM,DMCB,32     R6 = A(DESCRIPTION X'20' ELEMENT)            
         USING GDELEM,R6                                                        
*                                                                               
         CLC   =C'**',REQMKT       TEST PURPOSE CODES                           
         BNE   CALC2                                                            
         CLC   GDIDR,REQMKT+2      TEST MATCH                                   
         BE    CALC2               YES                                          
         MVI   PURPFLAG,C'N'       SET PURPOSE CODE MISMATCH                    
         B     CALCX                                                            
*                                                                               
CALC2    MVC   CPPCLT,GDCPPCL      GET CPP CLIENT CODE                          
         MVC   CPPEST,GDCPPES      GET CPP ESTIMATE NUMBER                      
         MVC   CPPEST2,GDCPPES2    GET SECOND CPP ESTIMATE NUMBER               
*                                                                               
         GOTO1 GETELEM,DMCB,33     R6 = A(1ST GOAL WEEK X'21' ELEMENT)          
*                                                                               
CALC10   BNE   CALCX               IF NO ELEM, THEN GET NEXT GOAL REC           
         USING GLEMENT,R6                                                       
         CLC   GLWEEK,BINSTRT      IF WEEK NOT IN ESTIMATE PERIOD               
         BL    CALCNX                                                           
         CLI   TMPDAILY,C'Y'       IF DOING DAILY                               
         BNE   CALC12                                                           
         CLC   GLWEEK,BINEND                                                    
         BH    CALCNX              SKIP IF DATE HIGH                            
         B     CALC14              ELSE PROCESS                                 
*                                                                               
CALC12   CLC   GLWEEK,BINEND                                                    
         BH    CALCNX              THEN GET NEXT GOAL WEEK ELEMENT              
*                                                                               
CALC14   LA    R2,ACCTAB           R2 = A(1ST WEEK IN ACCUMULATOR TAB)          
         USING ACCTABD,R2                                                       
*                                                                               
CALC20   CLC   GLWEEK,ACCWEEK      IF WEEK BETWEEN THE 2 WEEKS                  
         BNH   CALCCPP                                                          
         CLC   GLWEEK,ACCWEEK+ACCTABL                                           
         BL    CALCCPP             THEN CHECK FOR USE OF CPP                    
*                                                                               
         LA    R2,ACCTABL(R2)      ELSE BUMP R2 TO NEXT WK                      
*                                                                               
         B     CALC20              LOOP BACK                                    
*                                                                               
CALCCPP  MVC   BUCDATE,GLWEEK      MAKE A COPY OF THE WEEK                      
         MVC   BUCGPTS,GLGRP                          GOAL POINTS               
         MVC   BUCGDOLS,GLBUDGET                      GOAL DOLLARS              
*                                                                               
         OC    BUCGDOLS,BUCGDOLS   IF ZERO GOAL DOLLARS OR ZERO GOAL            
         BZ    *+18                    POINTS                                   
         OC    BUCGPTS,BUCGPTS     THEN CHECK FOR CPP = Y                       
         BZ    *+8                                                              
         B     CALCADD             ELSE ADD AMOUNTS TO ACCUMULATOR              
*                                                                               
         CLI   CPROFILE+8,C'0'     IF CPP NOT REQUESTED                         
         BNE   CALCADD             THEN ADD THE AMOUNTS TO ACCUMULATOR          
*                                                                               
         CLC   CPPMKT,KEY+5        TEST CHANGE OF MARKET                        
         BE    CALCPP2                                                          
         MVI   POLGIND,0           FORCE NEW CPP GUIDE                          
         MVC   CPPMKT,KEY+5        AND SAVE THE NEW MARKET                      
*                                                                               
CALCPP2  CLI   CPPEST,0            IF NEW CPP GUIDE NOT USED                    
         BE    CALCPP10            THEN TEST IF WE GOT THIS CPP FOR EST         
*                                                                               
         CLI   POLGIND,0           IF CPP GUIDE NOT RECALLED                    
         BE    CALCPP20            THEN READ IT                                 
         B     CALCCONV            ELSE DO THE CONVERSION                       
*                                                                               
CALCPP10 CLC   POLGIND,KEY+7       IF WE GOT CPP FOR THIS EST                   
         BE    CALCCONV            THEN DO THE CONVERSION                       
*                                                                               
CALCPP20 BAS   RE,GETGUIDE         GET CPP GUIDES                               
         MVC   KEY(13),CURRKEY     RESTORE KEY                                  
         GOTO1 READ                                                             
         DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         DS    0H                                                               
CALCCONV GOTO1 CPPCONV,DMCB,(TMPDPTNM,BUCGPTS),(KEY+10,BUCGDOLS),      X        
               (KEY+8,0)                                                        
*                                                                               
CALCADD  ICM   R1,15,BUCGPTS       ADD GOAL POINTS TO ACCUMULATOR'S             
         ICM   R0,15,ACCGRP            GOAL POINTS                              
         AR    R1,R0                                                            
         STCM  R1,15,ACCGRP                                                     
*                                                                               
         ICM   R1,15,BUCGDOLS      ADD GOAL DOLLARS TO ACCUMULATOR'S            
         ICM   R0,15,ACCBUDGT          GOAL DOLLARS                             
         AR    R1,R0                                                            
         STCM  R1,15,ACCBUDGT                                                   
*                                                                               
CALCNX   GOTO1 NEXTELEM            GET NEXT GOAL WEEK ELEMENT                   
         B     CALC10              LOOP BACK                                    
*                                                                               
CALCX    B     XIT                 DONE CALCULATING THE GOAL                    
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE FILLS THE FRAME WITH A GOAL OBJECT.                              
*                                                                               
* GOAL OBJECT DATA:                                                             
* BYTE  0 -  3 BYTES - MASTER PRODUCT                                           
* BYTE  3 -  3 BYTES - PIGGYBACK PRODUCT                                        
* BYTE  6 -  2 BYTES - ESTIMATE                                                 
* BYTE  8 -  4 BYTES - MARKET NUMBER                                            
* BYTE 12 -  1 BYTE  - DAYPART CODE                                             
* BYTE 13 -  2 BYTES - SPOT LENGTH                                              
* BYTE 15 -  2 BYTES - TOTAL SPOT LENGTH                                        
* BYTE 17 - 12 BYTES - ESTIMATE'S PERIOD                                        
* BYTE 29 - ?? BYTES - GOAL POINTS AND DOLLARS                                  
***********************************************************************         
FILLFRM  NTR1                                                                   
         LA    R2,OUTAREA          R2 = A(OUTPUT AREA)                          
         LA    R4,CURRKEY          R4 = A(GOAL KEY CALCULATED)                  
         USING GOALRECD,R4                                                      
*                                                                               
         MVC   0(L'QPRD,R2),QPRD   SHOW THE PRODUCT(S) WE HAVE                  
         MVC   L'QPRD(L'QPRD2,R2),QPRD2                                         
         LA    R2,L'QPRD+L'QPRD2(R2)                                            
*                                                                               
*                                  PUT OUT THE ESTIMATE CODE                    
FILLF10  GOTO1 HEXOUT,DMCB,GKEYEST,0(R2),L'GKEYEST                              
         LA    R2,L'ESTCODE*2(R2)                                               
*                                                                               
*                                  PUT OUT MARKET NUMBER W/ LEADING 0'S         
FILLF20  ZICM  R0,GKEYMKT,(3)                                                   
         CVD   R0,DUB                                                           
         UNPK  0(L'GKEYMKT*2,R2),DUB+5(3)                                       
         OI    L'GKEYMKT*2-1(R2),X'F0'                                          
         LA    R2,L'GKEYMKT*2(R2)                                               
*                                                                               
*                                  PUT OUT THE DAYPART CODE                     
FILLF30  MVC   0(L'GKEYDPT,R2),GKEYDPT                                          
         LA    R2,L'GKEYDPT(R2)                                                 
*                                                                               
*                                  PUT OUT MASTER PRODUCT SPOT LENGTH           
FILLF40  GOTO1 HEXOUT,DMCB,GKEYSLN,0(R2),L'GKEYSLN                              
         LA    R2,L'GKEYSLN*2(R2)                                               
*                                  PUT OUT TOTAL SPOT LENGTH                    
         GOTO1 HEXOUT,DMCB,GKEYSEC,0(R2),L'GKEYSEC                              
         LA    R2,L'GKEYSEC*2(R2)                                               
*                                  PUT OUT THE INTERSECTION PERIOD              
FILLF50  MVC   0(2*L'DATESTRT,R2),DATESTRT                                      
         CLI   0(R2),X'FA'                                                      
         BL    FILLF51                                                          
         ZIC   R1,0(R2)                                                         
         S     R1,=F'10'                                                        
         STC   R1,0(R2)                                                         
FILLF51  CLI   6(R2),X'FA'                                                      
         BL    FILLF52                                                          
         ZIC   R1,6(R2)                                                         
         S     R1,=F'10'                                                        
         STC   R1,6(R2)                                                         
FILLF52  LA    R2,2*L'DATESTRT(R2)                                              
*                                                                               
         ZIC   R3,NUMWEEKS         R3 = NUMBER OF GOALS TO OUTPUT               
         LA    R4,ACCTAB           R4 = A(1ST ENTRY IN ACCUMULATOR TAB)         
         USING ACCTABD,R4                                                       
*                                                                               
FILLF60  OC    SVC2FCTR,SVC2FCTR       TEST COS2 ESTIMATE                       
         BZ    FILLF62                                                          
* ADJUST GOAL DOLLARS BY COS2 FACTOR                                            
         ICM   R0,15,ACCBUDGT                                                   
         CVD   R0,DUB              MAKE AMOUNT PACKED                           
         ZAP   P16,DUB                                                          
         SRP   P16,6,0             SCALE UP BY 10**6                            
*                                                                               
         L     R0,SVC2FCTR         GET COS2 FACTOR                              
         CVD   R0,DUB              MAKE IT PACKED                               
         DP    P16,DUB             GOAL DOLS/FACTOR                             
*                                                                               
         ZAP   DUB,P16(8)          GET QUOTIENT                                 
         CVB   R0,DUB                                                           
         STCM  R0,15,ACCBUDGT                                                   
         B     FILLF70                                                          
*                                                                               
FILLF62  OC    SVPWPCT,SVPWPCT     TEST PW ESTIMATE                             
         BZ    FILLF70             NO                                           
* CALL PWCALC TO ADJUST GOAL DOLLARS                                            
         XC    WORK,WORK                                                        
         LA    R6,WORK                                                          
         USING PWBLKD,R6                                                        
         MVI   PWACT,PWGETGOL                                                   
         MVC   PWPCT,SVPWPCT                                                    
         MVC   PWACTGOL,ACCBUDGT                                                
         GOTO1 VPWCALC,DMCB,(R6)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ACCBUDGT,PWVAL                                                   
         DROP  R6                                                               
*                                  PUT NUMBERS TO OUTPUT AREA                   
FILLF70  GOTO1 HEXOUT,DMCB,ACCGRP,0(R2),L'ACCGRP+L'ACCBUDGT                     
*                                                                               
*                                  BUMP TO NEXT WEEK                            
         LA    R2,L'ACCGRP*2*2(R2)                                              
         LA    R4,ACCTABL(R4)                                                   
*                                                                               
         BCT   R3,FILLF60          IF MORE WEEKS THEN LOOP BACK                 
*                                                                               
         LA    R0,OUTAREA          PUT OUT GOAL OBJECT                          
         SR    R2,R0                                                            
         L     R3,=A(ITGOALS)                                                   
         GOTO1 PUTITEM,DMCB,(R3),(R2),OUTAREA                                   
         BNE   EXIT                                                             
*                                                                               
FILLFX   B     XIT                 RETURN TO CALLER                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WAS COPIED FROM SPMIS01.  ASK MEL IF YOU HAVE QUESTIONS.         
*                                                                               
* THIS ROUTINE GETS 30 SEC CPP GUIDES AND SETS POLGIND TO EST NUMBER            
* READ.                                                                         
***********************************************************************         
GETGUIDE NTR1                                                                   
         MVC   AIO,AIO2            USE ANOTHER IOAREA                           
*                                                                               
         L     R0,AGUIDES          CLEAR WEEKLY CPP GUIDES BY DAYPART           
         LA    R1,15*LENGUIDE                                                   
         SR    RF,RF                                                            
         LR    RE,R0                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   POLGIND,KEY+7       SAVE GOAL EST NUMBER                         
         MVI   KEY+4,X'FF'         CPP ARE IN REALLY POL GOALS                  
*                                                                               
         MVI   BYTE4,0             NO NEED FOR SECOND CPP GUIDE YET             
*                                                                               
         CLI   CPPEST,0            IF NOT USING NEW CPP GUIDE                   
         BE    GETGD10             THEN CLEAR REST OF KEY FROM DAYPART          
*                                                                               
         MVC   KEY+2(2),CPPCLT                                                  
         MVC   KEY+7(1),CPPEST     SAVE FIRST EST NUMBER                        
         MVC   POLGIND,KEY+7                                                    
         MVC   BYTE4,CPPEST2       SAVE SECOND CPP EST NUMBER                   
*                                                                               
GETGD10  XC    KEY+8(5),KEY+8      CLEAR REST OF KEY FROM DAYPART ON            
*                                                                               
         GOTO1 HIGH                READ THE FIRST CPP GUIDE                     
         B     GETGD20                                                          
*                                                                               
GETGD15  GOTO1 SEQ                                                              
*                                                                               
GETGD20  CLC   KEY(8),KEYSAVE      02/A-M/C/P/MKT/EST                           
         BE    GETGD30             GET RECORD IF KEYS MATCH                     
*                                                                               
         CLI   BYTE4,0             IF WE DON'T NEED SECOND CPP GUIDE            
         BE    GETGDX              THEN RETURN TO CALLER                        
*                                                                               
         XC    KEY,KEY             ELSE USE 2ND CPP GUIDE EST #                 
         MVC   KEY(7),KEYSAVE                                                   
         MVC   KEY+7(1),BYTE4                                                   
         MVI   BYTE4,0                                                          
         B     GETGD10                                                          
*                                                                               
GETGD30  GOTO1 GETREC              READ RECORD                                  
*                                                                               
         L     R5,AGUIDES          R5 = A(WEEKLY CPP GUIDE W/ DAYPART)          
         LA    R0,15               R0 = MAXIMUM # OF DAYPARTS                   
*                                                                               
GETGD40  CLC   0(1,R5),KEY+8       GET DAYPART SLOT OR MAKE ONE                 
         BE    GETGD50                                                          
         CLI   0(R5),0                                                          
         BE    GETGD50                                                          
         LA    R5,LENGUIDE(R5)                                                  
         BCT   R0,GETGD40                                                       
         DC    H'0'                TOO MANY DAYPARTS                            
*                                                                               
GETGD50  MVC   0(1,R5),KEY+8       COPY DAYPART TO TABLE                        
         LA    R5,1(R5)            R5 = A(FIRST WEEK'S DOLLARS)                 
         LA    R4,PRDWEEKS         R4 = A(WEEKS IN PERIOD LIST)                 
*                                                                               
         GOTO1 GETELEM,DMCB,33     GET GOAL WEEK ELEMENT (X'21')                
         USING GLEMENT,R6                                                       
*                                                                               
GETGD60  CLI   0(R6),X'21'         IF NOT X'21' ELEMENT                         
         BNE   GETGD15             THEN GET NEXT CPP GUIDE                      
*                                                                               
         CLC   2(2,R6),0(R4)       IF ELEMENT'S DATE LESS THAN TABLE'S          
         BL    GETGD70             THEN GET NEXT ELEMENT                        
*                                                                               
         CLI   2(R4),X'FF'         IF THERE ARE NO MORE WKS AFTER               
         BE    GETGD15             THEN GET NEXT CPP GUIDE                      
*                                                                               
         CLC   2(2,R6),2(R4)       IF ELEM'S DATE >= TBL'S NEXT WK              
         BNL   GETGD80             THEN BUMP TO NEXT WEEK                       
*                                                                               
GETGD65  MVC   0(4,R5),8(R6)       ELSE COPY DOLLARS                            
         B     GETGD80             BUMP TO NEXT WEEK                            
*                                                                               
GETGD70  GOTO1 NEXTELEM            R6 = A(NEXT ELEMENT)                         
         B     GETGD60             LOOP BACK AND USE THIS ELEMENT               
*                                                                               
GETGD80  LA    R5,4(R5)            R5 = A(NEXT WEEK'S DOLLARS)                  
         LA    R4,2(R4)            R4 = A(NEXT WEEK IN PERIOD)                  
*                                                                               
         CLI   0(R4),X'FF'         IF LAST MONTH                                
         BE    GETGD15             THEN GET NEXT CPP GUIDE                      
         B     GETGD60             ELSE TEST WITH THIS WEEK                     
*                                                                               
GETGDX   MVC   AIO,AIO1            RESTORE OUR REGULAR IOAREA                   
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WAS COPIED FROM SPMIS01.  ASK MEL IF YOU HAVE QUESTIONS.         
* MODIFIED TO FIT INTO OVERLAY.                                                 
*                                                                               
* ROUTINE TO CONVERT DOLLARS VIA CPP GUIDE                                      
*                                                                               
*        P1=A(GOAL POINTS)         BYTE 0 = DAYPART CODE)                       
*        P2=A(GOAL DOLLARS)        BYTE 0 = SPOT LENGTH                         
*        P3=        ---            BYTE 0 = ALPHA DAYPART)                      
***********************************************************************         
CPPCONV  NTR1                                                                   
         ST    R1,FULL             SAVE PARM REG                                
         XC    DUB,DUB             CLEAR FOR EQUIVALENCE FACTORS                
         SPACE 1                                                                
* GET EQUIVALENCE FACTOR FROM EQUREC *                                          
         SPACE 1                                                                
         IC    R4,0(R1)            DAYPART CODE                                 
         N     R4,=F'15'           GROUP MENUS IGNORED                          
         IC    R4,EQUDPT(R4)       TABLE IND                                    
         LTR   R4,R4                                                            
         BNZ   *+8                                                              
         LA    R4,1                                                             
         MH    R4,=H'60'                                                        
*                                                                               
         LA    R4,EQUSECT1-60(R4)  R4 = A(EQUIV TABLE FOR THE DAYPART)          
         LA    R5,SLNTAB           R5 = A(SPOT LENGTH TABLE)                    
*                                                                               
         MVC   DUB+2(2),12(R4)     30 SECOND FACTOR                             
         MVC   DUB+6(2),12(R4)     DEFAULT EQUIVALENCY FACTOR                   
*                                                                               
CPPCV100 CLI   0(R5),0             IF SPOT LENGTH NOT IN TABLE                  
         BE    CPPCV201            THEN USE DEFAULT EQUIVALENCY FACTOR          
*                                                                               
         CLC   0(1,R5),4(R1)       IF SPOT LENGTH IS IN TABLE                   
         BE    CPPCV200            THEN WE HAVE EQUIVALENCY FACTOR              
*                                                                               
         LA    R4,4(R4)            ELSE R4 = A(NEXT EQUIV. FACTOR)              
         LA    R5,1(R5)                 R5 = A(NEXT SPOT LENGTH)                
         B     CPPCV100                 LOOP BACK TIL WE MATCH LENGTHS          
*                                                                               
CPPCV200 MVC   DUB+6(2),0(R4)      SAVE EQUIV FACTOR                            
*                                                                               
CPPCV201 L     R4,AGUIDES          GET CPP GUIDE PTS AND $'S                    
         LA    R5,15                                                            
CPPCV202 CLC   0(1,R4),8(R1)       MATCH DPT                                    
         BE    CPPCV204            ELSE RETURN TO CALLER WITH NOTHING           
         LA    R4,LENGUIDE(R4)                                                  
         CLI   0(R4),0                                                          
         BE    CPPCVXIT                                                         
         BCT   R5,CPPCV202                                                      
         B     CPPCVXIT                                                         
* FIND WEEKLY CPP                                                               
CPPCV204 LA    R4,1(R4)            R4 = A(FIRST WEEK'S DOLLARS)                 
         LA    R5,PRDWEEKS         R5 = A(WEEKS IN PERIOD LIST)                 
*                                                                               
CPPCV206 CLC   BUCDATE,0(R5)       IGNORE IF PRIOR TO WEEK START                
         BL    CPPCV208                                                         
*                                                                               
         CLC   BUCDATE,2(R5)       USE IF BEFORE NEXT WEEK                      
         BL    CPPCV210                                                         
*                                                                               
CPPCV208 LA    R4,4(R4)            R4 = A(NEXT WEEK'S DOLLARS)                  
         LA    R5,2(R5)            R5 = A(NEXT WEEK'S DATE)                     
*                                                                               
         CLI   0(R5),X'FF'         IF DATE NOT IN PERIOD LIST                   
         BNE   CPPCV206                                                         
         B     INVLCPPG            THEN ERROR, CAN'T FIND CPP GUIDE             
         EJECT                                                                  
*****                                                                           
* CALCULATE EITHER THE GOAL POINTS OR THE GOAL DOLLARS                          
*****                                                                           
*                                  RE = A(GOAL POINTS)                          
CPPCV210 LM    RE,RF,0(R1)         RF = A(GOAL DOLLARS)                         
*                                                                               
         OC    0(4,RF),0(RF)       IF NOT ZERO GOAL DOLLARS                     
         BNZ   CPPPTS              THEN CALCULATE GOAL POINTS                   
*****                                                                           
*                GOAL POINTS * CPP$     EF                                      
* GOAL DOLLARS = ------------------- * ----                                     
*                      CPP PTS         EF30                                     
*****                                                                           
         L     R3,0(RE)            R3 = GOAL PTS                                
         MVC   FULL,0(R4)          CPP DOLLARS                                  
*                                                                               
         M     R2,DUB+4            (R2,R3) = GOAL PTS * CPP$ * EQUV FAC         
         M     R2,FULL                                                          
*                                                                               
         LH    R8,DUB+2            R8 = EF30 * 1000                             
         MH    R8,=H'1000'                                                      
*                                                                               
         DR    R2,R8               GP * CPP$     EF                             
*                                  ---------- * ----                            
*                                     1000      EF30                            
         SRA   R8,1                                                             
         CR    R2,R8               REMAINDER                                    
         BL    *+8                                                              
         LA    R3,1(R3)            ROUND                                        
*                                                                               
         ST    R3,0(RF)            SAVE GOAL DOLLARS CALCULATED                 
         B     CPPCVXIT                                                         
*****                                                                           
*               GOAL DOLLARS * 1000     EF30                                    
* GOAL POINTS = -------------------  *  ----                                    
*                       CPP$             EF                                     
*****                                                                           
CPPPTS   L     R3,0(RF)            (R2,R3) = (GOAL $ * 2000)/CPP$               
         M     R2,=F'2000'                                                      
         D     R2,0(R4)                                                         
         A     R3,=F'1'            ROUND                                        
         SRL   R3,1                / 2                                          
         AR    R3,R3               * 2                                          
*                                                                               
         M     R2,DUB              * EF30                                       
         LH    R8,DUB+6            / EF                                         
         DR    R2,R8                                                            
         A     R3,=F'1'            ROUND                                        
         SRL   R3,1                / 2                                          
         ST    R3,0(RE)            SAVE GOAL POINTS CALCULATED                  
*                                                                               
CPPCVXIT B     XIT                 RETURN TO CALLER                             
*                                                                               
SLNTAB   DS    0H                                                               
       ++INCLUDE SPSLNTAB                                                       
         DC    5X'00'              EOL FLAGS                                    
         SPACE 1                                                                
***********************************************************************         
* ERRORS USED BY THIS OVERLAY.                                                  
***********************************************************************         
*                                  NO ACCESS TO THIS CLIENT                     
NOACCESS MVC   APPLERR,=Y(ER05NOCL)                                             
         B     ERROBJ                                                           
*                                  INVALID GOAL REQUEST OBJECT                  
INVLRQST MVC   APPLERR,=Y(ER05RQST)                                             
         B     ERROBJ                                                           
*                                  REQUESTED EST CODE NOT VALID HEX             
INVLQEST MVC   APPLERR,=Y(ER05QEST)                                             
         B     ERROBJ                                                           
*                                  GOAL ESTIMATE RECORD NOT FOUND               
INVLGEST MVC   APPLERR,=Y(ER05GEST)                                             
         B     ERROBJ                                                           
*                                  INVALID PERIOD                               
INVLPERD MVC   APPLERR,=Y(ER05PERD)                                             
         B     ERROBJ                                                           
*                                  PRD CODE NOT IN CLT PRODUCT LIST             
INVLPLST MVC   APPLERR,=Y(ER05PLST)                                             
         B     ERROBJ                                                           
*                                  INVALID SPOT LENGTH                          
INVLSLN  MVC   APPLERR,=Y(ER05SLN)                                              
         B     ERROBJ                                                           
*                                  INVALID MARKET                               
INVLMKT  MVC   APPLERR,=Y(ER05MKT)                                              
         B     ERROBJ                                                           
*                                  INVALID GOAL (GOAL NOT FOUND)                
INVLGOAL MVC   APPLERR,=Y(ER05GOAL)                                             
         B     ERROBJ                                                           
*                                  CAN'T FIND CPP GUIDE                         
INVLCPPG MVC   APPLERR,=Y(ER05CPPG)                                             
         B     ERROBJ                                                           
*                                  INVALID POL REQUEST                          
INVLPOLR MVC   APPLERR,=Y(ER05POLR)                                             
         B     ERROBJ                                                           
*                                                                               
NOPURPS  MVC   APPLERR,=Y(ER05NOPR) NO PURPOSE CODE ALLOWED                     
         B     ERROBJ                                                           
*                                                                               
MSSGPURP MVC   APPLERR,=Y(ER05MSPR) PURPOSE CODE REQD                           
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
         MVI   FATALERR,C'Y'       STOP PROCESSING !                            
*                                                                               
         GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,ITAMFMER,4,FULL                                     
         BNE   EXIT                                                             
*                                  PUT END OF DATA ITEM                         
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*=========================================================                      
* CALL OFFICER TO VALIDATE LIMIT ACCESS                                         
*=========================================================                      
         SPACE 1                                                                
CALLOFCR NTR1  BASE=*,LABEL=*                                                   
         SPACE 1                                                                
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         OC    TWASAGN,TWASAGN          ON NEW SECURITY                         
         BNZ   *+14                                                             
         OC    TWAACCS,TWAACCS          OR HAVE LIMIT ACCESS                    
         JZ    YES                                                              
         DROP  RE                                                               
*                                                                               
         L     R0,AIO                                                           
         LHI   R1,1024                                                          
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     RF,ACOMFACS         INITIALIZE SECRET                            
         L     RF,CSECRET-COMFACSD(RF)                                          
         GOTO1 (RF),DMCB,('SECPINIT',AIO),0                                     
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A38' GET OFFICER ADDRESS                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    BLOCK,BLOCK                                                      
         LA    R1,BLOCK                                                         
         USING OFFICED,R1                                                       
*                                                                               
         MVI   OFCSYS,C'S'                                                      
         L     RE,ATWA                                                          
         USING TWAD,RE                                                          
         MVC   OFCAUTH,TWAACCS     SET AUTH CODE PASSED TO US                   
         MVC   OFCLMT(4),TWAACCS   4 CHAR VERSION TOO                           
         DROP  RE                                                               
         MVC   OFCAGY,SIGNON2C                                                  
         MVC   OFCOFC,SVOFFC                                                    
         MVC   OFCCLT,QCLT                                                      
         MVC   OFCSAGMD,BAGYMED                                                 
         MVC   OFCACCSC(3),SVCACCS    ACCESS LIST FROM CLTHDR                   
         MVC   OFCSECD,AIO                                                      
         DROP  R1                                                               
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'N',BLOCK),ACOMFACS                                  
         CLI   0(R1),0                                                          
         JE    YES                                                              
         J     NO                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
*                                                                               
AGUIDES  DS    A                   A(WEEKLY CPP GUIDES BY DAYPART)              
*                                                                               
*                   OBJECTS                                                     
*                                                                               
REQUEST  DS    0C                  STRUCTURE OF GOAL REQUEST OBJECT             
REQMED   DS    CL1                 MEDIA                                        
REQCLT   DS    CL3                 CLIENT NAME                                  
REQPRDS  DS    0CL6                PRODUCTS                                     
REQPR1   DS    CL3                 1ST PRODUCT                                  
REQPR2   DS    CL3                 2ND OR PIGGYBACK PRODUCT                     
REQEST   DS    CL2                 EST # XL1 EBCDIC (BLANK IF ALL)              
REQPERD  DS    0CL12               REQUEST PERIOD                               
REQSTDT  DS    CL6                 REQUEST START DATE                           
REQNDDT  DS    CL6                 REQUEST END DATE                             
REQDYPT  DS    CL1                 REQUEST DAYPART (BLANK IF ALL)               
REQSPLN  DS    CL2                 REQUEST SPOT LENGTH (BLANK IF ALL)           
REQMKT   DS    (MAXMNUMS)CL4       LIST OF MKT'S (NONE IF ALL)                  
*                                  OR **PURPCD FOLLOWED BY MARKETS              
REQLEN   EQU   *-REQUEST                                                        
*                                                                               
*                   MISCELLANEOUS                                               
*                                                                               
DATESTRT DS    CL6                 PERIOD START DATE                            
DATEEND  DS    CL6                 PERIOD END DATE                              
BINSTRT  DS    XL2                 BINARY START DATE                            
BINEND   DS    XL2                 BINART END BOUNDARY DATE                     
NUMWEEKS DS    XL1                 NUMBER OF WEEK WITHIN PERIOD                 
*                                                                               
BITFLAG1 DS    XL1                 BYTE WHICH TELLS WHAT FLAGS ARE SET          
BF1NWGOL EQU   X'80'                 NEW GOAL REQUEST, CK EST IF DAILY          
BF1PLPRD EQU   X'40'                 POL REQUESTED                              
BF1PIGGY EQU   X'20'                 PIGGYBACK REQUESTED                        
*                                                                               
QPRD2    DS    CL3                 PIGGYBACK PRODUCT                            
PRDCDE2  DS    XL1                 PIGGYBACK PRODUCT CODE                       
ALLFLAGS DS    XL1                 BYTE WHICH TELLS WHAT REQUESTED ALL          
*                                  X'80' - ALL PRODUCTS REQUESTED               
*                                  X'40' - ALL MARKET NUMBERS REQUESTED         
*                                  X'20' - ALL ESTIMATES REQUESTED              
*                                  X'10' - ALL DAYPARTS REQUESTED               
*                                  X'08' - ALL SPOT LENGTHS REQUESTED           
NUMMKTS  DS    XL1                 # OF MARKET NUMBERS IN LIST                  
MKTLIST  DS    XL(2*(MAXMNUMS+1))  VARIABLE LENGTH LIST FOR MARKET #'S          
NUMGOALS DS    XL2                 NUMBER OF GOALS READ                         
CURRKEY  DS    XL(L'GKEY)          EXTRA GOAL KEY                               
*                                                                               
*                                  KEY SAVED IN TEMPSTR                         
TMPKEY   DS    0XL(L'GKEY+2*2+L'ESTDYMNU)                                       
TMPGKEY  DS    XL(L'GKEY)          GOAL KEY                                     
TMPPERDS DS    XL2                 START DATE OF INTERSECTION                   
TMPPERDE DS    XL2                 END DATE OF INTERSECTION                     
TMPOWROT DS    XL1                 OUT OF WEEK ROTATOR                          
TMPDPTNM DS    XL1                 DAYPART NUMBER                               
TMPDAILY DS    CL1                 DAILY GOAL INDICATOR                         
*                                                                               
CPPCLT   DS    XL2                 CPP CLIENT CODE                              
CPPMKT   DS    XL2                                                              
CPPEST   DS    XL1                 CPP ESTIMATE NUMBER                          
CPPEST2  DS    XL1                 CPP SECOND ESTIMATE NUMBER                   
POLGIND  DS    CL1                 POL CPP GUIDE INDICATOR                      
PRDWEEKS DS    XL(53*2)            PERIOD SET UP IN WEEKS                       
EQUREC   DS    XL164               EQUIVALENCE RECORD                           
BYTE4    DS    XL1                 SPARE BYTE                                   
TMPENTRY DS    XL(ETBRECL)         TEMPORARY RECORD FOR ESTIMATE TABLE          
NUMESTB  DS    F                   NUMBER OF ESTIMATE ENTRIES IN TABLE          
SVPWPCT  DS    F                                                                
VPWCALC  DS    A                                                                
SVC2FCTR DS    F                                                                
SVB0PROF DS    CL16                G0 PROFILE                                   
         DS    0D                                                               
P16      DS    PL16                                                             
*                                                                               
*                   THINGS FROM THE GOAL ELEMENT                                
*                                                                               
BUCDATE  DS    XL2                 WEEK OF GOAL ELEMENT                         
BUCGPTS  DS    XL4                 GOAL POINTS OF GOAL ELEMENT                  
BUCGDOLS DS    XL4                 GOAL DOLLARS OF GOAL ELEMENT                 
*                                                                               
*                   KEY USED TO BUMP THROUGH GOALS                              
*                                                                               
PARTKEY  DS    0CL10               PARTIAL KEY                                  
TYPECODE DS    XL1                 RECORD TYPE X'02'                            
TYPECODQ EQU   X'02'                                                            
AGMDCODE DS    XL1                 AGENCY/MEDIA COMBINATION                     
CLTCODE  DS    XL2                 CLIENT PACKED                                
PRDCDE   DS    XL1                 PRODUCT CODE                                 
MKTCODE  DS    XL2                 MARKET CODE                                  
ESTCODE  DS    XL1                 ESTIMATE CODE                                
DPTCODE  DS    XL1                 DAYPART CODE                                 
SLNCODE  DS    XL1                 SPOT LENGTH CODE                             
PURPFLAG DS    C                   C'N' IF PURPOSE CODE MISMATCH                
FATALERR DS    C                   STOP PROCESSING IF Y                         
SVOFFC   DS    C                                                                
SVCACCS  DS    CL3                                                              
*                                                                               
*                   TABLES                                                      
*                                                                               
         DS    0D                                                               
ACCTAB   DS    (MAXWEEKS)CL(ACCTABL)      ACCUMULATOR TABLE                     
         DS    0D                                                               
ESTTABLE DS    (MAXESTB)XL(ETBRECL)       ESTIMATE TABLE                        
*                                                                               
*                   AREA USED TO PUT/GET GOAL OBJECT                            
*                                                                               
         DS    0D                                                               
OUTAREA  DS    CL2048                                                           
*                                                                               
*                   EQUATES                                                     
*                                                                               
MAXWEEKS EQU   53                  MAXIMUM NUMBER OF WEEKS                      
MAXMNUMS EQU   10                  MAXIMUM NUMBER OF MARKET NUMBERS             
MAXESTB  EQU   200                 MAXIMUM NUMBER OF ESTIMATE ENTRIES           
         EJECT                                                                  
*                                                                               
*                   EQUIVALENCE RECORD DSECT                                    
*                                                                               
         ORG   EQUREC                                                           
       ++INCLUDE SPGENEQU                                                       
         EJECT                                                                  
*                                                                               
*                   ACCUMULATOR TABLE DSECT                                     
*                                                                               
ACCTABD  DSECT                                                                  
ACCWEEK  DS    XL2                 DATE FOR THE WEEK                            
ACCGRP   DS    XL4                 GOAL POINTS                                  
ACCBUDGT DS    XL4                 GOAL DOLLARS                                 
ACCTABL  EQU   *-ACCTABD           LENGTH OF TABLE ENTRY                        
*                                                                               
*                   ESTIMATE TABLE DSECT                                        
*                                                                               
ETBENTRY DSECT                                                                  
ETBEST   DS    XL1                 (KEY) - ESTIMATE                             
ETBQPRD  DS    XL3                 (KEY) - EBCDIC PRODUCT                       
ETBDPT   DS    XL1                 (KEY) - EBCDIC DAYPART                       
ETBPERDS DS    XL2                 START DATE FOR ESTIMATE COMPRESSED           
ETBPERDE DS    XL2                 END DATE FOR ESTIMATE COMPRESSED             
ETBOWROT DS    XL1                 OUT OF WEEK ROTATOR                          
ETBDPTNM DS    XL1                 DAYPART NUMBER                               
ETDAILY  DS    CL1                 DAILY ESTIMATE (Y/N)                         
ETBRECL  EQU   *-ETBENTRY          LENGTH OF TABLE ENTRY                        
ETBKEYL  EQU   ETBDPT+L'ETBDPT-ETBENTRY   LENGTH OF KEY FOR TABLE               
         EJECT                                                                  
LENGUIDE EQU   53*4+1              LENGTH OF ONE CPP GUIDE                      
LENGUIDS EQU   15*LENGUIDE         ROOM FOR CPP GUIDES                          
LENFREE  EQU   LENGUIDS            SPARE MEMORY NEEDED FOR OVERLAY              
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE SPPWBLOCK                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE FASECRETD                                                      
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FATWA                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024CTMAD05   09/11/03'                                      
         END                                                                    
