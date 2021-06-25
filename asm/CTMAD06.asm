*          DATA SET CTMAD06    AT LEVEL 139 AS OF 05/06/03                      
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   
* THIS PROGRAM IS DECEASED. CURRENT LIVE SOURCE IS CTMAD26                      
*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                   
*PHASE TA0C06A,*                                                                
*INCLUDE RECUP                                                                  
         TITLE 'TA0C06 - $MAD UPLOAD BUYLINES'                                  
TA0C06   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LENFREE,TA0C06,RA,RR=R2                                          
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
*        INITIALIZE OVERLAY WIDE REGISTERS                                      
*                                                                               
         LR    RF,RC               RF = A(OVERLAY'S SPARE MEMORY)               
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
         DROP  R7                                                               
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         ST    RC,ACONTROL         SAVE A(CONTROLLER STORAGE)                   
         ST    RF,ABLNTBL          SAVE A(BUYLINE TABLE)                        
         ST    R2,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         L     RE,=V(RECUP)                                                     
         AR    RE,R2                                                            
         ST    RE,RECUP                                                         
*                                                                               
         L     RE,ACOMFACS                                                      
         USING COMFACSD,RE                                                      
         MVC   PERVERT,CPERVERT                                                 
         DROP  RE                                                               
*                                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QDAYUNPK                                                  
         GOTO1 CALLOV,DMCB,0                                                    
         MVC   DAYUNPK,DMCB                                                     
*                                                                               
         MVI   SPACES,C' '                                                      
         MVC   SPACES+1(L'SPACES-1),SPACES                                      
*                                                                               
         LA    R0,AXTRAN           SET EXTENSION ROUTINES                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         L     R1,=A(EXTRA)                                                     
         AR    R1,R2                                                            
         ST    R1,AXTRA(RE)                                                     
         STC   RF,AXTRA(RE)                                                     
         LA    RF,1(RF)                                                         
         LA    RE,4(RE)                                                         
         BCT   R0,*-16                                                          
         EJECT                                                                  
***********************************************************************         
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
***********************************************************************         
MAIN     DS    0H                                                               
         BRAS  RE,INIT             INITIALIZE OVERLAY                           
         BNE   EXIT2                                                            
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
M20      B     EXIT2               ELSE EXIT (SKIP WORKER FILE CLOSE)           
*                                                                               
MX       B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE PROCESSES THE START MODE.  IT GETS THE HEADER OBJECT             
* AND THEN PROCEEDS TO GET THE SCHEDULE OBJECTS.                                
***********************************************************************         
PROCSTRT NTR1                                                                   
         GOTO1 WRKCRE,DMCB,0       OPEN WORKER FILE FOR CREATION                
*                                                                               
         BAS   RE,GETHEADR         GET THE HEADER OBJECT                        
*                                                                               
         BAS   RE,PROCALL          DO THIS EVERY TRANSACTION                    
*                                                                               
PSX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE CONTINUATION MODE.  IT CONTINUES TO GET            
* ALL THE SCHEDULES UNTIL AN END-OF-DATA OBJECT OCCURS.                         
***********************************************************************         
PROCMID  NTR1                                                                   
         CLI   ENDSCHED,C'Y'       IF END OF SCHEDULES                          
         BNE   PM10                                                             
         GOTO1 WRKREGT             REOPEN WORKER FILE FOR GETS                  
         B     PM20                                                             
*                                                                               
PM10     GOTO1 WRKREPT             ELSE REOPEN WORKER FILE FOR PUTS             
*                                                                               
PM20     BAS   RE,PROCALL          DO THIS EVERY TRANSACTION                    
*                                                                               
PMX      B     XIT                                                              
*                                                                               
***********************************************************************         
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
***********************************************************************         
PROCEND  NTR1                                                                   
PEX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE HEADER OBJECT FROM THE INPUT FRAME.                     
***********************************************************************         
PROCALL  NTR1                                                                   
         CLI   ENDSCHED,C'Y'       IF NOT END OF SCHEDULES                      
         BE    PA10                                                             
*                                                                               
         BAS   RE,GETSCHED         THEN GET THE SCHEDULE OBJECTS                
*                                                                               
         CLI   ENDSCHED,C'Y'       IF END OF SCHEDULES                          
         BNE   PAX                                                              
         GOTO1 WRKCLOS             THEN CLOSE WORKER FILE                       
         GOTO1 WRKLOC              REOPEN WORKER FILE FOR GETS                  
         MVI   EOTFLAG,C'N'        INITIALIZE END OF TEMP FILE FLAG             
         GOTO1 WRKGET,DMCB,AFREE   GET AND THROW AWAY MAD HEADER OBJ            
*                                                                               
PA10     BAS   RE,CONFIRM          SEND OUT CONFIRMATIONS                       
*                                                                               
         CLI   MDLAST,C'Y'         IF LAST TRANSACTION                          
         BNE   PAX                                                              
         CLI   HDRGENTA,C'Y'       TEST TO GENERATE T/A REQUESTS                
         BNE   PAX                                                              
         GOTO1 AADDREQ                                                          
*                                                                               
PAX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE HEADER OBJECT FROM THE INPUT FRAME.                     
***********************************************************************         
GETHEADR NTR1                                                                   
*                                  CLEAR ANY OLD INFORMATION                    
         XC    HDROBJCT(HDRLEN),HDROBJCT                                        
         XC    NUMBKEYS,NUMBKEYS                                                
         MVI   NUMSCHED,0                                                       
         MVI   NUMERRS,0           ZERO SLINE ERROR COUNT                       
         LA    RE,ERRTAB           CLEAR SLINE ERROR TABLE                      
         LA    RF,MAXERRS*L'ERRTAB                                              
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   MDLAST,C'N'                                                      
         MVI   BITFLAG,0                                                        
         MVI   ENDSCHED,C'N'       NOT END OF SCHEDULES                         
         MVI   CONFLAG,C'N'                                                     
*                                  GET AN OBJECT                                
         GOTOR GETITEML                                                         
*                                  ERROR IF NOT A HEADER OBJECT                 
         CLC   TYPENUM,=A(ITUPLBUY)                                             
         BNE   INVLHDR                                                          
*                                                                               
         L     R4,ADATA            COPY THE OBJECT TO HEADER OBJECT IN          
         L     R1,DATALEN              LOCAL STORAGE                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HDROBJCT(0),0(R4)                                                
*                                                                               
         BAS   RE,VALHEADR         VALIDATE THE HEADER OBJECT                   
*                                                                               
         GOTOR GETDAT              GET SCHEDULE DATES                           
*                                                                               
GHDR     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MAKES SURE THE HEADER OBJECT IS VALID.                           
***********************************************************************         
VALHEADR NTR1                                                                   
*                                                                               
         GOTO1 VALIMED,DMCB,HDRMED VALIDATE THE MEDIA (PAN=CTMADUS)             
         BNE   INVLMED                                                          
* EXTRACT FLAG1 FROM AGYHDR                                                     
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   AFLAG1,AGYFLAG1                                                  
         DROP  R6                                                               
*                                                                               
         GOTO1 VALICLT,DMCB,HDRCLT VALIDATE THE CLIENT                          
         BNE   INVLCLT                                                          
*                                                                               
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         MVC   SVRFPGRP,CRFPGRP                                                 
         DROP  R6                                                               
*                                                                               
         MVI   BINPBPRD,0          CLEAR PIGGYBACK CODE FIRST                   
         OC    HDRPR1,=C'   '                                                   
         OC    HDRPPB,=C'   '                                                   
*                                                                               
         CLC   =C'   ',HDRPPB      IF NO PRODUCT SPECIFIED                      
         BE    VH02                                                             
*                                                                               
         GOTO1 VALIPRD,DMCB,HDRPPB VALIDATE THE PIGGYBACK PRODUCT               
         BNE   INVLPRD                                                          
*                                                                               
         MVC   BINPBPRD,BPRD       SAVE PIGGY BACK BINARY CODE                  
         CLC   =C'POL',HDRPPB      POL CANNOT BE USED IN PIGGBACK               
         BE    INVLPIG                                                          
         CLC   =C'POL',HDRPR1                                                   
         BE    INVLPIG                                                          
         CLC   HDRPR1,HDRPPB       PIGGYBACK PRODUCTS MUST BE DIFFERENT         
         BE    INVLPRD                                                          
*                                                                               
VH02     GOTO1 VALIPRD,DMCB,HDRPR1 VALIDATE THE PRODUCT                         
         BNE   INVLPRD                                                          
*                                                                               
         MVC   KEYPRD,BPRD         PRD IN KEY WILL BE SAME IF NOT POOL          
*                                                                               
VH04     GOTO1 VALIEST,DMCB,HDREST GET BRAND ESTIMATE RECORD                    
         MVC   SVECOST2,ESTCOST2                                                
*                                                                               
         MVC   QPRD,=C'POL'        IF POL ESTIMATE DOESN'T EXIST                
         GOTO1 VALIEST,DMCB,HDREST                   (PAN=CTMADUS)              
         BE    VH10                                                             
*                                  THEN IF THE RECORD ISN'T THERE               
         CLC   APPLERR,=Y(ERA1EST)                                              
         BE    VH20                     THEN CHECK FOR BRAND ESTIMATE           
         B     INVLEST                  ELSE SOME OTHER ERROR, EXIT             
*                                                                               
VH10     OI    BITFLAG,X'80'       ELSE FLAG THAT POL ESTIMATE EXISTS           
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         MVC   SVEFLAG1,EFLAG1                                                  
         DROP  R6                                                               
*                                                                               
         MVI   KEYPRD,X'FF'        POL PRODUCT CODE TO BE IN THE KEY            
         CLI   CPROFILE,C'0'       TEST TRUE POOL                               
         BE    VH12                YES                                          
         CLI   APROFILE+11,C'Y'    NO-TEST BRAND POOL NPW                       
         BNE   VH12                                                             
         OI    BITFLAG,X'40'       YES-SET INDICATOR                            
*                                                                               
VH12     CLI   BPRD,X'FF'          TEST FOR PRODUCT=POL                         
         BNE   VH15                NO                                           
         CLI   CPROFILE,C'0'       TEST TRUE POOL                               
         BE    VH15                YES-OK TO USE POL(UNALLOCATED)               
         B     INVLPOL             NO-CANNOT USE IT FOR BRAND POOL              
*                                                                               
VH15     B     VH25                                                             
*                                                                               
VH20     MVC   QPRD,HDRPRD         IF BRAND ESTIMATE DOESN'T EXISTS             
         GOTO1 VALIEST,DMCB,HDREST                                              
         BNE   INVLEST             THEN EXIT                                    
         XC    APPLERR,APPLERR     RESET IT--NOTE BRAND BUYING                  
*                                                                               
VH25     L     RE,AIO                                                           
         USING ESTHDRD,RE                                                       
         MVC   ESTRATE,ERATE                                                    
         MVI   PWFLAG,0                                                         
         OC    EPWPCT,EPWPCT                                                    
         BZ    *+8                                                              
         OI    PWFLAG,X'01'        SET PW EST FLAG                              
         DROP  RE                                                               
*                                                                               
VH30     CLI   HDRSERV,C'A'       TEST FOR VALID RATING SERVICES                
         BE    *+12                                                             
         CLI   HDRSERV,C'N'                                                     
         BNE   INVLRSRV                                                         
*                                                                               
         GOTO1 HEXIN,DMCB,HDRBOOKS,BOOKS,L'HDRBOOKS                             
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLBOOK                                                         
*                                                                               
         LA    R4,BOOKS            CONVERT ANY AUG BOOKS TO JUL BOOKS           
         LA    R0,L'BOOKS/2                                                     
VH31     OC    0(2,R4),0(R4)                                                    
         BZ    VH35                                                             
         CLI   1(R4),8             DID THEY SEND US AN AUG BOOK?                
         BNE   *+8                                                              
         MVI   1(R4),7             YES, HARD CODE THAT TO A JUL BOOK            
* CONVERT BOOK TO Y2K FORMAT                                                    
         MVC   FULL(2),0(R4)                                                    
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(3,FULL),DUB   CONV TO YYMMDD                        
         GOTO1 (RF),(R1),DUB,(3,FULL)     AND THEN BACK TO 3 BYTE               
         MVC   0(2,R4),FULL                                                     
         LA    R4,2(R4)                                                         
         BCT   R0,VH31                                                          
*                                                                               
* CONVERT ALL HEADER DATES TO Y2K FORMAT                                        
VH35     GOTO1 DATCON,DMCB,HDRSDATE,(3,DUB)                                     
         GOTO1 (RF),(R1),(3,DUB),HDRSDATE                                       
         GOTO1 DATCON,DMCB,HDREDATE,(3,DUB)                                     
         GOTO1 (RF),(R1),(3,DUB),HDREDATE                                       
         GOTO1 DATCON,DMCB,HDRPREND,(3,DUB)                                     
         GOTO1 (RF),(R1),(3,DUB),HDRPREND                                       
*                                                                               
         BAS   RE,VSTARTDT         VALIDATE BUY/TRANSFER START DATE             
*                                                                               
*                                  IF # OF WEEKS NOT VALID HEX                  
VALINMWK GOTO1 HEXIN,DMCB,HDRWKS,BNUMWKS,L'HDRWKS                               
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLWKS             THEN ERROR                                   
*                                                                               
         CLI   BNUMWKS,0           ALSO ERROR IF NO WEEKS                       
         BE    INVLWKS                                                          
*                                                                               
         CLI   BNUMWKS,53          ALSO ERROR IF GREATER THAN 53                
         BH    INVLWKS                                                          
*                                                                               
         BAS   RE,VDATACAT         VALIDATE DATA CATEGORIES                     
         B     XIT                 DONE VALIDATING THE HEADER OBJECT            
         EJECT                                                                  
***********************************************************************         
* VALIDATES THE BUY/TRANSFER START DATE.                                        
*                                                                               
* RETURNS:     BDAY1               START DATE'S DAY NUMBER                      
***********************************************************************         
VSTARTDT NTR1                                                                   
         CLC   HDRSDATE,ESTSTRT    IF BUY START DATE < ESTIMATE'S START         
         BL    INVLSTDT            THEN ERROR                                   
*                                                                               
         CLC   HDRSDATE,ESTEND     IF BUY START DATE > ESTIMATE'S END           
         BH    INVLSTDT            THEN ERROR                                   
*                                                                               
         CLC   HDREDATE,ESTEND     TEST SCHED END PAST ESTIMATE END             
         BH    INVLENDT                                                         
*                                  CALCULATE DAY NUMBER FOR BUY START           
         GOTO1 GETDAY,DMCB,HDRSDATE,WORK                                        
*                                                                               
VSD1     MVC   BDAY1,0(R1)         SAVE BUY START DAY                           
VSDX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATES THE DATA CATEGORIES.                                                
*                                                                               
* RETURNS:     NUMDCATS            NUMBER OF DEMO CATEGORIES                    
***********************************************************************         
VDATACAT NTR1                                                                   
         SR    R2,R2               R2 = NUMBER OF VALID DEMO TYPES              
         LA    R3,HDRDTYPS         R3 = A(1ST ENTRY IN DEMO TYPE LIST)          
         LA    R4,DTYPLIST         R4 = A(1ST ENTRY IN BINARY LIST)             
*                                                                               
VDCLP    CLC   =C'FF',0(R3)        IF END OF DEMO TYPE LIST                     
         BE    VDC10               THEN DONE WITH DEMO TYPES                    
*                                                                               
*                                  CONVERT DEMO TYPE & PUT IN OUR LIST          
         GOTO1 HEXIN,DMCB,0(R3),0(R4),L'HDRDTYPS                                
*                                                                               
         OC    12(4,R1),12(R1)     IF NOT VALID HEX                             
         BZ    INVLDTYP            THEN ERROR                                   
*                                                                               
         LA    R3,L'HDRDTYPS(R3)   R3 = A(NEXT ENTRY IN DEMO TYPE LIST)         
         LA    R4,L'DTYPLIST(R4)   R4 = A(NEXT ENTRY IN BINARY LIST)            
         LA    R2,1(R2)            INCREMENT NUMBER OF VALID DEMO TYPES         
*                                                                               
         B     VDCLP               LOOP BACK                                    
*                                                                               
VDC10    STC   R2,NUMDCATS         STORE # VALID DEMO CATEGORIES                
*                                                                               
         LA    R3,2(R3)            BUMP PAST C'FF'                              
         MVI   DAILY,0                                                          
         CLI   0(R3),C'Y'          TEST DAILY SCHEDULING FLAG                   
         BNE   *+8                                                              
         MVI   DAILY,C'Y'          YES-SET IT                                   
         LA    R3,1(R3)                                                         
         CLI   0(R3),0                                                          
         BE    VDCX                TEST FOR MORE DATA                           
*                                                                               
VDC15    MVC   RATE,0(R3)          RATE FOLLOWS DAILY FLAG                      
         CLI   RATE,C'0'                                                        
         BL    INVLRATE                                                         
         CLI   RATE,C'8'                                                        
         BH    INVLRATE                                                         
*                                                                               
         LA    R3,1(R3)                                                         
         GOTO1 HEXIN,DMCB,0(R3),REP,4                                           
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLREP                                                          
         CLC   REP,=H'999'                                                      
         BH    INVLREP                                                          
*                                                                               
VDC20    OC    REP,REP             TEST FOR A SPECIAL REP                       
         BZ    VDCX                NO                                           
*                                                                               
         MVI   KEY,C'0'            YES-VALIDATE IT                              
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+5(2),SIGNON2C                                                
         SR    R0,R0                                                            
         ICM   R0,3,REP                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(3),DUB                                                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     RE,AIO                                                           
         CLC   KEY(15),0(RE)       TEST IF REP FOUND                            
         BNE   INVLREP             BAD REP                                      
*                                                                               
VDCX     B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* SEVERE ERROR EXITS, MOSTLY HEADER OBJECT ERRORS, WHICH CAUSE THE              
* UPLOAD TO ABORT                                                               
***********************************************************************         
         SPACE 1                                                                
*                                  NEED A HEADER BEFORE ANYTHING                
INVLHDR  MVI   BYTE,1                                                           
         B     ABORT                                                            
*                                  INVALID NUMBER OF WEEKS                      
INVLWKS  MVI   BYTE,2                                                           
         B     ABORT                                                            
*                                  INVALID SCHEDULE SEQUENCE NUMBER             
INVLSSEQ MVI   BYTE,3                                                           
         B     ABORT                                                            
*                                  START DATE NOT WITHIN ESTIMATE RANGE         
INVLSTDT MVI   BYTE,4                                                           
         B     ABORT                                                            
*                                  INVALID OBJECT                               
INVLOBJ  MVI   BYTE,5                                                           
         B     ABORT                                                            
*                                  CAN'T BUILD WITHOUT SCHEDULES                
INVLNSCH MVI   BYTE,6                                                           
         B     ABORT                                                            
*                                  NEED AT LEAST ONE VALID DEMO TYPE            
INVLDTYP MVI   BYTE,7                                                           
         B     ABORT                                                            
*                                  NOT ENOUGH AVAILABLE BUYLINES                
INVLFULL MVI   BYTE,8                                                           
         MVI   UNWIND,C'Y'         SET FLAG TO UNWIND TRANSACTIONS              
         B     ABORT                                                            
*                                                                               
INVLBOOK DS    0H                  INVALID BOOK                                 
         MVI   BYTE,9                                                           
         B     ABORT                                                            
*                                                                               
INVLRSRV DS    0H                  INVALID RATING SERVICE                       
         MVI   BYTE,10                                                          
         B     ABORT                                                            
*                                                                               
INVLENDT MVI   BYTE,11             SCHED END PAST ESTIMATE END                  
         B     ABORT                                                            
*                                                                               
INVLMED  MVI   BYTE,12             INVALID MEDIA CODE                           
         B     ABORT                                                            
*                                                                               
INVLERRT MVI   BYTE,13             TOO MANY SLINE ERRORS                        
         B     ABORT                                                            
*                                                                               
INVLCLT  MVI   BYTE,14             INVALID CLIENT CODE                          
         B     ABORT                                                            
*                                                                               
INVLPRD  MVI   BYTE,15             INVALID PRODUCT CODE                         
         B     ABORT                                                            
*                                                                               
INVLEST  MVI   BYTE,16             INVALID ESTIMATE CODE                        
         B     ABORT                                                            
*                                                                               
INVLPOL  MVI   BYTE,17             PRODUCT POL ONLY FOR TRUE POOL               
         B     ABORT                                                            
*                                                                               
INVLPIG  MVI   BYTE,18             POL IS NOT A VALID PIGGYBACK PRODUCT         
         B     ABORT                                                            
*                                                                               
INVLRATE MVI   BYTE,19             INVALID RATE TYPE                            
         B     ABORT                                                            
*                                                                               
INVLREP  MVI   BYTE,20             INVALID SPECIAL REP                          
         B     ABORT                                                            
*                                                                               
INVLMKT  MVI   BYTE,21             INVALID MARKET                               
         B     ABORT                                                            
*                                                                               
INVLPW   MVI   BYTE,22             PW RECORD LOCKED                             
         B     ABORT                                                            
*                                                                               
INVLDAYS MVI   BYTE,23             ANOTHER STELLAR PERFORMANCE BY               
         B     ABORT               THOSE WONDERFUL FOLKS AT STRATA              
*                                  PUT ABORT, EOD ITEMS, SET MDLAST=Y           
ABORT    GOTO1 HEXOUT,DMCB,BYTE,HALF,1                                          
         GOTO1 PUTITEM,DMCB,ITUPLABT,2,HALF                                     
         BNE   EXIT                                                             
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
         MVI   MDLAST,C'Y'                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE OBJECTS AND PROCESSES THEM ACCORDINGLY.                 
***********************************************************************         
GETSCHED NTR1                                                                   
*                                  GET AN OBJECT                                
GSCHLOOP GOTOR GETITEML                                                         
*                                                                               
         CLI   EIFFLAG,C'Y'        IF END OF FRAME                              
         BNE   GSCH10                                                           
*                                                                               
         TM    BITFLAG,X'01'       THEN IF PREVIOUS OBJ WAS END-OF-DATA         
         BO    GSCHDONE            THEN WE'RE DONE                              
*                                                                               
         B     GSCHX               ELSE WE NEED MORE OBJECTS                    
*                                                                               
GSCH10   TM    BITFLAG,X'01'       ERROR IF END-OF-DATA WAS PREV OBJECT         
         BO    INVLOBJ                                                          
*                                                                               
         CLC   TYPENUM,=A(ITEOD)   IF OBJECT IS END-OF-DATA                     
         BNE   GSCH20                                                           
         OI    BITFLAG,X'01'       THEN SET BIT IN OBJECT FLAG                  
*                                                                               
         TM    BITFLAG,X'02'       ERROR IF NO SCHEDULES READ                   
         BZ    INVLNSCH                                                         
*                                                                               
         B     GSCHLOOP            LOOP BACK, CAN'T HAVE ANY MORE DATA          
*                                                                               
*                                  IF NOT A SCHEDULE OBJECT                     
GSCH20   CLC   TYPENUM,=A(ITUPLBYS)                                             
         BNE   INVLOBJ             THEN ERROR, NO MORE OBJECTS                  
*                                                                               
         OI    BITFLAG,X'02'       SET AT LEAST 1 SLINE PROCESSED               
         LA    RE,SCHOBJCT         CLEAR SCHEDULE OBJECT                        
         LA    RF,SCHLEN                                                        
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,VALISCHD         VALIDATE & ADD SCHEDULE TO TEMPSTR           
*                                                                               
         B     GSCHLOOP            LOOP BACK TO GET NEXT OBJECT                 
*                                                                               
GSCHDONE MVI   ENDSCHED,C'Y'       NO MORE SCHEDULES TO READ                    
         NI    BITFLAG,X'FF'-X'03' CLEAR BITS FOR LATER USE                     
*                                                                               
GSCHX    B     XIT                 RETURN TO CALLER                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE MAKES SURE THE SCHEDULE OBJECT IS VALID AND ADDS ITS             
* BINARY EQUIVALENT TO TEMPSTR.                                                 
***********************************************************************         
VALISCHD NTR1                                                                   
         L     R2,ADATA            R2 = A(SCHEDULE OBJECT)                      
         USING SLD,R2                                                           
         CLI   HDRRETR,C'Y'        TEST OVERALL RETRANSFER                      
         BNE   *+8                 NO                                           
         OI    SCHINDS,SCHIRET     YES-SET SLINE RETRANSFER FLAG                
*                                                                               
*                                  STORE SCHEDULE LINE SEQUENCE NUMBER          
         GOTO1 HEXIN,DMCB,SLSEQ,SCHLNNUM,L'SLSEQ                                
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLSSEQ                                                         
*                                                                               
* VALIDATE ORIGINAL TRANSFER DATE/TIME - IF BOTH ARE ZERO, ITS                  
* AN ORIGINAL TRANSFER EVEN IF HEADER RETRANSFER FLAG IS YES                    
*                                                                               
         GOTO1 DECIN,DMCB,SLODATE,L'SCHODATE                                    
         BNE   INVLODTE                                                         
         MVC   SCHODATE,SLODATE    ORIGINAL TRANSFER DATE                       
         OC    0(4,R1),0(R1)       TEST FOR ZEROES                              
         BNZ   *+10                                                             
         XC    SCHODATE,SCHODATE   YES-FORCE ORIG XFR ON SLINE                  
*                                                                               
         GOTO1 HEXIN,DMCB,SLOTIME,SCHOTIME,L'SLOTIME                            
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLOTIM                                                         
*                                                                               
VSCH02   OC    SCHODATE(L'SCHODATE+L'SCHOTIME),SCHODATE                         
         BNZ   VSCH04                                                           
         NI    SCHINDS,FF-SCHIRET  IF ZERO-TURN OFF RETRANSFER FLAG             
*                                                                               
*                                                                               
VSCH04   GOTO1 HEXIN,DMCB,SLDAYTIM,SCHDT,L'SLDAYTIM                             
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLDATM                                                         
*                                                                               
         BAS   RE,GETDT            SET THE DAY AND START/END TIMES              
         TM    SCHDAY,X'7F'        THIS MUST NOT HAPPEN                         
         BZ    INVLDAYS            MUST HAVE AT LEAST ONE BIT ON                
*                                                                               
         MVC   SCHSTA,SLSTAT                                                    
         GOTO1 VALISTA,DMCB,SCHSTA VALIDATE THE STATION                         
         BNE   INVLSTA                                                          
         L     RE,AIO                                                           
         USING STAREC,RE                                                        
         MVC   PWMKT,SMKT                   CURRENT MARKET                      
         MVC   SVOLDMKT(2),STOLDMK1         PREVIOUS MARKETS                    
         MVC   SVOLDMKT+2(2),STOLDMK2                                           
         DROP  RE                                                               
*&&DO                                                                           
         GOTOR GETMKT               MAKE SURE IT'S VALID                        
         BNE   INVLMKT                                                          
         L     RE,AIO                                                           
         USING MKTREC,RE                                                        
         MVC   ALPHMRKT,MKTALST    SAVE 1ST ALPHA MARKET FROM LIST              
         DROP  RE                                                               
*&&                                                                             
         TM    PWFLAG,X'01'        TEST WI PW ESTIMATE                          
         BNO   VSCH05                                                           
         GOTOR CHKMKT              GET CORRECT MKT FROM PW RECS                 
         GOTOR GETMKT              MAKE SURE IT'S VALID                         
         BNE   INVLMKT                                                          
*&&DO                                                                           
         L     RE,AIO                                                           
         USING MKTREC,RE                                                        
         MVC   ALPHMRKT,MKTALST    SAVE 1ST ALPHA MARKET FROM LIST              
         DROP  RE                                                               
*&&                                                                             
         GOTOR GETPW                 MAKE SURE IT'S NOT LOCKED                  
         BNE   INVLPW                                                           
         PACK  DUB,PWMKT                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,BMKTSTA        FORCE CORRECT MARKET IN BUY KEY              
*&&DO                                                                           
VSCH05   GOTOR CHKDMBKS                                                         
         BNE   INVLSTBK                                                         
*&&                                                                             
VSCH05   GOTO1 VALIDPT,DMCB,SLDAYPRT                                            
         BNE   INVLDPT                                                          
         MVC   SCHDYPRT,SLDAYPRT   STORE THE DAYPART CODE                       
*                                                                               
         GOTO1 VALISLN,DMCB,SLLENGTH                                            
         BNE   INVLSPLN                                                         
         MVC   SCHTOTLN,BSPOTLEN                                                
*                                                                               
         GOTO1 VALISLN,DMCB,SLMASLEN                                            
         BNE   INVLSPLN                                                         
         MVC   SCHMASLN,BSPOTLEN   MASTER PRODUCT SPOT LENGTH                   
*                                                                               
         CLI   BINPBPRD,0          TEST FOR PIGGYBACK                           
         BE    *+14                NO                                           
         CLC   SCHTOTLN,SCHMASLN   YES-PREVENT ALLOCATING ALL TIME              
         BE    INVLSPLN            TO MASTER                                    
*                                                                               
         GOTO1 HEXIN,DMCB,SLCOST,SCHCOST,L'SLCOST                               
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLCOST                                                         
*                                                                               
         MVC   SCHPROG,SLPROG                                                   
         OC    SCHPROG,SPACES                                                   
*                                                                               
         GOTO1 HEXIN,DMCB,SLOVERS,SCHOVERS,L'SLOVERS                            
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLOVER                                                         
*                                                                               
         LA    R5,SLDATA           STORE SPOTS PER WEEK                         
         LA    R3,SCHDATA          R3 = A(1ST ENTRY FOR SPOTS PER WEEK)         
         ZIC   R4,BNUMWKS                                                       
         SLL   R4,1                                                             
         GOTO1 HEXIN,DMCB,0(R5),0(R3),(R4)                                      
         OC    12(4,R1),12(R1)                                                  
         BZ    INVLSPOT                                                         
         TM    BITFLAG,X'40'       TEST BRAND POOL NPW                          
         BZ    VSCH06              NO                                           
*                                                                               
         ZIC   R0,BNUMWKS          CHECK NO MORE THAN 31 SPOTS IN EACH          
         LA    RE,SCHSPOTS         WEEK                                         
         CLI   0(RE),63            MAX OF 6 BITS TO HOLD SPOTS                  
         BH    INVLNUMS                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,*-12                                                          
*                                                                               
VSCH06   AR    R5,R4               STORE DEMO VALUES                            
         ZIC   R4,BNUMWKS          R3 = A(1ST ENTRY FOR DEMO VALUES)            
         AR    R3,R4                                                            
         SR    R4,R4                                                            
         CLI   NUMDCATS,0          IF THERE ARE DEMO CATEGORIES                 
         BE    VSCH10                                                           
         IC    R4,NUMDCATS         R4 = NUMBER OF BYTES TO CONVERT              
         SLL   R4,2                X 4 FOR LENGTH OF DEMO VALUE                 
         GOTO1 HEXIN,DMCB,0(R5),0(R3),(R4)                                      
         OC    12(4,R1),12(R1)     ERROR IF INVALID HEX                         
         BZ    INVLDEMS                                                         
*                                                                               
VSCH10   AR    R5,R4               BUMP PAST DEMO VALUES                        
         SRL   R4,1                R3 = A(AFTER ALL DATA)                       
         AR    R3,R4                                                            
*                                                                               
VSCH20   TM    SCHINDS,SCHIRET     TEST FOR RETRANSFER                          
         BZ    VSCH30              NO                                           
*                                                                               
* VALIDATE PREVIOUSLY TRANSFERRRED SPOTS ARRAY                                  
*                                                                               
         ZIC   R4,BPRVWKS                                                       
         SLL   R4,1                X 2 FOR LENGTH OF WEEK                       
         GOTO1 HEXIN,DMCB,0(R5),0(R3),0(R4)                                     
         OC    12(4,R1),12(R1)     TEST FOR VALID HEX                           
         BZ    INVLTRAN                                                         
*                                                                               
         AR    R5,R4               POINT TO NEXT DATA AREA                      
         SRL   R4,1                                                             
         AR    R3,R4               BUMP PAST TRANSFERRED SPOTS ARRAY            
*                                                                               
* LOOK FOR OPTIONAL COMMENT AT END OF OBJECT                                    
*                                                                               
VSCH30   LR    R4,R5               R4=A(END OF OBJECT)                          
         S     R4,ADATA            COMPUTE LENGTH PROCESSED SO FAR              
         C     R4,DATALEN          TEST FOR MORE DATA TO PROCESS                
         BNL   VSCHSV              NO                                           
*                                                                               
         LNR   R4,R4               MAKE LENGTH PROCESSED NEGATIVE               
         A     R4,DATALEN          COMPUTE LENGTH REMAINING                     
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R5)       MOVE COMMENT TO OBJECT                       
*                                                                               
         LR    RE,R3               RE=START OF COMMENT                          
         LA    RF,SCHOBJCT                                                      
         SR    RE,RF               COMPUTE DISPLACEMENT TO COMMENT              
         STCM  RE,3,SCHCOMDS       AND SAVE IT                                  
         LA    R4,1(R4)            RESTORE COMMENT LENGTH                       
         LA    R5,0(R4,R5)         POINT PAST COMMENT                           
         LA    R3,0(R4,R3)                                                      
         STC   R4,SCHCOMLN         SAVE COMMENT LENGTH                          
*                                                                               
VSCHSV   LA    RE,SCHOBJCT         SAVE SCHEDULE FOR LATER PROCESSING           
         SR    R3,RE                                                            
         XC    APPLERR,APPLERR                                                  
         CLI   SCHERROR,0          IF ERROR OCCURED                             
         BE    *+8                                                              
         BAS   RE,ADDERR           ADD ERROR TO TABLE                           
         GOTOR PUTTMPL,DMCB,SCHOBJCT,(R3)                                       
         BNE   EXIT                                                             
*                                                                               
         ZIC   R1,NUMSCHED         INCREMENT NUMBER OF SCHEDULES READ           
         LA    R1,1(R1)                                                         
         STC   R1,NUMSCHED                                                      
*                                                                               
VSX      B     XIT                 DONE VALIDATING THE SCHEDULE OBJECT          
         DROP  R2                                                               
*                                                                               
INVLODTE MVI   SCHERROR,1          INVALID ORIGINAL TRANSFER DATE               
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLOTIM MVI   SCHERROR,2          INVALID ORIGINAL TRANSFER TIME               
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLTIME MVI   SCHERROR,3          INVALID START/END TIMES                      
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLSPLN MVI   SCHERROR,4          INVALID SPOT LENGTH                          
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLCOST MVI   SCHERROR,5          INVALID COST                                 
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLSPOT MVI   SCHERROR,6          INVALID SPOTS                                
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLDEMS MVI   SCHERROR,7          INVALID DEMOS                                
         LA    R3,SCHODATE                                                      
         B     VSCHSV              SAVE THE ERROR                               
*                                                                               
INVLDATM MVI   SCHERROR,8          INVALID DAY/TIME BLOCK                       
         LA    R3,SCHODATE                                                      
         B     VSCHSV                                                           
*                                                                               
INVLDPT  MVI   SCHERROR,9          INVALID DAYPART                              
         LA    R3,SCHODATE                                                      
         B     VSCHSV                                                           
*                                                                               
INVLSTA  MVI   SCHERROR,10         INVALID STATION                              
         LA    R3,SCHODATE                                                      
         B     VSCHSV                                                           
*                                                                               
INVLTRAN MVI   SCHERROR,11         INVALID TRANSFERRED SPOTS ARRAY              
         LA    R3,SCHODATE                                                      
         B     VSCHSV                                                           
*                                                                               
INVLOVER MVI   SCHERROR,13         INVALID DEMO OVERRIDE BIT FIELD              
         LA    R3,SCHODATE                                                      
         B     VSCHSV                                                           
*                                                                               
INVLNUMS MVI   SCHERROR,14         MORE THAN 31 SPOTS IN A WEEK                 
         LA    R3,SCHODATE         FOR BRAND POL NOW                            
         B     VSCHSV                                                           
*&&DO                                                                           
INVLSTBK MVI   SCHERROR,15         ONE OF THE BOOKS DOESN'T EXIST FOR           
         LA    R3,SCHODATE            THIS STATION                              
         B     VSCHSV                                                           
*&&                                                                             
         SPACE 2                                                                
* SUB-ROUTINE TO ADD SLINE ERROR TO TABLE                                       
*                                                                               
ADDERR   ST    RE,FULL                                                          
         LA    RE,ERRTAB                                                        
         SR    R1,R1                                                            
         ICM   R1,1,NUMERRS        GET N'ERRORS                                 
         BZ    ADDERR4                                                          
         LR    R0,R1               COPY ERROR COUNT TO R0                       
*                                                                               
ADDERR2  CLC   SCHLNNUM,0(RE)      TEST IF SLINE NUMBER IS IN TABLE             
         BE    ADDERRX             YES-EXIT                                     
         LA    RE,L'ERRTAB(RE)                                                  
         BCT   R0,ADDERR2                                                       
*                                                                               
ADDERR4  LA    R1,1(R1)                                                         
         CH    R1,=Y(MAXERRS)      TEST FOR TABLE OVERFLOW                      
         BH    INVLERRT                                                         
         STC   R1,NUMERRS                                                       
*                                                                               
         MVC   0(4,RE),SCHLNNUM    SET SLINE NUMBER                             
         MVC   4(1,RE),SCHERROR    COPY ERROR NUMBER                            
*                                                                               
ADDERRX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE DERIVES THE SLINE DAY AND START/END TIMES FROM THE               
* DAYTIME BLOCK.  OUTPUTS ARE SCHDAY, SCHSTIME, SCHNTIME, SCHINDS               
***********************************************************************         
         SPACE 1                                                                
GETDT    NTR1  ,                                                                
         MVI   SCHDAY,0                                                         
         MVI   SCHDAYNM,0                                                       
         XC    SCHSTIME(4),SCHSTIME                                             
         LA    R2,SCHDT            R2=A(DAYTIME BLOCK)                          
         USING DTD,R2                                                           
         SR    R3,R3               R3=N'DAYTIME BLOCK ENTRIES                   
*                                                                               
GETDT2   CLI   DTDAY,0             TEST FOR EOB                                 
         BE    GETDT4              YES                                          
*                                                                               
         OC    SCHDAY,DTDAY        FORM CUMULATIVE DAY MASK                     
*                                                                               
         CLI   SCHDAYNM,0          TEST IF DAY NUMBERS HAVE BEEN SET            
         BNE   *+10                NO                                           
         MVC   SCHDAYNM,DTDAYNUM                                                
*                                                                               
         OC    SCHSTIME,SCHSTIME   TEST IF ANY START TIME SET                   
         BZ    *+14                NO SO SET IT                                 
         CLC   DTSTART,SCHSTIME    TEST FOR LOWEST START TIME                   
         BNL   *+10                NO                                           
         MVC   SCHSTIME,DTSTART                                                 
*                                                                               
         OC    SCHNTIME,SCHNTIME   TEST IF ANY END TIME SET                     
         BZ    *+14                NO                                           
         CLC   DTEND,SCHNTIME      TEST FOR HIGHEST END TIME                    
         BNH   *+10                NO                                           
         MVC   SCHNTIME,DTEND                                                   
*                                                                               
         LA    R3,1(R3)            INCREMENT ENTRY COUNT                        
         LA    R2,DTLNQ(R2)        NEXT DAYTIME BLOCK ENTRY                     
         B     GETDT2                                                           
*                                                                               
GETDT4   CH    R3,=H'1'            TEST TO SET ORBIT FLAG                       
         BNH   *+8                                                              
         OI    SCHINDS,SCHIORB     YES-MORE THAN ONE ENTRY                      
*                                                                               
GETDTX   B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CONFIRMS THE BUILDING OF THE BUY RECORDS USING THE               
* SCHEDULE OBJECTS BY SENDING OUT CONFIRMATION OBJECTS.                         
***********************************************************************         
CONFIRM  NTR1  ,                                                                
         CLI   CONFLAG,C'Y'        IF PENDING CONFIRM OBJECT PUTITEM            
         BE    CNF20               THEN PUT OUT CONFIRMATION OBJECT             
*                                                                               
*                                  GET A SCHEDULE OBJECT FROM TEMPFILE          
CNFLOOP  GOTOR GETTMPL,DMCB,SCHOBJCT                                            
*                                                                               
         CLI   EOTFLAG,C'Y'        DONE IF NO MORE SCHEDULES                    
         BNE   CNF10                                                            
         MVI   MDLAST,C'Y'         LAST OUTPUT DATA FRAME                       
         MVI   SETSENT,C'Y'        SET WORKER FILE TO SENT AFTER CLOSE          
*                                                                               
*                                  PUT END-OF-DATA OBJECT OUT                   
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
         B     CNFX                EXIT ROUTINE                                 
*                                                                               
*                                  CLEAR OLD CONFIRMATION OBJECT                
CNF10    XC    CNFOBJCT(CNFLENQ),CNFOBJCT                                       
*                                                                               
         GOTO1 HEXOUT,DMCB,SCHLNNUM,CNFLNNUM,L'SCHLNNUM                         
*                                                                               
         CLI   NUMERRS,0           TEST IF ANY SLINE ERRORS                     
         BE    *+8                 NO                                           
         BAS   RE,CHKERR           CHECK IF ANY ERRORS FOR THIS SLINE           
*                                                                               
         OC    SCHERROR,SCHERROR   IF THERE WAS AN ERROR                        
         BZ    CNF15               YES                                          
*                                                                               
         MVI   CNFLEN,L'CNFLNNUM+L'CNFERROR                                     
         GOTO1 HEXOUT,DMCB,SCHERROR,CNFERROR,L'SCHERROR                         
         B     CNF20               PUT OUT THE ERROR IN CONFIRMATION            
*                                                                               
CNF15    MVC   BSPOTLEN,SCHTOTLN   SPOT LENGTH IS TOTAL SPOT LENGTH             
*                                                                               
         GOTO1 ABLDFACT            BUILD X'98' ELEMENT W/ GETFACT               
*                                                                               
         MVI   CNFERROR,C'0'       NO ERROR                                     
         MVI   CNFERROR+1,C'0'                                                  
         GOTO1 HEXOUT,DMCB,SCHLNNUM,CNFLNNUM,L'SCHLNNUM                         
         MVI   CNFLEN,CNFLNQ1                                                   
*                                                                               
         BAS   RE,BLDBRECS         BUILD THE BUY RECORDS                        
*                                                                               
CNF20    L     R3,=A(ITUPLCNF)                                                  
         ZIC   R2,CNFLEN                                                        
         GOTO1 PUTITEM,DMCB,(R3),(R2),CNFOBJCT                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME FLAG SET                     
         BE    CNFX                THEN WE NEED MORE                            
*                                                                               
         B     CNFLOOP             LOOP BACK UNTIL NO MORE SCHEDULES            
*                                                                               
CNFX     MVI   CONFLAG,C'Y'        CONFIRM OBJECT PUTITEM PENDING               
         B     XIT                 RETURN TO THE CALLER                         
         SPACE 2                                                                
* SUB-ROUTINE TO CHECK FOR ANY ERRORS FOR IDENTICAL SLINE NUMBER                
* THIS DEALS WITH POSSIBILITY OF ERRORS WITH COMBOS                             
*                                                                               
* ON EXIT, SCHERROR IS SET                                                      
*                                                                               
CHKERR   ST    RE,FULL                                                          
         ZIC   R1,NUMERRS                                                       
         LA    RE,ERRTAB                                                        
*                                                                               
CHKERR2  CLC   SCHLNNUM,0(RE)                                                   
         BE    CHKERR4                                                          
         LA    RE,L'ERRTAB(RE)                                                  
         BCT   R1,CHKERR2                                                       
         B     CHKERRX                                                          
*                                                                               
CHKERR4  MVC   SCHERROR,4(RE)      SET ERROR NUMBER                             
*                                                                               
CHKERRX  L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE BUY RECORDS BASED ON THE INFORMATION FROM THE         
* SCHEDULE OBJECTS.                                                             
***********************************************************************         
         SPACE 1                                                                
BLDBRECS NTR1  ,                                                                
         GOTO1 VALISTA,DMCB,SCHSTA RE-READ THE STATION (PAN=CTMADUS)            
*                                                                               
         GOTO1 ABLDBLTB            BUILD THE BUYLINE TABLE                      
         XC    LNEWTOT,LNEWTOT     CLEAR NEW SPOTS/WEEK ARRAY                   
         MVI   LFLAG,0                                                          
*                                                                               
         TM    SCHINDS,SCHIRET     TEST RE-TRANSFER                             
         BO    BLDB05              YES                                          
*                                                                               
* ORIGINAL TRANSFER FOR SLINE                                                   
*                                                                               
BLDB02   XC    LSPOTS,LSPOTS       CLEAR SPOTS/WEEK ARRAY                       
         ZIC   R1,BNUMWKS          SET UP SPOTS/WK ARRAY                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LSPOTS(0),SCHSPOTS                                               
*                                                                               
BLDB04   GOTO1 ABLDWKS,LBADD                                                    
         BAS   RE,ADDBUY                                                        
         BE    BRCYES                                                           
         B     BRCNO                                                            
*                                                                               
* RE-TRANSFER FOR SLINE                                                         
*                                                                               
BLDB05   BAS   RE,REXFR                                                         
         BNE   BRCNO                                                            
*                                                                               
BRCYES   TM    LFLAG,LDELETE                                                    
         BO    BRCYES1                                                          
         GOTOR ADDSTA              UPDATE STATION LIST                          
*                                                                               
BRCYES1  MVI   BYTE,0              INDICATORS                                   
         TM    SCHINDS,SCHIRET     TEST RETRANSFER                              
         BZ    BRCYES2                                                          
         TM    LFLAG,LDELETE                                                    
         BZ    *+8                                                              
         OI    BYTE,CNFIDEL        SET BUY LINES DELETED FLAG                   
         TM    LFLAG,LFREEZE                                                    
         BZ    *+8                                                              
         OI    BYTE,CNFIFRZ        SET FREEZE INDICATOR                         
*                                                                               
BRCYES2  GOTO1 HEXOUT,DMCB,BYTE,CNFINDS,1                                       
         ZIC   R3,BNUMWKS          SEND BACK NEW SPOTS/WK ARRAY                 
         GOTO1 HEXOUT,DMCB,LNEWTOT,CNFSPOTS,(R3)                                
         ZIC   R1,CNFLEN                                                        
         SLL   R3,1                ADJUST CONFIRM OBJECT LENGTH                 
         AR    R1,R3                                                            
         STC   R1,CNFLEN                                                        
         B     YES                 RETURN 'YES' TO CALLER                       
*                                                                               
BRCNO    GOTO1 HEXOUT,DMCB,BYTE,CNFERROR,L'BYTE                                 
         MVI   CNFLEN,L'CNFLNNUM+L'CNFERROR                                     
         XC    APPLERR,APPLERR                                                  
         B     NO                  RETURN 'NO' TO CALLER                        
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE BUY LINE FROM THE BUYLINE TABLE.                        
*                                                                               
* FORMAT OF BUYLINE TABLE                                                       
* -----------------------                                                       
* BYTE 0  - 4 BYTES - SCHEDULE SEQUENCE NUMNBER                                 
* BYTE 4  - 3 BYTES - TRANSACTION DATE                                          
* BYTE 7  - 3 BYTES - TRANSACTION TIME                                          
* BYTE 10 - 1 BYTES - MASTER PRODUCT CODE                                       
* BYTE 11 - 1 BYTES - PIGGYBACK PRODUCT CODE                                    
*                                                                               
* RETURNS:     AENTRY              A(ENTRY IN BUYLINE TABLE)                    
*              NEWLINE             NEXT BUY LINE AVAILABLE                      
***********************************************************************         
         SPACE 1                                                                
GETBLINE NTR1  ,                                                                
         L     R2,ABLNTBL          R2 = A(BUYLINE TABLE)                        
         USING BLND,R2                                                          
         LA    R4,255              R4 = MAXIMUM NUMBER OF BUYLINES              
         LA    R3,1                R3 = CURRENT BUYLINE NUMBER                  
*                                                                               
*                                                                               
GTBLP    CLI   BLNMAS,0            IF THERE IS A PRODUCT IN BUYLINE             
         BE    GTB20                                                            
         LA    R2,BLNLNQ(R2)       THEN CHECK NEXT BUYLINE ENTRY                
         LA    R3,1(R3)                                                         
         BCT   R4,GTBLP            LOOP UNTIL WE GET ONE THAT DOESN'T           
         B     INVLFULL            ELSE ERROR                                   
*                                                                               
GTB20    STC   R3,NEWLINE          SAVE BUYLINE TO USE                          
         ST    R2,AENTRY           SAVE A(BUYLINE ENTRY)                        
*                                                                               
GTBX     XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD AND ADD BUY RECORD(S)                                         *         
* ASSUMES BLDWKS HAS BEEN CALLED BEFORE ADDBUY                        *         
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                    *         
***********************************************************************         
         SPACE 1                                                                
ADDBUY   NTR1  ,                                                                
         BAS   RE,GETBLINE                                                      
         GOTO1 ABLDBKEY                                                         
         GOTO1 AMOVKIO2                                                         
*                                                                               
         L     R2,AIO2             BUILD BUY RECORD                             
         USING BUYRECD,R2                                                       
         ST    R2,AIO                                                           
         XC    LBUYCOST,LBUYCOST                                                
         MVC   LBUYCOST+1(3),SCHCOST                                            
*                                                                               
         GOTO1 ABUYDESC,LBADD      ADD BUY DESCRIPTION ELEMENT                  
         BNE   NO                  EXIT RIGHT AWAY FOR ERROR                    
         GOTO1 ADDELEM,DMCB,DESCELEM                                            
*                                                                               
         GOTO1 ABLDDEMO                                                         
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
         CLI   SCHCOMLN,0          TEST FOR A COMMENT                           
         BE    ADDB0A                                                           
         GOTO1 ABLDCOM                                                          
         LA    R4,ELEMENT                                                       
*                                                                               
ADDB0    CLI   0(R4),0             ADD THE COMMENT ELEMENTS                     
         BE    ADDB0A                                                           
         GOTO1 ADDELEM,DMCB,(R4)                                                
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     ADDB0                                                            
*                                                                               
ADDB0A   DS    0H                                                               
         TM    BITFLAG,X'80'       TEST FOR NON-POOL                            
         BO    ADDB1                                                            
         CLI   BINPBPRD,0          AND PIGGYBACKS                               
         BE    ADDB1                                                            
*                                                                               
         GOTO1 ABLDPIGS                                                         
         GOTO1 ADDELEM,DMCB,PIGELEM                                             
*                                                                               
ADDB1    TM    SCHINDS,SCHIORB     TEST ORBIT                                   
         BZ    ADDB2                                                            
         GOTO1 ABLDORB                                                          
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADDB2    GOTO1 ADDELEM,DMCB,TRCEELEM                                            
         OC    BOOKS,BOOKS                                                      
         BZ    ADDB3                                                            
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    RE,ELEMENT          ADD RADIO DEMO DESC ELEM FOR BOOKS           
         USING RDELEM,RE                                                        
         MVI   RDCODE,X'63'                                                     
         MVI   RDLEN,10                                                         
         MVC   RDBOOKS,BOOKS                                                    
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         DROP  RE                                                               
*                                                                               
         OC    STAPST,STAPST       ADD CANADIAN PST ELEMENT                     
         BZ    ADDB2A                                                           
         MVI   ELEMENT,X'6B'                                                    
         MVI   ELEMENT+1,12                                                     
         MVC   ELEMENT+2(10),STAPST                                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADDB2A   CLI   MCLUNQ,0            ADD TRAFFIC MASTER ELEMENT                   
         BE    ADDB2B                                                           
         MVI   ELEMENT,X'61'                                                    
         MVI   ELEMENT+1,6                                                      
         MVC   ELEMENT+2(2),MCLCOD                                              
         MVC   ELEMENT+4(1),MCLUNQ                                              
         MVC   ELEMENT+5(1),MCLPRD                                              
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADDB2B   ICM   R0,15,SVECOST2                                                   
         BNZ   ADD2BC                                                           
         ICM   R0,15,CLTCOST2                                                   
         BZ    ADDB3                                                            
*                                                                               
ADD2BC   MVI   ELEMENT,X'73'                                                    
         MVI   ELEMENT+1,COS2LENQ                                               
         STCM  R0,15,ELEMENT+2                                                  
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
ADDB3    SR    R1,R1               GET MAX N'SPOTS THAT'LL FIT                  
         ICM   R1,3,BUYRLEN                                                     
         BAS   RE,GETMAXSP                                                      
         SR    R0,R0               CALCULATE MAX SPOTS PER WEEK                 
         SR    RF,RF                                                            
         ICM   RF,1,LNSPWKS                                                     
         BZ    INVLSPOT            STUPID FUCKING STRATA DATA                   
         DR    R0,RF                                                            
         STC   R1,LMAXSPW                                                       
         CLI   LMAXSPW,0           AVOID DISASTER LATER                         
         BNE   *+8                                                              
         MVI   LMAXSPW,1                                                        
*                                                                               
* BUILD SPOT ELEMENTS                                                           
*                                                                               
ADDB4    XC    BUYELEM,BUYELEM     BUILD SPOT ELEMENTS                          
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         TM    BITFLAG,X'80'       TEST FOR POOL ESTIMATE OPEN                  
         BZ    ADDB18                                                           
         TM    BITFLAG,X'40'       TEST FOR BRAND POL NPW                       
         BO    ADDB24              YES                                          
*                                                                               
* BUILD ONE ELEMENT FOR EACH SPOT FOR BRAND OR TRUE POOL                        
*                                                                               
         MVI   RCODE,11            YES - POOL ORIGINAL ELEMENT                  
         CLI   BINPBPRD,0          TEST FOR PIGGYBACKS                          
         BNE   ADDB6               YES                                          
*                                                                               
         MVI   RLEN,10                                                          
         CLI   BPRD,X'FF'          TEST FOR PRODUCT=POL (TRUE POL)              
         BE    ADDB9               YES-ADD SPOTS AS UNALLOCATED                 
*                                                                               
         MVI   RLEN,14                                                          
         MVC   RPPRD,BPRD          NO - ALLOCATED PRD IS CAMPAIGN PRD           
         MVC   RPTIME,BDSEC             TIME SHARE IS SPOT LENGTH               
         B     ADDB9                                                            
*                                                                               
ADDB6    MVI   RLEN,18             PIGGYBACKS- SET ACTIVE AND PASSIVE           
         MVC   RPPRD,BPRD                      PRODUCTS AND TIME SHARES         
         MVC   RPTIME,SCHMASLN                                                  
         MVC   RPPRD+L'RPALLOC(1),BINPBPRD                                      
         ZIC   R1,SCHTOTLN                                                      
         ZIC   R0,SCHMASLN                                                      
         SR    R1,R0                                                            
         STC   R1,RPTIME+L'RPALLOC                                              
         B     ADDB9                                                            
*                                                                               
ADDB9    LA    R4,LWKTAB                                                        
         LA    R3,LNEWTOT                                                       
         LA    R0,MAXWEEKS                                                      
         MVC   LNSPTS,LMAXSPTS+3                                                
         CLC   LSPTOT,LMAXSPTS     TEST MORE THAN MAX SPOTS/RECORD              
         BNH   ADDB10                                                           
         MVC   LNSPTS,LMAXSPW      YES - RESTRICT TO MAX SPOTS/WEEK             
*                                                                               
ADDB10   CLI   0(R4),FF            TEST END OF TABLE                            
         BE    ADDB16                                                           
         OC    0(2,R4),0(R4)       TEST ANY SPOTS THIS WEEK                     
         BZ    ADDB14                                                           
         MVC   RDATE,2(R4)         BUY DATE                                     
         MVC   ELEMENT(L'BUYELEM),BUYELEM                                       
*                                                                               
         ZIC   R5,LNSPTS           SET NUMBER OF SPOTS WE'LL PROCESS            
         CLC   LNSPTS,1(R4)        THIS WEEK                                    
         BNH   *+8                                                              
         IC    R5,1(R4)                                                         
*                                                                               
         ZIC   RF,0(R3)                                                         
         AR    RF,R5               UPDATE CUMULATIVE SPOTS/WEEK                 
         STC   RF,0(R3)                                                         
*                                                                               
ADDB12   BAS   RE,ADDELS                                                        
         BNE   ADDBN                                                            
         L     RE,LSPTOT           DECREMENT TOTAL SPOTS LEFT                   
         BCTR  RE,0                                                             
         ST    RE,LSPTOT                                                        
         LH    RE,0(R4)            DECREMENT SPOTS LEFT THIS WEEK               
         BCTR  RE,0                                                             
         STH   RE,0(R4)                                                         
         BCT   R5,ADDB12                                                        
*                                                                               
ADDB14   LA    R4,4(R4)            NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,ADDB10           DO FOR ALL WEEKS                             
*                                                                               
ADDB16   B     ADDB30              NOW WRITE THE BUY RECORD                     
*                                                                               
* ADD SPOT ELEMENTS FOR BRAND BUYING (NON-POOL) - ONE FOR EACH WEEK             
*                                                                               
ADDB18   MVI   RCODE,6             NON-POOL SPOTS                               
         MVI   RLEN,10                                                          
         CLI   BINPBPRD,0          TEST FOR PIGGYBACKS                          
         BE    *+8                                                              
         MVI   RLEN,12             YES - EXPAND ELEMENT                         
         LA    R4,LWKTAB                                                        
         LA    R3,LNEWTOT          R3=A(NEW SPOTS/WEEK ARRAY)                   
         LA    R0,MAXWEEKS                                                      
*                                                                               
ADDB20   CLI   0(R4),FF            TEST END OF SCHEDULE                         
         BE    ADDB23                                                           
         OC    0(2,R4),0(R4)       TEST ANY SPOTS THIS WEEK                     
         BZ    ADDB22                                                           
         MVC   RDATE,2(R4)         BUY DATE                                     
         MVC   RNUM,1(R4)          NUMBER OF SPOTS                              
         MVC   ELEMENT(L'BUYELEM),BUYELEM                                       
         BAS   RE,ADDELS           ADD BUY ELEMENT                              
         BNE   ADDBN                                                            
*                                                                               
         ZIC   RF,0(R3)            GET N'SPOTS FOR WEEK                         
         ZIC   RE,RNUM             GET N'SPOTS ADDED TO WEEK                    
         AR    RF,RE                                                            
         STC   RF,0(R3)            REPLACE CUMULATIVE SPOTS/WEEK                
*                                                                               
ADDB22   LA    R4,4(R4)            NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,ADDB20           DO FOR ALL WEEKS                             
*                                                                               
ADDB23   B     ADDB30                                                           
*                                                                               
* ADD SPOT ELEMENTS FOR BRAND POL NPW - ONE SPOT ELEMENT PER WEEK               
* WHERE HIGH ORDER 6 BITS OF COST CONTAIN THE NUMBER OF SPOTS                   
*                                                                               
ADDB24   MVI   RCODE,11            BRAND POL NPW                                
         MVI   RLEN,14                                                          
         MVC   RPPRD,BPRD          SET PRODUCT ALLOCATION                       
         MVC   RPTIME,BDSEC                                                     
         LA    R4,LWKTAB           R4=A(WEEK TABLE)                             
         LA    R3,LNEWTOT          R3=A(NEW SPOTS/WEEK ARRAY)                   
         LA    R0,MAXWEEKS         R0=LOOP COUNTER                              
*                                                                               
ADDB26   CLI   0(R4),FF            TEST FOR EOT                                 
         BE    ADDB30              YES                                          
         OC    0(2,R4),0(R4)       TEST ANY SPOTS THIS WEEK                     
         BZ    ADDB28              NO                                           
         MVC   RDATE,2(R4)                                                      
         ZIC   RE,1(R4)            GET SPOTS THIS WEEK                          
         SLL   RE,2                USE HIGH ORDER 6 BITS                        
         STC   RE,RPCOST                                                        
*                                                                               
         ZIC   RF,0(R3)            UPDATE NEW SPOTS/WEEK                        
         SRL   RE,2                RESTORE NUMBER OF SPOTS                      
         AR    RF,RE                                                            
         STC   RF,0(R3)                                                         
*                                                                               
         MVC   ELEMENT(L'BUYELEM),BUYELEM                                       
         BAS   RE,ADDELS                                                        
         BNE   ADDBN                                                            
*                                                                               
ADDB28   LA    R4,4(R4)            NEXT WEEK                                    
         LA    R3,1(R3)                                                         
         BCT   R0,ADDB26                                                        
*                                                                               
ADDB30   GOTO1 AWRTBREC                                                         
*                                                                               
         GOTOR CHGBTBL             UPDATE BUY LINE TABLE ENTRY                  
*                                                                               
         TM    BITFLAG,X'80'       TEST POOL BUY                                
         BZ    ADDBX               NO-DONE                                      
         TM    BITFLAG,X'40'       TEST BRAND POL NPW                           
         BO    ADDBX                                                            
         OC    LSPTOT,LSPTOT       TEST ANY SPOTS LEFT                          
         BZ    ADDBX               NO-DONE                                      
*                                                                               
         BAS   RE,GETBLINE         GET NEXT AVAILABLE LINE NUMBER               
         GOTO1 ABLDBKEY            CONSTRUCT BUY LINE KEY IN KEY                
         MVC   BUYKEY+10(2),KEY+11 SET BUY LINE NUMBER BYTES IN RECORD          
         MVI   BUYKEY+12,0                                                      
         GOTO1 DELELEM,DMCB,11     DELETE ALL BUY ELEMENTS                      
         B     ADDB4               PROCESS REMAINING SPOTS                      
*                                                                               
ADDBX    MVC   AIO,AIO1            RESET IO POINTER                             
         B     YES                                                              
*                                                                               
ADDBN    MVI   BYTE,12             SET ERROR CODE FOR RECORD OVERFLOW           
         B     NO                                                               
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO RE-TRANSFER TO BUY PROGRAM                               *         
***********************************************************************         
         SPACE 1                                                                
REXFR    NTR1  ,                                                                
         XC    LXFRTOT,LXFRTOT                                                  
         LA    R4,LSKEDS           R4=A(SKED TABLE)                             
         LR    RE,R4               CLEAR SKED TABLE                             
         LA    RF,LSKEDSL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
* SET LAST TRANSFER SPOTS PER WEEK ARRAY                                        
*                                                                               
         ZIC   R3,BNUMWKS                                                       
         LA    R3,SCHDATA(R3)      INDEX PAST SCHEDULED SPOTS                   
         ZIC   R1,NUMDCATS         R1=NUMBER OF DEMOS                           
         SLL   R1,1                X LENGTH OF A DEMO VALUE                     
         LA    R3,0(R1,R3)         INDEX PAST DEMOS                             
         ZIC   R1,BPRVWKS          GET NUMBER OF WEEKS IN LAST TRANSFER         
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   LXFRTOT(0),0(R3)                                                 
*                                                                               
* BUY LINES PREVIOUSLY CREATED BY TRANSFER ARE SEEDED INTO SKED TABLE           
*                                                                               
REX2     L     R3,ABLNTBL          R3 = A(BUYLINE TABLE)                        
         USING BLND,R3                                                          
         LA    R5,1                R5 = BUY LINE NUMBER TO BUMP THROUGH         
         GOTO1 DATCON,DMCB,(0,SCHODATE),(3,BINODATE)                            
*                                                                               
REX3     CLI   BLNMAS,0            IF THERE IS NO PRODUCT FOR BUYLINE           
         BE    REX4                THEN CHECK NEXT BUYLINE                      
*                                                                               
         CLC   SCHLNNUM,BLNSEQ     IF SCHEDULE #'S MATCH                        
         BNE   REX4                                                             
*                                                                               
         CLC   BINODATE,BLNDATE    AND ORIGINAL TRANSFER DATE MATCHES           
         BNE   REX4                                                             
*                                                                               
         CLC   SCHOTIME,BLNTIME    AND ORIGINAL TRANSFER TIME MATCHES           
         BNE   REX4                                                             
*                                                                               
         CLC   BPRD,BLNMAS         AND PRODUCT CODE MATCHES                     
         BNE   REX4                                                             
*                                                                               
         TM    BITFLAG,X'80'       AND PB MATCHES IF THERE IS ONE               
         BO    *+14                                                             
         CLC   BINPBPRD,BLNPIG                                                  
         BNE   REX4                                                             
*                                                                               
* TRY TO READ BUY LINE AND TEST IF BUY PERIOD ENDS BEFORE MONDAY                
* OF SCHEDULE START WEEK                                                        
*                                                                               
         STC   R5,NEWLINE                                                       
         GOTO1 ABLDBKEY            BUILD A BUY KEY FOR LINE                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'BUYKEY),KEYSAVE TEST IF LINE FOUND                         
         BNE   REX4                NO                                           
*                                                                               
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
         CLC   BDEND,SCHSTMNB      TEST BUY ENDS BEFORE FIRST MON               
         BL    REX4                YES-SKIP IT                                  
*                                                                               
         MVC   0(1,R4),NEWLINE     SET BUY LINE IN SKED TABLE                   
         LA    R4,LSKEDENL(R4)     NEXT SKED TABLE ENTRY                        
         DROP  R2                                                               
*                                                                               
REX4     LA    R3,BLNLNQ(R3)                                                    
         LA    R5,1(R5)            INCREMENT BUY NUMBER                         
         CH    R5,=H'256'                                                       
         BL    REX3                                                             
*                                                                               
         LA    R4,LSKEDS           RESET R4 TO START OF SCHED TAB               
         CLI   0(R4),0             TEST IF ANY BUY LINES FOUND                  
         BNE   REX6                YES                                          
         OI    LFLAG,LDELETE       NO-SET BUY LINE DELETED FLAG                 
         B     REXX                AND EXIT RIGHT AWAY                          
*                                                                               
* ZERO WEEK INDICATOR TABLE THEN IF RETRANSFER OF ALREADY                       
* TRANSFERRED WEEKS IS NOT ALLOWED, MARK PREVIOUSLY TRANSFERRED                 
* WEEKS AS FROZEN                                                               
*                                                                               
REX6     XC    LWKINDS,LWKINDS                                                  
         CLI   HDRREPRV,C'N'                                                    
         BNE   REX10                                                            
         LA    R6,LWKINDS          YES -                                        
         LA    R3,LXFRTOT                                                       
         ZIC   R0,BPRVWKS          R0=LOOP COUNTER=# OF WKS IN LAST XFR         
*                                                                               
REX7     CLI   0(R3),0             TEST THIS WEEK HAS TRANSFER SPOTS            
         BE    REX9                                                             
*                                                                               
REX8     OI    0(R6),LFRZ          FREEZE THE WEEK                              
         OI    LFLAG,LFREEZE                                                    
*                                                                               
REX9     LA    R6,1(R6)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,REX7                                                          
*                                                                               
REX10    XC    LBUYTOT,LBUYTOT     FIND TOTAL SPOTS/WEEK ACCORDING              
         MVI   LCHGIND,0           (INITIALIZE CHANGE INDICATOR)                
         LA    R4,LSKEDS           R4=A(SKED TABLE)                             
*                                                                               
* READ THE NEXT BUY LINE CREATED BY LAST TRANSFER                               
*                                                                               
REX12    CLI   0(R4),0             TEST ANY MORE BUY LINES                      
         BE    REX32               NO                                           
         MVC   NEWLINE,0(R4)       SET BUY LINE NUMBER                          
         GOTO1 ABLDBKEY                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         GOTO1 ABUYCHG             DETERMINE CHANGES SINCE LAST XFR             
         SR    R0,R0                                                            
         L     R6,AIO                                                           
         LA    R6,BDELEM-BUYREC(R6)  SCAN BUY ELEMENTS                          
         USING REGELEM,R6                                                       
*                                                                               
* SEARCH FOR BUY ELEMENTS                                                       
*                                                                               
REX14    CLI   0(R6),0             TEST END OF BUY RECORD                       
         BE    REX30                                                            
         CLI   0(R6),6             NON-POOL BUY ELEMENT                         
         BE    REX16                                                            
         CLI   0(R6),11            POOL BUY ELEMENT                             
         BE    REX16                                                            
         CLI   0(R6),7             NON-POOL OTO ELEMENT                         
         BE    REX16                                                            
         CLI   0(R6),12            POOL OTO ELEMENT                             
         BE    REX16                                                            
         CLI   0(R6),X'10'         TEST FOR AFFIDAVIT ELEMENT                   
         BNE   REX28                                                            
         OI    LFLAG,LAFFDVT       YES - FLAG                                   
         B     REX28                                                            
*                                                                               
* FIND WEEK OF BUY                                                              
*                                                                               
REX16    CLI   DAILY,C'Y'          TEST FOR DAILY SCEHDULING                    
         BE    *+18                                                             
         CLC   RDATE,SCHSTMNP      COMPARE BUY DATE TO START MONDAY             
         BL    REX28               LOW - IGNORE IT                              
         B     REX17                                                            
*                                                                               
         CLC   RDATE,SCHDATS       COMPARE BUY DATE VS FIRST DATE               
         BL    REX28               LOW - IGNORE IT                              
*                                                                               
REX17    LA    RE,SCHDATS          FIND WHICH WEEK THIS BUY DATE IS IN          
         LA    RF,2(R4)            RF = A(SPOTS PER WEEK FIELD)                 
         LA    R3,LWKINDS                                                       
         LA    R2,LBUYTOT                                                       
         ZIC   R0,BNUMWKS                                                       
         CLM   R0,1,BPRVWKS        USE THE LARGER OF NUMBER OF WEEKS            
         BNL   *+8                 IN SCHEDULE NOW VS. NUMBER OF WKS            
         IC    R0,BPRVWKS          AT TIME OF LAST TRANSFER                     
*                                                                               
REX18    CLC   RDATE,2(RE)         IS BUY DATE IN THIS WEEK                     
         BNH   REX20                                                            
         LA    RE,4(RE)            NO - NEXT WEEK                               
         LA    RF,2(RF)                                                         
         LA    R3,1(R3)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,REX18                                                         
         B     REX28               BUY DATE GT PERIOD END - IGNORE IT           
*                                                                               
* BUY IS WITHIN SCHEDULE PERIOD--ACCUMULATE SPOTS FOR EACH WEEK AND             
* NOTE FROZEN WEEKS.  WEEKS ARE FROZEN FOR PRESENCE OF OTO'S, BILLED            
* OR PAID SPOTS, OR SPOTS WITH AFFIDAVIT DATA.  LBUYTOT WILL CONTAIN            
* THE SUM OF ALL BUY LINE SPOTS BY WEEK ON EXIT FROM THE LOOP.                  
*                                                                               
REX20    CLI   RCODE,7             TEST NON-POOL OTO                            
         BE    REX26               YES - FREEZE THIS WEEK                       
         CLI   RCODE,12            TEST POOL OTO                                
         BE    REX26               YES - FREEZE THIS WEEK                       
*                                                                               
         ZIC   R1,0(RF)            ACCUMULATE NUMBER OF SPOTS/WEEK              
         LA    RE,1                IN THIS BUYLINE                              
         CLI   RCODE,6                                                          
         BNE   *+8                                                              
         IC    RE,RNUM                                                          
         TM    BITFLAG,X'40'       TEST BRAND POL NPW                           
         BZ    *+12                NO                                           
         IC    RE,RPCOST           YES-NUMBER OF SPOTS IS HIGH                  
         SRL   RE,2                ORDER 6 BITS OF COST                         
*                                                                               
         AR    R1,RE                                                            
         STC   R1,0(RF)                                                         
         IC    R1,0(R2)            ACCUMULATE TOTAL SPOTS/WEEK OVER             
         AR    R1,RE               ALL BUYLINES                                 
         STC   R1,0(R2)                                                         
         TM    0(R3),LFRZ          TEST WEEK IS ALREADY FROZEN                  
         BO    REX28               YES - NEXT BUY ELEMENT                       
         OC    RPAY,RPAY           TEST SPOT IS PAID                            
         BNZ   REX26               YES - FREEZE THIS WEEK                       
*                                                                               
REX24    IC    R0,1(R6)            LOOK AT NEXT ELEMENT                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         TEST NEXT ELEMENT IS AFFIDAVIT               
         BNE   REX14               NO - WEEK IS OK                              
*                                                                               
REX26    OI    0(R3),LFRZ          FREEZE THIS WEEK                             
         OI    LFLAG,LFREEZE                                                    
*                                                                               
REX28    IC    R0,1(R6)            NEXT ELEMENT                                 
         AR    R6,R0                                                            
         B     REX14                                                            
*                                                                               
REX30    LA    R4,LSKEDENL(R4)     NEXT BUY LINE                                
         B     REX12               READ NEXT BUY RECORD                         
*                                                                               
* COMPARE TOTAL SPOTS LAST TRANSFERRED VERSUS TOTAL SPOTS ON                    
* BUY LINES BY WEEK TO TEST FOR ANY CHANGES SINCE LAST TRANSFER                 
*                                                                               
REX32    LA    RE,LXFRTOT          COMPARE SPOTS/WEEK BETWEEN                   
         LA    RF,LBUYTOT          LAST BUY TRANSFER AND WHAT THE               
         LA    R6,LWKINDS          BUY RECORDS NOW SAY                          
         ZIC   R0,BPRVWKS          # OF WKS AT TIME OF LAST TRANSFER            
*                                                                               
REX34    CLC   0(1,RE),0(RF)                                                    
         BE    *+12                                                             
         OI    0(R6),LFRZ          NOT EQUAL - FREEZE THIS WEEK                 
         OI    LFLAG,LFREEZE                                                    
         LA    R6,1(R6)                                                         
         LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,REX34                                                         
*                                                                               
* MOVE SCHEDULED SPOTS TO FIRST SKED TABLE ENTRY                                
*                                                                               
         LA    R1,SCHSPOTS         R1=BXLE INDEX                                
         ZIC   RF,BNUMWKS          NUMBER OF WEEKS IN SCHEDULE                  
         LA    RF,SCHSPOTS-1(RF)   RF=BXLE LIMIT                                
         LA    RE,1                RE=BXLE LIMIT                                
         LA    R5,LSKEDS+2                                                      
         MVC   1(1,R5),0(R1)       SCHEDULED SPOTS/WEEK                         
         LA    R5,2(R5)                                                         
         BXLE  R1,RE,*-10                                                       
*                                                                               
* IF THE NUMBER OF SCHEDULED SPOTS ON THE PC EXCEEDS THE NUMBER OF              
* SPOTS IN THE BUY PROGRAM FOR THE WEEK, SPREAD THE EXCESS TO                   
* SUBSEQUENT BUYLINES IF POSSIBLE.  IF A WEEK IS FROZEN, SPREADING              
* WILL NOT OCCUR.                                                               
*                                                                               
REX36    LA    R4,LSKEDS                                                        
*                                                                               
REX38    CLI   0(R4),0             TEST END OF EXISTING BUY LINES               
         BE    REX44                                                            
         LA    R6,LWKINDS                                                       
         LA    R5,2(R4)            R5=A(SKED WEEK POSITION)                     
         ZIC   R0,BNUMWKS                                                       
*                                                                               
REX40    TM    0(R6),LFRZ          TEST WEEK FROZEN                             
         BO    REX42                                                            
         CLC   0(1,R5),1(R5)       COMPARE BUY SPOTS TO PC SPOTS                
         BNL   REX41                                                            
         LA    R1,LSKEDENL(R5)     LOW --                                       
         CLI   0(R1),0             TEST ANY BUY SPOTS FOR THIS WEEK IN          
         BE    REX41               NEXT BUYLINE                                 
         ZIC   RE,1(R5)            YES -                                        
         ZIC   RF,0(R5)            DIFFERENCE BETWEEN BUY SPOTS AND             
         SR    RE,RF               PC SPOTS GOES INTO PC SPOTS FOR              
         STC   RE,1(R1)            NEXT BUYLINE                                 
         STC   RF,1(R5)            FOR THIS BYLINE, PC SPTS = BUY SPTS          
*                                                                               
REX41    CLC   0(1,R5),1(R5)       TEST ANY CHANGE IN #SPOTS/WEEK               
         BE    REX42                                                            
         OI    1(R4),LCHG          YES - INDICATE CHANGE FOR BUYLINE            
*                                                                               
REX42    LA    R6,1(R6)            NEXT WEEK                                    
         LA    R5,2(R5)            NEXT SKED WEEK POSITION                      
         BCT   R0,REX40            DO FOR ALL WEEKS                             
*                                                                               
         LA    R4,LSKEDENL(R4)     NEXT SKED BUYLINE ENTRY                      
         B     REX38               DO FOR ALL BUYLINES                          
         EJECT                                                                  
***********************************************************************         
* PROCESS ALL THE BUYLINES                                            *         
***********************************************************************         
         SPACE 1                                                                
REX44    LA    R6,LSKEDS                                                        
         MVC   AIO,AIO2            RE-BUILD RECORDS IN IO2                      
*                                                                               
* READ EACH BUY AND REPLACE THE NON-BUY ELEMENTS                                
*                                                                               
REX49    OC    0(LSKEDENL,R6),0(R6)   TEST END OF BUYLINES                      
         BZ    REX90                                                            
         ST    R6,LASKED           SAVE ADDRESS OF THIS SCHEDULE ENTRY          
         MVI   LNSPTS,255          PRE-SET MAX SPOTS/WEEK                       
         CLI   0(R6),0             TEST NEW BUY LINE TO BE CREATED              
         BE    REX66                                                            
*                                                                               
         MVC   NEWLINE,0(R6)       SET BUY LINE NUMBER                          
         GOTO1 ABLDBKEY                                                         
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
         CLI   HDRDEMO,C'N'        TEST DEMO CHANGES ALLOWED                    
         BE    REX55               NO                                           
         GOTO1 DELELEM,DMCB,2      DELETE DEMO ELEMENT                          
         GOTO1 ABLDDEMO            BUILD NEW DEMO ELEMENT                       
         GOTO1 ADDELEM,DMCB,ELEMENT   ADD DEMO ELEMENT                          
*                                                                               
* REPLACE RADIO DESCRIPTION ELEMENT                                             
*                                                                               
REX55    GOTO1 DELELEM,DMCB,99                                                  
         OC    BOOKS,BOOKS                                                      
         BZ    REX55A                                                           
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         LA    RE,ELEMENT                                                       
         USING RDELEM,RE                                                        
         MVI   RDCODE,X'63'                                                     
         MVI   RDLEN,10                                                         
         MVC   RDBOOKS,BOOKS                                                    
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
         DROP  RE                                                               
*                                                                               
* REPLACE CANADIAN PST ELEMENT                                                  
*                                                                               
REX55A   GOTO1 DELELEM,DMCB,107                                                 
         OC    STAPST,STAPST                                                    
         BZ    REX55B                                                           
         MVI   ELEMENT,X'6B'                                                    
         MVI   ELEMENT+1,12                                                     
         MVC   ELEMENT+2(10),STAPST                                             
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
* REPLACE TRAFFIC MASTER ELEMENT                                                
*                                                                               
REX55B   GOTO1 DELELEM,DMCB,97                                                  
         CLI   MCLUNQ,0                                                         
         BE    REX55C                                                           
         MVI   ELEMENT,X'61'                                                    
         MVI   ELEMENT+1,6                                                      
         MVC   ELEMENT+2(2),MCLCOD                                              
         MVC   ELEMENT+4(1),MCLUNQ                                              
         MVC   ELEMENT+5(1),MCLPRD                                              
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
* REPLACE COST2 ELEMENT                                                         
*                                                                               
REX55C   GOTO1 DELELEM,DMCB,X'73'                                               
         ICM   R0,15,SVECOST2                                                   
         BNZ   REX55D                                                           
         ICM   R0,15,CLTCOST2                                                   
         BZ    REX56                                                            
*                                                                               
REX55D   MVI   ELEMENT,X'73'                                                    
         MVI   ELEMENT+1,COS2LENQ                                               
         STCM  R0,15,ELEMENT+2                                                  
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
* REPLACE COMMENT ELEMENT                                                       
*                                                                               
REX56    MVI   BYTE,1                                                           
         GOTO1 SRCHDEL,DMCB,102,(1,BYTE)                                        
         MVI   BYTE,2                                                           
         GOTO1 (RF),(R1),102,(1,BYTE)                                           
         MVI   BYTE,3                                                           
         GOTO1 (RF),(R1),102,(1,BYTE)                                           
         CLI   SCHCOMLN,0          TEST IF ANY COMMENT                          
         BE    REX58               NO                                           
*                                                                               
         GOTO1 ABLDCOM                                                          
         LA    R4,ELEMENT                                                       
*                                                                               
REX57    CLI   0(R4),0             ADD THE COMMENT ELEMENTS                     
         BE    REX58                                                            
         GOTO1 ADDELEM,DMCB,(R4)                                                
         ZIC   R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     REX57                                                            
*                                                                               
* REPLACE ORBIT ELEMENT                                                         
*                                                                               
REX58    GOTO1 DELELEM,DMCB,103                                                 
         TM    SCHINDS,SCHIORB     TEST ORBIT                                   
         BZ    REX60                                                            
         GOTO1 ABLDORB                                                          
         GOTO1 ADDELEM,DMCB,ELEMENT                                             
*                                                                               
* FOR POOL ESTIMATES-FIND LENGTH OF RECORD WITHOUT SPOT ELEMENTS                
*                                                                               
REX60    TM    BITFLAG,X'80'       FOR POOL BUY  --                             
         BZ    REX66                                                            
         TM    BITFLAG,X'40'       TEST BRAND POOL NPW                          
         BO    REX66               YES-ITS SIMILAR TO BRAND BUYING              
         L     R4,AIO                                                           
         LA    R4,BDELEM-BUYRECD(R4)  FIND LENGTH OF RECORD W/O                 
         SR    R0,R0               SPOT ELEMENTS                                
         LA    R1,BDELEM-BUYRECD                                                
*                                                                               
REX61    CLI   0(R4),0                                                          
         BE    REX62                                                            
         IC    R0,1(R4)                                                         
         CLI   0(R4),X'0B'                                                      
         BL    *+12                                                             
         CLI   0(R4),X'18'                                                      
         BNH   *+6                                                              
         AR    R1,R0                                                            
         AR    R4,R0                                                            
         B     REX61                                                            
*                                                                               
REX62    BAS   RE,GETMAXSP         GET MAX N'SPOTS FOR THIS BUY RECORD          
*                                                                               
* COMPUTE MAXIMUM NUMBER OF SPOTS WHICH CAN BE ADDED TO ANY WEEK                
* AS A PRELUDE TO SPREADING EXCESS SPOTS OVER SUBSEQUENT LINES                  
* (POOL ONLY)                                                                   
*                                                                               
         LA    R6,2(R6)            R6=A(SKED WEEKLY SPOTS BUY VS. SCH)          
         LA    R0,MAXWEEKS         R0=LOOP COUNTER                              
         LA    R4,LWKINDS                                                       
         SR    R1,R1               R1 = N'WEEKS GETTING MORE SPOTS              
         SR    R2,R2               R2 = N'SPOTS STAYING IN BUY RECORD           
         SR    R3,R3               R3 = N'SPOTS TO ADD TO BUY RECORD            
         SR    RE,RE                                                            
         SR    RF,RF                                                            
*                                                                               
REX63    IC    RE,0(R6)                                                         
         TM    0(R4),LFRZ          TEST WEEK IS FROZEN                          
         BO    REX64               YES-ALL EXISITING SPOTS STAY                 
         IC    RF,1(R6)                                                         
         CR    RE,RF               COMPARE EXISTING TO PROPOSED                 
         BL    *+10                                                             
         LR    RE,RF               NOT LOW - N'PROPOSED SPOTS STAY              
         B     REX64                                                            
         SR    RF,RE               LOW-ALL EXISTING SPOTS STAY                  
         AR    R3,RF                   ADD THE DIFFERENCE                       
         LA    R1,1(R1)                AUGMENT N'WEEKS GETTING MORE             
*                                                                               
REX64    AR    R2,RE                                                            
         LA    R4,1(R4)                                                         
         LA    R6,2(R6)                                                         
         BCT   R0,REX63            NEXT WEEK                                    
*                                                                               
         LTR   R3,R3               TEST ANY SPOTS TO BE ADDED                   
         BZ    REX66                                                            
         L     RF,LMAXSPTS         SET MAX SPOTS PER WEEK HIGH                  
         STC   RF,LNSPTS                                                        
         SR    RF,R2               RF=MAX N'SPOTS THAT CAN BE ADDED             
         BP    *+10                                                             
         SR    RF,RF               NOT POSITIVE-ALL NEW SPOTS GO TO             
         B     REX65                            NEXT BUYLINE                    
         CR    R3,RF               TEST SPTS TO BE ADDED GT MAX ALLOWED         
         BNH   REX66                                                            
         SR    RE,RE               YES-DIVIDE TO GIVE MAX N'SPOTS THAT          
         DR    RE,R1                   CAN BE ADDED PER WEEK                    
*                                                                               
REX65    STC   RF,LNSPTS           SET MAX N'SPOTS CAN BE ADDED PER WK          
*                                                                               
* SPREAD EXCESS SPOTS OVER SUBSEQUENT BUY LINES                                 
*                                                                               
REX66    ZIC   R0,BNUMWKS                                                       
         LA    R4,LWKINDS          R4=A(WEEK INDICATORS)                        
         L     R6,LASKED           R6=A(SKED TABLE ENTRY)                       
         LA    R3,2(R6)            R3=A(SKED WEEKLY SPOTS ARRAY)                
         LA    R2,LNEWTOT          R2=A(CUMULATIVE NEW SPOTS/WEEK)              
         ZIC   R5,LNSPTS           R5=MAX SPOTS ALLOWED TO ADD PER WEEK         
         XC    LSPOTS,LSPOTS       CLEAR SPOTS/WEEK ARRAY                       
         LA    R7,LSPOTS           R7=SPOTS/WK ARRAY FOR BLDWK                  
*                                                                               
REX68    ZIC   RE,0(R3)                                                         
         LR    R1,RE               R1=N'SPOTS FOR THIS WEEK                     
         TM    0(R4),LFRZ          TEST WEEK IS FROZEN                          
         BO    REX70               YES-SPOTS STAY PUT                           
         ZIC   RF,1(R3)                                                         
         LR    R1,RF                                                            
         SR    RF,RE               TEST SPOTS TO ADD                            
         BNP   REX70                                                            
         SR    RF,R5               YES-TEST GREATER THAN MAX ALLOWED            
         BNP   REX70                                                            
         SR    R1,RF               YES-THIS BUYLINE ONLY GETS MAX               
         STC   R1,1(R3)                                                         
         IC    RE,LSKEDENL+1(R3)   AND ADD REMAINDER TO NEXT BUYLINE            
         AR    RE,RF                                                            
         STC   RE,LSKEDENL+1(R3)                                                
         OI    LSKEDENL+1(R6),LCHG MAKE SURE NEXT BUYLINE IS CHANGED            
*                                                                               
REX70    CLI   0(R6),0             TEST IF WILL BE ADDING BUY                   
         BE    REX71                                                            
         ZIC   RF,0(R2)            UPDATE CUMULATIVE SPOTS/WEEK                 
         AR    RF,R1                                                            
         STC   RF,0(R2)                                                         
*                                                                               
REX71    STC   R1,0(R7)            SET WEEK IN SPOTS ARRAY                      
*                                                                               
REX72    LA    R4,1(R4)            NEXT WEEK                                    
         LA    R3,2(R3)                                                         
         LA    R2,1(R2)                                                         
         LA    R7,1(R7)                                                         
         BCT   R0,REX68            DO FOR ALL WEEKS                             
*                                                                               
         LA    R1,LBADD            PASS ADD/CHANGE INDICATOR TO BLDWKS          
         CLI   0(R6),0                                                          
         BE    *+8                                                              
         LA    R1,LBCHA                                                         
         GOTO1 ABLDWKS             BUILD WEEKS TABLE                            
*                                                                               
         CLI   0(R6),0             TEST NEW BUY LINE TO BE ADDED                
         BNE   REX73                                                            
         TM    BITFLAG,X'80'       YES - MUST BE POOL BUY                       
         BO    *+6                                                              
         DC    H'0'                                                             
         TM    BITFLAG,X'40'       AND NOT BRAND POOL NPW                       
         BZ    *+6                                                              
         DC    H'0'                                                             
         CLI   LNSPWKS,0           TEST ANY WEEKS WITH SPOTS                    
         BNE   *+6                                                              
         DC    H'0'                                                             
         BAS   RE,ADDBUY           YES-GO ADD THE NEW BUY LINE                  
         MVC   AIO,AIO2            RESTORE IO POINTER                           
         BE    REX84                                                            
         B     REX99                                                            
*                                                                               
REX73    XC    LBUYCOST,LBUYCOST   ALTER EXISTING BUY LINE                      
         MVC   LBUYCOST+1(3),SCHCOST                                            
         L     R4,AIO                                                           
         LA    R4,BDELEM-BUYREC(R4)                                             
         USING BDELEM,R4                                                        
         CLI   HDRCOST,C'Y'        TEST COST CHANGES ALLOWED                    
         BE    *+10                YES                                          
         MVC   LBUYCOST+1(3),BDCOST NO-FUDGE COST TO BUY COST                   
         DROP  R4                                                               
*                                                                               
         GOTO1 ABUYDESC,LBCHA                                                   
         BNE   NO                  EXIT RIGHT AWAY WITH ERROR                   
         GOTO1 DELELEM,DMCB,1                                                   
         GOTO1 ADDELEM,DMCB,DESCELEM                                            
*                                                                               
         TM    LCHGIND,LSTDATE+LSLN   TEST ANY CHANGES                          
         BZ    REX74         THAT WILL EFFECT SPOT ELEMENTS                     
         GOTO1 ASPOTCHG      YES-GO CHANGE THEM                                 
*                                                                               
REX74    TM    1(R6),LCHG          TEST ANY CHANGE TO NUMBER OF SPOTS           
         BZ    REX83               IN THIS BUYLINE                              
         SPACE 1                                                                
* ADD/DELETE SPOT ELEMENTS IN BUY RECORD                                        
*                                                                               
         OI    LFLAG,LFSTWK                                                     
         LA    R0,MAXWEEKS         R0=LOOP COUNTER                              
         LA    R4,LWKINDS          R4=A(WEEK INDICATORS)                        
         LA    R5,LWKTAB           R5=A(WEEKLY SCHEDULED SPOTS TABLE)           
         LA    R2,2(R6)            R2 = A(SPOTS/WEEK FIELD IN SKED TBL)         
         LA    R3,SCHDATS          R3=A(BROADCAST WEEK CALENDAR)                
*                                                                               
REX76    TM    0(R4),LFRZ          TEST WEEK IS FROZEN                          
         BO    REX80               YES - LEAVE WEEK ALONE                       
         CLC   0(1,R2),1(R2)       TEST CHANGE TO NUMBER OF SPOTS               
         BE    REX80               NO - SKIP THIS WEEK                          
         TM    BITFLAG,X'40'       TEST BRAND POOL NPW                          
         BO    REX77                                                            
         TM    BITFLAG,X'80'                                                    
         BO    REX78                                                            
*                                                                               
* ADD/DELETE NON-POOL SPOTS                                                     
*                                                                               
         GOTO1 ANPOLSPT,DMCB,AIO,(R5),(R2),(R3)                                 
         BNE   REX99                                                            
         B     REX80                                                            
*                                                                               
* ADD/DELETE BRAND NPW SPOTS                                                    
*                                                                               
REX77    GOTO1 ANPWSPT,DMCB,AIO,(R5),(R2),(R3)                                  
         BNE   REX99                                                            
         B     REX80                                                            
*                                                                               
* ADD/DELETE POOL SPOTS                                                         
*                                                                               
REX78    GOTO1 APOOLSPT,DMCB,AIO,(R5),(R2),(R3)                                 
         BNE   REX99               EXIT FOR RECORD OVERFLOW                     
*                                                                               
REX80    LA    R4,1(R4)            NEXT WEEK                                    
         LA    R5,4(R5)                                                         
         LA    R2,2(R2)                                                         
         LA    R3,4(R3)                                                         
         NI    LFLAG,FF-LFSTWK                                                  
         BCT   R0,REX76            DO FOR ALL WEEKS                             
*                                                                               
REX83    GOTO1 AWRTBREC            RE-READ FOR UPDATE/WRITE IT BACK             
*                                                                               
REX84    L     R6,LASKED                                                        
         LA    R6,LSKEDENL(R6)     NEXT BUY LINE                                
         B     REX49                                                            
*                                                                               
REX90    MVC   AIO,AIO1            RESET IO AREA                                
         B     REXX                                                             
*                                                                               
REX99    MVI   BYTE,12             BIG PROBLEM - RECORD OVERFLOW                
         B     NO                                                               
*                                                                               
REXX     B     YES                                                              
         EJECT                                                                  
***********************************************************************         
* CALCULATE MAX NUMBER OF SPOTS THAT WILL FIT IN REMAINDER OF BUYREC  *         
* INPUT:  R1=LENGTH OF BUY RECORD                                     *         
* OUTPUT: R1=LMAXSPTS=MAX NUMBER OF SPOTS                             *         
***********************************************************************         
         SPACE 1                                                                
GETMAXSP LNR   R1,R1                                                            
         LA    R1,2000(R1)                                                      
         SR    R0,R0                                                            
         D     R0,=F'24'           ** ALLOW 24 BYTES PER SPOT **                
         ST    R1,LMAXSPTS                                                      
         BR    RE                                                               
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO ADD AN ELEMENT                                       *             
* AT ENTRY, AIO=RECORD AND ELEMENT CONTAINS ELEMENT TO BE ADDED   *             
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                *             
*******************************************************************             
         SPACE 1                                                                
ADDELS   NTR1  ,                                                                
         GOTO1 HELLO,DMCB,(C'P',=CL8'SPTFIL'),AIO,ELEMENT                       
         CLI   12(R1),0            TEST OK                                      
         BE    YES                                                              
         B     NO                                                               
         EJECT                                                                  
***********************************************************************         
* SET OF ERRORS USED BY THIS OVERLAY.                                           
***********************************************************************         
*                                                                               
EXIT     GOTO1 WRKCLOS             CLOSE WORKER FILE                            
         CLI   SETSENT,C'Y'        IF SET SENT FLAG IS TRUE                     
         BNE   EXIT2                                                            
         GOTO1 WRKSENT             SET WORKER FILE TO SENT                      
*                                                                               
EXIT2    L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE CALLS GETITEM BUT ALSO TRACES THE OBJECT TO THE                  
* WORKER FILE.                                                                  
***********************************************************************         
GETITEML NTR1  BASE=*,LABEL=*                                                   
         GOTO1 GETITEM             GET ITEM FROM INPUT FRAME                    
*                                                                               
         CLI   EIFFLAG,C'Y'        EXIT IF END OF FRAME                         
         BE    GILX                                                             
         CLC   TYPENUM,=A(ITEOD)   OR END OF DATA OBJECT                        
         BE    GILX                                                             
*                                                                               
         L     RF,AFREE            RF = A(MVS TYPE RECORD)                      
         L     R2,ADATA            R2 = A(OBJECT DATA)                          
*                                                                               
         CLC   TYPENUM,=A(ITUPLBUY)    TEST HEADER OBJECT                       
         BNE   GIL10                                                            
         USING HDROBJCT,R2                                                      
*                                                                               
         MVC   4(4,RF),TYPENUM     PUT HEADER DATA IN RECORD                    
         MVC   8(1,RF),HDRMED                                                   
         MVI   9(RF),C'º'                                                       
         MVC   10(3,RF),HDRCLT                                                  
         MVI   13(RF),C'º'                                                      
         MVC   14(3,RF),HDRPR1                                                  
         MVI   17(RF),C'º'                                                      
         MVC   18(3,RF),HDRPPB                                                  
         MVI   21(RF),C'º'                                                      
         MVC   22(2,RF),HDREST                                                  
         MVI   24(RF),C'º'                                                      
         MVC   25(1,RF),HDRSERV                                                 
         MVI   26(RF),C'º'                                                      
         MVC   27(16,RF),HDRBOOKS                                               
         MVI   43(RF),C'º'                                                      
         MVC   44(6,RF),HDRSDATE                                                
         MVI   50(RF),C'º'                                                      
         MVC   51(6,RF),HDREDATE                                                
         MVI   57(RF),C'º'                                                      
         MVC   58(2,RF),HDRWKS                                                  
         MVI   60(RF),C'º'                                                      
         MVC   61(1,RF),HDRGENTA                                                
         MVI   62(RF),C'º'                                                      
         MVC   63(1,RF),HDRRETR                                                 
         MVI   64(RF),C'º'                                                      
         MVC   65(6,RF),HDRPREND                                                
         MVI   71(RF),C'º'                                                      
         MVC   72(1,RF),HDRREPRV                                                
         MVI   73(RF),C'º'                                                      
         MVC   74(1,RF),HDRCOST                                                 
         MVI   75(RF),C'º'                                                      
         MVC   76(1,RF),HDRDEMO                                                 
         MVI   77(RF),C'º'                                                      
*                                                                               
         LA    RF,78(RF)           LOOP UNTIL END OF DEMOS                      
         LA    RE,HDRDTYPS                                                      
         LR    R1,RE                                                            
         CLC   0(2,RE),=C'FF'                                                   
         BE    *+12                                                             
         LA    RE,6(RE)                                                         
         B     *-14                                                             
         LA    RE,2(RE)                                                         
         SR    RE,R1                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8              MOVE DEMOS INTO RECORD                       
         B     *+10                                                             
         MVC   0(0,RF),HDRDTYPS                                                 
         LA    RF,1(RE,RF)                                                      
         MVI   0(RF),C'º'                                                       
         LA    RE,1(RE,R1)                                                      
*                                                                               
         MVC   1(1,RF),0(RE)       HDRDAILY                                     
         MVI   2(RF),C'º'                                                       
         MVC   3(1,RF),1(RE)       HDRRATE                                      
         MVI   4(RF),C'º'                                                       
         MVC   5(4,RF),2(RE)       HDRREP                                       
*                                                                               
         LA    RE,9(RF)            PUT LENGTH IN HEADER                         
         L     RF,AFREE                                                         
         SR    RE,RF                                                            
         STH   RE,0(RF)                                                         
         XC    2(2,RF),2(RF)                                                    
         B     GIL90                                                            
*                                                                               
GIL10    CLC   TYPENUM,=A(ITUPLBYS)    TEST SLINE OBJECT                        
         BNE   GIL20                                                            
         USING SLD,R2                                                           
*                                                                               
         MVC   4(4,RF),TYPENUM     PUT SLINE DATA IN RECORD                     
         MVC   8(8,RF),SLSEQ                                                    
         MVI   16(RF),C'º'                                                      
         MVC   17(6,RF),SLODATE                                                 
         MVI   23(RF),C'º'                                                      
         MVC   24(6,RF),SLOTIME                                                 
         MVI   30(RF),C'º'                                                      
         MVC   31(5,RF),SLSTAT                                                  
         MVI   36(RF),C'º'                                                      
         MVC   37(62,RF),SLDAYTIM                                               
         MVI   99(RF),C'º'                                                      
         MVC   100(1,RF),SLDAYPRT                                               
         MVI   101(RF),C'º'                                                     
         MVC   102(2,RF),SLMASLEN                                               
         MVI   104(RF),C'º'                                                     
         MVC   105(2,RF),SLLENGTH                                               
         MVI   107(RF),C'º'                                                     
         MVC   108(6,RF),SLCOST                                                 
         MVI   114(RF),C'º'                                                     
         MVC   115(18,RF),SLPROG                                                
         MVI   133(RF),C'º'                                                     
         MVC   134(4,RF),SLOVERS                                                
         MVI   138(RF),C'º'                                                     
         DROP  R2                                                               
*                                                                               
         L     RE,=A(SLDATA-SLD)   PUT VARIABLE LENGTH DATA AT THE END          
         L     R0,ADATA                                                         
         AR    R0,RE                                                            
         L     R1,DATALEN                                                       
         SR    R1,RE                                                            
         LA    R2,139(RF)                                                       
         LR    R3,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
         L     RF,AFREE            PUT LENGTH IN HEADER                         
         SR    R2,RF                                                            
         STH   R2,0(RF)                                                         
         XC    2(2,RF),2(RF)                                                    
         B     GIL90                                                            
*                                  ANY OTHER OBJECT TYPE...                     
GIL20    MVC   4(4,RF),TYPENUM     PUT TYPE NUMBER IN FIRST 4 BYTES             
*                                                                               
         L     R0,ADATA            PUT DATA AFTER TYPE NUMBER                   
         L     R1,DATALEN                                                       
         LA    R2,8(RF)                                                         
         LR    R3,R1                                                            
         MVCL  R2,R0                                                            
*                                                                               
GIL90    GOTO1 WRKPUT,DMCB,AFREE   PUT RECORD TO WORKER FILE                    
*                                                                               
GILX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE REPLACES THE PUTTMP CALL THAT WAS MADE IN THE OLD DAYS           
* WITH A WRKPUT CALL.  THIS IS A RESULT OF THE CHANGE FROM USING                
* THE TEMPSTR TEMP FILE TO USING NWRK FILES.                                    
***********************************************************************         
PUTTMPL  NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R3,0(R1)         R2 = A(OBJ)  R3 = LENGTH                     
*                                                                               
         L     RF,AFREE            RF = A(MVS TYPE RECORD)                      
*                                                                               
         LA    RE,8(R3)            PUT LENGTH IN HEADER                         
         STH   RE,0(RF)                                                         
         XC    2(2,RF),2(RF)                                                    
*                                                                               
         MVC   4(4,RF),=F'9999'    PUT FAKE TYPE IN FIRST 4 BYTES               
*                                                                               
         LA    R0,8(RF)            PUT DATA AFTER TYPE NUMBER                   
         LR    R1,R3                                                            
         MVCL  R0,R2                                                            
*                                                                               
         GOTO1 WRKPUT,DMCB,AFREE   PUT RECORD TO WORKER FILE                    
*                                                                               
PTLX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE REPLACES THE GETTMP CALL THAT WAS MADE IN THE OLD DAYS           
* WITH A WRKGET CALL.  THIS IS A RESULT OF THE CHANGE FROM USING                
* THE TEMPSTR TEMP FILE TO USING NWRK FILES.                                    
***********************************************************************         
GETTMPL  NTR1  BASE=*,LABEL=*                                                   
         L     R2,0(R1)            R2 = A(OBJ)                                  
*                                                                               
         GOTO1 WRKGET,DMCB,AFREE   GET AND THROW AWAY MAD SCHED OBJ             
*                                                                               
         L     RF,AFREE            IF END OF WORKER FILE                        
         CLC   0(8,RF),=C'*EOFEOF*'                                             
         BNE   GTL10                                                            
         MVI   EOTFLAG,C'Y'        THEN SET FLAG AND RETURN                     
         B     GTLX                                                             
*                                                                               
GTL10    GOTO1 WRKGET,DMCB,AFREE   GET SCHEDULE OBJECT                          
*                                                                               
         L     RF,AFREE            RF = A(MVS TYPE RECORD)                      
*                                                                               
         LH    R1,0(RF)            ELSE GET LENGTH FROM HEADER                  
         S     R1,=F'8'            SUBTRACT HEADER AND TYPE LENGTHS             
*                                                                               
         LA    R0,8(RF)            MOVE DATA THAT FOLLOWS TYPE NUMBER           
         LR    R3,R1                   INTO OBJECT                              
         MVCL  R2,R0                                                            
*                                                                               
GTLX     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE ALTERS THE BUYLINE TABLE ENTRY POINTED BY AENTRY.                
***********************************************************************         
CHGBTBL  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,TRCEELEM                                                      
         USING BTRCELEM,R3                                                      
         L     R2,AENTRY           SAVE BUYLINE TABLE INFORMATION               
         USING BLND,R2                                                          
         MVC   BLNSEQ,BTRCSEQN                                                  
         MVC   BLNDATE,BTRCDATE                                                 
         MVC   BLNTIME,BTRCTIME                                                 
         MVC   BLNMAS,BPRD                                                      
         MVC   BLNPIG,BINPBPRD                                                  
*                                                                               
CHGBX    XIT1                      RETURN TO CALLER                             
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ADD STATION TO LIST OF STATIONS ADDED TO OR CHANGED IN BUY FILE     *         
* OUTPUT : R1=A(STATION LIST ENTRY)                                   *         
***********************************************************************         
         SPACE 1                                                                
ADDSTA   NTR1  BASE=*,LABEL=*                                                   
         LA    R1,LSTALIST                                                      
         LA    RF,MAXSTA                                                        
*                                                                               
ADDSTA2  OC    0(L'LSTALIST,R1),0(R1)                                           
         BNZ   *+14                                                             
         MVC   0(L'BMKTSTA,R1),BMKTSTA                                          
         B     ADDSTAX                                                          
         CLC   BMKTSTA,0(R1)                                                    
         BE    ADDSTAX                                                          
         LA    R1,L'LSTALIST(R1)                                                
         BCT   RF,ADDSTA2                                                       
*                                                                               
ADDSTAX  XIT1                                                                   
         EJECT                                                                  
*==============================================================*                
* ROUTINE TO FIGURE OUT WHICH MARKET TO USE                    *                
*     INPUT : PWMKT(4) = CURRENT MARKET                                         
*             SVOLDMKT = OLD MARKETS                                            
*     OUTPUT: PWMKT(4) = VALID PW MARKET                                        
*==============================================================*                
         SPACE 1                                                                
CHKMKT   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SVOLDMKT,SVOLDMKT   TEST ANY OLD MARKET NUMBER                   
         BZ    CHKMKTX             NO - EXIT                                    
*                                                                               
         XC    KEY,KEY             CHECK OLD MARKET FIRST                       
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMED     A-M                                          
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD         USE BRAND NOT POL                            
         MVC   PWKEST,BEST                                                      
         MVC   PWKMKT,SVOLDMKT                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   CHKMKT20            NO - TRY SECOND OLD MARKET                   
*                                                                               
         GOTO1 GETREC                                                           
         BAS   RE,CHKDOLS                                                       
         BZ    CHKMKT20            NO LOCKED DOLLARS, TRY AGAIN                 
         MVC   PWKSTA,BMKTSTA+2    MOVE STATION TO KEY                          
*                                                                               
         GOTO1 HIGH                TEST STATION IN THIS MARKET TOO              
         CLC   KEY(12),KEYSAVE                                                  
         BE    CHKMKT30            YES- USE THIS MARKET                         
*                                                                               
CHKMKT20 OC    SVOLDMKT+2(2),SVOLDMKT+2   TEST SECOND OLD MKT                   
         BZ    CHKMKTX                    NO - DONE                             
         XC    KEY,KEY                    ELSE TRY AGAIN                        
         MVC   KEY(7),KEYSAVE             TYPE/A-M/CLT/PRD/EST                  
         MVC   PWKMKT,SVOLDMKT+2                                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   CHKMKTX             IF NOT, USE ORIGINAL MARKET                  
         GOTO1 GETREC                                                           
         BAS   RE,CHKDOLS                                                       
         BZ    CHKMKTX             NO LOCKED DOLLARS, USE CURRENT MKT           
*                                                                               
         MVC   PWKSTA,BMKTSTA+2    MOVE STATION TO KEY                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     TEST THIS STATION WAS LOCKED                 
         BNE   CHKMKTX             NO - USE CURRENT MKT                         
*                                                                               
CHKMKT30 EDIT  PWKMKT,(4,PWMKT),FILL=0                                          
*                                                                               
CHKMKTX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*==============================================================*                
* CHECK FOR LOCKED PW DOLLARS AND RETURN WITH CC NEQ IF FOUND                   
*==============================================================*                
         SPACE 1                                                                
CHKDOLS  NTR1                                                                   
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
*                                                                               
CHKDOL2  CLI   0(R6),0                                                          
         BE    CHKDOLX                                                          
         CLI   0(R6),6                                                          
         BNE   CHKDOL4                                                          
         USING PWDOLEL,R6                                                       
*                                                                               
         OC    PWDOLWG,PWDOLWG                                                  
         BNZ   CHKDOLNX                                                         
         OC    PWDOLWN,PWDOLWN                                                  
         BNZ   CHKDOLNX                                                         
         OC    PWDOLCG,PWDOLCG                                                  
         BNZ   CHKDOLNX                                                         
*                                                                               
CHKDOL4  SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     CHKDOL2                                                          
*                                                                               
CHKDOLX  CR    RE,RE                                                            
         B     *+6                                                              
CHKDOLNX LTR   RE,RE                                                            
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* ROUTINE TO READ A MARKET RECORD                                               
*     INPUT : PWMKT(4) = MARKET                                                 
*     OUTPUT: MKTNAME                                                           
*==============================================================*                
*                                                                               
GETMKT   NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(14),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED       MEDIA                                        
         MVC   KEY+2(4),PWMKT      MARKET                                       
         MVC   KEY+6(2),SIGNON2C   AGY ALPHA                                    
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         L     R2,AIO                                                           
         CLC   KEY(8),0(R2)                                                     
         BNE   GMXNO                                                            
GMXYES   SR    RC,RC                                                            
GMXNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*==============================================================*                
* ROUTINE TO READ PW STATUS RECORD                             *                
*==============================================================*                
         SPACE 1                                                                
GETPW    NTR1  BASE=*,LABEL=*                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
         MVC   PWKTYP,=X'0D7A'                                                  
         MVC   PWKAGMD,BAGYMED     A-M                                          
         MVC   PWKCLT,BCLT                                                      
         MVC   PWKPRD,BPRD         USE BRAND NOT POL                            
         MVC   PWKEST,BEST                                                      
         PACK  DUB,PWMKT                                                        
         CVB   R1,DUB                                                           
         STCM  R1,3,PWKMKT                                                      
         DROP  R6                                                               
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     TEST FOUND                                   
         BNE   GETPWYES            IF NOT, CAN'T BE LOCKED                      
         GOTO1 GETREC                                                           
         USING PWRECD,R6                                                        
         L     R6,AIO                                                           
*                                                                               
         MVC   BYTE,PWGNFLG        MOVE STATUS FLAG                             
         NI    BYTE,X'C0'          DROP ALL BUT BUY LOCKED BITS                 
         OC    PWFLAG,BYTE                                                      
* NOW SEE IF DOLLARS ARE LOCKED IN YET                                          
         LA    R6,PWEL             POINT TO FIRST ELEMENT                       
GETPW6   CLI   0(R6),0                                                          
         BE    GETPW10                                                          
         CLI   0(R6),6                                                          
         BE    GETPW8                                                           
         ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     GETPW6                                                           
GETPW8   OI    PWFLAG,X'20'        SET DOLLARS LOCKED FLAG                      
*                                                                               
GETPW10  TM    PWFLAG,X'C0'                                                     
         BNZ   GETPWNO                                                          
GETPWYES SR    RC,RC                                                            
GETPWNO  LTR   RC,RC                                                            
         XIT1                                                                   
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* ROUTINE TO CHECK IF THE DEMO BOOKS ARE AVAILABLE FOR THE STATION              
***********************************************************************         
CHKDMBKS NTR1  BASE=*,LABEL=*                                                   
         LA    R6,BOOKS                                                         
CKDBK10  XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'TP '                                                   
         L     R0,AIO                                                           
         ST    R0,DBAREC                                                        
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBSELMED,HDRMED                                                  
         MVC   DBSELSRC,HDRSERV                                                 
         MVC   DBSELSTA,SCHSTA                                                  
         MVC   DBSELALF,ALPHMRKT    ALPHA MARKET FROM MARKET RECORD             
         OC    DBSELALF,SPACES                                                  
         MVC   DBSELAGY,SIGNON2C                                                
         MVC   DBSELBK,0(R6)                                                    
***      MVC   DBBTYPE,             NOT BLACK, HISPANIC, OR OLYMPIC             
         MVI   DBFUNCT,DBGETTLB                                                 
*                                                                               
         GOTO1 DEMAND,DMCB,DBLOCK,0,0                                           
         CLI   DBERROR,0           TEST FOR DEMAND ERRORS                       
         BNE   CKDBKNO                                                          
*                                                                               
         LA    R6,2(R6)            NEXT BOOK                                    
         LA    R0,BOOKS+L'BOOKS                                                 
         CR    R6,R0               PAST ALL 4 BOOKS?                            
         BNL   CKDBKYES            YES                                          
         OC    0(2,R6),0(R6)       NO, ANY BOOK HERE?                           
         BNZ   CKDBK10                 YES, CHECK THIS ONE TOO                  
*                                                                               
CKDBKYES SR    RC,RC                                                            
CKDBKNO  LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
***********************************************************************         
INIT     NTR1  BASE=*,LABEL=*                                                   
         MVI   SETSENT,C'N'                                                     
*                                  INITIALIZE SYSTEM                            
         GOTO1 SETSYS,DMCB,=C'SPOT',=CL8'SPTDIR',=CL8'SPTFIL'                   
         BNE   INXNO                                                            
*                                                                               
INXYES   SR    RC,RC                                                            
INXNO    LTR   RC,RC                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* EXTENSION ROUTINES                                                  *         
***********************************************************************         
         SPACE 1                                                                
EXTRA    NMOD1 0,TA0C06X*,RA                                                    
         L     RC,ACONTROL                                                      
         SRL   RF,24                                                            
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
*                                                                               
         B     BLDWKS                                                           
         B     BUYCHG                                                           
         B     BUYDESC                                                          
         B     BUYDATE                                                          
         B     BLDDEMO                                                          
         B     BLDCOM                                                           
         B     BLDORB                                                           
         B     COSTCHA                                                          
         B     SPOTCHG                                                          
         B     POOLSPT                                                          
         B     NPOOLSPT                                                         
         B     NPWSPT                                                           
         B     RECUPA                                                           
         B     ADDREQ                                                           
         B     BLDBLTAB                                                         
         B     BLDGFACT                                                         
         B     BLDSPTL                                                          
         B     BLDPIGS                                                          
         B     BLDBKEY                                                          
         B     MOVKIO2                                                          
         B     WRITBREC                                                         
*                                                                               
XIT2     XIT1  ,                                                                
*                                                                               
YES2     CLI   *+1,0               SET CC=EQ                                    
         B     XIT2                                                             
*                                                                               
NO2      CLI   *,0                 SET CC=NEQ                                   
         B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
* BUILD WEEKS TABLE                                                   *         
* TABLE ENTRY FOR EACH WEEK IN CAMPAIGN PERIOD :                      *         
*    +0(1) X'FF' END-OF-TABLE                                         *         
*    +0(2) #SPOTS                                                     *         
*    +2(2) START DATE                                                 *         
* INPUT  : R1 = LBADD WHEN BUILDING NEW BUY RECORD                    *         
*               LBCHA WHEN CHANGING EXISTING BUY RECORD               *         
*               LSPOTS=SPOTS/WEEK ARRAY                               *         
* OUTPUT : LSPTOT  = TOTAL NUMBER OF SPOTS                            *         
*          LAWKEN  = A(LAST WEEK IN TABLE WITH NON-ZERO SPOTS)        *         
*          LAWKST  = A(FIRST WEEK IN TABLE WITH NON-ZERO SPOTS)       *         
*          LNSPWKS = NUMBER OF WEEKS WITH SPOTS                       *         
* CODE FOR OUT OF WEEK ROTATORS WAS REMOVED                           *         
***********************************************************************         
         SPACE 1                                                                
BLDWKS   STC   R1,LBADDCHA         SAVE ADD/CHANGE INDICATOR                    
         MVI   LDAYDSPL,0                                                       
*                                                                               
* TIM FORCES ORBIT START DAY TO MONDAY.  WE WILL COMMENT THIS                   
* OUT AND TEST IT WITH GRANT'S HELP.                                            
*                                                                               
*        TM    SCHINDS,SCHIORB     TEST ORBIT                                   
*        BO    BLDW2               YES - FIRST DAY = MONDAY                     
         SR    RE,RE               FIND FIRST DAY OF SCHEDULE WEEK              
         SR    RF,RF                                                            
         ICM   RF,8,SCHDAY                                                      
         SLDL  RE,1                                                             
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,1,ESTOWKSD                                                    
         BZ    BLDW1               ITS NOT AN OUT-OF-WEEK ROTATOR               
*                                                                               
* FOR AN OUT-OF-WEEK ROTATOR-REARRANGE THE DAY BITS AS THOUGH BIT               
* PATTERN STARTED ON THE OUT-OF-WEEK DAY IN THE ESTIMATE HEADER                 
*                                                                               
         SH    R1,=H'8'                                                         
         LPR   R1,R1               ISOLATE BITS STARTING WITH                   
         SR    RF,RF               OUT-OF-WEEK DAY                              
         IC    RE,SCHDAY           GET SLINE DAY BIT MASK                       
         SRDL  RE,1                                                             
         BCT   R1,*-4                                                           
         LR    R1,RF               SAVE BIT PATTERN STARTING AT OWK DAY         
         SR    RF,RF               SHIFT BITS PRECEDING OWK DAY TO              
         SRDL  RE,7                RIGHTMOST POSITIONS                          
         OR    RF,R1               ATTACH THESE BITS TO RIGHT OF                
         SR    R1,R1               BITS STARTING AT OWK DAY                     
*                                                                               
BLDW1    SLDL  RE,1                                                             
         LTR   RE,RE                                                            
         BNZ   *+12                                                             
         LA    R1,1(R1)                                                         
         B     *-14                                                             
         STC   R1,LDAYDSPL         SAVE DAY DISPLACMENT (0=MO,1=TU,ETC)         
*                                                                               
BLDW2    DS    0H                  SET EFFECTIVE DATES                          
*                                                                               
BLDW4    XC    LWKTAB,LWKTAB                                                    
         LA    R2,LWKTAB                                                        
         LA    RE,1                SET RE,RF FOR BXLE                           
         ZIC   RF,BNUMWKS                                                       
         LA    RF,LSPOTS-1(RF)     RF=BXLE LIMIT                                
         LA    R6,LSPOTS           R6=A(SCHEDULED SPOTS)                        
         LA    R5,SCHDATS          R5=A(BROADCAST WEEK CALENDAR)                
         XC    LSPTOT,LSPTOT                                                    
         XC    LAWKST,LAWKST                                                    
         XC    LAWKEN,LAWKEN                                                    
         MVI   LNSPWKS,0                                                        
*                                                                               
BLDW6    CLI   0(R6),0             TEST FOR ANY SPOTS THIS WEEK                 
         BE    BLDW8               NO                                           
         ZIC   R1,LNSPWKS          AUGMENT NUMBER OF WEEKS WITH SPOTS           
         LA    R1,1(R1)                                                         
         STC   R1,LNSPWKS                                                       
         ST    R2,LAWKEN                                                        
         MVC   1(1,R2),0(R6)       NUMBER OF SPOTS                              
         L     R1,LSPTOT                                                        
         AH    R1,0(R2)                                                         
         ST    R1,LSPTOT           ACCUMULATE TOTAL                             
         OC    LAWKST,LAWKST                                                    
         BNZ   BLDW8                                                            
         ST    R2,LAWKST           A(FIRST ENTRY WITH NON-ZERO SPOTS)           
*                                                                               
BLDW8    CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BE    BLDW9               YES-USE ACTUAL DATE                          
*                                                                               
         SR    R1,R1               NO-TEST DAYS START ON MONDAY                 
         ICM   R1,1,LDAYDSPL                                                    
         BZ    BLDW9               YES-USE MONDAY DATE                          
         ST    R1,DMCB+8           NO-CALCULATE START DATE                      
         STM   RE,RF,DUB                                                        
         GOTO1 DATCON,DMCB,(2,(R5)),WORK                                        
         GOTO1 ADDAY,DMCB,WORK,WORK                                             
         GOTO1 DATCON,DMCB,(0,WORK),(2,2(R2))                                   
         LM    RE,RF,DUB                                                        
         B     BLDW10                                                           
*                                                                               
BLDW9    MVC   2(2,R2),0(R5)                                                    
*                                                                               
BLDW10   LA    R2,4(R2)            NEXT WEEK TABLE                              
         LA    R5,4(R5)            NEXT BROADCAST WEEK START/END                
         BXLE  R6,RE,BLDW6                                                      
         MVI   0(R2),FF            MARK END OF TABLE                            
*                                                                               
* EXTEND THE WEEKS TABLE IF SCHEDULE HAS BEEN CUT BACK SINCE LAST               
* LAST TRANSFER                                                                 
*                                                                               
BLDW12   CLC   BPRVWKS,BNUMWKS     TEST IF SCHEDULE CUT BACK                    
         BNH   BLDWX               NO                                           
*                                                                               
         ZIC   R6,BPRVWKS                                                       
         ZIC   R1,BNUMWKS                                                       
         SR    R6,R1               NUMBER OF WEEKS REDUCED                      
*                                                                               
BLDW14   XC    0(2,R2),0(R2)       ZERO SPOTS AREA                              
         MVC   2(2,R2),0(R5)       SET MONDAY IN ENTRY                          
         LA    R2,4(R2)                                                         
         LA    R5,4(R5)                                                         
         BCT   R6,BLDW14                                                        
         MVI   0(R2),FF                                                         
*                                                                               
BLDWX    B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
* COMPARE BUY DESCRIPTION ELEMENT FOR SCHEDULE OBJECT AGAINST BUY     *         
* INPUT  : SCHEDULE OBJECT                                            *         
* OUTPUT : LCHGIND = CHANGE INDICATORS                                *         
***********************************************************************         
         SPACE 1                                                                
BUYCHG   L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
*                                                                               
         CLC   BDSEC,SCHTOTLN      TEST SPOT LENGTH CHANGE                      
         BE    *+8                                                              
         OI    LCHGIND,LSLN                                                     
*                                                                               
*        TM    SCHINDS,SCHIORB     TEST ORBIT                                   
*        BO    BCHG4               YES - DAYS=0 ANYHOW                          
         MVC   BYTE,SCHDAY                                                      
         CLC   BYTE,BDDAY                                                       
         BE    BCHG4                                                            
         OI    LCHGIND,LDAYS       DAYS CHANGE                                  
         GOTO1 DAYUNPK,DMCB,BYTE,(7,WORK)                                       
         GOTO1 DAYUNPK,(R1),BDDAY,(7,WORK+7)                                    
         LA    RE,WORK                                                          
         LA    R0,7                                                             
*                                                                               
BCHG2    CLC   0(1,RE),7(RE)                                                    
         BE    *+12                                                             
         OI    LCHGIND,LSTDATE     START DAY CHANGE                             
         B     BCHG4                                                            
         CLI   0(RE),C'.'                                                       
         BNE   BCHG4                                                            
         LA    RE,1(RE)                                                         
         BCT   R0,BCHG2                                                         
*                                                                               
BCHG4    CLC   BDTIMST(4),SCHSTIME                                              
         BE    BCHG6                                                            
         OI    LCHGIND,LTIMES      TIMES CHANGE                                 
         CLC   SCHSTIME,BDTIMST                                                 
         BH    *+14                                                             
         CLC   SCHNTIME,BDTIMEND                                                
         BNL   BCHG6                                                            
         OI    LCHGIND,LTIMNOTX    TIMES CHANGE - NOT EXPANSION                 
*                                                                               
BCHG6    CLI   HDRCOST,C'N'        TEST COST CHANGES ALLOWED                    
         BE    BCHGX                                                            
         CLC   BDCOST,SCHCOST      TEST CHANGE AGAINST SCHED OBJECT             
         BE    *+8                                                              
         OI    LCHGIND,LCST        COST CHANGE                                  
*                                                                               
BCHGX    B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE BUY DESCRIPTION ELEMENT FOR THE BUY RECORD.           
* BUYDATE IS CALLED TO COMPLETE BUY PERIOD AND WEEK RELATED FIELDS.             
***********************************************************************         
         SPACE 1                                                                
BUYDESC  STC   R1,LBADDCHA         SAVE ADD/CHANGE INDICATOR                    
         XC    DESCELEM,DESCELEM                                                
         LA    R6,DESCELEM         BUILD THE DESCRIPTION ELEMENT USING          
         USING BDELEM,R6               ONE WEEK & # OF WKS METHOD               
         CLI   LBADDCHA,LBADD      TEST ADDING RECORD                           
         BE    BUYD2               YES                                          
*                                                                               
* FOR CHANGE, COPY IN EXISTING BUY DESCRIPTION ELEMENT                          
*                                                                               
         L     RE,AIO                                                           
         ZIC   R1,BDLEN-BUYREC(RE)                                              
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BDELEM(0),BDELEM-BUYREC(RE)                                      
*                                                                               
BUYD2    MVI   BDCODE,X'01'                                                     
         MVI   BDLEN,NDELEM-BDELEM                                              
         MVI   BDINPUT,2                                                        
*                                                                               
         MVC   BDDAY,SCHDAY        STORE DATA INTO DESCRIPTION ELEMENT          
         CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BE    *+10                YES-DON'T PRESET DAY NUMBERS                 
         MVC   BDSEDAY,SCHDAYNM    SET DAY NUMBERS                              
         MVC   BDWKS,BNUMWKS                                                    
         MVC   BDSEC,BSPOTLEN                                                   
         MVC   BDPROGRM,SCHPROG    NO PROGRAM=SPACES                            
         MVC   BDNTAX,BTAXRATE                                                  
         MVC   BDREP,ESTREPCD                                                   
         OC    REP,REP             TEST FOR UPLOAD SPECIAL REP                  
         BZ    *+10                                                             
         MVC   BDREP,REP           YES                                          
         MVC   BDTIMST(L'BDTIMST+L'BDTIMEND),SCHSTIME                           
*                                                                               
* ADJUST 5AM BASED PC TIMES TO 0-2400 MAINFRAME SCHEME                          
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,BDTIMST        GET START TIME                               
         LA    R0,2400             R0=2400                                      
         CR    R1,R0                                                            
         BNH   *+6                                                              
         SR    R1,R0               ADJUST 1201A-5A                              
         STCM  R1,3,BDTIMST                                                     
*                                                                               
         ICM   R1,3,BDTIMEND                                                    
         CR    R1,R0                                                            
         BNH   *+6                                                              
         SR    R1,R0                                                            
         STCM  R1,3,BDTIMEND                                                    
*                                                                               
         GOTO1 ABUYDATE                                                         
         BNE   NO2                                                              
*                                                                               
         CLI   LBADDCHA,LBCHA      TEST FOR CHANGE                              
         BE    *+14                                                             
         MVC   BDCOST,LBUYCOST+1                                                
         B     BUYD4                                                            
*                                                                               
         TM    LCHGIND,LCST        TEST FOR COST CHANGE                         
         BZ    BUYD4               NO                                           
         GOTO1 ACOSTCHA                                                         
*                                                                               
BUYD4    TM    BITFLAG,X'80'       TEST IF POOL ESTIMATE                        
         BO    BUYD5                                                            
         CLI   BINPBPRD,0          TEST IF PIGGYBACK                            
         BE    BUYD5               NO                                           
         MVC   BDTIME,SCHMASLN                                                  
         MVC   BDCOSTP,SCHMASLN                                                 
*                                                                               
BUYD5    MVI   BDCIND,X'20'        PLAIN SPOTS = CASH SPOTS = DEFAULT           
         MVI   BDCIND2,0                                                        
*                                                                               
         MVC   BYTE,RATE                                                        
         CLI   BYTE,C'0'           TEST FOR HEADER RATE TYPE OVERRIDE           
         BH    BUYD6               YES                                          
         CLI   ESTRATE,C'*'        TEST ESTIMATE SAYS NO RATE TYPE              
         BE    BUYD10                                                           
         MVC   BYTE,ESTRATE        SET RATE TYPE                                
         CLI   ESTRATE,0                                                        
         BNE   *+10                                                             
         MVC   BYTE,CPROFILE+14                                                 
*                                                                               
BUYD6    LA    RE,RATETYPS         RE=A(RATE TYPE TABLE)                        
*                                                                               
BUYD8    CLI   0(RE),0             TEST FOR EOT                                 
         BE    BUYD10                                                           
         CLC   BYTE,0(RE)          MATCH ON RATE TYPE                           
         BE    BUYD9                                                            
         LA    RE,3(RE)            NEXT TABLE ENTRY                             
         B     BUYD8                                                            
*                                                                               
BUYD9    MVC   BDCIND,1(RE)        SET COST INDICATORS FROM TABLE               
         MVC   BDCIND2,2(RE)                                                    
*                                                                               
BUYD10   CLI   LBADDCHA,LBADD                                                   
         BNE   *+8                                                              
         MVI   BDWHY,X'80'         NEW BUY CREATED TODAY                        
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(3,BDCHG)                                      
         OI    BDSTAT,X'08'        SO MEL KNOWS IT'S $MAD RADIO BUY             
*                                                                               
         CLI   KEYPRD,X'FF'        IF POOL                                      
         BNE   BUYD12                                                           
         CLI   BPRD,X'FF'          TEST PRODUCT=POL(TRUE POOL)                  
         BE    BUYD12              YES-SKIP MASTER ALLOCATION                   
*                                  NO-THEN SET POOL MASTER PRODUCT CODE         
         MVC   BDMASPRD(1),PRDCODE+1                                            
         MVC   BDMASPRD+1(1),BINPBPRD                                           
*                                                                               
BUYD12   MVC   BDDAYPT,SCHDYPRT                                                 
         TM    BITFLAG,X'40'       TEST BRAND POL NPW                           
         BZ    *+8                 NO                                           
         OI    BDSTAT,X'80'        THEN SET FLAGS TO USE HIGH ORDER 5           
         CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BNE   *+8                                                              
         OI    BDSTAT2,X'80'       DAILY SKED BIT                               
         TM    AFLAG1,X'02'        TEST TRADE AGENCY (NOT CTA)                  
         BZ    *+8                                                              
         OI    BDSTAT2,X'20'       SET TRADE AGENCY BUY                         
*                                                                               
BUYDX    B     YES2                RETURN 'YES' TO CALLER                       
         DROP  R6                                                               
*                                                                               
RATETYPS DC    C'1',X'0400'        S                                            
         DC    C'2',X'8000'        F                                            
         DC    C'3',X'1000'        N                                            
         DC    C'4',X'4000'        Q                                            
         DC    C'5',X'0800'        V                                            
         DC    C'6',X'0200'        X                                            
         DC    C'7',X'0000'        P                                            
         DC    C'8',X'2080'        C                                            
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD DATE RELATED BUY RECORD FIELDS                                *         
* INPUT  : LAWKST   = A(FIRST ENTRY IN WEEKS TABLE)                   *         
*          LAWKEN   = A(LAST ENTRY IN WEEKS TABLE)                    *         
*          LNSPWKS  = N'WEEKS WITH SPOTS                              *         
*          AIO      = A(BUY RECORD), WITH UPDATED BDELEM              *         
* EXIT   : CC=EQ IF OK, CC=NEQ IF NOT OK                              *         
***********************************************************************         
         SPACE 1                                                                
BUYDATE  LA    R2,DESCELEM                                                      
         USING BDELEM,R2                                                        
*                                                                               
* SET BDSTART - START OF BUY PERIOD                                             
*                                                                               
         OC    LAWKST,LAWKST       PROTECT AGAINST NO SPOTS                     
         BZ    BDATX               (COULD HAPPEN ON RE-TRANSFER)                
         L     R5,LAWKST                                                        
         GOTO1 DATCON,DMCB,(2,2(R5)),(3,BDSTART)   BUY START DATE               
         GOTO1 (RF),(R1),(3,BDSTART),WORK            WORK=START DATE            
*                                                                               
* FOR DAILY SCHEDULING, ADJUST START DATE TO START DAY(BDDAY) IF NEEDED         
*                                                                               
         CLI   DAILY,C'Y'          TEST DAILY SKED                              
         BNE   BDAT4                                                            
         GOTO1 GETDAY,(R1),WORK,WORK+6      YES-GET DAY-OF-WEEK                 
         CLC   WORK+6(3),SPACES                OF START DATE                    
         BH    *+6                                                              
         DC    H'0'                                                             
         ZIC   R3,DMCB                                                          
*                                                                               
BDAT1    SR    R1,R1               GET DAY-OF-WEEK OF FIRST DAY OF DAYS         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         ICM   RF,8,BDDAY                                                       
         BNZ   BDAT2                                                            
         DC    H'0'                                                             
*        TM    SCHINDS,SCHIORB     TEST ORBIT                                   
*        BO    *+6                                                              
*        DC    H'0'                                                             
*        LA    R1,1                YES-FIRST DAY ALWAYS = MONDAY                
*        B     BDAT3                                                            
*                                                                               
BDAT2    SLDL  RE,1                FIND NUMBER OF LOWEST DAY(ON BIT)            
         LTR   RE,RE                                                            
         BNZ   BDAT3                                                            
         LA    R1,1(R1)            RESULT IS IN R1                              
         B     BDAT2                                                            
*                                                                               
BDAT3    SR    R1,R3               TEST IT'S THE SAME DAY                       
         BNM   BDAT4                                                            
         ST    R1,DMCB+8           NO-BRING START DATE BACK                     
         GOTO1 ADDAY,DMCB,WORK,WORK+6                                           
         GOTO1 DATCON,(R1),WORK+6,(3,BDSTART)                                   
         MVC   WORK(6),WORK+6                                                   
         CLC   WORK(6),ESTSTRT     TEST BEFORE ESTIMATE START                   
         BL    BDAT99              YES-ABORT                                    
*                                                                               
* SET BDEND - END OF BUY PERIOD                                                 
* FIRST FIND MONDAY OF LAST WEEK IN SCHEDULE WITH SPOTS                         
*                                                                               
BDAT4    MVC   DUB(6),SCHSTMON     FIND BUY END DATE                            
         CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BNE   *+10                                                             
         MVC   DUB(6),HDRSDATE     YES-USE SHEAD START DATE                     
         L     R5,LAWKEN           R5=A(LAST WEEK TABLE ENTRY)                  
         LA    RE,LWKTAB           RE=A(WEEK TABLE START)                       
         LR    R3,R5               R3=A(LAST WEEK TABLE ENTRY)                  
         SR    R3,RE               COMPUTE INDEX INTO TABLE                     
         BZ    BDAT4A              ALL DONE IF ZERO OR FIRST WEEK               
*                                                                               
         LA    R3,SCHDATS(R3)      WEEK TABLE LEN=SCHED DATES TAB LEN           
         GOTO1 DATCON,DMCB,(2,0(R3)),(0,DUB)                                    
*                                                                               
BDAT4A   CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BNE   BDAT5               NO                                           
*                                                                               
* FOR DAILY SCHEDULING, ADJUST END DATE TO END DAY(BDDAY) IF NEEDED             
*                                                                               
         GOTO1 GETDAY,DMCB,DUB,WORK+6                                           
         CLC   WORK+6(3),SPACES                                                 
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R1,7                R1=END DAY NUMBER                            
         SR    RF,RF               CLEAR RF FOR BIT TESTING                     
         ZIC   RE,BDDAY                                                         
         SRDL  RE,1                                                             
         LTR   RF,RF               TEST FOR HIGHEST DAY ('ON')                  
         BNZ   *+8                                                              
         BCT   R1,*-10                                                          
*                                                                               
         ZIC   RE,DMCB             GET POTENTIAL END DATE DAY                   
         SR    R1,RE               FIND BDDAY END DAY - END DATE DAY            
         BP    *+14                BDDAY END DAY FOLLOWS END DATE DAY           
         MVC   WORK+6(6),DUB       SET END DATE                                 
         B     BDAT8                                                            
*                                                                               
         ST    R1,DMCB+8           BRING END DATE FORWARD                       
         GOTO1 ADDAY,DMCB,DUB,WORK+6                                            
         B     BDAT8               TO AGREE WITH END DAY                        
*                                                                               
* FIND DAY NUMBER-1 OF LAST DAY IN BDDAY                                        
*                                                                               
BDAT5    LA    R1,6                SET LOOP COUNTER TO SUNDAY-1                 
*        TM    SCHINDS,SCHIORB                                                  
*        BO    BDAT6                                                            
         SR    RE,RE                                                            
         SR    RF,RF               CLEAR RF FOR BIT TESTING                     
         ICM   RE,1,BDDAY                                                       
         SR    R0,R0                                                            
         ICM   R0,1,ESTOWKSD                                                    
         BZ    BDAT6                                                            
*                                                                               
* FOR AN OUT-OF-WEEK ROTATOR, RE-ARRANGE DAY BITS AS THOUGH BIT MASK            
* STARTED WITH OUT-OF-WEEK DAY                                                  
*                                                                               
         SH    R0,=H'8'                                                         
         LPR   R0,R0                                                            
         SRDL  RE,1                                                             
         BCT   R0,*-4                                                           
         LR    R0,RF                                                            
         SR    RF,RF                                                            
         SRDL  RE,7                                                             
         OR    RF,R0                                                            
         SLDL  RE,7                                                             
         SR    RF,RF                                                            
*                                                                               
BDAT6    SRDL  RE,1                CALCULATE END DATE                           
         LTR   RF,RF               SHIFT OVER RIGHTMOST BIT                     
         BNZ   BDAT7               AND TEST IF ON                               
         BCT   R1,*-10                                                          
         MVC   WORK+6(6),DUB       MONDAY IS LAST DAY                           
         B     BDAT8                                                            
*                                                                               
BDAT7    ST    R1,DMCB+8           SET NUMBER OF DAYS TO ADD TO MONDAY          
         GOTO1 ADDAY,DMCB,DUB,WORK+6                                            
*                                                                               
BDAT8    GOTO1 DATCON,DMCB,WORK+6,(3,BDEND)   END DATE                          
         CLI   DAILY,C'Y'          TEST FOR DAILY SCHEDULING                    
         BE    BDAT22                                                           
*                                                                               
* TEST IF CALCULATED BDEND IS PAST SCHEDULE END                                 
*                                                                               
         CLC   BDEND,SCHENDB       NO-TEST END AFTER SCHEDULE END               
         BNH   *+10                                                             
         MVC   BDEND,SCHENDB       YES-SET END TO SCHEDULE END                  
*                                                                               
         L     R4,LAWKST           R4 INITIALIZED TO 1ST WEEK W SPOTS           
         MVI   BYTE,0              INITIALIZE SPOTS/WEEK                        
         MVI   FLAG,0              INITIALIZE INTERVAL BETWEEN WEEKS            
*                                                                               
* WALK WEEK TABLE TO SEE IF SAME NUMBER OF SPOTS PER WEEK APPEARS               
* WITH SAME INTERVAL BETWEEN WEEKS WITH SPOTS                                   
*                                                                               
BDAT9    CLI   1(R4),0             TEST IF ANY SPOTS IN WEEK                    
         BE    BDAT12              NO                                           
         CLI   BYTE,0              BYTE = SPOTS/WEEK                            
         BNE   *+14                                                             
         MVC   BYTE,1(R4)          FIRST WEEK WITH SPOTS                        
         B     BDAT11                                                           
         CLC   BYTE,1(R4)          TEST FOR CHANGE IN SPOTS/WEEK                
         BNE   BDAT14              YES                                          
         LR    R0,R4               NO-COMPUTE INTERVAL BETWEEN WEEKS            
         S     R0,FULL             DISTANCE BETWEEN WEEK TABLE ENTRIES          
         SRL   R0,2                / LENGTH OF WEEK TABLE ENTRY                 
         CLI   FLAG,0              FLAG = FREQUENCY                             
         BNE   BDAT10                                                           
         STC   R0,FLAG             SET FIRST FREQUENCY                          
         B     BDAT11                                                           
*                                                                               
BDAT10   CLM   R0,1,FLAG           TEST FOR CHANGE IN FREQUENCY                 
         BNE   BDAT14              YES                                          
*                                                                               
BDAT11   ST    R4,FULL             SAVE A(LAST WEEK WITH SPOTS)                 
*                                                                               
BDAT12   LA    R4,4(R4)            NEXT WEEK                                    
         C     R4,LAWKEN           TEST IF PAST LAST WEEK W SPOTS               
         BNH   BDAT9               NO                                           
*                                                                               
* ONLY FALL THROUGH IF SAME NUMBER OF SPOTS APPEARS WITH SAME                   
* FREQUENCY (INTERVAL BETWEEN WEEKS)                                            
*                                                                               
         CLI   FLAG,2              TEST FOR FREQUENCY = 2-4                     
         BL    BDAT14                                                           
         CLI   FLAG,4                                                           
         BH    BDAT14                                                           
         ZIC   RF,FLAG                                                          
         BCTR  RF,0                SUBTRACT 2 FROM FREQUENCY                    
         BCTR  RF,0                                                             
         LA    RE,=C'ATF'                                                       
         AR    RE,RF               INDEX INTO FREQUENCY INDICATORS              
         MVC   BDWKIND,0(RE)       WEEK INDICATOR - A/T/F                       
         MVC   BDWKS,LNSPWKS       ACTUAL NUMBER OF WEEKS                       
         B     BDAT16                                                           
*                                                                               
BDAT14   MVI   BDWKIND,C'O'        NO A/T/F PATTERN -                           
         ICM   RE,15,LAWKEN        SET BDWKS TO OVERALL NUMBER OF WEEKS         
         BZ    BDAT16                                                           
         S     RE,LAWKST           DISP BETWEEN LAST/FIRST WK W SPTS            
         SRL   RE,2                / LENGTH OF WEEK ENTRY                       
         LA    RE,1(RE)                                                         
         STC   RE,BDWKS                                                         
*                                                                               
* FIND MOST FREQUENT NUMBER OF SPOTS/WEEK                                       
*                                                                               
BDAT16   L     R4,LAWKST           R4=A(FIRST WEEK W SPOTS)                     
         SR    R3,R3               R3=NUMBER OF SPOTS                           
         SR    R5,R5               R5=CUMULATIVE NUMBER OF SPOTS                
         XC    ELEMENT,ELEMENT     TABLE OF SPOTS/WK                            
*                                                                               
BDAT18   IC    R3,1(R4)            GET SPOTS/WEEK                               
         IC    R5,ELEMENT(R3)      INDEX INTO FREQUENCY TABLE                   
         LA    R5,1(R5)            INCREMENT NUMBER OF OCCURRENCES              
         STC   R5,ELEMENT(R3)                                                   
         LA    R4,4(R4)            NEXT WEEK ENTRY                              
         C     R4,LAWKEN                                                        
         BNH   BDAT18                                                           
*                                                                               
         LA    R1,ELEMENT          R1=A(HIGHEST FREQUENCY WEEK)                 
         LA    RE,ELEMENT+1        RE=TABLE INDEX REGISTER                      
         LA    RF,99               RF=LOOP COUNTER                              
         MVI   BYTE,0              MAXMIMUM OCCURRENCES                         
*                                                                               
BDAT20   CLC   BYTE,0(RE)          TEST FOR NEW MAXIMUM                         
         BNL   *+12                                                             
         MVC   BYTE,0(RE)          YES-SAVE NUMBER                              
         LR    R1,RE               AND SAVE POINTER TO ENTRY                    
         LA    RE,1(RE)            NEXT WEEK                                    
         BCT   RF,BDAT20                                                        
         LA    RE,ELEMENT          DISP OF MAXIMUM OCCURRENCE WEEK              
         SR    R1,RE               FROM START OF TABLE =                        
         STC   R1,BDNOWK           NUMBER OF SPOTS PER WEEK                     
         B     BDATX                                                            
*                                                                               
* DAILY SCHEDULING - FIRST SET START/END DAY NUMBER BYTE                        
*                                                                               
BDAT22   MVI   BDWKIND,C'O'        DAILY SKED --------------                    
         CLI   BDSEDAY,0           TEST S/E DAYS ALREADY SET                    
         BNE   BDAT24                                                           
*                                                                               
         SR    RE,RE               NO--                                         
         SR    RF,RF                                                            
         ICM   RF,1,BDDAY          TEST DAYS SET YET                            
*        BNZ   BDAT23                                                           
*        TM    SCHINDS,SCHIORB     NO-IF IT'S AN ORBIT,                         
*        BZ    BDAT24                                                           
*        LA    RF,X'7F'            ASSUME M-SU                                  
*                                                                               
BDAT23   LR    R4,RF               SAVE DAY BITS IN R4                          
         SLL   RF,24                                                            
         SR    R1,R1                                                            
         SLDL  RE,1                                                             
         LTR   RE,RE               TEST FOR FIRST 'ON' BIT=START DAY            
         BNZ   *+12                                                             
         LA    R1,16(R1)           HOB/HIGH NIBBLE OF R1 HAS DAY NUMBER         
         B     *-14                                                             
*                                                                               
         LR    R0,R1               SAVE START DAY NUMBER IN R0                  
         LR    RE,R4               RESTORE DAY BITS TO RE                       
         SR    RF,RF               RF=BIT TEST REGISTER                         
         LA    R1,7                R1=END DAY NUMBER                            
         SRDL  RE,1                FIND HIGHEST 'ON' BIT=END DAY                
         LTR   RF,RF                                                            
         BNZ   *+10                                                             
         BCTR  R1,0                                                             
         B     *-12                                                             
         OR    R1,R0               COMBINE START/END DAY NUMBER                 
         STC   R1,BDSEDAY                                                       
*                                                                               
* SET NUMBER OF WEEKS IN BUY LINE                                               
*                                                                               
BDAT24   GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    RE,12(R1)           NUMBER OF DAYS/7                             
         OC    10(2,R1),10(R1)     TEST FOR ANY REMAINDER                       
         BZ    *+8                                                              
         LA    RE,1(RE)            YES-ADD ANOTHER WEEK                         
         STC   RE,BDWKS                                                         
*                                                                               
* BUILD A TABLE OF BROADCAST WEEK DATES FROM SCHEDULE START DATE                
*                                                                               
         L     R4,LAWKST                                                        
         GOTO1 DATCON,DMCB,(2,2(R4)),WORK                                       
         L     R4,LAWKEN                                                        
         GOTO1 (RF),(R1),(2,2(R4)),WORK+6                                       
         XC    BLOCK,BLOCK                                                      
         MVC   BLOCK(4),GETBROAD                                                
         MVC   BLOCK+4(4),ADDAY                                                 
         MVC   BLOCK+8(4),GETDAY                                                
         MVC   BLOCK+12(4),DATCON                                               
         CLI   ESTOWKSD,0                                                       
         BE    *+10                                                             
         MVC   BLOCK+24(1),ESTOWKSD                                             
         GOTO1 MOBILE,(R1),(10,WORK),(4,WORK+12),BLOCK,BLOCK+16                 
*                                                                               
* WORK OUT FROM THE SPOTS/DAY, THE NUMBER OF SPOTS IN EACH WEEK                 
*                                                                               
BDAT25   LA    R4,WORK+12                                                       
         L     R3,LAWKST                                                        
         LA    R0,10               R0=MAXIMUM NUMBER OF WEEKS                   
         LA    R5,BLOCK            R5=SPOTS/WEEK ARRAY                          
         XC    BLOCK(16),BLOCK                                                  
         SR    RF,RF               RF=ACCUMULATOR FOR SPOTS IN WEEK             
*                                                                               
BDAT26   CLC   2(2,R3),2(R4)       TEST IF DAY IS <= WEEK END DATE              
         BNH   BDAT28              YES-ITS IN THE WEEK                          
         LA    R4,4(R4)            NO-ADVANCE TO NEXT WEEK START/END            
         STC   RF,0(R5)            SAVE SPOTS IN PREVIOUS WEEK                  
         LA    R5,1(R5)            POINT TO NEXT WEEK                           
         SR    RF,RF               ZERO SPOTS ACCUMULATOR                       
         BCT   R0,*+6                                                           
         DC    H'0'                                                             
         CLI   0(R4),X'FF'                                                      
         BNE   BDAT26                                                           
         DC    H'0'                                                             
*                                                                               
BDAT28   ZIC   RE,1(R3)            GET SPOTS FOR TODAY                          
         AR    RF,RE               UPDATE WEEK ACCUMULATOR                      
         LA    R3,4(R3)            NEXT DAY                                     
         C     R3,LAWKEN           TEST IF PAST LAST DAY                        
         BNH   BDAT26              NO                                           
         STC   RF,0(R5)                                                         
*                                                                               
* FIND MOST FREQUENT NUMBER OF SPOTS/WEEK                                       
*                                                                               
BDAT30   LA    R4,BLOCK            R4=A(SPOTS/WEEK ARRAY)                       
         LA    R0,10               R0=LOOP COUNTER                              
         SR    R3,R3               R3=NUMBER OF SPOTS                           
         SR    R5,R5               R5=CUMULATIVE NUMBER OF SPOTS                
         XC    ELEMENT,ELEMENT     TABLE OF SPOTS/WK                            
*                                                                               
BDAT32   IC    R3,0(R4)            GET SPOTS/WEEK                               
         IC    R5,ELEMENT(R3)      INDEX INTO FREQUENCY TABLE                   
         LA    R5,1(R5)            INCREMENT NUMBER OF OCCURRENCES              
         STC   R5,ELEMENT(R3)                                                   
         LA    R4,1(R4)            NEXT WEEK ENTRY                              
         BCT   R0,BDAT32                                                        
*                                                                               
         LA    R1,ELEMENT          R1=A(HIGHEST FREQUENCY WEEK)                 
         LA    RE,ELEMENT+1        RE=TABLE INDEX REGISTER                      
         LA    RF,99               RF=LOOP COUNTER                              
         MVI   BYTE,0              MAXMIMUM OCCURRENCES                         
*                                                                               
BDAT34   CLC   BYTE,0(RE)          TEST FOR NEW MAXIMUM                         
         BNL   *+12                                                             
         MVC   BYTE,0(RE)          YES-SAVE NUMBER                              
         LR    R1,RE               AND SAVE POINTER TO ENTRY                    
         LA    RE,1(RE)            NEXT WEEK                                    
         BCT   RF,BDAT34                                                        
         LA    RE,ELEMENT          DISP OF MAXIMUM OCCURRENCE WEEK              
         SR    R1,RE               FROM START OF TABLE =                        
         STC   R1,BDNOWK           NUMBER OF SPOTS PER WEEK                     
         B     BDATX                                                            
*                                                                               
BDATX    B     YES2                SET CC=EQ                                    
*                                                                               
BDAT99   MVI   BYTE,15             SET ERROR MESSAGE                            
         B     NO2                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE DEMOGRAPHICS ELEMENT FOR THE BUY RECORD.              
***********************************************************************         
         SPACE 2                                                                
BLDDEMO  LA    R6,ELEMENT          BUILD 'ORIGINAL' DEMO ELEMENT                
         USING NDELEM,R6                                                        
         XC    ELEMENT,ELEMENT                                                  
         MVI   NDCODE,X'02'                                                     
*                                                                               
*                                                                               
         LA    RF,ESTDEMOS         COMPUTE NUMBER OF DEMOS IN ESTIMATE          
         CLI   1(RF),0                                                          
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     *-12                                                             
         SR    RE,RE                                                            
         D     RE,=F'3'                                                         
         C     RF,=F'20'                                                        
         BNH   *+8                                                              
         L     RF,=F'20'           RF HAS NUMBER OF DEMOS NOW                   
*                                                                               
*                                                                               
         LR    R1,RF               STORE LENGTH OF OUR DEMO ELEMENT             
         SLL   R1,3                                                             
         LA    R1,NDEMNO-NDELEM(R1)                                             
         STC   R1,NDLEN                                                         
*                                                                               
         OC    NDBOOK,BOOKS        USE FIRST BOOK IN PC HEADER IF THERE         
         BNZ   *+10                                                             
         MVC   NDBOOK,ESTBOOK      DEFAULT TO EST HDR RATING BOOK               
*                                                                               
*                                  BUILD NDEMNO FROM SCHED DEMOS                
         GOTO1 BLDNDEM,DMCB,NDEMNO                                              
*                                                                               
BDMYES   B     XIT2                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE THE NDEMNO FIELD OF THE BUY DEMO ELEMENT              
* FROM THE SCHED DEMO CATEGORY AND VALUE LIST.  PARAMETER 1 HOLDS THE           
* ADDRESS OF NDEMNO.                                                            
***********************************************************************         
BLDNDEM  NTR1  ,                                                                
         L     R6,0(R1)            R6 = A(NDEMNO)                               
         LA    R5,ESTDEMOS         R5 = A(BRD OR BRD/POL EST DEMOS)             
*                                                                               
BND10    CLI   1(R5),0             WHILE NOT END OF ESTIMATE DEMOS              
         BE    BNDX                                                             
*                                                                               
         XC    FULL,FULL           INIT DEMO VALUE TO ZERO                      
*                                                                               
         CLI   NUMDCATS,0          IF NO DEMOS IN SCHED THEN USE ZEROS          
         BE    BND90                                                            
*                                                                               
         LA    R4,DTYPLIST         R4 = A(SCHED ESTIMATE DEMOS)                 
         ZIC   R3,BNUMWKS          R3 = A(SCHED DEMO VALUES)                    
         LA    R3,SCHDATA(R3)                                                   
         ZIC   R2,NUMDCATS         R2 = # SCHED DEMO CATEGORIES                 
         SR    RF,RF                                                            
         ICM   RF,12,SCHOVERS      RF=DEMO OVERRIDE BIT MASK                    
*                                                                               
BND50    SR    RE,RE               GET BIT POSITION FOR DEMO IN RE              
         SLDL  RE,1                                                             
         CLC   0(3,R5),0(R4)       IF DEMO MATCHES ESTIMATE'S                   
         BE    BND70               THEN USE THE CORRESPONDING VALUE             
*                                                                               
         LA    R4,3(R4)            ELSE BUMP TO NEXT SCHED DEMO                 
         LA    R3,2(R3)                                                         
         BCT   R2,BND50            LOOP BACK                                    
*                                                                               
         B     BND90               USE ZEROS FOR DEMO VALUE                     
*                                                                               
BND70    MVC   FULL+2(2),0(R3)     USE DEMO VALUE FROM SCHED                    
         LTR   RE,RE               TEST FOR 'ON' OVERRIDE BIT                   
         BZ    *+8                 NO                                           
         OI    FULL,X'80'          YES-TURN ON SPOT OVERRIDE BIT                
*                                                                               
*                                  FILL NDEMNO ENTRY FOR THIS DEMO              
BND90    MVC   0(3,R6),0(R5)           DEMO CATEGORY                            
         MVI   3(R6),100               HUT ADJ (ALWAYS 100)                     
         MVC   4(4,R6),FULL            DEMO VALUE                               
*                                                                               
         LA    R5,3(R5)            BUMP TO NEXT EST DEMO                        
         LA    R6,8(R6)            BUMP TO NEXT NDEMNO ENTRY                    
         B     BND10               LOOP BACK                                    
*                                                                               
BNDX     B     XIT2                                                             
         EJECT                                                                  
******************************************************************              
* ROUTINE TO BUILD A 1-3 COMMENT ELEMENTS IN ELEMENT             *              
* BUY PROGRAM HAS A MAXIMUM OF 76 BYTES PER ELEMENT              *              
******************************************************************              
         SPACE 1                                                                
BLDCOM   XC    ELEMENT,ELEMENT                                                  
         CLI   SCHCOMLN,3*MAXCOM                                                
         BNH   *+8                                                              
         MVI   SCHCOMLN,3*MAXCOM                                                
         ZIC   R4,SCHCOMLN         R4=COMMENT LENGTH                            
         LA    R3,MAXCOM           R3=MAX LENGTH OF 1 BUY PROG COMMENT          
         LA    R2,1                R2=COMMENT NUMBER                            
         SR    R5,R5                                                            
         ICM   R5,3,SCHCOMDS       GET DISPLACEMENT TO COMMENT                  
         LA    R5,SCHOBJCT(R5)     R5=A(COMMENT)                                
         LA    R6,ELEMENT          R6=COMMENT ELEMENT POINTER                   
         USING COMELEM,R6                                                       
*                                                                               
BLDCOM2  MVI   CMCODE,X'66'                                                     
         MVI   CMLEN,CMDATA-CMCODE                                              
         STC   R2,CMNUM            SET COMMENT NUMBER                           
         LR    R1,R4               GET LENGTH REMAINING                         
         CR    R1,R3               TEST IF MORE THAN 1 LINE                     
         BNH   *+6                 NO                                           
         LR    R1,R3               YES-TRUNCATE LENGTH                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CMDATA(0),0(R5)                                                  
         EX    R1,*+8              FORCE COMMENT TO BE UPPER CASE               
         B     BLDCOM4                                                          
         OC    CMDATA(0),SPACES                                                 
*                                                                               
BLDCOM4  LA    R1,1(R1)            RESTORE COMMENT LENGTH                       
         ZIC   RF,CMLEN                                                         
         AR    RF,R1               COMPUTE ELEMENT LENGTH                       
         STC   RF,CMLEN                                                         
         LA    R6,CMCODE(RF)       POINT TO NEXT ELEMENT POSITION               
         LA    R5,0(R1,R5)         NEXT COMMENT POSITION                        
         LA    R2,1(R2)            INCREMENT COMMENT NUMBER                     
         SR    R4,R1               REDUCE LENGTH LEFT TO PROCESS                
         BP    BLDCOM2             MORE LEFT TO GO                              
*                                                                               
BLDCOMX  B     XIT2                                                             
         DROP  R6                                                               
         EJECT                                                                  
******************************************************************              
* ROUTINE TO BUILD AN ORBIT ELEMENT FROM THE DAYTIME BLOCK       *              
******************************************************************              
         SPACE 1                                                                
BLDORB   XC    ELEMENT,ELEMENT                                                  
         LA    R6,ELEMENT                                                       
         USING ORBELEM,R6                                                       
         MVI   ORBCODE,X'67'                                                    
         MVI   ORBLEN,4                                                         
         LA    R2,SCHDT            R2=A(DAYTIME BLOCK)                          
         USING DTD,R2                                                           
         LA    R3,ORBDAY           R3=A(OUTPUT POINTER)                         
         ZIC   R4,ORBLEN           R4=CUMULATIVE ELEMENT LENGTH                 
*                                                                               
BLDORB2  CLI   DTDAY,0             TEST FOR END OF BLOCK                        
         BE    BLDORBX             YES                                          
*                                                                               
         MVC   0(1,R3),DTDAY                                                    
         MVC   1(2,R3),DTSTART                                                  
         MVC   3(2,R3),DTEND                                                    
*                                                                               
         SR    R1,R1                                                            
         LA    R0,2400                                                          
         ICM   R1,3,1(R3)          ADJUST THE TIME FROM 5AM BASED TO 0          
         CR    R1,R0                                                            
         BNH   *+6                                                              
         SR    R1,R0                                                            
         STCM  R1,3,1(R3)                                                       
*                                                                               
         ICM   R1,3,3(R3)                                                       
         CR    R1,R0                                                            
         BNH   *+6                                                              
         SR    R1,R0                                                            
         STCM  R1,3,3(R3)                                                       
*                                                                               
         MVC   5(L'ORBDESC,R3),SPACES                                           
         LA    R3,16(R3)                                                        
         LA    R4,16(R4)                                                        
         LA    R2,DTLNQ(R2)                                                     
         ZIC   R1,ORBLEN                                                        
         LA    R1,16(R1)                                                        
         STC   R1,ORBLEN                                                        
         B     BLDORB2                                                          
*                                                                               
BLDORBX  B     XIT2                                                             
         DROP  R2,R6                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHANGE THE COST OF EXISTING BUY RECORD                   *         
* CALLED BY BUYDESC WHICH BUILDS BDELEM AT DESCELEM                   *         
* AT ENTRY, LBUYCOST = NEW COST                                       *         
***********************************************************************         
         SPACE 1                                                                
COSTCHA  MVI   BYTE,0                                                           
         L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
*                                                                               
COST1    LA    R4,BDELEM                                                        
         SR    R0,R0                                                            
*                                                                               
COST2    CLI   0(R4),0                                                          
         BE    COST10                                                           
         CLI   KEYPRD,FF           TEST POL BUY                                 
         BE    COST4                                                            
         CLI   0(R4),6             NO-TEST BUY ELEMENT                          
         BL    COST8                                                            
         CLI   0(R4),8                                                          
         BH    COST8                                                            
         USING REGELEM,R4                                                       
         OC    RPAY,RPAY           YES-TEST PAID                                
         BNZ   COSTX               YES-EXIT WITHOUT COST CHANGE                 
         B     COST8                                                            
*                                                                               
COST4    CLI   0(R4),11            TEST POL BUY ELEMENT                         
         BL    COST8                                                            
         CLI   0(R4),13                                                         
         BH    COST8                                                            
         TM    RSTATUS,X'20'       YES-TEST ALREADY HAS COST OVERRIDE           
         BO    COST8                                                            
         CLI   BYTE,0              NO-TEST LOOKING FOR PAID SPOTS               
         BNE   COST6                                                            
         OC    RPAY,RPAY           YES-TEST PAID                                
         BZ    COST8                                                            
         MVI   BYTE,1              YES-NOW PUT COST OVERRIDES ON UNPAID         
         B     COST1                   SPOTS                                    
*                                                                               
COST6    OC    RPAY,RPAY           TEST PAID                                    
         BNZ   COST8                                                            
         OI    RSTATUS,X'20'       SET COST OVERRIDE FLAG                       
         TM    BITFLAG,X'40'       TEST BRAND POL NPW                           
         BO    COST7               YES-HANDLE COST FIELD DIFFERENTLY            
         MVC   RPCOST,LBUYCOST+1   NO-PUT COST OVERRIDE                         
         B     COST8                                                            
*                                                                               
COST7    MVC   DUB(1),RPCOST       GET HIGH ORDER BYTE OF PRESENT COST          
         NI    DUB,X'FF'-X'03'     ISOLATE HIGH ORDER 6 BITS                    
         MVC   RPCOST,LBUYCOST+1                                                
         NI    RPCOST,X'03'        MAKE SURE HIGH ORDER 6 BITS ARE OFF          
         OC    RPCOST(1),DUB       COMBINE NPW BITS WITH COST                   
*                                                                               
COST8    IC    R0,1(R4)            NEXT ELEMENT                                 
         AR    R4,R0                                                            
         B     COST2                                                            
*                                                                               
COST10   CLI   KEYPRD,FF           TEST POL BUY                                 
         BNE   *+12                                                             
         CLI   BYTE,0              YES-TEST FOUND PAID SPOTS                    
         BNE   COSTX                                                            
         LA    R6,DESCELEM                                                      
         USING BDELEM,R6                                                        
         MVC   BDCOST,LBUYCOST+1   NO-CHANGE THE COST                           
*                                                                               
COSTX    B     XIT2                                                             
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* MAKE CHANGES TO BUY SPOT ELEMENT BUY DATES AND TIME ALLOCATIONS     *         
***********************************************************************         
         SPACE 1                                                                
SPOTCHG  L     R2,AIO                                                           
         USING BUYRECD,R2                                                       
         LA    R4,BDELEM                                                        
         USING REGELEM,R4                                                       
         GOTO1 DATCON,DMCB,(3,HDREDATE),(2,HALF)                                
         SR    RF,RF               HALF=CAMPAIGN END                            
*                                                                               
SPOT2    CLI   0(R4),0             SCAN ALL THE BUY ELEMENTS                    
         BE    SPOT12                                                           
         CLI   0(R4),6             NON-POOL SPOT ELEMENT                        
         BE    SPOT4                                                            
         CLI   0(R4),11            POOL SPOT ELEMENT                            
         BE    *+12                                                             
         CLI   0(R4),12                                                         
         BNE   SPOT10                                                           
         TM    LCHGIND,LSLN        TEST SPOT LENGTH CHANGE                      
         BZ    SPOT4                                                            
         CLI   1(R4),14            YES-TEST ONE ALLOCATED PRODUCT               
         BNE   *+14                                                             
         MVC   RPTIME,BDSEC        YES-ALTER THE LENGTH                         
         B     SPOT4                                                            
*                                                                               
         CLI   1(R4),18            TEST PIGGYBACK                               
         BNE   SPOT4                                                            
         MVC   RPTIME,SCHMASLN     RESET MASTER PRODUCT SECONDS                 
         ZIC   R0,SCHTOTLN                                                      
         ZIC   R1,SCHMASLN                                                      
         SR    R0,R1               COMPUTE PB PARTNER SPLIT                     
         STC   R0,RPTIME+L'RPALLOC                                              
*                                                                               
SPOT4    CLI   0(R4),12            OTO'S ONLY AFFECTED BY LENGTH CHANGE         
         BE    SPOT10                                                           
         TM    LCHGIND,LSTDATE                                                  
         BZ    SPOT10                                                           
         CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BE    *+18                YES                                          
         CLC   RDATE,SCHSTMNP      TEST BUY DATE BEFORE CAMP START MON          
         BL    SPOT10              YES - IGNORE                                 
         B     *+14                                                             
*                                                                               
         CLC   RDATE,SCHDATS       TEST BUY DATE BEFORE START DATE              
         BL    SPOT10                                                           
*                                                                               
         ZIC   R0,BNUMWKS          FIND WHICH WEEK THIS ELEMENT BELONGS         
         LA    R6,LWKTAB                                                        
         LA    R3,SCHDATS                                                       
         LA    R5,LWKINDS                                                       
*                                                                               
SPOT6    CLC   RDATE,2(R3)         TEST ELEMENT BELONGS TO THIS WEEK            
         BH    SPOT8                                                            
         TM    0(R5),LFRZ          YES - TEST WEEK IS FROZEN                    
         BO    SPOT10                    YES - IGNORE                           
         CLC   2(2,R6),HALF        MAKE SURE BUY DATE CHANGE NOT BEYOND         
         BH    SPOT10              CAMPAIGN END                                 
         MVC   RDATE,2(R6)         MAKE BUY DATE CHANGE                         
         B     SPOT10                                                           
*                                                                               
SPOT8    LA    R6,4(R6)            NEXT WEEK                                    
         LA    R3,4(R3)                                                         
         LA    R5,1(R5)                                                         
         BCT   R0,SPOT6                                                         
*                                                                               
SPOT10   IC    RF,1(R4)            NEXT ELEMENT                                 
         AR    R4,RF                                                            
         B     SPOT2                                                            
*                                                                               
SPOT12   B     XIT2                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE POOL SPOT ELEMENTS                                       *         
* INPUT : P1 = A(BUY RECORD)                                          *         
*         P2 = A(WEEK TABLE ENTRY)                                    *         
*         P3 = A(WEEK POSITION IN SCHEDULE TABLE ENTRY)               *         
*         P4 = A(CAMPAIGN DATES TABLE ENTRY)                          *         
*         LFLAG = LFSTWK FOR THE FIRST WEEK                           *         
* OUTPUT: FVMSGNO = FVRECOF IF RECORD OVERFLOW                        *         
***********************************************************************         
         SPACE 1                                                                
POOLSPT  MVC   ABUY(16),0(R1)      SAVE PARMS                                   
         LM    R2,R3,ABUY          R2=BUY REC, R3=A(WKTAB ENTRY)                
         USING BUYRECD,R2                                                       
         L     R4,ADATES           R4=A(BROADCAST WEEK START/END)               
         L     RE,ASKED            R1=A(SKED ENTRY)                             
         ZIC   R5,0(RE)            NUMBER OF CURRENT BUY SPOTS                  
         ZIC   R6,1(RE)            NUMBER OF PROPOSED NEW SPOTS                 
         SR    R5,R6               COMPARE BUY SPTS TO NEW SPTS                 
         BZ    POOLY               EQUAL - NONE TO ADD OR DELETE                
         BP    POOL2               HIGH - R5 = NUMBER SPTS TO DELETE            
         LPR   R5,R5               LOW  - R5 = NUMBER SPTS TO ADD               
         ICM   R6,1,0(RE)          R6 = NUMBER BUY ELEMS TO READ BEFORE         
         BNZ   POOL2                    ADD/DELETE                              
         OI    LFLAG,LNEWEEK       NO BUY SPOTS - INDICATE A NEW WEEK           
         B     POOL12                                                           
*                                                                               
POOL2    SR    RF,RF                                                            
         LA    R1,BDELEM                                                        
*                                                                               
POOL4    CLI   0(R1),0             FIND BUY ELEMS FOR THIS WEEK                 
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),11            POOL BUY ELEMENT                             
         BNE   POOL6                                                            
         USING REGELEM,R1                                                       
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+18                                                             
         CLC   RDATE,SCHSTMNP      YES - COMPARE TO CAMPAIGN START MON          
         BL    POOL6                                                            
         B     *+14                                                             
         CLC   RDATE,0(R4)         TEST BUY ELEM IN THIS WEEK                   
         BL    POOL6                                                            
         CLC   RDATE,2(R4)                                                      
         BH    POOL6                                                            
         LTR   R6,R6               YES - TEST DELETING ALL                      
         BZ    POOL8                     YES                                    
         BCT   R6,*+8                    NO - READ (R6) OF THEM                 
         B     *+14                                                             
POOL6    IC    RF,1(R1)            NEXT BUY ELEMENT                             
         AR    R1,RF                                                            
         B     POOL4                                                            
*                                                                               
POOL7    IC    RF,1(R1)            POINT TO NEXT ELEMENT                        
         AR    R1,RF               R1 = A(INSERTION/DELETION POINT)             
         CLI   0(R1),X'10'         GO BEYOND ALL ELEMENTS ASSOCIATED            
         BL    POOL8               WITH LAST BUY ELEMENT                        
         CLI   0(R1),X'1F'                                                      
         BNH   POOL7                                                            
*                                                                               
POOL8    L     RE,ASKED                                                         
         CLC   0(1,RE),1(RE)       TEST DELETING SPOTS (BUY VS SCHED)           
         BNH   POOL12                                                           
*                                  YES - DELETE THE NEXT (R5) ELEMS             
POOL10   CLI   0(R1),11            THIS ELEM MUST BELONG TO THIS WEEK           
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+16                                                             
         CLC   RDATE,SCHSTMNP      YES - COMPARE TO CAMPAIGN START MON          
         BNL   *+18                                                             
         DC    H'0'                                                             
         CLC   RDATE,0(R4)                                                      
         BNL   *+6                                                              
         DC    H'0'                                                             
         CLC   RDATE,2(R4)                                                      
         BNH   POOL11                                                           
         DC    H'0'                                                             
*                                                                               
POOL11   MVI   0(R1),FF            DELETE THE ELEMENT                           
         ST    R1,FULL                                                          
         GOTO1 DELELEM,DMCB,FF                                                  
         L     R1,FULL                                                          
         CLI   0(R1),X'0C'         DELETE ALL ASSOCIATED ELEMENTS               
         BL    *+12                                                             
         CLI   0(R1),X'1F'                                                      
         BNH   POOL11                                                           
         BCT   R5,POOL10           DELETE (R5) TIMES                            
         B     POOLY               DONE WITH THIS WEEK                          
         DROP  R1                                                               
*                                                                               
POOL12   XC    BUYELEM,BUYELEM     ADD SPOTS - BUILD BUY ELEMENT                
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         MVI   RCODE,11                                                         
         MVC   RDATE,2(R3)         BUY DATE                                     
*                                                                               
         MVI   RLEN,10                                                          
         CLI   BPRD,X'FF'          TEST UNALLOCATED SPOTS(PRODUCT=POL)          
         BE    POOL14              YES                                          
*                                                                               
         MVI   RLEN,14                                                          
         MVC   RPPRD,BPRD                                                       
         MVC   RPTIME,BSPOTLEN                                                  
         CLI   BINPBPRD,0          TEST FOR PIGGYBACKS                          
         BE    POOL14              NO                                           
*                                                                               
         MVI   RLEN,18             RESET LENGTH FOR PIGGYBACK                   
         MVC   RPTIME,SCHMASLN     FIRST PRODUCT IS MASTER LENGTH               
         MVC   RPPRD+L'RPALLOC(L'RPPRD),BINPBPRD                                
         ZIC   RE,SCHTOTLN                                                      
         ZIC   RF,SCHMASLN                                                      
         SR    RE,RF               COMPUTE PIGGYBACK LENGTH                     
         STC   RE,RPTIME+L'RPALLOC                                              
*                                                                               
POOL14   TM    LFLAG,LNEWEEK       TEST NEW WEEK                                
         BZ    POOL22                                                           
*                                                                               
POOL20   BAS   RE,ADDREG           YES - ADD THE ELEMENT                        
         BNE   POOLN                     EXIT FOR RECORD OVERFLOW               
         BCT   R5,POOL20                 (R5) TIMES                             
         B     POOLY                                                            
*                                  NO -                                         
POOL22   ST    R1,DUB+4            R1 = A(INSERTION POINT)                      
         GOTO1 ARECUPA,DUB,BUYREC       ADD THE ELEMENT                         
         BNE   POOLN               EXIT FOR RECORD OVERFLOW                     
         L     R1,DUB+4                                                         
         ZIC   RE,RLEN             NEXT INSERTION POINT IS BEYOND               
         AR    R1,RE               ELEMENT JUST ADDED                           
         BCT   R5,POOL22           ADD (R5) ELEMENTS                            
*                                                                               
POOLY    CR    RB,RB               SET CC=EQ                                    
         B     POOLX                                                            
*                                                                               
POOLN    LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
POOLX    B     XIT2                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE NON-POOL SPOT ELEMENTS                                   *         
* INPUT : P1 = A(BUY RECORD)                                          *         
*         P2 = A(WEEK TABLE ENTRY)                                    *         
*         P3 = A(WEEK POSITION IN SCHEDULE TABLE ENTRY)               *         
*         P4 = A(CAMPAIGN DATES TABLE ENTRY)                          *         
*         LFLAG = LFSTWK FOR THE FIRST WEEK                           *         
***********************************************************************         
         SPACE 1                                                                
NPOOLSPT LM    R2,R5,0(R1)         GET PARMS                                    
         USING BUYRECD,R2                                                       
         CLI   0(R4),0             TEST CURRENTLY ANY SPOTS THIS WEEK           
         BNE   NPOL2                                                            
         XC    BUYELEM,BUYELEM     NO - BUILD NEW SPOT ELEMENT                  
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         MVI   RCODE,6                                                          
         MVI   RLEN,10                                                          
         MVC   RDATE,2(R3)         BUY DATE                                     
         MVC   RNUM,1(R4)          NUMBER OF SPOTS                              
         BAS   RE,ADDREG           ADD SPOT ELEMENT                             
         BE    NPOLY                                                            
         B     NPOLN                                                            
         DROP  R6                                                               
*                                                                               
NPOL2    SR    RF,RF               FIND THE SPOT ELEMENT FOR THIS WEEK          
         LA    R1,BDELEM                                                        
*                                                                               
NPOL4    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),6             NON-POOL BUY ELEMENT                         
         BNE   NPOL6                                                            
         USING REGELEM,R1                                                       
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+18                                                             
         CLC   RDATE,SCHSTMNP      YES-COMP DATE TO SDEF START MON              
         BL    NPOL6                                                            
         B     *+14                                                             
         CLC   RDATE,0(R5)         TEST BUY DATE IS IN THIS WEEK                
         BL    NPOL6                                                            
         CLC   RDATE,2(R5)                                                      
         BNH   NPOL8                                                            
*                                                                               
NPOL6    IC    RF,1(R1)            NEXT BUY ELEMENT                             
         AR    R1,RF                                                            
         B     NPOL4                                                            
*                                                                               
NPOL8    CLI   1(R4),0             BUY ELEMENT FOUND -                          
         BE    *+14                                                             
         MVC   RNUM,1(R4)          NON-ZERO SPOTS - CHANGE NUMBER               
         B     NPOLY                                                            
         MVI   0(R1),FF            NO SPOTS NOW - DELETE THE ELEMENT            
         GOTO1 DELELEM,DMCB,FF                                                  
*                                                                               
NPOLY    CR    RB,RB                                                            
         B     NPOLX                                                            
*                                                                               
NPOLN    LTR   RB,RB                                                            
*                                                                               
NPOLX    B     XIT2                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ADD/DELETE BRAND POOL NPW SPOT ELEMENTS                             *         
* INPUT : P1 = A(BUY RECORD)                                          *         
*         P2 = A(WEEK TABLE ENTRY)                                    *         
*         P3 = A(WEEK POSITION IN SCHEDULE TABLE ENTRY)               *         
*         P4 = A(CAMPAIGN DATES TABLE ENTRY)                          *         
*         LFLAG = LFSTWK FOR THE FIRST WEEK                           *         
***********************************************************************         
         SPACE 1                                                                
NPWSPT   LM    R2,R5,0(R1)         GET PARMS                                    
         USING BUYRECD,R2                                                       
         CLI   0(R4),0             TEST CURRENTLY ANY SPOTS THIS WEEK           
         BNE   NPW2                                                             
         XC    BUYELEM,BUYELEM     NO - BUILD NEW SPOT ELEMENT                  
         LA    R6,BUYELEM                                                       
         USING REGELEM,R6                                                       
         MVI   RCODE,11                                                         
         MVI   RLEN,14                                                          
         MVC   RDATE,2(R3)         BUY DATE                                     
         MVC   RPPRD,BPRD          SET PRODUCT ALLOCATION                       
         MVC   RPTIME,BDSEC                                                     
         ZIC   RE,1(R4)            NUMBER OF SPOTS                              
         SLL   RE,2                USE HIGH ORDER 6 BITS OF COST                
         STC   RE,RPCOST                                                        
         BAS   RE,ADDREG           ADD SPOT ELEMENT                             
         BE    NPWY                                                             
         B     NPWN                                                             
         DROP  R6                                                               
*                                                                               
NPW2     SR    RF,RF               FIND THE SPOT ELEMENT FOR THIS WEEK          
         LA    R1,BDELEM                                                        
*                                                                               
NPW4     CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),11            BRAND POOL BUY ELEMENT                       
         BNE   NPW6                                                             
         USING REGELEM,R1                                                       
         TM    LFLAG,LFSTWK+LWEEKLY  TEST WEEKLY SKED AND FIRST WEEK            
         BNO   *+18                                                             
         CLC   RDATE,SCHSTMNP      YES-COMP DATE TO SDEF START MON              
         BL    NPW6                                                             
         B     *+14                                                             
         CLC   RDATE,0(R5)         TEST BUY DATE IS IN THIS WEEK                
         BL    NPW6                                                             
         CLC   RDATE,2(R5)                                                      
         BNH   NPW8                                                             
*                                                                               
NPW6     IC    RF,1(R1)            NEXT BUY ELEMENT                             
         AR    R1,RF                                                            
         B     NPW4                                                             
*                                                                               
NPW8     CLI   1(R4),0             BUY ELEMENT FOUND -                          
         BE    NPW10                                                            
*                                                                               
         NI    RPCOST,X'03'        TURN OFF HIGH ORDER 6 BITS OF COST           
         ZIC   RE,1(R4)            NON-ZERO SPOTS - CHANGE NUMBER               
         SLL   RE,2                                                             
         STC   RE,BYTE                                                          
         OC    RPCOST(1),BYTE                                                   
         B     NPWY                                                             
*                                                                               
NPW10    MVI   0(R1),FF            NO SPOTS NOW - DELETE THE ELEMENT            
         ST    R1,FULL                                                          
         GOTO1 DELELEM,DMCB,FF                                                  
         L     R1,FULL                                                          
         CLI   0(R1),X'10'         DELETE ANY TRAILING ELEMENTS ALSO            
         BL    NPWY                                                             
         CLI   0(R1),X'1F'                                                      
         BNH   NPW10                                                            
*                                                                               
NPWY     CR    RB,RB                                                            
         B     NPWX                                                             
*                                                                               
NPWN     LTR   RB,RB                                                            
*                                                                               
NPWX     B     XIT2                                                             
         DROP  R1,R2                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A REGULAR BUY ELEMENT                                *         
* AIO=A(BUY RECORD)                                                   *         
* BUYELEM CONTAINS BUY ELEMENT                                        *         
* ON EXIT, CC=EQ IF OK, CC=NEQ FOR RECORD OVERFLOW                    *         
***********************************************************************         
         SPACE 1                                                                
ADDREG   NTR1  ,                                                                
         L     R2,AIO              R2=A(BUY RECORD)                             
         USING BUYRECD,R2                                                       
         LA    R3,BDELEM                                                        
         SR    R0,R0                                                            
         SR    R5,R5                                                            
         MVI   HALF,6              SET RANGE OF BRAND SPOT EL CODES             
         MVI   HALF+1,8                                                         
         TM    BITFLAG,X'80'       TEST POOL ESTIMATE                           
         BZ    AREG2                                                            
         MVI   HALF,11             YES-CHANGE RANGE OF EL CODES                 
         MVI   HALF+1,13                                                        
*                                                                               
AREG2    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    AREG4                                                            
         CLC   0(1,R3),HALF                                                     
         BL    AREG2                                                            
         CLC   0(1,R3),HALF+1                                                   
         BH    AREG2                                                            
         LR    R5,R3                                                            
         USING REGELEM,R3                                                       
         CLC   RDATE,BUYELEM+RDATE-REGELEM                                      
         BL    AREG2                                                            
         B     AREG8                                                            
*                                                                               
AREG4    LTR   R3,R5                                                            
         BZ    AREG10                                                           
*                                                                               
AREG6    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),0                                                          
         BE    AREG8                                                            
         CLI   0(R3),X'10'                                                      
         BL    AREG8                                                            
         CLI   0(R3),X'1F'                                                      
         BNH   AREG6                                                            
*                                                                               
AREG8    GOTO1 ARECUPA,DUB,BUYREC,(R3)                                          
         BE    AREGY                                                            
         B     AREGN                                                            
*                                                                               
AREG10   GOTO1 ADDELEM,DMCB,BUYELEM                                             
*                                                                               
AREGY    CR    RB,RB                                                            
         B     AREGX                                                            
*                                                                               
AREGN    LTR   RB,RB                                                            
*                                                                               
AREGX    B     XIT2                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
*******************************************************************             
* ROUTINE TO ADD A BUY ELEMENT AT A SPECIFIC LOCATION             *             
* AT ENTRY, P1=A(RECORD), P2=A(INSERTION POINT), BUYELEM=NEW      *             
* ELEMENT.  ON EXIT, CC=EQ IF OK, NEQ IF OVERFLOW                 *             
*******************************************************************             
         SPACE 1                                                                
RECUPA   MVC   DMCB(4),0(R1)       A(RECORD)                                    
         MVI   DMCB,0              SPOTPAK                                      
         LA    RE,BUYELEM                                                       
         ST    RE,DMCB+4                                                        
         MVC   DMCB+8(4),4(R1)     INSERTION POINT                              
         MVI   DMCB+8,C'R'         RETURN ON RECORD OVERFLOW                    
         GOTO1 RECUP,DMCB                                                       
         CLI   8(R1),0             TEST FOR ERROR                               
         BE    *+10                YES                                          
         CR    RB,RB               SET CC=EQ                                    
         B     RECUPAX                                                          
         LTR   RB,RB               SET CC=NEQ                                   
*                                                                               
RECUPAX  B     XIT2                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD BUY TURNAROUND REQUEST                               *         
* ON ENTRY, IOAREA2 CONTAINS BUY RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
ADDREQ   L     R2,AIO2                                                          
         USING BUYRECD,R2                                                       
         MVI   BYTE,0                                                           
         LA    R6,LSTALIST         DO FOR EVERY STATION                         
         LA    R3,MAXSTA                                                        
*                                                                               
ADDR1    OC    0(L'LSTALIST,R6),0(R6)  TEST NO MORE STATIONS                    
         BZ    ADDRX                                                            
*                                                                               
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+14,106                                                   
         LA    R4,ELEMENT+26                                                    
         MVC   0(80,R4),SPACES                                                  
         MVC   0(2,R4),=C'61'      NON-POL GET 61'S                             
*                                                                               
         LA    R5,BLOCK                                                         
         XC    BLOCK(255),BLOCK                                                 
         USING STAPACKD,R5                                                      
*                                                                               
         MVI   STAPACT,C'U'        UNPACK THE MARKET/STATION                    
         MVC   STAPAGY,SIGNON2C                                                 
         MVI   STAPMED,C'R'                                                     
         MVC   STAPACOM,ACOMFACS                                                
         MVC   STAPMKST,0(R6)                                                   
         GOTO1 STAPACK,STAPACKD                                                 
*                                                                               
         CLI   KEYPRD,FF           TEST POL                                     
         BNE   *+12                                                             
         CLI   CPROFILE,C'2'       YES - TEST BRD POL 61'S                      
         BNE   ADDR2                                                            
*                                                                               
         CLI   CPROFILE+2,C'1'     TEST BRD POL 61'S BY STA                     
         BNE   ADDR4                                                            
         MVC   18(5,R4),STAPQSTA                                                
         MVI   BYTE,1                                                           
         B     ADDR4                                                            
*                                                                               
ADDR2    MVC   0(2,R4),=C'81'      PROF+0 = 0 OR 1 SO GET 81'S                  
         CLI   CPROFILE+2,C'2'     TEST U3 BY MKT                               
         BNE   *+14                                                             
         MVC   0(2,R4),=C'U3'                                                   
         B     ADDR4                                                            
*                                                                               
         CLI   CPROFILE+2,C'3'     TEST U3 BY STATION                           
         BNE   *+10                                                             
         MVC   0(2,R4),=C'U3'                                                   
         MVC   18(5,R4),STAPQSTA    AND ALL 81'S ARE BY STATION                 
         MVI   BYTE,1                                                           
*                                                                               
ADDR4    CLC   0(2,R4),=C'U3'                                                   
         BE    ADDR6                                                            
         PACK  DUB,0(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,ELEMENT+10                                                    
*                                                                               
ADDR6    MVC   2(2,R4),SIGNON2C                                                 
         MVI   4(R4),C'R'                                                       
         MVC   5(3,R4),QCLT                                                     
         MVC   8(2,R4),=C'NN'      NO MKT/PRD GRPS                              
         MVC   11(3,R4),HDRPRD                                                  
         CLI   KEYPRD,FF           TEST POL                                     
         BNE   ADDR8               NO                                           
         CLI   CPROFILE,C'0'       TEST BRD POL                                 
         BE    ADDR8               NO                                           
         MVC   11(3,R4),HDRPR1     YES-SET MASTER PRODUCT ALLOCATION            
*                                                                               
ADDR8    MVC   14(4,R4),STAPQMKT   MARKET                                       
*                                                                               
         ZIC   R0,BEST             EST                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
*                                                                               
         MVC   37(12,R4),ESTSTRT   EST START/END DATES                          
*                                                                               
         MVI   59(R4),C'B'         T/A IND                                      
         CLI   ELEMENT+10,81                                                    
         BNE   *+8                                                              
         MVI   59(R4),C'A'         SET TO GEN U4                                
         CLI   ELEMENT+10,61                                                    
         BNE   ADDR10                                                           
         MVC   61(5,R4),=C'30000'                                               
         MVI   ELEMENT+10,0                                                     
         MVC   ELEMENT+26(2),=C'U3'                                             
         MVC   ELEMENT+26+49(19),SPACES CLEAR ALL OPTIONS                       
*                                                                               
ADDR10   MVC   68(12,R4),SPACES                                                 
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',ELEMENT,ELEMENT               
         CLI   8(R1),0                                                          
         BNE   ADDR90              DATAMGR ERROR                                
*                                                                               
         CLC   SVRFPGRP,SPACES                                                  
         BNH   ADDR12                                                           
         CLC   =C'G7',SIGNON2C                                                  
         BNE   *+12                                                             
         TM    SVEFLAG1,EF1REQ                                                  
         BZ    ADDR12                                                           
* ADD RFP GROUP REQUEST                                                         
         MVC   ELEMENT+128(80),ELEMENT      SAVE ORIGINAL REQUEST               
         MVC   0(2,R4),=C'RF'                                                   
         MVC   49(8,R4),SVRFPGRP                                                
         GOTO1 (RF),(R1)                                                        
         MVC   ELEMENT+26(80),ELEMENT+128   RESTORE ORIGINAL REQUEST            
*                                                                               
ADDR12   CLI   BDTIME,0            TEST PIGGYBACK                               
         BE    ADDR14              NO                                           
         MVC   11(3,R4),HDRPPB     PASSIVE PRD                                  
         ZIC   R0,BEST             PASSIVE EST                                  
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R4),DUB                                                     
         GOTO1 DATAMGR,DMCB                                                     
         CLI   8(R1),0                                                          
         BNE   ADDR90              DATAMGR ERROR                                
         B     ADDR20                                                           
*                                                                               
ADDR14   CLI   BINPBPRD,0          CHECK FOR P/B MASPRD                         
         BE    ADDR20              NO                                           
*                                                                               
ADDR16   MVC   11(3,R4),HDRPPB     ADD REQUEST FOR SECOND BRAND                 
         GOTO1 DATAMGR,DMCB                                                     
         CLI   8(R1),0                                                          
         BNE   ADDR90                                                           
*                                                                               
ADDR20   CLI   BYTE,1              TEST BY STATION                              
         BNE   ADDRX                                                            
         LA    R6,L'LSTALIST(R6)   YES-NEXT STATION                             
         BCT   R3,ADDR1                                                         
         B     ADDRX                                                            
*                                                                               
ADDR90   DS    0H                  DATAMGR ERROR                                
*                                                                               
ADDRX    B     XIT2                                                             
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE BUYLINE TABLE BY GOING THROUGH ALL THE                
* BUYLINES FOR A GIVEN MASTER KEY.                                              
*                                                                               
* FORMAT OF BUYLINE TABLE                                                       
* -----------------------                                                       
* BYTE 0  - 4 BYTES - SCHEDULE SEQUENCE NUMNBER                                 
* BYTE 4  - 3 BYTES - TRANSACTION DATE                                          
* BYTE 7  - 3 BYTES - TRANSACTION TIME                                          
* BYTE 10 - 1 BYTES - MASTER PRODUCT CODE                                       
* BYTE 11 - 1 BYTES - PIGGYBACK PRODUCT CODE                                    
***********************************************************************         
         SPACE 1                                                                
BLDBLTAB DS    0H                                                               
         L     R0,ABLNTBL          CLEAR THE BUYLINE TABLE                      
         LA    R1,255*BLNLNQ                                                    
         LA    RE,BLOCK                                                         
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R4,KEY              BUILD KEY FOR THE MASTER KEY                 
         USING BUYRECD,R4                                                       
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMED                                                   
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,KEYPRD                                                   
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,BEST                                                     
*                                                                               
         MVC   PARTKEY,KEY         MAKE A COPY OF KEY - DETAILS                 
*                                                                               
         GOTO1 HIGH                GET 1ST KEY THAT MATCHES MASTER KEY          
*                                                                               
BBLLOOP  CLC   PARTKEY,KEY         IF SAME MASTER KEY                           
         BNE   BBLX                                                             
*                                                                               
         ZIC   R2,BUYKEST+2        R2 = A(BUYLINE ENTRY)                        
         BCTR  R2,0                                                             
         MH    R2,=Y(BLNLNQ)                                                    
         A     R2,ABLNTBL                                                       
         USING BLND,R2                                                          
*                                                                               
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         CLI   KEYPRD,X'FF'        IF PRODUCT IN KEY IS NOT POL                 
         BE    BBL10                                                            
         MVC   BLNMAS,KEYPRD        THEN COPY THE PRODUCT CODE                  
         B     BBL20                                                            
*                                                                               
BBL10    GOTO1 GETELEM,DMCB,1      ELSE GET IT FROM DESCRIPTION ELEM            
         BE    *+6                                                              
         DC    H'0'                DIE IF NO DESCRIPTION ELEMENT                
         USING BDELEM,R6                                                        
         MVC   BLNMAS,BDMASPRD                                                  
         CLI   BLNMAS,0            TEST FOR UNALLOCATED MASTER PRODUCT          
         BNE   *+8                 NO                                           
         MVI   BLNMAS,FF           YES-SET TO POL (TRUE POL)                    
         DROP  R6                                                               
*                                                                               
BBL20    GOTO1 GETELEM,DMCB,4      GET PIGGYBACK CODE IF ANY                    
         BNE   BBL30                                                            
         USING PBELEM,R6                                                        
         MVC   BLNPIG,PBPROD                                                    
         DROP  R6                                                               
*                                                                               
BBL30    GOTO1 GETELEM,DMCB,BTRCCODQ GET TRACE ELEMENT (X'98')                  
         BNE   BBLNEXT                                                          
         USING BTRCELEM,R6                                                      
         MVC   BLNSEQ,BTRCSEQN     EXTRACT SCHEDULE SEQUENCE #                  
         MVC   BLNDATE,BTRCDATE    ORIGINAL TRANSFER DATE                       
         MVC   BLNTIME,BTRCTIME    ORIGINAL TRANSFER TIME                       
         DROP  R6                                                               
*                                                                               
BBLNEXT  GOTO1 SEQ                                                              
         B     BBLLOOP                                                          
*                                                                               
BBLX     B     XIT2                RETURN TO CALLER                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE X'98' ELEMENT SO WE CAN BACK TRACE TO WHERE           
* BUY CAME FROM: SCHEDULE #, DATE, TIME, AND LUID.  ALSO SET THE DATE           
* AND TIME IN THE CONFIRMATION OBJECT.                                          
***********************************************************************         
         SPACE 1                                                                
BLDGFACT DS    0H                                                               
         LA    R6,TRCEELEM         SET UP THE TRACE ELEMENT                     
         USING BTRCELEM,R6                                                      
         XC    TRCEELEM,TRCEELEM                                                
         MVI   BTRCCODE,BTRCCODQ                                                
         MVI   BTRCLEN,BTRCLENQ                                                 
         MVC   BTRCSEQN,SCHLNNUM   SAVE SCHEDULE LINE SEQUENCE NUMBER           
*                                                                               
         GOTO1 GETFACT,DMCB,(X'02',0)  GET INFO (DATE, TIME, LUID)              
*                                                                               
         L     R1,0(R1)            R1 = A(GETFACT BLOCK)                        
         USING FACTSD,R1                                                        
*                                                                               
         MVC   BTRCDATE,FADATEB    SAVE THE DATE                                
         MVC   BTRCLUID,FASYM      SAVE THE TERMINAL SYSMOLIC ID                
*                                                                               
         TIME  DEC                 R0 = HHMMSSTH                                
         LR    R2,R0                                                            
*                                                                               
         XC    DUB,DUB             CONVERT HOURS TO BINARY AND SAVE             
         SRL   R0,24                                                            
         SLL   R0,4                                                             
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BTRCTIME                                                      
*                                                                               
         XC    DUB,DUB             CONVERT MINUTES AND SAVE                     
         LR    R0,R2                                                            
         SLL   R0,8                                                             
         SRL   R0,24                                                            
         SLL   R0,4                                                             
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BTRCTIME+1                                                    
*                                                                               
         XC    DUB,DUB             CONVERT SECONDS AND SAVE                     
         LR    R0,R2                                                            
         SLL   R0,16                                                            
         SRL   R0,24                                                            
         SLL   R0,4                                                             
         ST    R0,DUB+4                                                         
         OI    DUB+7,X'0F'                                                      
         CVB   R1,DUB                                                           
         STC   R1,BTRCTIME+2                                                    
*                                  FILL IN SOME CONFIRMATION INFO               
         TM    SCHINDS,SCHIRET     TEST FOR RE-TRANSFER                         
         BO    BFCX                YES-DON'T NEED TO PASS IT BACK               
         GOTO1 HEXOUT,DMCB,BTRCTIME,CNFTTIME,L'BTRCTIME                         
         GOTO1 HEXOUT,DMCB,BTRCDATE,CNFTDATE,L'BTRCDATE                         
*                                                                               
BFCX     B     XIT2                RETURN TO CALLER                             
         DROP  R1,R6                                                            
         EJECT                                                                  
********************************************************************            
* BUILD OF LIST OF PRODUCTS IN SPOT BUYREC BEFORE PUTREC           *            
********************************************************************            
         SPACE 2                                                                
BLDSPTL  DS    0H                                                               
         L     R6,AIO              POINT TO BUY RECORD                          
         XC    ELEMENT,ELEMENT     CLEAR LIST AREA                              
         CLI   3(R6),X'FF'         TEST POL BUY                                 
         BNE   XIT2                NO                                           
*                                                                               
         LA    R6,BDELEM-BUYREC(R6) POINT TO FIRST ELEMENT                      
BLDSPTL2 SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BZ    XIT2                IGNORE ERROR                                 
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    BLDSPTL6                                                         
         CLI   0(R6),X'0B'                                                      
         BL    BLDSPTL2                                                         
         CLI   0(R6),X'0D'                                                      
         BH    BLDSPTL2                                                         
*                                                                               
         ZIC   R0,1(R6)            GET ELEMENT LENGTH                           
         SH    R0,=H'10'                                                        
         BNP   BLDSPTL2            SKIP IF NOT ALLOCATED                        
         SRL   R0,2                GIVES NUMBER OF ALLOCATIONS                  
*                                                                               
BLDSPTL4 LA    R1,10(R6)           POINT TO FIRST ALLOCATION                    
         BAS   RE,BLDSINS                                                       
         LA    R1,4(R1)            NEXT PRODUCT                                 
         BCT   R0,BLDSPTL4                                                      
         B     BLDSPTL2                                                         
*                                                                               
BLDSPTL6 CLI   ELEMENT,0           NO ALLOCATED ELEMENTS                        
         BE    XIT2                                                             
         LA    RE,ELEMENT                                                       
         ST    RE,APRDLIST         SET DM6 FOR PUTREC                           
         B     XIT2                                                             
*                                                                               
BLDSINS  LA    RF,ELEMENT          TEST PRODUCT IN LIST ALREADY                 
*                                                                               
BLDSINS2 CLI   0(RF),0                                                          
         BE    BLDSINS4                                                         
         CLC   0(1,RF),0(R1)        R1 POINTS TO NEW PRODUCT                    
         BER   RE                                                               
         LA    RF,1(RF)                                                         
         B     BLDSINS2                                                         
*                                                                               
BLDSINS4 MVC   0(1,RF),0(R1)       MOVE NEW PRD TO LIST                         
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE PIGGYBACK ELEMENT IF NEEDED.                          
***********************************************************************         
BLDPIGS  DS    0H                                                               
         XC    PIGELEM,PIGELEM                                                  
         LA    R6,PIGELEM          THEN BUILD PIGGBACK ELEMENT                  
         USING PBELEM,R6                                                        
         MVI   PBCODE,X'04'                                                     
         MVI   PBLEN,L'PIGELEM                                                  
         MVC   PBPROD,BINPBPRD                                                  
         MVC   PBEST,BEST                                                       
         ZIC   R0,SCHTOTLN                                                      
         ZIC   R1,SCHMASLN                                                      
         SR    R0,R1                                                            
         STC   R0,PBTIME                                                        
         STC   R0,PBCOST                                                        
         MVC   PBPRD,HDRPPB                                                     
*                                                                               
BLDPGX   B     XIT2                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE BUY KEY  - AT ENTRY, NEWLINE CONTAINS BUY             
* LINE NUMBER                                                                   
***********************************************************************         
BLDBKEY  DS    0H                                                               
         LA    R6,KEY              BUILD BUY KEY                                
         USING BUYKEY,R6                                                        
         XC    KEY,KEY                                                          
         MVC   BUYKAM,BAGYMED                                                   
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,KEYPRD                                                   
         MVC   BUYMSTA,BMKTSTA                                                  
         MVC   BUYKEST,BEST                                                     
*                                                                               
         MVC   BUYKBUY+1(L'NEWLINE),NEWLINE                                     
         MVI   BUYKBUY+2,X'01'                                                  
*                                                                               
BBKX     B     XIT2                RETURN TO CALLER                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* MOVE BUY KEY TO IO2                                                           
***********************************************************************         
MOVKIO2  DS    0H                                                               
         L     RE,AIO2             CLEAR IO2                                    
         LA    RF,LIOS                                                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO2             MOVE KEY TO IO2                              
         USING BUYRECD,R6                                                       
         MVC   BUYKEY(10),KEY                                                   
*                                  SHIFT OVER BUYLINE                           
         MVC   BUYKEY+10(2),KEY+11                                              
         MVI   BUYKEY+12,0                                                      
         LA    R0,BDELEM-BUYREC    STORE DISPLACEMENT TO 1ST ELEMENT            
         STCM  R0,3,BUYRLEN                                                     
         MVC   BUYALPHA,SIGNON2C   INSERT AGENCY CODE                           
         B     XIT2                                                             
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE WRITES THE BUY RECORD TO THE FILE.                               
***********************************************************************         
         SPACE 1                                                                
WRITBREC DS    0H                                                               
         L     R6,AIO2             POINT TO NEW RECORD                          
*                                                                               
WBR05    DS    0H                                                               
         OI    DMINBTS,X'08'       READ DELETED RECORDS                         
         MVI   RDUPDATE,C'Y'       READ FOR UPDATE                              
         GOTO1 HIGH                                                             
*                                  IF RECORD ISN'T ALREADY THERE                
         CLC   KEY(L'BUYKEY),KEYSAVE                                            
         BE    WBR10                                                            
*                                                                               
         MVC   AIO,AIO2            THEN ADD IT                                  
         GOTO1 ADDREC                                                           
         B     WBRX                                                             
*                                                                               
WBR10    MVC   AIO,AADDIO          TEMPORARY IO AREA (SINCE NO AIO3)            
         TM    KEY+L'BUYKEY,X'80'  IF DELETED                                   
         BZ    WBR20                                                            
*                                  THEN UNDELETE IT AND WRITE RECORD            
         NI    KEY+L'BUYKEY,X'FF'-X'80'                                         
         GOTO1 WRITE                                                            
*                                                                               
WBR20    GOTO1 GETREC              READ OLD RECORD                              
*                                                                               
         MVC   AIO,AIO2            WRITE OUR RECORD IN ITS PLACE                
         GOTO1 ABLDSPTL            BUILD ADDED PRODUCT LIST                     
         GOTO1 PUTREC                                                           
*                                  DON'T READ DELETED RECORDS                   
WBRX     NI    DMINBTS,X'FF'-X'08'                                              
         MVI   RDUPDATE,C'N'       RESET READ FOR UPDATE                        
         B     XIT2                RETURN TO CALLER                             
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS A SCHEDULE BROADCAST TABLE AND SETS SOME                  
* OTHER DATE FIELDS                                                             
***********************************************************************         
         SPACE 1                                                                
GETDAT   NTR1  BASE=*,LABEL=*                                                   
         MVC   DUB,HDREDATE        SAVE END DATE                                
         CLI   HDRRETR,C'Y'                                                     
         BNE   GETDAT1                                                          
         CLC   HDREDATE,HDRPREND   TEST IF END DATE > LAST TRANSFER END         
         BH    *+10                YES                                          
         MVC   HDREDATE,HDRPREND   FUDGE THE LAST TRANSFER                      
*                                                                               
GETDAT1  CLI   DAILY,C'Y'          TEST DAILY SCHEDULING                        
         BE    GETDAT10                                                         
*                                                                               
         XC    WORK,WORK           ZERO DUMMY SYSPROF AREA                      
         MVC   WORK+8(1),ESTOWKSD  OUT OF WEEK START                            
         MVC   BLOCK(4),GETBROAD                                                
         MVC   BLOCK+4(4),ADDAY                                                 
         MVC   BLOCK+8(4),GETDAY                                                
         MVC   BLOCK+12(4),DATCON                                               
*                                                                               
         GOTO1 MOBILE,DMCB,('MAXWEEKS',HDRSDATE),(5,SCHDATS),BLOCK,WORK         
         MVC   HDREDATE,DUB        RESTORE END DATE IN CASE                     
*                                                                               
         GOTO1 DATCON,DMCB,(2,SCHDATS),(0,SCHSTMON)                             
         GOTO1 (RF),(R1),(0,SCHSTMON),(3,SCHSTMNB)                              
         GOTO1 (RF),(R1),(0,HDREDATE),(3,SCHENDB)                               
         MVC   SCHSTMNP,SCHDATS                                                 
         CLI   HDRRETR,C'Y'        TEST FOR RETRANSFER                          
         BNE   GETDATX             NO                                           
         CLI   ESTOWKSD,0          TEST FOR OUT-OF-WEEK ROTATOR EST             
         BNE   GETDAT6             YES                                          
*                                                                               
* COMPUTE NUMBER OF WEEKS IN SCHEDULE AS OF LAST TRANSFER                       
*                                                                               
GETDAT2  GOTO1 GETDAY,DMCB,HDRPREND,FULL                                        
         MVC   BYTE,0(R1)          GET DAY NUMBER OF FORMER LAST DAY            
*                                                                               
         MVC   WORK(6),SCHSTMON    WORK(6)=START WEEK'S MONDAY                  
         MVC   WORK+6(6),HDRPREND  WORK+6(6)=FORMER END WEEK'S SUNDAY           
         CLI   BYTE,7              TEST IF IT WAS SUNDAY                        
         BE    GETDAT4             YES                                          
*                                                                               
* FIND SUNDAY OF FORMER END DATE'S WEEK                                         
*                                                                               
         LA    R2,7                                                             
         ZIC   RE,BYTE                                                          
         SR    R2,RE               COMPUTE # OF DAYS TO ADD                     
         GOTO1 ADDAY,DMCB,HDRPREND,WORK+6,(R2)                                  
*                                                                               
GETDAT4  GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         MVC   BPRVWKS,DMCB+13                                                  
         B     GETDATX                                                          
*                                                                               
* FOR OUT-OF-WEEK ROTATOR, FIND NUMBER OF WEEKS BETWEEN START                   
* AND END DATE INCLUSIVE.                                                       
*                                                                               
GETDAT6  MVC   WORK(12),HDRSDATE   ACTUAL START/END DATE                        
         GOTO1 PERVERT,DMCB,WORK,WORK+6                                         
         LH    RE,12(R1)           GET N'DAYS/7                                 
         OC    10(2,R1),10(R1)     TEST FOR REMAINDER                           
         BZ    *+8                 NO                                           
         LA    RE,1(RE)            YES-INCREMENT N'WEEKS                        
         STC   RE,BPRVWKS                                                       
         B     GETDATX                                                          
*                                                                               
* FOR DAILY SCHEDULING, ADD ONE DATE ENTRY FOR EACH DAY                         
*                                                                               
GETDAT10 MVC   WORK(12),HDRSDATE   START DATE/END DATE                          
         LA    R4,SCHDATS                                                       
*                                                                               
GETDAT12 CLC   WORK(6),WORK+6      TEST PAST SCHEDULE END                       
         BH    GETDAT14            YES                                          
         GOTO1 DATCON,DMCB,WORK,(2,(R4))                                        
         MVC   2(2,R4),0(R4)       START DATE=END DATE                          
         GOTO1 ADDAY,DMCB,WORK,WORK+12,1                                        
         MVC   WORK(6),WORK+12     NEW DATE                                     
         LA    R4,4(R4)            NEXT DATE PAIR                               
         B     GETDAT12                                                         
*                                                                               
GETDAT14 MVI   0(R4),X'FF'                                                      
         MVC   HDREDATE,DUB        RESTORE REAL END DATE                        
         GOTO1 DATCON,DMCB,HDREDATE,(3,SCHENDB)                                 
         CLI   HDRRETR,C'Y'        TEST FOR RETRANSFER                          
         BNE   GETDATX                                                          
*                                                                               
* FIND PREVIOUS NUMBER OF DAYS IN SCHEDULE                                      
*                                                                               
         GOTO1 PERVERT,DMCB,HDRSDATE,HDRPREND                                   
         MVC   BPRVWKS,DMCB+9      NUMBER OF DAYS INCLUSIVE                     
*                                                                               
GETDATX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
       ++INCLUDE CTMADDSECT                                                     
         SPACE 3                                                                
* DDCOMFACS                                                                     
* DDCOREQUS                                                                     
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
APRELO   DS    A                                                                
APBASE1  DS    A                                                                
APBASE2  DS    A                                                                
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
ACONTROL DS    A                   A(CONTD)                                     
AENTRY   DS    A                   A(ENTRY) FOR BUYLINE                         
RECUP    DS    V                   V(RECUP)                                     
DAYUNPK  DS    V                   V(DAYUNPK)                                   
PERVERT  DS    V                                                                
NUMBKEYS DS    A                   NUMBER OF DIFFERENT BUY KEYS                 
ABLNTBL  DS    A                   A(BUYLINE TABLE)                             
SETSENT  DS    C                   SET WORKER FILE TO SENT (Y/N)                
*                                                                               
*                   BINARY EQUIVALENTS                                          
*                                                                               
KEYPRD   DS    XL1                 PRODUCT CODE TO BE IN THE KEY                
BNUMWKS  DS    XL1                 # OF WEEKS WITHIN ESTIMATE PERIOD            
BPRVWKS  DS    XL1                 # OF WEEKS AS OF LAST TRANSFER               
BDAY1    DS    XL1                 BUY START DAY                                
*                                                                               
*                   ELEMENTS                                                    
*                                                                               
DESCELEM DS    XL256               DESCRIPTION ELEMENT                          
PIGELEM  DS    XL(PBPRD+L'PBPRD-PBELEM)  PIGGYBACK ELEMENT                      
TRCEELEM DS    XL(BTRCLENQ)        TRACE ELEMENT                                
BUYELEM  DS    XL18                BUY ELEMENT                                  
*                                                                               
*                   MISCELLANEOUS                                               
*                                                                               
SPACES   DS    CL80                BLANKS FILLED FIELD                          
NUMSCHED DS    XL1                 NUMBER OF SCHEDULE OBJECTS                   
NUMWEEKS DS    XL1                 NUMBER OF WEEKS USED FOR LOOP                
FLAG     DS    XL1                                                              
BITFLAG  DS    XL1                 OBJECT FLAG                                  
*                                  X'80' - POOL PRODUCT                         
*                                  X'40' - USE HIGH ORDER 5 BITS FOR #          
*                                  X'02' - AT LEAST 1 SLINE PROCESSED           
*                                  X'01' - END-OF-DATA OBJECT READ              
NUMERRS  DS    XL1                 SLINE ERROR COUNT                            
PARTKEY  DS    CL(L'BUYKEY-L'BUYKBUY)   PARTIAL MASTER KEY                      
NEWLINE  DS    XL1                 BUYLINE FOR RECORD                           
NUMDCATS DS    XL1                 NUMBER OF VALID DATA CATEGORIES              
BINODATE DS    XL3                 ORIGINAL TRANSFER DATE IN BINARY             
ENDSCHED DS    CL1                 NO MORE SCHEDULES (Y/N)                      
BINPBPRD DS    XL1                 PIGGYBACK PRODUCT CODE                       
BOOKS    DS    XL8                 BOOKS4| - YEAR/MONTH                        
**ALPHMRKT DS    CL3                 ALPHA MARKET CODE (FOR DEMAND)             
ESTRATE  DS    XL2                 ESTIMATE RATE TYPE                           
CONFLAG  DS    CL1                 Y/N=CONFIRM OBJECT PUTITEM PENDING           
SVRFPGRP DS    CL8                 CLIENT RFP REQUEST GROUP                     
SVEFLAG1 DS    XL1                 EFLAG1                                       
SVECOST2 DS    XL4                 ECOST2 FROM BRAND ESTIMATE                   
*                                                                               
PWFLAG   DS    XL1                                                              
PWMKT    DS    CL4                                                              
SVOLDMKT DS    XL4                 SAVED OLD MARKETS                            
*                                                                               
DAILY    DS    CL1                 Y=DAILY SCHEDULING                           
RATE     DS    CL1                 RATE TYPE                                    
REP      DS    XL2                 SPECIAL REP                                  
AFLAG1   DS    XL1                 FLAG1 FROM AGYHDR                            
DTYPLIST DS    (MAXDEMOS+1)XL3     DEMO TYPE LIST AND ROOM FOR X'FF'            
*                                                                               
**     ++INCLUDE DEDBLOCK                                                       
*                                                                               
*                   OBJECTS                                                     
*                                                                               
HDROBJCT DS    0C                  HEADER OBJECT STRUCTURE                      
HDRMED   DS    CL1            C    MEDIA                                        
HDRCLT   DS    CL3            C    CLIENT NAME                                  
HDRPRD   DS    0CL6           C    PRODUCT NAME WITH PIGGYBACK                  
HDRPR1   DS    CL3                                                              
HDRPPB   DS    CL3                                                              
HDREST   DS    CL2            X    ESTIMATE #                                   
HDRSERV  DS    CL1            C    RATING SERVICE                               
HDRBOOKS DS    XL16           X    BOOKS (4 ZERO PADDED YEAR/MONTH)             
HDRSDATE DS    CL6            N    BUY OR RETRANSFER START DATE (YMD)           
HDREDATE DS    CL6            N    END DATE (YMD)                               
HDRWKS   DS    CL2            N    # OF WEEKS                                   
HDRGENTA DS    CL1            C    GENERATE T/A REQUEST(Y/N)                    
HDRRETR  DS    CL1            C    RETRANSFER (Y/N)                             
HDRPREND DS    CL6            N    PREVIOUS END DATE (YMD)                      
HDRREPRV DS    CL1            C    RETRANSFER PREVIOUSLY UPLOADED WEEKS         
HDRCOST  DS    CL1            C    COST CHANGES ALLOWED (Y/N)                   
HDRDEMO  DS    CL1            C    DEMO CHANGES ALLOWED (Y/N)                   
HDRDTYPS DS    (MAXDEMOS)CL6  X    VARIABLE LENGTH LIST OF DEMO TYPES           
         DS    CL2            X    IN CASE WE HAVE MAX # OF DEMO TYPES          
HDRDAILY DS    CL1                 Y=DAILY SCHEDULE                             
HDRRATE  DS    CL1                 RATE TYPE                                    
HDRREP   DS    XL4                 SPECIAL REP                                  
         DS    XL1                 NULL TERMINATOR                              
HDRLEN   EQU   *-HDROBJCT          LENGTH OF HEADER OBJECT                      
*                                                                               
SCHOBJCT DS    0C                  STRUCTURE OF SCHEDULE OBJECT (BIN)           
SCHERROR DS    XL1                 ERRORS IF ANY                                
SCHLNNUM DS    XL4                 SCHEDULE LINE SEQUENCE NUMBER                
SCHINDS  DS    XL1                 INDICATORS                                   
SCHIRET  EQU   X'80'               SLINE IS A RE-TRANSFER                       
SCHIORB  EQU   X'40'               SLINE IS AN ORBIT                            
SCHODATE DS    CL6                 ORIGINAL TRANSFER DATE                       
SCHOTIME DS    XL3                 ORIGINAL TRANSFER TIME                       
SCHSTA   DS    CL5                 STATION CALL LETTERS                         
SCHDAY   DS    XL1                 DAY CODE (BITWISE)                           
SCHSTIME DS    XL2                 START TIME (MILITARY)                        
SCHNTIME DS    XL2                 END TIME                                     
SCHDYPRT DS    CL1                 DAYPART CODE                                 
SCHMASLN DS    XL1                 MASTER PRD SPOT LENGTH IN SECONDS            
SCHTOTLN DS    XL1                 TOTAL SPOT LENGTH IN SECONDS                 
SCHCOST  DS    XL3                 SPOT COST                                    
SCHPROG  DS    CL18                PROGRAM NAME                                 
SCHDT    DS    XL31                DAYTIME BLOCK FOR ORBIT                      
SCHOVERS DS    XL2                 DEMO OVERRIDE BIT LIST                       
SCHCOMLN DS    XL1                 COMMENT LENGTH                               
SCHCOMDS DS    XL2                 COMMENT DISPLACEMENT                         
SCHDAYNM DS    XL1                 SPOTPAK DAY NUMBERS                          
SCHDATA  DS    0C                  ALLOCATE THE MAXIMUM SPACE NEEDED            
SCHSPOTS DS    (MAXWEEKS)XL1       SPOTS PER WEEK                               
SCHDVALS DS    (MAXDEMOS)XL2       DEMO VALUES PER DEMO TYPE                    
SCHTRANS DS    (MAXWEEKS)XL1       SPOTS PER WEEK IN LAST TRANSFER              
SCHCOM   DS    XL256               COMMENT                                      
SCHLEN   EQU   *-SCHOBJCT          LENGTH OF SCHEDULE OBJECT                    
*                                                                               
CNFOBJCT DS    0C                  CONFIRMATION OBJECT STRUCTURE                
CNFLNNUM DS    CL8                 SCHEDULE LINE SEQUENCE NUMBER                
CNFERROR DS    CL2                 ERROR IF ANY                                 
CNFINDS  DS    CL2                 INDICATORS                                   
CNFIFRZ  EQU   X'80'               SOMETHING WAS FROZEN ON BUY SIDE             
CNFIDEL  EQU   X'40'               BUY LINE(S) WERE DELETED                     
CNFTDATE DS    CL6                 TRANSFER DATE                                
CNFTTIME DS    CL6                 TRANSFER TIME (HOURS/MINS./SECONDS)          
CNFLNQ1  EQU   *-CNFOBJCT          FIXED LENGTH OF CONFIRMATION OBJECT          
CNFSPOTS DS    XL(MAXWEEKS*2)      TRANSFERRED SPOTS/WEEK                       
CNFLENQ  EQU   *-CNFOBJCT          LENGTH OF CONFIRMATION OBJECT                
CNFLEN   DS    CL1                                                              
         EJECT                                                                  
*                                                                               
SCHDATS  DS    XL((MAXWEEKS*4)+1)                                               
SCHSTMON DS    CL6                 SCHEDULE START MONDAY (YYMMDD)               
SCHSTMNP DS    XL2                 SCHEDULE START MONDAY (COMPRESSED)           
SCHSTMNB DS    XL3                 SCHEDULE START MONDAY (BINARY)               
SCHENDB  DS    XL3                 SCHEDULE END DATE(BINARY)                    
*                                                                               
* PARAMETERS TO POOLSPT AND NPOOLSPT ROUTINES                                   
*                                                                               
ABUY     DS    A                                                                
AWKTAB   DS    A                                                                
ASKED    DS    A                                                                
ADATES   DS    A                                                                
*                                                                               
AXTRA    DS    0F               ** EXTENSION ROUTINE ADDRESSES **               
ABLDWKS  DS    A                                                                
ABUYCHG  DS    A                                                                
ABUYDESC DS    A                                                                
ABUYDATE DS    A                                                                
ABLDDEMO DS    A                                                                
ABLDCOM  DS    A                                                                
ABLDORB  DS    A                                                                
ACOSTCHA DS    A                                                                
ASPOTCHG DS    A                                                                
APOOLSPT DS    A                                                                
ANPOLSPT DS    A                                                                
ANPWSPT  DS    A                                                                
ARECUPA  DS    A                                                                
AADDREQ  DS    A                                                                
ABLDBLTB DS    A                                                                
ABLDFACT DS    A                                                                
ABLDSPTL DS    A                                                                
ABLDPIGS DS    A                                                                
ABLDBKEY DS    A                                                                
AMOVKIO2 DS    A                                                                
AWRTBREC DS    A                                                                
AXTRAN   EQU   (*-AXTRA)/L'AXTRA                                                
*                                                                               
LAWKST   DS    A                                                                
LAWKEN   DS    A                                                                
LASKED   DS    A                                                                
LSPTOT   DS    F                                                                
LMAXSPTS DS    F                                                                
LBUYCOST DS    F                                                                
*                                                                               
LWKINDS  DS    XL(MAXWEEKS)        WEEK INDICATORS TABLE                        
LFRZ     EQU   X'80'               THIS WEEK IS FROZEN                          
LXFRTOT  DS    XL(MAXWEEKS)        ORIGINAL TRANSFER TOTALS SPTS/WEEK           
LBUYTOT  DS    XL(MAXWEEKS)        CURRENT BUYLINES TOTALS SPTS/WEEK            
LNEWTOT  DS    XL(MAXWEEKS)        NEW SPOTS/WEEK TOTALS                        
LSPOTS   DS    XL(MAXWEEKS)        SPOTS/WEEK ARRAY FOR BLDWK                   
LWKTAB   DS    XL((MAXWEEKS+1)*4)  WEEKS TABLE (SEE BLDWKS FOR FORMAT)          
*                                                                               
LNSPTS   DS    X                                                                
LMAXSPW  DS    X                                                                
LNSPWKS  DS    X                                                                
LDAYDSPL DS    X                                                                
*                                                                               
LBADDCHA DS    X                                                                
LBADD    EQU   1                                                                
LBCHA    EQU   2                                                                
*                                                                               
LFLAG    DS    X                                                                
LFREEZE  EQU   X'80'                                                            
LAFFDVT  EQU   X'40'                                                            
LFSTWK   EQU   X'20'                                                            
LNEWEEK  EQU   X'10'                                                            
LDELETE  EQU   X'08'                                                            
LORBIT   EQU   X'02'                                                            
LWEEKLY  EQU   X'01'                                                            
*                                                                               
LCHGIND  DS    X                   RECORD CHANGE INDICATOR                      
LSLN     EQU   X'01'                                                            
LDAYS    EQU   X'02'                                                            
LSTDATE  EQU   X'04'                                                            
LTIMES   EQU   X'08'                                                            
LTIMNOTX EQU   X'10'                                                            
LCST     EQU   X'20'                                                            
LNEWPRD  EQU   X'80'                                                            
*                                                                               
LORBDAY  DS    XL1                                                              
LORBTIM1 DS    XL2                                                              
LORBTIM2 DS    XL2                                                              
*                                                                               
LSTALIST DS    (MAXSTA)XL(L'BMKTSTA)  STATION LIST                              
         EJECT                                                                  
***********************************************************************         
* SCHEDULES TABLE                                                     *         
* TABLE ENTRY HAS THE FOLLOWING FORMAT :                              *         
* +0(1) BUYLINE NUMBER                                                *         
* +1(1) BUYLINE INDICATOR (LCHG = THIS BUYLINE WILL CHANGE)           *         
* +2(1) WEEK 1 CURRENT BUY RECORD NUMBER OF SPOTS                     *         
* +3(1) WEEK 1 PROPOSED NUMBER OF SPOTS ACCORDING TO NEW SCHEDULE     *         
* +4(2) WEEK 2                                                        *         
* +6(2) WEEK 3                                                        *         
* ..... ETC                                                           *         
***********************************************************************         
         SPACE 1                                                                
LSKEDENL EQU   2+2*MAXWEEKS        ENTRY LENGTH                                 
LSKEDS   DS    (MAXBLINE+1)XL(LSKEDENL)      TABLE                              
LCHG     EQU   X'80'               BUYLINE CHANGE INDICATOR                     
LADD     EQU   X'40'               ADD BUY ELEMENT(S) TO BUYLINE                
LDEL     EQU   X'20'               DELETE ALL SPOTS FROM BUYLINE                
LSKEDSL  EQU   *-LSKEDS            LENGTH OF TABLE                              
         SPACE 2                                                                
* SLINE ERROR TABLE                                                             
*                                                                               
ERRTAB   DS    (MAXERRS)XL5                                                     
         EJECT                                                                  
*                                                                               
*                   OTHER EQUATES                                               
*                                                                               
MAXWEEKS EQU   53                  MAXIMUM NUMBER OF WEEKS                      
MAXBELEM EQU   47                  65% OF MAXIMUM # OF BUY ELEMENTS             
MAXBKEYS EQU   52                  MAXIMUM BUY KEYS PER HEADER OBJECT           
MAXBLINE EQU   10                  MAXIMUM NUMBER OF BUYLINES/SCHEDULE          
MAXDEMOS EQU   14                                                               
MAXSTA   EQU   40                  MAXIMUM STATION TABLE ENTRIES                
MAXERRS  EQU   100                 MAXIMUM SLINE ERRORS PER HEADER              
MAXCOM   EQU   74                  MAXIMUM BUY PROGRAM COMMENT LEN              
LENBKEY  EQU   L'BUYKAM+L'BUYKCLT+L'BUYKPRD+L'BUYMSTA+L'BUYKEST                 
LIOS     EQU   2048                LENGTH OF IO AREA AS IN CONTROLLER           
FF       EQU   X'FF'                                                            
*                                                                               
*                   DSECTS                                                      
*                                                                               
*              DSECT TO COVER SLINE OBJECT                                      
*                                                                               
SLD      DSECT                     ** SLINE OBJECT DSECT **                     
SLSEQ    DS    CL8                 SEQUENCE NUMBER                              
SLODATE  DS    CL6                 ORIGINAL TRANSFER DATE                       
SLOTIME  DS    XL6                 ORIGINAL TRANSFER TIME                       
SLSTAT   DS    CL5                 STATION                                      
SLDAYTIM DS    XL62                ORBIT DAYTIME BLOCK                          
SLDAYPRT DS    CL1                 DAYPART CODE                                 
SLMASLEN DS    XL2                 2 BINARY SPOT LENGTHS (MASTER)               
SLLENGTH DS    CL2                 2 BINARY SPOT LENGTHS (TOTAL)                
SLCOST   DS    XL6                 BINARY COST                                  
SLPROG   DS    CL18                PROGRAM NAME                                 
SLOVERS  DS    XL4                 DEMO OVERRIDE BIT MASK                       
SLDATA   DS    CL524               VARIABLE DATA                                
SLLENQ   EQU   *-SLD               OBJECT LENGTH                                
*                                                                               
*                                                                               
*              DSECT TO COVER BUY LINE TABLE ENTRY                              
*                                                                               
BLND     DSECT                                                                  
BLNSEQ   DS    XL4                 SEQUENCE NUMBER                              
BLNDATE  DS    XL3                 TRANSFER DATE                                
BLNTIME  DS    XL3                 TRANSFER TIME                                
BLNMAS   DS    XL1                 MASTER PRODUCT CODE                          
BLNPIG   DS    XL1                 PIGGYBACK PRODUCT CODE                       
BLNLNQ   EQU   *-BLND              ENTRY LENGTH                                 
         SPACE 2                                                                
*              DSECT TO COVER DAYTIME BLOCK                                     
*                                                                               
DTD      DSECT                                                                  
DTDAY    DS    XL1                 SPOTPAK DAY BITS                             
DTDAYNUM DS    XL1                 DAY NUMBERS                                  
DTSTART  DS    XL2                 START TIME                                   
DTEND    DS    XL2                 END TIME                                     
DTLNQ    EQU   *-DTD               BLOCK ENTRY LENGTH                           
         EJECT                                                                  
*              EQUATES                                                          
*                                                                               
LENBLNTB EQU   255*BLNLNQ          SPARE MEMORY FOR BUYLINE TABLE               
LENFREE  EQU   LENBLNTB            AMOUNT OF SPARE MEMORY NEEDED                
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
****   ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
       ++INCLUDE SPGENWIPW                                                      
         EJECT                                                                  
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'139CTMAD06   05/06/03'                                      
         END                                                                    
