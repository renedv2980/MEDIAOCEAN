*          DATA SET CTMAD03    AT LEVEL 033 AS OF 05/01/02                      
*PHASE TA0C03A,*                                                                
         TITLE 'TA0C03 - $MAD DOWNLOAD GENERAL SPOT RECORDS'                    
TA0C03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,TA0C03,RA                                                      
*                                                                               
         USING CONTROLD,RC         CONTROLLER COMMON STORAGE                    
         USING APPLD,R7            FIRST APPLICATION COMMON STORAGE             
         USING USUKD,R8            US/UK APPLICATION COMMON STORAGE             
         USING OVERD,R9            OVERLAY SAVED STORAGE                        
*                                                                               
         L     RC,ACONTD           RC = A(CONTROLLER COMMON STORAGE)            
         L     R9,AOVER            R9 = A(OVERLAY SAVED STORAGE)                
*                                                                               
         ST    RD,SAVEDRD          SAVE STACK POINTER FOR RETURNING             
         EJECT                                                                  
* MAIN DRIVER - CALLS THE THREE SUBROUTINES THAT HANDLE EACH OF THE             
* THREE MODES THAT THE CONTROLLER CALLS AN OVERLAY WITH (START, MIDDLE,         
* AND END).                                                                     
*                                                                               
MAIN     DS    0H                                                               
         BAS   RE,INIT             INITIALIZE OVERLAY                           
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
* THIS ROUTINE INITIALIZES VARIABLES IN THIS OVERLAY THAT NEED                  
* INITIALIZATION.                                                               
*                                                                               
INIT     NTR1                                                                   
*                                                                               
*        INITIALIZE LOCAL WORKING STORAGE                                       
*                                                                               
*                                  INITIALIZE SYSTEM                            
         GOTO1 SETSYS,DMCB,=C'SPOT',=CL8'SPTDIR',=CL8'SPTFIL'                   
         BNE   EXIT                                                             
*                                                                               
INX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE START MODE.  IT VALIDATES THE INQUIRY              
* FILTER OBJECT PASSED BY ALINK AND RETURNS THE FIRST FRAME FULL OF             
* INQUIRY OBJECTS.                                                              
*                                                                               
PROCSTRT NTR1                                                                   
*                                  IF ACTION DOWNLOAD STATIONS                  
         CLC   PCACTION,=Y(ACDNLSTA)                                            
         BE    PS05                OR NEW STATION                               
         CLC   PCACTION,=Y(ACDNNSTA)                                            
         BNE   PS10                                                             
*                                                                               
PS05     BAS   RE,STARTSTA                                                      
         B     PSX                                                              
*                                  ELSE IF ACTION DOWNLOAD MARKETS              
PS10     CLC   PCACTION,=Y(ACDNLMKT)                                            
         BNE   PS20                                                             
         BAS   RE,STARTMKT                                                      
         B     PSX                                                              
*                                  ELSE IF ACTION DOWNLOAD STA COMBOS           
PS20     CLC   PCACTION,=Y(ACDNLCOM)                                            
         BNE   PS30                                                             
         BAS   RE,STARTCOM                                                      
         B     PSX                                                              
*                                                                               
PS30     DS    0H                                                               
*                                                                               
PSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE STARTS THE STATION DOWNLOAD.  IT VALIDATES THE REQUEST           
* OBJECT, FILLS IN THE START KEY AND CALLS FILLSTA.                             
*                                                                               
STARTSTA NTR1                                                                   
         GOTO1 GETITEM             VALIDATE REQUEST OBJECT                      
         BNE   EXIT                                                             
         CLC   TYPENUM,=A(ITSTAREQ)                                             
         BNE   ERRSTRQ                                                          
*                                  VALIDATE MEDIA                               
         GOTO1 VALIMED,DMCB,ADATA                                               
         BNE   ERROBJ                                                           
*                                                                               
         XC    CURRSTA,CURRSTA     CLEAR CURRENT STATION                        
*                                                                               
         XC    KEY,KEY             BUILD STATION START KEY                      
         LA    R4,KEY                                                           
         USING STAHDRD,R4                                                       
         MVI   STAKTYPE,C'S'                                                    
         MVC   STAKMED,QMED                                                     
*                                                                               
         CLI   QMED,C'T'           TV IS ONLY CABLE                             
         BE    SS2                 SO DON'T READ TV STATIONS                    
         CLI   QMED,C'X'           STRATA MIGHT SEND X FOR CABLE                
         BNE   SS4                 WHICH IS TV REALLY                           
*                                                                               
SS2      MVI   STAKMED,C'T'        CABLE ACTUALLY READS TV                      
         MVI   STAKCALL,C'0'       AND NUMERIC STATIONS ONLY                    
         DROP  R4                                                               
*                                                                               
SS4      MVI   MYSTATUS,1          SET MIDDLE OF STATIONS                       
         BAS   RE,FILLSTA          FILL FRAME WITH STATIONS                     
*                                                                               
         MVI   MYSTATUS,9          IF DONE AFTER ONE FRAME THEN SET             
         BAS   RE,PROCMID              GO TO PROCMID STATUS END OF DATA         
*                                                                               
SSX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE STARTS THE MARKET DOWNLOAD.  IT VALIDATES THE REQUEST            
* OBJECT, FILLS IN THE START KEY AND CALLS FILLMKT.                             
*                                                                               
STARTMKT NTR1                                                                   
         GOTO1 GETITEM             VALIDATE REQUEST OBJECT                      
         BNE   EXIT                                                             
         CLC   TYPENUM,=A(ITMKTREQ)                                             
         BNE   ERRMKRQ                                                          
*                                  VALIDATE MEDIA                               
         GOTO1 VALIMED,DMCB,ADATA                                               
         BNE   ERROBJ                                                           
*                                                                               
         XC    KEY,KEY             BUILD MARKET START KEY                       
         LA    R4,KEY                                                           
         USING MKTHDRD,R4                                                       
         MVI   MKTKTYPE,C'M'                                                    
         MVC   MKTKMED,QMED                                                     
*                                                                               
         CLI   MKTKMED,C'X'        THERE IS NO MEDIA X                          
         BNE   *+8                                                              
         MVI   MKTKMED,C'T'        IT'S A FUCKED UP CABLE REQUEST               
         DROP  R4                                                               
*                                                                               
         MVI   MYSTATUS,3          SET MIDDLE OF MARKETS                        
         BAS   RE,FILLMKT          FILL FRAME WITH MARKETS                      
*                                                                               
         MVI   MYSTATUS,9          IF DONE AFTER ONE FRAME THEN SET             
         BAS   RE,PROCMID              GO TO PROCMID STATUS END OF DATA         
*                                                                               
SMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE STARTS THE STATION COMBO DOWNLOAD.  IT VALIDATES THE             
* REQUEST OBJECT, FILLS IN THE START KEY AND CALLS FILLMKT.                     
*                                                                               
STARTCOM NTR1                                                                   
         GOTO1 GETITEM             VALIDATE REQUEST OBJECT                      
         BNE   EXIT                                                             
         CLC   TYPENUM,=A(ITCOMREQ)                                             
         BNE   ERRCORQ                                                          
*                                  VALIDATE MEDIA                               
         GOTO1 VALIMED,DMCB,ADATA                                               
         BNE   ERROBJ                                                           
*                                                                               
         XC    KEY,KEY             BUILD COMBO START KEY                        
         LA    R4,KEY                                                           
         USING CLKEY,R4                                                         
         MVI   CLKTYPE,X'0D'                                                    
         MVI   CLKSUB,X'5E'                                                     
         MVC   CLKAM,BAGYMED                                                    
         DROP  R4                                                               
*                                                                               
         MVI   MYSTATUS,4          SET MIDDLE OF STATION COMBOS                 
         BAS   RE,FILLCOM          FILL FRAME WITH STATION COMBOS               
*                                                                               
         MVI   MYSTATUS,9          IF DONE AFTER ONE FRAME THEN SET             
         BAS   RE,PROCMID              GO TO PROCMID STATUS END OF DATA         
*                                                                               
SCX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES MIDDLE MODE.  IT RETURNS THE NEXT FRAME FULL           
* OF REPORT DATA OBJECTS AND SETS THE LAST FRAME FLAG IF IT REACHES THE         
* END OF THE REPORT.                                                            
*                                                                               
PROCMID  NTR1                                                                   
         CLI   MYSTATUS,1          IF IN THE MIDDLE OF STATIONS                 
         BNE   PM10                                                             
*                                                                               
         L     R2,OBJDISP          THEN PUT EXISTING STATION OBJECT             
         GOTO1 PUTITEM,DMCB,ITSTARET,(R2),OUTOBJ                                
         BNE   EXIT                                                             
*                                                                               
         MVC   OUTOBJ(5),CURRSTA   BUILD NEW STATION ELEMENT                    
         MVC   OBJDISP,=F'5'                                                    
         XC    DISP000,DISP000     INITIALIZE POINTERS TO 000 CLIENT            
         XC    PAST000,PAST000                                                  
*                                                                               
         BAS   RE,FILLSTA          FILL FRAME WITH STATIONS                     
*                                                                               
         MVI   MYSTATUS,2          SET END OF STATIONS                          
*                                                                               
PM10     CLI   MYSTATUS,2          IF END OF STATIONS                           
         BNE   PM20                                                             
*                                                                               
         L     R2,OBJDISP          THEN PUT EXISTING STATION OBJECT             
         GOTO1 PUTITEM,DMCB,ITSTARET,(R2),OUTOBJ                                
         BNE   EXIT                                                             
*                                  IF END OF FRAME REACHED THEN DONE            
         CLI   EOFFLAG,C'Y'                                                     
         BE    EXIT                                                             
*                                                                               
         MVI   MYSTATUS,9          SET END OF DATA                              
*                                                                               
PM20     CLI   MYSTATUS,3          IF MIDDLE OF MARKETS                         
         BNE   PM30                                                             
*                                                                               
         BAS   RE,FILLMKT          FILL FRAME WITH MARKETS                      
*                                                                               
         MVI   MYSTATUS,9          SET END OF DATA                              
*                                                                               
PM30     CLI   MYSTATUS,4          IF MIDDLE OF STATION COMBOS                  
         BNE   PM40                                                             
*                                                                               
         BAS   RE,FILLCOM          FILL FRAME WITH STATION COMBOS               
*                                                                               
         MVI   MYSTATUS,9          SET END OF DATA                              
*                                                                               
*                                  PUT END OF DATA ITEM                         
PM40     GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                  IF END OF FRAME REACHED THEN DONE            
         CLI   EOFFLAG,C'Y'                                                     
         BE    EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         ELSE SET LAST FRAME INDICATOR                
*                                                                               
PMX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE PROCESSES THE END MODE.  IT CURRENTLY DOES NOTHING.              
*                                                                               
PROCEND  NTR1                                                                   
*                                                                               
PEX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE FILLS THE FRAME WITH STATION OBJECTS.                            
*                                                                               
FILLSTA  NTR1                                                                   
         L     R4,AIO              R4 = A(STATION KEY)                          
         USING STAHDRD,R4                                                       
*                                  READ NEXT STATION RECORD                     
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
*                                                                               
*                                  SAVE KEY OF RECORD IN KEY                    
FS10     MVC   KEY(L'STAKEY),0(R4)                                              
*                                                                               
         CLI   STAKTYPE,C'S'       IF END OF FILE THEN DONE                     
         BNE   FSX                                                              
         CLC   STAKMED,QMED                                                     
         BNE   FSX                                                              
*                                                                               
         CLC   STAKAGY,SIGNON2C    IF NOT SAME AGENCY THEN SKIP                 
         BNE   FS90                                                             
*                                                                               
         OC    CURRSTA,CURRSTA     IF NO CURRENT STATION SET                    
         BNZ   FS20                                                             
         MVC   CURRSTA,STAKCALL    THEN SET THIS STATION AS CURRENT             
         B     FS50                AND INITIALIZE OBJECT WITH STATION           
*                                                                               
FS20     CLC   STAKCALL,CURRSTA    ELSE IF SAME STATION                         
         BNE   FS30                                                             
*                                                                               
         CLC   STAKCLT,=C'000'     IF 000 CLIENT                                
         BNE   *+10                                                             
         MVC   DISP000,OBJDISP     THEN SAVE DISP TO 000 CLIENT                 
*                                                                               
         LA    RF,OUTOBJ           ADD THIS CLT/MKT PAIR TO OBJECT              
         A     RF,OBJDISP                                                       
         MVC   0(3,RF),STAKCLT                                                  
         MVC   3(4,RF),SMKT                                                     
         L     R1,OBJDISP                                                       
         LA    R1,7(R1)                                                         
         ST    R1,OBJDISP                                                       
*                                                                               
         LA    R1,7(RF)            CHECK 000 CLIENT FOR STATION                 
         BAS   RE,NEWSTA              FORMAT INFO                               
*                                                                               
         CLC   STAKCLT,=C'000'     IF 000 CLIENT                                
         BNE   *+10                                                             
         MVC   PAST000,OBJDISP     THEN SAVE DISP PAST 000 CLIENT               
*                                                                               
         B     FS90                READ NEXT CLIENT OVERRIDE                    
*                                                                               
FS30     MVC   CURRSTA,STAKCALL    ELSE SAVE NEW STATION AS CURRENT             
*                                                                               
         BAS   RE,CHECK000         MAKE SURE 000 IS LAST CLIENT                 
*                                                                               
         L     R2,OBJDISP          THEN PUT EXISTING STATION OBJECT             
         GOTO1 PUTITEM,DMCB,ITSTARET,(R2),OUTOBJ                                
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    EXIT                                                             
*                                                                               
FS50     XC    DISP000,DISP000     INITIALIZE POINTERS TO 000 CLIENT            
         XC    PAST000,PAST000                                                  
*                                                                               
         CLC   STAKCLT,=C'000'     IF 000 CLIENT                                
         BNE   *+10                                                             
         MVC   DISP000,=F'5'       THEN SAVE DISP TO 000 CLIENT                 
*                                                                               
         MVC   OUTOBJ(5),CURRSTA   BUILD NEW STATION ELEMENT                    
         MVC   OUTOBJ+5(3),STAKCLT                                              
         MVC   OUTOBJ+8(4),SMKT                                                 
         MVC   OBJDISP,=F'12'                                                   
         LA    R1,OUTOBJ+12        POINT TO START OF LAST DATA INPUT            
         BAS   RE,NEWSTA                                                        
*                                                                               
         CLC   STAKCLT,=C'000'     IF 000 CLIENT                                
         BNE   *+10                                                             
         MVC   PAST000,OBJDISP     THEN SAVE DISP PAST 000 CLIENT               
*                                                                               
*                                  GET NEXT STATION RECORD                      
FS90     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,AIO                  
         B     FS10                LOOP BACK                                    
*                                                                               
FSX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*                                                                               
*        THIS ROUTINES DOWNLOADS CATAGORY/FORMAT/REP/AFFILIATE                  
*        FOR NEW STATION DOWNLOAD FROM THE GLOBAL MASTER RECORD                 
*        R1 - LOCATION TO PLACE DATA                                            
*        R4 - STATION RECORD                                                    
*                                                                               
         USING STAHDRD,R4                                                       
NEWSTA   NTR1                                                                   
         CLC   PCACTION,=Y(ACDNNSTA)                                            
         BNE   NSX                 IF DOWNLOADING NEW STATION                   
         CLC   STAKCLT,=C'000'                                                  
         BNE   NSX                 AND THIS IS THE GLOBAL MASTER REC            
         CLC   SFORMAT,BLANKS                                                   
         BH    NS10                IF THERE IS INFO IN FORMAT                   
         CLC   SCATGRY,BLANKS                                                   
         BH    NS10                CATAGORY OR REP                              
         CLC   SCONREP,=C'000'                                                  
         BNE   NS10                                                             
         CLC   SNETWRK,BLANKS '                                                 
         BNH   NSX                                                              
*                                                                               
NS10     MVC   0(4,R1),SCATGRY                                                  
         MVC   4(4,R1),SFORMAT                                                  
         MVC   8(3,R1),SCONREP                                                  
         MVC   11(3,R1),SNETWRK                                                 
         L     RF,OBJDISP                                                       
         LA    RF,14(RF)                                                        
         ST    RF,OBJDISP                                                       
*                                                                               
NSX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE MAKES SURE THAT THE '000' CLIENT OVERRIDE FOR A                  
* STATION'S MARKET IS THE LAST IN THE LIST.  IF IT ISN'T IT WILL                
* SWAP ALL CLIENT OVERRIDES THAT ARE FOUND BEYOND THE '000' CLIENT              
* WITH THE '000' CLIENT.                                                        
*                                                                               
CHECK000 NTR1                                                                   
         OC    PAST000,PAST000     IF NO 000 CLIENT THEN DONE                   
         BZ    CKX                                                              
*                                                                               
         CLC   PAST000,OBJDISP     IF ALREADY AT END THEN DONE                  
         BE    CKX                                                              
*                                                                               
         LA    R2,OUTOBJ           MOVE '000' CLIENT TO TEMP                    
         A     R2,DISP000                                                       
         MVC   BLOCK(21),0(R2)                                                  
*                                                                               
         LA    R3,OUTOBJ           MOVE CLIENTS PAST '000' TO SPOT              
         A     R3,PAST000             WHERE '000' IS                            
         L     RF,OBJDISP                                                       
         S     RF,PAST000                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(R3)                                                    
*                                                                               
         LA    R2,OUTOBJ           MOVE TEMP TO END                             
         A     R2,DISP000                                                       
         A     R2,OBJDISP                                                       
         S     R2,PAST000                                                       
         MVC   0(21,R2),BLOCK                                                   
*                                                                               
CKX      B     XIT                                                              
         EJECT                                                                  
* THIS ROUTINE SCANS THE STATION FILE AND PUTS MARKET OBJECTS UNTIL             
* THE END OF THE STATION FILE.                                                  
*                                                                               
FILLMKT  NTR1                                                                   
         L     R4,AIO              R4 = A(MARKET KEY)                           
         USING MKTHDRD,R4                                                       
*                                  READ NEXT MARKET RECORD                      
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
*                                                                               
*                                  SAVE KEY OF RECORD IN KEY                    
FM10     MVC   KEY(L'MKTKEY),0(R4)                                              
*                                                                               
         CLI   MKTKTYPE,C'M'       IF END OF FILE THEN DONE                     
         BNE   FMX                                                              
         CLC   MKTKMED,QMED                                                     
         BNE   FMX                                                              
*                                                                               
         CLC   MKTKAGY,SIGNON2C    IF NOT SAME AGENCY THEN SKIP                 
         BNE   FM90                                                             
*                                                                               
         MVC   OUTOBJ(4),MKTKMKT   BUILD MARKET OBJECT                          
         MVC   OUTOBJ+4(24),MKTNAME                                             
         OC    OUTOBJ+4(24),=CL24' '                                            
*                                  PUT MARKET OBJECT TO OUTPUT                  
         GOTO1 PUTITEM,DMCB,ITMKTRET,28,OUTOBJ                                  
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    EXIT                                                             
*                                  GET NEXT MARKET RECORD                       
FM90     GOTO1 DATAMGR,DMCB,(0,=C'DMRSEQ'),=C'STATION',KEY,AIO                  
         B     FM10                LOOP BACK                                    
*                                                                               
FMX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
* THIS ROUTINE SCANS THE SPOT FILE AND PUTS STATION COMBO OBJECTS               
* UNTIL THE END OF THE SPOT FILE.                                               
*                                                                               
FILLCOM  NTR1                                                                   
         L     R4,AIO              R4 = A(STATION COMBO KEY)                    
         USING COMRECD,R4                                                       
*                                                                               
         GOTO1 HIGH                READ NEXT STATION COMBO RECORD               
*                                                                               
FC10     GOTO1 GETREC              GET RECORD AND SAVE KEY                      
*                                                                               
         MVC   KEY(L'CLKEY),0(R4)  SAVE KEY OF RECORD IN KEY                    
*                                                                               
         CLI   CLKTYPE,X'0D'       IF END OF FILE THEN DONE                     
         BNE   FCX                                                              
         CLI   CLKSUB,X'5E'                                                     
         BNE   FCX                                                              
         CLC   CLKAM,BAGYMED                                                    
         BNE   FCX                                                              
*                                                                               
         MVC   OUTOBJ(10),CLKMKAL  BUILD COMBO KEY INTO OBJECT                  
         CLI   OUTOBJ+3,0                                                       
         BNE   *+8                                                              
         MVI   OUTOBJ+3,C' '                                                    
         CLI   OUTOBJ+4,0                                                       
         BNE   *+8                                                              
         MVI   OUTOBJ+4,C' '                                                    
*                                  GET DESCRIPTION ELEMENT                      
         GOTO1 GETELEM,DMCB,CLDELQ                                              
         USING CLDELEM,R6                                                       
         BNE   FC90                                                             
*                                  ADD COMBO DESC TO OBJECT                     
         MVC   OUTOBJ+10(CLDLENEQ),CLDDESC                                      
*                                  POINT R3 TO FIRST PLACE TO PUT STAS          
         LA    R3,OUTOBJ+10+L'CLDDESC                                           
*                                  GET FIRST STATIONS ELEMENT                   
         GOTO1 GETELEM,DMCB,CLSELQ                                              
         USING CLSELEM,R6                                                       
         BNE   FC90                                                             
*                                                                               
FC50     ZIC   R2,CLSLEN           COMPUTE LENGTH OF STATIONS DATA              
         S     R2,=F'2'                                                         
*                                                                               
         STC   R2,BYTE             STORE LENGTH IN OBJECT                       
         GOTO1 HEXOUT,DMCB,BYTE,0(R3),1                                         
         LA    R3,2(R3)                                                         
*                                                                               
         BCTR  R2,0                MOVE STATION DATA TO OBJECT                  
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),CLSFORM                                                  
*                                                                               
         LA    R3,1(R3,R2)         BUMP R4 TO NEXT PLACE TO PUT STAS            
*                                                                               
         GOTO1 NEXTELEM            REPEAT UNTIL END OF STATIONS ELEMS           
         BE    FC50                                                             
*                                                                               
         LA    R2,OUTOBJ           PUT COMBO OBJECT TO OUTPUT                   
         SR    R3,R2                                                            
         GOTO1 PUTITEM,DMCB,ITCOMRET,(R3),OUTOBJ                                
         BNE   EXIT                                                             
*                                                                               
         CLI   EOFFLAG,C'Y'        IF END OF FRAME REACHED THEN DONE            
         BE    EXIT                                                             
*                                                                               
FC90     GOTO1 SEQ                 GET NEXT MARKET RECORD                       
         B     FC10                LOOP BACK                                    
*                                                                               
FCX      B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
*        INVALID STATION REQUEST OBJECT                                         
ERRSTRQ  MVC   APPLERR,=Y(ER03STRQ)                                             
         B     ERROBJ                                                           
*        INVALID MARKET REQUEST OBJECT                                          
ERRMKRQ  MVC   APPLERR,=Y(ER03MKRQ)                                             
         B     ERROBJ                                                           
*        INVALID STATION COMBO REQUEST OBJECT                                   
ERRCORQ  MVC   APPLERR,=Y(ER03CORQ)                                             
         B     ERROBJ                                                           
*                                  RETURN APPL ERRORS IN ERR OBJECT             
ERROBJ   GOTO1 HEXOUT,DMCB,APPLERR,FULL,2                                       
         GOTO1 PUTITEM,DMCB,ITAMFMER,4,FULL                                     
         BNE   EXIT                                                             
*                                  PUT END OF DATA ITEM                         
         GOTO1 PUTITEM,DMCB,ITEOD,0                                             
         BNE   EXIT                                                             
*                                                                               
         MVI   MDLAST,C'Y'         SET LAST FRAME INDICATOR                     
*                                                                               
EXIT     L     RD,SAVEDRD          RESTORE RD                                   
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
*                                                                               
BLANKS   DC    C'                                '                              
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
* CTMADWORKD                                                                    
       ++INCLUDE CTMADWORKD                                                     
         EJECT                                                                  
* CTMADEQUS                                                                     
       ++INCLUDE CTMADEQUS                                                      
         EJECT                                                                  
* CTMADDSECT                                                                    
       ++INCLUDE CTMADDSECT                                                     
         EJECT                                                                  
STAHDRD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
MKTHDRD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
COMRECD  DSECT                                                                  
       ++INCLUDE SPGENCOMBO                                                     
         EJECT                                                                  
* OVERLAY WORKING STORAGE                                                       
*                                                                               
OVERD    DSECT                                                                  
SAVEDRD  DS    A                   RD UPON ENTRY TO OVERLAY                     
MYSTATUS DS    C                   INDICATOR OF HOW TO RESTART OVERLAY          
OUTOBJ   DS    XL1024              AREA FOR STATION OUTPUT OBJECT               
OBJDISP  DS    F                   DISP WITHIN OUTOBJ TO NEXT CLT/MKT           
DISP000  DS    F                   DISP WITHIN OUTOBJ TO '000' CLIENT           
PAST000  DS    F                   DISP WITHIN OUTOBJ PAST '000' CLI            
CURRSTA  DS    CL5                 CURRENT STATION                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'033CTMAD03   05/01/02'                                      
         END                                                                    
