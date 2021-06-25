*          DATA SET RESFM19A   AT LEVEL 151 AS OF 05/01/02                      
*PHASE T81819A,*                                                                
*INCLUDE LOADER                                                                 
         TITLE 'T81819 - RESFM19 - KATZ DAILY STATION ACTIVITY REPORT'          
*                                                                               
**********************************************************************          
*                                                                    *          
*        RESFM19 (T81819) --- KATZ DAILY STATION ACTIVITY REPORT     *          
*                                                                    *          
*  DEC11/96 (BU ) --- EXPAND SIZE OF TSAR SPACE                      *          
*                                                                    *          
*  JAN18/00 (MLB) --- SENDING REPORTS BY EMAIL                       *          
*                                                                    *          
* ------------------------------------------------------------------ *          
**********************************************************************          
                                                                                
T81819   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1819**,RA,R7   ** NOTE BASE REGISTERS **                     
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=A(REGSPECS)                                                  
         ST    R1,SPECS                                                         
*                                                                               
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
****************************************************************                
****************************************************************                
*              PROCESS REPORT                                  *                
****************************************************************                
****************************************************************                
         SPACE 1                                                                
PREP     DS    0H                                                               
*                                                                               
         BAS   RE,INIT             INITIALIZE                                   
*                                                                               
         BAS   RE,READRECS         READ CONTRACTS-PUT TO TSAR                   
*                                                                               
         BAS   RE,PRNTRECS         GET FROM TSAR-PRINT REPORT                   
*                                                                               
         BAS   RE,CLOSEUP          CLOSE IT UP                                  
*                                                                               
         B     XIT                                                              
                                                                                
******************************************************************              
******************************************************************              
                                                                                
                                                                                
                                                                                
* CLOSE UP SHOP                                                                 
CLOSEUP  NTR1                                                                   
         L     R1,TSARBUFF                       RETURN MEMORY                  
         L     R2,TSARBUFL                                                      
         FREEMAIN RC,LV=(2),A=(1)                                               
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
                                                                                
         EJECT                                                                  
******************************************************************              
                                                                                
* READ CONTRACT RECORDS                                                         
READRECS NTR1                                                                   
*                                                                               
         CLI   OFFICVER,C'Y'       OFFICE VERSION?                              
         BNE   RDR10                                                            
                                                                                
* OFFICE VERSION                                                                
         MVI   RCSUBPRG,1                                                       
         L     R1,ATWA                                                          
         USING CONHEADH-64,R1                                                   
         LA    R2,KEY                                                           
         USING RCONRTYP,R2                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,X'AC'           USE PASSIVE POINTER                          
         MVC   RCONRREP,TWAAGY                                                  
         DROP  R1                                                               
         MVC   RCONROFF,OFFICE                                                  
         MVC   RCONRTEM,TEAM                                                    
         MVC   RCONRSAL,SLSPRSN                                                 
         MVC   RCONRSTA,STATION                                                 
         GOTO1 HIGH                                                             
         B     RDR05                                                            
*                                                                               
NXTREC2  GOTO1 SEQ                                                              
                                                                                
*                                                                               
RDR05    LA    R2,KEY              RESET R2 -> KEY                              
         USING RCONRTYP,R2                                                      
         CLC   KEY(3),KEYSAVE                                                   
         BNE   XIT                                                              
         CLI   OFFICE,0                                                         
         BE    *+14                                                             
         CLC   RCONROFF,OFFICE                                                  
         BNE   XIT                                                              
         CLI   TEAM,0                                                           
         BE    *+14                                                             
         CLC   RCONRTEM,TEAM                                                    
         BNE   NXTREC2                                                          
         CLI   SLSPRSN,0                                                        
         BE    *+14                                                             
         CLC   RCONRSAL,SLSPRSN                                                 
         BNE   NXTREC2                                                          
         CLI   STATION,0                                                        
         BE    *+14                                                             
         CLC   RCONRSTA,STATION                                                 
         BNE   NXTREC2                                                          
         EJECT                                                                  
                                                                                
* CHECK FOR NONGRAPHNET STATION                                                 
         L     R3,=A(NONGRAPH)     NONGRAPHNET STATION TABLE                    
RDR7     CLI   0(R3),X'FF'         END OF FILE                                  
         BE    RDR8                                                             
         CLI   0(R3),0                                                          
         BE    RDR8                                                             
         CLC   0(5,R3),RCONRSTA                                                 
         BE    NXTREC2             A MATCH - SKIP RECORD                        
         LA    R3,5(R3)            BUMP TABLE                                   
         B     RDR7                                                             
*                                                                               
                                                                                
RDR8     MVC   MYKEYSV,KEY         SAVE PASSIVE KEY FOR SEQ READ                
         B     RDR22                                                            
                                                                                
* - STATION VERSION                                                             
RDR10    L     R1,ATWA                                                          
         USING CONHEADH-64,R1      BASE SCREEN                                  
         XC    KEY,KEY                                                          
         MVI   KEY,X'0C'                                                        
         MVC   KEY+2(2),TWAAGY                                                  
         GOTO1 HIGH                                                             
         B     RDR20                                                            
         DROP  R1                                                               
*                                                                               
NXTREC   CLI   OFFICVER,C'Y'       IF OFFICE VERSION                            
         BE    NXTREC2             GO TO TIS SEQ                                
         GOTO1 SEQ                                                              
*                                                                               
RDR20    CLC   KEY(4),KEYSAVE                                                   
         BNE   XIT                                                              
         CLI   STATION,0                                                        
         BE    RDR21                                                            
         CLC   KEY+6(5),STATION                                                 
         BNE   NXTREC                                                           
         B     RDR22                                                            
                                                                                
* CHECK FOR NONGRAPHNET STATION                                                 
RDR21    L     R3,=A(NONGRAPH)     NONGRAPHNET STATION TABLE                    
RDR21B   CLI   0(R3),X'FF'         END OF FILE                                  
         BE    RDR22                                                            
         CLI   0(R3),0                                                          
         BE    RDR22                                                            
         CLC   0(5,R3),KEY+6                                                    
         BE    NXTREC              A MATCH - SKIP RECORD                        
         LA    R3,5(R3)            BUMP TABLE                                   
         B     RDR21B                                                           
                                                                                
RDR22    DS    0H                                                               
         OI    DMINBTS,X'08'     NEED DELETES- X'AC' KEYS NOT DELETED           
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'FF'-X'08'   TURN OFF DELETE BITS                       
         DROP  R2                                                               
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         TM    RCONCNTL,X'80'        IF DELETED/SKIP                            
         BO    NXTREC                                                           
         EJECT                                                                  
                                                                                
*************************************************************                   
* CONTRACT MUST BE GRAPHNET AND                                                 
* CONTRACT  MUST BE LAST SENT BY REP TODAY OR CONFIRMED TODAY                   
                                                                                
         TM    RCONMODR+1,X'40'    ...MUST BE GRAPHNET                          
         BNO   NXTREC                                                           
                                                                                
         MVI   ELCODE,X'20'        ...AND MUST BE LAST SENT BY REP              
         BAS   RE,GETEL                                                         
         BNE   RDR25                                                            
         USING RCONSEND,R6                                                      
         TM    RCONSENF,X'80'      LAST SENT BY REP?                            
         BZ    RDR25                                                            
         CLC   TODAY2,RCONSRDT     SENT BY REP TODAY?                           
         BE    RDR28                                                            
                                                                                
RDR25    L     R6,AIO              ...OR CONFIRMED TODAY                        
         MVI   ELCODE,X'1F'                                                     
         BAS   RE,GETEL                                                         
         BNE   NXTREC                                                           
         USING RCONXEL,R6                                                       
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BNO   NXTREC                                                           
                                                                                
         MVI   ELCODE,X'22'        MOD DATE/TIME ELEM                           
         L     R6,AIO                                                           
         BAS   RE,GETEL                                                         
         BNE   NXTREC                                                           
         USING RMODELEM,R6                                                      
         CLC   TODAY2,RMODEL1D     TODAY?                                       
         BNE   NXTREC                                                           
                                                                                
                                                                                
*******************************************************                         
* OTHER FILTERS                                                                 
RDR28    L     R6,AIO                                                           
         USING RCONREC,R6                                                       
                                                                                
         CLC   =C'ACC-',RCONBUYR   SKIP ACC CONTRACTS                           
         BE    NXTREC                                                           
                                                                                
                                                                                
         TM    RCONMODR,X'08'      WAS/IS TAKEOVER                              
         BO    NXTREC              EXCLUDE WAS/IS TAKEOVER                      
                                                                                
         MVI   ELCODE,X'12'        EXCLUDE FORECAST CONTRACTS                   
         BAS   RE,GETEL                                                         
         BNE   RDR30                                                            
         USING RSARXCO,R6                                                       
         TM    RSARXFLG,X'08'      IF ITS FORECAST                              
         BO    NXTREC                                                           
         B     RDR30                                                            
         EJECT                                                                  
******************************************************                          
* RECORD PASSED FILTERS - FILL SORTREC KEY/DATA FIELDS                          
*                                                                               
RDR30    LA    RE,SRTREC          CLEAR RECORD AREA                             
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         CLI   OFFICVER,C'Y'       OFFICE VERSION                               
         BNE   RDRSTAT                                                          
         B     RDROFFIC                                                         
                                                                                
* SORTREC KEY FOR OFFICE VERSION ***********************                        
RDROFFIC MVC   PNDKOOF,RCONKOFF    OFFICE                                       
         MVC   PNDKOTM,RCONTEM     TEAM                                         
         MVC   PNDKOSL,RCONSAL     SALESPERSON                                  
         MVC   PNDKOSTA,RCONKSTA   STATION                                      
         MVC   PNDKOAGY,RCONKAGY   AGENCY                                       
         MVC   PNDKOADV,RCONKADV   ADVERTISER                                   
         MVC   PNDKOCON,RCONKCON   CONTRACT#                                    
         B     RDR31                                                            
                                                                                
* SORTREC KEY STATION VERSION ***************************                       
RDRSTAT  MVC   PNDKREP,RCONKREP    REP                                          
         MVC   PNDKOFF,RCONKOFF    OFFICE                                       
         MVC   PNDKAGY,RCONKAGY    AGENCY                                       
         MVC   PNDKADV,RCONKADV    ADVERTISER                                   
         MVC   PNDKSLS,RCONSAL     SALESPERSON                                  
         MVC   PNDKSTAT,RCONKSTA   STATION                                      
         MVC   PNDKCON,RCONKCON    CONTRACT #                                   
                                                                                
                                                                                
* SORTREC DATA ********************************************                     
RDR31    UNPK  PNDCON(9),RCONKCON(5)          CONTRACT NUMBER (PWO)             
         MVI   PNDCON+8,0                     CLEAR SIGN BYTE                   
* GET RID OF LEADING ZEROS OF CONTRACT NUMBER                                   
         XC    WORK,WORK                                                        
         LA    R1,8                                                             
         LA    RE,PNDCON                                                        
RDR32    CLI   0(RE),C'0'                                                       
         BNE   RDR33                                                            
         LA    RE,1(RE)                                                         
         BCT   R1,RDR32                                                         
         DC    H'0'                CONTRACT # CAN'T BE ALL ZEROS                
RDR33    BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(RE)                                                    
         MVC   PNDCON,WORK                                                      
                                                                                
                                                                                
         BAS   RE,GETSLSNM                  SALSPERSON NAME                     
         MVC   PNDSLSNM,WORK                SALESMAN NAME                       
         MVC   PNDDADV,RCONKADV             ADVERTISER                          
         BAS   RE,GETADVR                                                       
         MVC   PNDADVN,WORK                 ADVERTISER NAME                     
         BAS   RE,FILLPROD                  RETURNS PROD IN WORK                
         MVC   PNDPROD,WORK                                                     
         BAS   RE,FILLAGY                   RETURNS AGY NAME IN WORK            
         MVC   PNDDAGY,WORK                                                     
         MVC   PNDBUYER,RCONBUYR            BUYER                               
         MVC   PNDFLITE,RCONDATE            FLITE DATES                         
*                                                                               
         CLI   OFFICVER,C'Y'       IF OFFICE VERSION                            
         BNE   RDR38                                                            
         MVC   WORK(2),RCONKREP    PASS REP AND STATION                         
         MVC   WORK+2(5),RCONKSTA                                               
         BAS   RE,CHKMKTNM         CHK MARKET NAME                              
         MVC   PNDMKTNM,WORK                                                    
*                                                                               
                                                                                
RDR38    DS    0H                                                               
* GET CONTRACT $ INTO FULL                                                      
         XC    FULL,FULL                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,3                                                         
         BAS   RE,GETEL                                                         
         B     *+8                                                              
RDR40    BAS   RE,NEXTEL                                                        
         BNE   RDR50                                                            
         USING RCONBKEL,R6                                                      
         SR    R1,R1                                                            
         MVC   DUB(4),RCONBKAM                                                  
         L     R1,DUB                                                           
         A     R1,FULL                                                          
         ST    R1,FULL                                                          
         B     RDR40                                                            
* - FULL HAS $ AMOUNT                                                           
RDR50    MVC   PNDDOLS,FULL                                                     
                                                                                
                                                                                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'1F'        EXTENED DESCRIPTION ELEM                     
         BAS   RE,GETEL                                                         
         BNE   RDR55                                                            
         USING RCONXEL,R6                                                       
         MVI   PNDMOD,C'N'         SET MOD TO N=NEW=NEVER CONFIRMED             
         TM    RCONCONF,X'60'      NOT CONFIRMED NOW OR PREVIOUS ?              
         BZ    RDR55                                                            
         MVI   PNDMOD,C'U'         SET MOD TO U=NOT CONFIRMED NOW               
         TM    RCONCONF,X'40'      CONFIRMED NOW?                               
         BZ    *+8                                                              
         MVI   PNDMOD,0            YES/CONFIRMED                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'22'        CONFIRMED -  GET DATE                        
         BAS   RE,GETEL                                                         
         BE    RDR54                                                            
         L     R6,AIO              NO X'22' GO GET X'20'                        
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDR55               PER BILL UHR/IT JUST AIN'T THERE             
         USING RCONSEND,R6                                                      
         MVC   PNDCNFDT,RCONSSDT                                                
         MVC   PNDCNFTM,RCONSSTI                                                
         B     RDR55                                                            
         USING RMODELEM,R6                                                      
RDR54    MVC   PNDCNFDT,RMODEL1D   DATE                                         
         MVC   PNDCNFTM,RMODEL1T   TIME                                         
         DROP  R6                                                               
                                                                                
* SENT BY REP DATE/TIME                                                         
RDR55    L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   RDR60                                                            
         USING RCONSEND,R6                                                      
         MVC   PNDRSDT,RCONSRDT    DATE                                         
         MVC   PNDRSTM,RCONSRTI    TIME                                         
*                                                                               
RDR60    DS    0H                                                               
* MODIFICATIO/VERION NUMBER                                                     
         L     R6,AIO                         VERSION NUMBER                    
         USING RCONREC,R6                                                       
         CLI   PNDMOD,C'N'         NEVER CONFIRMED ?                            
         BE    RDR62                                                            
         CLI   PNDMOD,C'U'         UNCONFIRMED ?                                
         BE    RDR62                                                            
         MVI   PNDMOD,0                                                         
         CLI   RCONMOD,X'FF'                  MODIFICATION                      
         BE    *+10                                                             
         MVC   PNDMOD,RCONMOD                                                   
         USING RCONSEND,R6                                                      
* CHECK SEND ELEMENT                                                            
RDR62    MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         CLC   RCONSRV,RCONSSV     REP OR STATION VERSION HIGHER?               
         BH    RDR66                                                            
* STATION                                                                       
         MVC   PNDVER,RCONSSV                                                   
         B     RDR70                                                            
* REP                                                                           
RDR66    DS    0H                                                               
         MVC   PNDVER,RCONSRV                                                   
         B     RDR70                                                            
*                                                                               
                                                                                
                                                                                
********************************************************                        
RDR70    DS    0H                                    AGE                        
                                                                                
         B     RDR100              PASS REC TO TSAROFF                          
         EJECT                                                                  
* GET MARKET NAME - SAVE STATION/NAME IN MKTSAVE                                
CHKMKTNM NTR1                                                                   
         LA    RE,12               ROOM FOR 12 STATIONS                         
         LA    RF,MKTSAVE                                                       
MKTLOOP  CLI   0(RF),0             DO WE ALREADY HAVE IT?                       
         BNE   CHKMKT10            NO - AND TABLE IS FULL                       
         LA    RE,MKTSAVE             - CLEAR TABLE                             
         LA    RF,MKTSLNE                                                       
         XCEF                                                                   
         B     CHKMKT20               - GET NAME AND SAVE IT                    
*                                                                               
CHKMKT10 CLC   WORK+2(5),0(RF)     MATCH ON STATION                             
         BE    GOTIT               GOT IT                                       
         LA    RF,25(RF)                                                        
         BCT   RE,MKTLOOP                                                       
         B     CHKMKT20            NO CAN FIND/GET IT                           
*                                                                               
GOTIT    MVC   WORK(20),5(RF)      PASS MKT NAME IN WORK                        
         B     CHKMKTX             AND EXIT                                     
                                                                                
* - DON'T HAVE NAME IN TABLE                                                    
CHKMKT20 LA    RE,12               FIND BLANK SAVE SPACE                        
         LA    RF,MKTSAVE                                                       
CHKMKT30 CLI   0(RF),0                                                          
         BE    CHKMKT40                                                         
         LA    RF,25(RF)                                                        
         BCT   RE,CHKMKT30                                                      
         DC    H'0'                SHOULD NEVER GET HERE                        
CHKMKT40 DS    0H                                                               
         MVC   0(5,RF),WORK        STATION                                      
         BAS   RE,GETFAXN          RETURNS MARKET NAME IN WORK                  
         MVC   5(20,RF),WORK       MARKET NAME                                  
CHKMKTX  B     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* ADD RECORD TO TSAROFF                                                         
*                                                                               
RDR100   DS    0H                                                               
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
         LA    RE,MYTSREC                                                       
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         LA    RF,MYTSREC                                                       
         LA    R1,SRTLENE                                                       
         LA    RE,SRTREC                                                        
         MOVE  ((RF),(R1)),(RE)                                                 
                                                                                
         MVI   TSOFFACT,TSAADD                                                  
         LA    RE,MYTSREC                                                       
         ST    RE,TSAREC                                                        
         GOTO1 ATSAROFF,(R2)                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,MYCOUNT                                                       
         LA    R1,1(R1)                                                         
         ST    R1,MYCOUNT                                                       
*****    GOTO1 =V(PRNTBL),DMCB,=C'TSR',MYTSREC,C'DUMP',20,=C'1D'                
*                                                                               
         L     R6,AIO              RESET FOR SEQUENTIAL READ                    
         MVC   KEY,0(R6)                                                        
         CLI   OFFICVER,C'Y'       IF OFFICE VERSION                            
         BNE   *+10                                                             
         MVC   KEY,MYKEYSV         USE SAVED PASSIVE KEY                        
         GOTO1 HIGH                                                             
         B     NXTREC              GET NEXT RECORD                              
*                                                                               
RDRX     B     XIT                                                              
         DROP  R2                                                               
MYCOUNT  DS    F                   TESTING                                      
         EJECT                                                                  
*********************************************************************           
* - GET RECS FROM TSAROFF                                                       
*                                                                               
PRNTRECS NTR1                                                                   
         XC    PREVIOUS,PREVIOUS                                                
         LA    R5,TSAREA                                                        
         USING TSARD,R5                                                         
         LA    RE,MYTSREC          CLEAR I/O AREA                               
         LA    RF,SRTLENE                                                       
         XCEF                                                                   
         MVI   TSOFFACT,TSAGET     READ BY NUMBER                               
         LA    R0,1                                                             
PRT20    STH   R0,TSRNUM                                                        
         LA    R0,MYTSREC                                                       
         ST    R0,TSAREC                                                        
         GOTO1 ATSAROFF,(R5)                                                    
         TM    TSERRS,TSEEOF                  ,,END OF FILE?                    
         BO    PRTX                                                             
                                                                                
         LA    RF,SRTREC                MOVE RECORD TO SRTREC AREA              
         LA    RE,MYTSREC                                                       
         LA    R1,SRTLENE                                                       
         MOVE  ((RF),(R1)),(RE)                                                 
*                                                                               
* CHECKFLG NTR1                                                                 
         OC    PREVIOUS,PREVIOUS                  FIRST TIME                    
         BNZ   PRT60                                                            
         MVI   PREVIOUS,1                                                       
         MVC   STAHEAD,PNDKSTAT                                                 
         MVC   REPHEAD,PNDKREP                                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
*                                                                               
         MVI   RSTAKTYP,X'02'                                                   
         CLI   OFFICVER,C'Y'       IF OFFICE VERSION                            
         BNE   GETFLG10                                                         
*                                                                               
         MVC   RSTAKREP,WORK       OFFICE VER PASSES ITS OWN REP                
         MVC   RSTAKSTA,WORK+2     OFFICE VER PASSES ITS OWN STATION            
         B     GETFLG12                                                         
*                                                                               
GETFLG10 MVC   RSTAKREP,PNDKREP    REP                                          
         MVC   RSTAKSTA,PNDKSTAT   STATION CALL LETTERS                         
GETFLG12 GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     NOT CHECKING LAST POSITION(A,F,ETC)          
         BNE   SKIPFLG                                                          
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'25'        EMAIL ELEMENT                                
         BAS   RE,GETEL                                                         
         BNE   SKIPFLG                                                          
         USING RSTAEML,R6                                                       
*                                                                               
         TM    RSTAFLG,RSTAFLGQ    SEND BY EMAIL ?                              
         BZ    SKIPFLG                                                          
         BAS   RE,EMAILIT                                                       
         DROP  R6                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     PRT35                                                            
SKIPFLG  MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         EJECT                                                                  
*                                                                               
*                                                                               
         CLI   OFFICVER,C'Y'       OFFICE VERSION?                              
         BNE   PRT30A                                                           
                                                                                
***** OFFICE VERSION ************                                               
         OC    PREVIOUS,PREVIOUS                  FIRST TIME                    
         BNZ   *+10                                                             
         MVC   PREVIOUS,SRTREC                    YES                           
         CLC   PREVIOUS(SALESBRK),SRTREC          NO-SALSPRSON BREAK?           
         BE    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVC   PREVIOUS,SRTREC                                                  
         B     PRT35                                                            
                                                                                
* STATION VERSION                                                               
PRT30A   OC    PREVIOUS,PREVIOUS    FIRST TIME?                                 
         BNZ   PRT30                                                            
         MVC   PREVIOUS,SRTREC      YES                                         
         MVC   STAHEAD,PNDKSTAT     SAVE STATION FOR HEADLINE                   
         MVC   REPHEAD,PNDKREP      SAVE REP FOR HEADLINE                       
         BAS   RE,GETFAXN           GET NEW FAX NUM                             
         BAS   RE,DOFAX2                                                        
*                                                                               
PRT30    DS    0H                   NO                                          
         CLC   PREVIOUS(STATBRK),SRTREC          STATION BREAK ?                
         BE    PRT32                             NO                             
         MVC   STAHEAD,PNDKSTAT                  STATION FOR HEADS/FAX          
         MVC   REPHEAD,PNDKREP                    REP FOR HEADS/FAX             
         BAS   RE,GETFAXN                        GET NEW FAX NUM                
         BAS   RE,DOFAX2                                                        
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=X'0001'       START AT NEW PAGE                            
PRT32    MVC   PREVIOUS,SRTREC                                                  
                                                                                
         EJECT                                                                  
* SET  DATA TO PRINT LINE                                                       
PRT35    EQU   *                                                                
         CLI   OFFICVER,C'Y'       IF OFFICE VERSION                            
         BE    PRTO35              GO THERE                                     
                                                                                
* STATION VERSION                                                               
         LA    R2,P                                                             
         USING PLINED,R2                                                        
         MVC   PLOFF(2),PNDKOFF               OFFICE                            
         MVC   PLAGY,PNDDAGY                  AGENCY                            
         MVC   PLAGY+132(20),PNDADVN    LINE2 ADVERTISER                        
         MVC   PLAGY+264(20),PNDPROD    LINE3 PRODUCT                           
         MVC   PLSLSNM,PNDSLSNM               SALESPERSON                       
         MVC   PLCONT,PNDCON                  CONTRACT #                        
* DROP PENNIES FROM PNDDOLS                                                     
         SR    R0,R0                                                            
         ICM   R1,15,PNDDOLS                                                    
         D     R0,=F'100'                                                       
         EDIT  (R1),(12,PLDOLS),FLOAT=$            DOLLARS                      
*                                                                               
         CLI   PNDRSDT,0                                                        
         BE    PRT40                                                            
         GOTO1 DATCON,DMCB,(2,PNDRSDT),(4,PLFAX)     DATE SENT                  
         MVC   PLFAX+132(2),PNDRSTM                  TIME SENT                  
         MVI   PLFAX+134,C':'                                                   
         MVC   PLFAX+135(2),PNDRSTM+2                                           
*                                                                               
PRT40    CLI   PNDCNFDT,0                                                       
         BE    PRT42                                                            
         GOTO1 DATCON,DMCB,(2,PNDCNFDT),(4,PLCONF)                              
         MVC   PLCONF+132(2),PNDCNFTM                                           
         MVI   PLCONF+134,C':'                                                  
         MVC   PLCONF+135(2),PNDCNFTM+2                                         
*                                                                               
PRT42    CLI   PNDVER,0                                                         
         BE    PRT45                                                            
         MVI   PLTYPE,C'R'                                                      
         EDIT  (B1,PNDVER),(3,PLTYPE+1),ALIGN=LEFT                              
                                                                                
PRT45    DS    0H                                                               
         MVC   PLTYPE+132(3),=C'NEW'       NEVER CONFIRMED?                     
         CLI   PNDMOD,C'N'                                                      
         BE    PRT50                                                            
         MVC   PLTYPE+132(4),=C'UNCF'      NOT CONFIRMED NOW?                   
         CLI   PNDMOD,C'U'                                                      
         BE    PRT50                                                            
         CLI   PNDMOD,0            CONFIRMED?                                   
         BNE   *+14                                                             
         MVC   PLTYPE+132(4),=C'CNF '                                           
         B     PRT50                                                            
         MVC   PLTYPE+132(3),=C'MOD'                                            
         EDIT  (B1,PNDMOD),(2,PLTYPE+135),ALIGN=LEFT                            
         B     PRT50                                                            
         EJECT                                                                  
* OFFICE VERSION                                                                
PRTO35   EQU   *                                                                
         LA    R2,P                                                             
         USING PLINED2,R2                                                       
*        MVC   POSTA,PNDKOSTA                 STATION                           
         MVC   POSTA,PNDKSTAT                 STATION                           
         MVC   POSTA+132(20),PNDMKTNM         MARKET NAME                       
         MVC   POAGY,PNDDAGY                  AGENCY                            
         MVC   POAGY+132(20),PNDADVN    LINE2 ADVERTISER                        
         MVC   POAGY+264(20),PNDPROD    LINE3 PRODUCT                           
         MVC   POCONT,PNDCON                  CONTRACT #                        
* DROP PENNIES FROM PNDDOLS                                                     
         SR    R0,R0                                                            
         ICM   R1,15,PNDDOLS                                                    
         D     R0,=F'100'                                                       
         EDIT  (R1),(12,PODOLS),FLOAT=$            DOLLARS                      
*                                                                               
         CLI   PNDRSDT,0                                                        
         BE    PRTO40                                                           
         GOTO1 DATCON,DMCB,(2,PNDRSDT),(4,POFAX)     DATE SENT                  
         MVC   POFAX+132(2),PNDRSTM                  TIME SENT                  
         MVI   POFAX+134,C':'                                                   
         MVC   POFAX+135(2),PNDRSTM+2                                           
*                                                                               
PRTO40   CLI   PNDCNFDT,0                                                       
         BE    PRTO42                                                           
         GOTO1 DATCON,DMCB,(2,PNDCNFDT),(4,POCONF)                              
         MVC   POCONF+132(2),PNDCNFTM                                           
         MVI   POCONF+134,C':'                                                  
         MVC   POCONF+135(2),PNDCNFTM+2                                         
*                                                                               
PRTO42   CLI   PNDVER,0                                                         
         BE    PRTO45                                                           
         MVI   POTYPE,C'R'                                                      
         EDIT  (B1,PNDVER),(3,POTYPE+1),ALIGN=LEFT                              
                                                                                
PRTO45   DS    0H                                                               
         MVC   POTYPE+132(4),=C'OPEN'      OPEN?                                
         CLI   PNDMOD,C'O'                                                      
         BE    PRTO50                                                           
         MVC   POTYPE+132(4),=C'NEW '      NEVER CONFIRMED?                     
         CLI   PNDMOD,C'N'                                                      
         BE    PRTO50                                                           
         MVC   POTYPE+132(4),=C'UNCF'      NOT CONFIRMED NOW?                   
         CLI   PNDMOD,C'U'                                                      
         BE    PRTO50                                                           
         CLI   PNDMOD,0            CONFIRMED?                                   
         BNE   *+14                                                             
         MVC   POTYPE+132(4),=C'CNF '                                           
         B     PRTO50                                                           
         MVC   POTYPE+132(3),=C'MOD'                                            
         EDIT  (B1,PNDMOD),(2,POTYPE+135),ALIGN=LEFT                            
PRTO50   DS    0H                                                               
         B     PRT50                                                            
                                                                                
*                                                                               
PRT50    MVI   SPACING,2           SKIP 2 LINES                                 
         BAS   RE,SPLAT            PRINT REPORT                                 
                                                                                
PRT60    LH    R0,TSRNUM           BUMP TSAR ID NUMBER                          
         A     R0,=F'1'                                                         
         STH   R0,TSRNUM                                                        
         B     PRT20               GET NEXT TSAR REC                            
*                                                                               
PRTX     B     XIT                                                              
         EJECT                                                                  
*                                                                               
* INITIALIZE                                                                    
INIT     NTR1                                                                   
         GOTO1 =A(XRTNS),DMCB,(0,(RC)),SORTREC                                  
         B     XIT                                                              
         EJECT                                                                  
* READ STATION REC TO GET FAX NUMBER                                            
* IF OFFICE VERSION RETURNS MARKET NAME IN WORK                                 
GETFAXN  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         CLI   OFFICVER,C'Y'       IF OFFICE VERSION                            
         BNE   GETF10                                                           
         MVC   RSTAKREP,WORK       OFFICE VER PASSES ITS OWN REP                
         MVC   RSTAKSTA,WORK+2     OFFICE VER PASSES ITS OWN STATION            
         B     GETF12                                                           
*                                                                               
GETF10   MVC   RSTAKREP,PNDKREP    REP                                          
         MVC   RSTAKSTA,STAHEAD    STATION CALL LETTERS                         
GETF12   GOTO1 HIGH                                                             
         CLC   KEY(26),KEYSAVE     NOT CHECKING LAST POSITION(A,F,ETC)          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
                                                                                
* - FOR OFFICE VERSION JUST RETURN MARKET NAME                                  
         CLI   OFFICVER,C'Y'                                                    
         BNE   *+14                                                             
         MVC   WORK(20),RSTAMKT                                                 
         B     GTFXX                                                            
*                                                                               
         MVI   ELCODE,X'08'        EXTRA DESCRIPTION ELEMENT                    
         BAS   RE,GETEL                                                         
         BNE   GTFXX                                                            
         USING RSTAXXEL,R6                                                      
         MVC   FAXNUMBR,RSTAOFAX   FAXNUMBER                                    
         DROP  R6                                                               
*                                                                               
GTFXX    DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
*                                                                               
                                                                                
*******************************                                                 
EMAILIT  NTR1                                                                   
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
*                                                                               
         MVC   P+4(5),=C'*HDR*'                                                 
         MVI   P+69,C'M'           PUT C'M' IN COLUMN 70                        
*                                  SEND SPECIAL PRINT LINE                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BAS   RE,SPLAT                                                         
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
         MVC   EDIPROG,=C'SKT'     FOR TYPE ORD                                 
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
* USE EDIRDT (REP KWX MESSAGE)                                                  
*        MVC   EDIRDTRP,REPHEAD                         REP CODE                
         MVC   EDIRDTRP,PNDKREP                         REP CODE                
*        MVC   EDIRDTST,STAHEAD                         STATION CODE            
         MVC   EDIRDTST,PNDKSTAT                        STATION CODE            
         GOTO1 DATCON,DMCB,(2,TODAY2),(0,EDIRDTSD)      NO/ GET TODAY           
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BAS   RE,SPLAT                                                         
         DROP  R5                                                               
*                                                                               
* PRINT RECEIVING EMAIL ADDRESS                                                 
*                                                                               
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'RCP'                                                  
         L     R6,AIO              STATION RECORD                               
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTAEML,R6                                                       
*        MVC   P+15(L'RSTAADD),RSTAADD     RECEIVING EMAIL ADDRESS              
         ZIC   R5,RSTAEMLN                                                      
         SHI   R5,4                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+15(0),RSTAADD             RECEIVING EMAIL ADDRESS              
         DROP  R6                                                               
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BAS   RE,SPLAT                                                         
*                                                                               
         MVC   P(5),=C'++DDS'                                                   
         MVC   P+11(3),=C'SUB'                                                  
         MVC   P+15(28),=C'DAILY STA REPORT FOR STATION'                        
*        MVC   P+45(5),STAHEAD                                                  
         MVC   P+45(5),PNDKSTAT                                                 
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BAS   RE,SPLAT                                                         
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
**********************************************************************          
* GETS PRODUCT NAME FROM ELEMENT 5 OR PRODUCT RECORD                            
* AND RETURNS IT IN WORK                                                        
FILLPROD NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
**       CLI   PNDPROD,X'40'       ,,IF PNDPROD NOT = BLANK                     
**       BNH   FILL10                                                           
         CLI   RCONPRD,X'40'       ,,IF PROD NOT = BLANK                        
         BNH   FILL10                                                           
         MVC   WORK(3),RCONPRD     ,,PASS PROD CODE IN WORK                     
**       MVC   WORK(3),PNDPROD     ,,PASS PROD CODE IN WORK                     
         BAS   RE,GETPROD               RETURNS PRODUCT NAME IN WORK            
         B     FILL25                                                           
FILL10   MVI   ELCODE,5            ,,ELSE PROD NAME ON ELEM 5                   
         BAS   RE,GETEL                                                         
         USING RCONEXEL,R6                                                      
         MVC   WORK(20),RCONEXPR                                                
         B     FILL25                                                           
*                                                                               
FILL25   EQU   *                                                                
*                                                                               
FILLX    B     XIT                                                              
*                                                                               
         DROP R6                                                                
                                                                                
                                                                                
*****************************************************************               
* READS PRODUCT RECORD                                                          
* EXPECTS PROD CODE IN WORK - CONTRACT RECORD IN AIO                            
* RETURNS PROD NAME IN WORK                                                     
*                                                                               
GETPROD  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RPRDREC,R5                                                       
         MVI   RPRDKTYP,9                                                       
         MVC   RPRDKADV,RCONKADV                                                
         MVC   RPRDKPRD,WORK                                                    
         MVC   RPRDKREP,RCONKREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RPRDNAME                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     XIT                                                              
         DROP  R5                                                               
*                                                                               
         EJECT                                                                  
*********************************************************************           
* RETURN AGENCY NAME - EXPECTS CONTRACT REC IN AIO                              
*                                                                               
FILLAGY  NTR1                                                                   
         L     R6,AIO                                                           
         USING RCONREC,R6                                                       
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING RAGYKEY,R5                                                       
         MVI   RAGYKTYP,X'0A'                                                   
         MVC   RAGYKAGY,RCONKAGY                                                
         MVC   RAGYKAOF,RCONKAOF                                                
         MVC   RAGYKREP,RCONKREP                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         L     R5,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RAGYNAM1                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     XIT                                                              
         DROP  R5                                                               
*********************************************************************           
         EJECT                                                                  
* - PRINT ROUTINE                                                               
SPLAT    NTR1                                                                   
         GOTO1 SPOOL,DMCB,(R8)                                                  
SPLATX   B     XIT                                                              
         EJECT                                                                  
                                                                                
* HEADLINE HOOK                                                                 
HOOK     NTR1                                                                   
         CLI   OFFICVER,C'Y'       OFFICE VERSION ?                             
         BNE   HOOK10                                                           
         MVC   H5+1(9),=C'SLSPERSON'                                            
         MVC   H5+12(3),PNDKOSL    INITIALS                                     
         MVC   H5+16(20),PNDSLSNM  NAME                                         
         MVC   H3+1(6),=C'OFFICE'                                               
         MVC   H3+12(2),PNDKOOF                                                 
         MVC   H4+1(4),=C'TEAM'                                                 
         MVC   H4+12(2),PNDKOTM                                                 
         B     HOOK20                                                           
*                                                                               
HOOK10   MVC   H4+1(7),=C'STATION'                                              
         MVC   H4+9(5),STAHEAD                                                  
         B     HOOK20                                                           
*                                                                               
HOOK20   DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,TODAY2),(5,H3+55)                                 
HOXX     B     XIT                                                              
                                                                                
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
         EJECT                                                                  
*                                                                               
                                                                                
                                                                                
GETADVR  NTR1                                                                   
         XC    WORK(5),WORK                                                     
         XC    KEY,KEY                                                          
         MVI   KEY,8                                                            
         MVC   KEY+21(4),RCONKADV                                               
         MVC   KEY+25(2),RCONKREP                                               
GETADV2  GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   GETADV3                                                          
         CLC   RCONKREP,KEY+25                                                  
         BE    GETADV4                                                          
         CLC   =C'ZZ',KEY+25                                                    
         BE    GETADV4                                                          
         MVC   KEY+25(2),=C'ZZ'                                                 
         B     GETADV2                                                          
GETADV3  MVC   WORK(12),=C'ADV UNKNOWN '                                        
         B     GETADVX                                                          
*                                                                               
GETADV4  EQU   *                                                                
         USING RADVREC,R4                                                       
         MVC   AIO,AIO2                                                         
         L     R4,AIO                                                           
         GOTO1 GETREC                                                           
         MVC   WORK(20),RADVNAME                                                
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
         B     GETADVX                                                          
         DROP  R4                                                               
GETADVX  EQU   *                                                                
         B     XIT                                                              
                                                                                
*                                                                               
GETSLSNM NTR1                                                                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R1,KEY                                                           
         USING RSALREC,R1                                                       
         MVI   RSALKTYP,X'06'                                                   
         MVC   RSALKREP,RCONKREP                                                
         MVC   RSALKSAL,RCONSAL                                                 
         DROP  R1                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GTNOSAL                                                          
         SPACE 1                                                                
GTDISSAL EQU   *                                                                
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING RSALREC,R1                                                       
         MVC   WORK(20),RSALNAME                                                
         B     GTSALX                                                           
         DROP  R1                                                               
                                                                                
GTNOSAL  MVC   WORK(20),=CL20'NOT FOUND'                                        
         B     GTSALX                                                           
                                                                                
GTSALX   EQU   *                                                                
         MVC   AIO,AIO1                                                         
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
DOFAX2   NTR1                                                                   
*                                                                               
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         CLI   OPENFAX,C'Y'                        IF ALREADY OPENED            
         BNE   WUI05                                                            
         MVC   P(26),=C'*** END OF DDS MESSAGE ***'  CLOSE IT                   
         BAS   RE,SPLAT                                                         
*                                                                               
WUI05    MVI   OPENFAX,C'Y'                                                     
*                                                                               
         MVC   P+4(5),=C'*HDR*'                                                 
         MVC   P+9(5),STAHEAD      DEFAULT STATION CALL LETTERS                 
         OC    FAXNUMBR,FAXNUMBR   IF FAX# VALID, USE IT INSTEAD OF             
         BZ    WUI30               STATION CALL LETTERS                         
*                                                                               
* MAIL BOX                                                                      
*                                                                               
         CLC   =C'MB=',FAXNUMBR                                                 
         BNE   WUI10                                                            
         MVC   P+9(8),FAXNUMBR+3                                                
         B     WUI30                                                            
*                                                                               
* INTERNATINOAL FAX NUMBER                                                      
*                                                                               
WUI10    DS    0H                                                               
         CLI   FAXNUMBR,0          INTERNATIONAL?                               
         BNE   WUI20                                                            
         CLI   FAXNUMBR+1,0        HUH? NO LENGTH? SKIP IT!                     
         BE    WUI30                                                            
         MVC   P+9(4),=C'FAX '                                                  
*                                                                               
         ZIC   R3,FAXNUMBR+1       INTERNATIONAL CODE                           
         EDIT  (R3),(3,P+13),FILL=0                                             
*                                                                               
         UNPK  WORK(16),FAXNUMBR+3(8)                                           
         ZIC   RF,FAXNUMBR+2       LENGTH OF SIGNIFICANT DIGITS                 
         LA    RE,16               MAXIMUM LENGTH OF FIELD                      
         SR    RE,RF               GET SIGNIFICANT OFFSET                       
         LA    RF,WORK             A(UNPACKED NUMBER)                           
         AR    RF,RE               ADD OFFSET                                   
         ZIC   RE,FAXNUMBR+2       GET LENGTH OF FAX# FIELD AGAIN               
         BCTR  RE,0                DECREMENT FOR EX                             
         EX    RE,*+8              MOVE BY LENGTH                               
         B     *+10                                                             
         MVC   P+16(0),0(RF)                                                    
*                                                                               
         EX    RE,*+8              DISPLAY NUMBER IN ETI ALSO                   
         B     *+10                                                             
         MVC   P+41(0),0(RF)                                                    
         ZIC   R3,FAXNUMBR+1                                                    
         EDIT  (R3),(3,P+38),FILL=0                                             
*                                                                               
         MVI   P+35,C'P'           REPL X'89' IN REP W/EASYLINK /PAGE           
         B     WUI30                                                            
*                                                                               
* NORMAL FAX NUMBER                                                             
*                                                                               
WUI20    DS    0H                                                               
         LA    RE,FAXNUMBR                                                      
                                                                                
WUI23    DS    0H                                                               
         CLI   0(RE),0             PADDED WITH NULLS OR                         
         BE    WUI24                                                            
         CLI   0(RE),C' '          IT MIGHT BE PADDED WITH SPACES               
         BE    WUI24                                                            
         CLI   0(RE),C'0'          MUST BE NUMERIC                              
         BL    WUI30                                                            
         CLI   0(RE),C'9'                                                       
         BH    WUI30                                                            
         LA    RF,FAXNUMBR                                                      
         LA    RF,L'FAXNUMBR(RF)                                                
         LA    RE,1(RE)                                                         
         CR    RE,RF                                                            
         BNH   WUI23                                                            
                                                                                
WUI24    DS    0H                  FAX NUMBER MUST BE EXACTLY 10 CHARS          
         LA    RF,FAXNUMBR                                                      
         SR    RE,RF                                                            
         CH    RE,=H'10'                                                        
         BNE   WUI30                                                            
                                                                                
         MVC   P+9(4),=C'FAX '                                                  
         MVC   P+13(10),FAXNUMBR                                                
         MVI   P+35,C'P'           REPL X'89' IN REP W/EASYLINK /PAGE           
                                                                                
         MVI   P+38,C'('           FORMATTED FAX# FOR $ETI                      
         MVC   P+39(3),FAXNUMBR    AREA CODE                                    
         MVI   P+42,C')'                                                        
         MVC   P+44(3),FAXNUMBR+3  PREFIX                                       
         MVI   P+47,C'-'                                                        
         MVC   P+48(4),FAXNUMBR+6  SUFFIX                                       
         DROP  R6                                                               
                                                                                
WUI30    DS    0H                                                               
         LA    R1,P                                                             
         MVI   34(R1),C'W'         132 CHARS WIDE                               
         MVI   35(R1),C'P'         PAGE BREAKS                                  
         CLI   P+38,C'('           FORMATTED FAX NUMBER?                        
         BE    WUI42               YES                                          
*                                  NO/SO FORMAT STATION                         
         XC    WORK,WORK                                                        
         MVC   WORK(4),STAHEAD                                                  
         MVI   WORK+4,C'-'                                                      
         CLI   STAHEAD+4,C'A'                                                   
         BNE   *+14                                                             
         MVC   WORK+5(2),=C'AM'                                                 
         B     WUI40                                                            
         CLI   STAHEAD+4,C'F'                                                   
         BNE   *+14                                                             
         MVC   WORK+5(2),=C'FM'                                                 
         B     WUI40                                                            
         CLI   STAHEAD+4,C' '                                                   
         BNE   *+14                                                             
         MVC   WORK+5(2),=C'TV'                                                 
         B     WUI40                                                            
         DC    H'0'                                                             
WUI40    MVC   38(7,R1),WORK     FORMATTED DESTINATION NAME                     
*                                                                               
WUI42    MVC   P+54(2),=C'XX'      EASYLINK BILLING INFO                        
*                                  SEND SPECIAL PRINT LINE                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BAS   RE,SPLAT                                                         
*                                                                               
* PRINT A ++DDS CARD WITH 58 BYTES OF DATA FOR EDICT XACTION PROG               
*                                                                               
         LA    R5,P                                                             
         USING EDICTD,R5                                                        
         MVC   EDIDDSID,=C'++DDS'                                               
         MVC   EDISYST,=C'RE'      FOR SYSTEM REP                               
         MVC   EDIPROG,=C'SKT'     FOR TYPE ORD                                 
         MVC   EDIIDEN,=C'TRN'     XACTION FILE DATA                            
* USE EDIRDT (REP KWX MESSAGE)                                                  
         MVC   EDIRDTRP,REPHEAD                         REP CODE                
         MVC   EDIRDTST,STAHEAD                         STATION CODE            
         GOTO1 DATCON,DMCB,(2,TODAY2),(0,EDIRDTSD)      NO/ GET TODAY           
*                                                                               
WUI60    DS    0H                  SEND SPECIAL PRINT LINE                      
         MVI   SKIPSPEC,C'Y'                                                    
         MVI   FORCEHED,C'N'                                                    
         MVI   FORCEMID,C'N'                                                    
         BAS   RE,SPLAT                                                         
         DROP  R5                                                               
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
WUIXX    XIT1                                                                   
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
                                                                                
SORTREC  DS    0CL500              USE THIS LABEL TO ESTABLISH                  
*                                  ADDRESSABILITY FOR XRTNS                     
SRTREC   DS    0CL500                                                           
*                                                                               
SRTKEY   DS    CL100                                                            
SRTDATA  DS    CL400                                                            
SRTEND   EQU   *                                                                
SRTLENE  EQU   *-SRTREC                                                         
*                                                                               
         ORG   SRTKEY                                                           
                                                                                
PNDKEY   DS    0CL100                                                           
*                                  ** KEY FOR STATION VERSION                   
         DS    CL1                 SPARE                                        
PNDKREP  DS    CL2                 REP                                          
PNDKSTAT DS    CL5                 STATION                                      
STATBRK  EQU   *-PNDKEY            STATION BREAK                                
PNDKOFF  DS    CL2                 OFFICE                                       
PNDKAGY  DS    CL4                 AGENCY                                       
PNDKADV  DS    CL4                 ADVERTISER                                   
PNDKSLS  DS    CL3                 SALESPERSON                                  
PNDKCON  DS    CL8                 CONTRACT NUMBER                              
         DS    CL71                SPARE                                        
*                                                                               
         ORG   PNDKEY                                                           
*                                  ** KEY FOR OFFICE VERSION                    
         DS    CL1                 SPARE                                        
PNDKOOF  DS    CL2                 OFFICE                                       
PNDKOTM  DS    CL2                 TEAM                                         
PNDKOSL  DS    CL3                 SALESPERSON                                  
SALESBRK EQU   *-PNDKEY            SALESPERSON BREAK                            
PNDKOSTA DS    CL5                 STATION                                      
PNDKOAGY DS    CL4                 AGENCY                                       
PNDKOADV DS    CL4                 ADVERTISER                                   
PNDKOCON DS    CL8                 CONTRACT NUMBER                              
         DS    CL71                SPARE                                        
*                                                                               
PNDDATA  DS    0CL400                                                           
PNDCON   DS    CL8                 CONTRACT NUMBER                              
PNDSLSNM DS    CL20                SALESPERSON NAME                             
PNDCREAT DS    CL3                 CREATION DATE YMD                            
PNDDADV  DS    CL4                 ADVERTISER                                   
PNDADVN  DS    CL20                ADVERTISER NAME                              
PNDPROD  DS    CL20                PRODUCT                                      
PNDDAGY  DS    CL20                AGENCY                                       
PNDBUYER DS    CL20                BUYER                                        
PNDFLITE DS    CL6                 FLIGHT DATE START/END (YMD)                  
PNDDOLS  DS    CL4                 BUDGET                                       
*                                                                               
PNDCNFDT DS    CL2                 CONFIRMED DATE                               
PNDCNFTM DS    CL6                 CONFIRMED TIME                               
PNDRSDT  DS    CL2                 SENT BY REP DATE                             
PNDRSTM  DS    CL6                 SENT BY REP TIME                             
PNDMOD   DS    CL1                 MODIFICATION #                               
PNDVER   DS    CL1                 VERSION #                                    
*                                                                               
PNDMKTNM DS    CL20                MARKET NAME                                  
         DS    CL253               SPARE                                        
*                                                                               
                                                                                
* -  CONSTANTS FOR TSAROFF                                                      
TSARBUFF DS    A                   ADDRESS OF GETMAIN BUFF                      
TSARBUFL DC    A(SRTLENE*14000)    EXPANDED FROM 12000                          
*                                                                               
*                                                                               
TSAREA   DS    XL64                                                             
*                                                                               
MYTSREC  DS    0D                                                               
*                                                                               
MYTSKEY  DS    CL(L'SRTKEY)                                                     
MYTSKEYL EQU   *-MYTSKEY                                                        
MYTSDATA DS    CL(SRTEND-SRTDATA)                                               
MYTSRECL EQU   *-MYTSREC                                                        
*                                                                               
*                                                                               
                                                                                
                                                                                
* REPORT HEADLINE SPECS                                                         
         SPACE 2                                                                
REGSPECS DS    0H                                                               
         SPROG 0,1                                                              
         PSPEC H3,89,AGYNAME                                                    
         PSPEC H4,89,RUN                                                        
         PSPEC H5,89,REPORT                                                     
         PSPEC H6,89,REQUESTOR                                                  
         PSPEC H7,89,PAGE                                                       
         PSPEC H9,1,C'                    '                                     
         SPROG 0                                                                
         PSPEC H1,50,C'DAILY STA FAX ACTIVITY'                                  
         PSPEC H2,50,C'----- --- --- --------'                                  
         PSPEC H9,21,C'OFF   AGY / ADV / PROD        SALESPERSON       X        
               CONTRACT    SENT     CNF    VER/STAT       AMOUNT'               
         PSPEC H10,1,C'--------------------'                                    
         PSPEC H10,21,C'-----------------------------------------------X        
               --------------------------------------------------'              
         SPROG 1                                                                
         PSPEC H1,50,C'DAILY OFF FAX ACTIVITY'                                  
         PSPEC H2,50,C'----- --- --- --------'                                  
         PSPEC H9,21,C'STATION/MARKET          AGY /ADV / PROD        CX        
               ONTRACT    SENT     CNF    VER/STAT       AMOUNT'                
         PSPEC H10,1,C'--------------------'                                    
         PSPEC H10,21,C'-----------------------------------------------X        
               --------------------------------------------------'              
         DC    X'00'                                                            
         SPACE 3                                                                
         EJECT                                                                  
DBLOK    DS    0H                                                               
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
* OVERFLOW ROUTINES                                                             
         CSECT                                                                  
XRTNS    NMOD1 0,*183BX*                                                        
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R7,4(R1)                                                         
         USING SORTREC,R7                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         ZIC   RF,0(R1)                                                         
         SLL   RF,2                                                             
         B     ROUTINE(RF)                                                      
*                                                                               
ROUTINE  DS    0H                                                               
         B     XINIT                                                            
XRX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
* REPIOTBL INITIALIZED IN RESFM3A                                               
*                                                                               
XINIT    DS    0H                                                               
         CLI   OFFICVER,C'Y'       IF NOT OFFICE                                
         BE    *+8                                                              
         OI    GENSTAT2,NOREQDET      NO REQ DET                                
*                                                                               
* GET DATE OF MONDAY PREVIOUS TO TODAY'S DATE                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(0,WORK)    TODAY'S DATE-YYMMDD           
         CLI   TODAY2,0            DID WE OVERRIDE TODAY'S DATE?                
         BNE   INIT4                                                            
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,TODAY2)  COMPRESSED                    
INIT4    GOTO1 GETDAY,DMCB,WORK,WORK+6            RETURNS DAY OF WEEK           
         CLI   DMCB+4,X'40'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R2,DMCB           R2 =TODAY=1-7(DAY OF WEEK)                     
         BCTR  R2,0              NO OF DAYS DIFF BETWEEN TODAY-MONDAY           
         MH    R2,=H'-1'           MAKE NUM OF DAYS DIFF NEGATIVE               
         PRINT GEN                                                              
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R2)                                      
         PRINT NOGEN                                                            
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,MONDAYDT)                              
*                                                                               
                                                                                
* - INITIALIZE TSAROFF                                                          
         PRINT GEN                                                              
         GOTO1 =V(LOADER),DMCB,=CL8'T00A7D',0    TSAROFF                        
         PRINT NOGEN                                                            
         MVC   ATSAROFF,DMCB+4                                                  
         OC    ATSAROFF,ATSAROFF                                                
         BNO   *+6                                                              
         DC    H'0'                                                             
                                                                                
         L     R0,TSARBUFL                                                      
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,TSARBUFF                                                      
*                                                                               
         XC    TSAREA,TSAREA                                                    
         LA    R2,TSAREA                                                        
         USING TSARD,R2                                                         
*                                                                               
         MVI   TSOFFACT,TSAINI                                                  
         MVC   TSABUF,TSARBUFF                                                  
         MVC   TSAREC,TSARBUFL                                                  
         LA    R0,MYTSKEYL                                                      
         STC   R0,TSKEYL                                                        
         LA    R0,MYTSRECL                                                      
         STH   R0,TSRECL                                                        
         GOTO1 ATSAROFF,(R2)                                                    
*                                                                               
***      MVC   WORK(4),TSARBUFF                                                 
*                                                                               
         CLI   TSERRS,0                                                         
         BE    INIT6                                                            
         DC    H'0'                                                             
                                                                                
************************************************************                    
* READ THROUGH STATION RECORDS AND PUT NON-GRAPHNET STATIONS                    
* INTO NONGRAPH TABLE - EXCLUDE THESE FROM ACTIVITY REPORTING                   
*                                                                               
INIT6    DS    0H                                                               
         MVC   AIO,AIO2                                                         
         LA    R2,NONGRAPH                                                      
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING RSTAREC,R6                                                       
         MVI   RSTAKTYP,X'02'                                                   
         MVC   RSTAKREP,TWAAGY     REP                                          
         GOTO1 HIGH                                                             
INIT6A   CLC   KEY(22),KEYSAVE                                                  
         BNE   INIT8                                                            
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         USING RSTAREC,R6                                                       
         OC    RSTAEND,RSTAEND     HAS STATION LEFT GRAPHNET?                   
         BNZ   INIT6B              YES                                          
         CLI   RSTATRAF,C'G'       NO - MUST BE TRAFFIC=A/G                     
         BE    INIT7                                                            
         CLI   RSTATRAF,C'A'                                                    
         BE    INIT7                                                            
INIT6B   MVC   0(5,R2),RSTAKSTA    PUT STATION INTO NON-ACTIVE TBL              
         LA    R2,5(R2)                                                         
         CLI   0(R2),X'FF'         END OF TABLE                                 
         BNE   *+6                                                              
         DC    H'0'                   INCREASE TABLE                            
INIT7    GOTO1 SEQ                                                              
         B     INIT6A                                                           
INIT8    DS    0H                                                               
         MVC   AIO,AIO1            RESET AIO AREA                               
         L     R6,AIO                                                           
         B     INIT10                                                           
         DROP  R6                                                               
***********************************************************                     
                                                                                
*                                                                               
INIT10   DS    0H                                                               
*                                                                               
         B     XRX                                                              
         EJECT                                                                  
         LTORG                                                                  
* TABLE OF NONGRAPHNET STATIONS                                                 
NONGRAPH DS    CL5000              ROOM FOR 1000 STATIONS                       
         DC    X'FF'                                                            
         EJECT                                                                  
       ++INCLUDE DDTSARD                                                        
         EJECT                                                                  
       ++INCLUDE REGENCON                                                       
         EJECT                                                                  
       ++INCLUDE REGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE REGENAGY                                                       
         EJECT                                                                  
       ++INCLUDE REGENADV                                                       
         EJECT                                                                  
       ++INCLUDE REGENSAL                                                       
         EJECT                                                                  
       ++INCLUDE REGENSTAB                                                      
         EJECT                                                                  
       ++INCLUDE REGENOFF                                                       
         EJECT                                                                  
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
       ++INCLUDE DDWIDED                                                        
         EJECT                                                                  
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RESFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RESFMC3D                                                       
         EJECT                                                                  
       ++INCLUDE RESFMABD                                                       
         EJECT                                                                  
       ++INCLUDE RESFMWORKD                                                     
         SPACE 3                                                                
         ORG   SYSSPARE                                                         
         PRINT ON                                                               
*                                                                               
*               WORK AREA                                                       
         DS    0F                                                               
MYWORK   DS    0CL1000                                                          
OFFICVER DS    CL1                 OFFICE VERSION OF REPORT                     
STATION  DS    CL5                 STATION FILTER                               
OFFICE   DS    CL2                 OFFICE FILTER                                
OFFICNAM DS    CL20                OFFICE NAME                                  
TEAM     DS    CL2                 TEAM FILTER                                  
TEAMNM   DS    CL20                DIV/TEAM NAME                                
SLSPRSN  DS    CL3                 INITIALS                                     
SLSNAME  DS    CL20                SALESPERSON NAME                             
*                                                                               
ATSAROFF DS    F                                                                
ACTSTR   DS    CL3                 YMD (OVERRIDES PREV MON-RUNDAT)              
ACTEND   DS    CL3                 YMD  (OVERRIDES PREV MON-RUNDAT)             
YMDST    DS    CL3                 START DATE YMD                               
YMDND    DS    CL3                 END DATE YMD                                 
TODAY2   DS    CL2                 TODAY'S DATE COMPRESSED                      
FAXNUMBR DS    CL13                                                             
PREVIOUS DS    CL40                                                             
WORK2    DS    CL200                                                            
STAHEAD  DS    CL5                                                              
REPHEAD  DS    CL2                                                              
OPENFAX  DS    CL1                                                              
*                                                                               
MONDAYDT DS    CL3                 PREVIOUS MONDAY'S DATE                       
MYKEYSV  DS    CL40                                                             
MKTSAVE  DS    CL300             CL5(STATION)+CL20(MKTNAME)=12 MKTNMS           
MKTSLNE  EQU   *-MKTSAVE                                                        
MYWORKND EQU   *                                                                
*                                                                               
         EJECT                                                                  
*                                                                               
* PRINT LINE DSECT FOR REPORT                                                   
*                                                                               
PLINED   DSECT                                                                  
         DS    CL20                                                             
PLOFF    DS    CL5                                                              
         DS    CL1                                                              
PLAGY    DS    CL20                AGY                                          
         DS    CL2                                                              
PLSLSNM  DS    CL20                SALESPERSON NAME                             
         DS    CL2                                                              
PLCONT   DS    CL8                 CONTRACT                                     
         DS    CL2                                                              
PLFAX    DS    CL7                 ORDER SENT BY REP                            
         DS    CL2                                                              
PLCONF   DS    CL7                 ORDER CONFIRMED                              
         DS    CL2                                                              
PLTYPE   DS    CL5                 VERSION #                                    
         DS    CL2                                                              
PLDOLS   DS    CL10                DOLLARS                                      
*                                                                               
PLINED2  DSECT                     * FOR OFFICE VERSION                         
         DS    CL20                                                             
POSTA    DS    CL5                 STATION                                      
         DS    CL17                (MARKET NAME ON 2ND LINE)                    
POAGY    DS    CL20                AGY                                          
         DS    CL6                                                              
POCONT   DS    CL8                 CONTRACT                                     
         DS    CL2                                                              
POFAX    DS    CL7                 ORDER SENT BY REP                            
         DS    CL2                                                              
POCONF   DS    CL7                 ORDER CONFIRMED                              
         DS    CL2                                                              
POTYPE   DS    CL5                 VERSION #                                    
         DS    CL2                                                              
PODOLS   DS    CL10                DOLLARS                                      
         EJECT                                                                  
MASTD    DSECT                                                                  
       ++INCLUDE DDMASTC                                                        
EDICTD   DSECT                                                                  
       ++INCLUDE EDIDDSHD                                                       
       ++INCLUDE EDILINKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'151RESFM19A  05/01/02'                                      
         END                                                                    
