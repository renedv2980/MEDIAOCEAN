*          DATA SET ACBIL08    AT LEVEL 012 AS OF 12/17/12                      
*PHASE T60E08A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:       T60E08 - CREATIVE BILLING - WRITE-OFF                 *         
*                                                                     *         
*  SCREEN:      T60EF8 - NARRATIVE SCREEN                             *         
*                                                                     *         
*  COMMENTS:    OVERLAY MAKES THE FOLLOWING POSTINGS:                 *         
*               1.  -DR SJ CL/PRD/JB        C/A 1R PERSONNEL          *         
*               2.   DR SKIPS (FROM MEMO)   C/A CL/PRD/JB             *         
*               3.   CR SIIPS               C/A CL/PRD                *         
*               4.  -CR SIWO                C/A CL/PRD                *         
*               5.   COSTING POSTINGS TO 1C & 12                      *         
*                                                                     *         
*               ACTION 'RECOVER' (UN-WRITE-OFF) IS ALSO               *         
*               EXECUTED IN THIS OVERLAY.  IT CANCELS THE             *         
*               BATCH THAT WAS CREATED AND RESTORES '4B'              *         
*               ELEMENT SO JOB CONTAINS ALLOCATED CHARGES             *         
*               OR IT MAKES NEW POSTINGS TO COUNTER THE               *         
*               ORIGINAL WRITE-OFF                                    *         
*                                                                     *         
*  CALLED FROM: CREATIVE BILLING ROOT - (T60E00)                      *         
*                                                                     *         
*  CALLS TO:    DATAMGR,  DATCON,   DATVAL,   DICTATE,  PRORATA,      *         
*               SQUASHER, XSORT                                       *         
*                                                                     *         
*  CALLS TO T60E00 ROUTINES:                                          *         
*               ABADDLST, ABADDRFT, ABDELDPT, ABLDCHR,  AGETNAME,     *         
*               AINITSCR, ALDADTRN, ATRXEL                            *         
*                                                                     *         
*  INPUTS:      ALLOCATED CHARGES FROM T60E06 PHASE                   *         
*                                                                     *         
*  OUTPUTS:     WRITE-OFF OF ALLOCATED CHARGES TO SIWO ACCOUNT        *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - WORK - FREQUENTLY POINTS TO THE CURRENT RECORD        *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - THIRD BASE REGISTER                                   *         
*          R6 - WORK                                                  *         
*          R7 - OVRTWA                                                *         
*          R8 - TWA                                                   *         
*          R9 - GLOBAL WORKING STORAGE                                *         
*          RA - SECOND BASE REGISTER                                  *         
*          RB - FIRST BASE REGISTER                                   *         
*          RC - LOCAL WORKING STORAGE                                 *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'T60E08 - CREATIVE BILLING - WRITE-OFF'                          
ACBIL08  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**BIL8**,RA,R5,CLEAR=YES                               
*                                                                               
         USING GWS,R9              MAP  GLOBAL    WORKING   STORAGE             
         USING TWAD,R8             MAP  TWA                                     
         USING LWSD,RC             MAP  LOCAL     WORKING   STORAGE             
*                                                                               
         USING OVRTWAD,R7          MAP  TWA  OVERLAY   WORK AREA                
         LA    R7,OVRTWA                                                        
         CLI   ANYKEY,YES          HAS  THE  KEY  CHANGED ?                     
         BNE   WO10                NO,  DO   NOT  REINITIALIZE   MEMORY         
         BAS   RE,INITOTWA         INITIALIZE     THE  OVRTWA    AREA           
         LA    R1,SCRTXT           ->   SCREEN    TEXT TABLE                    
         GOTO1 AINITSCR            INITIALIZE     SCREEN    TEXT                
*                                  SUBTOTAL                                     
         MVCDD WONSBT1(L'WONSBT1),AC#SUBT,R                                     
         LA    R2,WONSBT1                                                       
         BAS   RE,CALLDICT                                                      
         MVC   WONSBT2,WONSBT1                                                  
         OI    WONSBT1H+6,X'08'    TRANSMIT                                     
         OI    WONSBT2H+6,X'08'    TRANSMIT                                     
*                                                                               
WO10     GOTO1 ALDADTRN            LOAD ADDTRN    AND  ACBIL40                  
*                                                                               
         ZAP   ALLNET,=P'0'        INIT PACKED    FIELDS                        
         ZAP   SAVETOT,=P'0'                                                    
*                                                                               
         CLI   SMODE,DSPLY         HAVE TO   LOOP THROUGH   SCREEN ?            
         BE    DISP50              YES, HAVE MORE DATA TO   DISPLAY             
         CLI   BILMOS,0            DID  THEY OVERRIDE  THE  MOS  DATE ?         
         BNE   WO20                YES, KEEP IT                                 
         MVC   BILMOS,MOS          NO,  USE  TODAY                              
         MVC   PBILMOS,PMOS        AND  TODAY'S   PWO  YYMM                     
*                                                                               
WO20     CLI   ACTION,WRT          ARE THEY WRITING OFF?                        
         BNE   WO30                NO                                           
         GOTO1 AVALMLK,TRNTWRTO    YES, CHECK SECURITY                          
         BNE   ERRXIT                                                           
*                                                                               
WO30     CLI   ACTION,REC          DO   A    RECOVERY ?  (UN-WRITE-OFF)         
         BNE   WO50                                                             
         CLI   SMODE,69            HAS  DATE BEEN INPUT     YET ?               
         BE    RECVR               YES, THEN GO   MAKE THE  POSTINGS            
         MVI   SMODE,69            ELSE DISPLAY   RECOVERY  SCREEN              
*                                  CLEAR     ALL  FLDS AFTER     DATE           
         TWAXC WONDATEH,WONTABH,PROT=N                                          
         LA    R1,WONHED1H                                                      
         LA    R0,WONTABH                                                       
*                                                                               
WO40     OI    1(R1),X'0C'         LOW  INTENSITY                               
         OI    6(R1),FVOXMT        TRANSMIT                                     
         ZIC   R2,0(,R1)                                                        
         AR    R1,R2               BUMP TO   NEXT FIELD                         
         CR    R1,R0                                                            
         BL    WO40                                                             
         B     WOENTDAT            ENTER     DATE OF   WRITE-OFF                
*                                                                               
WO50     GOTO1 ABLDCHR             BUILD     TBL  OF  ALLOCATED CHARGES         
         CLI   CHRCNT,0            CHECK     NUM  OF  ALLOCATED CHARGES         
         BE    NOCHR               NO   ALLOCATED CHARGES                       
*                                                                               
         CLI   SMODE,MARK          HAS  NARRATIVE BEEN ENTERED   AND            
         BE    MAIN                     CHARGES   ALREADY   DISPLAYED ?         
         GOTO1 TRANSMIT                                                         
         EJECT ,                                                                
***********************************************************************         
*              SHOW ALLOCATION CHARGES                                *         
*-------------------------------------------------------              *         
*                                                                     *         
*        THIS ROUTINE DISPLAYS ALLOCATED CHARGES BY CONTRA            *         
*        ACCOUNT OVER MULTIPLE WORK CODES.                            *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         EXTED TOTALL,WONAMT1,2,ALIGN=LEFT,FLOAT=-                              
*                                                                               
         XC    NUMREC,NUMREC       CLEAR     COUNTER   FOR  XSORT               
         LA    R3,NAMETBLE         INIT TABLE     AS   EMPTY                    
         MVC   0(14,R3),EONMTB     END  OF   NAME TABLE                         
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
*                                  READ 1ST  RCD  FOR  JOB                      
         GOTO1 RD1STREC,DMCB,AIOAREA1                                           
*                                                                               
DISP10   BNE   DISP40              NOT  FOUND,    PROCESS                       
         L     R2,AIOAREA1         ->   JOB  RECORD                             
*                                                                               
         BAS   RE,GETALL           ALLOCATED AMOUNT    IN   ALLNET              
         CP    ALLNET,=P'0'                                                     
         BNE   DISP20                                                           
         CLC   ALLHRS(4),=F'0'                                                  
         BE    DISP30                                                           
*                                                                               
DISP20   ZAP   SAVETOT,ALLNET      SAVE ALLOCATED AMOUNT                        
         MVC   SAVECON,TRNKULC     SAVE CONTRA    ACCOUNT                       
         BAS   RE,PUTTBLE          STORE     ACCT/AMOUNT    IN   TABLE          
*                                                                               
*                                  READ NEXT RCD  FOR  JOB                      
DISP30   GOTO1 RDNXTREC,DMCB,AIOAREA1                                           
         B     DISP10              LOOP BACK UP TO PROCESS                      
*                                                                               
DISP40   ZIC   R4,NUMREC           LOAD NUM  OF   RECS IN   TABLE               
         LTR   R4,R4               ANY  DATA TO   WRITE-OFF?                    
         BZ    NODATA              NO                                           
         LA    R3,NAMETBLE                                                      
         GOTO1 VXSORT,DMCB,(0,(R3)),(R4),ENTRYQ,ENTRYQ,0    SORT TABLE          
         MVC   SAVECON,SPACES                                                   
*                                                                               
DISP50   TWAXC WONFRSTH,WONTABH,PROT=Y  DISPLAY   CONDENSED TABLE               
         MVI   LINES,0             COUNT     NUM  ACCTS     DISPLAYED           
         LA    R4,WONFRSTH         START     OF   COL  ON   SCREEN              
         LA    R3,NAMETBLE         TABLE     OF   ACCOUNTS                      
         CLC   SAVECON,SPACES      LOOP THROUGH   TABLE ?                       
         BE    DISP80                                                           
*                                                                               
         USING ENTRY,R3            MAP  ACCOUNT   TABLE                         
         CLC   CONTRA,EONMTB       END  OF   NAME TABLE ?                       
         BE    DISP90              YES, EXIT                                    
*                                                                               
DISP60   CLC   SAVECON,CONTRA      MATCH     IN   TABLE ?                       
         BE    DISP70              YES, PROCESS   MATCH                         
         LA    R3,ENTRYQ(,R3)      BUMP TO   NEXT EL   IN   TABLE               
         CLC   CONTRA,EONMTB       END  OF   NAME TABLE ?                       
         BNE   DISP60              NO,  CONTINUE  LOOP                          
         B     DISP90              YES, EXIT                                    
*                                                                               
DISP70   LA    R3,ENTRYQ(,R3)      GET  NEXT ENTRY     IN   TABLE               
*                                                                               
DISP80   CLC   CONTRA,EONMTB       END  OF   NAME TABLE ?                       
         BE    DISP90              YES, EXIT                                    
*                                  NO,  MOVE IN   CONTRA    ACCOUNT             
         MVC   8(L'CONTRA,R4),CONTRA                                            
         MVC   SAVECON,CONTRA                                                   
         OI    6(R4),FVOXMT        TRANSMIT  ACCOUNT                            
         ZIC   R0,0(,R4)           GET  LENGTH    OF   FIELD                    
         AR    R4,R0               BUMP TO   AMOUNT    FIELD                    
         EXTED SUBTOT,TEMPAMT,2,FLOAT=-                                         
         MVC   8(L'TEMPAMT,R4),TEMPAMT                                          
         OI    6(R4),FVOXMT        TRANSMIT  AMOUNT    FIELD                    
         ZIC   R0,0(,R4)           GET  LENGTH    OF   FIELD                    
         AR    R4,R0               BUMP FOR  NEXT PAIR                          
*                                                                               
         LA    R3,ENTRYQ(,R3)                                                   
         ZIC   R0,LINES            INCREMENT LINE COUNTER                       
         AH    R0,=H'1'                                                         
         STC   R0,LINES                                                         
*                                                                               
*                                  #    OF   ACCOUNTS  DISPLAYED >              
         CLI   LINES,16                 MAX  ALLOWED?                           
         BE    DISPEX              HAVE TO   EXIT OUT  AND  REDISPLAY           
*                                                                               
*                                  END  OF   NAME TABLE,I.E.                    
         CLC   CONTRA,EONMTB            ALL  ACCOUNTS  PRINTED ?                
         BNE   DISP80              NO,  LOOP BACK UP                            
*                                                                               
DISP90   MVI   SMODE,MARK          SET  FOR  READ IN   NARRATIVE                
         B     DISPWONR            ENTER     WRITE-OFF NARRATIVE                
*                                                                               
DISPEX   MVI   SMODE,DSPLY         SET  FLAG TO   DISPLAY   AGAIN               
*                                  ENTER     FOR  NEXT PAGE OF                  
         B     DISPNXPG                      ACCOUNTS                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*        ADD AN ITEM TO THE 1R ACCOUNT TABLE                          *         
*-------------------------------------------------------              *         
*                                                                     *         
*        THIS ROUTINE IS CALLED ONLY FROM THE DISPLAY ACCOUNTS        *         
*        ROUTINE DIRECTLY ABOVE THIS.  IT ADDS THE ENTRY INTO         *         
*        THE 1R ACCOUNT TABLE.  MAX # OF ACCOUNTS IS NAMTBLMX.        *         
*        NUMREC CONTAINS THE CURRENT NUMBER OF RECORDS IN THE         *         
*        TABLE.                                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ENTRY,R3            MAP  ACCOUNT   TABLE                         
         SPACE 1                                                                
PUTTBLE  NTR1                                                                   
         LA    R3,NAMETBLE         ->   NAME TABLE                              
*                                                                               
PUTTBL1  CLC   SAVECON,CONTRA      ENTRY     IN   TABLE     ALREADY ?           
         BE    PUTTBL3             YES, UPDATE    TABLE     ENTRY               
         CLC   CONTRA,EONMTB       END  OF   NAME TABLE ?                       
         BE    PUTTBL5             YES, ADD  NEW  ENTRY     TO   TABLE          
         LA    R3,ENTRYQ(,R3)      GET  NEXT ENTRY                              
         B     PUTTBL1                                                          
*                                                                               
*                                  ADD  TO   TBL  AMOUNT    FROM                
PUTTBL3  AP    SUBTOT,SAVETOT           NEW  ACCOUNT                            
         B     EXIT                                                             
*                                                                               
*                                  NEW  ENTRY                                   
PUTTBL5  CLI   NUMREC,NAMTBLMX     WILL NEW  ENTRY     FIT ?                    
         BNL   PUTTBLER            NO,  ERROR                                   
         MVC   CONTRA,SAVECON      STORE     NEW  ACCOUNT                       
         ZAP   SUBTOT,SAVETOT      STORE     NEW  AMOUNT                        
         ZIC   R1,NUMREC                                                        
         LA    R1,1(,R1)           INCREMENT RECORD    COUNTER                  
         STC   R1,NUMREC                                                        
         LA    R1,CONTRA                                                        
         LA    R1,ENTRYQ(,R1)                                                   
         MVC   0(14,R1),EONMTB     END  OF   NAME TABLE                         
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*        MAIN PROCESSING                                              *         
*-------------------------------------------------------              *         
*                                                                     *         
*        THE GENERAL FLOW OF THIS OVERLAY IS THE FOLLOWING:           *         
*        1. INITIALIZATION                                            *         
*        2. GET THE NEXT WRITE-OFF NUMBER FROM THE LEDGER RECORD      *         
*        3. LOOP THROUGH TRANSACTIONS ON THE JOB                      *         
*        4. BUILD POSTING ELEMENTS AND MAKE THE POSTINGS              *         
*           BY CREATING A BATCH 57.                                   *         
*        5. UPDATE THE 77 ELEMENTS ON THE TRANSACTIONS FOR JOB        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MAIN     ZAP   BATCHTOT,=P'0'      CLEAR     BATCH     TOTAL                    
         XC    BATCHHRS,BATCHHRS   CLEAR     BATCH     HOURS                    
         MVI   SMODE,INIT          RESET     DISPLAY   FLAG                     
*                                                                               
         BAS   RE,VALACC           VALIDATE  THE  ACCOUNTS                      
         CLI   FERN,OK             ANY  ERRORS    IN   VALIDATION PROC?         
         BNE   MAINEX                                                           
         CLC   FVMSGNO,FVFOK                                                    
         BNE   MAINEX                                                           
*                                                                               
         BAS   RE,GETWO            GET  WRITE-OFF NUMBER                        
         BAS   RE,GETNAR           PROCESS   THE  NARRATIVE                     
*                                                                               
         BAS   RE,RDLOCK           READ AND  LOCK RECORDS                       
         CLI   EFLAG,0             ANY  ERRORS ?                                
         BNE   MAINEX              YES, EXIT                                    
*                                                                               
         EXTED CTATOT,TEMPTOT,2,ALIGN=LEFT,FLOAT=-                              
         OC    WONAMT1,SPACES                                                   
         OC    TEMPTOT,SPACES                                                   
         CLC   WONAMT1,TEMPTOT     DO   TOTALS    MATCH ?                       
         BNE   MAINBAD#            NO,  SAY  NUMS DO   NOT  MATCH               
*                                                                               
         BAS   RE,RDJOB            MAKE POSTINGS  TO   ACCTS                    
         CLI   EFLAG,1             WAS  THERE     AN   ERR  IN POSTING?         
         BNE   *+6                 NO,  CONTINUE                                
         DC    H'0'                ABEND,    ERROR     IN   WRITE-OFF;          
*                                            SOMEBODY  UPDATED   FILES          
*                                                                               
         EXTED BATCHTOT,WONAMT2,2,ALIGN=LEFT,FLOAT=-                            
         OC    WONAMT2,SPACES                                                   
         OI    WONAMT2H+6,FVOXMT   TRANSMIT                                     
*                                                                               
         BAS   RE,MKTRS            MARK THE  TRANSACTIONS                       
*                                                                               
         MVC   FVMSGNO,=AL2(2110)  WRITE-OFF NNNN COMPLETE                      
         MVI   FVMTYPE,FVMINFO                                                  
         MVC   XTRAMESS,SPACES     NUMBER    OF   THE  WRITE-OFF                
         MVC   XTRAMESS(L'WONUMCHR),=C'0000'                                    
         OC    XTRAMESS(L'WONUMCHR),WONUMCHR                                    
*                                                                               
         MVI   FERN,OK                                                          
         B     MAINEX                                                           
*                                                                               
MAINEX   B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        RECOVER WRITE-OFFS (UN-WRITE-OFF)                            *         
*-------------------------------------------------------              *         
*                                                                     *         
*        THE GENERAL FLOW FOR ACTION = REC IS:                        *         
*        1. INITIAL VALIDATION                                        *         
*        2. LOCK THE RECORDS                                          *         
*        3. IF THE WRITE-OFF WAS CREATED TODAY,                       *         
*                A. DELETE THE BATCH POINTERS                         *         
*                B. WRITE RESTORED RECORDS                            *         
*           ELSE                                                      *         
*                C. LOOP THROUGH THE TRANSACTIONS ON THE JOB          *         
*                D. BUILD REVERSAL RECORDS AND  MAKE THE POSTINGS     *         
*                   BY CREATING A NEW BATCH                           *         
*                E. WRITE RESTORED RECORDS                            *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
RECVR    DS    0H                                                               
         LA    R1,BILNUMH                                                       
         CLI   5(R1),0                                                          
         BE    RECVWO#R            NONE,     WRITE-OFF NUMBER  REQUIRED         
*                                                                               
         CLC   BILNUM+4(3),SPACES  WAS  NUMBER    VALID ?                       
         BNE   RECVWO#L            NO,  WRITE-OFF NUMBER     TOO  LARGE         
*                                                                               
         GOTO1 VDATVAL,DMCB,(0,WONDATE),WRDATE                                  
         CLC   0(4,R1),=F'0'                                                    
         BNE   RECVR10             VALIDATE  DATE ENTERED                       
         OC    WONDATE,SPACES                                                   
         CLC   WONDATE,SPACES                                                   
         BNE   RECVINVD            NOT  SPACES,   INVALID   DATE FORMAT         
         MVC   WRDATE,TODAY        TODAY     YYMMDD                             
*                                                                               
RECVR10  GOTO1 VDATCON,DMCB,(0,WRDATE),(10,WONDATE)                             
         OI    WONDATEH+6,FVOXMT   TRANSMIT                                     
*                                                                               
         GOTO1 VDATCON,DMCB,(0,WRDATE),(2,WRDATE2)                              
         GOTO1 VDATCON,DMCB,(0,WRDATE),(1,WRDATEP)                              
         EDIT  (P4,BILNUMP),(4,WONUMCHR)                                        
         OC    WONUMCHR,=C'0000'                                                
         ZAP   BATCHTOT,=P'0'                                                   
         XC    BATCHHRS,BATCHHRS                                                
*                                                                               
*                                  CANCEL    BATCH     OR                       
         CLC   WRDATE2,TODAYC           CREATE    NEW  ONE ?                    
         BNE   RECVR20                                                          
*                                  CANCEL    BATCH                              
         MVI   OKDRAFTS,YES        PROCESS   DRAFT     TRANSACTIONS             
         BAS   RE,RDLOCK           READ AND  LOCK RECORDS                       
         CLI   EFLAG,0             ANY  ERRORS ?                                
         BNE   RECVREX             YES, EXIT                                    
         MVI   BTTYPE,TRNTWRTO     WRITE-OFF TYPE                               
         GOTO1 ABDELDPT            DELETE    BATCH     DIR  POINTERS            
*                                  NOTE:                                        
*                                       DO   NOT  CHECK     FOR ERRS.           
*                                       IF   ERRORS    WERE FOUND,              
*                                       THEN THEY WERE DUE  TO   A              
*                                       SYSTEM CRASH     DURING                 
*                                       MKTRS                                   
         BAS   RE,MKTRS                                                         
         B     RECVWORE            WRITE-OFF RESTORED-ENTER NEXT ACTION         
*                                                                               
*                                  CREATE    NEW  BATCH                         
RECVR20  BAS   RE,VALACC                                                        
         CLI   FERN,OK             POSTING   VALID ?                            
         BNE   RECVR40             NO,  RETURN                                  
         CLC   FVMSGNO,FVFOK                                                    
         BNE   RECVR40                                                          
*                                                                               
         BAS   RE,GETNAR                                                        
*                                                                               
         BAS   RE,RDLOCK           READ AND  LOCK RECORDS                       
         CLI   EFLAG,0             ANY  ERRORS ?                                
         BNE   RECVREX             YES, EXIT                                    
*                                                                               
         BAS   RE,RDJOB                                                         
         CLI   EFLAG,1             ANY  ERRORS ?                                
         BNE   *+6                 NO,  CONTINUE                                
         DC    H'0'                YES, DIE  TO   ENABLE    RECOVERY            
*                                                                               
         CP    BATCHTOT,=P'0'      ANY  AMOUNT ?                                
         BNE   RECVR30             YES, CONTINUE                                
         OC    BATCHHRS,BATCHHRS   ANY  HOURS ?                                 
         BNZ   RECVR30             YES, CONTINUE                                
         DC    H'0'                NO,  DIE  TO   ENABLE    RECOVERY            
*                                                                               
RECVR30  BAS   RE,MKTRS                                                         
         B     RECVWORE            WRITE-OFF RESTORED-ENTER NEXT ACTION         
*                                                                               
RECVR40  LA    R1,BILNUMH          ->   DATA IN   ERROR     AND                 
         ST    R1,FADR             LEAVE     FERN SET  TO   MSG  NUM            
*                                                                               
RECVREX  B     EXIT                EXIT THIS MODULE                             
         EJECT ,                                                                
***********************************************************************         
*        VALIDATE POSTING ACCOUNTS                                    *         
*-------------------------------------------------------              *         
*                                                                     *         
*        VALACC VALIDATES ALL THE ACCOUNTS THAT ARE POSTED TO         *         
*        EXCLUDING THE 1R ACCTS.  THEY ARE VALIDATED AS EACH          *         
*        TRANSACTION IS READ.                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALACC   NTR1                                                                   
         MVC   KEY,SPACES          CLEAR     KEY  FOR  READ                     
*                                                                               
         USING JOBD,R1             MAP  JOB  INFORMATION                        
         LA    R1,JOBINFO          HAS  SR   AND  1C   ACCTS     IN  IT         
         MVC   OFFICE,JOFFC        STORE     THE  OFFICE    OF  THE JOB         
         DROP  R1                                                               
*                                                                               
VALACC2  MVC   KEY,SPACES                                                       
         MVI   KEY,PMDKTYPQ        READ MEDIA     RECORD (X'09')                
         MVC   KEY+1(1),COMPANY                                                 
         MVC   KEY+2(1),LJOB       JOB'S     MEDIA     CODE                     
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 MUST FIND IT                                 
         DC    H'0'                                                             
*                                                                               
         L     R4,AIOAREA1                                                      
         AH    R4,DATADISP                                                      
         SR    R0,R0                                                            
*                                                                               
VALACC3  CLI   0(R4),0             TEST FOR  EOR                                
         BNE   *+6                                                              
         DC    H'0'                BIG  TROUBLE                                 
*                                                                               
         CLI   0(R4),PMDELQ        TEST FOR  MEDIA     ELEMENT                  
         BE    VALACC4             YES                                          
         IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     VALACC3                                                          
*                                                                               
         USING PMDELD,R4           MAP  PRODUCTION     MEDIA     EL             
VALACC4  CLI   PMDLN,PMDLN2Q       TEST LARGE     ENOUGH    ELEMENT             
         BL    VALACC5             NO                                           
         OC    WRTDEF,PMDTWO+1     LOOK FOR  TIME WRITE-OFF A/C                 
         BNZ   VALACC6             YES, HAVE ONE                                
*                                                                               
VALACC5  MVC   WRTDEF,SPACES                                                    
         MVC   WRTDEF(4),=C'SIWO'  USE  OLD  DEFAULT                            
         DROP  R4                                                               
*                                                                               
VALACC6  CLI   WOAOVR,0            DID  THEY OVERRIDE  WRITE-OFF ACCT ?         
         BE    VALACC7             NO                                           
         MVC   WRTACC,WOAOVR       YES, VERIFY    THE OVERRIDE UP FRONT         
         BAS   RE,GETWRT           READ THE  ACCOUNT                            
         BNE   VALACCX             ON   ERROR,    EXIT                          
*                                                                               
VALACC7  MVC   KEY,SPACES                                                       
         MVC   KEY(LCULACT),JOBKEY CHECK     CLI/PRD/JOB                        
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   VALACCX             ON   ERROR,    EXIT                          
         BAS   RE,POSTEST                                                       
         BNE   VALACCX             ON   ERROR,    EXIT                          
         XC    TMELVSJ,TMELVSJ     CLEAR HIERARCHY SAVE AREA                    
         MVC   TMELVSJ(L'PRODHEIR),PRODHEIR                                     
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   SJCPJNM,WORK        SAVE NAME OF   JOB                           
*                                                                               
         CLI   COSTING,NO          IS   COMPANY   ON   COST ACCOUNTING?         
         BE    VALACCOK            DO   NOT  VALIDATE  1C/12     IF NOT         
*                                                                               
         USING JOBD,R1             MAP  JOB  INFORMATION                        
         LA    R1,JOBINFO          CHECK     1C   ACCOUNT                       
         MVC   KEY,SPACES                                                       
         MVC   KEY(LCULACT),JCOST                                               
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         DROP  R1                                                               
*                                                                               
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   VALACCX             ON   ERROR,    EXIT                          
         BAS   RE,POSTEST                                                       
         BNE   VALACCX             ON   ERROR,    EXIT                          
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   ACC1CN,WORK                                                      
         MVC   ACC1C,KEY                                                        
*                                                                               
VALACCOK MVC   XTRAMESS,SPACES     CLEAR     EXTRA     DATA                     
*                                                                               
VALACCX  B     EXIT                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*        GET WRITE-OFF NUMBER FROM LEDGER RECORD                      *         
*-------------------------------------------------------              *         
*                                                                     *         
*        GETWO GETS THE NEXT WRITE-OFF NUMBER AVAILABLE.              *         
*        THIS NUMBER IS THEN STORED IN THE LEDGER RECORD              *         
*        AFTER IT IS INCREMENTED.  THE WRITE-OFF NUMBER               *         
*        IS STORED IN BINARY IN TWO BYTES.  THE NUMBER RESETS         *         
*        ITSELF AFTER REACHING 9999.                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETWO    NTR1                                                                   
         MVC   KEY,SPACES          BUILD     KEY  FOR  LEDGER    RECORD         
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(LUL),SJ                                                    
         MVC   KEYSAVE,KEY                                                      
         GOTO1 AREADL,AIOAREA2     READ AND  UPDATE    THE  RECORD              
         BE    *+6                                                              
         DC    H'0'                MUST BE   ABLE TO   READ LEDGER REC          
*                                                                               
         L     R4,AIOAREA2                                                      
         AH    R4,DATADISP                                                      
*                                                                               
GETWO1   CLI   0(R4),LDGELQ        FIND LEDGER    ELEMENT  (X'14')              
         BE    GETWO3                                                           
         CLI   0(R4),0                                                          
         BE    EXIT                                                             
         ZIC   R0,1(,R4)                                                        
         AR    R4,R0                                                            
         B     GETWO1                                                           
*                                                                               
         USING LDGELD,R4           MAP  LEDGER    ELEMENT                       
GETWO3   SR    R2,R2                                                            
         ICM   R2,3,LDGWONUM       LOAD IN   W/O  NUMBER                        
         L     R1,=F'9999'                                                      
         CR    R2,R1               TIME TO   ROLLOVER  TO   ZERO ?              
         BL    *+6                                                              
         SR    R2,R2                                                            
         LA    R2,1(,R2)           INCREMENT W/O  NUMBER                        
         STCM  R2,3,LDGWONUM                                                    
         XC    WONUM,WONUM                                                      
         STCM  R2,3,WONUM          SAVE WRITE-OFF NUMBER                        
         GOTO1 AWRITE,AIOAREA2                                                  
*                                                                               
         EDIT  (B2,WONUM),(4,WONUMCHR),ZERO=NOBLANK                             
         OC    WONUMCHR(4),=C'0000'                                             
*                                                                               
GETWO8   GOTO1 VDATAMGR,DMCB,DMUNLK,ACCOUNT,AIOAREA2                            
         B     EXIT                                                             
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*        FORMAT NARRATIVE                                             *         
*-------------------------------------------------------              *         
*                                                                     *         
*        THIS ROUTINE READS FROM THE SCREEN TWO 55 CHARACTER          *         
*        LINES AND STORES THEM IN A NARRATIVE BUFFER.  THE            *         
*        ACTUAL NARRATIVE IS 140 BYTES LONG SINCE THE FIRST           *         
*        THIRTY BYTES CONTAINS THE DATE OF THE WRITE-OFF, THE         *         
*        WRITE-OFF NUMBER AND THE NUMBER OF HOURS                     *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETNAR   NTR1                                                                   
         MVC   NARRATVE(100),SPACES                                             
         MVC   NARRATVE+100(50),SPACES                                          
         MVC   NARRATVE(3),=C'W/O'                                              
         CLI   ACTION,REC                                                       
         BNE   *+10                                                             
         MVC   NARRATVE(3),=C'W/R'                                              
         MVC   NARRATVE+4(4),WONUMCHR                                           
         GOTO1 VDATCON,DMCB,(2,TODAYC),(8,NARRATVE+9)                           
         MVC   NARRATVE+19(12),=C'XXXXXXXXXXXX'                                 
         CLI   ACTION,REC                                                       
         BE    GETNARX                                                          
         LA    R3,NARRATVE+35                                                   
         MVC   0(55,R3),WONLIN1                                                 
         LA    R3,55(,R3)                                                       
         MVC   0(55,R3),WONLIN2                                                 
*                                                                               
GETNARX  GOTO1 VSQUASHR,DMCB,NARRATVE,150                                       
         ICM   R3,15,4(R1)                                                      
         STC   R3,NARRLEN                                                       
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*        READ TRANSACTIONS                                            *         
*-------------------------------------------------------              *         
*                                                                     *         
*        RDJOB LOOPS THROUGH ALL TRANSACTIONS ON THE SPECIFIED        *         
*        JOB LOOKING FOR 77 ELEMENTS WITH PENDING ALLOCATIONS.        *         
*                                                                     *         
***********************************************************************         
         USING TRNRECD,R2                                                       
RDJOB    NTR1                                                                   
         L     R2,AIOAREA3         R2 = CURRENT TRANSACTION                     
         XC    SAVEHOUR,SAVEHOUR                                                
         XC    TMSCOST,TMSCOST                                                  
         XC    TMSOFF,TMSOFF                                                    
         XC    TMSDATE,TMSDATE                                                  
*                                                                               
         MVI   BTTYPE,TRNTWRTO     WRITE-OFF TYPE                               
         GOTO1 ABBLDKHD            BUILD KEY FOR BATCH HEADER                   
*                                  READ 1ST  RCD  FOR  JOB                      
         GOTO1 RD1STREC,DMCB,('READLOCK',AIOAREA3)                              
*                                                                               
         B     RDJOB20                                                          
*                                  READ NEXT RCD  FOR  JOB                      
RDJOB10  GOTO1 RDNXTREC,DMCB,('READLOCK',AIOAREA3)                              
RDJOB20  BNE   RDJOBEX             NOT FOUND, END OF JOB RECORDS                
*                                                                               
         USING TRNELD,R4                                                        
         LA    R4,ACCORFST(,R2)    R4 = A(1ST ELEMENT)                          
*                                                                               
         CLI   ACTION,REC          RECOVER?                                     
         BNE   RDJOB30             NO, SKIP                                     
         CLI   TRNTYPE,TRNTWRTO    WRITE-OFF?                                   
         BNE   RDJOB10             NO, READ NEXT RECORD                         
*                                                                               
RDJOB30  MVC   TMOS,TRNMOS         SAVE TRANSACTION MOS                         
         ZAP   SAVEAMNT,TRNAMNT    SAVE TRANSACTION TOTAL                       
*                                                                               
         BAS   RE,GELDATA          GET ELEMENT DATA                             
*                                                                               
         BAS   RE,GETCTAMT         GET CTAMNT                                   
         CLI   EFLAG,X'FF'         SKIP RECORD?                                 
         BE    RDJOB10             YES, READ NEXT                               
         CLI   EFLAG,0             ANY ERRORS?                                  
         BE    *+6                 NO, SKIP                                     
         DC    H'0'                YES, ABEND - SHOULD NOT OCCUR                
*                                       SEE - FERN AND FVMSGNO                  
*                                                                               
         AP    BATCHTOT,CTAMNT     KEEP TRACK OF BATCH TOTAL                    
*                                                                               
         MVC   NARRATVE+18(12),SPACES                                           
         OC    CTHOUR,CTHOUR                                                    
         BZ    RDJOB40                                                          
         L     R1,CTHOUR                                                        
         LCR   R1,R1               REVERSE THE SIGN OF THE HOURS                
         ST    R1,CTHOUR                                                        
         LA    R3,NARRATVE+18                                                   
         EXTED (B4,CTHOUR),(6,(R3)),2,FLOAT=-,LANG='LANGEUS'                    
         MVC   NARRATVE+27(3),=C'HRS'                                           
*                                                                               
         L     R1,BATCHHRS                                                      
         L     R0,CTHOUR                                                        
         AR    R0,R1                                                            
         ST    R0,BATCHHRS                                                      
*                                                                               
RDJOB40  XC    WRTACC,WRTACC       RE-INITIALIZE WRITE-OFF ACCOUNT              
         BAS   RE,POSTAMT          ADD POSTINGS                                 
         B     RDJOB10             GET NEXT                                     
*                                                                               
RDJOBEX  MVI   EFLAG,0             NORMAL EXIT                                  
         GOTO1 ABADDHDR            ADD THE BATCH ITEMS AND HEADER               
         OI    TRNINDS,TRNILAST    CALL ADDTRNS TO FINISH UP                    
         OI    TRNINDS2,TRNIUPDG   UPDATE THE G/L                               
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    EXIT                                                             
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*        GET ELEMENT DATA FOR A RECORD                                *         
*-------------------------------------------------------              *         
*                                                                     *         
*        GELDATA SAVES DATA FROM THE FOLLOWING ELEMENTS:              *         
*        1. SPECIAL     POSTING ACCOUNT ELEMENT (X'2C')               *         
*        2. PERSONNEL   RATE            ELEMENT (X'40')               *         
*        3. TRANSACTION STATUS          ELEMENT (X'60')               *         
*        4. CLIENT      OFFICE  CODE    ELEMENT (X'65')               *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
GELDATA  NTR1                                                                   
         L     R2,AIOAREA3         ->   CURRENT   TRANS                         
*                                                                               
         XC    SVPRTEL,SVPRTEL     CLEAR SAVE PERSONNEL RATE EL                 
         XC    SVPIDEL,SVPIDEL     CLEAR PID ELEMENT SAVE AREA                  
         MVI   TMSSTAT2,0          CLEAR TRSSTAT2  SAVE AREA                    
         MVI   TMSSTAT3,0          CLEAR TRSSTAT3  SAVE AREA                    
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
         XC    TMEPID,TMEPID       CLEAR PID FOR CADET                          
*                                                                               
         LA    R4,ACCORFST(,R2)                                                 
*                                                                               
GELDAT10 ZIC   R0,1(,R4)           GET  ELEMENT   DATA                          
         AR    R4,R0                                                            
         CLI   0(R4),0             END  OF   THE  RECORD ?                      
         BE    GELDATEX            YES, EXIT                                    
         CLI   TRNEL,SPAELQ        2C - CLIENT    COSTING   ACCOUNT             
         BE    GELDAT20                                                         
         CLI   TRNEL,PRTELQ        40 - PERSONNEL ELEMENT   FOR  RATE           
         BE    GELDAT30                                                         
         CLI   TRNEL,TRSELQ        60 - STATUS FOR TMSSTAT2 & TMSSTAT3          
         BE    GELDAT40                                                         
         CLI   TRNEL,ANOELQ        65 - CLIENT    OFFICE    CODE                
         BE    GELDAT50                                                         
         CLI   TRNEL,PIDELQ        X'D8' - PERSON ID ELEMENT                    
         BE    GELDAT60                                                         
         B     GELDAT10            LOOP BACK UP   AGAIN                         
*                                                                               
         USING SPAELD,R4           MAP  SPECIAL   POSTING   ACCOUNT  EL         
GELDAT20 CLI   SPATYPE,SPATCCST    CLIENT    COSTING ?                          
         BNE   GELDAT10            NO                                           
         MVC   TMSCOST,SPAAULA     SAVE UNIT/LEDGER/ACCOUNT                     
         B     GELDAT10            GET  NEXT ELEMENT                            
         DROP  R4                                                               
*                                                                               
         USING PRTELD,R4           MAP  PERSONNEL RATE ELEMENT                  
GELDAT30 XC    SVPRTEL,SVPRTEL     SAVE PERSONNEL RATE ELEMENT                  
         ZIC   R1,PRTLN            GET  ELEMENT   LENGTH                        
         BCTR  R1,0                                                             
         EXMVC R1,SVPRTEL,0(R4)    SAVE X'40'     ELEMENT                       
*                                                                               
         ZAP   DUB,PRTHOUR         SAVE HOURS                                   
         CVB   R1,DUB                                                           
         STCM  R1,15,SAVEHOUR                                                   
         ZAP   TMSRATE,PRTRATE     SAVE HOURLY    RATE                          
*                                  SAVE HOURLY    RATE EFFECTIVE                
         MVC   TMSEFF,PRTSTRT           START     DATE                          
         MVI   TMSTYP,TIMTCB       CLIENT    BILLABLE                           
         TM    PRTSTAT,PRTSRTEQ    SPECIAL   NON-BILLABLE   TIME 'R' ?          
         BZ    *+8                 NO,  SKIP                                    
         MVI   TMSTYP,TIMTCR       CLIENT    REALIZATION                        
         TM    PRTSTAT,PRTSNOTQ    NOT  BILLABLE  TIME 'N' ?                    
         BZ    *+8                                                              
         MVI   TMSTYP,TIMTCN       CLIENT    NON-BILLABLE                       
         B     GELDAT10            GET  NEXT ELEMENT                            
         DROP  R4                                                               
*                                                                               
         USING TRSELD,R4           MAP  TRANSACTION    STATUS    EL             
GELDAT40 MVC   TMSSTAT2,TRSSTAT2   SAVE STATUS                                  
*                                  ONLY NEED TMS  STUFF                         
         NI    TMSSTAT2,TRSSTADJ+TRSSTMSS+TRSSTIME                              
         OI    TMSSTAT2,TRSSTIME   X'01'     TIMESHEET REGULAR                  
         MVC   TMSSTAT3,TRSSTAT3                                                
         NI    TMSSTAT3,X'FF'-TRSSNBIL                                          
         MVC   TMOS,TRSPMOS        POSTING   MOS                                
         MVC   TMSDATE,TRSDATE     ACTIVITY  DATE                               
         B     GELDAT10            GET  NEXT ELEMENT                            
         DROP  R4                                                               
*                                                                               
         USING ANOELD,R4           MAP  ANALYZED  OFFICE ELEMENT                
GELDAT50 CLI   ANOTYPE,ANOTCLI     CLIENT    OFFICE ?                           
         BNE   GELDAT10            NO                                           
         MVC   TMSOFF,ANOOFFC      SAVE OFFICE    CODE                          
         B     GELDAT10            GET  NEXT ELEMENT                            
         DROP  R4                                                               
*                                                                               
         USING PIDELD,R4                                                        
GELDAT60 ZIC   R1,PIDLN            GET ELEMENT LENGTH                           
         BCTR  R1,0                                                             
         EXMVC R1,SVPIDEL,0(R4)    SAVE X'D8' ELEMENT                           
         MVC   TMEPID,PIDNO                                                     
         B     GELDAT10                                                         
         DROP  R4                                                               
*                                                                               
GELDATEX B     EXIT                RETURN    TO   CALLER                        
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*        BUILD POSTING ELEMENTS                                       *         
*-------------------------------------------------------              *         
*                                                                     *         
*        POSTAMT CREATES THE CREDIT/DEBIT POSTING ELEMENTS.           *         
*        ALL RECORDS SHOULD BE VALID BY THE TIME POSTAMT IS           *         
*        ENTERED, AND ANY ERROR FOUND WILL CREATE A DUMP.             *         
*        THE INCOME SUSPENSE ACCOUNT IS TAKEN FROM THE 1R             *         
*        PERSON RECORD; IT IS THE SAME AS THE INCOME SUSPENSE         *         
*        WITH THE ONLY DIFFERENCE BEING THE LEDGER.                   *         
*        ALL RECORDS CREATED ARE NON-COMMISSIONABLE.                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
POSTAMT  NTR1                                                                   
         L     R2,AIOAREA3         ->   CURRENT   TRANS                         
*                                                                               
         BAS   RE,TESTMS           SEE  IF   TMS  POSTINGS  NEEDED              
*                                                                               
         BAS   RE,GETCACTS         GET  CONTRA    ACCOUNTS                      
         CLI   EFLAG,0             ANY  ERRORS ?                                
         BE    *+6                 NO,  CONTINUE                                
         DC    H'0'                YES, ABEND -   SHOULD    NOT  OCCUR          
*                                       SEE -     FERN AND  FVMSGNO             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - DR JOB                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
                                                                                
         BAS   RE,ADD2SUB          ADD POSTING ACCOUNTS TO SUBTAB               
         BRAS  RE,BUILDDB          BUILD THE WORKCODE ELEMENT                   
*                                                                               
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
         MVC   NEW.TRNKCULA,TRNKCULA    DEBIT     JOB                           
         MVC   NEW.TRNKCULC,TRNKCULC    CONTRA    ACCOUNT                       
         MVC   NEW.TRNKWORK,TRNKWORK    ORIGINAL  WORK CODE                     
         MVC   NEW.TRNKDATE,TRNKDATE    ORIGINAL  TRANSACTION    DATE           
         MVC   NEW.TRNKREF,TRNKREF      ORIGINAL  REFERENCE NUMBER              
         MVC   TRNCACNM,CONNAME         CONTRA    ACCOUNT   NAME                
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL           ->   WORK TRANSACTION    ELEMENT             
         MVI   TRNEL,TRNELQ        TRANSACTION    ELEMENT                       
*                                  BILL DATE                                    
         MVC   TRNDATE,NEW.TRNKDATE                                             
         MVC   TRNREF,NEW.TRNKREF  REFERENCE NUMBER                             
         MVI   TRNTYPE,TRNTWRTO    WRITE-OFF TYPE                               
*                                  DEBIT     AND  NON-COMMISSIONABLE            
         MVI   TRNSTAT,TRNSDR+TRNSNOCM                                          
         MVC   TRNBTCH,BTCHREF     BATCH     REFERENCE                          
         ZAP   DUB,CTAMNT          MOVE IN   AMOUNT    ALLOCATED                
         MP    DUB,=P'-1'          -DR                                          
         ZAP   TRNAMNT,DUB                                                      
         MVC   TRNANAL,TRNKWORK    WORK CODE                                    
*                                  NARRATIVE                                    
         MVC   TRNNARR(L'NARRATVE),NARRATVE                                     
         GOTO1 VSQUASHR,DMCB,TRNNARR,L'NARRATVE                                 
         ICM   R4,15,4(R1)         GET  NEW  LEN  OF   NARRATIVE                
         LA    R4,TRNLN1Q(,R4)     ADD  BASE LENGTH                             
         STC   R4,TRNLN            SAVE EL   LENGTH                             
*                                                                               
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
         DROP  R3,NEW                                                           
*                                                                               
         USING SPAELD,R3           MAP  SPECIAL   POSTING   A/C  EL             
         LA    R3,ELEMENT          ->   ELEMENT   BUILD     AREA                
         XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         MVI   SPAEL,SPAELQ        X'2C' -   POSTING   EL - W/O  ACCT           
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATWOFF    WRITE-OFF ACCOUNT                            
         MVC   SPAAULA,WRTACC                                                   
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    SPECIAL   POSTING  EL         
*                                                                               
         OC    TMSCOST,TMSCOST     ANY  TMS  COST ACCOUNT ?                     
         BZ    POST16              NO,  DO   NOT  ADD TMS OFFICE EITHER         
         XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         MVI   SPAEL,SPAELQ        X'2C' -   SPECIAL  POSTING    EL             
         MVI   SPALN,SPALNQ                                                     
         MVI   SPATYPE,SPATCCST    COST ACCOUNT                                 
         MVC   SPAAULA,TMSCOST                                                  
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    SPECIAL   POSTING  EL         
*                                                                               
POST16   CLI   SVPRTEL+1,0         ANY  PERSONNEL RATE EL   SAVED ?             
         BE    POST17              NO,  SKIP                                    
*                                                                               
         USING PRTELD,R3           MAP  PERSONNEL RATE ELEMENT                  
         XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         ZIC   R1,SVPRTEL+1        GET  LENGTH    OF   ELEMENT                  
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),SVPRTEL    MOVE THE  X'40'     ELEMENT                  
         ICM   R1,15,CTHOUR        GET  HOURS WRITTEN OFF                       
         CVD   R1,DUB                                                           
         ZAP   PRTHOUR,DUB                                                      
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    PERSONNEL RATE EL             
*                                                                               
         USING SPDELD,R3           MAP  SUBSIDIARY     POSTING   EL             
POST17   XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         MVI   SPDEL,SPDELQ        X'4C' -   SUBSIDIARY POSTING  EL             
*                                  LENGTH    OF   ELEMENT                       
         MVI   SPDLN,SPDLN1Q+LULACT                                             
         MVC   SPDACCS(LULACT),SKACCNT+1                                        
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    SUBSIDIARY POSTING EL         
*                                                                               
         USING TRSELD,R3           MAP  TRANSACTION    STATUS    EL             
         XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         MVI   TRSEL,TRSELQ        X'60' -   TRANSACTION    STATUS   EL         
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT3,TMSSTAT3                                                
         OI    TRSSTAT3,TRSSNBIL   INDICATE NON-BILLABLE                        
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    PROD TX   ACTIVITY EL         
*                                                                               
         USING ANOELD,R3           MAP  ANALYZED  OFFICE    ELEMENT             
         OC    TMSCOST,TMSCOST     ANY  TMS  COST ACCOUNT ?                     
         BZ    POST18              NO,  DO   NOT  ADD TMS OFFICE EITHER         
         OC    TMSOFF,TMSOFF       ANY  TMS  OFFICE ?                           
         BZ    POST18              NO,  DO   NOT  ADD  TMS  OFFICE              
         XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         MVI   ANOEL,ANOELQ        X'65' -   ANALYZED  OFFICE    EL             
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTCLI     OFFICE    CODE                               
         MVC   ANOOFFC,TMSOFF      OFFICE                                       
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    ANALYZED  OFFICE   EL         
*                                                                               
         USING PTAELD,R3           MAP  PROD TRANSACTION    ACTIVITY EL         
POST18   XC    ELEMENT,ELEMENT     CLEAR     THE  BUILD     AREA                
         MVI   PTAEL,PTAELQ        X'77' -   PROD TRANSACTION  ACTIVITY         
         MVI   PTALN,PTAWLN2Q                                                   
         MVC   PTADATE,TODAYC      TODAY     COMPRESSED                         
         MVC   PTAWDAT,TODAYP      TODAY     PACKED                             
         MVI   PTATYPE,PTATWOF                                                  
         MVC   PTACUR,CURCOD                                                    
         MVC   PTAWREF(2),WO                                                    
         ZAP   PTANET,=P'0'                                                     
         ZAP   PTANETF,=P'0'                                                    
         ZAP   PTACDSC,=P'0'                                                    
         MVC   PTAMOA,PMOS                                                      
*                                                                               
         CLI   ACTION,REC          RECOVERY ?                                   
         BNE   POST19              NO,  SKIP                                    
         MVI   PTATYPE,PTATWOFR                                                 
         MVC   PTAWREF(2),WR                                                    
         MVC   PTAWDAT,WRDATEP                                                  
*                                                                               
POST19   MVC   PTAWREF+2(4),WONUMCHR                                            
         ZAP   DUB,CTAMNT                                                       
         MP    DUB,=P'-1'                                                       
         ZAP   PTANET,DUB                                                       
*                                                                               
         L     RF,CTHOUR                                                        
         STH   RF,PTAHOURS                                                      
*                                                                               
         MVC   PTAWWOT,BILTYP      SAVE WRITE-OFF TYPE FROM HEADLINE            
         CLI   ACTION,WRT          TEST ACTION=WRITE-OFF                        
         BE    *+10                YES, SKIP                                    
         MVC   PTAWWOT,WOTYPE      FOR RECOVERY, SET TYPE=WRITE-OFF             
         GOTO1 ABADDLST,(R3)       ADD BATCH: PROD TX ACTIVITY EL               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         USING PIDELD,R3                                                        
         CLI   SVPIDEL,0           ANY PID ELEMENT?                             
         BE    POST20              NO                                           
         XC    ELEMENT,ELEMENT     CLEAR BUILD AREA                             
         ZIC   R1,SVPIDEL+1        GET LENGTH OF ELEMENT                        
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),SVPIDEL    MOVE THE X'D8' ELEMENT                       
         GOTO1 ABADDLST,(R3)       ADD PID ELEMENT                              
*                                                                               
POST20   GOTO1 ABADDRFT            ADD DRAFT TRANSACTION                        
*                                                                               
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - DEBIT SK ACCOUNT                           *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
         CLC   SKACCNT+1(LUL),SI   ANY  SK   ACCOUNT ?                          
         BE    POST28              NO,  SKIP THIS POSTING                       
*                                                                               
         L     R6,ATIO                                                          
*                                                                               
         MVC   NEW.TRNKCULA,SKACCNT     SK   ACCOUNT   TO   POST                
         MVC   NEW.TRNKCULC,TRNKCULA    CONTRA =  JOB                           
         MVC   NEW.TRNKOFF,SPACES                                               
         MVI   NEW.TRNKSBR,0            TX    SUB-REFERENCE NUMBER              
         MVC   TRNCACNM,SJCPJNM         CONTRA    ACCOUNT   NAME                
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
*                                  DEBIT     AND  NON-COMMISSIONABLE            
         MVI   TRNSTAT,TRNSDR+TRNSNOCM                                          
         ZAP   TRNAMNT,CTAMNT      MOVE IN   AMOUNT    ALLOCATED                
         MVC   TRNOFFC,OFFICE      MOVE IN   OFFICE                             
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
POST28   GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
         DROP  R2,R3,NEW                                                        
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - CREDIT SI ACCOUNT                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
         MVC   WODATE,TODAYP       WRITE-OFF DATE                               
         MVC   WOREF(2),WO         WRITE-OFF NUMBER                             
         CLI   ACTION,REC          RECOVERY ?                                   
         BNE   POST30              NO,  SKIP                                    
         MVC   WODATE,WRDATEP      DATE OF   WRITE-OFF                          
         MVC   WOREF(2),WR         NO.  OF   WRITE-OFF                          
*                                                                               
POST30   MVC   WOREF+2(4),WONUMCHR WRITE-OFF NUMBER                             
*                                                                               
         CLC   SKACCNT+1(LUL),SI   DO   WE   HAVE AN   SK   ACCOUNT ?           
         BE    POST32              NO,  SKIP THIS POSTING                       
*                                                                               
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
*                                  CREDIT    INCOME    ROLLOVER                 
         MVC   NEW.TRNKCULA,SIACCNT          ACCOUNT                            
         MVC   NEW.TRNKCULC,SPACES CLEAR     CONTRA    ACCOUNT                  
*                                  CONTRA    PRODUCT                            
         L     RF,ADPRD            ->        PRODUCT   RECORD                   
         MVC   NEW.TRNKCULC,TRNKCULA-TRNRECD(RF)                                
         MVC   NEW.TRNKOFF,SPACES  OFFICE                                       
         MVC   NEW.TRNKDATE,WODATE WRITE-OFF DATE                               
         MVC   NEW.TRNKREF,WOREF   WRITE-OFF NUMBER                             
         MVC   TRNCACNM,SJCPNM     GET  CONTRA    NAME                          
*                                                                               
         LA    R3,WKTRNL           ->   WORK TRANSACTION    ELEMENT             
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
*                                  WRITE-OFF DATE                               
         MVC   TRNDATE,NEW.TRNKDATE                                             
         MVC   TRNREF,NEW.TRNKREF  WRITE-OFF NUMBER                             
         MVI   TRNSTAT,TRNSNOCM    CREDIT    AND  NON-COMMISSIONABLE            
         MVC   TRNOFFC,OFFICE      OFFICE                                       
         ZAP   TRNAMNT,CTAMNT      MOVE IN   AMOUNT    ALLOCATED                
*                                                                               
         ZAP   DUB,TRNAMNT         TRANSACTION    AMOUNT                        
         BAS   RE,BLDMT            BUILD     MEDIA     TRANSFER  EL             
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
         DROP  R2,R3,NEW                                                        
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - CREDIT WRITE-OFF ACCOUNT                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
POST32   L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
         MVC   NEW.TRNKCPY,COMPANY COMPANY                                      
         MVC   NEW.TRNKULA,WRTACC  WRITE-OFF ACCOUNT                            
         MVC   NEW.TRNKCULC,SPACES CLEAR     CONTRA    JOB                      
*                                  CONTRA    PRODUCT                            
         L     RF,ADPRD            ->        PRODUCT   RECORD                   
         MVC   NEW.TRNKCULC,TRNKCULA-TRNRECD(RF)                                
         MVC   NEW.TRNKOFF,SPACES  OFFICE                                       
         MVC   NEW.TRNKDATE,WODATE WRITE-OFF DATE                               
         MVC   NEW.TRNKREF,WOREF   WRITE-OFF NUMBER                             
         MVC   TRNCACNM,SJCPNM     GET  CONTRA    NAME                          
*                                                                               
         LA    R3,WKTRNL           ->   WORK TRANSACTION    ELEMENT             
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
*                                  WRITE-OFF DATE                               
         MVC   TRNDATE,NEW.TRNKDATE                                             
         MVC   TRNREF,NEW.TRNKREF  WRITE-OFF NUMBER                             
         MVI   TRNSTAT,TRNSNOCM    CREDIT    AND  NON-COMMISSIONABLE            
         MVC   TRNOFFC,OFFICE      OFFICE                                       
         ZAP   DUB,CTAMNT          MOVE IN   AMOUNT    ALLOCATED                
         MP    DUB,=P'-1'          -CR                                          
         ZAP   TRNAMNT,DUB                                                      
*                                                                               
*        ZAP   DUB,TRNAMNT         TRANSACTION    AMOUNT                        
         BAS   RE,BLDMT            BUILD     MEDIA     TRANSFER  EL             
         DROP  R2,R3,NEW                                                        
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - DR/CR COSTING INCOME FOR                   *         
*                          SK TO SI TRANSFER POSTING.                 *         
*                          CREDIT 1C  WITH CONTRA 12I AND             *         
*                          DEBIT  12I WITH CONTRA 1C                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
         CLI   COSTING,NO                                                       
         BE    POST90              ADDREC    LAST BATCH     ITEM RCD            
*                                                                               
         CLC   SKACCNT+1(LUL),SI   DO   WE   HAVE AN   SK   ACCOUNT ?           
         BE    POST34              NO,  SKIP THIS POSTING                       
*                                                                               
         CLC   ACC12,ACC12I        ARE  12   ACCOUNTS  THE  SAME ?              
         BE    POST36              YES, SKIP THE  NEXT POSTINGS                 
*                                                                               
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
*                                  CREDIT    1C   WITH CONTRA    12I            
         MVC   NEW.TRNKCULA,ACC1C  COSTING   ACCOUNT                            
*                                  COSTING   INCOME    FOR                      
         MVC   NEW.TRNKCULC,ACC12I      WRITE-OFF ROLLOVER  SI                  
         MVC   NEW.TRNKOFF,SPACES  NO   WORK CODE                               
         MVC   NEW.TRNKDATE,WODATE WRITE-OFF DATE                               
         MVC   NEW.TRNKREF,WOREF   WRITE-OFF NUMBER                             
         MVC   TRNCACNM,ACC12IN    GET  CONTRA    NAME                          
*                                                                               
         LA    R3,WKTRNL           ->   WORK TRANSACTION    ELEMENT             
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
*                                  WRITE-OFF DATE                               
         MVC   TRNDATE,NEW.TRNKDATE                                             
         MVC   TRNREF,NEW.TRNKREF  WRITE-OFF NUMBER                             
*                                  DEBIT     AND  NON-COMMISSIONABLE            
         MVI   TRNSTAT,TRNSDR+TRNSNOCM                                          
         MVC   TRNOFFC,OFFICE      OFFICE                                       
         ZAP   TRNAMNT,CTAMNT      MOVE IN   AMOUNT    ALLOCATED                
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
*                                  DEBIT     12I  WITH CONTRA    1C             
*                                  SWITCH    ACCOUNT\CONTRA                     
         XC    NEW.TRNKCULA,NEW.TRNKCULC                                        
         XC    NEW.TRNKCULC,NEW.TRNKCULA                                        
         XC    NEW.TRNKCULA,NEW.TRNKCULC                                        
         MVI   NEW.TRNKSBR,0                                                    
         MVC   TRNCACNM,ACC1CN     CONTRA    NAME                               
*                                                                               
         LA    R3,NEW.TRNRFST                                                   
         MVI   TRNSTAT,TRNSNOCM    CREDIT    AND  NON-COMMISSIONABLE            
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
         DROP  R2,R3,NEW                                                        
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - DR/CR COSTING INCOME                       *         
*                          DEBIT  1C  WITH CONTRA 12  AND             *         
*                          CREDIT 12  WITH CONTRA 1C                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
*                                                                               
POST34   L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
*                                  DEBIT     1C   WITH CONTRA    12             
         MVC   NEW.TRNKCULA,ACC1C  COSTING   ACCOUNT                            
*                                  COSTING   INCOME    FOR                      
         MVC   NEW.TRNKCULC,ACC12       WRITE-OFF SI   ACCOUNT                  
         MVC   NEW.TRNKOFF,SPACES  NO   WORK CODE                               
         MVC   NEW.TRNKDATE,WODATE WRITE-OFF DATE                               
         MVC   NEW.TRNKREF,WOREF   WRITE-OFF NUMBER                             
         MVC   TRNCACNM,ACC12N     GET  CONTRA    NAME                          
*                                                                               
         LA    R3,WKTRNL           ->   WORK TRANSACTION    ELEMENT             
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
*                                  WRITE-OFF DATE                               
         MVC   TRNDATE,NEW.TRNKDATE                                             
         MVC   TRNREF,NEW.TRNKREF  WRITE-OFF NUMBER                             
*                                  DEBIT     AND  NON-COMMISSIONABLE            
         MVI   TRNSTAT,TRNSDR+TRNSNOCM                                          
         MVC   TRNOFFC,OFFICE      OFFICE                                       
         ZAP   DUB,CTAMNT          MOVE IN   AMOUNT    ALLOCATED                
         MP    DUB,=P'-1'          -DR                                          
         ZAP   TRNAMNT,DUB                                                      
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
*                                  CREDIT    12   WITH CONTRA    1C             
*                                  SWITCH    ACCOUNT\CONTRA                     
         XC    NEW.TRNKCULA,NEW.TRNKCULC                                        
         XC    NEW.TRNKCULC,NEW.TRNKCULA                                        
         XC    NEW.TRNKCULA,NEW.TRNKCULC                                        
         MVI   NEW.TRNKSBR,0                                                    
         MVC   TRNCACNM,ACC1CN     CONTRA    NAME                               
*                                                                               
         LA    R3,NEW.TRNRFST                                                   
         MVI   TRNSTAT,TRNSNOCM    CREDIT    AND  NON-COMMISSIONABLE            
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
POST36   GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
*                                                                               
         DROP  R2,R3,NEW                                                        
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - TMS POSTING - ADJUSTED TIME                *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
*                                                                               
         OC    TMSCOST,TMSCOST     IS   THIS A    TMS  POSTING ?                
         BZ    POSTX                                                            
*                                                                               
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
         MVC   NEW.TRNKCULA,TRNKCULC    CONTRA    JOB  =>   ACCOUNT             
         MVC   NEW.TRNKCCPY,COMPANY     COMPANY                                 
         MVC   NEW.TRNKULC,TMSCOST      CLIENT    COSTING   ACCOUNT             
         MVC   NEW.TRNKOFF,SPACES                                               
         MVC   NEW.TRNKDATE,TRNKDATE    ORIGINAL  DATE                          
         MVC   NEW.TRNKREF,WOREF        WRITE-OFF NUMBER                        
         MVC   TRNCACNM,TMSCOSTN        GET  CONTRA    NAME                     
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL2          ->   TRANSACTION    WORK EL   2              
         XC    WKTRNL2,WKTRNL2     CLEAR     ELEMENT                            
         MVI   TRNEL,TRNELQ        TRANSACTION    ELEMENT                       
         MVI   TRNLN,TRNLN1Q       ELEMENT   LENGTH                             
         MVC   TRNDATE,NEW.TRNKDATE     ORIGINAL  DATE                          
         MVC   TRNREF,NEW.TRNKREF       WRITE-OFF NUMBER                        
         MVI   TRNTYPE,TRNTWRTO         WRITE-OFF TYPE                          
         MVC   TMEDATE,TRNDATE     SAVE DATE FOR CADET                          
*                                  DEBIT     AND  NON-COMMISSIONABLE            
         MVI   TRNSTAT,TRNSDR+TRNSNOCM                                          
         MVC   TRNBTCH,BTCHREF     BATCH     REFERENCE                          
         ZAP   TRNAMNT,=P'0'       MOVE IN   ZERO AMOUNT                        
         MVC   TRNOFFC,TMSOFF      TMS  OFFICE                                  
         OC    TMSOFF,TMSOFF       ANY  TMS  OFFICE ?                           
         BNZ   *+10                YES, SKIP                                    
         MVC   TRNOFFC,OFFICE      NO,  USE  OFFICE                             
*                                                                               
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
*                                                                               
         DROP  R3,NEW                                                           
*                                                                               
         USING TRSELD,R3           MAP  TRANSACTION    STATUS    EL             
         LA    R3,ELEMENT          ->   WORK ELEMENT                            
         XC    ELEMENT,ELEMENT     CLEAR     ELEMENT                            
         MVI   TRSEL,TRSELQ        X'60' -   TRANSACTION    STATUS   EL         
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT2,TMSSTAT2   TMS  STATUS                                  
         MVC   TRSSTAT2,TMSSTAT3                                                
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    STATUS         
         DROP  R3                                                               
*                                                                               
         MVI   BLDTYPE,TIMIADJ     ADJUSTED  TIME ITEM TYPE                     
         BAS   RE,TIMEBLD          BUILD     TIME ELEMENTS                      
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  BUILD POSTING RECORDS - TMS POSTING - WRITE-OFF ITEM               *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
*                                                                               
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR                                        
         SR    R1,R1                         I/O                                
         MVCL  RE,R0                              AREA                          
*                                                                               
         MVC   NEW.TRNKCULA,TRNKCULC    CONTRA    JOB  =>   ACCOUNT             
         MVC   NEW.TRNKCCPY,COMPANY     COMPANY                                 
         MVC   NEW.TRNKULC,TMSCOST      CLIENT    COSTING   ACCOUNT             
         MVC   NEW.TRNKOFF,SPACES                                               
         MVC   NEW.TRNKDATE,TRNKDATE    ORIGINAL  DATE                          
         MVC   NEW.TRNKREF,WOREF        WRITE-OFF NUMBER                        
         MVC   TRNCACNM,TMSCOSTN        GET  CONTRA    NAME                     
*                                                                               
         LA    R3,WKTRNL2          ->   TRANSACTION    WORK EL   2              
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    EL             
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
         MVI   TRNSTAT,TRNSNOCM    CREDIT    AND  NON-COMMISSIONABLE            
         DROP  R3,NEW                                                           
*                                                                               
         USING TRSELD,R3           MAP  TRANSACTION    STATUS    EL             
         LA    R3,ELEMENT          ->   WORK ELEMENT                            
         XC    ELEMENT,ELEMENT     CLEAR     ELEMENT                            
         MVI   TRSEL,TRSELQ        X'60' -   TRANSACTION    STATUS   EL         
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSSTAT2,TMSSTAT2   TMS  STATUS                                  
         MVC   TRSSTAT2,TMSSTAT3                                                
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    TRANSACTION    STATUS         
         DROP  R3                                                               
*                                                                               
         MVI   BLDTYPE,TIMIWO      WRITE-OFF ITEM TYPE                          
         BAS   RE,TIMEBLD          BUILD     TIME ELEMENTS                      
*                                                                               
         BRAS  RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         GOTO1 ABADDRFT            ADD  DRAFT     TRANSACTION                   
*                                                                               
POST90   GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
         DROP  R2                                                               
                                                                                
POSTX    MVC   KEY,0(R2)                                                        
         GOTO1 AREAD,AIOAREA3      RESET     FOR  SEQ  READ IN   RDJOB          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*              ADD TO SUB-ACCOUNT TABLE                               *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
ADD2SUB  NTR1                                                                   
         L     R2,AIOAREA3         CURRENT TRANSACTION                          
                                                                                
         LA    RE,SUBTAB           CLEAR SUBTAB                                 
         LA    RF,(SUBTABL*SUBTABM)                                             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVI   SUBTABN,0           CLEAR SUBTAB COUNTER                         
                                                                                
         LA    R1,TRNKCULA         GET JOB FIRST                                
         MVI   BYTE,0                                                           
         BAS   RE,ADD2TAB                                                       
                                                                                
         LA    R1,WORK             WRITEOFF ACCOUNT                             
         MVC   0(1,R1),COMPANY                                                  
         MVC   1(L'WRTACC,R1),WRTACC                                            
         BAS   RE,ADD2TAB                                                       
                                                                                
         CLC   SKACCNT+1(2),SI     DO WE HAVE AN SK ACCOUNT?                    
         BE    ADD2SUB2            NO                                           
         LA    R1,SKACCNT          YES, SAVE IT                                 
         MVI   BYTE,X'80'                                                       
         BAS   RE,ADD2TAB                                                       
                                                                                
         LA    R1,SIACCNT          DO SI ACCOUNT ALSO                           
         MVI   BYTE,X'80'                                                       
         BAS   RE,ADD2TAB                                                       
                                                                                
ADD2SUB2 CLI   COSTING,NO          MAKING COST POSTINGS?                        
         BE    ADD2SUB6            NO, CHECK TMS                                
         CLC   SKACCNT+1(2),SI     DO WE HAVE AN SK ACCOUNT?                    
         BE    ADD2SUB4            NO                                           
         CLC   ACC12,ACC12I                                                     
         BE    ADD2SUB6                                                         
                                                                                
         MVI   BYTE,X'80'          YES, SAVE COSTING                            
         LA    R1,ACC1C                                                         
         BAS   RE,ADD2TAB                                                       
                                                                                
         LA    R1,ACC12I           AND INCOME ROLLOVER                          
         BAS   RE,ADD2TAB                                                       
                                                                                
ADD2SUB4 MVI   BYTE,X'80'          YES, SAVE COSTING                            
         LA    R1,ACC1C                                                         
         BAS   RE,ADD2TAB                                                       
         LA    R1,ACC12            WRITEOFF INCOME                              
         BAS   RE,ADD2TAB                                                       
                                                                                
ADD2SUB6 OC    TMSCOST,TMSCOST     IS THIS TMS?                                 
         BZ    ADD2SUBX            NO, DONE                                     
                                                                                
         MVI   BYTE,X'80'          YES SAVE CONTRA AS DEBIT                     
         LA    R1,TRNKCULC                                                      
         BAS   RE,ADD2TAB                                                       
                                                                                
         MVI   BYTE,0              AND AS A CREDIT                              
         BAS   RE,ADD2TAB                                                       
                                                                                
ADD2SUBX XIT1                                                                   
         DROP  R2                                                               
         EJECT                                                                  
         USING SUBTABD,RF                                                       
ADD2TAB  LA    RF,SUBTAB             RF=A(TABLE OF PRIMARY ACCOUNTS)            
         LHI   R0,SUBTABM            R0=MAX NUMBER OF ENTRIES IN TABLE          
                                                                                
ADD2TAB2 OC    0(SUBTABL,RF),0(RF)   VACANT SPOT?                               
         BZ    ADD2TAB6              YES                                        
         CLC   SUBSTA,BYTE                                                      
         BNE   ADD2TAB4                                                         
         CLC   SUBACC,1(R1)                                                     
         BER   RE                    YES, LEAVE IT                              
                                                                                
ADD2TAB4 AHI   RF,SUBTABL            BUMP TO NEXT TABLE ENTRY                   
         BCT   R0,ADD2TAB2                                                      
         BR    RE                                                               
                                                                                
ADD2TAB6 MVC   SUBSTA,BYTE                                                      
         MVC   SUBACC,1(R1)                                                     
         IC    RF,SUBTABN          INCREMENT NUMBER OF TABLE ENTRIES            
         AHI   RF,1                                                             
         STC   RF,SUBTABN                                                       
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*  TEST IF TMS POSTINGS NEEDED                                        *         
*                                                                     *         
*  CLEAR TMSCOST AND TMSOFF IF:                                       *         
*    1) NOT ON TMS                                                    *         
*    2) NO TMS COSTING ACCOUNT                                        *         
*    3) NO TMS OFFICE CODE                                            *         
*    4) 1R/1C RECORD NOT TMS                                          *         
*                                                                     *         
*    INPUT:                                                           *         
*      R2       = AIOAREA3                                            *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      TMSCOST  = CLIENT   COSTING ACCOUNT                            *         
*      TMSOFF   = CLIENT   OFFICE                                     *         
*      TMSDATE  = ACTIVITY DATE                                       *         
*                                                                     *         
*    USES:                                                            *         
*      IOAREA1                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
TEST     USING TRNRECD,R3          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
TESTMS   NTR1                                                                   
         TM    COMPSTA7,CPYSTMSY   ARE  THEY ON   TMS ?                         
         BNO   TESTMSN             NO,  CLEAR     1C   AND  OFFICE              
*                                                                               
*                                  YES, IS   THIS CONTRA    1R ?                
         CLC   TRNKCUNT(LUL),=C'1R'                                             
         BNE   TESTMSN             NO,  NOT  TMS, SO   CLEAR                    
*                                                                               
         OC    COMPTMSD,COMPTMSD   ANY  START     DATE ?                        
         BZ    TESTMS2             NO                                           
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TRNKDATE),(2,WORK)                               
*                                  YES, COMPARE   TO   TRANSACTION DATE         
         CLC   WORK(L'COMPTMSD),COMPTMSD                                        
         BL    TESTMSN             TOO  LOW, SKIP IT                            
*                                                                               
TESTMS2  OC    TMSCOST,TMSCOST     ANY  1C   ACCOUNT ?                          
         BZ    TESTMSN             NO,  CLEAR     THE  OFFICE    THEN           
*                                                                               
         MVC   WORK(L'TMSOFF),TMSOFF    HOLD THE  OFFICE                        
         TM    COMPSTA4,X'01'           ANY  NEW  OFFICES ?                     
         BO    TESTMS4                  YES, CHECK     FOR  OFFICE              
*                                                                               
         CLC   TMSDATE,COMPTMSD         COMPARE   ADD  DATE TO TMS DATE         
         BL    TESTMSN                  TOO  LOW, SKIP IT                       
         MVC   WORK(L'TMSOFF),SPACES    OK,  USE  BLANKS                        
         B     TESTMS6                  BRANCH    AROUND                        
*                                                                               
TESTMS4  OC    TMSOFF,TMSOFF       DO   WE   HAVE THE  TMS  OFFICE ?            
         BZ    TESTMSX             NO,  MUST HAVE FOR  2-BYTES                  
*                                                                               
TESTMS6  XC    KEY,KEY                                                          
         LA    R3,KEY              READ RECORD    TO   SEE  IF   TMS            
         MVC   TEST.TRNKCULA,TRNKCULC   ACCOUNT=  CONTRA                        
         MVC   TEST.TRNKWORK,WORK       WORK      CODE                          
         MVC   TEST.TRNKCCPY,COMPANY    COMPANY                                 
         MVC   TEST.TRNKULC,TMSCOST     CLIENT    COSTING   ACCOUNT             
         MVC   TEST.TRNKDATE,TRNKDATE   ORIGINAL  DATE                          
         MVC   TEST.TRNKREF,=C'*TIME*'                                          
         GOTO1 AREAD,AIOAREA1                                                   
         BE    TESTMSX                                                          
*                                                                               
TESTMSN  XC    TMSCOST,TMSCOST     CLEAR     COSTING   ACCOUNT                  
         XC    TMSOFF,TMSOFF       CLEAR     COSTING   OFFICE                   
         XC    TMSDATE,TMSDATE     CLEAR     COSTING   DATE                     
*                                                                               
TESTMSX  B     EXIT                                                             
         DROP  R2,TEST                                                          
         EJECT ,                                                                
***********************************************************************         
*  BUILD MEDIA TRANSFER ELEMENT FOR INCOME POSTINGS                   *         
*                                                                     *         
*  BLDMT BUILDS THE MEDIA TRANSFER ELEMENT FOR INCOME POSTINGS.       *         
*                                                                     *         
*    INPUT:                                                           *         
*      DUB      = TRANSACTION AMOUNT                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING MDTELD,R3           MAP  MEDIA     TRANSFER  ELEMENT             
         SPACE 1                                                                
BLDMT    ST    RE,SAVRE            SAVE REGISTER                                
         LA    R3,ELEMENT          ->        WORK AREA                          
         XC    ELEMENT,ELEMENT     CLEAR     WORK AREA                          
         MVI   MDTEL,MDTELQ        X'1A' -   MEDIA     TRANSFER  EL             
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSPROD     SYSTEM=   PRODUCTION                         
         MVC   MDTMED,LJOB         MEDIA     CODE                               
*                                  CLIENT/PRODUCT/JOB                           
         MVC   MDTCLI(LACT),JOBKEY+3                                            
         MVC   MDTMOS,PBILMOS      MONTH     OF   SERVICE  (PACKED)             
         MVC   MDTDSCP,SJCPJNM     JOB  NAME                                    
         CVB   R0,DUB                                                           
         STCM  R0,15,MDTCOM        INCOME   (COMMISSION)                        
         STCM  R0,15,MDTINTL       INCOME   (INTERNAL)                          
         GOTO1 ABADDLST,(R3)       ADD  BATCH:    MEDIA     TRANSFER EL         
*                                                                               
         L     RE,SAVRE            RESTORE   REGISTER                           
         BR    RE                                                               
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
*  BUILD 8B TIMEL TO BE HOOKED ONTO 1R POSTINGS                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION RECORD                      
         USING TIMELD,R3           MAP  TIME DETAIL ELEMENT                     
         SPACE 1                                                                
TIMEBLD  NTR1                                                                   
         LA    R3,ELEMENT          BUILD 8B ELEMENT                             
         XC    ELEMENT,ELEMENT                                                  
         MVI   TIMEL,TIMELQ                                                     
         MVI   TIMETYP,TIMEINP     INPUT DETAIL TYPE                            
         MVC   TIMACC,TRNKULA      SJ ACCOUNT                                   
         MVC   TIMOFF,TMSOFF       TMS  OFFICE                                  
         OC    TMSOFF,TMSOFF       ANY TMS OFFICE ?                             
         BNZ   *+10                YES, SKIP                                    
         MVC   TIMOFF,OFFICE       NO, USE CLIENT OFFICE                        
         MVC   TIMTSK,TRNKWORK     TASK CODE                                    
         MVC   TIMTTYP,TMSTYP      TYPE OF TIME (B/N/R)                         
         MVC   TIMIND,BLDTYPE      TIME INDICATOR (ADJUSTED/WRITE-OFF)          
         MVC   TIMMOA,TMOS         MONTH OF ACTIVITY                            
         OI    TIMSTAT,TIMNOTAX    TAX SUPPRESSED FOR THIS ITEM                 
         MVC   TIMADAT,TODAYP      ACTIVITY  DATE                               
         ICM   R1,15,CTHOUR                                                     
         CVD   R1,DUB                                                           
         TM    TIMIND,TIMIADJ      TIME ADJUST TYPE?                            
         BO    *+10                YES, SKIP                                    
         MP    DUB,=P'-1'          NO, MAKE HOURS NEGATIVE                      
         ZAP   TIMHRS,DUB                                                       
         ZAP   TMEHRS,DUB          SAVE FOR CADET                               
         LA    R1,TIMILN1Q         BASE RECORD LENGTH                           
*                                                                               
         CLI   TMSTYP,TIMTCB       CLIENT BILLABLE?                             
         BE    TIME02              YES, HAS RATE                                
         CLI   TMSTYP,TIMTCR       CLIENT REALIZATION?                          
         BNE   TIME04              NO, SKIP RATE                                
*                                                                               
TIME02   ZAP   TIMRATE,TMSRATE     RATE                                         
         MVI   TIMRBSTA,TIMRORAT   RATE INDICATOR                               
         MVC   TIMREFF,TMSEFF      RATE EFFECTIVE DATE                          
         MVC   TIMINC,SIACCNT+1    INCOME ACCOUNT                               
         CLC   SIACCNT,SPACES                                                   
         BH    *+10                                                             
         MVC   TIMINC,SKACCNT+1                                                 
         OC    TIMINC,SPACES                                                    
         ZAP   DUB,CTAMNT          AMOUNT STARTS POSITIVE                       
         TM    TIMIND,TIMIADJ      TIME ADJUST TYPE ?                           
         BZ    *+10                NO, SKIP                                     
         MP    DUB,=P'-1'          YES, MAKE NEGATIVE                           
         ZAP   TIMAMNT,DUB                                                      
         LA    R1,TIMILN2Q         RECORD LENGTH                                
*                                                                               
TIME04   STC   R1,TIMLN            SAVE RECORD LENGTH                           
         GOTO1 ABADDLST,(R3)       ADD 8B ELEMENT AT END OF RECORD              
         CLI   NARRLEN,0           ANY NARRATIVE ?                              
         BE    TIME06              NO, SKIP                                     
         XC    ELEMENT,ELEMENT     CLEAR WORK AREA                              
         MVI   TIMEL,TIMELQ        X'8B' TIME DETAIL ELEMENT                    
         MVI   TIMETYP,TIMENAR     NARRATIVE TYPE                               
         ZIC   R1,NARRLEN          LENGTH OF NARATIVE                           
         BCTR  R1,0                                                             
         EXMVC R1,TIMNARR,NARRATVE INSERT NARRATIVE                             
         LA    R1,TIMHLNQ+1(,R1)   LENGTH OF NARRATIVE-1                        
         STC   R1,TIMLN            SAVE LENGTH                                  
         GOTO1 ABADDLST,(R3)       ADD 8B ELEMENT AT END OF RECORD              
*                                                                               
TIME06   CLI   TMSSTAT3,TRSSMCS    IS THIS BRANDO TIME?                         
         BNE   TIMEX               NO, DONE                                     
         XC    CADETBLK,CADETBLK   BUILD CADET BLOCK                            
         XC    ELEMENT,ELEMENT     CLEAR WORK AREA                              
         USING CADETD,R6                                                        
         LA    R6,CADETBLK                                                      
         MVC   CCOMFACS,ACOMFACS                                                
         MVC   CADASEC,ASECBLK                                                  
         MVC   CADAIOA,ACADETIO    A(IO AREA TO USE)                            
         ST    R3,CADAELM          A(ELEMENT)                                   
         MVC   CAD1RLVS,TMELV1R    1R CUMULATIVE LEVEL LENGTHS                  
         MVC   CADSJLVS,TMELVSJ    SJ CUMULATIVE LEVEL LENGTHS                  
         MVC   CAD1RACT,TRNKCACT   1R ACCOUNT CODE                              
         MVC   CADTMULA,TRNKULA    SJ U/L/ACCOUNT CODE                          
         MVC   CADCPYCD,COMPANY                                                 
         MVC   CADPIDN,TMEPID      PID                                          
         ZAP   CADTSHRS,TMEHRS     HOURS                                        
         MVC   CADTSPED,TMEDATE    PERIOD END DATE                              
         MVC   CADTODAY,TODAYP                                                  
         MVC   CADCUID,TWAUSRID    CONNECTED USER-ID                            
         MVI   CADXCALL,CADXFXRR                                                
         GOTO1 ACADET,CADETD                                                    
*                                                                               
         MVI   TIMEL,TIMELQ        BUILD ETIME STATUS ELEMENT                   
         MVI   TIMLN,TIMETLNQ                                                   
         MVI   TIMETYP,TIMETIME                                                 
         MVC   TIMEIDNO,CADHIROW                                                
         OI    TIMEPST1,TIMESAPR   MARK APPROVED                                
         MVC   TIMEPIDC,TMEPID                                                  
         MVC   TIMETDT1,TMEDATE                                                 
         MVC   TIMEHRS1,TMEHRS                                                  
*                                                                               
         GOTO1 VDATCON,DMCB,(1,TIMETDT1),(0,WORK)                               
         LA    R0,6                                                             
         LA    R6,TIMETDT2                                                      
*                                                                               
TIME08   GOTO1 VADDAY,DMCB,(C'D',WORK),WORK+6,F'-6'                             
         GOTO1 VDATCON,DMCB,(0,WORK+6),(1,0(R6))                                
         ZAP   3(L'TIMEHRS1,R6),=P'0'                                           
         AHI   R6,L'TIMEHRS1+L'TIMETDT1                                         
         BCT   R0,TIME08                                                        
*                                                                               
         GOTO1 ABADDLST,(R3)       ADD 8B ELEMENT AT END OF RECORD              
*                                                                               
TIMEX    B     EXIT                RETURN    TO   CALLER                        
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  MARK/UNMARK TRANSACTIONS                                           *         
*                                                                     *         
*  MKTRS HANDLES THE IMMEDIATE UPDATE OF 77 ELEMENTS FOR THE JOB.     *         
*  IT WRITES-OFF CHARGES BY INSERTING THE BATCH REFERENCE NUMBER IN   *         
*  THE 77 ELEMENT AND CHANGING THE TYPE.                              *         
*  IT RECOVERS THE CHARGES BY SEARCHING FOR THE SPECIFIC BATCH        *         
*  REFERENCE IN THE 77 ELEMENT, MARKS IT RECOVERED, AND CREATES A     *         
*  77 ELEMENT FOR THE RECOVERY.                                       *         
*                                                                     *         
*    INPUT:                                                           *         
*      ACTION   = WRT OR REC                                          *         
*      WONUMCHR = WRITE-OFF NUMBER                                    *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
MKTRS    NTR1                                                                   
         MVC   MATCH,SPACES        LOOK FOR 4B ELEMENTS WITH BLANKS             
         MVC   REPLACE(2),WO       REPLACE WITH CODE FOR WRITE-OFF              
         MVC   REPLACE+2(4),WONUMCHR                                            
         ZAP   MKAMNT,=P'0'                                                     
         MVI   DATASW,NO           INDICATE NO DATA YET                         
         CLI   ACTION,REC                                                       
         BNE   MKTR05                                                           
         MVC   MATCH(6),REPLACE                                                 
         XC    REPLACE(6),REPLACE  AMOUNTS COME BACK UNALLOCATED                
*                                                                               
*                                  READ 1ST  RCD  FOR  JOB                      
MKTR05   GOTO1 RD1STREC,DMCB,('READLOCK',AIOAREA3)                              
         B     MKTR15                                                           
*                                                                               
*                                  READ NEXT RCD  FOR  JOB                      
MKTR10   GOTO1 RDNXTREC,DMCB,('READLOCK',AIOAREA3)                              
*                                                                               
MKTR15   BNE   MKTR75              NOT  FOUND,    END  OF   JOB  RCDS           
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         L     R2,AIOAREA3         ->   RECORD                                  
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
         LA    R4,ACCORFST(,R2)    ->   TRANSACTION    ELEMENT                  
         CLI   TRNTYPE,TRNTWRTO    WRITE-OFF TYPE RECORD ?                      
         BE    MKTR55              YES, PROCESS   WRITE-OFF                     
*                                                                               
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY EL         
         L     R4,APROLST          ->   PRORATA   LIST                          
         CLI   0(R4),PTAELQ        X'77' -   PROD TRANSACTION ACTIVITY?         
         B     MKTR25                                                           
*                                                                               
MKTR20   MVI   ELCODE,PTAELQ       X'77' -   PROD TRANSACTION  ACTIVITY         
         BAS   RE,NEXTEL           FIND 77   ELEMENTS                           
*                                                                               
MKTR25   BNE   MKTR10              NOT  77   EL,  READ NEXT RECORD              
*                                                                               
         CLI   PTATYPE,PTATRAL     ALLOCATED ?                                  
         BNE   MKTR30              NO,  SEE  IF   WE   ARE  RECOVERING          
         TM    PTASTAT1,PTASPEND   YES, IS   IT   PENDING ?                     
         BZ    MKTR20              NO,  SKIP IT                                 
         CLI   ACTION,REC          ARE  WE   RECOVERING ?                       
         BE    MKTR20              YES, SKIP IT                                 
*                                                                               
         MVI   PTATYPE,PTATWOF     CHANGE    THE  TYPE                          
         NI    PTASTAT1,X'FF'-PTASPEND                                          
         XC    PTAWVAL(PTAWLN2Q-(PTAWVAL-PTAEL)),PTAWVAL                        
         MVC   PTADATE,TODAYC      TODAY     COMPRESSED                         
         MVC   PTAWDAT,TODAYP      TODAY     PACKED                             
         MVC   PTAWREF,REPLACE                                                  
         MVC   PTAWWOT,BILTYP                                                   
         MVC   PTAMOA,PMOS                                                      
         AP    MKAMNT,PTANET                                                    
         B     MKTR45                                                           
*                                                                               
MKTR30   CLI   ACTION,REC          RECOVERY ?                                   
         BNE   MKTR20              NO,  GET  NEXT ELEMENT                       
         CLI   PTATYPE,PTATWOF     YES, WRITE-OFF PTA  EL   TYPE ?              
         BNE   MKTR20              NO,  GET  NEXT ELEMENT                       
         TM    PTASTAT1,PTASPEND   YES, PENDING ?                               
         BO    MKTR20              YES, GET  NEXT ELEMENT                       
*                                                                               
         CLC   PTAWREF,MATCH       DOES THE  WRITE-OFF NUM  MATCH ?             
         BNE   MKTR20              NO,  GET  NEXT ELEMENT                       
         CLC   PTAWDAT,WRDATEP     YES, DOES DATE MATCH ?                       
         BNE   MKTR20              NO,  GET  NEXT ELEMENT                       
*                                                                               
         AP    MKAMNT,PTANET       ADD  AMOUNT                                  
*                                                                               
         CLC   PTAWDAT,TODAYP      IS   IT   FROM TODAY ?                       
         BNE   MKTR35              NO,  ADD  RECOVERY  ELEMENT                  
         XC    PTATYPE,PTATYPE     YES, CLEAR     IT   OUT                      
         XC    PTADATE,PTADATE                                                  
         XC    PTAMOA,PTAMOA                                                    
         XC    PTAHOURS,PTAHOURS                                                
         ZAP   PTANET,=P'0'                                                     
         MVI   PTASTAT1,0                                                       
         XC    PTAWVAL(PTAWLN2Q-(PTAWVAL-PTAEL)),PTAWVAL                        
         B     MKTR45                                                           
*                                                                               
MKTR35   XC    ELEMENT,ELEMENT     ADD  RECOVERY  ELEMENT                       
         SR    R1,R1                                                            
         IC    R1,PTALN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,ELEMENT,PTAEL    SAVE CURRENT   PTA  ELEMENT                  
*                                                                               
MKTR40   BAS   RE,NEXTEL           LOOK FOR  END  OF   ELEMENTS                 
         BE    MKTR40                                                           
         EXMVC R1,0(R4),ELEMENT    ADD  CURRENT   PTA  ELEMENT                  
*                                                                               
         MVC   PTADATE,TODAYC      UPDATE    NEW  ELEMENT                       
         MVI   PTATYPE,PTATWOFR                                                 
         ZAP   DUB,PTANET          REVERSE   AMOUNTS                            
         MP    DUB,=P'-1'          .    NET                                     
         ZAP   PTANET,DUB                                                       
         MVC   PTAMOA,PMOS                                                      
         ZAP   DUB,PTACDSC         REVERSE   AMOUNTS                            
         MP    DUB,=P'-1'          .    CASH DISCOUNT                           
         ZAP   PTACDSC,DUB                                                      
         MVC   PTAWREF(2),WR                                                    
         MVC   PTAWDAT,WRDATEP                                                  
         SR    RF,RF                                                            
         LH    RF,PTAHOURS                                                      
         LCR   RF,RF                                                            
         STH   RF,PTAHOURS                                                      
         LA    R4,1(R4,R1)                                                      
         MVI   0(R4),0                                                          
*                                                                               
MKTR45   GOTO1 VPRORATA,DMCB,(X'A0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         LA    R4,ACCORFST(,R2)                                                 
         MVI   ELCODE,TRSELQ                                                    
         BAS   RE,NEXTEL                                                        
         BNE   MKTR50                                                           
*                                                                               
         USING TRSELD,R4           MAP  TRANSACTION    STATUS    EL             
         OI    TRSSTAT3,TRSSNBIL   MAKE NON-BILLABLE   ON   A21                 
         CLI   ACTION,REC                                                       
         BNE   MKTR50                                                           
         NI    TRSSTAT3,X'FF'-TRSSNBIL                                          
*                                                                               
MKTR50   GOTO1 ATRXEL,AIOAREA3     UPDATE    X'75'     ELEMENT                  
         GOTO1 AWRITE,AIOAREA3     WRITE     BACK THE  RECORD                   
         MVI   DATASW,YES          INDICATE  WE   HAVE DATA                     
         B     MKTR10                                                           
*                                                                               
*        FOUND WRITE-OFF TYPE (57) ELEMENT:                                     
*              MAKE SURE IT IS OUR WRITE-OFF TRANSACTION AND                    
*              MAKE SURE NON-BILL BIT IS SET IN TRANSACTION STATUS EL           
*                                                                               
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY EL         
MKTR55   L     R4,APROLST          ->   PRORATA   LIST                          
         CLI   0(R4),PTAELQ        X'77' -   PROD TRANSACTION ACTIVITY?         
         B     MKTR65                                                           
*                                                                               
MKTR60   MVI   ELCODE,PTAELQ       X'77' -   PROD TRANSACTION  ACTIVITY         
         BAS   RE,NEXTEL           GET  NEXT ELEMENT                            
*                                                                               
MKTR65   BNE   MKTR10              NO,  READ NEXT RECORD                        
         CLC   PTAWREF,MATCH       IS   THIS THE  ONE  WE   WANT ?              
         BNE   MKTR60              NO,  GET  NEXT ELEMENT                       
         CLC   WRDATEP,PTAWDAT     YES, DATE MATCHES ?                          
         BNE   MKTR60              NO,  GET  NEXT ELEMENT                       
*                                                                               
         USING TRSELD,R4           MAP  TRANSACTION    STATUS    EL             
         LA    R4,ACCORFST(,R2)                                                 
         MVI   ELCODE,TRSELQ       LOOK FOR  STATUS    ELEMENT                  
         BAS   RE,NEXTEL                                                        
         BNE   MKTR10                                                           
*                                                                               
         OI    TRSSTAT3,TRSSNBIL   MAKE SURE IT   IS   NOT  BILLABLE            
         B     MKTR50                                                           
*                                                                               
MKTR75   CLI   DATASW,YES          DO   WE   HAVE DATA ?                        
         BNE   MKTREX              NO,  EXIT                                    
         MVC   KEY,SPACES          YES, READ JOB RECORD AGAIN                   
         MVC   KEY(LCULACT),JOBKEY                                              
         GOTO1 AREADL,AIOAREA3                                                  
         BE    *+6                                                              
         DC    H'0'                CAN  NOT  READ JOB                           
*                                                                               
         L     R4,AIOAREA3                                                      
         MVI   ELCODE,SCIELQ       LOOK FOR  SCIEL                              
         XC    ELFLAG,ELFLAG       CLEAR     ELEMENT    INDICATOR               
         BAS   RE,GETEL                                                         
         B     MKTR85                                                           
*                                                                               
MKTR80   BAS   RE,NEXTEL                                                        
*                                                                               
MKTR85   BNE   MKTR90                                                           
*                                                                               
         USING SCIELD,R4           MAP  SUBSIDIARY     CASH INFO EL             
         CLI   SCITYPE,SCITCBAP    ALLOCATION     ELEMENT ?                     
         BNE   MKTR80              NO                                           
         CLI   ACTION,WRT          YES, ARE  WE   WRITING OFF ?                 
         BNE   MKTR80              NO,  NOTHING   TO   UPDATE  THEN             
         SP    SCIAMNT,MKAMNT      SUBTRACT  NET                                
         OI    ELFLAG,SCITCBAP                                                  
         B     MKTR80                                                           
*                                                                               
MKTR90   GOTO1 AWRITE,AIOAREA3     UPDATE    THE  RECORD                        
*                                                                               
MKTREX   B     EXIT                RETURN    TO   CALLER                        
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  READ, LOCK, AND VALIDATE  RECORDS                                  *         
*                                                                     *         
*  RDLOCK READS AND LOCKS ALL THE RECORDS IN THE JOB.                 *         
*  THEN RDLOCK CALCULATES THE TOTAL AMOUNT FOR THE JOB.               *         
*  IN ADDITION, RDLOCK TRIES TO VERIFY THAT NO NEW ERRORS ARE FOUND   *         
*  DURING THE ACTUAL WRITE-OFF OR RECOVERY PROCESS.                   *         
*                                                                     *         
*    INPUT:                                                           *         
*      ACTION   = WRT OR REC                                          *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      CTATOT   = TOTAL ACCUMULATOR                                   *         
*      EFLAG    : 0 = NO ERRORS FOUND; 1 = ERRORS FOUND               *         
*                                                                     *         
*    CALLS:                                                           *         
*      GETCACTS, GETCTAMT                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
RDLOCK   NTR1                                                                   
         L     R2,AIOAREA3         R2 = CURRENT   TRANSACTION                   
         MVI   ANYPOSTS,NO         NOTHING   TO   BE   POSTED                   
         ZAP   CTATOT,=P'0'        CLEAR     TOTAL     ACCUMULATOR              
*                                                                               
*                                  READ 1ST  RCD  FOR  JOB                      
         GOTO1 RD1STREC,DMCB,('READLOCK',AIOAREA3)                              
         B     RDLOCK15                                                         
*                                                                               
RDLOCK10 DS    0H                  READ NEXT RCD  FOR  JOB                      
         GOTO1 RDNXTREC,DMCB,('READLOCK',AIOAREA3)                              
*                                                                               
RDLOCK15 DS    0H                                                               
         BNE   RDLOCK50            NOT  FOUND,    END  OF   JOB  RCDS           
*                                                                               
*                                                                               
         USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
         LA    R4,ACCORFST(,R2)    R4 = 1ST  EL,  I.E. TRANSACTION   EL         
*                                                                               
         CLI   ACTION,REC          RECOVER ?                                    
         BNE   RDLOCK20            NO,  SKIP                                    
         CLI   TRNTYPE,TRNTWRTO    WRITE-OFF ?                                  
         BNE   RDLOCK10            NO,  READ NEXT RECORD                        
*                                                                               
RDLOCK20 DS    0H                  **   UPDATE    TOTAL     AMOUNT   **         
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         ZAP   SAVEAMNT,TRNAMNT    SAVE TRANSACTION    TOTAL                    
         BAS   RE,GETCTAMT         GET  CTAMNT                                  
         CLI   EFLAG,X'FF'         SKIP RECORD ?                                
         BE    RDLOCK10            YES, READ NEXT                               
         CLI   EFLAG,0             ANY  ERRORS ?                                
         BNE   RDLOCKEX            YES, EXIT                                    
         AP    CTATOT,CTAMNT       ADD  THE  AMOUNT    ALLOCATED                
         CP    CTAMNT,=P'0'        ANY  AMOUNT    ALLOCATED ?                   
         BE    *+8                 NO,  SKIP                                    
         MVI   ANYPOSTS,YES        YES, AMOUNTS   TO   BE   POSTED              
         CLC   CTHOUR,=H'0'        ANY  HOURS     ALLOCATED ?                   
         BE    *+8                 NO,  SKIP                                    
         MVI   ANYPOSTS,YES        YES, AMOUNTS   TO   BE   POSTED              
*                                                                               
*                                  **   CHECK     FOR  ERRORS    **             
         BAS   RE,GETCACTS         CHECK     CONTRA    ACCOUNTS                 
         CLI   EFLAG,0             ANY  ERRORS ?                                
         BNE   RDLOCKEX            YES, EXIT                                    
         MVC   KEY,0(R2)                                                        
         GOTO1 AREAD,AIOAREA3      RESET     FOR  SEQ  READ                     
         B     RDLOCK10            READ NEXT RECORD                             
*                                                                               
RDLOCK50 CLI   ANYPOSTS,YES        ANYTHING  TO   BE   POSTED ?                 
         BE    RDLOCKOK            YES, EXIT                                    
         MVI   EFLAG,1             NO,  ERROR                                   
         B     NOPOSTS             GENERATE  ERROR     MESSAGE                  
*                                                                               
RDLOCKOK MVI   EFLAG,0             NORMAL    EXIT                               
*                                                                               
RDLOCKEX B     EXIT                RETURN    TO   CALLER                        
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  GET CTAMNT BASED ON PRORATA LIST                                   *         
*                                                                     *         
*  GETCTAMT LOOPS THROUGH PRORATA LIST IN ORDER TO UPDATE             *         
*  CTAMNT AND CTHOUR                                                  *         
*                                                                     *         
*    INPUT:                                                           *         
*      AIOAREA3 = A(CURRENT RECORD)                                   *         
*      APROLST  = A(PRORATA LIST)                                     *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      CTAMNT   = AMOUNT TO BE POSTED                                 *         
*      CTHOUR   = HOURS  TO BE WRITTEN OFF                            *         
*                                                                     *         
*      EFLAG    :                                                     *         
*                 X'00' = NO ERROR FOUND                              *         
*                 X'01' = ERROR FOUND                                 *         
*                 X'FF' = SKIP RECORD                                 *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY EL         
         SPACE 1                                                                
GETCTAMT NTR1                                                                   
         MVI   EFLAG,0             CLEAR     ERROR     FLAG                     
         ZAP   CTAMNT,=P'0'        CLEAR     AMOUNT    TO   BE   POSTED         
         XC    CTHOUR,CTHOUR       CLEAR     HOURS     TO   BE   POSTED         
         L     R2,AIOAREA3         ->   CURRENT   RECORD                        
         L     R4,APROLST          ->   PRORATA   LIST                          
         CLI   0(R4),PTAELQ        X'77' -   PROD TRANSACTION ACTIVITY?         
         B     GETCTA20                                                         
*                                                                               
GETCTA10 DS    0H                                                               
         MVI   ELCODE,PTAELQ       X'77' -   PROD TRANSACTION  ACTIVITY         
         BAS   RE,NEXTEL                                                        
*                                                                               
GETCTA20 DS    0H                                                               
         BNE   GETCTA70            BRANCH    IF   END  OF   PTA  ELS            
         CLI   ACTION,REC          RECOVER   RECORDS ?                          
         BNE   GETCTA50            NO                                           
         CLI   PTATYPE,PTATWOF     IS   THIS A    WRITE-OFF ?                   
         BNE   GETCTA45            NO,  CHECK     FOR  RECOVERY  RECORD         
         TM    PTASTAT1,PTASPEND   IS   IT   PENDING ?                          
         BO    GETCTA10            YES, NOT  THE  ONE  WE   WANT                
         CLC   PTAWREF(2),WO       IS   THIS AN   OLD  WRITE-OFF ?              
         BNE   GETCTA30            NO,  COMPARE   ALL  DIGITS                   
         CLC   PTAWREF+2(4),WONUMCHR    SAME WRITE-OFF NUMBER ?                 
         B     GETCTA40                                                         
*                                                                               
GETCTA30 DS    0H                                                               
         CLC   PTAWREF(4),WONUMCHR                                              
         BNE   GETCTA10                                                         
         CLC   PTAWREF+4(2),SPACES                                              
*                                                                               
GETCTA40 DS    0H                                                               
         BNE   GETCTA10                                                         
         CLC   PTAWDAT,WRDATEP     SAME DATE ?                                  
         BNE   GETCTA10                                                         
         B     GETCTA60                                                         
*                                                                               
GETCTA45 DS    0H                  CHECK     FOR  DUPLICATE RECOVERY            
         CLI   PTATYPE,PTATWOFR    WRITE-OFF RECOVERY ?                         
         BNE   GETCTA10            NO,  GET  NEXT PROD TRAN ACTIVITY EL         
         CLC   PTAWREF(2),WR       IS   THIS A    WRITE-OFF RECOVERY ?          
         BNE   GETCTA10            NO,  GET  NEXT PROD TRAN ACTIVITY EL         
         CLC   PTAWREF+2(4),WONUMCHR    SAME WRITE-OFF NUMBER ?                 
         BNE   GETCTA10            NO,  GET  NEXT PROD TRAN ACTIVITY EL         
         MVI   EFLAG,1             SAY  ERROR     FOUND                         
         MVC   FVMSGNO,=AL2(2227)  WRITE-OFF ALREADY   RECOVERED                
         B     GETCTA93            SET  CURSOR    AND  EXIT                     
*                                                                               
GETCTA50 DS    0H                                                               
         CLI   PTATYPE,PTATRAL     ONLY WANT ALLOCATED AMOUNTS                  
         BNE   GETCTA10                                                         
         TM    PTASTAT1,PTASPEND   IS   IT   PENDING ?                          
         BZ    GETCTA10            NO,  SKIP IT                                 
*                                                                               
GETCTA60 DS    0H                                                               
         AP    CTAMNT,PTANET       ADD  THE  AMOUNT    ALLOCATED                
         SR    R1,R1                                                            
         LH    R1,PTAHOURS         HOURS     ALLOCATED                          
         L     R0,CTHOUR                                                        
         AR    R0,R1               ADD  ALL  HOURS     TOGETHER                 
         ST    R0,CTHOUR                                                        
*                                  WE   SHOULD    ONLY HAVE ONE                 
         B     GETCTA10                 WRITE-OFF ALLOCATION                    
*                                                                               
GETCTA70 DS    0H                                                               
         CP    CTAMNT,=P'0'        ANY  DOLLARS ?                               
         BE    GETCTA80            NO,  LOOK AT   HOURS                         
         CP    SAVEAMNT,CTAMNT     IS   EVERYTHING     WRITTEN   OFF ?          
         BE    GETCTAEX            YES, EXIT                                    
         B     GETCTA90            NO,  MAKE SURE CLIENT    BILL TYPE           
*                                                                               
GETCTA80 DS    0H                                                               
         OC    CTHOUR,CTHOUR       NO,  ANY  HOURS ?                            
         BZ    GETCTA95            NO,  SKIP RECORD                             
         CLC   SAVEHOUR,CTHOUR     IS   EVERYTHING     WRITTEN   OFF ?          
         BE    GETCTAEX            YES, EXIT                                    
*                                                                               
         USING JOBD,R1             MAP  JOB  INFORMATION                        
GETCTA90 DS    0H                                                               
         LA    R1,JOBINFO                                                       
         CLI   JBTYPE,C'C'         IF   PARTIAL,  MUST BE   CLIENT              
         BE    GETCTAEX            YES, EXIT                                    
         MVI   EFLAG,1             NO,  ERROR                                   
         MVI   FERN,NOPART                                                      
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),TRNKULC                                         
*                                                                               
GETCTA93 DS    0H                                                               
         LA    R1,BILNUMH          SET  CURSOR                                  
         ST    R1,FADR                                                          
         B     GETCTAEX                                                         
         DROP  R1                                                               
*                                                                               
GETCTA95 DS    0H                                                               
         MVI   EFLAG,X'FF'         SAY  SKIP RECORD                             
*                                                                               
GETCTAEX DS    0H                                                               
         B     EXIT                RETURN    TO   CALLER                        
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*  GET ACCOUNTS                                                       *         
*                                                                     *         
*  GETCACTS ANALYZES THE TRANSACTION ELEMENTS TO FIND THE CONTRA      *         
*  ACCOUNTS.                                                          *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      EFLAG,    WRTACC,   SKACCNT,  SKACCTNM, SIACCNT,               *         
*      SIACCTNM, SJCPNM,   TMSCOSTN, CONNAME,  WOTYPE                 *         
*                                                                     *         
*      EFLAG    : 0 = NO ERROR FOUND; 1 = ERROR FOUND                 *         
*                                                                     *         
*    CALLS:                                                           *         
*      AGETNAME, GETCOST,  GETOWO,   GETWRT,   POSTEST                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
         SPACE 1                                                                
GETCACTS NTR1                                                                   
         MVI   EFLAG,0             CLEAR     ERROR     FLAG                     
         MVI   WOTYPE,0            WRITE-OFF TYPE                               
         L     R2,AIOAREA3                                                      
         LA    R4,ACCORFST(,R2)    R4 = 1ST  ELEMENT                            
*                                                                               
GETCA10  CLI   TRNEL,0             END  OF   RECORD ?                           
         BE    GETCA35             YES, CONTINUE                                
         CLI   TRNEL,SPAELQ        X'2C' -   SPECIAL        ACCOUNT ?           
         BE    GETCA25                                                          
         CLI   TRNEL,SPDELQ        X'4C' -   SUBSIDIARY     POSTING ?           
         BE    GETCA30                                                          
*                                                                               
GETCA20  ZIC   R0,TRNLN            BUMP TO                                      
         AR    R4,R0                         NEXT                               
         B     GETCA10                            ELEMENT                       
         DROP  R4                                                               
*                                                                               
         USING SPAELD,R4           MAP  SPECIAL   POSTING   ELEMENT             
GETCA25  CLI   ACTION,REC          ACTION=   RECOVER ?                          
         BNE   GETCA20             NO,  GET  NEXT ELEMENT                       
         CLI   SPATYPE,SPATWOFF    WRITE-OFF A/C  ELEMENT ?                     
         BNE   GETCA20             NO,  GET  NEXT ELEMENT                       
         MVC   WRTACC,SPAAULA      EXTRACT   ACCOUNT                            
         B     GETCA20             GET  NEXT ELEMENT                            
*                                                                               
         USING SPDELD,R4           MAP  SUBSIDIARY     POSTING   EL             
GETCA30  DS    0H                                                               
         ZIC   R3,SPDLN            GET  LENGTH    OF   ELEMENT                  
         SH    R3,=YL2(SPDLN1Q+1)  MINUS     BASE ELEMENT   LENGTH              
         BM    GETCA20             BAD  EL,  SKIP IT                            
         CHI   R3,LULACT-1         MORE THAN ONE  ACCOUNT ?                     
         BNH   *+8                 NO,  CONTINUE                                
         LA    R3,LULACT-1         YES, JUST USE  THE  1ST  ACCOUNT             
*                                                                               
         MVC   SKACCNT,SPACES      CLEAR     SK   ACCOUNT                       
         MVC   SIACCNT,SPACES      CLEAR     SI   ACCOUNT                       
         EXMVC R3,SKACCNT+1,SPDACCS     MOVE ACCOUNT   NAME TO   SK             
         MVC   SKACCNT(1),COMPANY       ADD  COMPANY   HEX  ID                  
         CLC   SKACCNT+1(LUL),SI   IS   THIS AN   SI   ACCOUNT ?                
         BE    GETCA45             YES, POSTING   ARE  ALREADY   DONE           
         B     GETCA40             NO,  PROCESS   SK   ACCOUNT                  
*                                                                               
*                                  NO   SUBSIDIARY     POSTING   ELS            
GETCA35  MVC   SKACCNT,TRNKCULC    C/A  IS   AN   INCOME   ACCOUNT              
*                                                                               
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY EL         
         L     R4,APROLST                                                       
         CLI   0(R4),PTAELQ        X'77' -   PROD TRANSACTION ACTIVITY?         
         BNE   GETCA40             NO   77'S,     PROCESS   ACCOUNT             
         CLI   ACTION,REC          ACTION=   RECOVER ?                          
         BNE   GETCA40             NO,  PROCESS   ACCOUNT                       
         MVC   WOTYPE,PTAWWOT      EXTRACT   WRITE-OFF TYPE                     
         DROP  R4                                                               
*                                                                               
GETCA40  MVC   SIACCNT,SKACCNT     SI   ACCOUNT   MIMICS    SK   ACCT           
         MVI   SIACCNT+2,C'I'      INCOME    ACCT SAME AS   SK   ACCT           
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),SKACCNT+1    OTHER     THAN SK=> SI             
         CLC   SKACCNT+1(LUL),SK   IS   IT   AN   SK   ACCOUNT ?                
         BNE   GETCAINV            NO,  TAKE ERROR     EXIT                     
*                                                                               
         MVC   KEY,SPACES          VALIDATE  THE  SK   ACCOUNT                  
         MVC   KEY(LCULACT),SKACCNT     FOR  POSTING                            
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),SKACCNT+1                                       
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETCAINV            NOT  FOUND,    ERROR     EXIT                
         BAS   RE,POSTEST          IS   IT   A    VALID     ACCOUNT ?           
         BNE   GETCAINV            ON   ERROR,    TAKE ERROR    EXIT            
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   SKACCTNM,WORK                                                    
*                                                                               
         MVC   KEY,SPACES          VALIDATE  THE  SI   ROLLOVER                 
         MVC   KEY(LCULACT),SIACCNT     ACCOUNT                                 
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),SIACCNT+1                                       
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETCAINV            NOT  FOUND,    ERROR     EXIT                
         BAS   RE,POSTEST          IS   IT   A    VALID     ACCT ?              
         BNE   GETCAINV            ON   ERROR,    TAKE ERROR    EXIT            
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   SIACCTNM,WORK                                                    
         CLI   COSTING,NO          COST ACCOUNTING     AGENCY ?                 
         BE    GETCA45             NO,  CONTINUE                                
*                                                                               
*                                  GET  COST ACCOUNTING     POINTER             
         GOTO1 GETCOST,DMCB,ACC12I      FROM SI                                 
         BNE   GETCANG             ON   ERROR,    TAKE ERROR    EXIT            
*                                                                               
GETCA45  OC    TMSCOST,TMSCOST     ANY  TMS  COSTING   ACCOUNT ?                
         BZ    GETCA50             NO,  SKIP                                    
         MVC   KEY,SPACES          YES, READ FOR  IT                            
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(L'TMSCOST),TMSCOST                                         
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETCA50             NOT  FOUND,    SKIP                          
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   TMSCOSTN,WORK                                                    
*                                                                               
GETCA50  MVC   KEY,SPACES          READ CONTRA    ACCOUNT                       
         MVC   KEY(LCULACT),TRNKCULC   (USUALLY   1R)  INTO IOAREA1             
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1   SAVE ACCT FOR  MESSAGE                  
         MVC   CONNAME,SPACES                                                   
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETCA55             CAN  NOT  READ CONTRA    ACCOUNT             
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   CONNAME,WORK        SAVE THE  NAME                               
*                                                                               
         CLI   ACTION,WRT          ACTION=   WRITE-OFF ?                        
         BNE   GETCA55             NO,  SKIP                                    
         CLI   WOAOVR,0            USER OVERRIDE  WRITE-OFF ACCOUNT ?           
         BNE   GETCA55             YES, SKIP                                    
*                                  1R   CONTRA    ACCOUNT ?                     
         CLC   TRNKCUNT(LUL),=C'1R'                                             
         BNE   GETCA55             NO,  SKIP                                    
         BAS   RE,GETOWO           EXTRACT   THE  WRITE-OFF ACCOUNT             
*                                                                               
GETCA55  CLI   ACTION,WRT          ACTION=   WRITE-OFF ?                        
         BNE   GETCA60             NO,  SKIP                                    
         CLI   WOAOVR,0            USER OVERRIDE  WRITE-OFF ACCOUNT ?           
         BE    *+10                NO,  SKIP                                    
         MVC   WRTACC,WOAOVR       EXTRACT   WRITE-OFF A/C  OVERRIDE            
         CLI   WRTACC,0            WRITE-OFF A/C  SET  YET ?                    
         BNE   *+10                YES, SKIP                                    
         MVC   WRTACC,WRTDEF       NO,  USE  THE  DEFAULT                       
         BAS   RE,GETWRT           VERIFY    A/C- REFRESH  COSTING DATA         
         BNE   GETCANG             ON   ERROR,    TAKE ERROR    EXIT            
         B     GETCA70             OKAY,     CONTINUE                           
*                                                                               
*                                  ACTION=   RECOVER                            
GETCA60  CLI   WRTACC,0            WRTACC    FOUND ?                            
         BNE   GETCA65             YES, VERIFY    ACCOUNT                       
         MVC   WRTACC,SPACES       NO,  IT   MUST HAVE BEEN SIWO                
         MVC   WRTACC(4),=C'SIWO'                                               
*                                                                               
GETCA65  BAS   RE,GETWRT           VERIFY    ACCOUNT                            
         BNE   GETCANG             ON   ERROR,    TAKE ERROR    EXIT            
*                                                                               
GETCA70  GOTO1 AGETNAME,ADPRD      GET  ACCT NAME FROM PROD RCD                 
         MVC   SJCPNM,WORK         SAVE NAME                                    
         MVC   XTRAMESS,SPACES     CLEAR     EXTRA     ERROR    DATA            
         B     GETCAEX             EXIT                                         
*                                                                               
GETCAINV MVI   FERN,INVACT         INVALID   ACCOUNT                            
*                                  NOTE: XTRAMESS HAS ACCOUNT NUMBER            
         LA    R1,BILNUMH          SET  CURSOR                                  
         ST    R1,FADR                                                          
*                                                                               
GETCANG  MVI   EFLAG,1             SET  ERROR     FLAG                          
*                                                                               
GETCAEX  B     EXIT                RETURN    TO   CALLER                        
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  EXTRACT THE WRITE-OFF ACCOUNT FROM THE 1R ACCOUNT                  *         
*                                                                     *         
*  ROUTINE WILL LOOK UPWARDS THROUGH THE 1R LEDGER                    *         
*                                                                     *         
*    INPUT:                                                           *         
*      AIOAREA1 = A(LOWEST LEVEL 1R ACCOUNT)                          *         
*      R2       = A(TRANSACTION KEY)                                  *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      WRTACC   = WRITE-OFF OVERRIDE ACCOUNT IF FOUND                 *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
GETOWO   NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         MVC   KEY(LCUL),TRNKCULC  GET  1R   LEDGER                             
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETOWOX                                                          
         XC    TMELV1R,TMELV1R     CLEAR HIERARCHY SAVE AREA                    
         SR    R0,R0                                                            
         L     R1,AIOAREA1         GET  LENGTHS                                 
         AH    R1,DATADISP                                                      
*                                                                               
GETOWO1  CLI   0(R1),0                                                          
         BE    GETOWOX                                                          
         CLI   0(R1),ACLELQ                                                     
         BE    GETOWO3                                                          
         IC    R0,1(,R1)                                                        
         AR    R1,R0                                                            
         B     GETOWO1                                                          
*                                                                               
         USING ACLELD,R1           MAP  ACCOUNT   LENGTHS   ELEMENT             
GETOWO3  MVC   PERLENS(1),ACLVLEN                                               
         MVC   PERLENS+1(1),ACLVLEN+(L'ACLVALS)                                 
         MVC   PERLENS+2(1),ACLVLEN+(L'ACLVALS*2)                               
         MVC   PERLENS+3(1),ACLVLEN+(L'ACLVALS*3)                               
         MVC   TMELV1R,PERLENS                                                  
         DROP  R1                                                               
*                                                                               
         LA    R6,PERLENS          R6=  1R   LEDGER    LENGTHS                  
         LA    R3,4                R3=  LOOP COUNTER                            
*                                                                               
GETOWO4  ZIC   RE,0(,R6)           GET  ACCOUNT   LENGTH                        
         LA    RE,2(,RE)           ADD  ON   FOR  COMP/UNIT/LEDG                
         MVC   KEY,SPACES                                                       
         EXMVC RE,KEY,TRNKCULC     CONTRA                                       
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETOWO13                                                         
*                                                                               
GETOWO5  L     R1,AIOAREA1         SEE  IF   IT   HAS  A    2C   EL             
         AH    R1,DATADISP                                                      
         SR    R0,R0                                                            
*                                                                               
GETOWO7  CLI   0(R1),0                                                          
         BE    GETOWO13                                                         
         CLI   0(R1),SPAELQ        TEST FOR  POINTER   ELEMENT                  
         BE    GETOWO11                                                         
*                                                                               
GETOWO9  IC    R0,1(,R1)                                                        
         AR    R1,R0                                                            
         B     GETOWO7                                                          
*                                                                               
         USING SPAELD,R1           MAP  APECIAL   POSTING   ACCOUNT  EL         
GETOWO11 CLI   SPATYPE,SPATWOFF    ANY  WRITE-OFF OVER-RIDE ?                   
         BNE   GETOWO9             NO,  KEEP LOOKING                            
         MVC   WRTACC,SPAAULA      YES, SAVE IT                                 
*                                                                               
GETOWO13 CLI   0(R6),12                                                         
         BNL   GETOWOX                                                          
         LA    R6,1(,R6)                                                        
         BCT   R3,GETOWO4                                                       
*                                                                               
GETOWOX  B     EXIT                                                             
         DROP  R1,R2                                                            
         EJECT ,                                                                
***********************************************************************         
*  VALIDATE THE WRITE-OFF ACCOUNT                                     *         
*                                                                     *         
*    INPUT:                                                           *         
*      WRTACC   - WRITE-OFF ACCOUNT                                   *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      WRTACCNM - WRITE-OFF ACCOUNT NAME                              *         
*                                                                     *         
*      CONDITION KEY : EQ = NO ERRORS FOUND; NE = ERRORS FOUND        *         
*                                                                     *         
*    CALLS:                                                           *         
*      AGETNAME, GETCOST AND POSTEST                                  *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
GETWRT   NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(LCUL),TRNKCULC  GET  1R   LEDGER                             
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   GETWR06                                                          
         XC    TMELV1R,TMELV1R     CLEAR HIERARCHY SAVE AREA                    
         SR    R0,R0                                                            
         L     R1,AIOAREA1         GET  LENGTHS                                 
         AH    R1,DATADISP                                                      
*                                                                               
GETWR02  CLI   0(R1),0                                                          
         BE    GETWR06                                                          
         CLI   0(R1),ACLELQ                                                     
         BE    GETWR04                                                          
         IC    R0,1(,R1)                                                        
         AR    R1,R0                                                            
         B     GETWR02                                                          
*                                                                               
         USING ACLELD,R1           MAP  ACCOUNT   LENGTHS   ELEMENT             
GETWR04  MVC   PERLENS(1),ACLVLEN                                               
         MVC   PERLENS+1(1),ACLVLEN+(L'ACLVALS)                                 
         MVC   PERLENS+2(1),ACLVLEN+(L'ACLVALS*2)                               
         MVC   PERLENS+3(1),ACLVLEN+(L'ACLVALS*3)                               
         MVC   TMELV1R,PERLENS                                                  
         DROP  R1                                                               
*                                                                               
GETWR06  MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(LULACT),WRTACC                                             
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT              NOT  FOUND,    ERROR     EXIT                
         BAS   RE,POSTEST                                                       
         BNE   GETWRNG             NOT  OKAY,     ERROR     EXIT                
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   WRTACCNM,WORK                                                    
         CLI   COSTING,NO          TEST FOR  NON-COST ACCOUNTING AGENCY         
         BE    GETWROK             YES, DO   NOT  NEED COSTING  POINTER         
*                                                                               
         GOTO1 GETCOST,DMCB,ACC12                                               
         BNE   GETWRNG             ON   ERROR, TAKE ERROR   EXIT                
*                                                                               
GETWROK  SR    RB,RB               CC = EQ = OKAY                               
         MVC   XTRAMESS,SPACES     CLEAR     EXTRA     ERROR    DATA            
*                                                                               
GETWRNG  LTR   RB,RB               CC = NEQ= ERROR                              
*                                                                               
         B     EXIT                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*  GET THE REVENUE ACCOUNT POINTED TO BY A SI ACCOUNT                 *         
*                                                                     *         
*    INPUT:                                                           *         
*      AIOAREA1 = A(SI ACCOUNT)                                       *         
*      PARM 1   = A(12 ACCOUNT KEY/NAME FOR OUTPUT)                   *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      12 ACCOUNT KEY AND NAME INITIALIZED                            *         
*                                                                     *         
*      CONDITION KEY : EQ = NO ERRORS FOUND; NE = ERRORS FOUND        *         
*                                                                     *         
*    CALLS:                                                           *         
*      AGETNAME AND POSTEST                                           *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
GETCOST  NTR1  ,                                                                
         L     R4,0(,R1)           GET  A(O/P     KEY/NAME)                     
         MVC   0(15,R4),SPACES     CLEAR     O/P  AREAS                         
         MVC   15(36,R4),SPACES                                                 
         XC    FULL,FULL           CLEAR     AREA FOR  STATUS   EL ADDR         
         L     R1,AIOAREA1                                                      
         AH    R1,DATADISP                                                      
*                                                                               
GETCO10  CLI   0(R1),0             TEST FOR  EOR                                
         BE    GETCO50             YES                                          
         CLI   0(R1),RSTELQ        TEST FOR  STATUS    ELEMENT                  
         BNE   GETCO20                                                          
         ST    R1,FULL             SAVE ITS  ADDRESS                            
         B     GETCO30                                                          
*                                                                               
GETCO20  CLI   0(R1),SPAELQ        TEST FOR  SPECIAL   ACCOUNT  POINTER         
         BE    GETCO40             YES                                          
*                                                                               
GETCO30  ZIC   R2,1(,R1)           GET  NEXT ELEMENT                            
         AR    R1,R2                                                            
         B     GETCO10                                                          
*                                                                               
         USING SPAELD,R1           MAP  SPECIAL   POSTING   ACCOUNT  EL         
GETCO40  CLI   SPATYPE,SPATANAL    TEST FOR  ANALYSIS  POINTER                  
         BNE   GETCO30             NO                                           
         MVC   0(1,R4),COMPANY                                                  
         MVC   1(2,R4),=C'12'      UNIT/LEDGER                                  
         MVC   3(12,R4),SPAAULA                                                 
         B     GETCO30                                                          
*                                                                               
GETCO50  CLI   0(R4),C' '          TEST IF   ANALYSIS  POINTER   FOUND          
         BNE   GETCO70             YES                                          
         ICM   R1,15,FULL          GET  A(STATUS  ELEMENT)                      
         BZ    GETCO60             NO                                           
*                                                                               
*                                  **   BUILD     12   COST ACCT **             
         USING RSTELD,R1           MAP  RECORD    STATUS    ELEMENT             
         MVC   0(1,R4),COMPANY     STORE     COMPANY                            
         MVC   1(2,R4),=C'12'      U/L  12                                      
*                                  GET  ANALYSIS  CODE                          
         MVC   3(L'RSTCOSTG,R4),RSTCOSTG                                        
         CLI   RSTCOSTG,C' '       TEST FOR  MISSING   POINTER                  
         BNE   GETCO70             NO                                           
         DROP  R1                                                               
*                                                                               
GETCO60  B     GETCOER             MISSING   ANALYSIS  POINTER   ON             
*                                                                               
GETCO70  MVC   KEY,SPACES          VALIDATE  12   ACCOUNT                       
         MVC   KEY(LCULACT),0(R4)                                               
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT              NOT  FOUND,    ERROR     EXIT                
         BAS   RE,POSTEST                                                       
         BNE   GETCONG             NOT  OKAY,     ERROR     EXIT                
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   15(36,R4),WORK                                                   
         B     GETCOOK             EXIT OKAY                                    
*                                                                               
GETCOER  MVC   FVMSGNO,=AL2(2215)  MISSING   ANALYSIS  POINTER   ON             
         L     RE,AIOAREA1                                                      
*                                  EXTRACT   SI   KEY                           
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),1(RE)                                           
         MVI   FVMTYPE,FVMERR      ERROR     MESSAGE                            
         MVI   FERN,OK                                                          
         B     ERRXIT              ERROR     EXIT                               
*                                                                               
GETCOOK  SR    RB,RB               CC = EQ = OKAY                               
         MVC   XTRAMESS,SPACES     CLEAR     EXTRA     ERROR    DATA            
*                                                                               
GETCONG  LTR   RB,RB               CC = NEQ= ERROR                              
*                                                                               
         B     EXIT                RETURN    TO   CALLER                        
         EJECT ,                                                                
***********************************************************************         
*  TEST POSTING STATUS OF ACCOUNTS                                    *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      CONDITION KEY : EQ = NO ERRORS FOUND; NE = ERRORS FOUND        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
POSTEST  NTR1                                                                   
         L     RF,AIOAREA1                                                      
         AH    RF,DATADISP                                                      
         MVI   FLAG,NO                                                          
         MVI   FERN,INVPOST                                                     
*                                                                               
PT10     CLI   0(RF),0                                                          
         BE    PT50                                                             
         CLI   0(RF),RSTELQ        X'30' -   RECORD    STATUS    EL             
         BE    PT30                                                             
         CLI   0(RF),ABLELQ        X'32' -   ACCOUNT   BALANCE   EL             
         BE    PT40                                                             
*                                                                               
PT20     ZIC   RE,1(,RF)                                                        
         AR    RF,RE                                                            
         B     PT10                                                             
*                                                                               
         USING RSTELD,RF           MAP  RECORD    STATUS    ELEMENT             
PT30     MVI   FERN,CLOSED                                                      
         TM    RSTSTAT,RSTSACIC    CLOSED ?                                     
         BNZ   ERRXIT                                                           
         MVI   FERN,LOCKED                                                      
         TM    RSTSTAT,RSTSACIL    LOCKED ?                                     
         BNZ   ERRXIT                                                           
         MVI   FERN,INVPOST                                                     
         B     PT20                                                             
         DROP  RF                                                               
*                                                                               
PT40     MVI   FLAG,YES            BALANCE   ELEMENT   PRESENT                  
         B     PT20                                                             
*                                                                               
PT50     CLI   FLAG,YES                                                         
         BNE   ERRXIT                                                           
         MVI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  READ 1ST JOB RECORD                                                *         
*                                                                     *         
*    INPUT:                                                           *         
*      PARM 1:                                                        *         
*               BYTE    0 = X'00'        = DO NOT   LOCK THE RECORD   *         
*                         = READLOCK     =          LOCK THE RECORD   *         
*               BYTES 1-3 = A(I/O  AREA) = AIOAREA1 OR   AIOAREA3     *         
*      ACTION             = ACTION REQ'D = WRT      OR   REC          *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      CONDITION CODE: EQ = RECORD FOUND; NE = RECORD NOT FOUND       *         
*                                                                     *         
*    NOTE:                                                            *         
*      FOR ACTION = WRT, DO NOT RETURN TRANSACTION TYPE (TRNTYPE)     *         
*                   WRITE-OFF (TRNTWRTO = 57) ELEMENTS                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
RD1STREC NTR1                                                                   
         L     R2,AREAD            ->   READ ROUTINE                            
         L     R3,ASEQ             ->   SEQ  ROUTINE                            
         TM    0(R1),READLOCK      LOCK REQUESTED ?                             
         BZ    RD1STR10            NO,  SKIP                                    
         L     R2,AREADL           ->   READ LOCK RTN                           
         L     R3,ASEQL            ->   SEQ  LOCK RTN                           
*                                                                               
RD1STR10 ST    R2,AREADRTN         SAVE READ RTN  ADDR                          
         ST    R3,ASEQRTN          SAVE SEQ  RTN  ADDR                          
*                                                                               
         L     R2,0(,R1)           ->   I/O  AREA                               
         LA    R2,0(,R2)           CLEAR     HIGH ORDER     BYTE                
         ST    R2,ARDIOA           SAVE ADDRESS                                 
*                                                                               
         MVC   KEY,SPACES          CLEAR     KEY                                
         MVC   KEY(LCULACT),JOBKEY GET  CURRENT   JOB                           
         GOTO1 AREADRTN,ARDIOA     READ 1ST  RCD  FOR  JOB                      
         BE    *+6                                                              
         DC    H'0'                CAN  NOT  READ JOB                           
*                                                                               
         B     RDRECSEQ            READ NEXT RECORD                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  READ NEXT JOB RECORD                                               *         
*                                                                     *         
*    INPUT:                                                           *         
*      PARM 1:                                                        *         
*               BYTE    0 = X'00'        = DO NOT   LOCK THE RECORD   *         
*                         = READLOCK     =          LOCK THE RECORD   *         
*               BYTES 1-3 = A(I/O  AREA) = AIOAREA1 OR   AIOAREA3     *         
*      ACTION             = ACTION REQ'D = WRT      OR   REC          *         
*                                                                     *         
*    OUTPUT:                                                          *         
*      CONDITION CODE: EQ = RECORD FOUND; NE = RECORD NOT FOUND       *         
*                                                                     *         
*    NOTE:                                                            *         
*      FOR ACTION = WRT, DO NOT RETURN TRANSACTION TYPE (TRNTYPE)     *         
*                   WRITE-OFF (TRNTWRTO = 57) ELEMENTS                *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         SPACE 1                                                                
RDNXTREC NTR1                                                                   
         L     R2,ASEQ             ->   SEQ  ROUTINE                            
         TM    0(R1),READLOCK      LOCK REQUESTED ?                             
         BZ    *+8                 NO,  SKIP                                    
         L     R2,ASEQL            ->   SEQ  LOCK RTN                           
         ST    R2,ASEQRTN          SAVE SEQ  RTN  ADDR                          
*                                                                               
         L     R2,0(,R1)           ->   I/O  AREA                               
         LA    R2,0(,R2)           CLEAR     HIGH ORDER     BYTE                
         ST    R2,ARDIOA           SAVE ADDRESS                                 
*                                                                               
RDRECSEQ GOTO1 ASEQRTN,ARDIOA      READ NEXT RCD  FOR  JOB                      
*                                                                               
*                                  ANALYZE   THE  RCD  READ                     
         CLC   JOBKEY,TRNKCULA     SAME JOB ?                                   
         BNE   RDRECNG             NO,  RCD  NOT  FOUND                         
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,ACCORFST(,R2)    GET  1ST  RECORD                             
         CLI   0(R3),TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   RDRECSEQ            NO,  READ NEXT RECORD                        
*                                                                               
         CLC   TRNANAL,=C'99'      ELIMINATE BILLING                            
         BE    RDRECSEQ                                                         
         CLC   TRNANAL,=C'**'      AND  OTHER     STUFF                         
         BE    RDRECSEQ                                                         
         CLC   TRNANAL,SPACES      AND  BLANK                                   
         BE    RDRECSEQ                                                         
*                                                                               
         TM    TRNRSTAT,TRNSREVS   REVERSAL ?                                   
         BO    RDRECSEQ            YES, READ NEXT RECORD                        
         TM    TRNRSTAT,TRNSDRFT   DRAFT ?                                      
         BZ    RDREC50             NO,  CONTINUE                                
         CLI   OKDRAFTS,YES        PROCESS   DRAFT     TRANSACTIONS ?           
         BNE   RDRECSEQ            NO,  READ NEXT RECORD                        
*                                                                               
RDREC50  CLI   ACTION,WRT          ACTION =  WRITE-OFF ?                        
         BNE   RDRECOK             NO,  GOOD RECORD                             
         CLI   TRNTYPE,TRNTWRTO    WRITE-OFF TYPE ?                             
         BE    RDRECSEQ            YES, READ NEXT RECORD                        
*        B     RDRECOK             NO,  GOOD RECORD                             
*                                                                               
RDRECOK  SR    RB,RB               CC = EQU                                     
*                                                                               
RDRECNG  LTR   RB,RB               CC = NEQ                                     
*                                                                               
RDRECEX  B     EXIT                RETURN    TO   CALLER                        
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
*  GET ALLOCATED BASED ON 77 DATA                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING PTAELD,R4           MAP  PROD TRANSACTION    ACTIVITY EL         
         SPACE 1                                                                
GETALL   NTR1                                                                   
         ZAP   ALLNET,=P'0'                                                     
         XC    ALLHRS,ALLHRS                                                    
*                                                                               
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
         L     R4,APROLST                                                       
         CLI   0(R4),PTAELQ        X'77' -   PROD TRANSACTION ACTIVITY?         
         B     GETA20                                                           
*                                                                               
GETA10   MVI   ELCODE,PTAELQ       X'77' -   PROD TRANSACTION  ACTIVITY         
         BAS   RE,NEXTEL           GET  NEXT 77   ELEMENT                       
*                                                                               
GETA20   BNE   EXIT                NO   MORE,     EXIT                          
*                                                                               
         TM    PTASTAT1,PTASPEND   YES, IS   IT   PENDING ?                     
         BZ    GETA10              NO,  GET  NEXT                               
         CLI   PTATYPE,PTATRAL     ALLOCATED TO   BILL ?                        
         BNE   GETA10              NO,  SKIP IT                                 
*                                                                               
         ZAP   ALLNET,PTANET       YES, GET  CURRENT   ALLOCATION               
*                                                                               
         SR    RF,RF                                                            
         LH    RF,PTAHOURS                                                      
         ST    RF,ALLHRS                                                        
*                                                                               
         B     GETA10              SHOULD    ONLY HAVE 1 ALLOCATED 77           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
*  INITIALIZE THE OVERLAY TWA AREA                                    *         
***********************************************************************         
         SPACE 1                                                                
INITOTWA DS    0H                                                               
         ST    RE,SAVRE            SAVE      RE                                 
         MVI   SMODE,INIT          SET  FOR  INITIALIZATION MODE                
         LA    RE,NAMETBLE         ->             NAME TABLE                    
         LA    RF,L'NAMETBLE       LENGTH    OF   NAME TABLE                    
         XCEF  ,                   CLEAR     TABLE     USED IN  DISPLAY         
         MVC   SAVECON,SPACES      CLEAR     KEY  IN   DISPLAY   TABLE          
         L     RE,SAVRE            RESTORE   RE                                 
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  CALL DICTATE                                                       *         
*                                                                     *         
*  INPUT:                                                             *         
*    R2=  ADDRESS OF FIELD TO BE TRANSLATED                           *         
*         NOTE: THE FIELD MUST HAVE BEEN PREVIOUSLY INITIALIZED WITH  *         
*               AN MVCDD INSTRUCTION.                                 *         
***********************************************************************         
         SPACE 1                                                                
CALLDICT DS    0H                                                               
         ST    RE,SAVRE            SAVE      RE                                 
         GOTO1 VDICTAT,DMCB,C'SL  ',(R2),0                                      
         L     RE,SAVRE            RESTORE   RE                                 
         BSM   0,RE                RETURN                                       
         EJECT ,                                                                
***********************************************************************         
*  DISPLAY MESSAGE ON SCREEN AND GO TO EXIT                           *         
***********************************************************************         
         SPACE 1                                                                
*                                  ENTER     DATE OF   WRITE-OFF                
WOENTDAT MVC   FVMSGNO,=AL2(2107)      (DEFAULT=TODAY)                          
         LA    R1,WONDATEH         PUT  CURSOR    AT   DATE FIELD               
         B     SETINFO                                                          
*                                                                               
DISPWONR MVC   FVMSGNO,=AL2(2108)  ENTER     WRITE-OFF NARRATIVE                
         LA    R1,WONLIN1H                                                      
         B     SETINFO                                                          
*                                                                               
*                                  HIT  ENTER     FOR  NEXT                     
DISPNXPG MVC   FVMSGNO,=AL2(2109)       NEXT PAGE OF   ACCOUNTS                 
         LA    R1,BILACTH                                                       
         B     SETINFO                                                          
*                                                                               
*                                  WRITE-OFF RESTORED -                         
RECVWORE MVC   FVMSGNO,=AL2(2111)       ENTER     NEXT ACTION                   
         LA    R1,BILACTH                                                       
         B     SETINFO                                                          
*                                                                               
RECVWO#R MVC   FVMSGNO,=AL2(2216)  WRITE-OFF NUMBER    REQUIRED                 
         B     SETERX                                                           
*                                                                               
RECVWO#L MVC   FVMSGNO,=AL2(2217)  WRITE-OFF NUMBER    TOO  LARGE               
         B     SETERX                                                           
*                                                                               
*                                  INVALID   DATE FORMAT                        
RECVINVD MVC   FVMSGNO,=AL2(2218)      (E.G. JAN01/99)                          
         LA    R1,WONDATEH                                                      
         B     SETERX                                                           
*                                                                               
*                                  NO   ACTIVITY  TODAY     ON                  
RECVNOAC MVC   FVMSGNO,=AL2(2219)    THIS    JOB                                
         LA    R1,BILNUMH                                                       
         B     SETERX                                                           
*                                                                               
*                                  TOO  MANY CONTRA    ACCOUNTS                 
PUTTBLER MVC   FVMSGNO,=AL2(2225)       ALLOCATED                               
         LA    R1,BILNUMH                                                       
         B     SETERX                                                           
*                                                                               
*                                  NOTHING   TO   BE   POSTED                   
NOPOSTS  CLI   ACTION,REC          RECOVERY ?                                   
         BE    RECVNOTF            YES, WRITE-OFF NOT  ON   FILE                
         MVC   FVMSGNO,=AL2(2220)  NO   DATA TO   WRITE-OFF                     
         LA    R1,BILNUMH          ->   BILL NUMBER                             
         B     SETERX                                                           
*                                                                               
RECVNOTF MVC   FVMSGNO,=AL2(325)   WRITE-OFF NOT  ON   FILE                     
         LA    R1,BILNUMH                                                       
         B     SETERX                                                           
*                                                                               
SETERX   MVI   FVMTYPE,FVMERR                                                   
         B     SETINFO2                                                         
*                                                                               
SETINFO  MVI   FVMTYPE,FVMINFO                                                  
*                                                                               
SETINFO2 ST    R1,FADR                                                          
*                                                                               
SETINFO4 MVI   FERN,OK                                                          
         B     EXIT                                                             
         EJECT ,                                                                
***********************************************************************         
*  ERROR ROUTINES - THE TEXT OF ALL OF THESE MESSAGES IS: ERROR - ... *         
***********************************************************************         
         SPACE 1                                                                
NODATA   MVC   FVMSGNO,=AL2(2220)  NO   DATA TO   WRITE-OFF                     
         B     SETERR                                                           
*                                                                               
*                                  WRITE-OFF NNNN CANCELLED -                   
MAINBAD# MVC   FVMSGNO,=AL2(2214)       AMOUNTS   DIFFER                        
         MVI   FVMTYPE,FVMERR                                                   
         MVC   XTRAMESS,SPACES     NUMBER    OF   THE  WRITE-OFF                
         MVC   XTRAMESS(L'WONUMCHR),=C'0000'                                    
         OC    XTRAMESS(L'WONUMCHR),WONUMCHR                                    
         B     SETERR                                                           
*                                                                               
NOCHR    MVC   FVMSGNO,=AL2(2202)  NO   ALLOCATED CHARGES   ON   JOB            
*                                                                               
SETERR   LA    R1,BILACTH                                                       
*                                                                               
SETERR2  ST    R1,FADR                                                          
         MVI   FERN,OK                                                          
         MVI   FVMTYPE,FVMERR                                                   
         GOTO1 AERRORX2                                                         
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
*                                                                               
ERRXIT   LTR   RB,RB               CC = NEQ                                     
         LA    R1,BILACTH                                                       
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  CONSTANTS                                                          *         
***********************************************************************         
         SPACE 1                                                                
SI       DC    CL2'SI'             SI   PREFIX                                  
SJ       DC    CL2'SJ'             SJ   PREFIX                                  
SK       DC    CL2'SK'             SK   PREFIX                                  
WO       DC    CL2'WO'             WRITE-OFF PREFIX                             
WR       DC    CL2'WR'             WRITE-OFF REFERENCE PREFIX                   
*                                  END  OF   NAME TBL  PREFIX                   
EONMTB   DC    CL(L'CONTRA)'999ENDOFTABLE'                                      
         EJECT ,                                                                
***********************************************************************         
*  TABLES                                                             *         
***********************************************************************         
         SPACE 1                                                                
SCRTXT   DS    0F                                                               
         DC    AL4(WONHED1H-TWAD,6002)  TARGET W/O                              
         DC    AL4(WONHED2H-TWAD,6003)  ACTUAL W/O                              
         DC    X'FF'                                                            
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*              FORMAT ELEMENT TO CARRY WORKCODE AND AMOUNT            *         
***********************************************************************         
*                                                                               
*              BUILD X'DB' ELEMENT WITH WORKCODE AND SJ AMOUNT                  
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         USING FFTELD,R5                                                        
BUILDDB  NTR1  BASE=*,LABEL=*                                                   
         L     R2,AIOAREA3         CURRENT TRANSACTION                          
         LA    R5,SVDBELM                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTWORK+L'FFTWAMT                      
         MVI   FFTTYPE,FFTTWRKC                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTWORK+L'FFTWAMT                                      
         MVC   FFTWORK,TRNKWORK                                                 
         ZAP   DUB,CTAMNT                                                       
         ZAP   FFTWAMT,DUB                                                      
*                                                                               
BLDDBX   XIT1                                                                   
         DROP  R2,R5                                                            
         EJECT                                                                  
***********************************************************************         
*              BUILD X'C0' ELEMENT AND ADD IT                         *         
***********************************************************************         
*                                                                               
         USING APEELD,R3                                                        
         USING TRNRECD,R6                                                       
BUILDC0  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,ELEMENT                                                       
         L     R6,ATIO                                                          
                                                                                
         LA    R1,SUBTAB           R1=A(TABLE OF ANALYSIS ACCOUNTS)             
         SR    R0,R0                                                            
         ICM   R0,1,SUBTABN        R0=NUMBER OF TABLE ENTRIES                   
         BZ    BLDC0X                                                           
                                                                                
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
                                                                                
         USING SUBTABD,R1                                                       
BLDC02   CLC   SUBACC,TRNKULA                                                   
         BE    BLDC04                                                           
         CLC   SUBACC,TRNKULC                                                   
         BE    BLDC04                                                           
                                                                                
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL ENTRY)                            
         MVC   APENSTAT,SUBSTA                                                  
         MVC   APENACT,SUBACC                                                   
         LA    RF,APENACT+L'APENACT-1                                           
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(RF),0             AND DROP TRAILING SPACES                     
         BCT   RF,*-12                                                          
         AHI   RF,1                                                             
         SR    RF,RE               RF=L'SUB-ELEMENT                             
         STC   RF,APENLEN                                                       
         DROP  RE                                                               
                                                                                
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,APELN                                                         
         AR    RE,RF                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         AHI   RE,1                                                             
         STC   RE,APENUM                                                        
                                                                                
BLDC04   AHI   R1,SUBTABL          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,BLDC02                                                        
         GOTO1 ABADDLST,(R3)                                                    
                                                                                
BLDC0X   XIT1                                                                   
         DROP  R1,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
*  TRANSMIT THE SCREEN                                                *         
***********************************************************************         
         SPACE 1                                                                
TRANSMIT NMOD1 0,**SMIT**                                                       
*                                  CLEAR     ALL  FIELDS    AFTER DATE          
         TWAXC WONDATEH,WONTABH,PROT=N DATE                                     
         LA    R1,BILTABH                                                       
         LA    R2,WONTABH                                                       
*                                                                               
TRNS1    NI    1(R1),X'F3'         NORMAL    INTENSITY                          
         TM    1(R1),X'20'         IS   IT   PROTECTED ?                        
         BZ    *+8                 NO,  SKIP                                    
         OI    1(R1),X'08'         YES, TURN ON   HIGH INTENSITY                
*                                                                               
         OI    6(R1),FVOXMT        TRANSMIT  THE  FIELD                         
         ZIC   R0,0(,R1)                                                        
         AR    R1,R0               BUMP TO   NEXT FIELD                         
         CR    R1,R2                                                            
         BL    TRNS1                                                            
         XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  OVERLAY TWA                                                        *         
***********************************************************************         
         SPACE 1                                                                
OVRTWAD  DSECT                                                                  
SMODE    DS    CL1                                                              
DSPLY    EQU   2                                                                
MARK     EQU   3                                                                
*                                                                               
SAVECON  DS    CL14                                                             
*                                                                               
OVRTWAQ1 EQU   *-OVRTWAD           LENGTH UP TO HERE                            
*                                  MAX  NUMBER OF ENTRIES IN TABLE              
NAMTBLMX EQU   (OVRTWALQ-OVRTWAQ1-(L'CONTRA+2))/ENTRYQ                          
*                                                                               
NAMETBLE DS    CL(NAMTBLMX*ENTRYQ+L'CONTRA+2)                                   
         SPACE 2                                                                
***********************************************************************         
*  COVER ACCOUNT TABLE                                                *         
***********************************************************************         
         SPACE 1                                                                
ENTRY    DSECT                                                                  
CONTRA   DS    CL14                                                             
SUBTOT   DS    PL8                                                              
ENTRYQ   EQU   *-ENTRY                                                          
         EJECT ,                                                                
***********************************************************************         
*  OVERLAY WORKING STORAGE                                            *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
ALLHRS   DS    F                   HRS                                          
LINES    DS    F                                                                
SAVEHOUR DS    F                   HOURS                                        
BATCHHRS DS    F                                                                
CTHOUR   DS    F                   HOURS   WRITTEN-OFF   IN TRANSACTION         
*                                                                               
SAVRE    DS    F                   SAVE    AREA FOR  RE                         
*                                                                               
AREADRTN DS    A                   A(READ  ROUTINE)                             
ASEQRTN  DS    A                   A(READ  SEQ  ROUTINE)                        
READLOCK EQU   X'80'               READ    WITH LOCK                            
*                                                                               
ARDIOA   DS    A                   A(I/O   AREA)                                
AALTIOA  DS    A                   A(ALT   I/O  AREA)                           
*                                                                               
CTAMNT   DS    PL8                 AMOUNT  TO   BE   POSTED                     
CTATOT   DS    PL8                 TOTAL   AMOUNT    TO   BE   POSTED           
MKAMNT   DS    PL8                 MARKED  AMOUNT                               
*                                                                               
ALLNET   DS    PL(L'PTANET)        PTANET  SAVE AREA                            
SAVETOT  DS    PL(L'ALLNET)                                                     
SAVEAMNT DS    PL(L'TRNAMNT)       TRNAMNT SAVE AREA                            
*                                                                               
SJCPJNM  DS    CL36                SJ      CLI/PRD/JOB    NAME                  
SJCPNM   DS    CL36                SJ      CLI/PRD        NAME                  
*                                                                               
WRTDEF   DS    CL14                WRITE-OFF    ACCOUNT   DEFAULT               
*                                                                               
WRTACC   DS    CL14                WRITE-OFF    ACCOUNT                         
WRTACCNM DS    CL36                WRITE-OFF    ACCOUNT   NAME                  
*                                                                               
ACC1C    DS    CL15                COSTING      ACCOUNT                         
ACC1CN   DS    CL36                COSTING      ACCOUNT   NAME                  
*                                                                               
ACC12    DS    CL15                WRITE-OFF    SI   12   ACCOUNT               
ACC12N   DS    CL36                                                             
*                                                                               
ACC12I   DS    CL15                INCOME       ROLLOVER  12   ACCOUNT          
ACC12IN  DS    CL36                INCOME       ROLLOVER  ACOUNT   NAME         
*                                                                               
*                                  **** BEGIN   KEEP TOGETHER                   
SKACCNT  DS    CL15                INCOME SUSPENSE  ACCOUNT                     
SKACCTNM DS    CL36                INCOME       SUSPENSE  ACCOUNT  NAME         
*                                                                               
SIACCNT  DS    CL15                ROLLOVER     INCOME    ACCOUNT               
SIACCTNM DS    CL36                ROLLOVER     INCOME    ACCOUNT  NAME         
*                                  **** END     KEEP TOGETHER                   
*                                                                               
CTACCNT  DS    CL15                TEMP    STORAGE   FOR  ACCOUNT               
*                                                                               
SVKEY    DS    CL42                SAVE    AREA FOR  KEY                        
READKEY  DS    CL42                                                             
TEMPKEY  DS    CL(LCULACT)                                                      
*                                                                               
NUMREC   DS    XL1                 NUMBER  OF   RECORDS   IN   TABLE            
TEMPAMT  DS    CL(L'WONAMTF)       TEMPORARY    AMOUNT                          
TEMPTOT  DS    CL(L'WONAMT1)       TEMPORARY    TOTAL     AMOUNT                
*                                                                               
OFFICE   DS    CL2                 OFFICE  CODE OF   JOB                        
EFLAG    DS    CL1                                                              
TMOS     DS    CL2                 TRANSACTION  MONTH     OF   SERVICE          
*                                                                               
MATCH    DS    CL6                 WHAT    TO   LOOK FOR  IN   ELEMENT          
REPLACE  DS    CL6                 WHAT    TO   REPLACE   IN   4B   EL          
*                                                                               
WONUM    DS    XL2                 WRITE-OFF    NUM  FROM LEDGER    REC         
WOTYPE   DS    CL1                 WRITE-OFF    TYPE (ACT=REC)                  
*                                                                               
WRDATE   DS    CL6                 WRITE-OFF    DATE EBCDIC                     
WRDATE2  DS    CL2                 WRITE-OFF    DATE COMPRESSED                 
WRDATEP  DS    CL3                 WRITE-OFF    DATE PACKED                     
*                                                                               
WODATE   DS    XL3                 WRITE-OFF    DATE PACKED                     
WOREF    DS    CL6                 WRITE-OFF    NUMBER                          
*                                                                               
TMSRATE  DS    PL(L'PRTRATE)       RATE                                         
TMSOFF   DS    CL2                 CLIENT  OFFICE                               
TMSCOST  DS    CL14                CLIENT  COSTING   ACCOUNT                    
TMSCOSTN DS    CL36                CLIENT  COSTING   ACCOUNT   NAME             
TMSEFF   DS    PL3                 EFFECTIVE    DATE                            
TMSTYP   DS    XL1                 BILLING TYPE                                 
TMSSTAT2 DS    XL1                 TMS     STATUS                               
TMSSTAT3 DS    XL1                 TMS     STATUS                               
TMSDATE  DS    XL2                 TMS     DATE                                 
*                                                                               
BLDTYPE  DS    X                   TIME    BUILD     INDICATOR                  
POSTTMS  DS    C                   TMS     INDICATOR                            
*                                                                               
CONNAME  DS    CL(L'DLPSCRNM)      JOB     POSTING   CONTRA-ACCT   NAME         
LASTKEY  DS    CL42                                                             
NARRATVE DS    CL150               WRITE-OFF    NARRATIVE                       
NARRLEN  DS    XL1                 LENGTH  OF   NARRATIVE                       
*                                                                               
ELFLAG   DS    X                                                                
DATASW   DS    C                   DATA    TO   WO   OR   WR                    
ANYPOSTS DS    C                   ANYTHING     TO   BE   POSTED  (Y/N)         
OKDRAFTS DS    C                   PROCESS DRAFT     TRANSACTIONS               
*                                                                               
WKMDTL   DS    XL(MDTLNQ)          MEDIA   TRANSFER  ELEMENT                    
SVPRTEL  DS    XL(L'ELEMENT)       SAVE PERSONNEL RATE ELEMENT                  
SVPIDEL  DS    XL(L'ELEMENT)       SAVE PID ELMENT                              
WKTRNL   DS    XL(L'ELEMENT)       TRANSACTION ELEMENT                          
WKTRNL2  DS    XL(TRNLN1Q)         TRANSACTION ELEMENT2                         
*                                                                               
SVDBELM  DS    CL(FFTLN1Q+L'FFTDLEN+L'FFTWORK+L'FFTWAMT)                        
*                                  TABLE OF ACCOUNTS FOR APEEL ELEMEN           
SUBTAB   DS    0D                                                               
         DS    CL(SUBTABL*SUBTABM)                                              
*                                                                               
SUBTABM  EQU   32                  MAXIMUM # OF SUBTAB ENTRIES                  
SUBTABN  DS    X                   ACTUAL # OF SUBTAB ENTRIES                   
*                                                                               
*                                  FIELDS NEEDED FOR CADET                      
TMEHRS   DS    PL4                 HOURS                                        
TMEDATE  DS    CL3                 DATE                                         
TMELV1R  DS    XL4                 1R ACCOUNT HIERARCHY                         
TMELVSJ  DS    XL4                 SJ ACCOUNT HIERARCHY                         
TMEPID   DS    XL2                 PID                                          
*                                                                               
CADETBLK DS    CL(CADETDQ)         CADET BLOCK                                  
*                                                                               
LWSX     DS    0C                                                               
         EJECT ,                                                                
SUBTABD  DSECT                                                                  
SUBACT   DS    0C                                                               
SUBSTA   DS    XL(L'APENSTAT)      STATUS                                       
SUBACC   DS    CL(L'APENACT)       ACCOUNT                                      
SUBTABL  EQU   *-SUBTABD                                                        
         EJECT                                                                  
*ACBILDSECT                                                                     
       ++INCLUDE ACBILDSECT                                                     
         EJECT                                                                  
*ACCADETD                                                                       
       ++INCLUDE ACCADETD                                                       
*ACBILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
         PRINT ON                                                               
         EJECT ,                                                                
         SPACE 1                                                                
*ACBILF8D                                                                       
       ++INCLUDE ACBILF8D                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACBIL08   12/17/12'                                      
         END                                                                    
