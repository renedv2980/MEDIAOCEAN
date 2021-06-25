*          DATA SET ACREPAJ02  AT LEVEL 066 AS OF 05/29/20                      
*PHASE ACAJ02A,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE BMONVAL                                                                
*INCLUDE CONVMOS                                                                
         TITLE 'CREATE POSTING FILE FROM CLIENT TAPE'                           
*----------------------------------------------------------------------         
*        HISTORY                                                                
*----------------------------------------------------------------------         
*        02/24/92                  ALLOW DUPLICATE KEYS IN WORKER FILE          
*                                  AS PER PMURDDNY                              
*        06/28/92                  CHANGE GLFEED TO 61 BYTE RECORD              
*                                  TRNDATE ON DETAIL REC W/DEFAULT IN           
*                                  HEADER                                       
*        12/31/97                  FIXED FOR YEAR 2000 CHANGES                  
*        03/25/02                  RGUP CHANGED P TO XP REPORT PRINTS           
*                                  ON ONE LINE INSTEAD OF CHOPPING              
*        09/12/03                  JSHA ADDED CZ TO AGENCY TABLE                
*----------------------------------------------------------------------         
* ID   LVL DATE    JIRA         DESCRIPTION                                     
* ---- --- ------- ------------ ---------------------------------------         
* CPAT 065 06APR18 <SPEC-19725> G/L UPLOAD-CHECKING THE NEW G/L SETTING         
*                               ON THE COMPANY RECORD.                          
*          06APR18 <SPEC-21272> G/L UPLOAD-CHANGE AAJ TO HONOR MOA LOCK         
*          19APR18 <ITMF-22323> FIX TO HANDLE -VE TRANSACTION AMOUNT            
*          26OCT18 <SPEC-19725> RESTRICTED THE VALIDATION TO UNIT 'G'           
* JSHA 066 29MAY20 <SPEC-46670> ADDING UWGCON TO THE DEFTAB                     
***********************************************************************         
*                                                                               
         EJECT ,                                                                
         PRINT NOGEN                                                            
ACAJ02   CSECT                                                                  
         NMOD1 WORKLN,**ACAJ**,R9,R7                                            
         L     RA,0(R1)                                                         
*                                                                               
         USING ACWORKD,RA                                                       
*                                                                               
         LA    RC,SPACEND                                                       
         USING ACAJD,RC                                                         
*                                                                               
         L     R8,VBIGPRNT                                                      
         USING BIGPRNTD,R8                                                      
*                                                                               
         L     R2,ADMASTC                                                       
         USING MASTD,R2                                                         
*                                                                               
         L     R4,MCBXAREA                                                      
         USING BOXD,R4                                                          
         MVC   BOXWIDTH,=F'198'    SET WIDTH FOR REPORT                         
         DROP  R2,R4                                                            
*                                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         CLI   MODE,REQFRST                                                     
         BNE   XIT                                                              
*                                                                               
         BAS   RE,INIT                                                          
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        READ TAPE RECORDS, PUT TO SORT                                         
*----------------------------------------------------------------------         
         SPACE 1                                                                
TAPE01   DS    0H                                                               
         GET   TAPE,TAPEIO                                                      
         AP    TAPECNT,=P'1'       BUMP COUNTER                                 
*                                                                               
         USING TAPERECD,R6                                                      
*                                                                               
         LA    R6,TAPEIO                                                        
         CLC   TAPEIO(10),=C'9999999999'    OGILVIES TAPE MARK                  
         BE    EOTAPE                                                           
*                                                                               
         CLC   TAPEIO(11),=C'END OF FILE'   SYNCROFILMS TAPE MARK               
         BE    EOTAPE                                                           
*                                                                               
         TM    RUNSTAT,SALTAPE                                                  
         BNO   TAPE10                                                           
         CP    TAPECNT,=P'1'       FIRST RECORD READ                            
         BNE   TAPE10                                                           
         BAS   RE,SAVHEAD          SAVE SALARY TAPE HEADER INFO                 
         B     TAPE01              GET NEXT RECORD                              
*                                                                               
TAPE10   BAS   RE,SETSORT          BUILD SORT RECORD                            
*                                                                               
         BAS   RE,VALREC           VALIDATE RECORD                              
         BAS   RE,MTHLOCK          VALIDATE LOCKED MONTH                        
*                                                                               
         TM    RUNSTAT,SALTAPE                                                  
         BO    TAPEX                                                            
*                                                                               
         CLC   ALPHAID,=C'JW'      JWT USES DEFAULT JOBS                        
         BNE   TAPEX                                                            
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
         LA    R5,SRTREC                                                        
         TM    SRTSTAT,SJERR+BADSTAT+NOACC32  ANYTHING WRONG?                   
         BZ    TAPEX                          NO, CONTINUE                      
*                                                                               
         TM    SRTSTAT1,SVALLGL    IS IT A BAD GL DATE?                         
         JZ    TAPEX               NO, CONTINUE                                 
*                                                                               
TAPE15   TM    JWSTAT,GOODCLI      AT LEAST A GOOD CLIENT                       
         BNO   TAPE20              NO, FORGET ABOUT IT                          
*                                                                               
         BAS   RE,JWSETDEF         FIND DEFAULT JOB                             
         CLI   SRTDEF,X'FF'        FOUND?                                       
         BNE   TAPE20              NO                                           
*                                                                               
         BAS   RE,VALPR            VALIDATE DEFAULT JOB                         
*                                                                               
         TM    SRTSTAT,SJERR+BADSTAT+NOACC32  STILL WRONG?                      
         BZ    TAPEX                          NO, CONTINUE                      
*                                                                               
         TM    SRTSTAT1,SVALLGL    IS IT A BAD GL DATE?                         
         JZ    TAPEX               NO, CONTINUE                                 
*                                                                               
TAPE20   BAS   RE,JWREST           RESTORE ORIGINAL ACCOUNT                     
*                                  WRITE ERROR IN NARRATIVE                     
*                                                                               
TAPEX    BAS   RE,PUTSORT                                                       
         AP    TAPECNT,=P'1'                                                    
         B     TAPE01                                                           
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GET RECORDS FROM SORT, VALIDATE ACCOUNTS, CODES                        
*----------------------------------------------------------------------         
         SPACE 1                                                                
EOTAPE   DS    0H                  END OF TAPE, PROCESS RECS                    
         CLOSE TAPE                                                             
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    RUNSTAT,SALTAPE     SALARY FEED TAPE                             
         BNO   EO30                NO                                           
         ZAP   DUB,SALDRS                                                       
         SP    DUB,SALCRS          DEBITS = CREDITS                             
         BNZ   EO20                NO                                           
*                                                                               
         CP    DEFCNT,MAXDEF       TOO MANY ERRORS                              
         BH    EO20                YES                                          
*                                                                               
*        TURN OFF ANY ERRORS WHICH CAN BE FIXED WITH DEFAULT ACCOUNTS           
*                                                                               
         NI    SALSTAT,X'FF'-BADACC-BADPOST-BAD2D-BAD28-BADCAC-BADOFF-BX        
               ADDATE                                                           
*                                                                               
         OC    SALSTAT,SALSTAT     ANY ERRORS AT ALL ON TAPE                    
         BZ    EO30                NO POST THEM                                 
*                                                                               
EO20     BAS   RE,DONTPOST                                                      
*                                                                               
EO30     ZAP   DRTOTAL,=P'0'       INIT POSTING TOTALS                          
         ZAP   CRTOTAL,=P'0'                                                    
         ZAP   POSTCNT,=P'0'                                                    
         ZAP   DEFCNT,=P'0'        REINIT FOR REPORT                            
*                                                                               
GETREC   GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R4,DMCB+4           GET ADDRESS OF RECORD                        
         LTR   R4,R4               ANYTHING THERE?                              
         BZ    NOMORECS            REPORT ON THE BAD RECS                       
*                                                                               
         LA    R5,SRTLN                                                         
         CLI   QOPT1,C'Y'          SALARY TAPE                                  
         BNE   *+8                                                              
         LA    R5,SRTLN1           YES, USE SHORT SORT                          
*                                                                               
         LR    R3,R5                          ODD REG FOR R1                    
         LA    R2,SRTREC                      ADDRESS FOR MVCL                  
         MVCL  R2,R4                          MOVE RECORD FROM SORT             
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
         LA    R5,SRTREC                                                        
*                                                                               
         CLI   QOPT6,C'Y'                     DUMP REQUESTED                    
         BNE   *+8                                                              
         BAS   RE,DMPGET                      YES, DUMP TAPE RECORD             
*                                                                               
         TM    RUNSTAT,SALTAPE     SALARY FEED TAPE                             
         BNO   GETR10              NO                                           
*                                                                               
         BAS   RE,VALSAL           GET NAMES                                    
*                                                                               
         BAS   RE,SALREP           REPORT RECORD                                
*                                                                               
         TM    SRTSTAT1,SVALLGL    IS IT A BAD GL DATE?                         
         JNZ   GETR06              NO                                           
*                                                                               
GETR04   TM    SRTSTAT,X'FF'-BADDATE  ANY UN FIXABLE ERRORS                     
         BZ    GETR05                 NO                                        
*                                                                               
         BAS   RE,SAVEDEF          SAVE TOTALS FOR A DEFAULT POSTING            
         B     GETR06              BUT DON'T POST                               
*                                                                               
GETR05   BAS   RE,POSTSAL          POST SAL RECORD                              
*                                                                               
GETR06   B     GETRX               GET NEXT RECORD                              
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
GETR10   LA    R5,SRTREC           VENDOR TAPE PROCESSING                       
*                                                                               
         MVC   PRDNUM(1),RCCOMPFL             REBUILD PRDNUM                    
         MVC   PRDNUM+1(L'SRTACCT),SRTACCT                                      
*                                                                               
         GOTO1 VALACC,DMCB,SRTCACT    VAL VENDOR                                
         MVC   VENNAME,ACCTNAME                                                 
         MVC   VENNUM,ACCTNUM                                                   
*                                                                               
         CLI   ACCTERR,0                                                        
         BE    GETR30                                                           
*                                                                               
         TM    ACCTERR,NOTFOUND                                                 
         BZ    *+8                                                              
         OI    SRTSTAT,SVERR                                                    
*                                                                               
         TM    ACCTERR,NOPOST                                                   
         BZ    *+8                                                              
         OI    SRTSTAT,NOVEN32                                                  
*                                                                               
GETR30   BAS   RE,CODES            PROCESS WORKCODES ON RECORD                  
*                                                                               
GETRX    B     GETREC                         READ NEXT RECORD                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        INITIALIZE WORKING STORAGE, ADSORT, OPEN TAPE                          
*----------------------------------------------------------------------         
         SPACE 1                                                                
INIT     NTR1                                                                   
         MVI   RCSUBPRG,0                                                       
         ZAP   SVGLMOA,=P'0'                                                    
         XC    SVCMPNY,SVCMPNY                                                  
         BAS   RE,GETBUFF                                                       
*                                                                               
         MVC   PREVACCT(L'PREVACCT),XSPACES   INIT FOR REPORT                   
*                                                                               
         LA    RE,PKBUCKS                                                       
         LA    R0,NUMBUCKS                                                      
*                                                                               
INIT20   ZAP   0(BUCKLN,RE),=P'0'                                               
         LA    RE,BUCKLN(RE)                                                    
         BCT   R0,INIT20                                                        
*                                                                               
         MVC   VENNAME(36),XSPACES            NAMES                             
*                                                                               
*              GET COMPANY NAME FOR HEADER                                      
*                                                                               
         USING ACCRECD,R4                                                       
*                                                                               
         LA    R4,SVKEY            SPACE TO BUILD KEY                           
         MVC   ACCKEY,XSPACES      SET UP ACCKEY FOR SV READ                    
         MVC   ACCKEY(1),RCCOMPFL       HEX COMP FROM MONAC                     
*                                                                               
         BAS   RE,READACC                                                       
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,RECORD                                                        
         MVC   SVCMPNY,ACTKCPY            COMPANY CODE                          
         BAS   RE,GETNAME                                                       
*                                                                               
         MVC   SVCOMPNM,WORK              COMPANY NAME                          
*                                                                               
         USING CPYELD,R4                                                        
*                                                                               
         LA    R4,RECORD                                                        
         MVI   ELCODE,CPYELQ                                                    
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   PRODLEDG,CPYPROD                                                 
         MVC   COMPSTAT,CPYSTAT1                                                
         MVC   DEPTLEN,CPYDEPTL                                                 
         MVI   OFFLEN,1                                                         
*                                                                               
         TM    CPYSTAT4,CPYSOFF2   2 BYTE OFFICE                                
         BZ    *+8                                                              
         MVI   OFFLEN,2                                                         
*                                                                               
         TM    CPYSTAT8,CPYNEWGL          NEW GL SYSTEM IN USE ?                
         JZ    INIT30                                                           
         MVC   SVGLMOA,CPYGLMOA           MOA                                   
*                                                                               
*              SET UP WORKER FILE                                               
INIT30   XC    ID,ID                                                            
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AAJ'                WORKER FILE ID                    
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
         OI    ID+13,X'01'                    ALLOW DUPLICATE KEYS              
         MVC   COMMAND,=CL6'OPEN'                                               
         BAS   RE,FILE                        OPEN POSTING FILE                 
*                                                                               
         LA    R1,SRTKLN           SORT KEY LENGTH                              
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
*                                                                               
         LA    R1,SRTLN            SORT RECORD LENGTH                           
*                                                                               
         CLI   QOPT1,C'Y'          SALARY TAPE                                  
         BNE   *+8                                                              
         LA    R1,SRTLN1           YES, USE SHORT SORT                          
*                                                                               
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
*                                                                               
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
*                                                                               
         CLI   QOPT1,C'Y'          SALARY TAPE                                  
         BNE   INIT60              NO USE DCB MACRO AS DEFINED                  
*                                                                               
         LA    R1,TAPE             POINTS TO OUTPUT TAPE DCB                    
*                                                                               
         USING IHADCB,R1           USING DCB DSECT                              
*                                                                               
         MVC   DCBLRECL,SALLRECL   FORCES CORRECT REC-LENGTH                    
         MVC   DCBBLKSI,SALBLK        "      "    BLK    "                      
*                                                                               
         CLC   ALPHAID,=C'CE'      CECOR                                        
         BNE   *+10                SPECIAL BLKSIZE FOR THEM                     
         MVC   DCBBLKSI,CEBLK                                                   
*                                                                               
INIT60   OPEN  (TAPE,(INPUT))                 OPEN TAPE                         
*                                                                               
*                                                                               
         L     RE,ABAD                        SAVE ADDRESS                      
         ST    RE,BADADDR                     OF THE TABLE AREA                 
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY3)   PACK TODAYS DATE                  
         GOTO1 DATCON,DMCB,(5,0),(2,TODAY2)   COMPRESS TODAYS DATE              
         XC    RUNSTAT,RUNSTAT                                                  
         XC    SALSTAT,SALSTAT                                                  
         OI    RUNSTAT,DATEMYD                                                  
         CLI   QOPT1,C'Y'          O+M TYPE 45 RUN                              
         BNE   INITX                                                            
         OI    RUNSTAT,DATEMMM                                                  
         OI    RUNSTAT,SALTAPE                                                  
         NI    RUNSTAT,X'FF'-DATEMYD                                            
*                                                                               
INITX    B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRE SORT VALIDATION                                                    
*        IF TYPE 1 TAPE, PRE VALIDATE SJ ACCOUNT, TYPE 45 VAL ALL               
*----------------------------------------------------------------------         
         SPACE 1                                                                
VALREC   NTR1                                                                   
         TM    RUNSTAT,SALTAPE                                                  
         BO    VALR10                                                           
         BAS   RE,VALPR            VALIDATE PRODUCTION FOR OFFICE               
         B     VALRX                                                            
*                                                                               
VALR10   BAS   RE,VALSAL                                                        
VALRX    B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        BUILD SRTREC FROM TAPE DATA                                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING TAPERECD,R6                                                      
         USING SRTD,R5                                                          
         SPACE 1                                                                
SETSORT  NTR1                                                                   
         LA    R6,TAPEIO                                                        
         LA    R5,SRTREC                                                        
*                                                                               
         LA    RE,SRTREC           CLEAR OUT SORT RECORD                        
         LA    RF,SRTLN                                                         
         XCEF                                                                   
*                                                                               
         TM    RUNSTAT,SALTAPE                                                  
         BNO   SETS10                                                           
         BAS   RE,SETSAL                                                        
         B     XIT                                                              
*                                                                               
SETS10   MVC   SRTKJOB,TPSJCPJ                                                  
         BAS   RE,VALDATE                                                       
*                                                                               
         TM    SRTSTAT,BADDATE                                                  
         BNO   *+10                                                             
         MVC   SRTKYMD,YMD                                                      
*                                                                               
         TM    SRTSTAT1,SVGLDAT    IS IT A BAD GL DATE?                         
         JNO   *+10                                                             
         MVC   SRTKYMD,YMD                                                      
*                                                                               
SETS15   MVC   SRTKREF,TPREF                                                    
*                                  BUILD SORT DATA RECORD                       
         MVC   SRTREF,TPREF                                                     
         MVC   SRTDATE,YMD                                                      
         MVI   SRTTYPE,X'1'        ASSUME TYPE 1                                
         BAS   RE,CALCMOS                                                       
         MVC   SRTMOS,SVMOS                                                     
*                                                                               
*        NOTE: GET OFFICE FROM THE VALPR ROUTINE                                
*                                                                               
         MVC   SRTBR,XSPACES                                                    
         MVC   SRTUL,PRODLEDG                                                   
         MVC   SRTACT,TPSJCPJ                                                   
         MVC   SRTCACT,TPCACT                                                   
*                                                                               
         MVC   SRTCACT(2),=C'SV'                                                
         CLC   TPUNLED,XSPACES             UNIT LEDGER SUPPLIED                 
         BE    *+10                           NO, USE DEFAULT                   
         MVC   SRTCACT(2),TPUNLED           GET U/L OFF OF THE TAPE             
         MVC   SRTCACT+2(12),TPSUPLR          SUPPLIER (VENDOR) - TAPE          
*                                                                               
         CLC   TPSUPLR(6),=C'162610'           TRAVEL ACCOUNT                   
         BNE   SETS20                                                           
         MVC   SRTCACT,=CL14'SCP162610NY'                                       
         OI    SRTFLAG,TRAVEL                 T FOR TRAVEL A/C RUN              
         B     SETS30                                                           
*                                                                               
SETS20   CLC   TPSUPLR(5),=C'16211'           TRAVEL ACCOUNT TWO                
         BNE   SETS30                                                           
         MVC   SRTCACT,=CL14'SCP162110'                                         
         OI    SRTFLAG,TRAVEL                 T FOR TRAVEL A/C RUN              
*                                                                               
SETS30   CLC   SRTCACT(2),=C'SC'            CONTRA POSTING TO SC?               
         BNE   *+8                                                              
         MVI   SRTTYPE,X'03'                 THEN ITS A TYPE 3                  
*                                                                               
         CLC   SRTCACT(2),=C'SR'            CONTRA POSTING TO SR?               
         BNE   *+8                                                              
         MVI   SRTTYPE,X'03'                 THEN ITS A TYPE 3                  
*                                                                               
         MVC   SRTNARR(L'TPNARR),TPNARR                                         
         OC    SRTNARR,XSPACES     CONVERT NARRATIVE TO UPPER CASE              
         MVC   SRTSPARE,TPSPARE                                                 
*                                                                               
         LA    R3,TPCODES                                                       
         LA    R2,SRTCODES                                                      
*                                                                               
         USING CODED,R3                                                         
         USING SRCOD,R2                                                         
*                                                                               
         LA    R0,8                NUMBER OF CODE FIELDS IN TPREC               
*                                                                               
SETS50   MVC   SRCCODE,COCODE                                                   
         ZAP   SRCAMNT,=P'0'                                                    
*                                                                               
         MVC   WORK(10),=C'0000000000'                                          
         MVZ   WORK(9),COAMOUNT                                                 
*                                                                               
         CLC   WORK(10),=C'0000000000'   ASSURE NUMERIC VALUES                  
         BNE   *+10                                                             
         PACK  SRCAMNT,COAMOUNT                                                 
*                                                                               
         MVI   SRCCOMM,C'C'                                                     
         CH    R0,=H'4'                                                         
         BH    *+8                                                              
         MVI   SRCCOMM,C'N'                                                     
*                                                                               
         LA    R2,SRCOLN(R2)                                                    
         LA    R3,CODEDLEN(R3)                                                  
         BCT   R0,SETS50                                                        
*                                                                               
         B     XIT                                                              
         DROP  R6,R2,R3                                                         
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SAVE THE SALARY TAPE HEADER INFO IN TAPEIO                             
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SALRECD,R6                                                       
         SPACE 1                                                                
SAVHEAD  NTR1                                                                   
         LA    R6,TAPEIO                                                        
*                                                                               
         CLC   SALORIG,ALPHAID                                                  
         BE    SAVH20                                                           
         BAS   RE,ORIGERR          PRINT ORIGIN ERROR                           
*                                                                               
SAVH20   MVC   SAVREC(SALHDLN),SALHEAD                                          
*                                                                               
         XC    DEFYMD,DEFYMD                                                    
         MVC   WORK(14),XSPACES                                                 
         MVC   WORK(3),SALHMMM                                                  
         MVI   WORK+3,C'/'                                                      
         MVC   WORK+4(2),SALHDD                                                 
         MVI   WORK+6,C'/'                                                      
         MVC   WORK+7(2),SALHYY                                                 
         CLI   WORK+7,C'9'         IS   THE  YEAR GREATER THAN 1999 ?           
         BNH   SAVH30              NO,  SKIP                                    
         ZIC   RE,WORK+7           CONVERT   X'FA' -> X'F0'                     
         SH    RE,=H'10'                     X'FB' -> X'F1'                     
         STC   RE,WORK+7                     ...                                
*                                                                               
SAVH30   GOTO1 DATVAL,DMCB,(0,WORK),WORK+10                                     
         CLI   DMCB+3,0                       IS DATE VALID                     
         BE    SAVH60              NO, NO DEFAULT DATE FOR RUN                  
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,DEFYMD)   CON TO YMD FORMAT           
*                                                                               
SAVH60   CLI   SALBM,C'9'          FIRST CHARACTER MUST BE 0-9                  
         BH    SAVH80                                                           
         CLI   SALBM,C'0'                                                       
         BL    SAVH80                                                           
         CLI   SALBM+1,C'9'        SECOND CHARACTER MUST BE 1-9                 
         BH    SAVH80                                                           
         CLI   SALBM+1,C'1'                                                     
         BNL   SAVH90              SECOND CHAR IS 1-9                           
*                                                                               
         CLI   SALBM+1,C'C'        CHECK FOR A-C                                
         BH    SAVH80                                                           
         CLI   SALBM+1,C'A'                                                     
         BNL   SAVH90                                                           
*                                                                               
SAVH80   OI    SALSTAT2,BADBM                                                   
         MVC   HDBR,=C'*ERROR'                                                  
         B     SAVHX                                                            
SAVH90   MVC   HDBR(2),SALBM                                                    
         MVC   HDBR+2(4),SALBR                                                  
*                                                                               
SAVHX    BAS   RE,VALDEFLT         SET AND VALIDATE DEFAULTS                    
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SET SORTREC FROM OMNY SALARY INTERFACE TAPE                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SALRECD,R6                                                       
         SPACE 1                                                                
SETSAL   NTR1                                                                   
         LA    R5,SRTREC                                                        
         XC    SRTKEY(SRTKLN),SRTKEY                                            
         MVC   SRTKSEQ,TAPECNT     KEEP IN TAPE ORDER                           
*                                                                               
         LA    R6,SAVREC           MOVE SAVED HEADER INFO TO SRTREC             
         MVC   SRTNARR,XSPACES                                                  
         MVC   SRTNARR(L'SALNARR),SALNARR                                       
*                                                                               
         OC    SRTNARR,XSPACES     CONVERT NARRATIVE TO UPPER CASE              
         MVC   SRTMOS,SALBM                                                     
         CLC   SALBM,XSPACES       DID THEY PASS A BATCH MONTH                  
         BH    SETSA20             YES                                          
         BAS   RE,CALCMOS          NO, GET IT FROM TRAN DATE                    
         MVC   SALBM,SVMOS                                                      
*                                                                               
SETSA20  MVC   SRTREF,SALREF                                                    
*                                                                               
         LA    R6,TAPEIO           GET DETAIL DATA                              
*                                                                               
         MVC   WORK(14),XSPACES    VAL A DATE                                   
         MVC   WORK(2),SALMM                                                    
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),SALDD                                                  
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),SALYY                                                  
         CLI   WORK+6,C'9'         IS   THE  YEAR GREATER THAN 1999 ?           
         BNH   SETSA25             NO,  SKIP                                    
         ZIC   RE,WORK+6           CONVERT   X'FA' -> X'F0'                     
         SH    RE,=H'10'                     X'FB' -> X'F1'                     
         STC   RE,WORK+6                     ...                                
*                                                                               
SETSA25  GOTO1 DATVAL,DMCB,(0,WORK),WORK+10                                     
         CLI   DMCB+3,0                       IS DATE VALID                     
         BNE   SETSA30                        YES, LENGTH RETURNED              
*                                                                               
         OC    DEFYMD,DEFYMD       DO I HAVE A DEFAULT YMD                      
         BNZ   *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
*                                  SINCE DEFYMD IS PACKED IN SAVH30,            
         MVC   YMD,DEFYMD          USE DEFAULT FROM HEADER REC                  
*                                                                               
         CLI   QOPT2,C'Y'          SUPPRESS BAD DATE ERROR                      
         BE    SETSA40                                                          
*                                                                               
         CLI   PROGPROF+2,C'Y'     SUPPRESS BAD DATE ERROR                      
         BE    SETSA40             YES                                          
*                                                                               
         OI    SRTSTAT,BADDATE     SET FLAG FOR REPORT                          
         B     SETSA40                                                          
*                                                                               
SETSA30  GOTO1 DATCON,DMCB,(0,WORK+10),(1,YMD)   CON TO YMD FORMAT              
*                                                                               
SETSA40  MVC   SRTKACCT,SALACC                                                  
         MVC   SRTKYMD,YMD                                                      
         MVC   SRTKREF,SALREF                                                   
*                                                                               
         MVC   SRTDATE,YMD                                                      
         MVC   SRTBR,SALBR                                                      
         MVC   SRTACCT,SALACC                                                   
         MVC   SRTCACT,SALCACC                                                  
         MVI   SRTTYPE,45                                                       
         MVC   SRTDC,SALDC         DEBIT OR CREDIT                              
         MVC   SRTOFF,SALOFF                                                    
         MVC   SRTDEPT,XSPACES                                                  
         MVC   SRTDEPT(L'SALDEPT),SALDEPT                                       
         PACK  SRTAMNT,SALAMNT                                                  
         CLI   SALAMNT,C'-'                                                     
         BNE   *+10                                                             
         MP    SRTAMNT,=P'-1'                                                   
*                                                                               
         MVI   SRTNARR+L'SALNARR,C'-'                                           
         MVC   SRTNARR+L'SALNARR+2(L'SALCYCLE),SALCYCLE                         
*                                                                               
         LA    R2,SALDRS           KEEP TOTAL OF DR'S AND CR'S ON TAPE          
         CLI   SRTDC,C'D'                                                       
         BE    *+8                                                              
         LA    R2,SALCRS                                                        
         AP    0(BUCKLN,R2),SRTAMNT                                             
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PUT SRTREC TO TAPE                                                     
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
PUTSORT  NTR1                                                                   
         LA    R5,SRTREC                                                        
         CLI   PROGPROF+1,C'Y'     SORT REPORT BY OFFICE                        
         BNE   PUTS20                                                           
         MVC   SRTKOFF,SVOFFICE    WRITE OFFICE TO SORT KEY                     
*                                                                               
PUTS20   GOTO1 ADSORTER,DMCB,=C'PUT',SRTREC                                     
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        VALIDATE AN PRODUCTION ACCOUNT                                         
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING ACCRECD,R4                                                       
         USING SRTD,R5                                                          
         SPACE 1                                                                
VALPR    NTR1                                                                   
         LA    R5,SRTREC                                                        
         LA    R4,SVKEY                       RESET R4 TO KEY                   
         MVC   ACCKEY,XSPACES                                                   
         MVC   ACCKEY(1),RCCOMPFL             HEX COMP FROM MONAC               
         MVC   ACCKEY+1(2),PRODLEDG           UNIT-LEDGER                       
         XC    JWSTAT,JWSTAT                  JW NEEDS TO KNOW IF THE           
*                                             CLIENT IS GOOD                    
         CLC   SRTCLI,XSPACES                                                   
         BE    VALPRERR                                                         
*                                                                               
         MVC   ACCKEY+3(3),SRTCLI             CLIENT FROM TAPE                  
         BAS   RE,READACC                     DATAMGR CALL                      
         CLI   DMCB+8,0                       RECORD FOUND?                     
         BNE   VALPRERR            NO, SET ERROR FLAG                           
*                                                                               
         LA    R4,RECORD                                                        
         BAS   RE,GETNAME                     R4 POINTS TO ACC RECORD           
*                                                                               
         MVC   SRTCLNM,WORK                   SAVE ACCOUNT NAME                 
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                      SEE IF CLIENT HAS A 24             
         BE    *+6                            ELEMENT - DIE IF IT               
         DC    H'0'                           DOES NOT                          
*                                                                               
         USING PPRELD,R4                      PROFILE ELEMENT DSECT(24)         
*                                                                               
         MVC   SVOFFICE,PPRGAOFF                                                
         OI    JWSTAT,GOODCLI                 JW WANTS AT LEAST THIS            
*                                                                               
*              GET PRODUCT RECORD FOR OFFICE                                    
*                                                                               
         USING ACCRECD,R4                                                       
*                                                                               
         LA    R4,SVKEY                       RESET R4 TO KEY                   
         CLC   SRTPRO,XSPACES                                                   
         BE    VALPRERR                                                         
         MVC   ACCKEY+6(3),SRTPRO             PRODUCT FROM TAPE                 
         BAS   RE,READACC                     R4 POINTS TO ACC RECORD           
         CLI   DMCB+8,0                       RECORD FOUND?                     
         BNE   VALPRERR            NO                                           
*                                                                               
         LA    R4,RECORD         POINT TO WHERE DATAMGR PUT THE RECORD          
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                      SEE IF PRODUCT HAS A 24            
         BNE   VALPR01                       NO, DONT WORRY ABOUT IT            
*                                                                               
         USING PPRELD,R4                      PROFILE ELEMENT DSECT(24)         
*                                                                               
         CLI   PPRGAOFF,X'40'                 BETTER THAN A SPACE?              
         BNH   VALPR01                       NO, DONT WORRY ABOUT IT            
         MVC   SVOFFICE,PPRGAOFF             SAVE THE OFFICE                    
*                                                                               
*              GET JOB RECORD                                                   
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
VALPR01  LA    R5,SRTREC                                                        
         MVC   SRTOFF,SVOFFICE     SAVE COMPOSIT OFFICE IN SORT REC             
*                                                                               
         USING ACCRECD,R4                                                       
*                                                                               
         LA    R4,SVKEY                       RESET R4 TO KEY                   
         CLC   SRTJOB,XSPACES                                                   
         BE    VALPRERR                                                         
         MVC   ACCKEY+1(14),SRTACCT                                             
         BAS   RE,READACC                     R4 POINTS TO ACC RECORD           
         CLI   DMCB+8,0                       RECORD FOUND?                     
         BNE   VALPRERR            NO                                           
*                                                                               
         LA    R4,RECORD                                                        
         BAS   RE,GETNAME                                                       
         MVC   SRTNAME(36),WORK                                                 
*                                                                               
         MVI   ELCODE,X'32'                                                     
         LA    R4,RECORD                                                        
         BAS   RE,GETEL                      SEE IF ACCOUNT HAS A 32            
         BE    *+8                            ELEMENT -                         
         OI    SRTSTAT,NOACC32                                                  
*                                                                               
         MVI   ELCODE,X'30'                  CHECK STATUS, FLAG CLOSED          
         LA    R4,RECORD                      OR LOCKED ACCOUNTS                
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                          DIE IF THERE IS NO STAT EL         
*                                                                               
         USING RSTELD,R4                                                        
*                                                                               
         TM    RSTSTAT,RSTSACIC+RSTSACIL     X'20'-LOCKED, X'40'-CLOSED         
         BZ    *+8                                                              
         OI    SRTSTAT,BADSTAT                SET STATUS ERROR                  
*                                                                               
         MVI   ELCODE,X'26'        CHECK JOB ELEMENT                            
         LA    R4,RECORD           FLAG XJOBS                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                          DIE IF THERE IS NO STAT EL         
*                                                                               
         USING JOBELD,R4                                                        
*                                                                               
         TM    JOBSTA1,JOBSXJOB                                                 
         BO    VALPRERR                                                         
*                                                                               
VALPRX   B     XIT                                                              
*                                                                               
VALPRERR OI    SRTSTAT,SJERR                  SET ERROR FLAG                    
         B     VALPRX                                                           
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        IF A BAD JOB WAS SENT ON THE TAPE, ATTEMPT TO FIND A                   
*        DEFAULT JOB BASED ON THE OFFICE                                        
*        ASSUMES THE CLIENT ON THE TAPE WAS GOOD AND SVOFFICE HAS BEEN          
*        SET                                                                    
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
JWSETDEF NTR1                                                                   
         LA    R5,SRTREC                                                        
         LAY   R3,JWDEF            TABLE LOCATION                               
         LAY   R2,JWNUM            NUMBER IN TABLE                              
*                                                                               
JWSET10  CLC   SVOFFICE,0(R3)      THIS JOB DEFAULT FOR OFF                     
         BNE   JWSET20             NO, LOOP AGAIN                               
*                                                                               
         MVC   SRTSPARE+2(12),SRTACT      SAVE ORIGINAL JOB                     
         MVC   SRTACT,2(R3)               POST TO  DEFAULT JOB                  
         NI    SRTSTAT,X'FF'-SJERR-NOACC32-BADSTAT TURN OFF ERROR BITS          
         OI    SRTFLAG,DEFAULT                                                  
         MVI   SRTDEF,X'FF'        SORT DEFAULTS TO END                         
         B     JWSETX              RECHECK C,P,J, GET NEW                       
*                                                                               
JWSET20  LA    R3,L'JWDEF(R3)      BUMP TO NEXT TABLE ENTRY                     
         BCT   R2,JWSET10                                                       
JWSETX   B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        EITHER A DEFAULT JOB HAS BEEN FOUND IN ERROR OR THE CLIENT             
*        WAS NO GOOD. IN EITHER CASE PRINT THE ORIGIN OF THE TAPE               
*        RECORD IN THE NARRATIVE                                                
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
JWREST   NTR1                                                                   
         LA    R5,SRTREC                                                        
         CLI   SRTDEF,X'FF'        DEFAULT IN ERROR                             
         BNE   *+10                                                             
         MVC   SRTACT,SRTSPARE+2   RESTORE CLI,PRO,JOB                          
*                                                                               
         MVC   SRTNARR,XSPACES                                                  
         MVC   SRTNARR(13),=C'RECORD ORIGIN'                                    
         MVC   SVOFFICE,SRTSPARE                                                
         LAY   R3,JWOFFTAB         TABLE LOCATION                               
         LAY   R2,JWOFFNUM         NUMBER IN TABLE                              
*                                                                               
JWREST10 CLC   0(2,R3),SRTSPARE    JW PUTS ORIGIN IN CC 170                     
         BNE   JWREST20                                                         
         MVC   SRTNARR+14(3),1(R3)                                              
         B     JWREST30                                                         
*                                                                               
JWREST20 LA    R3,L'JWOFFTAB(R3)                                                
         BCT   R2,JWREST10                                                      
*                                                                               
JWREST30 MVC   SRTNARR+14(1),SRTSPARE                                           
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        VALIDATE DEFAULT ACCOUNT/OFFICE USER                                   
*----------------------------------------------------------------------         
         SPACE 1                                                                
VALDEFLT NTR1                                                                   
         USING DEFTABD,R3                                                       
         LAY   R3,DEFTAB                                                        
         LAY   R0,DEFTABN                                                       
VALD010  CLC   ALPHAID,DEFTALP             MATCH ON ALPHA                       
         BE    VALD030                                                          
VALD020  LA    R3,DEFTLEN(R3)                                                   
         BCT   R0,VALD010                                                       
         LAY   R5,DEFENTRY                 SET OM AS DEFAULT                    
*                                                                               
VALD030  DS    0H                                                               
         GOTO1 VALACC,DMCB,DEFTACC                                              
         CLI   ACCTERR,0                   ERROR?                               
         BE    *+6                         NO                                   
         DC    H'0'                                                             
         MVC   DEFACC,ACCTNUM                                                   
         MVC   DEFACCN,ACCTNAME                                                 
*                                                                               
         GOTO1 VALACC,DMCB,DEFTCON                                              
         TM    ACCTERR,NOTFOUND+NOPOST                                          
         BZ    *+6                 NO                                           
         DC    H'0'                                                             
         MVC   DEFCAC,ACCTNUM                                                   
         MVC   DEFCACN,ACCTNAME                                                 
*                                                                               
         MVC   SVOFFICE,DEFTOFF                                                 
         BAS   RE,VALOFF                                                        
         BNE   VALD020                                                          
         MVC   DEFOFF,SVOFFICE                                                  
*                                                                               
         CP    DEFTMAX,=P'0'                                                    
         BE    XIT                                                              
         ZAP   MAXDEF,DEFTMAX      MAX DEFAULT POSTINGS                         
         B     XIT                                                              
*                                                                               
         DROP  R3                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        IF THERE WERE ERRORS IN POSTING ACCOUNTS                               
*        ADJUST BALANCE                                                         
*        NOTE: IN CALLING MAKEREC IT IS ASSUMED THAT THE LAST SORT REC          
*              READ IS STILL IN SRTREC AND THAT THE FOLLOWING INFO IS           
*              VALID FOR THIS DEFAULT POSTING                                   
*                                  TRNDATE                                      
*                                  TRNREF                                       
*                                  TRNBREF                                      
*                                  TRNMOS                                       
*                                  TRNTYPE                                      
*                                  TRNNARR                                      
*----------------------------------------------------------------------         
         SPACE 1                                                                
SETDEF   NTR1                                                                   
         MVC   POSTACC,DEFACC                                                   
*                                                                               
         CLC   ALPHAID,=C'H7'      MINDSHARE                                    
         BNE   *+10                                                             
         MVC   POSTCAC,DEFCAC      USE DEFAULT CONTRA FOR MINDSHARE             
*                                                                               
*        MVC   POSTCAC,DEFCAC      SOME REASON STARRED OUT LEV22 FOR OA         
         MVC   POSTCACN,DEFCACN                                                 
*                                                                               
         MVC   POSTOFF,DEFOFF                                                   
*                                                                               
         MVI   DEPSW,C'N'                                                       
*                                                                               
         USING SRTD,R5             TURN OFF ERRORS FOR STUFF I FIXED            
*                                                                               
         LA    R5,SRTREC                                                        
         NI    SRTSTAT,X'FF'-BADACC-BADPOST-BAD2D-BAD28-BADCAC-BADOFF           
*                                                                               
         CP    DEFDRS,=P'0'        ANY DEFAULT DEBITS TO POST                   
         BE    SETD50              NO                                           
         ZAP   POSTCASH,DEFDRS                                                  
         XC    POSTSTAT,POSTSTAT                                                
         XC    POSTST,POSTST                                                    
         OI    POSTST,TRNSDR                                                    
         BAS   RE,MAKEREC          MAKE DEFAULT DEBIT POSTING                   
         BAS   RE,POSTIT                                                        
*                                                                               
SETD50   CP    DEFCRS,=P'0'        ANY DEFAULT CREDITS TO POST                  
         BE    SETDX               NO                                           
         ZAP   POSTCASH,DEFCRS                                                  
         XC    POSTSTAT,POSTSTAT                                                
         XC    POSTST,POSTST                                                    
         BAS   RE,MAKEREC          MAKE DEFAULT CREDIT POSTINGS                 
         BAS   RE,POSTIT                                                        
*                                                                               
SETDX    B     XIT                                                              
*                                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        THERE WAS AN ERROR IN A POSTING ACCOUNT                                
*       SAVE THE TOTALS                                                         
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
SAVEDEF  NTR1                                                                   
         LA    R5,SRTREC                                                        
         LA    R2,DEFDRS           KEEP TOTAL OF DR'S AND CR'S ON TAPE          
         CLI   SRTDC,C'D'                                                       
         BE    *+8                                                              
         LA    R2,DEFCRS                                                        
         AP    0(BUCKLN,R2),SRTAMNT                                             
         B     XIT                                                              
*                                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        VALIDATE A 14 BYTE ACCOUNT SENT IN P1                                  
*        RETURNS, IN ACCTERR, 00 IF OK, NOTFOUND OR NOPOST                      
*        IF OK, RETURN 15 BYTE ACCOUNT IN ACCTNUM, NAME IN ACCTNAME             
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING ACCRECD,R4                                                       
         USING SRTD,R5                                                          
         SPACE 1                                                                
VALACC   NTR1                                                                   
         MVC   ACCTNUM,XSPACES                                                  
         MVC   ACCTNAME,XSPACES                                                 
         MVI   ACCTERR,0                                                        
         MVI   DEPSW,C'N'                                                       
*                                                                               
         L     R2,P1                                                            
         CLC   0(14,R2),XSPACES    ANYTHING HERE                                
         BNH   VALA20              NO, INVALID                                  
*                                                                               
         LA    R4,SVKEY            SPACE TO BUILD KEY                           
         MVC   ACCKEY,XSPACES      SET UP ACCKEY FOR SV READ                    
         MVC   ACCKEY(1),RCCOMPFL       HEX COMP FROM MONAC                     
         MVC   ACCKEY+1(14),0(R2)                                               
*                                                                               
         MVC   ACCTNUM,ACCKEY      ALWAYS RETURN ACCOUNT                        
*                                                                               
         BAS   RE,READACC          R4 POINTS TO ACC RECORD                      
         CLI   DMCB+8,0                                                         
         BE    VALAC40                                                          
*                                                                               
VALA20   OI    ACCTERR,NOTFOUND                                                 
         B     VALACX                                                           
*                                                                               
VALAC40  DS    0H                                                               
         LA    R4,RECORD                                                        
         BAS   RE,GETNAME                                                       
         MVC   ACCTNAME,WORK                                                    
*                                                                               
         LA    R4,RECORD                                                        
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL            GET STATUS ELEMENT                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSTELD,R4                                                        
*                                                                               
         TM    RSTSTAT,RSTSACIC+RSTSACIL     X'20'-LOCKED, X'40'-CLOSED         
         BZ    *+8                                                              
         OI    ACCTERR,LOCKCLOS                                                 
         TM    RSTSTAT,RSTSEADD    DEMAND DEPARTMENT                            
         BZ    *+8                                                              
         MVI   DEPSW,C'Y'                                                       
*                                                                               
         LA    R4,RECORD                                                        
         MVI   ELCODE,X'32'                                                     
         BAS   RE,GETEL            SEE IF ACCOUNT HAS A 32 ELEMENT              
         BE    *+8                                                              
         OI    ACCTERR,NOPOST                                                   
VALACX   B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        VALIDATE SALARY TAPE DATA                                              
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
VALSAL   NTR1                                                                   
         XC    POSTSTAT,POSTSTAT                                                
         LA    R5,SRTREC                                                        
         CLC   SRTREF,XSPACES                                                   
         BH    *+8                                                              
         OI    SALSTAT2,BADREF                                                  
*                                                                               
         GOTO1 VALACC,DMCB,SRTACCT                                              
         TM    ACCTERR,NOTFOUND                                                 
         BZ    *+8                                                              
         OI    SRTSTAT,BADACC                                                   
         TM    ACCTERR,NOPOST+LOCKCLOS                                          
         BZ    *+8                                                              
         OI    SRTSTAT,BADPOST                                                  
*                                                                               
         CLC   =C'SE',SRTACCT      SE POSTING                                   
         BNE   VALS10              NO                                           
         CLI   DEPSW,C'Y'          DEPT=Y ON SE ACCOUNT (SET IN VALACC)         
         BNE   VALS10              NO                                           
         OI    POSTSTAT,POST2D28   MAKE ADDITIONAL ANALYSIS POSTINGS            
*                                                                               
VALS10   MVC   SALNUM,ACCTNUM                                                   
         MVC   SALACCN,ACCTNAME                                                 
*                                                                               
         CLC   SRTCACT,PREVCACT    SAME C/A AS LAST TIME                        
         BNE   VALS20                                                           
         MVC   ACCTERR,PREVCAST    STATUS IS THE SAME                           
         B     VALS30                                                           
*                                                                               
VALS20   GOTO1 VALACC,DMCB,SRTCACT                                              
         MVC   SALCAC,ACCTNUM                                                   
         MVC   SALCACN,ACCTNAME                                                 
*                                                                               
VALS30   TM    ACCTERR,NOPOST+NOTFOUND+LOCKCLOS                                 
         BZ    *+8                                                              
         OI    SRTSTAT,BADCAC                                                   
*                                                                               
         MVC   PREVCACT,SRTCACT    SAVE INCASE THE SAME AS THE NEXT ONE         
         MVC   PREVCAST,ACCTERR                                                 
*                                                                               
         MVC   SVOFFICE,SRTOFF     VALIDATE OFFICE                              
         MVC   POSTOFF,SRTOFF                                                   
         BAS   RE,VALOFF                                                        
         BE    *+8                                                              
         OI    SRTSTAT,BADOFF                                                   
*                                                                               
         TM    POSTSTAT,POST2D28   MAKE ADDITIONAL POSTINGS                     
         BNO   VALSX               NO, ALL DONE                                 
*                                                                               
         MVC   TEMPACC,XSPACES                                                  
         MVC   TEMPACC(2),=C'2D'   BUILD 2D ACCOUNT                             
         MVC   TEMPACC+2(L'SRTOFF),SRTOFF                                       
         ZIC   R1,OFFLEN                                                        
         LA    R1,TEMPACC+2(R1)    BUMP PAST OFFICE                             
         MVC   0(L'SRTDEPT,R1),SRTDEPT                                          
         GOTO1 VALACC,DMCB,TEMPACC                                              
         TM    ACCTERR,NOTFOUND+NOPOST+LOCKCLOS                                 
         BZ    *+8                                                              
         OI    SRTSTAT,BAD2D                                                    
*                                                                               
         MVC   DEPNUM,ACCTNUM      SAVE 2D ACCOUNT AND NAME                     
         MVC   DEPNAME,ACCTNAME                                                 
*                                                                               
         MVC   TEMPACC,XSPACES                                                  
         MVC   TEMPACC(2),=C'28'   BUILD 28 ACCOUNT                             
         MVC   TEMPACC+2(L'SRTACCT-2),SRTACCT+2 28 MIRRORS SE                   
         GOTO1 VALACC,DMCB,TEMPACC                                              
         TM    ACCTERR,NOTFOUND+NOPOST+LOCKCLOS                                 
         BZ    *+8                                                              
         OI    SRTSTAT,BAD28                                                    
*                                                                               
         MVC   CRDSNUM,ACCTNUM     SAVE 28 ACCOUNT AND NAME                     
         MVC   CRDSNAME,ACCTNAME                                                
VALSX    MVC   SALSTAT1,SRTSTAT    SAVE ANY ERRORS IN SALSTAT1                  
*                                                                               
*        COUNT THE NUMBER OF SORT RECORDS WHICH WILL REQUIRE DEFAULT            
*        POSTINGS. IF IT GETS EXCESSIVE, REJECT TAPE                            
*                                                                               
         OC    SALSTAT,SALSTAT                                                  
         BZ    *+10                                                             
         AP    DEFCNT,=P'1'                                                     
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              LOOP THRU CODES/AMOUNTS ON SRTREC, POST/REPORT SJ                
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING SRCOD,R2                                                         
         SPACE 1                                                                
CODES    NTR1                                                                   
         LA    R5,SRTREC                                                        
*                                                                               
         LA    R2,SRTCODES                    START OF CODES ON TAPE            
         LA    R3,8                           NUMBER OF CODES ON TAPE           
         ZAP   SVTOTAL,=P'0'                  TOT FOR POSTSV AND DUPINV         
         MVI   BYTE,C'N'                      NO GOOD CODES YET                 
*                                                                               
CODE10   DS    0H                                                               
         CLC   SRCCODE,XSPACES                TAPE FIELD SPACES                 
         BNH   CODE60                         YES TRY NEXT                      
*                                                                               
         NI    SRTSTAT,X'FF'-CODEERR          TURN OFF ERROR                    
         MVC   SVWC,SRCCODE                                                     
         BAS   RE,VALCODE                                                       
*                                                                               
         OC    SRTSTAT,SRTSTAT                ANY ERRORS                        
         BZ    CODE40                         NO, CONTINUE                      
         BAS   RE,BADREC                      YES, PUT RECORD TO AREA           
         MVC   SRCCODE,XSPACES                BLANK OUT THE CODE                
         B     CODE60                         LOOK FOR NEXT ONE                 
*                                                                               
CODE40   DS    0H                                                               
         MVI   BYTE,C'Y'                      SET FOUND A CODE FLAG             
         CLC   VENNUM+1(2),=C'SR'             CONTRA POSTING TO SR?             
         BNE   CODE50                         NO                                
         SP    SVTOTAL,SRCAMNT                YES, POST A -DR'S                 
         B     CODE60                                                           
CODE50   AP    SVTOTAL,SRCAMNT                                                  
*                                                                               
CODE60   LA    R2,SRCOLN(R2)                  BUMP TO NEXT INPUT FIELD          
         BCT   R3,CODE10                      MORE FIELDS ON TAPE ?             
*                                                                               
*        ONLY PASS GOOD RECORDS PAST HERE                                       
*                                                                               
         TM    SRTSTAT,X'FF'-CODEERR         ANY NON-CODE ERRORS                
         BNZ   CODEX                         YES                                
         CLI   BYTE,C'Y'                     ANY GOOD WORK CODES ?              
         BNE   CODEX                         NO, EXIT                           
*        CHECK FOR DUPLICATE INVOICES                                           
*                                                                               
         CLC   TVENKEY,VENNUM            SEE IF BUFFALO TABLE                   
         BE    *+8                              ALREADY EXISTS                  
         BAS   RE,BLDTABLE                   IT DOESN'T SO BUILD                
*                                                                               
         BAS   RE,DUPCHECK         CHECK FOR DUPLICATE VENDOR POSTING           
*                                                                               
*        EITHER POST RECORD OR SAVE IT TO BAD AREA                              
*                                                                               
         USING SRCOD,R2                                                         
*                                                                               
         LA    R2,SRTCODES                    AREA OF RECORD FOR CODES          
         LA    R3,8                           MAX CODES IN RECORD               
CODE70   CLC   SRCCODE,XSPACES                ANYTHING THERE                    
         BNH   CODE90                                                           
*                                                                               
         TM    SRTSTAT,BADINV                                                   
         BZ    CODE80                                                           
         BAS   RE,BADREC                                                        
         B     CODE90                                                           
*                                                                               
CODE80   ZAP   PKAMOUNT,SRCAMNT              POST AND REPORT CODES              
         MVC   CODETYPE,SRCCOMM                                                 
         MVC   SVWC,SRCCODE                                                     
         XC    PRNTSTAT,PRNTSTAT                                                
         OI    PRNTSTAT,PRTSJ                                                   
         BAS   RE,REPORT                                                        
         BAS   RE,POSTSJ                                                        
CODE90   LA    R2,SRCOLN(R2)                                                    
         BCT   R3,CODE70                                                        
*                                                                               
         TM    SRTSTAT,BADINV                                                   
         BO    CODEX                                                            
         XC    PRNTSTAT,PRNTSTAT                                                
         OI    PRNTSTAT,PRTSV                                                   
         BAS   RE,REPORT                                                        
         BAS   RE,POSTSV                                                        
CODEX    B     XIT                            READ NEXT RECORD                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        VALIDATE THE WORKCODE IN SVWC                                          
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING WCORECD,R4                                                       
         USING SRTD,R5                                                          
         SPACE 1                                                                
VALCODE  NTR1                                                                   
         LA    R5,SRTREC                                                        
         LA    R4,SVKEY                       SPACE TO BUILD KEY                
         MVC   WCOKEY,XSPACES      SET UP ACKEYACC FOR 0A READ                  
         MVI   WCOKTYP,WCOKTYPQ           BUILD KEY                             
         MVC   WCOKCPY,RCCOMPFL           HEX COMP FROM MONAC                   
         MVC   WCOKUNT(2),=C'SJ'          UNIT/LEDGER                           
         MVC   WCOKWRK,SVWC               WORK CODE                             
         BAS   RE,READACC                     R4 POINTS TO ACC RECORD           
         CLI   DMCB+8,0                                                         
         BE    VALCX                          NOT FOUND                         
         OI    SRTSTAT,CODEERR                SET CODE NOT FOUND                
VALCX    B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              VALIDATE DATE IN TPDATE, CONVERT TO YYMMDD                       
*              IF ERROR, SET SRTSTAT                                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING TAPERECD,R6                                                      
         SPACE 1                                                                
VALDATE  NTR1                                                                   
         LA    R6,TAPEIO                                                        
         XC    YMD,YMD                                                          
         CLI   PROGPROF,C'Y'                  OPT TO USE TAPEDATE               
         BE    VALDA                                                            
         MVC   YMD,TODAY3                    NO, USE RCDATE                     
         B     VALDX                                                            
*                                                                               
VALDA    MVC   WORK,XSPACES                                                     
         MVC   WORK(2),TPDATE+2               MONTH                             
         MVI   WORK+2,C'/'                                                      
         MVC   WORK+3(2),TPDATE+4             DAY                               
         MVI   WORK+5,C'/'                                                      
         MVC   WORK+6(2),TPDATE               YEAR                              
         CLI   WORK+6,C'9'         IS   THE  YEAR GREATER THAN 1999 ?           
         BNH   VALDB               NO,  SKIP                                    
         ZIC   RE,WORK+6           CONVERT   X'FA' -> X'F0'                     
         SH    RE,=H'10'                     X'FB' -> X'F1'                     
         STC   RE,WORK+6                     ...                                
*                                                                               
VALDB    GOTO1 DATVAL,DMCB,(0,WORK),WORK+10                                     
         CLI   DMCB+3,0                       IS DATE VALID                     
         BNE   VALDC                          YES                               
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
         LA    R5,SRTREC                                                        
         OI    SRTSTAT,BADDATE     INVALID DATE ON RECORD                       
         B     VALDX                                                            
*                                                                               
*                                                  DATE IN YYMMDD               
VALDC    GOTO1 DATCON,DMCB,(0,WORK+10),(X'20',TPDATE)                           
         GOTO1 DATCON,DMCB,(0,TPDATE),(1,YMD)      CON TO YMD FORMAT            
*                                                                               
VALDX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        VALIDATE THE OFFICE CODE IN SVOFFICE                                   
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING OFFALD,R1                                                        
         SPACE 1                                                                
VALOFF   NTR1                                                                   
         CLI   OFFLEN,2            NEW OFFICES                                  
         BNE   VALOFFO             NO                                           
*                                                                               
         L     R1,ADOFFALD                                                      
         MVC   OFFAREQO,XSPACES                                                 
         MVC   OFFAOFFC,SVOFFICE                                                
         MVI   OFFAACT,OFFAVAL                                                  
         GOTO1 ADOFFAL                                                          
*        TM    OFFAERR,OFFAEOFF    INVALID OFFICE                               
         CLI   OFFAERR,0           SET CC NEQ IF ERROR                          
         B     XIT                                                              
*                                                                               
VALOFFO  CLI   SVOFFICE,C' '       VALIDATE OLD OFFICES                         
         BH    VALOFFO1                                                         
         CR    RB,R1               RETURN NEQ IF SPACE IN OFFICE                
         B     XIT                                                              
*                                                                               
         USING ACCRECD,R4                                                       
*                                                                               
VALOFFO1 MVI   SVOFFICE+1,C' '                                                  
         MVI   POSTOFF+1,C' '                                                   
         LA    R4,SVKEY            SPACE TO BUILD KEY                           
         MVC   ACCKEY,XSPACES      SET UP ACCKEY FOR SV READ                    
         MVC   ACCKEY(1),RCCOMPFL       HEX COMP FROM MONAC                     
         MVC   ACCKEY+1(2),=C'2D'                                               
         MVC   ACCKEY+3(1),SVOFFICE                                             
*                                                                               
         BAS   RE,READACC                                                       
         CLI   DMCB+8,0                                                         
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CHECK IF THERE IS A DUPLICATE VENDOR POSTING                           
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
DUPCHECK NTR1                                                                   
         LA    R5,SRTREC                                                        
         MVC   BUFFKEY(6),SRTREF                                                
         MVC   BUFFNAME(20),XSPACES                                             
         TM    SRTFLAG,TRAVEL                                                   
         BNO   *+10                                                             
         MVC   BUFFNAME(20),SRTNARR                                             
*                                                                               
         GOTO1 BUFFALO,DMCB,=C'GET',A(BUFFALOC),BUFFREC,1                       
         CLI   DMCB+8,0                                                         
         BNE   DUPC07                        NOT FOUND, ADD TO TABLE            
*                                                                               
         CP    SVTOTAL,BUFFAMT                                                  
         BE    DUPC08                        SAME INV. SAME AMT REJECT          
*                                                                               
DUPC07   MVC   BUFFKEY(6),SRTREF             NEW INV., SO ADD TO TABLE          
         MVC   BUFFNAME(20),XSPACES                                             
         TM    SRTFLAG,TRAVEL                                                   
         BNO   *+10                                                             
         MVC   BUFFNAME(20),SRTNARR                                             
*                                                                               
         ZAP   BUFFAMT,SVTOTAL                                                  
         GOTO1 BUFFALO,DMCB,=C'PUT',A(BUFFALOC),BUFFREC                         
         B     *+8                                                              
*                                                                               
DUPC08   OI    SRTSTAT,BADINV                                                   
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*         SET UP SJ POSTING RECORD AND POST TO WORKER                           
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
POSTSJ   NTR1                                                                   
         LA    R5,SRTREC                                                        
*                                                                               
         MVC   POSTACC,PRDNUM                                                   
         MVC   POSTCAC,VENNUM                                                   
         MVC   POSTCACN,VENNAME                                                 
*                                                                               
         MVC   POSTANAL,SVWC       CODE FOR KEY                                 
         MVI   POSTSTAT,POSTWORK   FLAG TO USE POSTANAL, NOT POSTOFF            
         ZAP   POSTCASH,PKAMOUNT                                                
         MVI   POSTST,X'80'        SJ POSTINGS ARE DR'S                         
         CLI   CODETYPE,C'N'       NON COMM                                     
         BNE   *+8                                                              
         OI    POSTST,X'01'        MAKE NON-COMMISSIONABLE                      
*                                                                               
         BAS   RE,MAKEREC                                                       
         BAS   RE,POSTIT                                                        
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              SET UP THE SV POSTING RECORD, POST TO WORKER                     
*              THIS ROUTINE DEPENDS ON POSTSJ HAVING SET UP TRNREC              
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
POSTSV   NTR1                                                                   
         LA    R5,SRTREC                                                        
*                                                                               
         XC    POSTSTAT,POSTSTAT                                                
         MVC   POSTACC,VENNUM                                                   
         MVC   POSTCAC,XSPACES                                                  
         MVC   POSTCAC(6),PRDNUM   C/A IS CLIENT                                
         MVC   POSTCACN,SRTCLNM    CLIENT NAME                                  
         MVC   POSTOFF,SRTOFF      OFFICE                                       
*                                                                               
         XC    POSTANAL,POSTANAL   NO WORKCODE (USE OFFICE)                     
*                                                                               
         ZAP   POSTCASH,SVTOTAL                                                 
*                                                                               
         MVI   POSTST,0            SV POSTINGS ARE CREDITS                      
         CLC   VENNUM+1(2),=C'SR'  POSTING TO SR?                               
         BNE   SVP10                                                            
         OI    POSTST,X'80'        SR'S ARE NEGATIVE DR'S                       
*                                                                               
SVP10    BAS   RE,MAKEREC                                                       
*                                                                               
         USING OTHELD,R2                      BUILD OTHERS EL FOR PROD          
*                                                                               
         MVC   OTHEL(2),=X'230F'                                                
         MVC   OTHNUM,XSPACES                                                   
         MVC   OTHNUM(3),PRDNUM+6            PROD (3+3 SPACES)                  
         MVC   OTHNUM+6(6),PRDNUM+9          JOB                                
*                                                                               
         BAS   RE,POSTIT                                                        
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        POST SRTREC AS SALARY JOURNAL ENTRIES                                  
*        POSTOFF SET IN VALSAL                                                  
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
POSTSAL  NTR1                                                                   
         LA    R5,SRTREC                                                        
*                                                                               
         MVC   POSTACC,SALNUM                                                   
         XC    POSTANAL,POSTANAL                                                
         MVC   POSTCAC,SALCAC                                                   
         MVC   POSTCACN,SALCACN                                                 
         ZAP   POSTCASH,SRTAMNT                                                 
         MVI   POSTST,0            ASSUME CREDIT                                
         CLI   SRTDC,C'D'                                                       
         BNE   *+8                                                              
         OI    POSTST,TRNSDR                                                    
*                                                                               
         BAS   RE,MAKEREC                                                       
         TM    POSTSTAT,POST2D28   MAKE ADDITIONAL POSTINGS                     
         BNO   POSTS10             NO                                           
*                                                                               
         BAS   RE,ADDC0            ADD ANALYSIS POINTERS                        
*                                                                               
POSTS10  BAS   RE,POSTIT                                                        
*                                                                               
         TM    POSTSTAT,POST2D28   MAKE ADDITIONAL POSTINGS                     
         BNO   POSTSX              NO                                           
*                                                                               
         CLC   =C'SE',SALNUM+1     DID I CR SE                                  
         BNE   POSTS20                                                          
         CLI   SRTDC,C'C'                                                       
         BNE   *+10                                                             
         MP    POSTCASH,=P'-1'     POST ANALYSIS AS NEG                         
POSTS20  MVC   POSTACC,DEPNUM                                                   
         XC    POSTANAL,POSTANAL                                                
         MVC   POSTCAC,CRDSNUM                                                  
         MVC   POSTCACN,CRDSNAME                                                
         MVI   POSTST,TRNSDR           DEBIT 2D                                 
         BAS   RE,MAKEREC                                                       
         BAS   RE,POSTIT                                                        
*                                                                               
         MVC   POSTACC,CRDSNUM                                                  
         XC    POSTANAL,POSTANAL                                                
         MVC   POSTCAC,DEPNUM                                                   
         MVC   POSTCACN,DEPNAME                                                 
         MVI   POSTST,0                CREDIT 28                                
         BAS   RE,MAKEREC                                                       
         BAS   RE,POSTIT                                                        
*                                                                               
POSTSX   B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        WRITE A POSTING HEADER AND TRANSACTION EL TO 0(R2) USING               
*        THE DATA SET IN POSTREC AND SRTREC                                     
*        ON XIT, 0(R2) IS A(AREA FOR ANY ADDITIONAL ELEMENTS)                   
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
MAKEREC  NTR1                                                                   
         LA    R5,SRTREC                                                        
*                                                                               
         TM    POSTSTAT,POSTWORK   POSTING BY WORKCODE                          
         BO    MAKER10             YES                                          
         CLC   POSTOFF,XSPACES     MAKE SURE OFFICE IS SET                      
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MAKER10  LA    R2,DRTOTAL          KEEP POSTING TOTALS                          
         TM    POSTST,X'80'                                                     
         BO    *+8                                                              
         LA    R2,CRTOTAL                                                       
         AP    0(BUCKLN,R2),POSTCASH                                            
         AP    POSTCNT,=P'1'                                                    
*                                                                               
         USING PSHEADD,R2                                                       
*                                                                               
         LA    R2,POSTAREA                                                      
         XC    POSTAREA,POSTAREA                                                
         XC    POSTHEAD,POSTHEAD                                                
         MVI   PSHDEL,PSHDELQ                                                   
         MVI   PSHDLEN,PSHEADL                                                  
*                                                                               
         MVC   PSHDACC,POSTACC                                                  
         MVC   PSHDANAL,XSPACES                                                 
         TM    POSTSTAT,POSTWORK   WANT A WORK CODE IN THE KEY                  
         BZ    *+10                NO                                           
         MVC   PSHDANAL,POSTANAL                                                
*                                                                               
         MVC   PSHDSBAC,POSTCAC                                                 
         MVC   PSHDSBNM,POSTCACN                                                
         LA    R2,PSHEADL(R2)                                                   
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
         LA    R5,SRTREC                                                        
*                                                                               
         USING TRNELD,R2                                                        
*                                                                               
         MVI   TRNEL,TRNELQ                                                     
         MVC   TRNDATE,SRTDATE                                                  
         MVC   TRNREF,SRTREF                                                    
         MVI   TRNSUB,X'00'                                                     
         MVC   TRNTYPE,SRTTYPE                                                  
         MVC   TRNSTAT,POSTST      DR/CR-COM/NONCOM                             
         MVC   TRNMOS,SRTMOS                                                    
         MVC   TRNBREF,SRTBR                                                    
         ZAP   TRNAMNT,POSTCASH                                                 
         MVC   TRNOFFC,POSTOFF                                                  
         TM    POSTSTAT,POSTWORK   WANT A WORK CODE                             
         BZ    *+10                NO                                           
         MVC   TRNANAL,POSTANAL    WORKCODE                                     
         LA    RF,L'SRTNARR                 L'NARRATIVE FOR SQUASH              
         GOTO1 ADSQUASH,DMCB,SRTNARR,(RF)   SQUASH, GET NEW LENGTH              
         ZIC   R1,DMCB+7                    LENGTH OF NARRATIVE                 
         LTR   R1,R1                                                            
         BZ    MAKER50             NO NARRATIVE                                 
*                                                                               
         BCTR  R1,0                DEC R1                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TRNNARR(0),SRTNARR                                               
         LA    R1,1(R1)            RESTORE LEN                                  
*                                                                               
MAKER50  LA    RE,TRNLN1Q                  LENGTH OF TRANS ELEMENT              
         AR    RE,R1                        ADDED TOGETHER                      
         STC   RE,TRNLN                     =TOTAL TRANSACTION LEN              
*                                                                               
         GOTO1 =V(CONVMOS),DMCB,(X'FE',TRNELD),MOS                              
*                                                                               
         SR    RE,RE                                                            
         IC    RE,TRNLN                     =TOTAL TRANSACTION LEN              
         LA    R2,0(RE,R2)         BUMP R2                                      
         DROP  R2                                                               
*                                                                               
         USING TRSELD,R2                                                        
         XC    0(TRSLNQ,R2),0(R2)                                               
         MVI   TRSEL,TRSELQ                                                     
         MVI   TRSLN,TRSLNQ                                                     
         MVC   TRSDATE,TODAY2                                                   
         MVC   TRSPMOS,MOS                                                      
         DROP  R2                                                               
*                                                                               
         XIT1  REGS=(R2)                                                        
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        ADD THE X'C0' ANALYSIS POINTER ELEMENT TO NEXT ELEMENT SPACE           
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING APEELD,R2                                                        
         SPACE 1                                                                
ADDC0    NTR1                                                                   
         MVI   APEEL,APEELQ                                                     
         MVI   APENUM,2            NUMBER OF ANALYSIS POINTERS                  
         LA    R3,DEPNUM           FIRST POINTER                                
         LA    R4,APENTRY                                                       
         MVI   1(R4),APENSDR       ADD 2D AS A DEBIT                            
         BAS   RE,ADDMINI                                                       
         LA    R4,0(R1,R4)                                                      
         XC    1(1,R4),1(R4)                                                    
         LA    R3,CRDSNUM                                                       
         BAS   RE,ADDMINI                                                       
         LA    R4,0(R1,R4)                                                      
         SR    R4,R2               LENGTH OF ELEMENT                            
         STC   R4,APELN                                                         
         B     XIT                                                              
*                                                                               
ADDMINI  LA    RF,14(R3)           RF TO LAST BYTE OF KEY                       
         LA    R1,13               KEY LESS 1 FOR COMPANY AND 1 FOR EX.         
         CLI   0(RF),X'41'         FIND LAST SIGNIFICANT BYTE                   
         BH    *+12                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         DC    H'0'                INVALID KEY                                  
*                                                                               
         CH    R1,=H'2'                                                         
         BNL   *+6                 NEED AT LEAST U/L/A                          
         DC    H'0'                                                             
*                                                                               
         EX    R1,*+8              MOVE ANALYSIS POINTER TO ELEMENT             
         B     *+10                                                             
         MVC   APENACT-APENTRY(0,R4),1(R3)                                      
         LA    R1,APENACT-APENTRY+1(R1) LENGTH OF MINI                          
         STC   R1,0(R4)                                                         
         BR    RE                                                               
         EJECT ,                                                                
*---------------------------------------------------------------------          
*        BUILD TABLE OF INVOICE AND AMOUNTS FOR DUPCHECK                        
*---------------------------------------------------------------------          
         SPACE 1                                                                
         USING ACTRECD,R4                                                       
         SPACE 1                                                                
BLDTABLE NTR1                                 SAVE REGISTERS                    
         MVC   TVENKEY,VENNUM                SAVE VENDOR ACCT.                  
         LA    R4,SVKEY                       READ SV, MATCH AMNT-INVNO         
         MVC   ACTKEY,XSPACES                 CLEAR KEY                         
         MVC   ACTKCULA,VENNUM                PUT IN VENDOR ACCT                
         BAS   RE,READACC                     R4 POINTS TO ACC RECORD           
         CLI   DMCB+8,0                                                         
         BE    *+6                            NOT FOUND THEN DIE                
         DC    H'0'                                                             
*        ******** INITIALIZE BUFF TO BUILD TABLE********                        
         GOTO1 BUFFALO,DMCB,=C'SET',A(BUFFALOC)                                 
*                                                                               
BLD10    CLC   SVKEY(15),RECORD              SAME VENDOR RETURNED               
         BNE   XIT                           I'M DONE HERE                      
*                                                                               
         USING TRNELD,R4                                                        
*                                                                               
         LA    R4,RECORD                                                        
         AH    R4,DATADISP                   POINT TO THE DATA                  
         CLI   TRNEL,TRNELQ                  TRANSACTION ?                      
         BNE   BLD50                         NO GOOD GET NEXT                   
*                                                                               
         TM    TRNSTAT,TRNSDR                IGNORE OFFSETS                     
         BO    BLD50                         NO GOOD GET NEXT                   
*                                                                               
         CLC   VENNUM+1(2),=C'SR'  -DR POSTINGS?                                
         BNE   BLD25               NO                                           
         TM    TRNSTAT,TRNSDR      IGNORE CREDITS                               
         BNO   BLD50                                                            
         B     BLD27               PUT DR'S TO BUFF TABLE                       
*                                                                               
BLD25    TM    TRNSTAT,TRNSDR      IGNORE DEBITS                                
         BO    BLD50                                                            
*                                                                               
BLD27    MVC   BUFFINV(6),TRNREF   GOOD, PUT IN TABLE -                         
         ZAP   BUFFAMT,TRNAMNT    REFENCE NUM AND AMT                           
         MVC   BUFFNAME(20),XSPACES                                             
         TM    SRTFLAG,TRAVEL                IS THIS A TRAVEL RUN               
         BNO   BLD40                         NO,NO NARRITIVE IN BUFKEY          
         ZIC   R1,TRNLN                      YES,FIRST 20 OF NARRATIVE          
         SH    R1,=H'29'                     INTO BUFFALO KEY                   
         CH    R1,=H'20'                                                        
         BL    BLD30                                                            
         LH    R1,=H'19'                                                        
BLD30    EX    R1,*+8                                                           
         B     BLD40                                                            
         MVC   BUFFNAME(0),TRNNARR                                              
BLD40    GOTO1 BUFFALO,DMCB,=C'PUT',A(BUFFALOC),BUFFREC                         
BLD50    BAS   RE,READSEQ                    GET NEXT SVACCT                    
         B     BLD10                                                            
         DROP  R4                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              READ ACC FILE                                                    
*----------------------------------------------------------------------         
         SPACE 1                                                                
READACC  DS    0H                                                               
         ST    RE,RESAVE                                                        
         MVC   DATACOM,=CL8'DMREAD'                                             
         BAS   RE,CALLDMGR                                                      
         L     RE,RESAVE                                                        
         BR    RE                                                               
*                                                                               
READSEQ  DS    0H                                                               
         ST    RE,RESAVE                                                        
         MVC   DATACOM,=CL8'DMRSEQ'                                             
         BAS   RE,CALLDMGR                                                      
         L     RE,RESAVE                                                        
         BR    RE                                                               
*                                                                               
READHIGH DS    0H                                                               
         ST    RE,RESAVE                                                        
         MVC   DATACOM,=CL8'DMRDHI'                                             
         BAS   RE,CALLDMGR                                                      
         L     RE,RESAVE                                                        
         BR    RE                                                               
*                                                                               
CALLDMGR NTR1                                                                   
         GOTO1 DATAMGR,DMCB,DATACOM,=C'ACCOUNT',SVKEY,RECORD                    
XIT      XIT1                                                                   
*                                                                               
PRINTEM  ST    RE,RESAVE                                                        
         GOTO1 ACREPORT                                                         
         L     RE,RESAVE                                                        
         BR    RE                                                               
*                                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              GET A NAME FROM AN ACC RECORD                                    
*----------------------------------------------------------------------         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         MVI   ELCODE,X'20'                   FOR NAME ELEMENT                  
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                           NO NAME                           
*                                                                               
         USING NAMELD,R4                                                        
*                                                                               
         MVC   WORK(L'NAMEREC),XSPACES                                          
         ZIC   R1,NAMLN                                                         
         SH    R1,=H'3'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EX    R1,*+8                                                           
         B     XIT                                                              
         MVC   WORK(0),NAMEREC                                                  
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              STORE RECORDS WHICH HAVE ERRORS                                  
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING SRTD,R5                                                          
         USING RECOUTD,R3                                                       
         USING SRCOD,R2                                                         
         SPACE 1                                                                
BADREC   NTR1                                                                   
         LA    R5,SRTREC                                                        
         L     R3,BADADDR                     ADDRESS OF AREA TO WRITE          
         AP    BADCNT,=P'1'                   ENOUGH ROOM TO WRITE THIS         
         CP    BADCNT,MAXBAD                                                    
         BL    *+6                            NO, DIE                           
         DC    H'0'                                                             
         MVC   REREF,SRTREF                   C/P/J, VENDOR DATE, INV #         
*                                                                               
         MVC   REDATE,SRTDATE                 PACKED DATE                       
         MVC   REACCT,SRTACCT                                                   
         MVC   RECAC(14),SRTCACT                                                
         MVC   REOFF,SRTOFF                                                     
         MVC   REDEPT,SRTDEPT                                                   
         MVC   RENARR(35),SRTNARR                                               
         MVC   RESTAT,SRTSTAT                                                   
*                                                                               
         ZAP   REAMOUNT,=P'0'                                                   
         TM    SRTSTAT,CODEERR     IS A WORKCODE IN ERROR                       
         BO    BADR50                                                           
         MVC   RECODE,SRCCODE                                                   
         ZAP   REAMOUNT,SRCAMNT                                                 
*                                                                               
BADR50   LA    R3,RECOLEN(R3)                 BUMP R3                           
         ST    R3,BADADDR                     THEN STORE FOR NEXTTIME           
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              NO MORE RECORDS LEFT TO PROCESS SO FINISH UP                     
*----------------------------------------------------------------------         
         SPACE 1                                                                
NOMORECS DS    0H                                                               
*                                                                               
         GOTO1 ADSORTER,DMCB,=C'END'                                            
*                                                                               
         TM    RUNSTAT,SALTAPE                                                  
         BZ    NOMO05                                                           
         BAS   RE,SETDEF           MAKE A DEFAULT POSTING, IF NECESS            
*                                                                               
         USING PLINED,R6                                                        
*                                                                               
         LA    R6,XP                                                            
         MVC   PRDEB,=C'------------------'                                     
         MVC   PRCRED,=C'------------------'                                    
         BAS   RE,PRINTEM                                                       
*                                                                               
         ZAP   DUB,SALCRS                                                       
         LA    R4,PRCRED                                                        
         BAS   RE,PACKPRT                                                       
*                                                                               
         ZAP   DUB,SALDRS                                                       
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         MVC   XP+1(11),=C'TAPE TOTALS'                                         
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         ZAP   DUB,DEFCRS                                                       
         LA    R4,PRCRED                                                        
         BAS   RE,PACKPRT                                                       
*                                                                               
         ZAP   DUB,DEFDRS                                                       
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         MVC   XP+1(11),=C'DEFAULT TOTALS'                                      
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         CLI   RCPOSTNG,C'Y'                                                    
         BNE   NOMO10                                                           
*                                                                               
         ZAP   DUB,CRTOTAL                                                      
         LA    R4,PRCRED                                                        
         BAS   RE,PACKPRT                                                       
*                                                                               
         ZAP   DUB,DRTOTAL                                                      
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         MVC   XP+1(14),=C'POSTING TOTALS'                                      
         BAS   RE,PRINTEM                                                       
         B     NOMO10                                                           
*                                                                               
NOMO05   CP    PKJOBCNT,=P'0'                ANY GOOD RECS HERE                 
         BE    NOMO10                        NO, JUST PRINT THE BAD             
         XC    PRNTSTAT,PRNTSTAT                                                
         OI    PRNTSTAT,LASTPRT+NEWJOB+NEWOFF FORCE JOB AND REPORT TOT          
         BAS   RE,REPORT                      REPORT ANYTHING LEFT              
*                                                                               
NOMO10   MVC   XP,XSPACES                     CLEAR TRASH FROM XP               
*                                                                               
         USING PSSUBFD,R2                                                       
*                                                                               
         LA    R2,POSTAREA                    CREATE TRAILING REC               
         XC    POSTAREA,POSTAREA              CLEAR POSTING AREA                
         XC    POSTHEAD,POSTHEAD              CLEAR RECORD HEADER               
         MVC   PSSBEL(2),=X'521D'             BUILD POSTING HEADER              
*                                                                               
         MVC   PSSBDESC(14),=C'VENDOR JOURNAL'                                  
         CLI   QOPT1,C'Y'                                                       
         BNE   *+10                                                             
         MVC   PSSBDESC(14),=C'SALARY JOURNAL'                                  
*                                                                               
         ZAP   PSSBRECS,POSTCNT               TOTAL RECS POSTED                 
         ZAP   PSSBCASH,DRTOTAL               DEBIT TOTAL                       
         ZIC   R3,PSSBLEN                     GET TRNSACTION LEN                
         AR    R2,R3                                                            
         MVI   0(R2),X'00'                    TRAILING 00'S                     
         LA    R3,5(R3)                       ADD 4 HEAD+1 TRAIL                
         STH   R3,POSTHEAD                    STORE LENGTH IN HEADER            
         LA    R3,POSTHEAD                                                      
         MVC   COMMAND,=CL6'ADD'                                                
         BAS   RE,FILE                        POST TRAILER REC                  
         MVC   COMMAND,=CL6'CLOSE'                                              
         BAS   RE,FILE                        CLOSE WORKER FILE                 
*                                                                               
         CP    BADCNT,=P'0'                   ANY BAD RECS THIS RUN?            
         BE    NOMOX                          NO, YOUR DONE                     
*                                                                               
         ZAP   BADTOT,=P'0'                   INIT DOLLAR TOTAL                 
*                                                                               
         USING RECOUTD,R3                                                       
         USING PRTERRD,R6                                                       
         LA    R6,XP                                                            
*                                                                               
         CVB   R4,BADCNT                      LOAD R4 FOR LOOP COUNT            
         L     R3,ABAD                        START OF AREA                     
         MVI   RCSUBPRG,1                     HEADER FOR THIS REPORT            
         BAS   RE,HEADUP                      REFRESH HEADERS                   
*                                                                               
NOMO20   DS    0H                             PRINT THE BAD RECORD              
         BAS   RE,PRINTEM                     FIRST A BLANK                     
         MVC   PRERREF,REREF                  REFRENCE NUMBER                   
         TM    RESTAT,BADDATE                                                   
         BO    NOMO25                                                           
         GOTO1 DATCON,DMCB,(1,REDATE),(5,PRERDATE)                              
*                                                                               
NOMO25   MVC   PRERCPJ,REACCT                 CLIENT PRODUCT JOB                
         MVC   PRERVEN,RECAC                  VENDOR                            
         MVC   PRERCODE,RECODE                                                  
*                                                                               
         AP    BADTOT,REAMOUNT                                                  
         EDIT  (P6,REAMOUNT),PRERAMNT,2,MINUS=YES,ZERO=NOBLANK                  
*                                                                               
NOMO30   MVC   PRERNARR(L'RENARR),RENARR                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
         GOTO1 PRTERR,DMCB,RESTAT,ERRMSG                                        
*                                                                               
         LA    R3,RECOLEN(R3)                 NEXT BAD RECORD                   
         BCT   R4,NOMO20                      READ ALL BAD RECS?                
*                                                                               
         MVI   SPACING,2                      DOUBLE SPACE                      
         BAS   RE,PRINTEM                                                       
         MVC   XP+11(12),=C'ERROR TOTALS'                                       
         ZAP   DUB,BADCNT                                                       
         LA    R4,XP+32                                                         
         BAS   RE,NUMPRT                      EDIT W/NO DECIMAL                 
         MVC   XP+38(5),=C'ITEMS'                                               
*                                                                               
         ZAP   DUB,BADTOT                                                       
         LA    R4,XP+61                                                         
         BAS   RE,PACKPRT                     EDIT W/TWO DECIMAL                
         BAS   RE,PRINTEM                                                       
*                                                                               
NOMOX    BAS   RE,CONTROLC                    REPORT TOTALS FOR CONTROL         
         BAS   RE,RELBUFF          RELEASE GETMAINED SPACE                      
         B     XIT                            AND EOJ                           
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              BUILD AND PRINT REPORT                                           
*----------------------------------------------------------------------         
         SPACE 1                                                                
REPORT   NTR1  *                                                                
         ZIC   RF,LINE                        CURRENT LINE                      
         AH    RF,=H'5'                       MIN LINES LEFT                    
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP                      REFRESH HEADERS                   
*                                                                               
         USING PLINED,R6                      PRINT DSECT                       
         USING SRTD,R5                                                          
*                                                                               
         LA    R6,XP                          FOR ACREPORT                      
         LA    R5,SRTREC                                                        
*                                                                               
         TM    PRNTSTAT,PRTSV                 IS THIS CALL FROM SV              
         BZ    REPB                                                             
         CLC   XP,XSPACES                     ANYTHING TO PRINT                 
         BE    REPA                           NO, JUST PRINT THE SV             
         MVI   SPACING,2                      YES, SET DOUBLE SPACE &           
         BAS   RE,PRINTEM                     PRINT                             
REPA     MVC   PRACC,VENNUM+1                 PRINT SV CREDIT AMOUNT            
         MVC   PRACCN,VENNAME                 PRINT SV CREDIT AMOUNT            
*                                                                               
         CLC   VENNUM+1(2),=C'SR'             IS THIS A SR POSTING              
         BNE   REPAA                          NO                                
         ZAP   DUB,SVTOTAL                    YES, ITS A -DR                    
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         BAS   RE,PRINTEM                                                       
         AP    TOTALDR,SVTOTAL               INCLUDE WITH TOTAL DEBITS          
         B     XIT                                                              
*                                                                               
REPAA    ZAP   DUB,SVTOTAL                                                      
         LA    R4,PRCRED                                                        
         BAS   RE,PACKPRT                                                       
         BAS   RE,PRINTEM                                                       
         AP    SVREPAMT,SVTOTAL               FOR REPORT TOTAL                  
         B     XIT                                                              
*                                                                               
REPB     CLI   PROGPROF+1,C'Y'                                                  
         BE    REPC                                                             
         MVC   PREVOFF,SRTOFF                 SUPRESS OFFICE TOTS               
         NI    PRNTSTAT,X'FF'-NEWOFF                                            
*                                                                               
REPC     CLC   PREVACCT,XSPACES               FIRST JOB                         
         BNE   REP01                          NO, TRY TICKET NUMBER             
         OI    PRNTSTAT,FIRSTJOB                                                
         BAS   RE,HEADUP                                                        
*                                                                               
REP01    CLC   PREVACCT,PRDNUM                NEW JOB NUMBER ?                  
         BE    *+8                                                              
         OI    PRNTSTAT,NEWJOB                                                  
*                                                                               
         CLC   PREVTICK,SRTREF                NEW TICKET NUMBER ?               
         BE    *+8                                                              
         OI    PRNTSTAT,NEWTICK                                                 
*                                                                               
         TM    SRTFLAG,TRAVEL       IS THIS A TRAVEL RUN                        
         BNO   *+8                                                              
         OI    PRNTSTAT,NEWTICK                                                 
*                                                                               
         TM    SRTFLAG,DEFAULT                DEFAULT JOB RUN                   
         BNO   *+8                              NO                              
         OI    PRNTSTAT,NEWTICK                                                 
*                                                                               
         CLC   SRTOFF,PREVOFF                                                   
         BE    *+8                              NO                              
         OI    PRNTSTAT,NEWOFF                                                  
*                                                                               
         CLC   PREVACCT(6),PRDNUM             CULCLI                            
         BE    *+8                                                              
         OI    PRNTSTAT,NEWCLI                                                  
*                                                                               
         MVC   PREVACCT,PRDNUM                SAVE JOB AND TICKET               
         MVC   PREVTICK,SRTREF                FOR NEXT TIME                     
         MVC   PREVOFF,SRTOFF                                                   
*                                                                               
         TM    PRNTSTAT,FIRSTJOB              IS THIS THE FIRST JOB             
         BO    REP02A                         SKIP TOTALS                       
         TM    PRNTSTAT,NEWJOB                IS THIS A NEW JOB                 
         BZ    REP03                          NO JOB TOTALS YET                 
         CLC   XP,XSPACES                     ANY OLD STUFF IN XP ?             
         BE    REP02                          NO, BRANCH                        
         MVI   SPACING,2                      YES, SET DOUBLE SPACE &           
         BAS   RE,PRINTEM                     PRINT IT !                        
*                                                                               
REP02    DS    0H                                                               
         CP    PKJOBCNT,=P'1'                 ONLY ONE ITEM                     
         BNE   REP0201                        NO JOB TOTALS BUT                 
         MVI   SPACING,2                      SET DOUBLE SPACE &                
         BAS   RE,PRINTEM                     PRINT                             
         B     REP0202                                                          
*                                                                               
REP0201  DS    0H                             PRINT TOTALS FOR A JOB            
         BAS   RE,PRINTEM                     PRINT A BLANK                     
         MVI   SPACING,2                      DOUBLE SPACE AFTER                
         MVC   PRACCN(9),=C'JOB TOTAL'                                          
*                                                                               
         ZAP   DUB,PKJOBAMT                                                     
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         BAS   RE,PRINTEM                                                       
*                                                                               
REP0202  ZAP   PKJOBAMT,=P'0'                 CLEAR TOTALERS                    
         ZAP   PKJOBCNT,=P'0'                                                   
*                                                                               
         TM    PRNTSTAT,NEWCLI                                                  
         BZ    REP0203                                                          
*                                                                               
         CP    PKCLICNT,=P'1'                                                   
         BNE   REP0202A                                                         
         MVI   SPACING,2                      SET DOUBLE SPACE &                
         BAS   RE,PRINTEM                     PRINT                             
         B     REP0202B                                                         
*                                                                               
REP0202A MVI   SPACING,3                      DOUBLE SPACE AFTER                
         MVC   PRACCN(12),=C'CLIENT TOTAL'                                      
         ZAP   DUB,PKCLICNT                                                     
         LA    R4,PRACCN+21                                                     
         BAS   RE,NUMPRT                                                        
         MVC   PRACC+6(5),=C'ITEMS'                                             
         ZAP   DUB,PKCLIAMT                   PRINT CLIENT DEBIT TOTAL          
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         BAS   RE,PRINTEM                                                       
*                                                                               
REP0202B ZAP   PKCLIAMT,=P'0'                 CLEAR TOTALERS                    
         ZAP   PKCLICNT,=P'0'                                                   
*                                                                               
REP0203  TM    PRNTSTAT,NEWOFF                                                  
         BZ    REP0204                                                          
*                                                                               
         MVC   PRACCN(12),=C'OFFICE TOTAL'                                      
         ZAP   DUB,PKOFFCNT                                                     
         LA    R4,PRACCN+21                                                     
         BAS   RE,NUMPRT                                                        
         MVC   PRACC+6(5),=C'ITEMS'                                             
         ZAP   DUB,PKOFFAMT                   PRINT OFFICE DEBIT TOTAL          
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         ZAP   DUB,PKOFFAMT                                                     
         LA    R4,PRCRED                      PRINT OFFICE CREDIT TOTAL         
         BAS   RE,PACKPRT                                                       
         BAS   RE,PRINTEM                                                       
         BAS   RE,HEADUP                                                        
         ZAP   PKOFFAMT,=P'0'                 CLEAR TOTALERS                    
         ZAP   PKOFFCNT,=P'0'                                                   
*                                                                               
REP0204  TM    PRNTSTAT,LASTPRT               IS THIS THE LASTTIME THRU         
         BZ    REP02A                         NO, NO REPORT TOTALS              
*                                                                               
         MVI   SPACING,2                      PRINT TOTALS FOR REPORT           
         MVI   FORCEHED,C'N'                                                    
         BAS   RE,PRINTEM                     PRINT                             
*                                                                               
         MVC   PRACCN(12),=C'REPORT TOTAL'                                      
         ZAP   DUB,PKREPCNT                                                     
         LA    R4,PRACCN+21                                                     
         BAS   RE,NUMPRT                                                        
         MVC   PRACC+6(5),=C'ITEMS'                                             
*                                                                               
         ZAP   DUB,PKREPAMT                   PRINT REPORT DEBIT TOTAL          
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         ZAP   DUB,SVREPAMT                                                     
         LA    R4,PRCRED                      PRINT REPORT CREDIT TOTAL         
         BAS   RE,PACKPRT                                                       
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
*                                                                               
REP02A   MVC   PRACC,PRDNUM+1                 SET UP NEW JOB LINE               
         MVC   PRACCN,SRTNAME                 ACCOUNT NAME                      
         OI    PRNTSTAT,NEWTICK               FORCE NEW TICK ROUTINE            
*                                                                               
REP03    TM    PRNTSTAT,NEWTICK               NEW TICKET?                       
         BZ    REP04                                                            
         TM    PRNTSTAT,NEWJOB                IS THIS A NEW JOB                 
         BO    REP03A                         YES, GET REST OF THE REC          
         CLC   XP,XSPACES                     ANY OLD STUFF IN P ?              
         BE    REP03A                                                           
*                                                                               
         MVI   SPACING,2                                                        
         BAS   RE,PRINTEM                                                       
*                                                                               
REP03A   DS    0H                                                               
         TM    SRTFLAG,DEFAULT               DEFAULT JOB USED                   
         BNO   REP03B                        PRINT IT INSTEAD OF NARR           
         MVC   PRNARR,XSPACES                                                   
         MVC   PRNARR(12),SRTSPARE+2                                            
         MVC   PRNARR+13(20),=C'IS IN ERROR, ORIGIN:'                           
         LAY   R3,JWOFFTAB                    TABLE LOCATION                    
         LAY   R2,JWOFFNUM                    NUMBER IN TABLE                   
REP03A1  CLC   0(2,R3),SRTSPARE               JW PUTS ORIGIN IN CC 170          
         BNE   REP03A2                        GET UNIT NAME FROM TABLE          
         MVC   PRNARR+34(3),2(R3)                                               
         B     REP03C                                                           
*                                                                               
REP03A2  LA    R3,L'JWOFFTAB(R3)                                                
         BCT   R2,REP03A1                                                       
         MVC   PRNARR+34(2),SRTSPARE          NOT FOUND, PRINT CODE             
         B     REP03C                                                           
*                                                                               
REP03B   MVC   PRNARR(L'SRTNARR),SRTNARR                                        
*                                                                               
REP03C   MVC   PRDATE,XSPACES                                                   
         MVC   PRREF,XSPACES                                                    
         MVC   PRREF,SRTREF                   PRINT TICKET NUMBER               
*                                                                               
         GOTO1 DATCON,DMCB,(1,SRTDATE),(5,PRDATE)                               
*                                                                               
REP04    DS    0H                                                               
         LA    R6,XP                                                            
         CLC   PRSVWC,XSPACES                                                   
         BE    *+8                                                              
         LA    R6,XPSECOND                                                      
         MVC   PRSVWC,SVWC                    PRINT CODE                        
         ZAP   DUB,PKAMOUNT                                                     
         LA    R4,PRDEB                                                         
         BAS   RE,PACKPRT                                                       
         CLI   CODETYPE,C'N'                                                    
         BNE   *+10                                                             
         MVC   PRCRED(3),=C'N/C'              PRINT A N/C                       
         LA    R1,XP                                                            
         CR    R6,R1                                                            
         BNH   *+8                                                              
         BAS   RE,PRINTEM                                                       
*                                                                               
REP06    DS    0H                                                               
         AP    PKJOBAMT,PKAMOUNT                                                
         AP    PKJOBCNT,=P'1'                                                   
         AP    PKOFFAMT,PKAMOUNT                                                
         AP    PKOFFCNT,=P'1'                                                   
         AP    PKCLIAMT,PKAMOUNT                                                
         AP    PKCLICNT,=P'1'                                                   
         AP    PKREPAMT,PKAMOUNT                                                
         AP    PKREPCNT,=P'1'                                                   
         AP    TOTALDR,PKAMOUNT                                                 
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT A SALARY TAPE POSTING                                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
SALREP   NTR1                                                                   
         ZIC   RF,LINE                        CURRENT LINE                      
         AH    RF,=H'5'                       MIN LINES LEFT                    
         ZIC   RE,MAXLINES                                                      
         CR    RF,RE                                                            
         BNH   *+8                                                              
         BAS   RE,HEADUP                      REFRESH HEADERS                   
*                                                                               
         USING PLINED,R6                      PRINT DSECT FOR ACREPORT          
         USING SRTD,R5                        PRINT DSECT                       
*                                                                               
         LA    R6,XP                          -> FIRST  PRINT LINE              
         LA    R5,SRTREC                                                        
         MVC   PRREF,XSPACES                                                    
         MVC   PRREF,SRTREF                   PRINT TICKET NUMBER               
*                                                                               
         GOTO1 DATCON,DMCB,(1,SRTDATE),(5,PRDATE)                               
         MVC   PRACC,SRTACCT                                                    
         MVC   PRCODE,SRTOFF                                                    
         MVC   PRACCN,SALACCN                                                   
         ZAP   DUB,SRTAMNT                                                      
         LA    R4,PRCRED                      PRINT REPORT CREDIT TOTAL         
         CLI   SRTDC,C'D'                                                       
         BNE   *+8                                                              
         LA    R4,PRDEB                       PRINT REPORT CREDIT TOTAL         
         BAS   RE,PACKPRT                                                       
         MVI   AREA,C' '                                                        
         MVC   AREA+1(L'AREA-1),AREA                                            
         MVC   AREA(L'SRTNARR),SRTNARR                                          
         LA    R1,AREA+L'SRTNARR+1                                              
         MVC   0(4,R1),=C'C/A='                                                 
         MVC   4(L'SRTCACT,R1),SRTCACT                                          
         LA    R1,L'SRTCACT+4(R1)                                               
*                                                                               
         TM    POSTSTAT,POST2D28   DID I MAKE ADDITIONAL POSTINGS               
         BNO   SALR60                                                           
         MVC   0(4,R1),=C'A/P='                                                 
         MVC   4(L'DEPNUM-1,R1),DEPNUM+1                                        
         LA    R1,L'DEPNUM+4(R1)                                                
*                                                                               
         MVC   0(L'CRDSNUM-1,R1),CRDSNUM+1                                      
*                                                                               
SALR60   LA    RF,L'AREA                                                        
         GOTO1 ADSQUASH,DMCB,AREA,(RF)                                          
         LA    RF,L'AREA                                                        
         GOTO1 CHOPPER,DMCB,((RF),AREA),(L'PRNARR,PRNARR),(L'XP,2)              
         BAS   RE,PRINTEM                                                       
         OC    SRTSTAT,SRTSTAT     ANY ERRORS                                   
         JZ    SALR70              NO                                           
         GOTO1 PRTERR,DMCB,SRTSTAT,SALMSG                                       
*                                                                               
SALR70   TM    SRTSTAT1,SVALLGL                                                 
         JZ    SALRX               YES, PRINT THE ERROR                         
         GOTO1 PRTERR,DMCB,SRTSTAT1,MOAMSG                                      
*                                                                               
SALRX    B     XIT                                                              
         DROP  R5,R6                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              PRINT LAST PAGE FOR CONTROL                                      
*----------------------------------------------------------------------         
         SPACE 1                                                                
         USING LOGOD,R6                                                         
         SPACE 1                                                                
CONTROLC NTR1                                                                   
         CLI   RCPOSTNG,C'N'                  IS THIS A DRAFT RUN ?             
         BE    XIT                                                              
         L     R6,LOGOC                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,(R6)                                                   
CNTRL2   DS    0H                                                               
         L     RF,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,RF                                                      
*                                                                               
         L     RF,ADMASTD                                                       
*                                                                               
         USING MASTD,RF                                                         
*                                                                               
         L     R6,MCVREMOT                                                      
*                                                                               
         USING REMOTED,R6                                                       
*                                                                               
         OC    REMOTKEY,REMOTKEY                                                
         BZ    CNTRL2A             NOT REMOTE                                   
         GOTO1 PRINT,DMCB,=C'CLOSE'                                             
         XC    REMOTKEY,REMOTKEY   TURN OFF REMOTE                              
*                                                                               
CNTRL2A  L     R6,LOGOC                                                         
*                                                                               
         USING LOGOD,R6                                                         
*                                                                               
         MVI   LOGOTYPE,C'S'                                                    
         MVI   LOGOEND,C'X'                                                     
         MVC   LOGO1,=C'CONTROL'                                                
         MVC   LOGONAME,CNTNAME                                                 
         MVC   LOGOADD,CNTADDR                                                  
         MVC   LOGOADD3,XSPACES                                                 
         GOTO1 LOGO,DMCB,(R6)                                                   
         MVI   RCSUBPRG,2                                                       
*                                                                               
         BAS   RE,HEADUP                      UPDATE HEADER INFO                
         ZAP   DUB,DRTOTAL                                                      
         LA    R4,XP+23                       PRINT TOTAL DEBITS                
         BAS   RE,PACKPRT                                                       
         ZAP   DUB,CRTOTAL                                                      
         LA    R4,XP+40                       PRINT TOTAL CREDITS               
         BAS   RE,PACKPRT                                                       
         LA    R4,XP+61                       PRINT NUMBER OF RECORDS           
         ZAP   DUB,POSTCNT                                                      
         BAS   RE,NUMPRT                                                        
         BAS   RE,PRINTEM                                                       
         L     R6,LOGOC                                                         
         MVI   LOGOTYPE,C'E'                                                    
         GOTO1 LOGO,DMCB,(R6)                                                   
         B     XIT                                                              
*                                                                               
CNTNAME  DC    CL33'******** INTERNAL CONTROL ******'                           
CNTADDR  DC    CL33'******** DO NOT  SEND OUT ******'                           
*                                                                               
         DROP  R6,RF                                                            
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        PRINT THE ERROR MESSAGES PASSED IN ERRORS                              
*        R1 POINTS TO ERROR BYTE                                                
*----------------------------------------------------------------------         
         SPACE 1                                                                
PRTERR   NTR1                                                                   
         L     R2,P1                                                            
         MVC   ERRORS,0(R2)                                                     
         L     R6,P2                          START OF ERROR MSGS               
         MVI   BIT,X'01'                      INIT BIT TO TEST 01 BIT           
*                                                                               
PRTE40   DS    0H                                                               
         MVC   BITSAVE,BIT                                                      
         NC    BITSAVE,ERRORS                 TEST WHY THE RECORD IS NG         
         BZ    PRTE50                         NO, LOOK AT NEXT BIT              
*                                                                               
         MVC   XP+1(9),=C'**ERROR**'                                            
         MVC   XP+11(L'ERRMSG),0(R6)                                            
         BAS   RE,PRINTEM                                                       
*                                                                               
PRTE50   ZIC   R1,BIT                                                           
         SLL   R1,1                                                             
         STC   R1,BIT                                                           
         LA    R6,L'ERRMSG(R6)                BUMP R6                           
         CLI   BIT,X'00'                      ALL BITS TESTED                   
         BNE   PRTE40                         NO                                
*                                                                               
PRTEX    B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        ADD THE RECORD IN POSTAREA TO THE WORKER FILE                          
*----------------------------------------------------------------------         
         SPACE 1                                                                
POSTIT   NTR1                                                                   
         XC    POSTHEAD,POSTHEAD                                                
         LA    R2,POSTAREA                                                      
         CLI   0(R2),PSHDELQ       GOT A POSTING HEADER                         
         BE    *+6                 YESSS                                        
         DC    H'0'                                                             
         ZIC   R1,1(R2)                                                         
         LA    R2,0(R1,R2)         FIRST EL IS THE TRANSACTION                  
         IC    R1,1(R2)                                                         
         LA    R2,0(R1,R2)                                                      
         CLI   0(R2),0             ANOTHER EL HERE                              
         BE    POSTI50             NO                                           
         IC    R1,1(R2)                                                         
         LA    R2,0(R1,R2)                                                      
*                                                                               
POSTI50  LA    R3,POSTAREA                                                      
         SR    R2,R3               R2 IS LENGTH OF POSTING FILE                 
         LA    R2,5(R2)            ADD 4 FOR HEAD PLUS 1 FOR THE 00             
         STH   R2,POSTHEAD         SET LENGTH IN HEADER                         
         MVC   COMMAND,=CL6'ADD'                                                
         LA    R3,POSTHEAD                                                      
         BAS   RE,FILE                        POST TO WORKER FILE               
         B     XIT                                                              
         DROP  R3                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        GET SPACE                                                              
*----------------------------------------------------------------------         
         SPACE 1                                                                
GETBUFF  NTR1                                                                   
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         GETMAIN R,LV=(0)                                                       
         LA    R0,MAINNUM                                                       
         LR    R5,R1               R5 IS BUFFER POINTER                         
         ST    R1,ABUFF            SAVE BUFF START                              
*                                                                               
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         STCM  R5,15,MCUSRDMP                                                   
         LR    RF,R5                                                            
         A     RF,=A(BUFSIZE)                                                   
         STCM  RF,15,MCUSRDMP+4                                                 
*                                                                               
         L     R2,=A(MAINTAB)                                                   
*                                                                               
         USING MAIND,R2                                                         
*                                                                               
GETB10   MVC   *+8(2),MAINAST     SCON OF WHERE TO STORE BUFF LOCATION          
         ST    R5,FULL             FULL IS A DUMMY FOR THE ASSEMBLER            
         A     R5,MAINSIZE                                                      
*                                                                               
         LA    R2,MAINLEN(R2)                                                   
         BCT   R0,GETB10                                                        
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        RELEASE GETMAINED SPACE                                                
*----------------------------------------------------------------------         
         SPACE 1                                                                
RELBUFF  NTR1                                                                   
         L     R1,ABUFF                                                         
         L     R0,=A(BUFSIZE)     SUM OF ALL TABLE SIZES                        
         FREEMAIN R,A=(1),LV=(0)                                                
*                                                                               
         L     R2,VEXTRAS                                                       
*                                                                               
         USING RUNXTRAD,R2                                                      
*                                                                               
         L     R2,ADMASTD          PRINT THE BUFFER IN DUMP                     
*                                                                               
         USING MASTD,R2                                                         
*                                                                               
         XC    MCUSRDMP,MCUSRDMP   CLEAR XTRA DUMP ADDRESS                      
*                                                                               
         B     XIT                                                              
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              WORKER FILE INTERFACE                                            
*              R3 =A(IO AREA) PASSED TO THIS ROUTINE                            
*----------------------------------------------------------------------         
         SPACE 1                                                                
FILE     NTR1                                                                   
         CLI   QOPT7,C'Y'                                                       
         BNE   FILE01                                                           
         BAS   RE,DMPPUT                                                        
FILE01   CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         L     R4,APBUFF                                                        
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
*                                                                               
*              EDIT AND PRINT PACKED NUMERIC FIELDS                             
PACKPRT  ST    RE,FULL                                                          
         CURED (P8,DUB),(18,0(R4)),2,COMMAS=YES,MINUS=YES,ZERO=NOBLANK          
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
*              EDIT AND PRINT PACKED NUMERIC FIELDS (NO DECIMAL)                
NUMPRT   EDIT  (P8,DUB),(6,0(R4)),COMMAS=YES,ZERO=NOBLANK                       
         BR    RE                                                               
*                                                                               
***********************************************************************         
* CHECK FOR MONTH LOCK                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING SRTD,R5                                                          
*                                                                               
MTHLOCK  NTR1 ,                                                                 
         LA    R5,SRTREC                                                        
         CLI   SRTKJBLG,UNITG      IS IT NOT UNIT 'G'?                          
         JNE   MTHLK10             YES - IGNORE THE GL CHANGES                  
*                                                                               
         OC    SVGLMOA,SVGLMOA     NEW GL SYSTEM IN USE ?                       
         JZ    MTHLK10                                                          
*                                                                               
         CLC   SVGLYMD,SRTDATE                                                  
         JH    MTHLK10                                                          
         OI    SRTSTAT1,SVGLDAT    SET A BAD GL DATE                            
*                                                                               
MTHLK10  ZAP   BCDUB,=P'0'                                                      
         XC    BCWORK,BCWORK                                                    
         GOTO1 DATCON,DMCB,(X'81',SRTDATE),(6,BCDUB)                            
*                                                                               
         L     R4,4(R1)            ACTUAL LENGTH OF OUTPUT FIELD                
         STCM  R4,8,BYTE           STORE FOR BMONVAL CALL                       
         GOTO1 VBMONVAL,DMCB,(BYTE,BCDUB),(SRTTYPE,ADCOMFAC),(0,BCWORK)X        
               ,(SVCMPNY,0)                                                     
*                                                                               
         USING BMONVALD,R3                                                      
         LA    R3,BCWORK                                                        
         CLI   BMOERR,BMOEOKQ      NO ERRORS?                                   
         JE    MTHLOCKX                                                         
*                                                                               
         TM    BMOERR,BMOEINVQ                                                  
         JZ    MTHLK20                                                          
         OI    SRTSTAT1,SVINVMOA   DATE INVALID                                 
*                                                                               
MTHLK20  TM    BMOERR,BMOELOKQ                                                  
         JZ    MTHLK30                                                          
         OI    SRTSTAT1,SVLCKMOA   MOS LOCKED                                   
*                                                                               
MTHLK30  TM    BMOERR,BMOERNGQ                                                  
         JZ    MTHLOCKX                                                         
         OI    SRTSTAT1,SVRNGMOA   DATE OUT OF RANGE                            
*                                                                               
MTHLOCKX OC    SRTSTAT1,SRTSTAT1                                                
         JZ    XIT                                                              
*                                                                               
         MVI   RCPOSTNG,C'N'       DON'T POST IF ERROR                          
         J     XIT                                                              
*                                                                               
         DROP  R3,R5                                                            
         EJECT ,                                                                
*                                                                               
         LTORG                                                                  
*----------------------------------------------------------------------         
*        AN ERROR HAS BEEN FOUND, PRINT MESSAGE AND DON'T POST                  
*----------------------------------------------------------------------         
         SPACE 1                                                                
DONTPOST NTR1  BASE=*,LABEL=*                                                   
         MVI   RCPOSTNG,C'N'                                                    
         BAS   RE,HEADUP                                                        
         MVC   XP+45(36),=C'+-----------------------------------+'              
         MVC   XPSECOND+45(36),=C'|DEBITS NOT EQUALTO CREDITS ON TAPE|'         
         MVC   XPTHIRD+45(37),=C'|OR AN ERROR HAS BEEN DETECTED IN A |'         
         MVC   XPFOURTH+45(37),=C'|TAPE FIELD OR MAX DEFAULT REACHED |'         
         BAS   RE,PRINTEM                                                       
         MVC   XP+45(37),=C'|PROGRAM WILL CONTINUE IN DRAFT MODE|'              
         MVC   XPSECOND+45(36),=C'|     NO POSTINGS WILL BE MADE     |'         
         MVC   XPTHIRD+45(36),=C'+----------------------------------+'          
         MVI   SPACING,4                                                        
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SALORIG (IN SAL TAPE HEADER) != ALPHAID (ACREPWORKC)                   
*----------------------------------------------------------------------         
         SPACE 1                                                                
ORIGERR  NTR1  BASE=*,LABEL=*                                                   
         MVI   RCPOSTNG,C'N'                                                    
         BAS   RE,HEADUP                                                        
         MVC   XP+45(35),=C'+---------------------------------+'                
         MVC   XPSECOND+45(35),=C'|ERROR-ORIGIN ON TAPE DIFFERS FROM|'          
         MVC   XPTHIRD+45(37),=C'|        REQUEST ORIGIN             |'         
         MVC   XPFOURTH+45(36),=C'|       TAPE ORIGIN ==>            |'         
         LA    R6,TAPEIO                                                        
*                                                                               
         USING SALRECD,R6                                                       
*                                                                               
         MVC   XPFOURTH+70(2),SALORIG                                           
         BAS   RE,PRINTEM                                                       
*                                                                               
         MVC   XP+45(37),=C'|        REQUEST ID  ==>            |'              
         MVC   XP+70(2),ALPHAID                                                 
         MVC   XPSECOND+45(36),=C'|     NO POSTINGS WILL BE MADE     |'         
         MVC   XPTHIRD+45(36),=C'+----------------------------------+'          
         MVI   SPACING,4                                                        
         BAS   RE,PRINTEM                                                       
         B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              PROCESS RECORD DUMPS, IF REQUESTED                               
*----------------------------------------------------------------------         
DMPGET   NTR1  BASE=*,LABEL=*                                                   
         AP    PDUMP,=P'1'                    R5 HAS THE ADDRESS OF             
         CP    PDUMP,MAXDUMP                  SRTREC                            
         BH    XIT                                                              
         LA    R2,TAPRLEN                                                       
         LA    R3,=C'GET'                                                       
*                                                                               
         GOTO1 PRNTBL,DMCB,(3,(R3)),(R5),C'DUMP',(R2),=C'2D',          X        
               (C'P',PRINT)                                                     
*              (C'XP',VBIGPRNT)                                                 
         B     XIT                                                              
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LH    R2,0(R3)                                                         
         LA    R4,=C'PUT'                                                       
*                                                                               
         GOTO1 PRNTBL,DMCB,(3,(R4)),(R3),C'DUMP',(R2),=C'2D',          X        
               (C'P',PRINT)                                                     
*              (C'XP',VBIGPRNT)                                                 
         B     XIT                                                              
*---------------------------------------------------------------------          
*        CONVERT SRTDATE  INTO A 2 BYTE MOS (IN SVMOS)                          
*                SVMOS  = YM WHERE                                              
*                         Y = C'0'-C'9'                                         
*                         M = C'0'-C'9' OR C'A'-C'C'                            
*                E.G. OCT/1991 = C'1A'                                          
*                     NOV/2016 = C'6B'                                          
*---------------------------------------------------------------------          
         SPACE 1                                                                
         USING SRTD,R5                                                          
         SPACE 1                                                                
CALCMOS  NTR1  BASE=*,LABEL=*                                                   
         LA    R5,SRTREC                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(1,SRTDATE),(0,WORK) YMD TO YYMMDD                   
*                                                                               
         MVC   SVMOS(1),WORK+1                ONE CHAR YEAR                     
         MVC   SVMOS+1(1),WORK+3              +MONTH                            
         CLI   WORK+2,C'1'                    MONTH LESS THAN 10                
         BNE   CALCMX                         YES                               
*                                                                               
         ZIC   R3,WORK+3                      THE LAST CHAR OF MONTH            
         LA    R3,1(R3)                       ADD 1                             
         STC   R3,SVMOS+1                                                       
         NI    SVMOS+1,X'CF'                  SET LOW ORDER TO C                
         B     CALCMX                                                           
*                                                                               
CALCMX   B     XIT                                                              
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              REFRESH HEADER INFO                                              
*----------------------------------------------------------------------         
HEADUP   NTR1  BASE=*,LABEL=*                                                   
         MVC   XHEAD3+75(8),=C'LIVE RUN'                                        
         CLI   RCPOSTNG,C'N'                                                    
         BNE   *+10                                                             
         MVC   XHEAD3+75(9),=C'DRAFT RUN'                                       
*                                                                               
         CLI   QOPT1,C'Y'          SAL TAPE                                     
         BE    HEAD20                                                           
         MVC   XHEAD1+72(14),=C'VENDOR JOURNAL'                                 
         MVC   XHEAD2+72(14),=C'--------------'                                 
         B     HEAD30                                                           
*                                                                               
HEAD20   MVC   XHEAD1+72(15),=C'PAYROLL JOURNAL'                                
         MVC   XHEAD2+72(15),=C'---------------'                                
         MVC   XHEAD4+122(10),=C'BATCH REF='                                    
         MVC   XHEAD4+132(6),HDBR                                               
*                                                                               
HEAD30   MVI   FORCEHED,C'Y'                                                    
         MVC   XHEAD3+10(36),SVCOMPNM                                           
         TM    RUNSTAT,SALTAPE     SAL TAPE                                     
         BO    HEAD40             YES, NO VENDOR IN HEADER                      
         MVC   XHEAD3+122(6),=C'VENDOR'                                         
         MVC   XHEAD3+129(14),VENNUM+1                                          
         MVC   XHEAD4+122(36),VENNAME          VENDOR NAME FOR HEADER           
*                                                                               
HEAD40   CLI   RCSUBPRG,0                     MAIN REPORT HEADERS               
         BNE   XIT                            NO, DONT PRINT OFFICE             
         CLI   PROGPROF+1,C'Y'                PRINT BY OFFICE?                  
         BNE   XIT                            NO, DONT PRINT OFFICE             
         MVC   XHEAD4+1(7),=C'OFFICE:'                                          
         MVC   XHEAD4+10(2),SVOFFICE                                            
         LA    R5,SRTREC                                                        
*                                                                               
         USING SRTD,R5                                                          
*                                                                               
         CLI   SRTDEF,X'FF'                                                     
         BNE   XIT                                                              
         MVC   XHEAD4+39(29),=C'POSTINGS MADE TO DEFAULT JOBS'                  
         B     XIT                                                              
         DROP  R5                                                               
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        CONSTANTS                                                              
*----------------------------------------------------------------------         
         SPACE 1                                                                
TAPE     DCB   DDNAME=TAPEIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=FB,                                               X        
               LRECL=200,                                              X        
               BLKSIZE=4000,                                           X        
               MACRF=GM,                                               X        
               EODAD=EOTAPE                                                     
*                                                                               
SNAPPLE  DCB   DDNAME=SNAPDD,                                          X        
               DSORG=PS,                                               X        
               RECFM=VBA,                                              X        
               LRECL=125,                                              X        
               BLKSIZE=1632,                                           X        
               MACRF=W                                                          
*                                                                               
         DS    0D                                                               
PRNTBL   DC    V(PRNTBL)                                                        
DATVAL   DC    V(DATVAL)                                                        
VBMONVAL DC    V(BMONVAL)                                                       
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,,)'                             
*                                                                               
MOAMSG   DS    0CL60                                                            
         DC    CL50'POSTING TO UNIT G AFTER THE G/L START DATE IS NOT '         
         DC    CL10'ALLOWED'                                                    
         DC    CL60'MOA DATE IS INVALID FOR POSTING'                            
         DC    CL60'MOA MONTH IS LOCKED FOR POSTING'                            
         DC    CL60'MOA DATE IS OUT OF RANGE FOR POSTING'                       
*                                                                               
ERRMSG   DS    0CL60                                                            
         DC    CL60'BAD ACCOUNT NUMBER FOR LEDGER J'                            
         DC    CL60'BAD ACCOUNT NUMBER FOR LEDGER V'                            
         DC    CL60'NO 32 ELEMENT FOUND ON LEDGER J'                            
         DC    CL60'NO 32 ELEMENT FOUND ON LEDGER V'                            
         DC    CL60'INVALID WORK CODE'                                          
         DC    CL60'ACCOUNT IS CLOSED OR LOCKED'                                
         DC    CL60'ERROR-DUPLICATE PAYMENT,NO POSTINGS MADE'                   
         DC    CL60'ERROR-INVALID DATE'                                         
*                                                                               
*        ERROR MESSAGES FOR SALARY JOURNAL POSTINGS                             
*                                                                               
SALMSG   DS    0CL60                                                            
         DC    CL60'POSTING ACCOUNT NOT FOUND'                                  
         DC    CL60'INVALID ACCOUNT FOR POSTING    '                            
         DC    CL60'INVALID CONTRA ACCOUNT'                                     
         DC    CL60'CANNOT MAKE THE THE 2D POSTING'                             
         DC    CL60'CANNOT MAKE THE THE 28 POSTING'                             
         DC    CL60'INVALID REFERENCE NUMBER   '                                
         DC    CL60'INVALID OFFICE'                                             
         DC    CL60'WARNING-INVALID DATE'                                       
*        SRT STAT ERRORS                                                        
SJERR    EQU   X'01'                                                            
SVERR    EQU   X'02'                                                            
NOACC32  EQU   X'04'                                                            
NOVEN32  EQU   X'08'                                                            
CODEERR  EQU   X'10'                                                            
BADSTAT  EQU   X'20'                                                            
BADINV   EQU   X'40'                          DUPLICATE INVOICE NUMBER          
BADDATE  EQU   X'80'                          BAD DATE                          
*        SAL TAPE ERRORS                                                        
BADACC   EQU   X'01'                                                            
BADPOST  EQU   X'02'                                                            
BADCAC   EQU   X'04'                                                            
BAD2D    EQU   X'08'                                                            
BAD28    EQU   X'10'                                                            
BADOFF   EQU   X'40'                                                            
*BADDATE  EQU   X'80'              BAD DATE (USE BADDATE EQUATE)                
PRNTSTAT DS    CL1                                                              
NEWJOB   EQU   X'01'                                                            
NEWTICK  EQU   X'02'                                                            
FIRSTJOB EQU   X'04'                                                            
LASTPRT  EQU   X'08'                                                            
PRTSJ    EQU   X'10'                                                            
PRTSV    EQU   X'20'                                                            
NEWOFF   EQU   X'40'                                                            
NEWCLI   EQU   X'80'                                                            
*                                                                               
*        MOA ERRORS                                                             
SVGLDAT  EQU   X'01'                          BAD DATE                          
SVINVMOA EQU   X'02'                          DATE INVALID                      
SVLCKMOA EQU   X'04'                          MOS LOCKED                        
SVRNGMOA EQU   X'08'                          DATE OUT OF RANGE                 
SVALLGL  EQU   X'0F'                          ALL ABOVE GL ERRORS               
*                                                                               
UNITG    EQU   C'G'                           UNIT - G                          
BCWORK   DS    XL10                                                             
SVCMPNY  DS    XL2                                                              
SVGLYMD  DS    XL3                            COMPANY GL START DATE             
         ORG   SVGLYMD                                                          
SVGLMOA  DS    XL2                            COMPANY GL START MOA              
SVGLDD   DC    XL1'01'                        COMPANY GL START DAY              
*                                                                               
         DS    0D                             DOUBLE WORD THE PACKED            
BCDUB    DS    D                                                                
BADCNT   DC    PL8'0'                         NUM OF BAD RECS - PL8             
GOODRECS DC    PL4'0'                         BECAUSE ITS USED IN A CVB         
DUMPCNT  DC    PL4'0'                                                           
PDUMP    DC    PL4'0'                                                           
MAXDUMP  DC    PL4'300'                                                         
MAXBAD   DC    PL4'2000'                                                        
MAXDEF   DC    PL4'50'                                                          
*                                                                               
*                                  DCB PARAMETERS FOR THE SALARY TAPE           
SALDCB   DS    0H                                                               
SALLRECL DC    H'61'                                                            
SALBLK   DC    H'24522'                                                         
CEBLK    DC    H'5063'             CE SAL TAPE BLOCKSIZE                        
         EJECT ,                                                                
*                                                                               
*----------------------------------------------------------------------         
*        TABLE OF DEFAULT JOBS FOR JWT OFFICES (N.Y. IS THE DEFAULT)            
*----------------------------------------------------------------------         
         SPACE 1                                                                
JWDEF    DS    0CL14                                                            
         DC    C'1 ',C'JWTNY TXXXXX'                                            
         DC    C'4 ',C'JWTAT TXXXXX'                                            
         DC    C'B ',C'JWTBC TXXXXX'                                            
         DC    C'6 ',C'JWTCH TXXXXX'                                            
         DC    C'5 ',C'JWTDE TXXXXX'                                            
         DC    C'F ',C'JWTFDATXXXXX'                                            
         DC    C'H ',C'JWTHI TXXXXX'                                            
         DC    C'2 ',C'JWTHC TXXXXX'                                            
         DC    C'3 ',C'JWTCHCTXXXXX'                                            
         DC    C'7 ',C'JWTSF TXXXXX'                                            
         DC    C'8 ',C'JWTLA TXXXXX'                                            
JWNUM    EQU   (*-JWDEF)/L'JWDEF                                                
*                                                                               
JWOFFTAB DS    0CL5                                                             
         DC    C'1 ',C'NYC'                                                     
         DC    C'6 ',C'CHI'                                                     
         DC    C'8 ',C'LA '                                                     
         DC    C'5 ',C'DET'                                                     
         DC    C'7 ',C'SF '                                                     
JWOFFNUM EQU   (*-JWOFFTAB)/L'JWOFFTAB                                          
*                                                                               
DEFTAB   DS    0C                                                               
         DC    C'BI',CL28'SE5202178001  SE5202178000',CL2'NY',PL2'0'            
         DC    C'CE',CL14'SB1104099',CL14'SCD210500',CL2'P ',PL2'10'            
         DC    C'CZ',CL14'SBZZZZ9999',CL14'S9B',CL2'01',PL2'0'                  
         DC    C'EZ',CL14'SBL5050201',CL14'SCC006',CL2'NA',PL2'0'               
         DC    C'GP',CL28'SB1038280D0100SB9PR000000000',CL2'20',PL2'0'          
         DC    C'H3',CL14'SB999999999999',CL14'S99',CL2'14',PL2'0'              
         DC    C'H7',CL14'SB2019130017',CL14'S99',CL2'DA',PL2'0'                
         DC    C'H7',CL14'SB2019130017',CL14'S99',CL2'J7',PL2'0'                
         DC    C'H7',CL14'SB2019130017',CL14'S99',CL2'MC',PL2'0'                
         DC    C'H7',CL14'SB2019130017',CL14'S99',CL2'MG',PL2'0'                
         DC    C'H7',CL14'SB2019130017',CL14'S99',CL2'MS',PL2'0'                
         DC    C'H7',CL14'SB2019130017',CL14'S99',CL2'CT',PL2'0'                
         DC    C'M2',CL14'SB913009',CL14'S99',CL2'MC',PL2'0'                    
         DC    C'M4',CL14'SBK400',CL14'SBK400',CL2'1A',PL2'0'                   
         DC    C'UM',CL28'SB1038280D0014SB9PR000000000',CL2'01',PL2'0'          
DEFENTRY DC    C'OA',CL14'SB34120',CL14'S9G',CL2'01',PL2'0'                     
DEFTABN  EQU   (*-DEFTAB)/(*-DEFENTRY)                                          
*                                                                               
         EJECT ,                                                                
         SPACE 1                                                                
         LTORG                                                                  
         EJECT ,                                                                
*----------------------------------------------------------------------         
*        SET UP BUFF FOR BUFFALO                                                
*----------------------------------------------------------------------         
         SPACE 1                                                                
         BUFF  LINES=1000,                                             X        
               ROWS=1,                                                 X        
               FLAVOR=PACKED,                                          X        
               COLUMNS=1,                                              X        
               KEYLIST=(26,A)                                                   
         EJECT ,                                                                
*--------------------------------------------------------------------*          
*        MAINTAB IS TABLE OF HOW GETMAIN CORE SHOULD BE SPLIT UP                
*--------------------------------------------------------------------*          
         SPACE 1                                                                
MAINTAB  DS    0F                                                               
         DC    S(ABAD)                                                          
         DC    H'0'                                                             
         DC    A(0)                                                             
         DC    A(BADSIZE)                                                       
         SPACE 1                                                                
         DC    S(APBUFF)                                                        
         DC    H'0'                                                             
         DC    A(0)                                                             
         DC    A(PBUFFLEN)                                                      
         SPACE 1                                                                
MAINNUM  EQU   (*-MAINTAB)/MAINLEN                                              
         SPACE 1                                                                
BADMAX   EQU   2001                                                             
BADSIZE  EQU   (BADMAX*RECOLEN)                                                 
         SPACE 1                                                                
PBUFFLEN EQU   4500                                                             
         SPACE 1                                                                
BUFSIZE  EQU   PBUFFLEN+BADSIZE                                                 
         EJECT ,                                                                
*----------------------------------------------------------------------         
*              DSECT FOR PROGRAM                                                
*----------------------------------------------------------------------         
         SPACE 1                                                                
ACAJD    DSECT                                                                  
RESAVE   DS    A                   AREA TO SAVE RE                              
ABUFF    DS    0A                  A(START OF GETMAINED SPACE                   
ABAD     DS    A                                                                
APBUFF   DS    A                                                                
BADADDR  DS    A                   POINTER FOR BAD REC TABLE                    
TODAY3   DS    CL3                 PACKED RCDATE                                
TODAY2   DS    XL2                 PACKED RCDATE                                
MOS      DS    XL2                 TRANSACTION MONTH OF SERVICE                 
SVKEY    DS    CL42                 AREA TO BUILD KEY                           
SVOFFICE DS    CL2                                                              
SVWC     DS    CL2                                                              
SVMOS    DS    CL2                 MONTH OF SERVICE IN 10/91 = 1A STYLE         
HDBR     DS    CL6                 BATCH REF IN HEADER ON SAL TAPE              
DR_OR_CR DS    CL1                 X'80' IF DR, ELSE NULL                       
DEPSW    DS    CL1                 Y, MAKE 2D/28 POSTINGS ON SAL EXP            
*                                                                               
ACCTERR  DS    CL1                                                              
NOTFOUND EQU   X'01'                                                            
NOPOST   EQU   X'02'                                                            
LOCKCLOS EQU   X'04'                                                            
*                                                                               
ACCTNUM  DS    CL15                                                             
ACCTNAME DS    CL36                                                             
*                                                                               
SALNUM   DS    CL15                PRIMARY ACCOUNT PASSED ON SAL TAPE           
SALACCN  DS    CL36                                                             
*                                                                               
SALCAC   DS    CL15                CONTRA ACCOUNT FROM SAL TAPE                 
SALCACN  DS    CL36                                                             
*                                                                               
PRDNUM   DS    CL15                S/J ACCOUNT                                  
PRDNAME  DS    CL36                                                             
*                                                                               
DEPNUM   DS    CL15                2/D ACCOUNT                                  
DEPNAME  DS    CL36                                                             
*                                                                               
CRDSNUM  DS    CL15                2/8 ACCOUNT                                  
CRDSNAME DS    CL36                                                             
*                                                                               
VENNUM   DS    CL15                S/V ACCOUNT                                  
VENNAME  DS    CL36                                                             
*                                                                               
DEFACC   DS    CL15                DEFAULT ACCOUNT FOR SAL TAPE                 
DEFACCN  DS    CL36                                                             
*                                                                               
DEFCAC   DS    CL15                DEFAULT CONTRA                               
DEFCACN  DS    CL36                                                             
*                                                                               
DEFOFF   DS    CL2                 DEFAULT OFFICE                               
*                                                                               
TEMPACC  DS    CL14                SPACE TO BUILD AN ACCOUNT                    
*                                                                               
POSTREC  DS    0C                  POSTING INTERFACE                            
POSTACC  DS    CL15                POSTING ACCOUNT                              
POSTCAC  DS    CL15                POSTING CONTRA ACCOUNT                       
POSTCACN DS    CL36                POSTING CONTRA NAME                          
POSTST   DS    CL1                 STATUS BYTE                                  
POSTOFF  DS    CL2                 POSTING OFFICE  FOR TRNSOFFC/ANAL            
POSTCASH DS    PL8                 AMOUNT TO POST                               
POSTANAL DS    CL2                 WORKCODE IN KEY                              
POSTRECL EQU   *-POSTREC                                                        
*                                                                               
PREVACCT DS    CL15                SAVED JOB                                    
PREVTICK DS    CL6                 SAVES TICKET NUMBER                          
PREVCACT DS    CL14                PREVIOUS CONTRA                              
PREVCAST DS    CL6                 STATUS OF PREVIOUS CONTRA                    
DATACOM  DS    CL8                 FOR DATAMGR CALLS                            
*                                                                               
PKBUCKS  DS    0PL8                                                             
PKJOBCNT DS    PL8                 POSTINGS PER JOB                             
PKJOBAMT DS    PL8                 DOLLARS POSTED PER JOB                       
PKOFFCNT DS    PL8                 POSTINGS PER OFFICE                          
PKOFFAMT DS    PL8                 DOLLARS POSTED PER OFFICE                    
PKCLICNT DS    PL8                 POSTINGS PER CLIENT                          
PKCLIAMT DS    PL8                 DOLLARS POSTED PER CLIENT                    
PKREPCNT DS    PL8                 POSTINGS PER REPORT                          
PKREPAMT DS    PL8                 DEBITS POSTED PER REP                        
TOTALDR  DS    PL8                                                              
SVREPAMT DS    PL8                 CREDITS POSTED PER REP                       
PKAMOUNT DS    PL8                 PACKED AMOUNT OF ITEM                        
SVTOTAL  DS    PL8                 ACCUM AMOUNTS FOR SV POST                    
POSTCNT  DS    PL8                 NUMBER OF RECORDS FOR POST TRAILER           
TAPECNT  DS    PL8                 NUMBER OF RECORDS READ                       
DEFCNT   DS    PL8                 NUMBER OF DEFAULT POSTINGS                   
BADTOT   DS    PL8                 DOLLAR TOTAL OF BAD RECS                     
DRTOTAL  DS    PL8                 POSTING TOTAL                                
CRTOTAL  DS    PL8                                                              
SALDRS   DS    PL8                 DEBITS ON SAL TAPE                           
SALCRS   DS    PL8                 CREDITS ON SAL TAPE                          
DEFDRS   DS    PL8                 DEFAULT DEBITS ON SAL TAPE                   
DEFCRS   DS    PL8                 DEFAULT CREDITS ON SAL TAPE                  
NUMBUCKS EQU   (*-PKBUCKS)/L'PKBUCKS                                            
BUCKLN   EQU   L'PKBUCKS                                                        
*                                                                               
YMD      DS    PL3                 PACKED DATE                                  
DEFYMD   DS    PL3                 PACKED DATE, EXTRACTED FROM HEADER           
*                                                                               
PRODLEDG DS    CL2                 COMPANY RECORD DATA                          
COMPSTAT DS    CL1                                                              
DEPTLEN  DS    CL1                                                              
OFFLEN   DS    CL1                                                              
*                                                                               
TVENKEY  DS    CL15                           TEMPORARY VENDOR KEY              
BUFFREC  DS    0CL34                          BUFFALO RECORD, FOR TABLE         
BUFFKEY  DS    0CL26                          BUFFALO KEY                       
BUFFINV  DS    CL6                            INVOICE NUMBER                    
BUFFNAME DS    CL20                           NARRATIVE IN TRAVEL ACC           
BUFFAMT  DS    PL8                            AMOUNT                            
*                                             OR SPACES                         
TAPEIO   DS    (SRTLN)C                     TAPE REC FROM SORT                  
SRTREC   DS    (SRTLN)C                                                         
SAVREC   DS    (SALHDLN)C                                                       
ID       DS    CL16                           FOR POSTING                       
         DS    F                                                                
ELCODE   DS    CL1                                                              
BIT      DS    CL1                                                              
BITSAVE  DS    CL1                            TO TEST BIT WITH                  
CODETYPE DS    CL1                            C OR N                            
*                                                                               
JWSTAT   DS    CL1                            STATUS FOR JW                     
GOODCLI  EQU   X'01'                          CLIENT IS GOOD, USE DEF           
*                                                                               
RUNSTAT  DS    CL1                 RUN PARAMETERS                               
DATEMYD  EQU   X'01'               DATE IS IN MMDDYY FORMAT                     
DATEMMM  EQU   X'02'               DATE IS IN MMDDDYY FORMAT                    
SALTAPE  EQU   X'04'               OMNY SALARY JOURNAL                          
*                                                                               
SALSTAT  DS    0CL2                                                             
SALSTAT1 DS    CL1                 ERRORS ON SAL TAPE DATA RECORD               
SALSTAT2 DS    CL1                 ERRORS ON SAL TAPE HEADER RECORD             
BADBM    EQU   1                                                                
BADREF   EQU   2                                                                
*                                                                               
ERRORS   DS    CL1                 WORK BYTE FOR PRTERR                         
*                                                                               
POSTSTAT DS    CL1                 POSTINGS TO MAKE                             
POST2D28 EQU   X'01'               MAKE ADDITIONAL 28/2D                        
POSTWORK EQU   X'02'               TRNOFFC IS WORKCODE                          
*                                                                               
COMMAND  DS    CL6                                                              
PREVOFF  DS    CL2                                                              
SAVELEN  DS    CL1                                                              
SVCOMPNM DS    CL36                           COMPANY NAME                      
SAVSBAC  DS    A                                                                
POSTHEAD DS    F                   RECORD HEADER                                
POSTAREA DS    CL200               AREA FOR POSTING                             
AREA     DS    CL255               ADDITIONAL AREA                              
RECORD   DS    0D                              SPACE FOR DATAMGR READ           
         DS    CL42                                                             
         DS    CL2000                                                           
WORKLN   EQU   *-ACAJD                                                          
         EJECT ,                                                                
         SPACE 1                                                                
TAPERECD DSECT                                                                  
TPREF    DS    CL6                                                              
TPDATE   DS    CL6                                                              
TPCLIENT DS    CL3                                                              
TPPROD   DS    CL3                                                              
TPJOB    DS    CL6                                                              
         ORG   TPCLIENT                                                         
TPSJCPJ  DS    CL12                                                             
TPCACT   DS    0CL14                                                            
TPUNLED  DS    CL2                            UNIT/LEDGER, SPACES=SV            
TPSUPLR  DS    CL12                                                             
TPCODES  DS    (8*CODEDLEN)C                  AREA FOR COMM CODES               
TPNARR   DS    CL35                           TAPE NARRATIVE                    
TPSPARE  DS    CL31                           SPACES ON END OF RECORD           
TAPRLEN  EQU   *-TAPERECD                     200 LRECL                         
         SPACE 3                                                                
CODED    DSECT                                FOR COMM CODES (AND N/C)          
COCODE   DS    CL2                            COMM WORK CODE                    
COAMOUNT DS   CL10                           COMM AMOUNT                        
CODEDLEN EQU   *-CODED                        LENGTH OF DSECT                   
         SPACE 3                                                                
DEFTABD  DSECT                             DEFAULT POSTING TABLE                
DEFTALP  DS    CL2                         AGENCY ALPHA                         
DEFTACC  DS    CL14                        DEFAULT ACCOUNT                      
DEFTCON  DS    CL14                        DEFAULT CONTRA ACCOUNT               
DEFTOFF  DS    CL2                         DEFAULT POSTING OFFICE               
DEFTMAX  DS    PL2                         DEFAULT MAX OVERRIDE                 
DEFTLEN  EQU   *-DEFTABD                                                        
         EJECT ,                                                                
         SPACE 1                                                                
SALRECD  DSECT                     TAPE RECORD FROM OACO AND FRIENDS            
SALREC   DS    0C                                                               
SALHEAD  DS    0C                  HEADER RECORD                                
SALBR    DS    CL4                 BATCH REF                                    
SALREF   DS    CL6                                                              
SALBM    DS    CL2                 YM BATCH MONTH                               
SALHDATE DS    0CL7                DEFAULT DATE                                 
SALHMMM  DS    CL3                                                              
SALHDD   DS    CL2                                                              
SALHYY   DS    CL2                                                              
SALNARR  DS    CL25                "PAYROLL JOURNAL"                            
SALORIG  DS    CL2                 TAPE ORIGIN/DESTINATION                      
SALHDLN  EQU   *-SALHEAD                                                        
*                                                                               
         ORG   SALREC                                                           
SALDATA  DS    0C                                                               
SALDC    DS    CL1                 DR OR CR                                     
SALACC   DS    CL14                SUBLEDGER + ACCOUNT                          
SALCACC  DS    CL14                CONTRA, S9G                                  
SALOFF   DS    CL2                 OFFICE, FOR 2D POSTINGS                      
SALDEPT  DS    CL3                 DEPT, FOR SE ENTRIES                         
SALAMNT  DS    CL12                ZONED DECIMAL AMOINT                         
SALCYCLE DS    CL5                                                              
SALDATE  DS    0CL10               MM/DD/YYYY                                   
SALMM    DS    CL2                                                              
         DS    CL1                                                              
SALDD    DS    CL2                                                              
         DS    CL1                                                              
         DS    CL2                 DECADE                                       
SALYY    DS    CL2                                                              
         EJECT ,                                                                
         SPACE 1                                                                
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTDEF   DS    X                                                                
SRTKOFF  DS    CL2                                                              
SRTKACCT DS    CL14                                                             
         ORG   SRTKACCT                                                         
SRTKJBLG DS    CL2                 LEDGER FOR JOB                               
SRTKJOB  DS    CL12                                                             
*                                                                               
         ORG   SRTKACCT                                                         
SRTKSEQ  DS    PL6                 RECORD NUMBER ON TAPE                        
         DS    CL8                                                              
*                                                                               
SRTKYMD  DS    PL3                 IS ON THE TAPE IF BADDATE                    
SRTKREF  DS    PL6                                                              
SRTKLN   EQU   *-SRTKEY                                                         
*                                                                               
SRTDATA  DS    0C                                                               
SRTACCT  DS    0CL14                                                            
SRTUL    DS    CL2                                                              
SRTACT   DS    CL12                                                             
         ORG   SRTACT                                                           
SRTCLI   DS    CL3                                                              
SRTPRO   DS    CL3                                                              
SRTJOB   DS    CL6                                                              
SRTCACT  DS    CL14                                                             
SRTTYPE  DS    CL1                                                              
SRTDC    DS    CL1                 DEBIT OR CREDIT                              
SRTREF   DS    CL6                                                              
SRTDATE  DS    CL3                                                              
SRTMOS   DS    CL2                 YM MOS                                       
SRTBR    DS    CL4                 BATCH REF                                    
SRTOFF   DS    0CL2                OFFICE                                       
SRTWC    DS    CL2                                                              
SRTDEPT  DS    CL6                                                              
SRTNARR  DS    CL36                NARRATIVE PASSED ON TAPE                     
SRTNAME  DS    CL36                ACCOUNT NAME, IF READ BEFORE SORT            
SRTCLNM  DS    CL36                CLIENT NAME, FOR VENDOR POSTING              
SRTSPARE DS    CL31                                                             
SRTSTAT  DS    X'00'               RECORD STATUS BYTE                           
SRTSTAT1 DS    X'00'               ANOTHER STATUS BYTE                          
SRTFLAG  DS    X'00'               DEFAULT JOB USED FLAG                        
DEFAULT  EQU   X'01'                                                            
TRAVEL   EQU   X'02'                                                            
SRTAMNT  DS    PL6                 AMOUNT                                       
SRTLN1   EQU   *-SRTKEY                                                         
*                                                                               
SRTCODES DS    (8*SRCOLN)C         TYPE 1 TAPE HAS 8 WC/AMOUNT BUCKETS          
SRTLN    EQU   *-SRTKEY                                                         
         EJECT ,                                                                
         SPACE 1                                                                
SRCOD    DSECT                                                                  
SRCCODE  DS    CL2                                                              
SRCCOMM  DS    CL1                                                              
SRCAMNT  DS    PL6                                                              
SRCOLN   EQU   *-SRCOD                                                          
         EJECT ,                                                                
         SPACE 1                                                                
RECOUTD  DSECT                                BAD RECORDS                       
REREF    DS    CL6                                                              
REDATE   DS    CL3                         PACKED DATE YYMMDD                   
REACCT   DS    CL14                                                             
RECAC    DS    CL14                                                             
REOFF    DS    CL2                                                              
REDEPT   DS    CL6                                                              
RECODE   DS    CL2                                                              
REAMOUNT DS    PL6                                                              
RENARR   DS    CL35                           NARRITIVE                         
RESTAT   DS    CL1                            RECORD STATUS                     
RECOLEN  EQU   *-RECOUTD                                                        
         EJECT ,                                                                
         SPACE 1                                                                
PLINED   DSECT                                DSECT FOR PRINT LINE              
PRSTART  DS    CL1                                                              
PRREF    DS    CL6                         REFERENCE                            
         DS    CL1                                                              
PRDATE   DS    CL8                                                              
         DS    CL2                                                              
PRACCN   DS    CL36                        ACCOUNT NAME                         
         DS    CL1                                                              
PRACC    DS    CL14                        ACCOUNT NUMBER                       
         DS    CL1                                                              
PRCODE   DS    CL2                                                              
         DS    CL3                                                              
PRSVWC   DS    CL2                                                              
         DS    CL2                                                              
PRDEB    DS    CL18                                                             
         DS    CL1                                                              
PRCRED   DS    CL18                                                             
         DS    CL5                                                              
PRNARR   DS    CL(L'XP-(*-PRSTART))        FOR NARRATIVE                        
         EJECT ,                                                                
         SPACE 1                                                                
PRTERRD  DSECT                              DSECT FOR ERRORS                    
         DS    CL1                                                              
PRERREF  DS    CL6                         REFERENCE                            
         DS    CL1                                                              
PRERDATE DS    CL8                         DATE                                 
         DS    CL2                                                              
PRERCPJ  DS    CL14                        CLIENT PRODCUT JOB                   
         DS    CL23                                                             
PRERVEN  DS    CL14                        VENDOR                               
         DS    CL1                                                              
PRERCODE DS    CL2                         CODE                                 
         DS    CL21                                                             
PRERAMNT DS    CL14                        AMOUNT                               
         DS    CL6                                                              
PRERNARR DS    CL(L'XP-(*-PRTERRD))        FOR NARRATIVE                        
         EJECT ,                                                                
*-------------------------------------------------------------------*           
* DSECT FOR MAIN TAB, A TABLE WHICH LOOPS THRU THE STORAGE GETMAIN              
*        GETS                                                                   
*-------------------------------------------------------------------*           
         SPACE 1                                                                
MAIND    DSECT                                                                  
MAINAST  DS    S                   ADDRESS TO STORE A(TABLE)                    
         DS    H                   SPACER FOR FULL ALLIGNMENT                   
MAINMAX  DS    A                                                                
MAINSIZE DS    A                                                                
MAINLEN  EQU   *-MAIND                                                          
*                                                                               
         SPACE 3                                                                
*        DCBD IS A MACRO WHICH GENERATES A IAHDCB, WHICH COVERS THE             
*        CODE GENERATED BY THE DCB MACRO                                        
         DCBD  DSORG=PS,DEVD=TA                                                 
         EJECT ,                                                                
         SPACE 1                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
* ACGENPOST                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
* ACREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
* ACGENMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
* LOGOD                                                                         
         PRINT OFF                                                              
       ++INCLUDE DDLOGOD                                                        
         PRINT ON                                                               
* REPMASTD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
* REMOTED                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDREMOTED                                                      
         PRINT ON                                                               
* REPXTRAD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDREPXTRAD                                                     
         PRINT ON                                                               
* ACOFFALD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACOFFALD                                                       
         PRINT ON                                                               
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
* ACBMONVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
* ACBIGPRNTD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066ACREPAJ02 05/29/20'                                      
         END                                                                    
