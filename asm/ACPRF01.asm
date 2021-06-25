*          DATA SET ACPRF01    AT LEVEL 004 AS OF 01/27/04                      
*PHASE T63001A                                                                  
T63001   TITLE 'ACPRF01 - PRESTO FALINK INTERFACE - MIMICS MF ZOOM'             
******** CLONED FROM ACPRO37                                                    
T63001   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACPRF01*,R7                                                    
*                                                                               
         LR    RC,R1                                                            
         USING WORKD,RC                                                         
*                                                                               
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
*                                                                               
         USING TSARD,TSARBLK                                                    
*                                                                               
         MVI   OVFLAG,0                                                         
*                                                                               
* VERSION CHECK                                                                 
         CLC   VERSION,=X'02070007'      VERSION 2.7.0.7 & HIGH ONLY            
         BNL   INIT                       ALLOWED!                              
         LA    R4,FAMSGBLK                                                      
         USING FAMSGD,R4                                                        
         MVC   FAMSGXTR(4),=X'02070007'                                         
         GOTO1 AADDDATA,DMCB,AFABLK,FALAYRF,FALAYRF                             
         B     EXIT                                                             
*                                                                               
INIT     MVI   DELMTER,C'`'              SET DELIMITER                          
         CLC   VERSION,=X'02070015'      VERSION 2.7.0.21 & HIGH ONLY           
         BNL   *+8                                                              
         MVI   DELMTER,C'|'              OLD DELIMITER                          
*                                                                               
         CLI   SVRCVEL+1,X'02'           ZOOM                                   
         BE    RCV02H                                                           
*                                                                               
         CLI   SVRCVEL+1,X'05'           CHECK LOGGED IN                        
         BE    RCV05H                                                           
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
* RECEIVE 02 HEADER - ZOOM                                                      
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
RCV02H   DS    0H                                                               
*                                                                               
         LA    R1,X'0002'                                                       
         BAS   RE,SENDH                                                         
*                                                                               
         BAS   RE,READALL                                                       
         MVC   ERROR,=Y(QNODATA)         NO DATA TO DISPLAY                     
         TM    OVFLAG,SENTDTL                                                   
         BNO   SNDERMSG                                                         
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
* READ RECORDS AND DETERMINE WHETHER PURCHASE ORDER OR TRANSACTION              
* AND BRANCH TO APPROPRIATE ROUTINE TO READ ELEMENTS                            
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
*                                                                               
READALL  NTR1                                                                   
         LA    R3,0                                                             
*                                                                               
         LA    R6,KEY                                                           
         USING ACKEYD,R6                                                        
         XC    KEY,KEY             READ FIRST RECORD                            
         MVC   ACKEYACC(3),CUL                                                  
         MVC   ACKEYACC+3(12),CPJ                                               
         GOTO1 HIGH                                                             
         B     REALL060                                                         
*                                                                               
REALL020 MVC   KEY,SEQKEY          GET KEY OF LAST RECORD                       
         GOTO1 HIGH                RE-READ IT                                   
*                                                                               
REALL040 GOTO1 SEQ                 GET NEXT RECORD                              
         LA    R6,KEY                                                           
*                                                                               
REALL060 CLC   KEYSAVE(L'ACKEYACC),KEY                                          
         BNE   REALLX                                                           
         MVC   SEQKEY,KEY          SAVE KEY FOR RE-READ                         
*                                                                               
         CLC   ACKEYWRK,=C'99'     DON'T WANT BILLING                           
         BE    REALL040                                                         
         CLC   ACKEYWRK,=C'**'     IF NOT AN ORDER, SEE IF WE                   
         BE    READPOS              WANT THIS WORK CODE                         
         CLC   ACKEYWRK,QWORK                                                   
         BNE   REALL040                                                         
         B     READTRNS                                                         
*                                                                               
REALL080 XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
*                                                                               
         LA    R1,SACCT+1          ACCOUNT                                      
         MVI   LENGTH,L'SACCT-1                                                 
         BAS   RE,SNDCODE                                                       
*                                                                               
         LA    R1,SACCTNAM         ACCOUNT NAME                                 
         MVI   LENGTH,L'SACCTNAM                                                
         BAS   RE,SNDDATA                                                       
*                                                                               
         GOTO1 VDATCON,DMCB,(1,SDATE),(10,WORK)        DATE                     
         LA    R1,WORK                                                          
         MVI   LENGTH,8                                                         
         BAS   RE,SNDDATA                                                       
*                                                                               
         LA    R1,SNUMB            REF #                                        
         MVI   LENGTH,L'SNUMB                                                   
         BAS   RE,SNDCODE                                                       
*                                                                               
         CLI   MAPCODE,MCOPNPO           SENDING OPEN PURCHASE ORDERS?          
         BNE   REALL090                                                         
*                                                                               
         LA    R1,SAUTHOR          AUTHORIZER                                   
         MVI   LENGTH,L'SAUTHOR                                                 
         BAS   RE,SNDDATA                                                       
         B     REALL100                                                         
*                                                                               
REALL090 LA    R1,SBATCH           BATCH                                        
         MVI   LENGTH,L'SBATCH                                                  
         BAS   RE,SNDCODE                                                       
*                                                                               
         ZAP   DUB,PA$NETBL        BILLED AMOUNT                                
         BAS   RE,SNDPNUM                                                       
*                                                                               
         LA    R1,SDESC            DESCRIPTION                                  
         MVI   LENGTH,L'SDESC                                                   
         BAS   RE,SNDDATA                                                       
*                                                                               
REALL100 ZAP   DUB,SAMOUNT         TRANSACTION AMOUNT                           
         BAS   RE,SNDPNUM                                                       
*                                                                               
*&&UK                                                                           
         CLC   SAUTH,SPACES                                                     
         BE    REALL110                                                         
         LA    R1,SAUTH                                                         
         MVI   LENGTH,L'SAUTH                                                   
         BAS   RE,SNDDATA                                                       
*&&                                                                             
*                                                                               
REALL110 BCTR  R4,0                                                             
         MVI   0(R4),C' '                REMOVE LAST DELIMITTER                 
*                                                                               
         ZIC   R1,MAPCODE                                                       
         LA    R5,BLOCK                                                         
         SR    R5,R4                     OUTPUT DATA LENGTH                     
         BCTR  R5,0                                                             
         LPR   R5,R5                                                            
         LA    R4,BLOCK                                                         
         BRAS  RE,SENDD                                                         
*                                                                               
         MVC   AIO,AIO1                  SWAP BUFFERS BACK                      
         B     REALL020                  RE-READ THIS ONE                       
*                                                                               
REALLX   B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* READ P.O.'S FOR A PARTICULAR JOB AND SAVE IN STORAGE FOR DISPLAY.             
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
READPOS  MVI   MAPCODE,MCOPNPO     SENDING OPEN PURCHASE ORDERS                 
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'44'        FIND THE TRANSACTION ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   REALL040            GET NEXT RECORD IF NONE                      
*                                                                               
         L     RE,AIO              SKIP DRAFT TRANSACTIONS                      
         USING TRNRECD,RE                                                       
         TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT TRANSACTION ?                
         BO    REALL040            YES, SKIP IT                                 
         DROP  RE                                                               
*                                                                               
         USING TRANSD,R6                                                        
REPOS010 MVC   SDATE,TRNSDATE      MOVE DATE AND REFERENCE                      
         MVC   SNUMB,TRNSREF                                                    
         ZAP   SAMOUNT,=P'0'       CLEAR AMOUNT FIELD                           
         MVI   SWT68,C' '          AND 68 INDICATOR                             
*                                                                               
         MVI   ELCODE,X'68'        GET ORDER AMOUNT ELEMENT                     
         BAS   RE,GETELIO                                                       
         BNE   REALL020            NO ORDER AMOUNT ELEMENT, GET NEXT            
*                                                                               
         USING ACOAMTD,R6                                                       
REPOS020 CLC   ACOAWC,QWORK        LOOK FOR WORKCODE                            
         BNE   REPOS070                                                         
         MVI   SWT68,C'Y'          INDICATE WE HAVE AN ELEMENT                  
         B     REPOS060                                                         
*                                                                               
REPOS060 AP    SAMOUNT,ACOAMT      YES, ADD TO BUCKET                           
         SP    SAMOUNT,ACOAIVAL    OPEN AMOUNT = ORDER AMT - INVOICED           
*                                                                               
REPOS070 BAS   RE,NEXTEL           AND GET THE NEXT ELEMENT                     
         BE    REPOS020                                                         
         CLI   SWT68,C'Y'                                                       
         BNE   REALL020                                                         
*                                                                               
REPOS100 MVC   AIO,AIO2            SWAP BUFFERS FOR THIS                        
         LA    R6,KEY                                                           
         USING ACKEYD,R6                                                        
         MVC   ACKEYDTE(ACRECORD-ACKEYDTE),SPACES                               
         XC    ACKEYD+36(6),ACKEYD+36                                           
         GOTO1 HIGH                READ FOR SUB-ACCOUNT                         
         CLC   KEYSAVE(ACKEYREF-ACKEYD),KEY                                     
         BE    *+6                                                              
         DC    H'0'                WE MUST FIND IT                              
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'43'        GET THE HEADER ELEMENT NOW                   
         BAS   RE,GETELIO2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSUBHD,R6                                                       
REPOS120 DS    0H                                                               
         MVC   SACCT,TRSBACNT      GET THE SUB-ACCOUNT NUMBER                   
         MVC   SACCTNAM,SPACES                                                  
         CLC   TRSBLEN,=YL1(TRSBNAME-TRSBEL)                                    
         BE    REPOS130                                                         
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=YL2(TRSBNAME-TRSBEL)                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SACCTNAM(0),TRSBNAME   SAVE NAME FOR NEXT ENTRY                  
*                                                                               
REPOS130 DS    0H                                                               
         LA    R6,KEY                                                           
         USING ACOKEY,R6                                                        
         XC    ACOKEY,ACOKEY       NOW READ ORDER RECORD                        
         MVI   ACOKCODE,ACOKCODQ                                                
         MVC   ACOKCOMP,CUL                                                     
         MVC   ACOKNUM,SNUMB                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(ACOKNUM-ACOKEY+6),KEY                                    
         BNE   REPOS140                                                         
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'67'        GET THE ORDER ELEMENT                        
         BAS   RE,GETELIO2                                                      
         BNE   REPOS140                                                         
         USING ACORDRD,R6                                                       
         MVC   SAUTHOR(L'ACORAUTH),ACORAUTH  SAVE AUTHORIZATION                 
REPOS140 B     REALL080                                                         
         EJECT                                                                  
***********************************************************************         
* READ TRANSACTIONS FOR A PARTICULAR JOB                                        
***********************************************************************         
READTRNS DS    0H                                                               
         MVI   MAPCODE,MCTRADT     SENDING TRANSACTION DETAIL                   
*                                                                               
         L     R6,AIO                                                           
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'44'        FIND THE TRANSACTION ELEMENT                 
         BAS   RE,GETELIO                                                       
         BNE   REALL040            GET NEXT RECORD IF NONE                      
*                                                                               
         L     RE,AIO              SKIP DRAFT TRANSACTIONS                      
         USING TRNRECD,RE                                                       
         TM    TRNRSTAT,TRNSDRFT   IS THIS A DRAFT TRANSACTION ?                
         BO    REALL040            YES, SKIP IT                                 
         DROP  RE                                                               
*                                                                               
         GOTO1 VPRORATA,DMCB,AIO,0,ACOMFACS,0,PROBLOCK                          
*                                                                               
         USING TRANSD,R6                                                        
RETRN020 MVC   SDATE,TRNSDATE      SAVE DATA                                    
         MVC   SNUMB,TRNSREF                                                    
         MVC   SBATCH,TRNSBTCH                                                  
         MVC   SAMOUNT,TRNSAMNT                                                 
*                                                                               
*&&US                                                                           
         TM    JOBJSTAT,ACJBXJOB   IS THIS AN X-JOB ?                           
         BZ    *+8                 NO                                           
         BAS   RE,GETXAMT          YES, GET AMOUNT FROM X'50' ELEMENT           
*&&                                                                             
*                                                                               
         MVC   SDESC,SPACES              SAVE DESCRIPTION                       
         SR    RF,RF                                                            
         IC    RF,TRNSLEN                LENGTH OF WHOLE ELEMENT                
         SH    RF,=AL2(TRNSNARR-TRNSEL)  SUBTRACT LEN OF REST OF ELEMNT         
         BZ    RETRN040                  NO DESCRIPTION                         
         CH    RF,=H'20'                 SEND A MAX OF 20 CHARACTERS            
         BNH   *+8                                                              
         LA    RF,20                                                            
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SDESC(0),TRNSNARR                                                
*                                                                               
RETRN040 DS    0H                                                               
*&&UK                                                                           
         MVC   SAUTH,SPACES              CLEAR SAUTH FIELD                      
         MVC   BYTE,TRNSSTAT             SAVE STATUS BYTE                       
*                                                                               
         MVI   ELCODE,X'25'                                                     
         BAS   RE,GETELIO                                                       
         BNE   RETRN120                                                         
*                                                                               
         MVC   SAUTH(4),=C'AUTH'                                                
         TM    BYTE,X'08'                                                       
         BO    *+10                                                             
         MVC   SAUTH(6),=C'UNAUTH'                                              
*                                                                               
         USING ACNOD,R6                                                         
RETRN070 LA    RE,SAUTH+6                                                       
         CLI   0(RE),C' '                                                       
         BH    *+8                                                              
         BCT   RE,*-8                                                           
         MVC   2(4,RE),=C'ORD='                                                 
         SR    R1,R1                                                            
         IC    R1,ACNOLEN                                                       
         SH    R1,=Y(ACNONUM-ACNOD+1)                                           
         EX    R1,*+4                                                           
         MVC   6(0,RE),ACNONUM                                                  
*                                                                               
         CLC   SDESC,SPACES        IF SDESC ISN'T FILLED IN MOVE                
         BNE   RETRN120            SAUTH TO SDESC FIELD                         
         MVC   SDESC,SAUTH                                                      
         MVC   SAUTH,SPACES                                                     
*                                                                               
*&&                                                                             
RETRN120 MVC   AIO,AIO2            SWAP BUFFERS FOR THIS                        
         LA    R6,KEY                                                           
         USING ACKEYD,R6                                                        
         MVC   ACKEYDTE(ACRECORD-ACKEYDTE),SPACES                               
         XC    ACKEYD+36(6),ACKEYD+36                                           
         GOTO1 HIGH                READ FOR SUB-ACCOUNT                         
         CLC   KEYSAVE(ACKEYREF-ACKEYD),KEY                                     
         BE    *+6                                                              
         DC    H'0'                WE MUST FIND IT                              
         GOTO1 GETREC                                                           
*                                                                               
         MVI   ELCODE,X'43'        GET THE HEADER ELEMENT NOW                   
         BAS   RE,GETELIO2                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRSUBHD,R6                                                       
RETRN140 MVC   SACCT,TRSBACNT      GET THE SUB-ACCOUNT NUMBER                   
*                                                                               
         MVC   SACCTNAM,SPACES                                                  
         CLC   TRSBLEN,=YL1(TRSBNAME-TRSBEL)                                    
         BE    REALL080                                                         
         SR    R1,R1                                                            
         IC    R1,TRSBLEN                                                       
         SH    R1,=YL2(TRSBNAME-TRSBEL)                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SACCTNAM(0),TRSBNAME   PUT OUT NAME NOW                          
         B     REALL080                                                         
         DROP  R6                                                               
*                                                                               
* SUB-ROUTINE TO GET AMOUNT FROM X'50' ELEMENT FOR X-JOBS                       
*                                                                               
*&&US                                                                           
         USING TRCASHD,R6                                                       
GETXAMT  NTR1  ,                                                                
         MVI   ELCODE,TRCSELQ                                                   
         BAS   RE,GETELIO                                                       
         BE    GETXAMT4                                                         
         B     GETXAMTX            USE TRANSACTION AMOUNT IF NO X'50'           
*                                                                               
GETXAMT2 SR    RF,RF                                                            
         IC    RF,1(R6)                                                         
         AR    R6,RF                                                            
         CLI   0(R6),0             END OF RECORD ?                              
         BE    GETXAMTX            YES                                          
         CLI   0(R6),TRCSELQ       NO, X'50' ELEMENT ?                          
         BNE   GETXAMT2            NO, KEEP LOOKING                             
*                                                                               
GETXAMT4 CLI   TRCSTYPE,C'S'       IS THIS THE EXPENSE AMOUNT ?                 
         BNE   GETXAMT2            NO                                           
         MVC   SAMOUNT,TRCSAMNT    YES, SAVE IT                                 
*                                                                               
GETXAMTX B     EXIT                                                             
         DROP  R6                                                               
*&&                                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
* RECEIVE 05 HEADER - CHECK LOGGED IN                                           
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *             
RCV05H   DS    0H                                                               
*                                                                               
         LA    R1,X'0005'                                                       
         BAS   RE,SENDH                                                         
*                                                                               
         LA    R1,MCRTNCD                NO, MARK END OF REV HISTORY            
         SR    R5,R5                                                            
         BAS   RE,SENDD                                                         
*                                                                               
         B     EXIT                                                             
         EJECT                                                                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* ON ENTRY R1 CONTAINS HEADER CODE                                              
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDH    LR    R0,RE                                                            
         GOTO1 GETHDR              GET HEADER ADDRESS                           
         GOTO1 ASETELEM,DMCB,AFABLK,HDRADDR,0,0                                 
SENDHX   LR    RE,R0                                                            
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PARMS ARE FABLK,MAP_TABLE_ENTRY,A(DATA),OVRD_LEN                              
* ON ENTRY R1 CONTAINS DATA ITEM NUMBER WITHIN CURRENT ELEMENT                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SENDD    LR    R0,RE                                                            
         GOTO1 GETDATA             GET DATA ITEM                                
         GOTO1 AADDDATA,DMCB,AFABLK,DATADDR,(R4),(R5)                           
         SR    R5,R5               CLEAR OVERRIDE LENGTH                        
         LR    RE,R0                                                            
         OI    OVFLAG,SENTDTL                                                   
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* PUT DATA IN THE BUFFER, IF ANY                                                
* CHECK IF DATA SENDING HAS A DELIMITER IN IT                                   
* BACK UP TO LAST NONBLANK AND INSERT DELIMITER                                 
* ON ENTRY R1 POINTS TO THE DATA SENDING                                        
*          R4 POINTS TO SPOT IN BUFFER TO PUT DATA                              
*          RE RETURN ADDRESS                                                    
*          LENGTH OF DATA SENDING                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDCODE  MVI   CODE,C'Y'                   DATA IS A CODE                       
*                                                                               
* MOVE DATA TO THE BUFFER POINTED TO BY R4                                      
SNDDATA  SR    RF,RF                       LENGTH OF DATA                       
         ICM   RF,1,LENGTH                                                      
         BZ    SNDDLM                      NO DATA                              
         MVI   LENGTH,0                    CLEAR FOR NEXT TIME                  
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   0(0,R4),0(R1)               MOVE DATA TO BUFFER                  
         LR    R1,R4                       POINT R1 TO START OF DATA            
         AR    R4,RF                       POINT R4 TO THE END OF DATA          
         AHI   RF,1                                                             
*                                                                               
CHK10    CLC   0(1,R1),DELMTER             REPLACE "`" WITH " ", IF ANY         
         BNE   CHK20                         IN THE DATA                        
         CLI   CODE,C'Y'                   WAS DATA A CODE?                     
         BE    CHK30                                                            
         MVI   0(R1),C' '                                                       
CHK20    AHI   R1,1                                                             
         BRCT  RF,CHK10                                                         
         MVI   CODE,C' '                   CLEAR CODE SETTING                   
         B     SNDDLM                                                           
CHK30    MVC   ERROR,=Y(QINVCODE)          UNLESS THIS IS A CODE, THEN          
         B     SNDERMSG                       SEND AN ERROR                     
*                                                                               
SNDDLM   CLI   0(R4),C' '                  REMOVE TRAILING SPACES               
         JH    *+8                                                              
         BRCT  R4,SNDDLM                                                        
         MVC   1(1,R4),DELMTER                                                  
         AHI   R4,2                                                             
         BR    RE                                                               
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
* TRANSMIT NUMERIC VALUE IN DUB AND INSERT SEMICOLON DELIMITER                  
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *         
SNDPNUM  DS    0H                                                               
         EDIT  (P8,DUB),(9,WORK),2,ALIGN=LEFT                                   
         CP    DUB,=P'0'           IS IT NEGATIVE?                              
         BNL   *+12                                                             
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         MVC   0(9,R4),WORK        AMOUNT                                       
         BCTR  R0,0                                                             
         AR    R4,R0                                                            
         B     SNDDLM                                                           
         EJECT                                                                  
*                                                                               
SNDERMSG GOTO1 SENDMSG                                                          
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),56,ELCODE                                                   
GETELIO2 L     R6,AIO2                                                          
         GETEL2 (R6),56,ELCODE                                                  
*                                                                               
EXIT     XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
       ++INCLUDE ACPRFWRK                                                       
WORKD    DSECT                                                                  
         ORG   MEDWORK                   480 BYTES IN OVWORK                    
*                                                                               
OVFLAG   DS    X                                                                
SENTDTL  EQU   X'80'                                                            
DELMTER  DS    C                   CHARACTER TO DELIMIT DOWNLOAD                
LENGTH   DS    X                   LENGTH OF DATA TO DOWNLOAD                   
CODE     DS    C                   Y = DATE IS A CODE                           
*                                                                               
SACCT    DS    CL15                SUB OR CONTRA ACCOUNT NUMBER                 
SACCTNAM DS    CL36                SUB OR CONTRA ACCOUNT NAME                   
SDATE    DS    PL3                 DATE                                         
SNUMB    DS    CL6                 INVOICE OR REFERENCE NUMBER                  
SBATCH   DS    CL6                 BATCH NUMBER                                 
SAUTHOR  DS    CL20                AUTHORIZER                                   
SDESC    DS    CL20                DESCRIPTION                                  
SEQKEY   DS    CL48                SAVE SEQUENTIAL KEY WHILE READING            
SAUTH    DS    CL20                AUTH AND ORDER MATCHING DETA                 
*                                                                               
SWT68    DS    C                                                                
STOTAL   DS    CL20                                                             
SAMOUNT  DS    PL6                 AMOUNT                                       
*                                                                               
MAPCODE  DS    X                   MAPCODE SENDING                              
PROBLOCK DS    0C                                                               
       ++INCLUDE ACPRORATAD                                                     
*INCLUDED DSECTS                                                                
*DDGLOBEQUS                                                                     
*DDGLVXCTLD                                                                     
*DDTSARD                                                                        
*FAFACTS                                                                        
*ACGENFILE                                                                      
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
       ++INCLUDE DDGLVXCTLD                                                     
       ++INCLUDE DDTSARD                                                        
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACPRF01   01/27/04'                                      
         END                                                                    
