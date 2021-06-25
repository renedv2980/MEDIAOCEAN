*          DATA SET SPSFM76    AT LEVEL 001 AS OF 05/11/11                      
*PHASE T21776A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T21776  -- AUTOPAY CLEARANCE MAINTENANCE & LIST      *         
*                                                                     *         
*  COMMENTS:     MAINTAINS AUTOPAY CLEARANCE RECORDS                  *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21700), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM2E (MAINT) & SCSFM2D (LIST)              *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- SECOND BASE                                    *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21776 - AUTOPAY CLEARANCE MAINTENANCE AND LIST'                
T21776   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1776**,R7,RR=R3                                              
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BAS   RE,SETUP            SET UP VALUES                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BE    VK                  YES                                          
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                  YES                                          
         CLI   MODE,DISPKEY        DISPLAY KEY?                                 
         BE    DK                  YES                                          
         CLI   MODE,DISPREC        DISPLAY RECORD?                              
         BE    DR                  YES                                          
         CLI   MODE,LISTRECS       LIST RECORDS?                                
         BE    LR                  YES                                          
         CLI   MODE,SETFILE        SET FILES?                                   
         BE    SF                  YES                                          
*                                                                               
XIT      XIT1                      EXIT                                         
***********************************************************************         
*                       SET FILE                                      *         
***********************************************************************         
SF       BAS   RE,SSV                                                           
         B     XIT                                                              
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       XC    KEYVALS,KEYVALS     CLEAR KEY VALUES                             
         BAS   RE,RSV              RESET SYSTEM VALUES                          
*                                                                               
         LA    R2,MNTMEDH          MEDIA HEADER                                 
         CLI   5(R2),0             ANY INPUT?                                   
         BE    ERRMIS              NO - ERROR                                   
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIMED             VALIDATE MEDIA                               
         MVC   KEYAGYMD,BAGYMD     A/M                                          
         MVC   MNTMDN,MEDNM        MEDIA NAME                                   
         OI    MNTMDNH+6,X'80'     TRANSMIT                                     
*                                                                               
         LA    R2,MNTCLTH          CLIENT HEADER                                
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK30                YES                                          
         B     ERRMIS              NO - ERROR                                   
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALICLT             VALIDATE CLIENT                              
         MVC   KEYCLT,BCLT         BINARY CLIENT                                
         MVC   KEYOFC,SVOFFC       CLIENT OFFICE                                
         MVC   MNTCLN,CLTNM        CLIENT NAME                                  
         OI    MNTCLNH+6,X'80'     TRANSMIT                                     
*                                                                               
VK30     LA    R2,MNTPRDH          PRODUCT HEADER                               
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK35                YES                                          
         B     ERRMIS              NO - ERROR                                   
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIPRD             VALIDATE PRODUCT                             
         MVC   KEYPRD,QPRD         PRODUCT                                      
         MVC   MNTPDN,PRDNM        PRODUCT NAME                                 
         OI    MNTPDNH+6,X'80'     TRANSMIT                                     
*                                                                               
VK35     LA    R2,MNTPTRH          PARTNER HEADER                               
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK40                NO                                           
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIPRD             VALIDATE PRODUCT                             
         MVC   KEYPTR,QPRD         PRODUCT                                      
         MVC   MNTPRN,PRDNM        PRODUCT NAME                                 
         OI    MNTPRNH+6,X'80'     TRANSMIT                                     
*                                                                               
VK40     LA    R2,MNTESTH          ESTIMATE HEADER                              
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK50                YES                                          
         B     ERRMIS              NO - ERROR                                   
         XC    BEST,BEST           CLEAR BINARY ESTIMATE                        
         CLC   =C'000',8(R2)       ESTIMATE 000?                                
         BE    VK50                YES                                          
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIEST             VALIDATE ESTIMATE                            
         MVC   KEYEST,BEST         BINARY ESTIMATE                              
         L     R6,AIO              R6 = A(ESTIMATE RECORD)                      
         USING ESTHDR,R6           ESTIMATE HEADER DSECT                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,MNTESD)                               
         MVI   MNTESD+8,C'-'       ESTIMATE START AND END DATES                 
         GOTO1 DATCON,DMCB,(0,EEND),(10,MNTESD+9)                               
         OI    MNTESDH+6,X'80'     TRANSMIT                                     
         DROP  R6                  DROP R6                                      
*                                                                               
VK50     LA    R2,MNTSTAH          STATION HEADER                               
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK60                YES                                          
         B     ERRMIS              NO - ERROR                                   
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALISTA             VALIDATE STATION                             
         MVC   KEYSTA,BSTA         BINARY STATION                               
         MVC   KEYMKT,BMKT         BINARY MARKET                                
         MVC   MNTMKN,MKTNM        MARKET NAME                                  
         OI    MNTMKNH+6,X'80'     TRANSMIT                                     
*                                                                               
VK60     LA    R2,MNTDATH          DATE HEADER                                  
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK70                YES                                          
         B     ERRMIS              NO - ERROR                                   
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         CLC   WORK,=C'000000'     DATE INPUT VALID?                            
         BE    ERRINV              NO                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,KEYDATE)                                 
         XC    KEYDATE,=X'FFFF'    COMPLIMENT THE DATE                          
*                                                                               
VK70     LA    R2,MNTMOSH          MOS HEADER                                   
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   *+16                YES                                          
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BE    VK80                YES                                          
         B     ERRMIS              NO - ERROR                                   
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         CLC   WORK,=C'000000'     DATE INPUT VALID?                            
         BE    ERRINV              NO                                           
         GOTO1 DATCON,DMCB,(0,WORK),(2,KEYDATE2)                                
*                                                                               
VK80     LA    R2,MNTREPH          REP HEADER                                   
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK90                NO                                           
         MVC   KEYREP,8(R2)        YES - MOVE IN REP                            
         OC    KEYREP,SPACES       SPACE PAD IT                                 
*                                                                               
VK90     BAS   RE,SSV              CHANGE TO XSPFILE                            
         XC    KEY,KEY             CLEAR THE KEY                                
         LA    R1,KEY              R1 = KEY                                     
         USING APXKEY,R1           AUTOPAY RECORD DSECT                         
*                                                                               
         MVI   APXKTYP,APXKTYPQ    X'0D'                                        
         MVI   APXKSUB,APXKSUBQ    X'3A'                                        
         MVC   APXKDATE,KEYDATE    DATE OF I2 RUN                               
         OC    APXKDATE,APXKDATE   IS THERE A DATE                              
         BZ    *+10                NO THEN DON'T BOTHER FILLING IN A/M          
         MVC   APXKAGMD,KEYAGYMD   A/M                                          
         MVC   APXKCLT,KEYCLT      CLIENT                                       
         MVC   APXKPRD,KEYPRD      PRODUCT                                      
         MVC   APXKPTN,KEYPTR      PARTNER                                      
         MVC   APXKEST,KEYEST      ESTIMATE                                     
         MVC   APXKMKT,KEYMKT      MARKET                                       
         MVC   APXKSTA,KEYSTA      STATION                                      
         MVC   APXKMON,KEYDATE2    MOS                                          
         MVC   APXKREP,KEYREP      REP                                          
         MVC   APXKOFC,KEYOFC      OFFICE                                       
         DROP  R1                  DROP R1                                      
*                                                                               
         MVC   SAVEKEY,KEY         SAVE THE KEY                                 
         MVC   AIO,AIO1            USE AIO1 FOR AUTOPAY RECORD                  
*                                                                               
VKX      B     XIT                 EXIT                                         
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       L     R6,AIO              A(AUTOPAY RECORD)                            
         MVI   ELCODE,X'01'        X'01' ELEMENT                                
         BAS   RE,GETEL            HAVE ONE?                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         USING APXEL,R6            ALPHA ELEMENT DSECT                          
*                                                                               
         XC    APXPAYER,APXPAYER   NO PAYER                                     
         LA    R2,MNTPAYH          PAYER/REQUESTOR                              
         CLI   5(R2),0             ANY INPUT?                                   
         BE    *+10                NO                                           
         MVC   APXPAYER,MNTPAY     SAVE THE PAYER TO THE X'01' ELEMENT          
*                                                                               
         XC    APXPAID,APXPAID     NO PAID DATE                                 
         LA    R2,MNTPDTH          PAID DATE                                    
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR80                NO                                           
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    0(4,R1),0(R1)       VALID INPUT?                                 
         BZ    ERRINV              NO                                           
         GOTO1 DATCON,DMCB,WORK,(2,APXPAID)                                     
*                                                                               
VR80     MVI   APXERRS,0           DEFAULT TO NO ERRORS                         
         LA    R2,MNTERRH          ERROR FIELD                                  
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR90                NO                                           
         GOTO1 HEXIN,DMCB,8(R2),WORK,2                                          
         OC    DMCB+12(4),DMCB+12  ANY ERRORS?                                  
         BZ    ERRINV              YES                                          
         MVC   APXERRS,WORK        NO - SAVE THE AUTOPAY ERRORS                 
         DROP  R6                  DROP R6                                      
*                                                                               
VR90     BAS   RE,SSV              CHANGE TO XSPFILE                            
         MVC   AIO,AIO1            RESTORE AIO TO AUTOPAY RECORD                
*                                                                               
VRX      B     DR                  REDISPLAY RECORD                             
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       BAS   RE,CLRDATA          CLEAR DATA FIELDS                            
*                                                                               
         L     R6,AIO              A(AUTOPAY RECORD)                            
         USING APXEL,R6            AUTOPAY RECORD DSECT                         
         MVI   ELCODE,APXELQ       X'01' ELEMENT                                
         BAS   RE,GETEL            HAVE A X'01'ELEMENT?                         
         BE    *+6                 YES                                          
         DC    H'0'                NO - DEATH                                   
*                                                                               
         MVC   MNTDME,APXMED       MEDIA                                        
         MVC   MNTDCL,APXCLT       CLIENT                                       
         MVC   MNTDPR,APXPRD       PRODUCT                                      
         MVC   MNTDP2,APXPTN       PARTNER                                      
         MVC   MNTDES,APXEST       ESTIMATE                                     
         MVC   MNTDST,APXSTA       STATION                                      
         MVC   MNTMKT,APXMKT       MARKET                                       
         MVC   MNTDMO,APXMONTH     MONTH                                        
         MVC   MNTSREP,APXSREP     REP                                          
*                                                                               
         MVC   MNTPAY,APXPAYER     PAYER                                        
         GOTO1 DATCON,DMCB,(2,APXPAID),(11,MNTPDT)                              
         GOTO1 HEXOUT,DMCB,APXERRS,MNTERR,1,=C'TOG'                             
         DROP  R6                  DROP R6                                      
*                                                                               
DR20     LA    R2,MNTIV1H          FIRST INVOICE                                
         LA    R5,MNTI13H          LAST INVOICE                                 
         L     R6,AIO              A(AUTOPAY RECORD)                            
         USING APXIEL,R6           INVOICE ELEMENT DSECT                        
         MVI   ELCODE,APXIELQ      X'10'                                        
         BAS   RE,GETEL            HAVE AN ELEMENT?                             
         B     *+8                 BRANCH OVER NEXTEL                           
*                                                                               
DR25     BAS   RE,NEXTEL           HAVE ANOTHER X'10' ELEMENT?                  
         BNE   DRX                 NO - DONE                                    
         MVC   8(10,R2),APXIINV    INVOICE                                      
         LLC   R1,0(R2)            LENGTH TO NEXT FIELD                         
         AR    R2,R1               A(NEXT FIELD)                                
         EDIT  APXIDOLS,(10,8(R2)),2  GROSS DOLLARS                             
         LLC   R1,0(R2)            LENGTH TO NEXT FIELD                         
         AR    R2,R1               A(NEXT FIELD)                                
         EDIT  APXINET,(10,8(R2)),2   NET DOLLARS                               
         LLC   R1,0(R2)            LENGTH TO NEXT FIELD                         
         AR    R2,R1               A(NEXT FIELD)                                
         EDIT  APXITAX,(10,8(R2)),2   TAX DOLLARS                               
         LLC   R1,0(R2)            LENGTH TO NEXT FIELD                         
         AR    R2,R1               A(NEXT FIELD)                                
         CR    R2,R5               LAST INVOICE?                                
         BE    DR25                YES - DON'T BUMP SCREEN!                     
         LA    R0,2                BUMP 2 FIELDS DOWN                           
         LLC   R1,0(R2)            LENGTH TO NEXT FIELD                         
         AR    R2,R1               A(NEXT FIELD)                                
         BCT   R0,*-8              BUMP 2 FIELDS DOWN                           
         B     DR25                PROCESS NEXT INVOICE ELEMENT                 
         DROP  R6                  DROP R6                                      
*                                                                               
DR90     CLI   ACTNUM,ACTCHA       ACTION = CHANGE?                             
         BE    DR100               YES - RESTORE THE RECORD                     
         CLI   ACTNUM,ACTSEL       SAVED ACTION = SEL?                          
         BNE   DRX                                                              
         CLI   LISTSEL,C'C'        SEL ACTION = CHANGE?                         
         BNE   DRX                                                              
*                                                                               
DR100    MVC   KEY(32),SAVEKEY     RESTORE THE KEY                              
         MVC   AIO,AIO2            USE AIO2                                     
         GOTO1 HIGH                READ HIGH                                    
         GOTO1 GETREC              GET THE AUTOPAY RECORD                       
         MVC   AIO,AIO1            RESTORE AIO1                                 
*                                                                               
DRX      B     XIT                 EXIT                                         
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       BAS   RE,RSV              CHANGE TO SPOTFILE                           
*                                                                               
         L     R6,AIO              A(AUTOPAY RECORD)                            
         USING APXRECD,R6          AUTOPAY RECORD DSECT                         
*                                                                               
         MVC   MNTMED,APXKAGMD     CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    MNTMED,X'0F'        TURN OFF AGENCY PORTION                      
         LA    R5,MEDTAB           MEDIA TABLE                                  
*                                                                               
DK30     CLC   MNTMED,1(R5)        MATCH ON HEX MEDIA?                          
         BE    DK40                YES                                          
         LA    R5,MEDTABLQ(R5)     NO - BUMP TO NEXT TABLE ENTRY                
         CLI   1(R5),X'FF'         END OF TABLE?                                
         BNE   DK30                NO                                           
         DC    H'0'                YES - DEATH                                  
*                                                                               
DK40     MVC   MNTMED,0(R5)        DISPLAY 1 CHAR MEDIA                         
         MVI   MNTMEDH+5,1         SET INPUT LENGTH MANUALLY                    
         LA    R2,MNTMEDH          MEDIA HEADER                                 
         MVI   USEIONUM,2          USE AIO2                                     
         GOTO1 VALIMED             VALIDATE THE MEDIA                           
         MVC   MNTMDN,MEDNM        DISPLAY MEDIA NAME                           
         OI    MNTMDNH+6,X'80'     TRANSMIT                                     
*                                                                               
         XC    MNTCLT,MNTCLT       CLEAR THE CLIENT ON SCREEN                   
         GOTO1 CLUNPK,DMCB,APXKCLT,MNTCLT                                       
         MVI   MNTCLTH+5,3         INPUT LENGTH OF 3                            
         CLI   MNTCLT+2,C' '       THIRD CHARACTER <= SPACE?                    
         BH    *+8                 NO                                           
         MVI   MNTCLTH+5,2         YES - INPUT LENGTH OF 2                      
         LA    R2,MNTCLTH          CLIENT HEADER                                
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALICLT             VALIDATE THE CLIENT                          
         MVC   MNTCLN,CLTNM        DISPLAY CLIENT NAME                          
         OI    MNTCLNH+6,X'80'     TRANSMIT                                     
*                                                                               
         XC    MNTPRD,MNTPRD       CLEAR THE PRODUCT                            
         MVC   MNTPRD,APXKPRD      GET PRODUCT MNMONIC                          
         MVI   MNTPRDH+5,3         INPUT LENGTH OF 3                            
         CLI   MNTPRD+2,C' '       THIRD CHARACTER <= SPACE?                    
         BH    *+8                 NO                                           
         MVI   MNTPRDH+5,2         YES - INPUT LENGTH OF 2                      
         LA    R2,MNTPRDH          PRODUCT HEADER                               
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIPRD             VALIDATE THE PRODUCT                         
         MVC   MNTPDN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    MNTPDNH+6,X'80'     TRANSMIT                                     
*                                                                               
         XC    MNTPTR,MNTPTR       CLEAR THE PARTNER                            
         OC    APXKPTN,APXKPTN     HAVE A PARTNER?                              
         BZ    DK60                NO                                           
         MVC   MNTPTR,0(R5)        GET PRODUCT MNMONIC                          
         MVI   MNTPTRH+5,3         INPUT LENGTH OF 3                            
         CLI   MNTPTR+2,C' '       THIRD CHARACTER <= SPACE?                    
         BH    *+8                 NO                                           
         MVI   MNTPTRH+5,2         YES - INPUT LENGTH OF 2                      
         LA    R2,MNTPTRH          PRODUCT HEADER                               
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIPRD             VALIDATE THE PRODUCT                         
         MVC   MNTPRN,PRDNM        DISPLAY PRODUCT NAME                         
         OI    MNTPRNH+6,X'80'     TRANSMIT                                     
*                                                                               
DK60     XC    MNTEST,MNTEST       CLEAR THE ESTIMATE                           
         EDIT  APXKEST,(L'MNTEST,MNTEST),FILL=0                                 
         MVI   MNTESTH+5,3         INPUT LENGTH OF 3                            
         MVI   MNTESTH+4,X'08'     VALID NUMERIC                                
         OI    MNTESTH+6,X'80'     TRANSMIT                                     
         LA    R2,MNTESTH          ESTIMATE HEADER                              
         XC    BEST,BEST           CLEAR THE BINARY ESTIMATE                    
         CLC   =C'000',8(R2)       ESTIMATE = 000?                              
         BE    DK62                YES - VALID                                  
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALIEST             VALIDATE THE ESTIMATE                        
*                                                                               
         L     R2,AIO              A(ESTIMATE RECORD)                           
         USING ESTHDR,R2           ESTIMATE RECORD DSECT                        
         GOTO1 DATCON,DMCB,(0,ESTART),(10,MNTESD)                               
         MVI   MNTESD+8,C'-'       "-" IN BETWEEN DATES                         
         GOTO1 DATCON,DMCB,(0,EEND),(10,MNTESD+9)                               
         OI    MNTESDH+6,X'80'     TRANSMIT                                     
         DROP  R2                                                               
*                                                                               
DK62     GOTO1 MSUNPK,DMCB,APXKMKT,FULL,MNTSTA                                  
         LA    R2,MNTSTAH          STATION HEADER                               
         MVI   MNTSTAH+5,5         INPUT LENGTH OF 5                            
         OI    MNTSTAH+6,X'80'     TRANSMIT                                     
         MVI   USEIONUM,2          USE I/O2 FOR VALIDATION CALLS                
         GOTO1 VALISTA             VALIDATE THE STATION                         
         MVC   MNTMKN,MKTNM        MARKET NAME                                  
         OI    MNTMKNH+6,X'80'     TRANSMIT                                     
*                                                                               
         MVC   HALF,APXKDATE       DATE                                         
         XC    HALF,=X'FFFF'       UNCOMPLMENT THE DATE                         
         GOTO1 DATCON,DMCB,(2,HALF),(10,MNTDAT)                                 
         OI    MNTDATH+6,X'80'     TRANSMIT                                     
         GOTO1 DATCON,DMCB,(2,APXKMON),(10,MNTMOS)                              
         OI    MNTMOSH+6,X'80'     TRANSMIT                                     
         MVC   MNTREP,APXKREP      REP                                          
         OI    MNTREPH+6,X'80'     TRANSMIT                                     
*                                                                               
         BAS   RE,SSV              CHANGE TO XSPFILE                            
         MVC   KEY(32),0(R6)       RESTORE KEY                                  
         MVC   AIO,AIO1            RESTORE AIO1                                 
         MVC   LISTSEL,THISLSEL    SELECTED ACTION ON LIST                      
*                                                                               
DKX      B     XIT                 EXIT                                         
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       LA    R6,KEY              R6 = KEY                                     
         USING APXRECD,R6          A(AUTOPAY RECORD)                            
         OC    KEY,KEY             FIRST TIME THROUGHT?                         
         BNZ   LR10                NO                                           
*                                                                               
         XC    KEY,KEY             CLEAR THE KEY                                
         MVC   KEY(32),SAVEKEY     SAVED KEY                                    
*                                                                               
LR10     GOTO1 HIGH                READ HIGH                                    
         B     LR30                GO TEST KEY                                  
*                                                                               
LR20     LA    R6,KEY              R6 = KEY                                     
         GOTO1 SEQ                 READ SEQ                                     
*                                                                               
LR30     CLC   KEY(2),KEYSAVE      X'0D3A'?                                     
         BNE   LRX                 NO - DONE                                    
         CLC   APXKAGMD,KEYAGYMD   SAME A/M?                                    
         BNE   LR20                NO - READ SEQ                                
         OC    KEYDATE,KEYDATE     ANYTHING IN DATE (FILTER)?                   
         BZ    *+14                NO                                           
         CLC   APXKDATE,KEYDATE    YES - DATE FILTER MATCHES?                   
         BNE   LR20                NO - READ SEQ                                
         OC    KEYCLT,KEYCLT       ANYTHING IN CLIENT (FILTER)?                 
         BZ    *+14                NO                                           
         CLC   APXKCLT,KEYCLT      YES - CLIENT FILTER MATCHES?                 
         BNE   LR20                NO - READ SEQ                                
         OC    KEYPRD,KEYPRD       ANYTHING IN PRODUCT (FILTER)?                
         BZ    *+14                NO                                           
         CLC   APXKPRD,KEYPRD      YES - PRODUCT FILTER MATCHES?                
         BNE   LR20                NO - READ SEQ                                
         OC    KEYPTR,KEYPTR       ANYTHING IN PARTNER (FILTER)?                
         BZ    *+14                NO                                           
         CLC   APXKPTN,KEYPTR      YES - PARTNER FILTER MATCHES?                
         BNE   LR20                NO - READ SEQ                                
         CLC   =C'000',MNTEST      ESTIMATE = 000?                              
         BE    *+14                YES                                          
         OC    KEYEST,KEYEST       ANYTHING IN ESTIMATE (FILTER)?               
         BZ    *+14                NO                                           
         CLC   APXKEST,KEYEST      YES - ESTIMATE FILTER MATCHES?               
         BNE   LR20                NO - READ SEQ                                
         OC    KEYSTA,KEYSTA       ANYTHING IN STATION (STARTING PT)?           
         BZ    *+14                NO                                           
         CLC   APXKSTA,KEYSTA      YES - STATION < KEY STATION?                 
         BL    LR20                YES - READ SEQ                               
         OC    KEYDATE2,KEYDATE2   ANYTHING IN MOS (FILTER)?                    
         BZ    *+14                NO                                           
         CLC   APXKMON,KEYDATE2    YES - DATE FILTER MATCHES?                   
         BNE   LR20                NO - READ SEQ                                
         OC    KEYREP,KEYREP       ANYTHING IN REP FILTER?                      
         BZ    *+14                NO                                           
         CLC   APXKREP,KEYREP      YES - REP FILTER MATCHES?                    
         BNE   LR20                NO - READ SEQ                                
*                                                                               
         MVC   LISTAR,SPACES       CLEAR LISTAR                                 
         GOTO1 GETREC              GET THE RECORD                               
*                                                                               
         L     R6,AIO              A(AUTOPAY RECORD)                            
*                                                                               
         MVC   LSMED,APXKAGMD      CONVERT BAGYMD INTO 1 CHAR MEDIA             
         NI    LSMED,X'0F'         TURN OFF AGENCY PORTION                      
         LA    R5,MEDTAB           MEDIA TABLE                                  
*                                                                               
LR40     CLI   1(R5),X'FF'         END OF TABLE?                                
         BE    ERRMED              YES                                          
         CLC   LSMED,1(R5)         MATCH ON MEDIA BITS?                         
         BE    LR50                YES                                          
         LA    R5,MEDTABLQ(R5)     NEXT ITEM IN MEDIA TABLE                     
         B     LR40                LOOP BACK AND CHECK NEXT ENTRY               
*                                                                               
LR50     MVC   LSMED,0(R5)         PUT 1 CHAR MEDIA INTO LIST LINE              
*                                                                               
         GOTO1 CLUNPK,DMCB,APXKCLT,LSCLT                                        
*                                                                               
         MVC   LSPRD,APXKPRD       PRODUCT CODE                                 
         MVC   LSPRD2,APXKPTN PARTNER                                           
*                                                                               
         EDIT  APXKEST,(L'LSEST,LSEST),FILL=0                                   
*                                                                               
         MVC   HALF,APXKDATE       I2 RUN DATE                                  
         XC    HALF,=X'FFFF'       UNCOMPLIMENT                                 
         GOTO1 DATCON,DMCB,(2,HALF),(10,LSDATE)                                 
*                                                                               
         MVC   BMKTSTA,APXKMKT     MKT/STA                                      
         GOTO1 MSUNPK,DMCB,BMKTSTA,FULL,LSSTA                                   
         DROP  R6                  DROP R6                                      
*                                                                               
         L     R6,AIO              A(AUTOPAY RECORD)                            
         USING APXEL,R6            ALPHA ELEMENT DSECT                          
         MVI   ELCODE,APXELQ       ELEMENT CODE X'01'                           
         BAS   RE,GETEL            HAVE A X'01' ELEMENT?                        
         BNE   ERR01               NO - ERROR                                   
         MVC   LSMOY,APXMONTH      MMM/YY                                       
         MVC   LSSREP,APXSREP      REP                                          
*                                                                               
         CLI   APXERRS,0           ANY ERRORS?                                  
         BE    LR60                NO                                           
         GOTO1 HEXOUT,DMCB,APXERRS,LSERR,1,=C'TOG'                              
*                                                                               
LR60     OC    APXPAID,APXPAID     ANY PAID DATE?                               
         BZ    LR70                NO                                           
         GOTO1 DATCON,DMCB,(2,APXPAID),(11,LSPAYDT)                             
         DROP  R6                  DROP R6                                      
*                                                                               
LR70     L     R6,AIO              A(AUTOPAY RECORD)                            
         USING APXIEL,R6           INVOICE ELEMENT DSECT                        
         MVI   ELCODE,APXIELQ      X'10' INVOICE ELEMENT                        
         BAS   RE,GETEL            HAVE AN INVOICE ELEMENT?                     
         BNE   LR150               NO                                           
         MVC   LSINV(10),APXIINV   YES - MOVE IT IN                             
         BAS   RE,NEXTEL           HAVE ANOTHER ONE?                            
         BNE   LR150               NO                                           
         MVI   LSINV+10,C'+'       YES - MOVE A "+" IN                          
         DROP  R6                  DROP R6                                      
*                                                                               
LR150    GOTO1 LISTMON             MOVE THE RECORD TO THE LIST LINE             
         B     LR20                GO READ SEQ                                  
*                                                                               
LRX      B     XIT                 EXIT                                         
***********************************************************************         
*         CLEARS DATA FIELDS ON MAINTENANCE SCREEN                    *         
***********************************************************************         
CLRDATA  NTR1                                                                   
         LA    R2,MNTDMEH          MEDIA FIELD                                  
         LA    R3,MNTLAST          CLEAR UP TO LAST FIELD ON THE SCREEN         
*                                                                               
CLRD10   TM    1(R2),X'20'         PROTECTED?                                   
         BO    CLRD20              YES - SKIP                                   
         LLC   R1,0(R2)            FIELD LENGTH                                 
         AHI   R1,-9               MINUS HEADER + 1 FOR EX                      
         BM    CLRD20              MUST NOT BE NEGATIVE                         
         EX    R1,*+8              EXECUTE                                      
         B     *+10                FOR IDF                                      
         XC    8(0,R2),8(R2)       CLEAR FIELD                                  
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
CLRD20   LLC   R1,0(R2)            FIELD LENGTH                                 
         AR    R2,R1               BUMP TO NEXT FIELD                           
         CR    R2,R3               < LAST FIELD?                                
         BL    CLRD10              YES - KEEP CLEARING SCREEN                   
*                                                                               
CLRDATAX B     XIT                 END OF CLEAR DATA FIELDS                     
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         OI    GENSTAT1,USKYMRG    USE KEYMERGE ON LIST SCREENS                 
         OI    GENSTAT1,NOSETEFH   DON'T SET EFH ADDR'S (APPL DOES IT)          
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL?                                
         BNE   SETUPX              NO                                           
         CLI   MODE,VALKEY         VALIDATE KEY?                                
         BNE   SETUPX              NO                                           
         CLI   ACTEQU,ACTADD       ACTION ADD?                                  
         BNE   SETUPX              NO                                           
         CLI   MNTMED,C'A'         MEDIA A?                                     
         BNE   SETUPX              NO                                           
         BAS   RE,SSV              CHANGE TO XSPFILE                            
         XC    KEY,KEY             CLEAR THE KEY                                
         MVI   KEY,APXKTYPQ        X'0D'                                        
         MVI   KEY+1,APXKSUBQ      X'3A'                                        
*                                                                               
         GOTO1 HIGH                READ HIGH                                    
         CLC   KEY(13),KEYSAVE     HAVE AN AUTOPAY RECORD ON FILE?              
         BE    SETUPX              YES                                          
         MVC   KEY,KEYSAVE         RESTORE THE DUMMY KEY                        
*                                                                               
         XC    ELEM,ELEM           CLEAR THE ELEMENT                            
         MVI   ELEM,APXELQ         X'01'                                        
         MVI   ELEM+1,APXLNQ       ELEMENT LENGTH                               
         L     R6,AIO              A(AIO)                                       
         MVC   0(L'KEY,R6),KEY     SET RECORD KEY                               
         GOTO1 ADDELEM             ADD THE DUMMY X'01' ELEMENT                  
         GOTO1 ADD                 ADD THE RECORD                               
*                                                                               
SETUPX   B     XIT                 EXIT                                         
***********************************************************************         
*                     SET SYSTEM VALUES                               *         
***********************************************************************         
SSV      MVC   LKEY,=H'32'         XSPFILE KEY LENGTH = 32                      
         MVC   LSTATUS,=H'4'       XSPFILE LENGTH OF STATUS                     
         MVC   DATADISP,=H'42'     XSPFILE DISPLACEMENT INTO FIRST ELEM         
         MVC   SYSFIL,=C'XSPFIL  ' FILE = XSPFILE                               
         MVC   SYSDIR,=C'XSPDIR  ' DIRECTORY = XSPDIR                           
         BR    RE                  RETURN TO CALLER                             
***********************************************************************         
*                   RESET SYSTEM VALUES                               *         
***********************************************************************         
RSV      MVC   LKEY,=H'13'         SPTFILE KEY LENGTH = 32                      
         MVC   LSTATUS,=H'1'       SPTFILE LENGTH OF STATUS                     
         MVC   DATADISP,=H'24'     SPTFILE DISPLACEMENT INTO FIRST ELEM         
         MVC   SYSFIL,=C'SPTFIL  ' FILE = SPTFILE                               
         MVC   SYSDIR,=C'SPTDIR  ' DIRECTORY = SPTDIR                           
         BR    RE                  RETURN TO CALLER                             
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
ERR01    XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREA                           
         MVC   CONHEAD(28),=C'ERROR - ALPHA ELEM NOT FOUND'                     
         B     ERRX2               GO REPORT ERROR                              
*                                                                               
ERRMED   XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREA                           
         MVC   CONHEAD(37),=C'ERROR - END OF MEDIA TABLE IS REACHED'            
         B     ERRX2               GO REPORT ERROR                              
*                                                                               
ERRCHA   XC    CONHEAD,CONHEAD     CLEAR MESSAGE AREA                           
         MVC   CONHEAD(40),=C'ERROR - DATA MUST BE SAME AS THAT OF KEY'         
ERRX2    OI    CONHEADH+6,X'80'    TRANSMIT MESSAGE AREA                        
         GOTO1 ERREX2              GO REPORT ERROR                              
*                                                                               
ERRINV   MVI   ERROR,INVALID       ERROR = INVALID                              
         B     VSFMERR             GO REPORT ERROR                              
ERRMIS   MVI   ERROR,MISSING       ERROR = MISSING                              
         B     VSFMERR             GO REPORT ERROR                              
*                                                                               
VSFMERR  MVC   AIO,AIO1            AIO = AIO1                                   
         GOTO1 ERREX               GO REPORT ERROR                              
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
***********************************************************************         
*        TABLE FOR MEDIA AND ELEMENT CODES                            *         
***********************************************************************         
MEDTAB   DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
MEDTABLQ EQU   *-MEDTAB                                                         
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM2ED          MAINTENACE SCREEN                            
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM2DD          LIST SCREEN                                  
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE SPGENXAPY         AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE SPGENEST          FOR ESTIMATE DATES                           
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
* START OF SAVED STORAGE (6144)                                                 
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
SAVEKEY  DS    CL32                SAVED KEY                                    
LISTSEL  DS    CL1                 FOR USE WITH LIST SELECTIONS                 
*                                                                               
KEYVALS  DS    0XL(KEYVLENQ)                                                    
KEYAGYMD DS    XL1                 WORKING STORAGE FOR KEY PARTS                
KEYCLT   DS    XL2                                                              
KEYPRD   DS    XL3                                                              
KEYPTR   DS    XL3                                                              
KEYEST   DS    XL1                                                              
KEYMKT   DS    XL2                                                              
KEYSTA   DS    XL3                                                              
KEYDATE  DS    XL2                                                              
KEYDATE2 DS    XL2                                                              
KEYREP   DS    XL3                                                              
KEYOFC   DS    XL1                                                              
KEYVLENQ EQU   *-KEYAGYMD                                                       
*                                                                               
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
*                                                                               
         EJECT                                                                  
GEND     DSECT                                                                  
         ORG   LISTAR              LABELS FOR LISTMON                           
LSMED    DS    CL1                                                              
LSERR    DS    CL2                                                              
         DS    CL1                                                              
LSCLT    DS    CL3                                                              
         DS    CL1                                                              
LSPRD    DS    CL3                                                              
         DS    CL1                                                              
LSPRD2   DS    CL3                                                              
         DS    CL1                                                              
LSEST    DS    CL3                                                              
         DS    CL1                                                              
LSSTA    DS    CL8                                                              
         DS    CL1                                                              
LSMOY    DS    CL6                                                              
         DS    CL1                                                              
LSSREP   DS    CL4                                                              
         DS    CL2                                                              
LSPAYDT  DS    CL8                                                              
         DS    CL2                                                              
LSINV    DS    CL11                                                             
         DS    CL1                                                              
LSDATE   DS    CL8                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPSFM76   05/11/11'                                      
         END                                                                    
