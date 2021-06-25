*          DATA SET SPSFM52    AT LEVEL 122 AS OF 11/04/20                      
*PHASE T21752A                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        T21752  -- ESTIMATE MAINTENANCE                      *         
*                                                                     *         
*  COMMENTS:     MAINTAINS ESTIMATE RECORDS                           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T21900), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM72 (MAINT)                               *         
*                                                                     *         
*  OUTPUTS:                                                           *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- RECORD                                         *         
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
* USER    JIRA       DATE                  CHANGE LOG                 *         
* ---- ----------  -------- ----------------------------------------- *         
* AKAT SPEC-38510  10/11/19 FCONTROL SUPPORT                          *         
* AKAT SPEC-33510  05/14/19 IF DAILY FLAG IS SET TO Y AND THE USER    *         
*                           CHANGES POL EST > 53 DAYS, DON'T UPDATE   *         
*                           THE BRANDS BEFORE GIVING THE ERROR        *         
*                           ALSO ENFORCE POL DAILY = BRAND DAILY      *         
* AKAT SPEC-33311  04/16/19 FORCE N-RATE FOR CANADA STARTING DEC30/19 *         
***********************************************************************         
*                                                                     *         
* JUL06/17 MHER   UNPROTECT USER FIELDS IN SCRGEN                     *         
*                 AND PROTECT THEM IN THIS PROGRAM SO UPLOAD WORKS    *         
* MAR21/14 AKAT - DONT ALLOW COS2 IF CLT COS2=T/O                     *         
* APR26/13 AKAT - ADD/MAINTAIN MEDIA C ESTIMATES FOR P&G              *         
* AUG30/07 AKAT - CLEAR WORKING STORAGE BECAUSE FIELDS LIKE OWSDAY    *         
*               - ARE ORGED ONTO OTHER SFM OVERLAYS AND CAN BE SET    *         
*               - BY SAY SPSFM53 (SEE OFFSET OF OUTDATE). IF WE DON'T *         
*               - EXPLICITLY CLEAR SYSSPARE THEN GARBAGE REMAINS!     *         
* JUN01/06 AKAT - NEW SPOT LENGTH VALIDATION                          *         
***********************************************************************         
         TITLE 'T21752 - ESTIMATE MAINTENANCE'                                  
T21750   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1752**,R7,RR=R3                                              
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
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*        CLI   MODE,RECDEL         DELETE RECORD                                
*        BE    DEL                                                              
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,XRECADD        RECORD JUST ADDED?                           
         BNE   *+8                 NO                                           
         BRAS  RE,XADD             YES - ADD PASSIVE                            
         CLI   MODE,XRECPUT        RECORD JUST CHANGED?                         
         BNE   *+8                 NO                                           
         BRAS  RE,XCHG             YES - CHANGE/ADD PASSIVE AS NEEDED           
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
*                                                                               
VK       LA    R3,KEY                                                           
         USING ESTHDR,R3                                                        
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,X'00'                                                   
*                                                                               
         MVC   ESTMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    ESTMEDNH+6,X'80'      AND PRODUCT NAME                           
         MVC   ESTCLIN,SPACES                                                   
         OI    ESTCLINH+6,X'80'                                                 
         MVC   ESTPRDN,SPACES                                                   
         OI    ESTPRDNH+6,X'80'                                                 
*                                                                               
         XC    CASHPRD,CASHPRD     CLEAR CASH PRDS                              
         XC    OLDCASH,OLDCASH                                                  
         MVI   OLDCPRD,0                                                        
***********************************************************************         
*                                                                               
         LA    R2,ESTMEDKH           MEDIA                                      
         LHI   R0,ERRNOMEDQ                                                     
         CLI   5(R2),0             TEST MISSING                                 
         JE    SPERR0                                                           
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ESTMEDN,MEDNM         MEDIA NAME                                 
         OI    ESTMEDNH+6,X'80'                                                 
                                                                                
*---------------------------------------------------------------------*         
*        NON DDS TERMINALS MAY UPDATE MEDIA T RECORDS, BUT NOT        *         
*                  MEDIA C AND N RECORDS (CANADA)                     *         
*---------------------------------------------------------------------*         
                                                                                
         CLI   T217FFD+1,C'*'      TEST DDS TERMINAL?                           
         BE    VK05                 YUP, SKIP MEDIA CHECK                       
         CLI   ACTNUM,ACTDIS                                                    
         BE    VK05                                                             
         CLI   ACTNUM,ACTLIST      NOT LIST OR DISP?.....                       
         BE    VK05                                                             
         CLI   ACTNUM,ACTSEL       COULD BE SELECT FROM LIST...                 
         BNE   VK03                 NO?  MUST BE UPDATIVE, CHECK MEDIA          
         CLI   THISLSEL,C'C'       SELECT FOR CHANGE?                           
         BNE   VK05                 NO, SKIP MEDIA CHECK FOR C AND N            
VK03     CLI   SVAPROF+7,C'C'      IF CANADIAN                                  
         BNE   VK05                                                             
         CLI   QMED,C'C'           MEDIUM C AND N ONLY FOR DISPLAY              
         BE    ERRUSET                                                          
         CLI   QMED,C'N'                                                        
         BE    ERRUSET                                                          
*                                                                               
VK05     L     RE,AIO                SAVE AGENCY RECORD DATA                    
         USING AGYHDR,RE                                                        
         MVC   SVAGYFL1,AGYFLAG1                                                
         DROP  RE                                                               
*                                                                               
         MVC   EKEYAM,BAGYMD         COPY MEDIA INTO KEY                        
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTCLIKH           CLIENT                                     
         LHI   R0,ERRNOCLTQ                                                     
         CLI   5(R2),0                                                          
         JE    SPERR0                                                           
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
         MVC   ESTCLIN,CLTNM         CLIENT NAME                                
         OI    ESTCLINH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                                                           
         USING CLTHDR,RE                                                        
         MVC   SVE1USER,CEU1         SAVE CLIENT RECORD DATA                    
         MVC   SVEU1TYP,CEU1TYPE                                                
         MVC   SVEU1LEN,CEU1LEN                                                 
         MVC   SVEU1FL1,CEU1FLG1                                                
         MVC   SVEU1FL2,CEU1FLG2                                                
         MVC   SVE2USER,CEU2                                                    
         MVC   SVEU2TYP,CEU2TYPE                                                
         MVC   SVEU2LEN,CEU2LEN                                                 
         MVC   SVEU2FL1,CEU2FLG1                                                
         MVC   SVEU2FL2,CEU2FLG2                                                
         MVC   SVCLOP1,COPT1                                                    
         MVC   SVCLOP2,COPT2                                                    
         MVC   SVCLOP3,COPT3                                                    
         MVC   SVCLOP4,COPT4                                                    
         MVC   SVCLTPOL,CPOLONLY                                                
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         MVC   SVCLDLY,CDAILY                                                   
         MVC   SVCLTPW,CPWPCT                                                   
         MVC   SVCCOST2,CCOST2                                                  
         MVC   SVCC2CON,CC2CONV                                                 
         MVC   SVCOFF,COFFICE                                                   
         DROP  RE                                                               
*                                                                               
* CHANGED TO SUPPORT ESTHDR AUTO UPLOAD VIA DDLINK                              
* FIELDS ARE NOW DEFINED AS UNPROTECTED AND CHANGED TO PROTECTED                
* IF NO ESTUSER DATA                                                            
         MVC   ESTDSC1,SVE1USER    DISPLAY UDEF HEADINGS                        
         OI    ESTDSC1H+6,X'80'                                                 
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
         MVC   ESTDSC2,SVE2USER                                                 
         OI    ESTDSC2H+6,X'80'                                                 
         OI    ESTUSR2H+6,X'80'                                                 
*                                                                               
         MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
*                                                                               
         BRAS  RE,RDPROF           READ 00 PROFILE                              
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTPRDKH           PRODUCT                                    
         LHI   R0,ERRNOPRDQ                                                     
         CLI   5(R2),0                                                          
         JE    SPERR0                                                           
         MVI   AAAOK,C'Y'                                                       
         GOTO1 VALIPRD                                                          
         MVI   AAAOK,C'N'                                                       
         MVC   ESTPRDN,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    ESTPRDNH+6,X'80'      PRODUCT NAME                               
*                                                                               
         L     RE,AIO                SAVE PRODUCT RECORD DATA                   
         USING PRDHDR,RE                                                        
         MVC   SVPRD,PCODE+1                                                    
         DROP  RE                                                               
*                                                                               
         MVC   EKEYPRD,QPRD          COPY PRODUCT INTO KEY                      
         OC    EKEYPRD,SPACES                                                   
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTESTKH           ESTIMATE                                   
         LHI   R0,ERRNOESTQ                                                     
         CLI   5(R2),0               DK ACTED AS INPUT TRANSLATOR AND           
         JE    SPERR0                SET EST HEADER FIELD                       
*                                                                               
         BRAS  RE,SPTOZER            CHANGES LEADING SPACES TO ZEROS            
*                                                                               
VK15     CLI   ACTEQU,ACTADD                                                    
         BE    VK20                                                             
         GOTO1 VALIEST               ACTION IS NOT ADD SO...                    
         MVC   ESTDESC,ESTNM         VALIDATE ESTIMATE CODE AND                 
         OI    ESTDESCH+6,X'80'      TRANSMIT ESTIMATE DESCRIPTION              
*                                                                               
         L     R3,AIO                                                           
         MVC   SVECOST2,ECOST2       SAVE CLIENT RECORD DATA                    
         LA    R3,KEY                                                           
*                                                                               
         B     VK25                                                             
*                                                                               
VK20     LHI   R0,ESTERR1            ESTIMATE CODE MUST BE NUMERIC              
         TM    ESTESTKH+4,X'08'      AND HAVE A LENGTH <=3                      
         BZ    SPERR0                                                           
*                                                                               
         LLC   RE,5(R2)              CONVERT ESTIMATE CODE TO BINARY            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         LHI   R0,ESTERR2            EST MUST BE BETWEEN 1 - 255                
         CHI   RE,1                                                             
         BL    SPERR0                                                           
         CHI   RE,255                                                           
         BH    SPERR0                                                           
         STC   RE,BEST               STORE BINARY ESTIMATE                      
*                                                                               
VK25     MVC   EKEYEST,BEST          SAVE ESTIMATE CODE INTO KEY                
         MVC   ESTKEY,KEY            SAVE ESTIMATE RECORD KEY                   
         MVC   SVKEY,ESTKEY                                                     
*                                                                               
**********************************************************************          
*                                                                               
         XC    SVPOLPW,SVPOLPW                                                  
         XC    SVPOLC2,SVPOLC2                                                  
         MVC   KEY(13),ESTKEY         READ ESTIMATE RECORD FOR POL              
         MVC   KEY+4(3),=C'POL'       PRODUCT                                   
         GOTO1 HIGH                                                             
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK60                                                             
*                                                                               
* CHANGING TO ALWAYS REQUIRE A POL ESTIMATE FIRST ON ADD - 11/27/01             
*                                                                               
         LHI   R0,ESTERR3                                                       
         CLI   SVCLTPOL,C'Y'          IF (CLIENT MUST ADD POL ESTIMATE          
         BE    VK30                   BEFORE ADDING BRAND ESTIMATES)            
         CLI   SVCLPROF+0,C'0'        ATTEMPTS TO ADD BRAND ESTIMATE            
         BNE   VK30                   BEFORE POL ... ERROR                      
         TM    SVCLOP3,COP3BRD        THIS CLIENT STILL ALLOWED BRD             
         BO    VK40                   SKIP ERR                                  
         LHI   R0,ESTERR11            MUST ADD POL EST FIRST                    
*                                                                               
VK30     CLI   BPRD,X'FF'                                                       
         BE    VKX                                                              
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPERR0                                                           
*                                                                               
VK40     LHI   R0,ESTERR3             IN NETPAK, IF ANY CLIENTS ATT-            
         CLI   BPRD,X'FF'             EMPTS TO ADD BRAND ESTIMATE               
         BE    VKX                    BEFORE POL ... ERROR                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK50                                                             
         CLI   QMED,C'N'                                                        
         JE    SPERR0                                                           
         CLI   SVCLTPOL,C'Y'                                                    
         BNE   VK60                                                             
         J     SPERR0                                                           
*                                                                               
VK50     LHI   R0,ESTERR4             IF EXISTING POL ESTIMATE IS A             
         MVC   AIO,AIO3               MASTER OR SUB-ESTIMATE ... CANNOT         
         GOTO1 GETREC                 ADD BRAND ESTIMATE                        
         L     R3,AIO                                                           
         CLI   EMSTRIND,0                                                       
         JNE   SPERR0                                                           
*                                                                               
VK60     LHI   R0,ESTERR3             FOR PW OR COS2 CONVERTED CLIENTS          
         CLI   BPRD,X'FF'             POL ESTIMATE MUST BE                      
         BE    VKX                    ADDED BEFORE ANY BRANDS                   
         OC    SVCLTPW,SVCLTPW                                                  
         BNZ   VK61                                                             
         OC    SVCC2CON,SVCC2CON      COS2 CONVERTED                            
         BZ    VKX                                                              
VK61     CLI   ACTEQU,ACTADD                                                    
         BNE   VK65                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPERR0                                                           
         B     VK70                                                             
*                                                                               
VK65     CLC   KEY(13),KEYSAVE                                                  
         BNE   VKX                                                              
VK70     MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   SVPOLPW,EPWPCT         SAVE PW FROM POL RECORD                   
         MVC   SVPOLC2,ECOST2         SAVE COST2 FROM POL RECORD                
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                                                                               
VKX      MVC   AIO,AIO1                                                         
         MVC   KEY,ESTKEY                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       LA    R0,ESTOPTH           LAST FIELD                                  
         LA    R2,ESTDESCH          FIRST FIELD                                 
         XR    R1,R1                                                            
*                                                                               
DR01     IC    R1,0(R2)             LENGTH OF FIRST FIELD                       
         AHI   R1,-9                MINUS HEADER AND 1 FOR EX                   
         TM    1(R2),X'02'         TEST EXTENDED FLDHDR                         
         JZ    *+8                                                              
         AHI   R1,-8                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES       BLANK CURRENT FIELD                         
         OI    6(R2),X'80'          TRANSMIT                                    
DR02     IC    R1,0(R2)             R1 = LENGTH OF FIELD + HEADER               
         AR    R2,R1                NEXT FIELD                                  
         CR    R2,R0                END OF SCREEN?                              
         BH    *+16                                                             
         TM    1(R2),X'20'          NO, IS FIELD PROTECTED?                     
         BZ    DR01                 NO, CLEAR IT                                
         B     DR02                 YES, BUMP TO NEXT FIELD                     
*                                                                               
***********************************************************************         
*                                                                               
         USING ESTHDR,R3            ESTIMATE RECORD                             
         L     R3,AIO                                                           
*                                                                               
***********************************************************************         
*                                                                               
*        TM    EPRDCD,X'80'         SEE IF NEW NETPAK ESTHDR                    
*        BZ    *+10                                                             
*        MVC   LFMKEXP+50(2),=C' N'                                             
*        FOUT  LFMKEXPH                                                         
*                                                                               
***********************************************************************         
*                                                                               
         MVC   ESTDESC,EDESC         DESCRIPTION                                
         OI    ESTDESCH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         CLI   SECFLDPO,C'N'         USER HAS READ OR WRITE ACCESS?             
         BE    DR9                   NO - DO NOT DISPLAY FIELD VALUE            
         MVC   ESTPON,EPONUM         PURCHASE ORDER NUMBER                      
         OI    ESTPONH+6,X'80'                                                  
*                                                                               
***********************************************************************         
*                                                                               
DR9      TM    ECNTRL,X'04'          STATUS                                     
         BZ    DR10                                                             
         MVC   ESTSTAT,=CL8'HOLD'    X'04' BIT ON ... HOLD STATUS               
         OI    ESTSTATH+6,X'80'                                                 
         B     DR40                                                             
*                                                                               
DR10     TM    ECNTRL,X'08'                                                     
         BZ    DR20                                                             
         MVC   ESTSTAT,=CL8'LOCK'    X'08' BIT ON ... LOCK STATUS               
         OI    ESTSTATH+6,X'80'                                                 
         B     DR40                                                             
*                                                                               
DR20     OC    ELOCKYM,ELOCKYM                                                  
         BZ    DR30                                                             
         MVC   ESTSTAT,SPACES                                                   
         MVC   WORK(L'ELOCKYM),ELOCKYM                                          
         NI    WORK+1,X'FF'-X'80'-X'40'                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,ESTSTAT)                                 
         TM    ELOCKMON,X'80'                                                   
         BZ    *+8                                                              
         MVI   ESTSTAT+6,C'-'                                                   
         TM    ELOCKMON,X'40'                                                   
         BZ    *+8                   OTHERWISE ... SAVE DATE INTO               
         MVI   ESTSTAT+6,C'+'        STATUS ... X'80' PRIOR ...                 
         B     *+10                  X'40' SUBSEQUENT                           
*                                                                               
DR30     MVC   ESTSTAT,SPACES                                                   
         OI    ESTSTATH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
DR40     GOTO1 DATCON,DMCB,(0,ESTART),(5,ESTSTRD)                               
         OI    ESTSTRDH+6,X'80'      START DATE                                 
*                                                                               
**********************************************************************          
*                                                                               
         GOTO1 DATCON,DMCB,(0,EEND),(5,ESTENDD)                                 
         OI    ESTENDDH+6,X'80'      END DATE                                   
*                                                                               
**********************************************************************          
*                                                                               
         OI    ESTBBASH+6,X'80'                                                 
         OI    ESTCPCTH+6,X'80'                                                 
         OI    ESTCBASH+6,X'80'                                                 
*                                                                               
         OC    EBILLBAS(5),EBILLBAS  BILLING FORMULA?                           
         BNZ   DR50                                                             
*                                                                               
         MVC   ESTBBAS,SPACES        NO BILLING FORMULA, SEND BLANK             
         MVC   ESTCPCT,SPACES        FIELDS FOR BILL BASIS, COMM PCT            
         MVC   ESTCBAS,SPACES        AND COM BASIS TO SCREEN                    
         B     DR100                                                            
*                                                                               
**********************************************************************          
*                                                                               
*                                    BILL BASIS                                 
DR50     CLI   SCFLDEBF,C'N'         USER HAS READ OR WRITE ACCESS?             
         BE    DR60                  NO - DO NOT DISPLAY FIELD VALUE            
         MVC   ESTBBAS,=CL5'CNET'    X'10' AND X'40' BIT ON ...                 
         TM    EBILLBAS,X'50'        SET BILL BASIS FIELD TO 'CNET'             
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=CL5'NET'     X'10' BIT ON ...                           
         TM    EBILLBAS,X'10'        SET BILL BASIS FIELD TO 'NET'              
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=C'CGROS'     X'40' BIT ON ...                           
         TM    EBILLBAS,X'40'        SET BILL BASIS FIELD TO 'CGROS'            
         BO    DR60                                                             
*                                                                               
*                                    OTHERWISE ...                              
         MVC   ESTBBAS,=C'GROSS'     SET BILL BASIS FIELD TO 'GROSS'            
*                                                                               
**********************************************************************          
*                                                                               
DR60     CLI   SCFLDEBF,C'N'         USER HAS READ OR WRITE ACCESS?             
         BE    DR90                  NO - DO NOT DISPLAY FIELD VALUE            
         ICM   R4,15,EBILLCOM        COM.PERCENTAGE                             
         LTR   R4,R4                                                            
         BNZ   DR65                                                             
*                                                                               
         MVC   ESTCPCT,SPACES        IF COM BASIS IS ZERO ...                   
         MVC   ESTCBAS,SPACES        MOVE BLANK COMMISSION PERCENTAGE           
         B     DR100                 AND BLANK COM BASIS TO SCREEN              
*                                                                               
DR65     LPR   RF,R4                                                            
         C     RF,=F'1000000'        IF COM BASIS IS 100% MOVE 100              
         BNE   DR70                  TO SCREEN                                  
         MVC   ESTCPCT+1(3),=C'100'                                             
         B     DR80                                                             
*                                                                               
*                                    OTHERWISE ...                              
*                                    EDIT COM.% INTO SCREEN FIELD               
DR70     EDIT  (R4),(8,ESTCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
DR80     LTR   R4,R4                                                            
         BNM   *+8                   COM.% NEGATIVE, MINUS SIGN                 
         MVI   ESTCPCT,C'-'                                                     
*                                                                               
**********************************************************************          
*                                                                               
*                                    COMM.BASIS                                 
DR90     CLI   SCFLDEBF,C'N'         USER HAS READ OR WRITE ACCESS?             
         BE    DR100                 NO - DO NOT DISPLAY FIELD VALUE            
         MVC   ESTCBAS,=C'GROSS'     X'01' BIT ON ...                           
         TM    EBILLBAS,X'01'        SET COMM.BASIS FIELD TO 'GROSS'            
         BZ    *+10                  OTHERWISE ...                              
         MVC   ESTCBAS,=CL5'NET'     SET COMM.BASIS FIELD TO 'NET'              
*                                                                               
**********************************************************************          
*                                                                               
DR100    XC    ESTDEMS,ESTDEMS       DEMOS                                      
         XC    ESTDEM2,ESTDEM2                                                  
         XC    ESTDEM3,ESTDEM3                                                  
*                                                                               
         BRAS  RE,DISPDEM                                                       
*                                                                               
         OI    ESTDEMSH+6,X'80'      TRANSMIT DEMO LINES                        
         OI    ESTDEM2H+6,X'80'                                                 
         OI    ESTDEM3H+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTWTS,ESTWTS         WEIGHTS                                    
         LA    R5,ESTWTS                                                        
         LA    R0,L'ESTWTS(R5)                                                  
         ST    R0,WTEND                                                         
*                                                                               
         CLI   EWGTNM,C' '           IF NO WEIGHTED DEMO NAME, TEST             
         BNH   DR190                 FOR TARGETS                                
*                                                                               
         LA    R6,DMAX               WEIGHTED DEMO NAME EXISTS ...              
         LA    R2,EDEMLST                                                       
         LA    R4,EWGTLST                                                       
         LA    RF,BLOCK                                                         
DR160    CLI   0(R4),0               IF ANYTHING LEFT IN WEIGHTLIST             
         BE    DR180                 CALL FMTDEMO WITH DEMO FROM                
         BRAS  RE,FMTDEMO            BLOCK                                      
*                                                                               
         CLI   1(R2),X'21'           IF USER DEMO ... ONLY UN=NN WILL           
         BNE   DR170                 APPEAR ON WEIGHTS LINE                     
         MVI   WORK,2                                                           
*                                                                               
DR170    LLC   R1,WORK               LENGTH OF DEMO RETURNED IN WORK            
         LR    R0,R5                 AND ADJUST FOR WEIGHT ... SEE IF           
         AR    R0,R1                 IT WILL FIT ON LINE (IF NOT,DIE)           
         AHI   R0,4                                                             
         C     R0,WTEND                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCTR  R1,0                  DEMO RETURNED IN WORK+1 ... COPY           
         EX    R1,*+8                IT TO SCREEN AND MOVE '=' TO END           
         B     *+10                                                             
         MVC   0(0,R5),WORK+1                                                   
         AR    R5,R1                                                            
         MVI   1(R5),C'='                                                       
*                                                                               
         LA    R5,2(R5)                                                         
         EDIT  (B1,0(R4)),(3,WORK2),0,ALIGN=LEFT                                
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK2                                                    
         AR    R5,R0                                                            
         MVI   0(R5),C','            EDIT WEIGHT INTO LINE AND MOVE A           
         LA    R5,1(R5)              COMMA TO THE END                           
*                                                                               
DR180    LA    RF,11(RF)             BUMP TO NEXT DEMO, NEXT WEIGHT             
         LA    R4,1(R4)              AND DEMO IN RECORD                         
         LA    R2,3(R2)                                                         
         BCT   R6,DR160                                                         
*                                                                               
*                                                                               
DR190    OC    ETRGLST,ETRGLST       ANY TARGETS?                               
         BZ    DR240                                                            
         LA    R1,ETRGLST            YES ... TARGET LIST                        
         LAY   R4,ONETWO             TARGET NUMBER LIST                         
*                                                                               
DR200    OC    0(3,R1),0(R1)                                                    
         BZ    DR230                                                            
         LA    RF,BLOCK                                                         
         LA    R2,EDEMLST                                                       
         LA    R6,DMAX                                                          
*                                                                               
DR210    CLC   0(3,R1),0(R2)                                                    
         BE    DR220                                                            
         LA    RF,11(RF)                                                        
         LA    R2,3(R2)                                                         
         BCT   R6,DR210              TARGET NOT IN EDMLIST ... DIE              
         DC    H'0'                                                             
*                                                                               
DR220    BRAS  RE,FMTDEMO                                                       
         CLI   1(R2),X'21'           IF USER DEMO ... ONLY UN=NN                
         BNE   *+8                   WILL APPEAR ON WEIGHTS LINE                
         MVI   WORK,2                                                           
*                                                                               
         LLC   RF,WORK               LENGTH OF DEMO RETURNED IN WORK            
         LR    R0,R5                 AND ADJUST FOR TARGET ... SEE IF           
         AR    R0,RF                 IT WILL FIT ON LINE (IF NOT,DIE)           
         AHI   R0,4                                                             
         C     R0,WTEND                                                         
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCTR  RF,0                  TARGET RETURNED IN WORK+1 ...              
         EX    RF,*+8                COPY IT TO SCREEN WITH '=' AND 'T'         
         B     *+10                  AT END                                     
         MVC   0(0,R5),WORK+1                                                   
         AR    R5,RF                                                            
         MVI   1(R5),C'='                                                       
         MVI   2(R5),C'T'                                                       
         LA    R5,3(R5)                                                         
*                                                                               
         EDIT  (B1,0(R4)),(3,WORK2),0,ALIGN=LEFT                                
         LR    RE,R0                                                            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),WORK2                                                    
         AR    R5,R0                                                            
         MVI   0(R5),C','            EDIT TARGET INTO LINE AND MOVE A           
         LA    R5,1(R5)              COMMA TO THE END                           
*                                                                               
DR230    LA    R1,3(R1)              BUMP TO NEXT TARGET AND NEXT               
         LA    R4,1(R4)              TARGET NUMBER                              
         CLI   0(R4),X'FF'                                                      
         BNE   DR200                                                            
*                                                                               
DR240    BCTR  R5,0                  NO MORE TARGETS ... BLANK OUT              
         CLI   0(R5),C','            LAST SPACE AND TRANSMIT FIELD              
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         OI    ESTWTSH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTCOPY,ECOPY         COPY CODE                                  
         OI    ESTCOPYH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTREP,ESTREP         SPECIAL REP                                
         OC    EREP,EREP                                                        
         BZ    DR250                                                            
*                                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QRCPACK                                                   
         GOTO1 CALLOV,DMCB           CALL RCPACK                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'U',EREP),ESTREP                                     
*        MVC   HALF,EREP                                                        
*        LH    R0,HALF                                                          
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  ESTREP,DUB                                                       
DR250    OI    ESTREPH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTRBK,=C'LATEST'     RATING BOOK                                
         OI    ESTRBKH+6,X'80'                                                  
         OC    EBOOK,EBOOK           IF EBOOK IS EMPTY, DEFAULT TO              
         BZ    DR290                 'LATEST'                                   
*                                                                               
DR260    CLI   QMED,C'N'                                                        
         BNE   DR270                                                            
         XR    R0,R0                                                            
         XC    ESTRBK,ESTRBK         IF NETWORK MEDIA ...                       
         IC    R0,EBOOK+1            RATING BOOK WILL BE IN BINARY              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK(2),DUB                                                    
         MVI   ESTRBK+2,C'/'                                                    
         IC    R0,EBOOK                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK+3(2),DUB                                                  
         B     DR290                                                            
*                                    IF NOT NETWORK ... CALL DATCON             
DR270    GOTO1 DATCON,DMCB,(3,EBOOK),(6,ESTRBK)                                 
*                                                                               
**********************************************************************          
*                                                                               
DR290    MVC   ESTHUT,=CL7'AUTO'     HUTADJ                                     
         OI    ESTHUTH+6,X'80'                                                  
         CLI   EHUTADJ,0             IF ESTHUT IS EMPTY,DEFAULT TO AUTO         
         BE    DR310                                                            
*                                                                               
DR300    LLC   R4,EHUTADJ            OTHERWISE ... CALL DATCON                  
         XC    ESTHUT,ESTHUT                                                    
         SRL   R4,4                                                             
         STC   R4,WORK+1                                                        
         MVI   WORK,77                                                          
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+10)                                 
         MVC   ESTHUT(3),WORK+10                                                
*                                                                               
**********************************************************************          
*                                                                               
DR310    MVC   ESTMENU(1),EDAYMENU   DAYPART MENU                               
         OI    ESTMENUH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTFLTR,EPROF         FILTERS                                    
         OI    ESTFLTRH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTCPPE,ESTCPPE       CPP EST                                    
         LA    R6,ESTCPPE                                                       
         OC    ECPPCLT,ECPPCLT                                                  
         BZ    DR320                                                            
*                                                                               
         MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 CALLOV,DMCB,0         CPP EST CLIENT EXISTS ...                  
         CLI   4(R1),X'FF'           GET IT'S INFORMATION                       
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),ECPPCLT,0(R6)                                          
         LA    R6,2(R6)                                                         
         CLI   0(R6),C' '                                                       
         BE    *+8                                                              
         LA    R6,1(R6)                                                         
         MVI   0(R6),C'/'                                                       
         LA    R6,1(R6)                                                         
*                                                                               
DR320    CLI   ECPPEST,0                                                        
         BE    DR330                                                            
         LLC   R0,ECPPEST            CPP ESTIMATE EXISTS ... COPY               
         CVD   R0,DUB                IT TO SCREEN AND TRANSMIT                  
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
DR330    FOUT  ESTCPPEH                                                         
*R330    OI    ESTCPPEH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTTYPE,ESTTYPE       TYPE                                       
         CLI   ECPPTYPE,0                                                       
         BE    DR360                                                            
*                                                                               
         MVC   ESTTYPE(3),=C'CUT'    IF TYPE IS 'C' MOVE 'CUT' SCREEN           
         CLI   ECPPTYPE,C'C'                                                    
         BE    DR360                                                            
*                                                                               
         XC    ESTTYPE,ESTTYPE                                                  
         LAY   R4,TYPTAB             OTHERWISE ... IS TYPE IN TYPE              
DR340    CLC   ECPPTYPE,2(R4)        TABLE?                                     
         BE    DR350                                                            
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BNE   DR340                                                            
         B     DR360                                                            
DR350    MVC   ESTTYPE(2),0(R4)                                                 
DR360    OI    ESTTYPEH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTRAN,ESTRAN         REQ RANGE                                  
         OC    EREQLO(2),EREQLO                                                 
         BZ    DR370                                                            
*                                                                               
         MVC   ESTRAN(2),=C'NO'      IF REQ RANGE IS 'NO' ... COPY              
         CLC   EREQLO(2),=C'NO'      = 'NO' TO SCREEN                           
         BE    DR370                                                            
*                                                                               
         XR    R0,R0                                                            
         IC    R0,EREQLO             IF NOT 'NO' COPY FIRST # OF RANGE          
         CVD   R0,DUB                INTO SCREEN FIELD                          
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRAN(3),DUB                                                    
         MVI   ESTRAN+3,C'-'         MOVE '-' BETWEEN RANGE NUMBERS             
         IC    R0,EREQHI                                                        
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRAN+4(3),DUB       COPY SECOND # OF RANGE TO SCREEN           
DR370    OI    ESTRANH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTRTYP,ESTRTYP       RATE TYPE                                  
         MVC   ESTRTYP(1),ERATE                                                 
         MVC   ESTRTYP+1(1),ERATECST                                            
         OI    ESTRTYPH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         OC    ECSSDTE,ECSSDTE       COMSCORE SURVEY DATES                      
         BZ    *+14                                                             
         MVC   ESTCSDT,ECSSDTE                                                  
         OI    ESTCSDTH+6,X'80'                                                 
         OC    ECSBKTYP,ECSBKTYP     COMSCORE BOOK TYPE                         
         BZ    *+14                                                             
         MVC   ESTCBTY,ECSBKTYP                                                 
         OI    ESTCBTYH+6,X'80'                                                 
*                                                                               
         OC    EBKTYPE,EBKTYPE                                                  
         BZ    DR372                                                            
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
DR371    CLI   0(RF),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   EBKTYPE,SPBKTYPN                                                 
         BE    *+10                                                             
         AR    RF,R0                                                            
         B     DR371                                                            
*                                                                               
         MVC   ESTBTYP,SPBKTYPA    BOOK TYPE                                    
         OI    ESTBTYPH+6,X'80'                                                 
         DROP  RF                                                               
*                                                                               
**********************************************************************          
*                                                                               
DR372    MVC   ESTOWD(4),=C'NO  '    OUT OF WEEK ROTATOR                        
         CLI   EOWSDAY,0                                                        
         BE    *+10                                                             
         MVC   ESTOWD(4),=C'YES '                                               
         OI    ESTOWDH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTECON,ESTECON       CONTROL FIELD                              
*                                                                               
         TM    ECONTROL,EBILESTQ     IF BILL PERIOD = ESTIMATE PERIOD           
         BZ    *+8                   MOVE 'E' TO SCREEN FIELD                   
         MVI   ESTECON,C'E'                                                     
*                                                                               
         TM    ECONTROL,ENSEPCMQ     IF NO SEPARATE COMM BILLING FOR            
         BZ    *+10                  EST ... MOVE 'NSC' TO SCREEN FLD           
         MVC   ESTECON(3),=C'NSC'                                               
         OI    ESTECONH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ESTERTL,ERTLSCHM      RETAIL SCHEME                              
         OI    ESTERTLH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         LA    RF,ESTUSR1H           FIRST USER                                 
         LA    RE,ESTDSC1H           FIRST DESCRIPTION                          
*                                                                               
         OI    ESTUSR1H+1,X'20'      FIRST DESCRIPTION                          
         MVC   8(L'SVE1USER,RE),SVE1USER                                        
         CLC   SPACES(L'SVE1USER),8(RE)                                         
         BL    DR375                                                            
*                                                                               
         LR    R0,RE                 FIRST DESCRIPTION ADDRESS                  
         LLC   RE,0(RF)              RE = L(HEADER) + L(INPUT)                  
         AHI   RE,-8                 RE = L(INPUT)                              
         TM    1(RF),X'02'           CHECK FOR EXTENSION                        
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         BCTR  RE,0                                                             
         EX    RE,*+10                                                          
         LR    RE,R0                                                            
         B     DR376                                                            
         XC    8(0,RF),8(RF)         CLEAR GARBAGE                              
DR375    NI    1(RF),X'FF'-X'20'     UNPROTECT INPUT FIELD                      
DR376    OI    6(RE),X'80'           TRANSMIT                                   
*                                                                               
         XC    ESTUSR1,ESTUSR1       COPY USER1 FIELD FROM RECORD INTO          
         OC    SVE1USER,SVE1USER     SCREEN AND TRAMSIT                         
         BZ    *+10                                                             
         MVC   ESTUSR1,EUSER1                                                   
         MVI   ESTUSR1H+5,32                                                    
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
*                                                                               
         LA    RF,ESTUSR2H           SECOND USER                                
         LA    RE,ESTDSC2H           SECOND DESCRIPTION                         
*                                                                               
         OI    ESTUSR2H+1,X'20'      SECOND DESCRIPTION LINE                    
         MVC   8(L'SVE2USER,RE),SVE2USER                                        
         CLC   SPACES(L'SVE2USER),8(RE)                                         
         BL    DR377                                                            
*                                                                               
         LR    R0,RE                 SECOND DESCRIPTION ADDRESS                 
         LLC   RE,0(RF)              RE = L(HEADER) + L(INPUT)                  
         AHI   RE,-8                 RE = L(INPUT)                              
         TM    1(RF),X'02'           CHECK FOR EXTENSION                        
         BZ    *+8                                                              
         AHI   RE,-8                                                            
         BCTR  RE,0                                                             
         EX    RE,*+10                                                          
         LR    RE,R0                                                            
         B     DR378                                                            
         XC    8(0,RF),8(RF)         CLEAR GARBAGE                              
DR377    NI    1(RF),X'FF'-X'20'     UNPROTECT INPUT FIELD                      
DR378    OI    6(RE),X'80'           TRANSMIT                                   
*                                                                               
         XC    ESTUSR2,ESTUSR2       COPY USER2 FIELD FROM RECORD INTO          
         OC    SVE2USER,SVE2USER     SCREEN AND TRANSMIT                        
         BZ    *+10                                                             
         MVC   ESTUSR2(16),EUSER2                                               
         MVI   ESTUSR2H+5,20                                                    
         OI    ESTUSR2H+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         LA    R1,ESTOPT             OPTIONS FIELD                              
         XC    SCRNFLAG,SCRNFLAG                                                
         MVC   0(L'ESTOPT,R1),SPACES                                            
*                                                                               
         LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR379                 NO                                         
         CLI   ETYPE,0               IS ETYPE REGULAR?                          
         BNE   *+8                   NO                                         
         MVI   ETYPE,C'R'                                                       
         CLI   ETYPE,C'R'            IS ETYPE REGULAR?                          
         BE    DR379                 YES...DON'T DISPLAY                        
*                                                                               
         MVC   0(5,R1),=C'TYPE='                                                
*                                                                               
         L     R6,=A(ETYPTAB)                                                   
         A     R6,RELO                                                          
         MVC   5(3,R1),=C'???'       IN CASE WE GET AN INVALID ETYPE            
*                                                                               
DR378A   CLI   0(R6),X'FF'           END OF TABLE?                              
         BE    DR378C                YES                                        
         CLC   ETYPE,1(R6)           MATCH ON ETYPE?                            
         BE    DR378B                YES                                        
         LLC   R0,0(R6)                                                         
         AR    R6,R0                                                            
         B     DR378A                                                           
*                                                                               
DR378B   MVC   5(3,R1),1(R6)                                                    
*                                                                               
DR378C   MVI   8(R1),C','            COMMA TO SCREEN                            
         LA    R1,9(R1)              BUMP TO NEXT AVAILIBLE SPACE               
*                                                                               
DR379    CLI   SVCLDLY,C'Y'          DAILY OPTION SET?                          
         BE    DR380                                                            
         CLI   EDAILY,C' '                                                      
         BNH   DR390                                                            
DR380    LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR390                                                            
         CLI   EDAILY,C' '                                                      
         BH    *+8                                                              
         MVI   EDAILY,C'N'                                                      
         MVC   0(6,R1),=C'DAILY='    YES ... COPY DAILY OPTION AND              
         MVC   6(1,R1),EDAILY        COMMA TO SCREEN                            
         MVI   7(R1),C','                                                       
         LA    R1,8(R1)                                                         
*                                                                               
DR390    TM    EFLAG1,EF1REQ         REQUEST OPTION SET?                        
         BNO   DR400                                                            
         LA    R0,6                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR400                                                            
         MVC   0(6,R1),=C'REQ=Y,'    YES ... COPY REQUEST OPTION TO             
         LA    R1,6(R1)              SCREEN                                     
*                                                                               
DR400    TM    EFLAG1,EF1NODEM       NO DEMOS REQUIRED FOR BUY OPTION           
         BNO   DR420                 SET?                                       
         LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR420                                                            
         MVC   0(8,R1),=C'DEMOS=N,'  YES ... COPY DEMOS OPTION TO               
         LA    R1,8(R1)              SCREEN                                     
*                                                                               
DR420    OC    ECGTPCT,ECGTPCT       CLIENT GROSS TRADE OPTION SET?             
         BZ    DR430                                                            
         LA    R0,10                 MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR430                                                            
         MVC   0(4,R1),=C'CGT='      YES ... COPY CGT OPTION AND COMMA          
         LA    R1,4(R1)              TO SCREEN                                  
         LR    R6,R1                                                            
         EDIT  ECGTPCT,(5,(R6)),2,ALIGN=LEFT                                    
         LR    R1,R6                                                            
         AR    R1,R0                                                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR430    TM    EFLAG1,EF1OOWPW       PW OOW BILLING FEAUTURE OPTION             
         BZ    DR440                 SET?                                       
         LA    R0,5                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR440                                                            
         MVC   0(4,R1),=C'OWPW'      YES ... COPY OWPW OPTION AND COMMA         
         MVI   4(R1),C','            TO SCREEN                                  
         LA    R1,5(R1)                                                         
*                                                                               
DR440    TM    EFLAG1,EF1SDE         SUPERDESK ESTIMATE                         
         BZ    DR470                 SET?                                       
         LA    R0,2                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR470                                                            
         MVC   0(2,R1),=C'SD'                                                   
         MVI   2(R1),C','            TO SCREEN                                  
         LA    R1,3(R1)                                                         
*                                                                               
DR470    CLI   ESLN,0                RESTRICTED SPOT LEN OPTION SET?            
         BE    DR480                                                            
         LA    R0,7                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR480                                                            
         MVC   0(4,R1),=C'SLN='      YES ... COPY RESTRICTED SPOT LEN           
         LA    R1,4(R1)              AND COMMA TO SCREEN                        
         LR    R6,R1                                                            
         EDIT  ESLN,(3,(R6)),0,ALIGN=LEFT                                       
         AR    R6,R0                                                            
         MVI   0(R6),C','                                                       
         LA    R1,1(R6)                                                         
*                                                                               
DR480    CLI   ECASHPRD,0            CASH PRODUCT OPTION SET?                   
         BE    DR510                                                            
         LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR510                                                            
         MVC   0(5,R1),=C'CASH='     YES ... COPY CASH PRODUCT AND              
         LA    R1,5(R1)              COMMA TO SCREEN                            
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RF,AIO                                                           
         USING CLTHDR,RF                                                        
         LA    RF,CLIST-CLTHDR(RF) FIND CASH PRD                                
DR490    CLI   0(RF),0                                                          
         BE    DR510                                                            
         CLC   3(1,RF),ECASHPRD                                                 
         BE    DR500                                                            
         LA    RF,4(RF)                                                         
         B     DR490                                                            
DR500    MVC   0(3,R1),0(RF)                                                    
         DROP  RF                                                               
         MVI   3(R1),C','                                                       
         LA    R1,4(R1)                                                         
*                                                                               
DR510    CLI   ETRDPRD,0             TRADE PRODUCT OPTION SET?                  
         BE    DR540                                                            
         LA    R0,7                  MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR540                                                            
         MVC   0(4,R1),=C'TRD='      YES ... COPY TRADE PRODUCT OPTION          
         LA    R1,4(R1)              AND COMMA TO SCREEN                        
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'00'                                                            
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     RF,AIO                                                           
         LA    RF,CLIST-CLTHDR(RF)                                              
DR520    CLI   0(RF),0                                                          
         BE    DR540                                                            
         CLC   3(1,RF),ETRDPRD                                                  
         BE    DR530                                                            
         LA    RF,4(RF)                                                         
         B     DR520                                                            
DR530    MVC   0(3,R1),0(RF)                                                    
         MVI   3(R1),C','                                                       
         LA    R1,4(R1)                                                         
*                                                                               
DR540    OC    EPWPCT,EPWPCT         PROFIT WITHIN PERCENTAGE OPTION            
         BZ    DR582                 SET?                                       
         LA    R0,11                 MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR582                                                            
         SR    R6,R6                 YES ... COPY PROFIT WITHIN PERCEN-         
         ICM   R6,14,EPWPCT          TAGE TO SCREEN                             
         SRA   R6,8                                                             
         MVC   0(3,R1),=C'PW='                                                  
         CLC   EPWPCT,=X'800000'                                                
         BNE   DR580                                                            
         MVI   3(R1),C'0'                                                       
         LA    R1,4(R1)                                                         
         B     DR581                                                            
DR580    LR    R4,R1                                                            
         EDIT  (R6),(7,3(R4)),2,ALIGN=LEFT,TRAIL=0,FLOAT=-                      
         LR    R1,R4                                                            
         LA    R1,3(R1)                                                         
         AR    R1,R0                                                            
DR581    MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR582    OC    ECOST2,ECOST2         COST FACTOR OPTION SET?                    
         BZ    DR586                                                            
         LA    R0,13                 MAX LENGTH OF OUTPUT DATA                  
         BRAS  RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR589                                                            
         MVC   0(5,R1),=C'COS2='     YES ... COPY COST FACTOR TO                
         LA    R4,5(R1)              SCREEN                                     
         LA    R1,5(R1)                                                         
         CLI   ECOST2,X'80'                                                     
         BNE   DR583                                                            
         MVC   0(3,R4),=C'0.0'                                                  
         LA    R0,3                                                             
         B     DR584                                                            
DR583    EDIT  ECOST2,(8,0(R4)),6,ALIGN=LEFT,FILL=0,DROP=5                      
DR584    AR    R1,R0                                                            
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR586    DS    0H                                                               
         CLI   ECPPRS,C'N'                                                      
         BNE   DR589                                                            
         LA    R0,7                                                             
         BRAS  RE,CHECKOPT                                                      
         BNZ   DR589                                                            
         MVC   0(7,R1),=C'CPPRS=N'                                              
         LA    R1,7(R1)                                                         
* * *                                                                           
DR589    BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   SCRNFLAG,0            IF ALL OPTIONS DIDN'T FIT ON               
         BE    DR600                 SCREEN - COPY 'DIDN'T FIT' FLAG            
         MVC   0(2,R1),=C',*'                                                   
DR600    OI    ESTOPTH+6,X'80'                                                  
DRX      B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
*                                                                               
DK       L     R3,AIO                                                           
         USING EKEY,R3                                                          
         MVC   BYTE,EKEYAM           ISOLATE MEDIA CODE                         
         NI    BYTE,X'0F'                                                       
         LAY   R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
DK10     CLC   BYTE,1(R5)                                                       
         BE    DK20                                                             
         LA    R5,MEDTABLQ(R5)                                                  
         CLI   0(R5),X'FF'                                                      
         BNE   DK10                                                             
DK20     MVC   ESTMEDK,0(R5)                                                    
         OI    ESTMEDKH+6,X'80'                                                 
         MVI   ESTMEDKH+5,1          TRANSMIT MEDIA CODE TO SCREEN              
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 CLUNPK,DMCB,(SVCPROF+6,EKEYCLT),ESTCLIK                          
         OI    ESTCLIKH+6,X'80'                                                 
         MVI   ESTCLIKH+5,3          TRANSMIT CLIENT CODE TO SCREEN             
*                                                                               
***********************************************************************         
*                                                                               
         MVC   ESTPRDK,EKEYPRD                                                  
         MVI   ESTPRDKH+5,3                                                     
         OI    ESTPRDKH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN            
*                                                                               
**********************************************************************          
*                                                                               
         EDIT  EKEYEST,ESTESTK,ALIGN=LEFT                                       
         OI    ESTESTKH+6,X'80'      TRANSMIT ESTIMATE CODE TO SCREEN           
         OI    ESTESTKH+4,X'08'      NUMERIC CODE                               
         MVI   ESTESTKH+5,3          LENGTH                                     
         CLI   ESTESTK+2,C' '                                                   
         BH    DKX                                                              
         MVI   ESTESTKH+5,2                                                     
         CLI   ESTESTK+1,C' '                                                   
         BH    DKX                                                              
         MVI   ESTESTKH+5,1                                                     
*                                                                               
DKX      B     VK                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VR       XC    ERRTEXT,ERRTEXT                                                  
         CLI   SVAPROF+7,C'C'      IF CANADIAN                                  
         BNE   VR00                                                             
*                                                                               
         CLI   SVCXTRA+8,C'P'      TEST P&G CANADA                              
         BE    *+12                                                             
         CLI   T217FFD+1,C'*'      TEST DDS TERMINAL?                           
         BE    VR00                                                             
*                                                                               
         LHI   R0,BADMEDQ                                                       
         CLI   QMED,C'C'           MEDIUM C AND N ONLY FOR DISPLAY              
         JE    SPERR0                                                           
         CLI   QMED,C'N'                                                        
         JE    SPERR0                                                           
*                                                                               
VR00     CLC   CLIPRO(4),=C'S0F0'    SAVE CLIENT'S F0 PROFILE                   
         BNE   VR01                                                             
         CLC   CLIPRO+4(2),AGENCY                                               
         BNE   VR01                                                             
         CLC   CLIPRO+6(1),QMED                                                 
         BNE   VR01                                                             
         CLC   CLIPRO+7(3),QCLT                                                 
         BE    VR02                                                             
*                                                                               
VR01     XC    CLIPRO,CLIPRO                                                    
         MVC   CLIPRO(4),=C'S0F0'                                               
         MVC   CLIPRO+4(2),AGENCY                                               
         MVC   CLIPRO+6(1),QMED                                                 
         MVC   CLIPRO+7(3),QCLT                                                 
*                                                                               
         CLI   SVCOFF,C' '                                                      
         BNH   *+14                                                             
         MVI   CLIPRO+10,C'*'                                                   
         MVC   CLIPRO+11(1),SVCOFF                                              
*                                                                               
         MVC   CLIPRO+7(3),QCLT                                                 
         GOTO1 GETPROF,DMCB,CLIPRO,WORK,DATAMGR                                 
         MVC   SVF0PROF,WORK                                                    
*                                                                               
***********************************************************************         
                                                                                
VR02     L     R3,AIO1                                                          
         USING ESTHDR,R3                                                        
*                                                                               
         XC    SVESTIM,SVESTIM                                                  
         CLI   ACTEQU,ACTSEL                                                    
         BE    *+12                                                             
         CLI   ACTEQU,ACTCHA         IF CHANGING ESTIMATE, SAVE OLD             
         BNE   VR03                  ESTIMATE DATA                              
*                                                                               
         CLI   EDDLINK,C'Y'        TEST EST ADDED BY UPLOAD                     
         JNE   VR02A               NO                                           
         TM    GENSTAT6,GES$UPLD   TEST UPLOAD DOING THIS CHANGE                
         JO    VR02A                                                            
         LHI   R0,NOTOWNER         NOT ALLOWED TO CHANGE                        
         J     SPERR0                                                           
*                                                                               
VR02A    MVC   SVCNTRL,ECNTRL                                                   
         MVC   SVSTRT,ESTART                                                    
         MVC   SVEND,EEND                                                       
         MVC   SVDLY,EDAILY                                                     
         MVC   SVELOCK,ELOCKYM                                                  
         MVC   SVOWS,EOWSDAY                                                    
         MVC   SVUSRNMS,EUSRNMS                                                 
         MVC   SVPWPCT,EPWPCT                                                   
         MVC   SVCOST2,ECOST2                                                   
         MVC   SVEMGD,EMGDTE                                                    
         MVC   SVCPP,ECPPEST                                                    
         MVC   SVDEMOS,EDEMOS                                                   
         MVC   SVBOOK,EBOOK                                                     
         MVC   SVHUT,EHUTADJ                                                    
         MVC   SVDPT,EDAYMENU                                                   
         MVC   SVETYPE,ETYPE                                                    
         MVC   SVECON,ECONTROL                                                  
         CLI   SVF0PROF,C'N'                                                    
         BE    *+10                                                             
         MVC   SVFLTRS,EPROF                                                    
         CLI   SVF0PROF+1,C'N'                                                  
         BE    *+10                                                             
         MVC   SVRTL,ERTLSCHM                                                   
*                                                                               
***********************************************************************         
*                                                                               
VR03     LA    RE,POLDATA                                                       
         LA    RF,(POLDATAX-POLDATA)                                            
         XCEF                                                                   
*                                                                               
         MVI   POLSW,0               IF CHANGING OR ADDING BRD EST              
         CLC   QPRD,=C'POL'          WITH NO POL EST SET POLSW=0                
         BNE   VR04                  IF (CHANGING OR ADDING A BRAND             
         CLI   ACTEQU,ACTADD         ESTIMATE FOR A CLIENT WITH A POL           
         BE    VR05                  ESTIMATE) SET POLSW TO 1                   
VR04     XC    KEY,KEY                                                          
         MVC   KEY(8),ESTKEY         IF (CHANGING THE POL ESTIMATE) SET         
         MVC   KEY+4(3),=C'POL'      POLSW TO 2                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY        IF (ADDING A POL ESTIMATE) SET             
         BNE   VR06                  POLSW TO 3                                 
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                READ THE EXISTING POL ESTIMATE             
         L     R3,AIO                INTO AIO2 AND SAVE NECCESSARY FLDS         
         MVI   POLSW,1                                                          
         MVC   POLSTRT,ESTART                                                   
         MVC   POLEND,EEND                                                      
         MVC   POLOWDAY,EOWSDAY                                                 
         MVC   POLDEMOS,EDEMOS                                                  
         MVC   POLBOOK,EBOOK                                                    
         MVC   POLHUT,EHUTADJ                                                   
         MVC   POLDPT,EDAYMENU                                                  
         MVC   POLETYPE,ETYPE                                                   
         MVC   POLCON,ECONTROL                                                  
         MVC   POLFLAG1,EFLAG1                                                  
         MVC   POLDLY,EDAILY                                                    
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   *+10                                                             
         MVC   POLNTRDM,ENONTDMS                                                
*                                                                               
         CLI   SVF0PROF,C'N'                                                    
         BE    *+10                                                             
         MVC   SVFLTRS,EPROF                                                    
         CLI   SVF0PROF+1,C'N'                                                  
         BE    *+10                                                             
         MVC   POLRTL,ERTLSCHM                                                  
         CLC   QPRD,=C'POL'                                                     
         BNE   VR06                                                             
         MVI   POLSW,2                                                          
         B     VR06                                                             
VR05     MVI   POLSW,3                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR06     L     R3,AIO1               BUILD NEW ESTIMATE RECORD AT AIO1          
         MVC   ELEN,=H'874'          SET DEFAULT ELEN                           
         TM    USRIDFLG,USRRNTKQ     TEST ACCESS TO COMSCORE?                   
         BZ    VR09                    NO                                       
         MVC   ELEN,=AL2(ESTHDR2Q)    YES, SET EXTENDED ELEN                    
*                                                                               
VR09     CLI   ESTOPTH+5,0           IF NO OPTIONS INPUTTED AND CLIENT          
         BNE   VR30                  NOT SET FOR DAILY, BLANK OPTIONS           
         MVC   ESTOPT,SPACES         FIELD                                      
*                                                                               
         CLI   SVCLDLY,C'Y'          IF NO OPTIONS INPUTTED AND CLIENT          
         BNE   VR10                  SET FOR DAILY, MOVE IN DAILY=Y             
         MVI   ESTOPTH+5,7                                                      
         MVC   ESTOPT(7),=C'DAILY=Y'                                            
VR10     OI    ESTOPTH+6,X'80'                                                  
*                                                                               
         CLI   ACTEQU,ACTADD         IF NO OPTIONS INPUTTED & CHANGING          
         BE    VR30                  EST RECORD FOR CANADIAN AGENCY,            
VR20     CLI   SVAPROF+7,C'C'        NETWORK OR COMBINED MEDIA ...              
         BNE   VR30                  DON'T VALIDATE OR MODIFY ANY REC           
         CLI   QMED,C'N'             ORD FIELDS                                 
         BE    VR1799                                                           
         CLI   QMED,C'C'                                                        
         BE    VR1799                                                           
*                                                                               
***********************************************************************         
*                                                                               
VR30     LA    R2,ESTSTATH           STATUS FIELD                               
         GOTO1 =A(CHKSTAT),RR=RELO   IF (STATUS CHANGED) ... IGNORE             
         CLI   WORK,C'S'             OTHER MODIFICATIONS, SKIP VALID-           
         BE    VR1799                ATION AND PUT ESTIMATE RECORD              
*                                                                               
***********************************************************************         
*                                                                               
         CLI   SECFLDPO,0            FULL ACCESS?                               
         BNE   VR32                  NO - CANNOT CHANGE THIS FIELD              
         LA    R2,ESTPONH                                                       
         MVC   EPONUM,ESTPON         VALIDATE PURCHASE ORDER NUMBER             
         OC    EPONUM,SPACES                                                    
         BRAS  RE,VALPROR            MAKE SURE PO IS ALPHANUMERIC               
         JNE   ERRINV                                                           
*                                                                               
***********************************************************************         
*                                                                               
VR32     GOTO1 =A(SETDLY),RR=RELO    SET DAILY FIELD SPEC-33510                 
*                                                                               
         GOTO1 =A(VR40),RR=RELO      VAL DESCRIPTION AND START DATE             
*                                                                               
***********************************************************************         
*                                                                               
         CLI   POLSW,2               IF WHILE (CHANGING THE POL EST-            
         BNE   VR359                 IMATE) START DATE, END DATE OR             
         CLC   SVSTRT,SYR            OUT OF WEEK ROTATOR WERE CHANGED           
         BNE   VR1799                ... DO NOT DO ANY FURTHER VALID-           
         CLC   SVEND,EYR             ATIONS OR RECORD CHANGES                   
         BNE   VR1799                                                           
         CLC   SVOWS,OWSDAY                                                     
         BNE   VR1799                                                           
*                                                                               
***********************************************************************         
*                                                                               
VR359    GOTO1 =A(VR360),RR=RELO     VAL BILL BAS, COM PCT & COM BAS            
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTDEMSH           DEMOS                                      
         XC    EDEMOS(124),EDEMOS                                               
         MVI   WTSW,0                                                           
         MVI   HMSW,0                                                           
         MVI   NHMSW,0                                                          
         L     R3,AIO2                                                          
         LA    R0,8                                                             
VR490    XC    0(250,R3),0(R3)                                                  
         LA    R3,250(R3)                                                       
         BCT   R0,VR490                                                         
         L     R3,AIO1                                                          
*                                                                               
         LHI   R0,TV1DEM             AT LEAST 1 DEMO REQUIRED FOR MEDIA         
         CLI   5(R2),0               TV                                         
         BNE   VR500                                                            
         CLI   QMED,C'T'                                                        
         JE    SPERR0                                                           
*                                                                               
         LHI   R0,NET1DEM            AT LEAST 1 DEMO REQUIRED FOR               
         CLI   OVSYS,3               NETPAK                                     
         BNE   VR800                                                            
         B     SPERR0                                                           
*                                                                               
***********************************************************************         
*                                                                               
VR500    LHI   R0,DEMERR                                                        
         CLC   8(5,R2),=C'MENU='                                                
         BNE   VR530                                                            
         LHI   R0,BADMENUQ                                                      
         LA    R2,ESTDEM2H           IF "MENU=" DEMO ...                        
         CLI   5(R2),0               2ND DEMO LINE MUST BE BLANK                
         JNE   SPERR0                                                           
         LA    R2,ESTDEM3H           IF "MENU=" DEMO ...                        
         CLI   5(R2),0               3RD DEMO LINE MUST BE BLANK                
         JNE   SPERR0                                                           
*                                                                               
         LHI   R0,DEMERR3                                                       
         LA    R2,ESTDEMSH           MENU OPTION MUST BE 1-4 CHARACTERS         
         LLC   R1,5(R2)              LONG                                       
         AHI   R1,-5                                                            
         JNP   SPERR0                                                           
         CHI   R1,4                                                             
         JH    SPERR0                                                           
*                                                                               
         LHI   R0,DEMERR4                                                       
         XC    KEY,KEY               READ 0D26, AGENCY, MENU OPTION             
         MVC   KEY(2),=X'0D26'       RECORD INTO AIO3 ...                       
         MVC   KEY+2(1),BAGYMD                                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),13(R2)                                                  
         OC    KEY+3(4),SPACES                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE       IF NOT FOUND ...                           
         JNE   SPERR0                MENU OPTION INVALID                        
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R5,AIO                IF FOUND ... GET DEMO ELEMENTS AND         
         LA    R5,24(R5)             MOVE DEMOS INTO RECORD (EDEMLST)           
         LA    R4,EDEMLST            UNTIL NO MORE DEMO ELEMENTS                
         LA    R6,DMAX                                                          
         MVI   ELCODE,X'05'                                                     
VR510    BRAS  RE,NEXTEL                                                        
         BNE   VR520                                                            
         MVC   0(3,R4),2(R5)                                                    
         LA    R4,3(R4)                                                         
         BCT   R6,VR510                                                         
*                                                                               
VR520    MVC   KEY(13),ESTKEY        RESTORE KEY                                
         B     VR550                                                            
*                                                                               
***********************************************************************         
*                                                                               
VR530    XC    BLOCK(255),BLOCK      IF NOT "MENU=" DEMO...                     
         XC    BLOCK+255(224),BLOCK+255                                         
         LA    RE,ENONTDMS                                                      
         LA    RF,400                                                           
         XCEF                                                                   
*                                                                               
         L     R5,AIO2                                                          
         USING DBLOCK,R5             SET UP CALL TO DEMOVAL                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = TP                            
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'             SET DBSELMED = R FOR RADIO                 
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         MVI   DBSELSRC,C'N'                                                    
*                                                                               
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   VR540                 AGENCY USING US DEMOS                      
         CLI   SV00PROF+10,C'Y'      VALIDATE WITH US DEMOS                     
         BNE   *+8                                                              
         OI    DBVOPT,X'80'                                                     
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    VR540                                                            
         MVI   DBSELMED,C'C'                                                    
*                                                                               
VR540    MVC   DMCB+4(4),=X'D9000AD9'                                           
         GOTO1 CALLOV,DMCB,0         CALL DEMOVAL                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
*                                                                               
VR540A   XC    DMCB,DMCB                                                        
*                                                                               
         L     R1,AIO2              DBLOCK IS USING AIO2                        
         AHI   R1,1024              DEMOVAL PARAM5 USING AIO2+1024              
         USING P5XD,R1                                                          
         MVC   P5XID,=C'P5X '       PARAM 5 EXTENDED BLOCK                      
         LA    RE,POLNTRDM                                                      
         ST    RE,P5XINNTD          A(INPUT NON-TRAD INDX DEMO LIST)            
         LA    RE,CSUTOKEN                                                      
         ST    RE,P5XLICNS          A(32 BYTE COMSCORE LICENSE)                 
         DROP  R1                                                               
         ST    R1,DMCB+16           EXTENDED PARAM5                             
         OI    DMCB+16,X'80'        SO DEMOVAL KNOWS EXTENDED BLOCK             
*                                                                               
******   GOTO1 (RF),(R1),(3,ESTDEMSH),(20,BLOCK),(C'S',(R5)),EUSRNMS,           
******         POLNTRDM,ENONTDMS                                                
         GOTO1 (RF),DMCB,(3,ESTDEMSH),(20,BLOCK),(C'S',(R5)),EUSRNMS,  *        
               ,ENONTDMS                                                        
*                                                                               
VR540C   CLI   DMCB+4,0                                                         
         BNE   VR540D                                                           
         CLI   DBSELSRC,C'A'                                                    
         BE    VR540ER                                                          
         MVI   DBSELSRC,C'A'                                                    
         B     VR540A                                                           
*                                                                               
VR540ER  L     RE,4(R1)            GET OUTPUT AREA ADDRESS                      
         MVC   ERRTEXT(7),0(RE)    RETURN THE BAD INPUT                         
         LHI   R0,BADDEMOQ                                                      
         J     SPERR0                                                           
*                                                                               
VR540D   LLC   R4,DMCB+4             NUMBER OF DEMOS                            
         MHI   R4,3                  EACH DEMO REQUIRES 3 BYTES                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                  STORE DEMOS INTO RECORD (EDEMLST)          
         MVC   EDEMLST(0),BLOCK                                                 
*                                                                               
         OC    ENONTDMS,ENONTDMS     ANY NON-TRAD EMOS?                         
         JZ    VR550                  NO                                        
         LHI   R0,BADCOMSCQ                                                     
         TM    USRIDFLG,USRRNTKQ     ACCESS TO COMSCORE?                        
         JZ    SPERR0                 NO                                        
         CLI   ESTMEDK,C'R'           RADIO?                                    
         JE    SPERR0                 NO                                        
         CLI   ESTMEDK,C'X'           NTWK RADIO?                               
         JE    SPERR0                 NO                                        
*                                     YES, IF ADDING A NEW REC, THEN            
*                                     ELEN SHOULD BE EXTENDED, O/W              
         CLC   ELEN,=AL2(ESTHDRLN)    IF CHANGE, WAS REC PREV EXTENDED?         
         BNH   ERRRCEX                 NO                                       
*                                                                               
         MVC   ECSBKTYP,=C'L '       DEFAULT TO LIVE FOR NOW                    
*                                                                               
VR550    LA    R4,DMAX-1             CHECK FOR DUPLICATE DEMOS                  
         LA    R5,EDEMLST            (R5 HOLDS STEADY AS R6 BUMPS               
VR560    LA    R6,3(R5)              THROUGH)                                   
         LA    R1,DMAX-1                                                        
*                                                                               
VR570    CLI   1(R6),0               END OF DEMOS?                              
         BE    VR580                                                            
         CLC   0(3,R5),0(R6)         DUPLICATE DEMO?                            
         JE    VR570ER               TRANSLATE DEMO FOR MESSAGE                 
         LA    R6,3(R6)              NOT A DUPLICATE, KEEP GOING                
         BCT   R1,VR570              TILL THE END OF DEMOLIST                   
         J     VR580                                                            
*                                                                               
VR570ER  LHI   R0,DUPDEMOQ                                                      
         LR    R4,R6               POINT R4 TO DUP DEMO                         
         J     VR660ERX                                                         
*                                                                               
VR580    LA    R5,3(R5)              BUMP UP R5                                 
         CLI   1(R5),0               END OF DEMOS?                              
         BE    VR590                                                            
         BCT   R4,VR560              NO, KEEP CHECKING FOR DUPLICATES           
*                                                                               
VR590    LA    R0,EUSRNMN-1                                                     
         LA    R1,EUSRNMN-1                                                     
         LA    R4,EUSRNMS                                                       
         LA    R5,EUSRNMS+7                                                     
*                                                                               
VR590A   OC    0(7,R4),0(R4)         EMPTY SLOT?                                
         BZ    VR590B                YES, KEEP CHECKING COULD HAVE MORE         
         CLC   0(7,R4),0(R5)         HAVE A DUPLICATE USER DEMO?                
         BE    DUPPERR               YES, ERROR                                 
         LA    R5,7(R5)                                                         
         BCT   R0,VR590A                                                        
*                                                                               
VR590B   LA    R4,7(R4)                                                         
         LA    R5,7(R4)                                                         
         LR    R0,R1                                                            
         BCTR  R0,0                                                             
         BCT   R1,VR590A                                                        
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   VR590Z                                                           
*                                                                               
         LA    R4,DMAX-1             CHECK FOR DUPLICATE NON-TRAD DEMOS         
         LA    R5,ENONTDMS           (R5 HOLDS STEADY AS R6 BUMPS               
VR590C   LA    R6,L'ENTRDDMO(R5)     THROUGH)                                   
         LA    R1,DMAX-1                                                        
*                                                                               
VR590D   CLI   0(R6),0               END OF DEMOS?                              
         BE    VR590E                                                           
         CLC   0(L'ENTRDDMO,R5),0(R6)   DUPLICATE DEMO?                         
         BE    SPERREX                                                          
         LA    R6,L'ENTRDDMO(R6)     NOT A DUPLICATE, KEEP GOING                
         BCT   R1,VR590D             TILL THE END OF DEMOLIST                   
*                                                                               
VR590E   LA    R5,L'ENTRDDMO(R5)     BUMP UP R5                                 
         CLI   0(R5),0               END OF DEMOS?                              
         BE    *+8                                                              
         BCT   R4,VR590C             NO, KEEP CHECKING FOR DUPLICATES           
*                                                                               
         LA    R5,ENONTDMS           SET NONT DEMO POINTER                      
         BRAS  RE,ADDNONT            GO ADD NONT SEQNUM POINTERS                
*                                                                               
VR590Z   CLI   POLSW,0                                                          
         BE    VR760                                                            
*                                                                               
***********************************************************************         
*                                                                               
         CLI   POLSW,3               IF (CHANGING POL RECORD) SEE IF            
         BE    VR595                 ANY DEMOS DELETED FROM DEMO LIST           
         CLI   POLSW,2                                                          
         BNE   VR680                                                            
*                                                                               
         CLC   POLDEMOS(60),EDEMOS   WITH THIS TEST, OLD AND NEW DEMO           
         BE    VR760                 LIST WILL ONLY MATCH IF THEY CON-          
*                                    TAIN SAME DEMOS IN SAME ORDER              
*                                                                               
*                                                                               
VR591A   LA    R5,PEDEMLST           BUT IT'S ALSO OK,(IF ALL OLD DEMOS         
         LA    RE,DMAX               ARE PRESENT BUT IN A DIFFERENT OR-         
VR591B   LA    R6,EDEMLST            DER) OR (IF ALL OLD DEMOS PRESENT          
         LA    RF,DMAX               WITH 1 OR MORE ADDITION)                   
VR591C   CLC   0(3,R5),0(R6)                                                    
         BE    VR591D                                                           
         LA    R6,3(R6)                                                         
         CLI   1(R6),0                                                          
         BE    VR595                                                            
         BCT   RF,VR591C                                                        
         B     VR595                                                            
VR591D   LA    R5,3(R5)              BUT IF ANY DEMO(S) WERE DELETED            
         CLI   1(R5),0               ...                                        
         BE    VR760                                                            
         BCT   RE,VR591B                                                        
*                                                                               
***********************************************************************         
*                                                                               
VR595    XC    PEDEMLST,PEDEMLST     IF (ADDING POL RECORD) OR WHILE            
         XC    KEY,KEY               (CHANGING POL RECORD) ANY DEMOS            
         MVC   KEY(4),ESTKEY         WERE DELETED ... MUST LOOP THROUGH         
VR600    GOTO1 =A(NEXTPRD),RR=RELO   ALL BRAND ESTIMATES AND BUILD LIST         
         BNE   VR640                 OF ALL BRAND DEMOS IN POLDEMOS             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         LA    R4,EDEMOS                                                        
         LA    R1,DMAX                                                          
VR610    LA    R5,POLDEMOS                                                      
         LA    R6,DMAX                                                          
*                                                                               
VR620    CLC   0(3,R4),0(R5)         IF BRAND ESTIMATE NOT ALREADY IN           
         BE    VR630                 POLDEMOS ... ADD IT                        
         CLC   0(3,R5),=3X'00'                                                  
         BNE   *+14                                                             
         MVC   0(3,R5),0(R4)                                                    
         B     VR630                                                            
         LA    R5,3(R5)                                                         
         BCT   R6,VR620                                                         
VR630    LA    R4,3(R4)              WHEN NO MORE DEMOS FOR BRAND EST           
         CLC   0(3,R4),=3X'00'       READ IN NEXT BRAND ESTIMATE                
         BE    VR600                                                            
         BCT   R1,VR610                                                         
         B     VR600                                                            
*                                                                               
*                                                                               
VR640    CLC   POLDEMOS(3),=3X'00'                                              
         BE    VR760                                                            
         L     R3,AIO1               COMPARE THE BRAND-BUILT POLDEMOS           
         LA    R4,POLDEMOS           WITH CHANGED OR ADDED POL DEMOS            
         LA    RE,DMAX                                                          
VR650    LA    R5,EDEMOS                                                        
         LA    RF,DMAX                                                          
VR660    CLC   0(3,R4),0(R5)         IF ANY DEMOS IN POLDEMOS MISSING           
         BE    VR670                 FROM POL RECORD RETURN ERROR               
         LA    R5,3(R5)                                                         
         CLC   0(3,R5),=3X'00'                                                  
         BE    VR660ER                                                          
         BCT   RF,VR660                                                         
         J     VR670                                                            
*                                                                               
VR660ER  LHI   R0,NODEMDELQ           CANNOT DELETE POL DEMO                    
*                                                                               
VR660ERX BRAS  RE,SETDBLOCK        TRANSLATE BAD DEMO CODE                      
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JH    *+10                                                             
         XC    ENONTDMS,ENONTDMS                                                
*                                                                               
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(1,(R4)),(13,BLOCK),(C'S',ELEM),EUSRNMS,      *        
               ENONTDMS                                                         
         MVC   ERRTEXT(8),BLOCK       MOVE TEXT                                 
         J     SPERR0                                                           
*                                                                               
VR670    LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'                                                  
         BE    VR760                                                            
         BCT   RE,VR650                                                         
         J     VR680                                                            
*                                                                               
***********************************************************************         
*                                                                               
VR680    MVC   ERRNUM,=AL2(DEMINVL2) IF (CHANGING OR ADDING A BRAND             
         CLC   POLDEMOS(3),=3X'00'   ESTIMATE FOR A CLIENT WITH A POL           
         BE    SPERREX               ESTIMATE) ... DEMOS MUST BE A              
         MVI   UDSW,0                SUBSET OF POL DEMOS                        
*                                                                               
         LA    R4,EDEMOS                                                        
         LA    R1,DMAX                                                          
VR690    LA    R5,POLDEMOS                                                      
         LA    R6,DMAX                                                          
         CLI   1(R4),X'21'           IF USER DEMO ENCOUNTERED IN BRAND          
         BNE   *+8                   DEMOS ... TURN ON USDW SWITCH              
         MVI   UDSW,1                                                           
VR700    CLC   0(3,R4),0(R5)         IF BRAND DEMO NOT IN POL DEMOS ...         
         BE    VR710                 ERROR                                      
         LA    R5,3(R5)                                                         
         BCT   R6,VR700                                                         
         B     SPERREX                                                          
VR710    LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'       WHEN END OF BRAND DEMOS HIT ...            
         BE    VR720                 EXIT BCT LOOP                              
         BCT   R1,VR690                                                         
*                                                                               
*                                                                               
VR720    MVC   ERRNUM,=AL2(USDMER)                                              
         CLI   UDSW,0                IF USER DEMO ENCOUNTERED ... BRAND         
         BE    VR750                 USER NAMES MUST MATCH POL USER             
         LA    R0,4                  NAMES                                      
         LA    R4,EUSRNMS                                                       
         LA    R5,PEUSRNMS                                                      
VR730    CLI   0(R4),C' '                                                       
         BNH   VR740                                                            
         CLC   0(7,R4),0(R5)         IF BRAND USER NAME NOT IN POL USER         
         BNE   SPERREX               NAMES ... ERROR                            
VR740    LA    R4,7(R4)                                                         
         LA    R5,7(R5)              WHEN END OF BRAND USER NAMES HIT           
         BCT   R0,VR730              ... EXIT BCT LOOP                          
*                                                                               
VR750    CLC   ELEN,=AL2(ESTHDRLN)   NON-TRAD DEMOS?                            
         JNH   VR758                                                            
*                                                                               
         LA    R4,ENTRDDMO                                                      
         LA    R1,DMAX                                                          
VR752    LA    R5,POLNTRDM                                                      
         LA    R6,DMAX                                                          
VR754    CLC   0(L'ENTRDDMO,R4),0(R5)    IF BRAND DEMO NOT IN POL DEMOS         
         BE    VR756                     ERROR                                  
         AHI   R5,L'ENTRDDMO                                                    
         BCT   R6,VR754                                                         
         MVC   ERRTEXT(8),0(R4)          MOVE DEMO TEXT                         
         J     SPERREX                                                          
*                                                                               
VR756    AHI   R4,L'ENTRDDMO                                                    
         BCT   R1,VR752                                                         
*                                                                               
VR758    MVC   ERRNUM,=AL2(USWDMER)                                             
         CLI   EWGTNM,C' '           BRAND ESTIMATE'S WEIGHTED DEMO             
         BNH   VR760                 MUST MATCH POL ESTIMATE'S WEIGHT-          
         CLC   EWGTNM,PEWGTNM        ED DEMO                                    
         BNE   SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR760    MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         CLI   QMED,C'N'             IF SYSTEM IS NETPAK, MEDIA IS NET-         
         BNE   VR800                 WORK AND CLIENT NOT CANADIAN ...           
         CLI   SVAPROF+7,C'C'        A TOTAL HMS DEMO AND AT LEAST ONE          
         BE    VR800                 OTHER NON-TOTAL HMS DEMO REQUIRED          
         CLI   OVSYS,3                                                          
         BNE   VR800                                                            
*                                                                               
         LA    R4,EDEMLST            SEE IF TOTAL HMS (C901) IS SOME-           
         LA    R0,DMAX               WHERE IN DEMO LIST ...                     
VR770    CLC   0(3,R4),=3X'00'                                                  
         BE    VR780                                                            
         CLC   1(2,R4),=X'C901'      WHEN TOTAL HMS FOUND SET HMSW              
         BNE   *+12                                                             
         MVI   HMSW,1                                                           
         B     *+8                   WHEN ANY OTHER DEMO FOUND SET              
         MVI   NHMSW,1               NHMSW                                      
         LA    R4,3(R4)                                                         
         BCT   R0,VR770                                                         
*                                                                               
VR780    MVC   ERRNUM,=AL2(TOTHERR1)  MUST HAVE TOTAL HOMES                     
         CLI   HMSW,1                                                           
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(TOTHERR2)  MUST HAVE SOME OTHER DEMO                 
         CLI   NHMSW,1                                                          
         BNE   SPERREX                                                          
*                                                                               
VR800    LA    R0,8                                                             
         L     R4,AIO3                                                          
VR810    XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         BCT   R0,VR810                                                         
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTWTSH            DEMO WEIGHTS                               
         CLI   5(R2),0                                                          
         BE    VR930                                                            
*                                                                               
         L     R4,AIO3               SCANNER BUILDS TABLE OF DEMOS AT           
         AHI   R4,1000               1000 BYTES BEYOND AIO3                     
         LA    R2,ESTDEMSH                                                      
         ST    R2,SVDEMADR                                                      
         GOTO1 SCANNER,DMCB,(R2),(15,(R4))                                      
         LHI   R0,BADWGTSQ                                                      
         CLI   DMCB+4,0                                                         
         JE    SPERR0                # OF DEMOS CANNOT EXCEED 14 OR BE          
         CLI   DMCB+4,DMAX           ZERO                                       
         JH    SPERR0                                                           
*                                                                               
         L     R2,SVDEMADR                                                      
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT DEMO INPUT LINE                
         LA    R0,ESTDEM3H                                                      
         CR    R2,R0                                                            
         BH    VR820                                                            
         CLI   5(R2),0             TEST FOR MORE INPUT                          
         BE    VR820               NO                                           
*                                                                               
         LLC   R0,DMCB+4                                                        
         MHI   R0,32                                                            
         L     R5,AIO3                                                          
         AHI   R5,1000                                                          
         AR    R5,R0                                                            
         LLC   RE,DMCB+4                                                        
         LA    R0,DMAX+1                                                        
         SR    R0,RE                                                            
         GOTO1 SCANNER,DMCB,(R2),((R0),(R5))                                    
         CLI   DMCB+4,0                                                         
         JE    SPERR0                                                           
*                                                                               
VR820    LA    R2,ESTWTSH            PARSNIP BUILDS TABLE OF WEIGHTS            
         L     R4,AIO3               AT 500 BYTES BEYOND AIO3                   
         AHI   R4,500                                                           
         L     RF,ACOMFACS                                                      
         L     RF,CPARSNIP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(15,(R4)),0                                       
         MVC   ERRNUM,=AL2(WGHTERR)                                             
         CLI   DMCB+4,0              # OF WEIGHTS CANNOT EXCEED 14 OR           
         JE    SPERREX               BE ZERO                                    
         CLI   DMCB+4,DMAX*2                                                    
         JH    SPERREX               CHANGE PARSNIP BLOCK TO BE SCANNER         
         GOTO1 =A(PARTOSCN),RR=RELO  LIKE BLOCK AT AIO3                         
*                                                                               
***********************************************************************         
*                                                                               
         LA    R6,DMAX                                                          
         L     R5,AIO3                                                          
         USING BSCAND,R5                                                        
VR830    CLC   0(2,R5),=X'0000'      WEIGHT LIST BROKEN DOWN BY BSCAND          
         BE    VR930                 DEMO LIST   <---   R6                      
         ST    R6,FULL                                                          
         LA    R0,DMAX                                                          
         LA    R4,EWGTLST            RECORD'S WEIGHT FIELD <--- R4              
         LA    R2,EDEMLST            RECORD'S DEMO FIELD   <--- R2              
         L     R6,AIO3                                                          
         AHI   R6,1000                                                          
*                                                                               
VR840    CLC   BFLD1,12(R6)          COMPARE WEIGHT WITH DEMO (FOR              
         BE    VR860                 LENGTH OF 11)                              
         CLI   BFLD1LEN,2                                                       
         BNE   VR850                 IF WEIGHT STARTS WITH 'U' COMPARE          
         CLI   BFLD1,C'U'            WITH DEMO FOR LENGTH OF 2                  
         BNE   VR850                                                            
         CLC   BFLD1(2),12(R6)                                                  
         BE    VR860                                                            
VR850    LA    R6,32(R6)             ADVANCE TO NEXT DEMO (IN TAB AND           
         LA    R4,1(R4)              RECORD) ... ADVANCE TO NEXT DEMO           
         LA    R2,3(R2)              (IN RECORD)                                
         BCT   R0,VR840                                                         
         LA    R2,ESTWTSH            IF WEIGHT NOT FOUND IN DEMO LIST           
         B     SPERREX               RETURN ERROR                               
*                                                                               
***********************************************************************         
*                                                                               
VR860    CLI   BFLD2,C'T'            IF WEIGHT IS A TARGET ... MUST             
         BNE   VR890                 HAVE A LENGTH OF 2, MUST BE EITHER         
         CLI   BFLD2LEN,2            'T1' OR 'T2' AND BE INPUTTED ONLY          
         BNE   ERRINV2               ONCE                                       
         LA    R1,ETRGLST                                                       
         CLC   BFLD2(2),=C'T1'                                                  
         BE    VR870                                                            
         LA    R1,ETRGLST+3                                                     
         CLC   BFLD2(2),=C'T2'                                                  
         BNE   ERRINV2                                                          
VR870    OC    0(3,R1),0(R1)                                                    
         BNZ   DUPPERR1                                                         
VR880    MVC   0(3,R1),0(R2)         MOVE DEMO FROM RECORD INTO TARGET          
         B     VR920                 LIST                                       
*                                                                               
***********************************************************************         
*                                                                               
VR890    CLI   0(R4),0               IF NOT A TARGET ...                        
         BNE   DUPPERR1              MAY ONLY INPUT EACH WEIGHT ONCE            
*                                                                               
VR900    CLI   1(R2),C'R'            IF A RATING ... RETURN ERROR               
         BE    RATNOWT               RATINGS CANNOT HAVE WEIGHTS                
*                                                                               
         L     R1,BFLD2B             OTHERWISE, VALIDATE RIGHT SIDE OF          
         CLI   BFLD2LEN,0            WEIGHT ...                                 
         BE    WGHTSPER                                                         
         TM    BFLD2VAL,X'80'        RIGHT SIDE OF WEIGHT MUST BE PRES-         
         BZ    WGHTSPER              ENT, NUMERIC AND <= 255                    
         CHI   R1,255                                                           
         BH    WGHTSPER                                                         
         LTR   R1,R1                                                            
         BZ    WGHTSPER                                                         
         STC   R1,0(R4)                                                         
*                                                                               
***********************************************************************         
*                                                                               
*                                    IF (CHANGING OR ADDING A BRAND             
         CLI   POLSW,1               ESTIMATE FOR A CLIENT WITH A POL           
         BE    *+18                  ESTIMATE) OR (CHANGING THE POL             
         MVC   ERRNUM,=AL2(CHAWT2)   ESTIMATE) ... WEIGHTS MUST MATCH           
         CLI   POLSW,2               POL WEIGHTS                                
         BNE   VR920                                                            
         CLC   POLDEMOS(3),=3X'00'                                              
         BE    WGHTSPR2                                                         
         GOTO1 =A(CKPOLWTS),RR=RELO                                             
*                                                                               
***********************************************************************         
*                                                                               
VR920    LA    R5,BSCANLNQ(R5)       VALIDATE THE NEXT WEIGHT                   
         L     R6,FULL                                                          
         BCT   R6,VR830                                                         
         DROP  R5                                                               
*                                                                               
***********************************************************************         
*                                                                               
VR930    LA    R2,ESTDEMSH                                                      
         MVC   ERRNUM,=AL2(WGHTERR4) IF WEIGHT WAS ENTERED WITH NO              
         OC    EWGTLST,EWGTLST       WEIGHTED DEMO ... RETURN ERROR             
         BZ    VR940                                                            
         CLI   EWGTNM,C' '                                                      
         BNH   SPERREX                                                          
         B     VR950                                                            
VR940    MVC   ERRNUM,=AL2(WGHTERR5) IF WEIGHTED DEMO WAS ENTERED WITH          
         CLI   EWGTNM,C' '           NO WEIGHT ... RETURN ERROR                 
         BH    SPERREX                                                          
*                                                                               
*                                                                               
VR950    CLI   POLSW,2               IF (CHANGING POL ESTIMATE) ...             
         BNE   VR979                 SAVE USER NAMES                            
         CLC   SVUSRNMS,EUSRNMS                                                 
         BE    VR979                                                            
         MVC   SVUSRNMS,EUSRNMS                                                 
*                                                                               
***********************************************************************         
*                                                                               
VR979    GOTO1 =A(VR980),RR=RELO     VALIDATE RATING BOOK & HUT ADJUST          
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 =A(VR1090),RR=RELO    DPT MENUS AND FILTERS                      
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 =A(VR1210),RR=RELO    CONTROL, RETAIL SCHEME,COM SCORE           
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTOPTH            OPTIONS                                    
         GOTO1 =A(VALPROF),RR=RELO                                              
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 =A(VR1340),RR=RELO                                               
*                                                                               
***********************************************************************         
*                                                                               
VR1510   MVC   AIO,AIO1                                                         
         L     R3,AIO                VALIDATE COPY CODE,SPECIAL REP,            
         GOTO1 =A(VR1520),RR=RELO    CPP EST,TYPE,REC RANGE & RATE TYPE         
*                                                                               
***********************************************************************         
*                                                                               
VR1799   GOTO1 =A(VR1800),RR=RELO    RECORD HAS BEEN VALIDATED ...              
VRX      OI    GENSTAT2,RETEQSEL     PUT IT AND REDISPLAY                       
         B     DR                                                               
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRANGE  LA    R2,ESTRANH                                                       
         J     ERRINV                                                           
ERRINV2  LA    R2,ESTWTSH            RESET R2 FOR WEIGHTS ERRORS                
ERRINV   MVI   ERROR,INVALID                                                    
         J     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         J     VSFMERR                                                          
*                                                                               
ERRBRPOL LHI   R0,BRPOLER1                                                      
         J     SPERR0                                                           
NODEMDEL LHI   R0,NODEMODE                                                      
         J     SPERR0                                                           
ERRUSET  LA    R2,ESTMEDKH           MAY NOT USE MEDIA C/N FOR ADD/CHA          
         LHI   R0,USEMEDT                                                       
         J     SPERR0                                                           
ERRCOMS  LA    R2,ESTDEMSH                                                      
         LHI   R0,COMSERR                                                       
         J     SPERR0                                                           
ERRRCEX  LA    R2,ESTDEMSH         RECORD MUST BE EXTENDED FIRST                
         LHI   R0,1416                                                          
         J     SPERR0                                                           
*                                                                               
ERRCBTY  LA    R2,ESTCBTYH           MAY NOT USE MEDIA C/N FOR ADD/CHA          
         LHI   R0,CBTYERR                                                       
         J     SPERR0                                                           
ERRCSDT  LA    R2,ESTCSDTH           MAY NOT USE MEDIA C/N FOR ADD/CHA          
         LHI   R0,CSDTERR                                                       
         J     SPERR0                                                           
*                                                                               
DUPPERR1 LA    R2,ESTWTSH            RESET R2 FOR ERROR MESS                    
DUPPERR  LHI   R0,DUPERR                                                        
         J     SPERR0                                                           
*                                                                               
WGHTSPR2 LA    R2,ESTWTSH            BRAND AND POL WEIGHTS MUST AGREE           
         LHI   R0,BRPOLER9                                                      
         J     SPERR0                                                           
*                                                                               
WGHTSPER LA    R2,ESTWTSH                                                       
         LHI   R0,WGHTERR3                                                      
         J     SPERR0               WEIGHT INVALID OR MISSING                   
*                                                                               
RATNOWT  LA    R2,ESTWTSH                                                       
         LHI   R0,WGHTERR2                                                      
         J     SPERR0                                                           
*                                                                               
SPERR0   STH   R0,ERRNUM                                                        
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         MVI   GT1INDS,0           CLEAR!                                       
*                                                                               
         LA    RE,ERRTEXT          AREA FOR APPENDED ERR TEXT                   
         CLI   0(RE),C' '          TEST IF THERE IS ANY TEXT                    
         JL    *+12                                                             
         ST    RE,GTATXT-1                                                      
         MVI   GTLTXT,L'ERRTEXT                                                 
*                                                                               
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*        ERRORMESSAGES                                                          
*                                  SHORT DESP OF ERROR MSGS                     
BADREP   EQU   29                  INVALID SPECIAL REP                          
BADDATE  EQU   20                  INVALID DATE                                 
BBERR    EQU   510                 BILL-BASIS IS CGROSS OR CNET                 
COMPERR1 EQU   511                 COM % REQUIRED                               
COMPERR2 EQU   515                 COM % VALID NUMERIC                          
COMPERR3 EQU   512                 100% IS MAX COM %                            
COMPERR4 EQU   513                 0% IS MIN COM %                              
COMPERR5 EQU   514                 1ST CHAR IS + OR -                           
CBASERR1 EQU   522                 COMM BASIS REQUIRED                          
CBASERR2 EQU   521                 COMM BASIS GROSS OR NET                      
CHGERR   EQU   546                 CANNOT CHANGE FIELD                          
PFERR    EQU   559                 INVALID PFKEY                                
BRPOLER1 EQU   561                 BRAND & POL EST DATES MUST AGREE             
EDOLERR  EQU   562                 ORDERED OR PAID $$ ON AN EST                 
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ESTERR2  EQU   564                 ESTIMATE CODE BETWEEN 1 - 255                
ESTERR3  EQU   565                 NO POL EST OPEN FOR BRAND POL CLT            
ESTERR4  EQU   566                 POL MSTR OR SUB EST OPEN - NO ADD            
ESTERR5  EQU   567                 CANNOT CHANGE DATES                          
ESTERR6  EQU   568                 CANNOT SHORTEN DURATION                      
ESTERR7  EQU   569                 END DATE BEFORE START DATE                   
ESTERR8  EQU   570                 SYS=NETPACK, MEDIA MUST = N                  
ESTERR9  EQU   571                 CAN'T HAVE PW ON EST > 14 WKS                
ESTERR10 EQU   572                 NO OUT-OF-WEEK ROTATOR                       
ESTERR12 EQU   1467                ESTIMATE DATES CANNOT CROSS DEC29/19         
TV1DEM   EQU   573                 TV MUST HAVE AT LEAST 1 DEMO                 
NET1DEM  EQU   721                 NETPAK MUST HAVE AT LEAST 1 DEMO             
TOTHERR1 EQU   576                 TOTAL HMS MUST BE INPUT FOR NETWORK          
TOTHERR2 EQU   1429                MUST HAVE AT LEAST ONE MORE DEMO             
DEMERR   EQU   584                 INVALID OR TOO MANY DEMOS                    
DEMERR2  EQU   585                 DEMO NOT IN POL DEMOS                        
WGHTERR  EQU   586                 INVALID OR TOO MANY WEIGHTS                  
WGHTERR2 EQU   587                 NO WEIGHTS FOR RATINGS                       
WGHTERR3 EQU   588                 WEIGHT INVALID OR MISSING                    
WGHTERR4 EQU   589                 NO WEIGHTED DEMO                             
WGHTERR5 EQU   590                 NO WEIGHTS                                   
FNDERR   EQU   591                 RECORD NOT FOUND                             
FLT1MSS  EQU   592                 FILTER 1 MISSING                             
FLT2MSS  EQU   593                 FILTER 2 MISSING                             
FLT3MSS  EQU   594                 FILTER 3 MISSING                             
DLYERR   EQU   595                 DAILY EST MAX 53 DAYS                        
REQERR   EQU   596                 ONLY VALID FOR POL EST                       
ESTERR   EQU   597                 NOT IN EST PERIOD                            
PRIDERR  EQU   654                 PRIMARY DEMOS MUST MATCH                     
ENDOLAP  EQU   655                 END DATE OVERLAPS                            
INVES    EQU   656                 ESTIMATE INVALID                             
ETYPERR  EQU   657                 ETYPE CAN'T BE DELETED                       
STRINJAN EQU   658                 START MONTH MUST BE JAN                      
ENDINDEC EQU   659                 END MONTH MUST BE DEC                        
DUPERR   EQU   660                 DUPLICATE FOUND                              
DSPERR   EQU   661                 INVALID DATE SPREAD                          
DAILYERR EQU   662                 DAILY EST MAX 53 DAYS                        
POLRQERR EQU   663                 ONLY VALID FOR POL EST                       
NOPWPCT  EQU   664                 PW % IS REQUIRED                             
NOTPOLPW EQU   665                 BRAND PW MUST = POL PW %                     
DELPWPOL EQU   666                 MUST REMOVE PW% FROM POL                     
CASHINV  EQU   667                 CASH OPTION INVALID FOR NON TRD PRD          
CASHREQ  EQU   668                 CASH OPTION REQ'D FOR TRADE PRDS             
NOCSHEST EQU   669                 NO ESTIMATE FOR CASH PRD                     
CASHSET  EQU   670                 CASH ESTIMATE ASSIGNED DIF TRD PRD           
CASHCHG  EQU   671                 CAN NOT CHANGE CASH PRODUCT                  
CASHTRD  EQU   672                 CASH PRD CANNOT BE TRADE PRD                 
SPTLNERR EQU   673                 SPOTLENGTH OPTION NOT VALID                  
INVPW    EQU   674                 CAN'T HAVE PW ON EST >14WKS                  
RATERR1  EQU   675                 RATING BOOK REQUIRED                         
HUTERR1  EQU   676                 HUT ADJ REQUIRED                             
TOOBIG   EQU   677                 TOO LONG                                     
STATERR1 EQU   679                 LOCK STATUS NOT VALID FOR ADDS               
STATERR2 EQU   680                 STATUS INVALID FOR PREV HELD EST             
STATERR3 EQU   681                 HOLD STATUS NOT VALID FOR ADDS               
STATERR4 EQU   682                 UNLOCK NOT VALID FOR ADDS                    
STATERR5 EQU   683                 CAN'T UNLOCK HELD ESTIMATE                   
STATERR6 EQU   684                 EST NOT PREV LOCKED                          
STATERR7 EQU   685                 REL NOT VALID STATUS FOR ADDS                
STATERR8 EQU   686                 DBT NOT VALID STATUS FOR ADDS                
STATERR9 EQU   687                 MUST BE FOLLOWED BY YEAR (2 DIGITS)          
STATERRA EQU   688                 CAN'T USE DATE FOR HELD ESTIMATE             
STATERRB EQU   689                 CAN'T USE DATE FOR LOCKED ESTIMATE           
STATERRC EQU   690                 STATUS REQ'D FOR NETPACK ADDS                
DESERR1  EQU   691                 DESCRIPTION REQ'D                            
DATERR1  EQU   692                 START DATE REQUIRED                          
DATERR2  EQU   693                 END DATE REQUIRED                            
OWRERR1  EQU   694                 O-W-R MUST START W/ N OR Y                   
OWRERR2  EQU   695                 O-W-R MUST START W/ N                        
DEMERR3  EQU   696                 MENU OPTION MUST BE 1-4 C'S LONG             
DEMERR4  EQU   697                 MENU OPTION INVALID                          
MENERR1  EQU   698                 DPT MENU REQUIRED                            
MENERR2  EQU   699                 DPT MENU MUST BE 1 CHAR LONG                 
BRPOLER2 EQU   701                 BRAND & POL OOWR MUST AGREE                  
BRPOLER3 EQU   702                 BRAND & POL RATING BOOK MUST AGREE           
BRPOLER4 EQU   703                 BRAND & POL DEPT MENY MUST AGREE             
BRPOLER5 EQU   704                 BRAND & POL HUT ADJ'S MUST AGREE             
BRPOLER6 EQU   705                 BRAND & POL CONTROL'S MUST AGREE             
BRPOLER7 EQU   706                 BRAND & POL RET SCHEME MUST AGREE            
BRPOLER8 EQU   707                 BRAND & POL FILTERS MUST AGREE               
BRPOLER9 EQU   708                 BRAND & POL WEIGHTS MUST AGREE               
BRPOLERA EQU   990                 BRAND & POL TYPE=OPTS MUST AGREE             
BRPOLERB EQU   1468                BRAND & POL DAILY OPT MUST AGREE             
CONTER1  EQU   709                 BILLING RECORDS EXIST                        
CONTER2  EQU   710                 BILLING RECORDS EXIST FOR BRAND EST          
CPPERR1  EQU   711                 FOR POL, CPP MUST BE NUMERIC                 
TYPERR1  EQU   712                 TYPE CANNOT BE 'CUT'                         
TYPERR2  EQU   713                 TYPE ONLY VALID FOR POL EST                  
TYPERR3  EQU   714                 TYPE MUST BE 2 CHARACTERS LONG               
TYPERR4  EQU   715                 TYPE MUST MATCH CPP EST TYPE                 
RANERR1  EQU   716                 FIRST HALF LESS THAN SECOND                  
RANERR2  EQU   717                 CODE MUST FIT IN RANGE                       
RATER1   EQU   718                 CLIENT REC DOES NOT ALLOW SPEC RATES         
NODEMODE EQU   722                 CAN'T DELETE DEMO FROM POL                   
DEMINVL2 EQU   724                 DEMO NOT IN POL ESTIMATE                     
USDMER   EQU   725                 BRAND USER NAMES MUST MATCH POL              
USWDMER  EQU   726                 BRAND WEIGHT. DEMO MUST MATCH POL            
CHAWT1   EQU   727                 WEIGHT DOES NOT MATCH POL EST                
CHAWT2   EQU   728                 CANNOT CHANGE WEIGHT ON POL EST              
CASH1    EQU   729                 INVALID CASH PRODUCT                         
CASH2    EQU   730                 CASH PRODUCT DOES NOT EXIST FOR CLT          
CPPERR2  EQU   731                 CPP EST MUST BE NUMERIC FOR POL EST          
REQERR1  EQU   732                 REQ OPTION MUST BE Y OR N                    
CGTERR   EQU   733                 CGT < = $99.99                               
ODEERR   EQU   734                 DEMO OPTION MUST BE Y OR N                   
COS2ERR  EQU   735                 COS2 MUST BE 0<X<9.99                        
MONDERR  EQU   736                 START DATE CANNOT BE MONDAY                  
ONLY1DIY EQU   782                 CAN ONLY HAVE 1 TRADE PRD PER EST            
INVCPPRS EQU   840                 CPPRS CAN BE Y OR N                          
ESTERR11 EQU   846                 MUST ADD POL EST FIRST                       
USEMEDT  EQU   911                 USE MEDIA T FOR UPDATIVE ACTIONS...          
*                                  ...INSTEAD OF C OR N                         
TYPERR   EQU   985                 TYPE OPT MUST BE STW,REG,BAR(TER)            
PWCOS2ER EQU   1133                CLT CONVERTED TO COS2, NO PW ALLOWED         
C2DELERR EQU   1142                CANNOT DELETE COS2                           
TYPEERR1 EQU   1145                TYPE=STW/BAR NOT ALLOWED                     
CBTYERR  EQU   1410                COMSCORE BOOK TYPE REQ                       
CSDTERR  EQU   1411                COMSCORE SURVEY DATES REQ                    
COMSERR  EQU   1415                UNAUTHORIZED TO USE COMSCORE                 
*                                                                               
ERRNOMEDQ EQU  1420                MEDIA IS REQUIRED                            
ERRNOCLTQ EQU  1421                CLIENT IS REQUIRED                           
ERRNOPRDQ EQU  1422                PRODUCT IS REQUIRED                          
ERRNOESTQ EQU  1423                ESTIMATE NUMBER IS REQUIRED                  
BADMENUQ  EQU  1424                IF INPUT MENU=, NO DEMO DATA ALLOWED         
BADCOMSCQ EQU  1425                COMSCORE AUTHORIZATION REQUIRED              
BADDEMOQ  EQU  1427                DEMO &T IS INVALID                           
DUPDEMOQ  EQU  1428                DUPLICATE DEMO IN LIST                       
BADWGTSQ  EQU  1430                MUST HAVE 1-14 WEIGHT ENTRIES                
BADBOOKQ  EQU  1431                RATING BOOK NOT VALID                        
BADHUTQ   EQU  1432                HUT SHOULD BE AUTO OR MMM                    
DPTERRQ   EQU  1433                DPT MENU NOT ON FILE                         
BADFLTRQ  EQU  1434                EST FILTER SHOULD BE 0-9, A-Z                
ERRNOFLTQ EQU  1435                EST FILTER IS REQD                           
BADCTRLQ  EQU  1436                CONTROL ENTRY NOT VALID                      
BADUSR1Q EQU   1437                &1 FIELD IS REQD                             
BADUSR2Q EQU   1438                &1 FIELD DATA IS NOT VALID                   
BADUSR3Q EQU   1439                &1 FIELD DATA MUST BE ALPHA                  
BADUSR4Q EQU   1440                &1 FIELD DATA MUST BE NUMERIC                
BADDAILYQ EQU  1441                DAILY MUST BE Y OR N                         
NOTPWCLTQ EQU  1443                CLIENT IS NOT A PW CLIENT                    
BADPWQ    EQU  1444                PW ENTRY NOT VALID                           
BADREQOPQ EQU  1445                REQ= ONLY VALID ON POL EST                   
BADCPYCDQ EQU  1446                COPY CODE NOT ALPHA                          
BADSPREPQ EQU  1447                SPECIAL REP NT VALID                         
BADRANGEQ EQU  1448                ESTIMATE RANGE NOT VALID                     
BADRATYPQ EQU  1449                INVALID RATE TYPE                            
BADMEDQ   EQU  1450                MEDIA C AND N NOT VALID                      
NODEMDELQ EQU  1451                CANNOT DELETE POL DEMO &1                    
BADSDATE  EQU  1452                INVALID START DATE                           
BADEDATE  EQU  1453                INVALID END DATE                             
BADBKTYP  EQU  1454                BAD BOOKTYPE                                 
BADCSBKTY EQU  1455                BAD COMSCORE BOOKTYPE                        
BADCSDTS  EQU  1456                BAD COMSCORE SURVEY DATES                    
NOTOWNER  EQU  1457                NOT RECORD OWNER - CAN'T CHANGE IT           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLES                                                                        
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
EOTBLQ   EQU   C'A'                                                             
DMAX     EQU   20                                                               
*                                                                               
***********************************************************************         
*        VALID NUMERICS TABLE                                         *         
***********************************************************************         
VALDNTBL DC    C' 0123456789-/'                                                 
*                                                                               
***********************************************************************         
*        MEDIA TABLE                                                  *         
***********************************************************************         
MEDTAB   DC   CL1'T',XL1'01'                                                    
MEDTABLQ EQU  *-MEDTAB                                                          
         DC   CL1'R',XL1'02'                                                    
         DC   CL1'N',XL1'03'                                                    
         DC   CL1'X',XL1'04'                                                    
         DC   CL1'C',XL1'08'                                                    
         DC   X'FF'                                                             
         EJECT                                                                  
***********************************************************************         
*        TARGET NUMBER LIST                                           *         
***********************************************************************         
ONETWO   DC    X'0102FF'                                                        
***********************************************************************         
*        ECTAGY                                                       *         
***********************************************************************         
ECTAGY   DC    C'WI'                                                            
         DC    C'WR'                                                            
         DC    C'WT'                                                            
         DC    C'WJ'               WITEST                                       
         DC    C'SJ'                                                            
         DC    C'SX'                                                            
         DC    C'FC'                                                            
         DC    C'BS'                                                            
         DC    C'TH'                                                            
         DC    X'FF'                                                            
***********************************************************************         
*        TYPTAB                                                       *         
***********************************************************************         
TYPTAB   DC    C'M$',X'03'                                                      
         DC    C'M%',X'04'                                                      
         DC    C'Q$',X'05'                                                      
         DC    X'FF'                                                            
***********************************************************************         
*        LINE ID TABLE                                                *         
***********************************************************************         
LINIDTAB DC    C'DD13C2D1'                                                      
         DC    C'DDL1136T'          DDS-LA                                      
         DC    C'DDL1137T'          DDS-LA                                      
         DC    C'DDL1138T'          DDS-LA                                      
         DC    C'DX06200T'         (WAS DDNY720T)                               
         DC    C'DDNY700T'                                                      
         DC    C'DDNYD03T'                                                      
         DC    C'DDNY916T'                                                      
         DC    C'HDTO847T'          HDTO (WAS HDTO823T)                         
         DC    C'HDTO829T'          HDTO (WAS HDTO830T)                         
         DC    C'XDDSC84A'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
ETYPTAB  DC    X'04',C'STW'                                                     
         DC    X'04',C'REG'                                                     
         DC    X'07',C'BARTER'                                                  
         DC    X'08',C'UNWIRED'                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*         FMTDEMO                                                     *         
***********************************************************************         
* ROUTINE TO FORMAT DEMOS ... RF POINTS TO 10 CHARACTER DESCRIPTION   *         
* ... R2 POINTS TO 3 BYTE DEMO ... WORK(1) RETURNS LENGTH ...         *         
* WORK+1 RETURNS DESCRIPTION                                          *         
***********************************************************************         
FMTDEMO  NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R3                                                        
         MVC   WORK(11),SPACES       INITIALIZE WORK                            
         MVI   WORK,0                                                           
         CLI   0(RF),C' '            IF NO DEMO TO FORMAT ... EXIT              
         BNH   FMTDEMOX                                                         
*                                                                               
         LA    R1,11                                                            
         LA    R4,10(RF)                                                        
FMTD5    CLI   0(R4),C' '            SCAN BACKWARDS FOR NON-SPACE               
         BH    FMTD10                                                           
         BCTR  R4,0                                                             
         BCT   R1,FMTD5                                                         
*                                                                               
FMTD10   STC   R1,WORK               LENGTH OF DEMO INTO WORK                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK+1(0),0(RF)       DEMO DESCRIPTION INTO WORK+1               
*                                                                               
         CLI   1(R2),X'21'           IF DOING A USER DEMO, INSERT               
         BNE   FMTD20                USER DEMO HEADER                           
FMTD15   MVC   WORK+11(7),WORK+1                                                
         MVC   WORK+1(3),=C'U /'                                                
         MVC   WORK+4(7),WORK+11                                                
         LLC   R0,2(R2)              USER NAME NUMBER                           
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK+2(1),DUB+7(1)                                               
         IC    R1,WORK               UPDATE LENGTH                              
         AHI   R1,3                                                             
         STC   R1,WORK                                                          
         B     FMTDEMOX                                                         
*                                                                               
FMTD20   CLC   WORK+1(7),EWGTNM                                                 
         BNE   FMTDEMOX                                                         
         MVC   WORK+10(7),WORK+1     IF DEMO MATCHES WEIGHTED DEMO              
         MVC   WORK+1(2),=C'W/'      INSERT HEADER                              
         MVC   WORK+3(7),WORK+10                                                
         IC    R1,WORK               UPDATE LENGTH                              
         AHI   R1,2                                                             
         STC   R1,WORK                                                          
FMTDEMOX J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                                                                               
CHECKOPT NTR1  BASE=*,LABEL=*                                                   
         AR    R0,R1               A(NEW FIELD POSITION)                        
         LA    R3,ESTOPT           A(SCREEN FIELD)                              
         LA    R3,L'ESTOPT-1(R3)   A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
         B     CKOPEXIT                                                         
*                                                                               
CKOPERR  LLC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
CKOPEXIT J     XIT                                                              
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        GET 00 PROFILE                                               *         
***********************************************************************         
*                                                                               
RDPROF   NTR1  BASE=*,LABEL=*                                                   
         XC    SV00PROF,SV00PROF                                                
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S000'                                                 
         MVC   WORK+4(2),AGENCY    READ AGENCY LEVEL                            
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),SVCOFF                                                
         GOTO1 GETPROF,DMCB,WORK,SV00PROF,DATAMGR                               
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
***********************************************************************         
*        VR40 - VALIDATE DESCRIPTION, START DATE, END DATE,           *         
*               OUT OF WEEK ROTATOR AND DAILY FLAG                    *         
***********************************************************************         
*                                                                               
         USING ESTHDR,R3                                                        
VR40     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
***********************************************************************         
*                                                                               
         MVC   ERRNUM,=AL2(DESERR1)                                             
         LA    R2,ESTDESCH           DESCRIPTION REQUIRED                       
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         MVI   WORK2,C' '                                                       
         MVI   SONIA,C' '                                                       
         CLC   =C'JILLREBER',ESTDESC                                            
         BE    VR50                                                             
         CLC   =C'SONIAPRICE',ESTDESC                                           
         BE    VR50                                                             
         CLC   =C'KENSUPEK',ESTDESC  IF DESCRIPTION IS JILLREBER,               
         BNE   VR60                  SONIAPRICE OR KENSUPEK ...                 
VR50     MVI   WORK2,C'D'            ALLOW DATE CHANGES W/O VALIDATING          
         MVI   SONIA,C'C'            ALLOW CHANGE OF CASH PRD ESTIMATE          
         B     VR70                  AND DON'T CHANGE DESCRIPTION               
*                                                                               
VR60     CLI   T217FFD+1,C'*'        DDS TERMINAL?                              
         BNE   VR62                  NO, CANNOT DELETE THESE!                   
         CLI   ACTEQU,ACTADD         ACTION ADD?                                
         BE    VR62                  YES, DOESN'T EXIST YET!                    
         CLC   =C'AKAT 8675309',ESTDESC                                         
         BE    VR61                                                             
         CLC   =C'ABEA 8675309',ESTDESC                                         
         BE    VR61                                                             
         CLC   =C'EJOR 8675309',ESTDESC                                         
         BNE   VR62                                                             
*                                                                               
VR61     GOTO1 =A(DEL),RR=RELO       DELETE DIR/FIL AND PASSIVE                 
         MVC   ERRNUM,=AL2(770)                                                 
         B     SPERREX                                                          
*                                                                               
VR62     MVC   EDESC,ESTDESC         DESCRIPTION VALIDATION COMPLETE            
         OC    EDESC,SPACES          ... SAVE TO RECORD                         
*                                                                               
***********************************************************************         
*                                                                               
VR70     LHI   R0,DATERR1                                                       
         LA    R2,ESTSTRDH           START DATE REQUIRED                        
         CLI   5(R2),0                                                          
         JE    SPERR0                                                           
*                                                                               
*                                                                               
         LHI   R0,BADSDATE            MUST BE VALID M/D/Y (US)                  
         GOTO1 DATVAL,DMCB,(0,ESTSTRD),SYR                                      
         OC    DMCB(4),DMCB          YY RETURNED IN SYR, MM RETURNED            
         JZ    SPERR0                IN SMN, DD RETURNED IN SDY                 
*                                                                               
*                                                                               
         LHI   R0,ESTERR5                                                       
         CLI   ACTEQU,ACTADD                                                    
         BE    VR100                                                            
         CLI   WORK2,C'D'            MASTER OR SUB-ESTIMATE'S START             
         BE    VR100                 DATE CANNOT BE CHANGED UNLESS              
         CLI   EMSTRIND,0            DATES CAN BE CHANGED W/0 VALID-            
         BE    VR80                  ATING                                      
         CLC   SVSTRT,SYR                                                       
         BE    VR100                                                            
         J     SPERR0                                                           
*                                                                               
VR80     LHI   R0,ESTERR6            NON-MASTER OR NON-SUB ESTIMATE'S           
         OC    SVCPP,SVCPP           START DATE CANNOT BE ADVANCED UN-          
         BNZ   VR90                  LESS CPP EST. IS 0 AND TERMINAL            
         GOTO1 =A(CKLINID),RR=RELO   IS AUTHORIZED (OR IF DATES CAN BE          
         BE    VR100                 CHANGED W/O VALIDATING)                    
VR90     CLC   SVSTRT,SYR                                                       
         JL    SPERR0                                                           
*                                                                               
***********************************************************************         
*                                                                               
VR100    LHI   R0,DATERR2                                                       
         LA    R2,ESTENDDH           END DATE REQUIRED                          
         CLI   5(R2),0                                                          
         JE    SPERR0                                                           
*                                                                               
*                                                                               
         LHI   R0,BADEDATE           MUST BE VALID M/D/Y (US)                   
         GOTO1 DATVAL,DMCB,(0,ESTENDD),EYR                                      
         OC    DMCB(4),DMCB          YY RETURNED IN EYR, MM RETURNED            
         JZ    SPERR0                IN EMN, DD RETURNED IN EDY                 
*                                                                               
         CLI   SVAPROF+7,C'C'        CANADIAN AGENCY?                           
         BNE   VR105                 NO                                         
         CLC   SYR(6),=X'FBF9F1F2F2F9' ESTIMATE START AFTER DEC29/19?           
         BH    VR105                   YES                                      
         CLC   EYR(6),=X'FBF9F1F2F2F9' ESTIMATE END ON/BEFORE DEC29/19?         
         BNH   VR105                   YES                                      
         LHI   R0,ESTERR12           EST DATES CANNOT CROSS DEC29/19            
         J     SPERR0                GIVE USER THE ERROR MESSAGE                
*                                                                               
VR105    MVI   OWSDAY,0              CLEAR THIS PLEAZE!!!                       
         XC    MAKEGDDT,MAKEGDDT     AND THIS!!                                 
*                                                                               
         LHI   R0,ESTERR5                                                       
         CLI   ACTEQU,ACTADD         MASTER OR SUB-ESTIMATE'S END DATE          
         BE    VR130                 CANNOT BE CHANGED UNLESS DATES CAN         
         CLI   WORK2,C'D'            BE CHANGED W/O VALIDATING                  
         BE    VR130                                                            
         CLI   EMSTRIND,0            SINCE NEITHER START OR END DATE            
         BE    VR110                 FOR MASTER OR SUB-ESTIMATE HAVE            
         CLC   SVEND,EYR             CHANGED ... DATE VALIDATION COMP-          
         JNE   SPERR0                LETE                                       
         B     VR350                                                            
*                                                                               
*                                                                               
VR110    LHI   R0,ESTERR6            NON-MASTER OR NON-SUB ESTIMATE'S           
         OC    ECPPEST,ECPPEST       END DATES CANNOT BE CUT BACK               
         BNZ   VR120                 UNLESS CPP EST IS 0 AND TERMINAL           
         GOTO1 =A(CKLINID),RR=RELO   IS AUTHORIZED (OR IF DATES CAN BE          
         BE    VR130                 CHANGED W/0 VALIDATING)                    
VR120    CLC   SVEND,EYR                                                        
         JH    SPERR0                                                           
*                                                                               
VR130    LHI   R0,ESTERR7            END DATE MUST BE LATER OR EQUAL TO         
         CLC   SYR(6),EYR            START DATE                                 
         JH    SPERR0                                                           
*                                                                               
***********************************************************************         
*                                                                               
         LHI   R0,ESTERR8            UNLESS START AND END DATE HAVE NOT         
         CLI   OVSYS,3               BEEN MODIFIED FOR A MASTER OR              
         BNE   VR140                 SUB-ESTIMATE ... NETPAK ESTIMATES          
         LA    R2,ESTMEDKH           MUST BE MEDIA N                            
         CLI   QMED,C'N'                                                        
         JNE   SPERR0                                                           
         B     VR150                                                            
*                                                                               
VR140    LA    R2,ESTSTATH           UNLESS START AND END DATE HAVE NOT         
         CLC   ESTSTAT(3),=C'OLD'    BEEN MODIFIED FOR A MASTER OR              
         BE    ERRINV                SUB-ESTIMATE ... 'OLD' AND 'NEW'           
         CLC   ESTSTAT(3),=C'NEW'    INVALID STATUS FOR NON-NETPAK              
         BE    ERRINV                ESTIMATES                                  
*                                                                               
***********************************************************************         
*                                                                               
VR150    CLI   ACTEQU,ACTADD         ON AN (ADD) OR (START DATE CHANGE)         
         BE    VR190                 OR (END DATE CHANGE)  ...                  
         CLC   SVSTRT,SYR                                                       
         BNE   VR190                                                            
         CLC   SVEND,EYR                                                        
         BE    VR220                                                            
*                                                                               
VR190    LHI   R0,ESTERR9                                                       
         GOTO1 =A(CHKEDTS),RR=RELO    CHECK DATES                               
         LA    R2,ESTSTRDH                                                      
         OC    SVPWPCT,SVPWPCT        IF PW ESTIMATE                            
         BZ    VR220                                                            
*                                                                               
VR192    GOTO1 ADDAY,DMCB,SYR,WORK,98        WITHIN PERCENTAGE IS ZERO)         
         CLC   WORK(6),EYR                                                      
         BNH   SPERREX                                                          
*                                                                               
VR220    GOTO1 DATCON,DMCB,EYR,(2,EMGDTE)  SAVE LATEST MAKEGOOD DATES           
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTOWDH            OUT OF WEEK ROTATOR                        
         CLI   5(R2),0               (START DATE'S DAY OF THE WEEK)             
         BNE   VR230                                                            
*                                                                               
         MVI   DMCB,0                IF NOT INPUTTED, BUT CLIENT CON-           
         CLI   SVCLEX+10,C'Y'        TROLLED ... GO VALIDATE START DATE         
         BE    VR250                                                            
         B     VR260                                                            
*                                                                               
VR230    LHI   R0,OWRERR1                                                       
         CLI   8(R2),C'N'            IF INPUTTED ...                            
         BE    VR260                 OUT OF WEEK ROTATOR MUST START             
VR240    CLI   8(R2),C'Y'                                                       
         JNE   SPERR0                                                           
*                                                                               
VR250    GOTO1 GETDAY,DMCB,(0,SYR),DUB                                          
         LHI   R0,BADDATE                                                       
         CLC   DUB(3),SPACES                                                    
         JE    SPERR0                                                           
         LHI   R0,MONDERR                                                       
         CLI   0(R1),1               COPY START DATE'S DAY OF THE WEEK          
         JE    SPERR0                INTO OWSDAY                                
VR260    MVC   OWSDAY,DMCB                                                      
         CLI   POLSW,0                                                          
         BE    VR350                                                            
*                                                                               
***********************************************************************         
*                                                                               
         CLI   POLSW,1                                                          
         BNE   VR290                                                            
         LA    R2,ESTSTRDH           IF (CHANGING OR ADDING THE BRAND           
         CLC   POLSTRT,SYR           ESTIMATE FOR A CLIENT WITH A POL           
         BNE   ERRBRPOL              ESTIMATE)...START DATES, END DATES         
         LA    R2,ESTENDDH           AND OUT OF WEEK ROTATOR MUST MATCH         
         CLC   POLEND,EYR            POL ESTIMATE OR ERROR                      
         BNE   ERRBRPOL                                                         
         LHI   R0,BRPOLER2                                                      
         LA    R2,ESTOWDH                                                       
         CLC   POLOWDAY,OWSDAY                                                  
         JNE   SPERR0                                                           
         LHI   R0,BRPOLERB           BRAND EST DAILY FLAG MUST AGREE            
         LA    R2,ESTOPTH            OPTIONS FIELD                              
         CLC   POLDLY,NEWDLY         BRAND DAILY FIELD MATCHES POL?             
         JNE   SPERR0                NO - ERROR                                 
         B     VR350                                                            
***********************************************************************         
* POLSW=0 - ADD/CHANGE BRAND AND POL DOES NOT EXISTS YET (DEFUNCT)              
* POLSW=1 - ADD/CHANGE BRAND AND POL EXISTS                                     
* POLSW=2 - CHANGE POL                                                          
* POLSW=3 - ADD POL                                                             
*                                                                               
* AT THIS POINT, POLSW IS 2 OR 3 (ADD/CHANGE OF POL EST)                        
* IF ANY OF THE FOLLOWING CHANGES, WE UPDATE ALL THE BRANDS                     
* ESTIMATE START DATE                                                           
* ESTIMATE END DATE                                                             
* OOWR                                                                          
* DAILY FLAG (THIS ONE IS NEW AS WE FOUND THAT BRD/POL ARE INDEPENDENT)         
*                                                                               
* NOTE *      * NOTE *      * NOTE *      * NOTE *      * NOTE *      *         
* IF POLSW=3 THEN WE SHOULD NOT HAVE ANY BRANDS ON THE FILE BUT THE             
* CODE BELOW WAS NEVER CHANGED WHEN WE SWITCHED TO TRUE POL                     
* NOTE *      * NOTE *      * NOTE *      * NOTE *      * NOTE *      *         
***********************************************************************         
VR290    CLI   EDAILY,C'Y'           IS DAILY POL EST SET TO A "Y"?             
         BNE   VR291                 NO                                         
         LA    R2,ESTSTRDH           SET R2 IN CASE OF ERROR                    
         MVC   ERRNUM,=AL2(DLYERR)   MAX 53 DAYS BETWEEN START & END            
         L     RF,ACOMFACS           RF = A(COMFACS)                            
         USING COMFACSD,RF           COMFACS DSECT                              
         GOTO1 PERVERT,DMCB,SYR,EYR,WORK,WORK+4,WORK+8                          
         LH    R4,8(R1)              NUMBER OF DAYS BETWEEN START & END         
         CHI   R4,53                 > 53 DAYS?                                 
         BH    SPERREX               YES - DON'T UPDATE BRD ESTIMATES!          
         DROP  RF                    DROP COMFACS USING                         
*                                                                               
VR291    CLI   POLSW,2               IF (CHANGING THE POL ESTIMATE) ...         
         BNE   VR310                 AND NO BRANDS EXIST, ALLOW POL             
         XC    KEY,KEY               DEMO DELETE                                
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 =A(NEXTPRD),RR=RELO                                              
*                                                                               
***********************************************************************         
*                                                                               
VR300    CLC   SVSTRT,SYR            POL EST START DATE CHANGED?                
         BNE   VR310                 YES - UPDATE BRAND ESTIMATES               
         CLC   SVEND,EYR             POL EST END DATE CHANGED?                  
         BNE   VR310                 YES - UPDATE BRAND ESTIMATES               
         CLC   SVOWS,OWSDAY          POL EST OOWR CHANGED?                      
         BNE   VR310                 YES - UPDATE BRAND ESTIMATES               
         CLC   SVDLY,NEWDLY          POL EST DAILY FLAG CHANGED?                
         BE    VR350                 NO - NO NEED TO UPDATE BRANDS              
*                                                                               
***********************************************************************         
*                                                                               
VR310    XC    KEY,KEY               IF (ADDING A POL ESTIMATE) ... ALL         
         MVC   KEY(4),ESTKEY         BRAND ESTIMATES FOR PRODUCT MUST           
         MVC   SVKEY,ESTKEY          SAVE POL KEY                               
*                                                                               
VR320    GOTO1 =A(NEXTPRD),RR=RELO   MATCH THE POL'S NEW START DATE,            
         BNE   VR348                 END DATE AND OUT OF WEEK ROTATOR           
         MVC   AIO,AIO1              USE IO 1                                   
         CLI   POLSW,3               UNLESS ADDING POL REC                      
         BNE   *+10                                                             
         MVC   AIO,AIO3              THEN USE IO 3                              
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   AIO,AIO1              RESET IO                                   
         CLI   ACTEQU,ACTADD                                                    
         BNE   VR330                                                            
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   ERRBRPOL                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   ERRBRPOL                                                         
         LHI   R0,BRPOLER2                                                      
         LA    R2,ESTOWDH                                                       
         CLC   EOWSDAY,OWSDAY                                                   
         JNE   SPERREX                                                          
*                                                                               
         LHI   R0,EDOLERR            CHECK FOR DOLLARS ON EST                   
         CLI   ESTDESC,C'@'          (UNLESS @ IN DESCRIPTION                   
         BE    VR320                 FIELD)                                     
*                                                                               
         LA    R1,26               CAN'T ADD POL EST IF PRDS ACTIVE             
         LA    R5,EORD                                                          
         CP    0(6,R5),=PL6'0'                                                  
         JNE   SPERR0              CHECK IF OK                                  
         LA    R5,6(R5)                                                         
         BCT   R1,*-14                                                          
*                                                                               
         LA    R1,26                                                            
         LA    R5,EPAID                                                         
         CP    0(6,R5),=PL6'0'                                                  
         JNE   SPERR0              CHECK IF OK                                  
         LA    R5,6(R5)                                                         
         BCT   R1,*-14                                                          
         B     VR320               CHECK NEXT PRD                               
*                                                                               
***********************************************************************         
*                                                                               
VR330    CLC   SVSTRT,SYR            IF (CHANGING A POL ESTIMATE) ...           
         BNE   VR340                                                            
         CLC   SVEND,EYR                                                        
         BNE   VR340                                                            
         CLC   SVOWS,OWSDAY                                                     
         BNE   VR340                                                            
         CLC   SVDLY,NEWDLY          POL EST DAILY FLAG CHANGED?                
         BE    VR320                 NO                                         
*                                                                               
VR340    MVC   ESTART,SYR            CHANGE THE START DATE, END DATE,           
         MVC   EEND,EYR              MAKE GOOD DATE AND OUT OF WEEK             
         MVC   EMGDTE,MAKEGDDT       ROTATOR FOR ALL THE PRODUCT'S              
         MVC   EOWSDAY,OWSDAY        BRAND ESTIMATES TO MATCH THE               
         MVC   EDAILY,NEWDLY         DAILY FLAG                                 
         GOTO1 =A(PTREC),RR=RELO     NEW POL DATA                               
         MVC   ESTKEY,KEY                                                       
*                                                                               
         MVC   CANKEY,KEY            SET KEY TO UPDATE                          
         GOTO1 =A(CANTV),RR=RELO     GO UPDATE N AND C RECS                     
         B     VR320                                                            
*                                                                               
***********************************************************************         
*                                                                               
VR348    MVI   WORK2,C' '            RESET DATE CHANGE W/0 VALIDATION           
         MVC   ESTKEY,SVKEY          RESTORE POL KEY                            
         CLI   ACTEQU,ACTADD                                                    
         BE    VR350                                                            
         MVC   KEY,ESTKEY                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
*                                                                               
VR350    MVI   WORK2,C' '            RESET DATE CHANGE W/0 VALIDATION           
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   ESTART,SYR            STORE START DATE, END DATE AND             
         MVC   EEND,EYR              OUT OF WEEK ROTATOR INTO RECORD            
         MVC   EOWSDAY,OWSDAY                                                   
         MVC   EMGDTE,MAKEGDDT                                                  
         MVC   EDAILY,NEWDLY                                                    
VR358    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VR360 - VALIDATE BILL BASIS, COMM PCT AND COMM BASIS         *         
***********************************************************************         
*                                                                               
VR360    NTR1  BASE=*,LABEL=*                                                   
         CLI   SCFLDEBF,0            FULL ACCESS?                               
         BNE   VR400                 NO - CANNOT CHANGE THIS FIELD              
*                                                                               
         LA    R2,ESTBBASH                                                      
         XC    EBILLBAS(5),EBILLBAS  BILL BASIS IS NOT REQUIRED                 
         CLI   5(R2),0                                                          
         BE    VR400                                                            
*                                                                               
VR370    XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'SB1X'   DOES PROFILE EXIST FOR THIS                
         MVI   WORK+16,X'A2'         AGENCY, MEDIA, CLIENT?                     
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
*                                                                               
         CLI   WORK+11,C' '          BILL FORMULA NOT ALLOWED IF                
         BNH   VR380                 OPTION 12 IS SET                           
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
*                                                                               
VR380    LLC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         LA    R5,8(R2)                                                         
*                                                                               
         CLI   8(R2),C'C'            CHECK BILL BASIS FOR COMMISION             
         BNE   VR390                                                            
         OI    EBILLBAS,X'40'                                                   
         BCTR  RE,0                                                             
         MVC   ERRNUM,=AL2(BBERR)    CANNOT BE JUST A "C"                       
         CLI   5(R2),1                                                          
         BE    SPERREX                                                          
         LA    R5,1(R5)              BUMP PAST THE "C"                          
*                                                                               
VR390    MVC   ERRNUM,=AL2(BBERR)    BILL-BASIS MUST BE EITHER                  
         EX    RE,*+8                "GROSS" OR "NET"                           
         B     *+10                                                             
         CLC   0(0,R5),=C'GROSS'                                                
         BE    VR400                                                            
*                                                                               
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'NET'                                                  
         BNE   SPERREX               BILL-BASIS X'40' FOR COMMISION,            
         OI    EBILLBAS,X'10'        X'10' FOR NET                              
*                                                                               
***********************************************************************         
*                                                                               
VR400    CLI   SCFLDEBF,0            FULL ACCESS?                               
         BNE   VR430                 NO - CANNOT CHANGE THIS FIELD              
*                                                                               
         LA    R2,ESTCPCTH                                                      
         MVC   ERRNUM,=AL2(COMPERR1) COMM.% REQUIRED IF COMM BASIS              
         CLI   5(R2),0               IS PRESENT                                 
         BNE   VR401                                                            
         CLI   ESTCBASH+5,0                                                     
         BNE   SPERREX                                                          
         B     VR430                                                            
*                                                                               
VR401    MVC   ERRNUM,=AL2(COMPERR5) 1ST CHARACTER OF COMM.% MUST BE            
         CLI   ESTCPCT,C'+'          EITHER + OR -                              
         BE    VR402                                                            
         CLI   ESTCPCT,C'-'                                                     
         BNE   SPERREX                                                          
*                                                                               
VR402    MVC   ERRNUM,=AL2(COMPERR2) COMM.% MUST BE VALID NUMERIC               
         LLC   R0,5(R2)              (EXCEPT 1ST CHAR)                          
         BCTR  R0,R0                                                            
         GOTO1 CASHVAL,DMCB,(4,ESTCPCT+1),(R0)                                  
         CLI   DMCB,X'FF'                                                       
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(COMPERR3) 100% IS MAXIMUM COMM.%                     
         L     R0,DMCB+4                                                        
         C     R0,=F'1000000'                                                   
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(COMPERR4) 1% IS MINIMUM COMM.%                       
         C     R0,=F'0'                                                         
         BNH   SPERREX                                                          
*                                                                               
         CLI   ESTCPCT,C'+'                                                     
         BE    VR410                                                            
         LCR   R0,R0                                                            
VR410    ST    R0,FULL               MAKE COMM.% NEGATIVE AND SAVE              
         MVC   EBILLCOM,FULL         INTO RECORD                                
*                                                                               
***********************************************************************         
*                                                                               
VR430    CLI   SCFLDEBF,0            FULL ACCESS?                               
         BNE   VR460                 NO - CANNOT CHANGE THIS FIELD              
*                                                                               
         LA    R2,ESTCBASH                                                      
         MVC   ERRNUM,=AL2(CBASERR1) COM.BASIS REQUIRED IF COM.% IS             
         CLI   5(R2),0               PRESENT                                    
         BNE   VR440                                                            
*                                                                               
         CLI   ESTCPCTH+5,0                                                     
         BNE   SPERREX                                                          
         B     VR460                                                            
*                                                                               
VR440    CLI   WORK+11,C' '                                                     
         BNH   VR450                                                            
         CLI   WORK+11,C'N'                                                     
         BNE   ERRINV                                                           
VR450    MVC   ERRNUM,=AL2(CBASERR2) COM.BASIS MUST BE EITHER "GROSS"           
         LLC   RE,5(R2)              OR "NET"                                   
         BCTR  RE,0                                                             
         LA    R5,8(R2)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'GROSS'                                                
         BE    VR460                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),=C'NET'                                                  
         BNE   SPERREX                                                          
*                                                                               
         OI    EBILLBAS,X'01'                                                   
VR460    OC    EBILLBAS(5),EBILLBAS                                             
         BNZ   VR470                                                            
         CLI   ESTBBASH+5,0                                                     
         BE    VR489                                                            
         B     VR480                                                            
*                                                                               
VR470    OC    EBILLCOM,EBILLCOM                                                
         BNZ   VR489                                                            
         CLI   EBILLBAS,X'40'        BILL BASIS SET X'01' FOR COM BASIS         
         BNE   VR489                 NET, X'80' FOR COM BASIS GROSS             
VR480    OI    EBILLBAS,X'80'                                                   
VR489    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        VR980 - VALIDATE RATING BOOK AND HUT ADJUSTMENT              *         
***********************************************************************         
VR980    NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ESTRBKH            RATING BOOK REQUIRED                       
         MVC   ERRNUM,=AL2(RATERR1)                                             
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         XC    EBOOK,EBOOK           IF NETPAK, RATING BOOK MUST BE             
         CLI   OVSYS,3               GREATER THAN ZERO                          
         BNE   VR990                                                            
         CLI   8(R2),C'0'                                                       
         BL    ERRINV                                                           
         B     VR1000                                                           
*                                                                               
VR990    CLC   8(6,R2),=C'LATEST'    IF NOT NETPAK AND RATING BOOK IS           
         BE    VR1010                'LATEST' DO NOT VALIDATE FOR DATE          
*                                                                               
VR1000   MVC   ERRNUM,=AL2(BADBOOKQ)          RATING BOOK NOT VALID             
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK     M/Y AND SAVE INTO RECORD          
         OC    DMCB(4),DMCB                                                     
         BZ    SPERREX                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   EBOOK,WORK+10                                                    
VR1010   CLI   POLSW,0                                                          
         BE    VR1050                                                           
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1030                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER3) ESTIMATE) ... RATING BOOK FOR              
         CLC   POLBOOK,EBOOK         BRAND ESTIMATES MUST MATCH THE             
         BNE   SPERREX               POL'S RATING BOOK                          
         B     VR1050                                                           
*                                                                               
VR1030   MVC   SVBOOK,EBOOK          IF (ADDING A POL ESTIMATE) OR              
*                                    (CHANGING A POL ESTIMATE) ...              
*                                    SAVE NEW RATING BOOK INTO SVBOOK           
*                                                                               
***********************************************************************         
*                                                                               
VR1050   LA    R2,ESTHUTH            HUT ADJUSTMENT REQUIRED                    
         LHI   R0,HUTERR1                                                       
         CLI   5(R2),0                                                          
         JE    SPERR0                                                           
*                                                                               
         MVI   EHUTADJ,0             IF HUT ADJUSTMENT NOT 'AUTO' ...           
         CLC   8(4,R2),=C'AUTO'      MUST BE 3 CHARACTERS LONG AND A            
         BE    VR1070                                                           
*                                                                               
         LHI   R0,BADHUTQ                                                       
         CLI   5(R2),3                                                          
         JNE   SPERR0                                                           
         MVC   WORK(3),8(R2)                                                    
         MVC   WORK+3(3),=C'/77'                                                
         GOTO1 DATVAL,DMCB,(2,WORK),WORK+10                                     
         OC    DMCB(4),DMCB                                                     
         JZ    SPERR0                                                           
         PACK  DUB,WORK+12(2)                                                   
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,EHUTADJ                                                       
*                                                                               
VR1070   CLI   POLSW,0                                                          
         BE    VR1081                                                           
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1080                ESTIMATE FOR A CLIENT WITH A POL           
         LHI   R0,BRPOLER5           ESTIMATE) ... HUT ADJUSTMENT FOR           
         CLC   POLHUT,EHUTADJ        BRAND ESTIMATE MUST MATCH POL'S            
         JNE   SPERR0                HUT ADJUST.                                
         B     VR1081                                                           
*                                                                               
*                                    IF (ADDING A POL ESTIMATE) OR              
VR1080   MVC   SVHUT,EHUTADJ         (CHANGING A POL ESTIMATE) ...              
VR1081   J     XIT                   SAVE NEW HUT ADJUSTMENT INTO SVHUT         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VR1090 - VALIDATE DAYPART MENU AND FILTERS                   *         
***********************************************************************         
VR1090   NTR1  BASE=*,LABEL=*                                                   
         MVC   ERRNUM,=AL2(MENERR1)                                             
         LA    R2,ESTMENUH           DAYPART MENU                               
         CLI   5(R2),0               REQUIRED                                   
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(MENERR2)  MUST BE ONE CHARACTER LONG                 
         CLI   5(R2),1                                                          
         BNE   SPERREX                                                          
*                                                                               
         MVC   EDAYMENU,8(R2)        IS DAYPART MENU ON FILE?                   
         MVC   DMCB(2),AGENCY        AGENCY                                     
         MVC   DMCB+2(1),QMED        MEDIA                                      
         MVC   DMCB+3(1),EDAYMENU    DAYPART MENU                               
         GOTO1 DPTRD,DMCB,,BLOCK,DATAMGR                                        
         MVC   ERRNUM,=AL2(DPTERRQ)                                             
         CLI   DMCB+8,X'FF'          IF MENU NOT ON FILE ... ERROR              
         BE    SPERREX                                                          
*                                                                               
VR1100   CLI   POLSW,0                                                          
         BE    VR1120                                                           
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1110                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER4) ESTIMATE) ... DAYPART MENU FOR             
         CLC   POLDPT,EDAYMENU       BRAND ESTIMATE MUST MATCH THE              
         BNE   SPERREX               POL'S DAYPART MENU                         
         B     VR1120                                                           
*                                                                               
VR1110   MVC   SVDPT,EDAYMENU        IF (ADDING A POL ESTIMATE) OR              
*                                    (CHANGING A POL ESTIMATE) ...              
*                                    SAVE NEW DEPT. MENU INTO SVDPT             
*                                                                               
**********************************************************************          
*                                                                               
VR1120   LA    R2,ESTFLTRH           FILTERS                                    
         XC    EPROF(3),EPROF                                                   
         CLI   5(R2),0                                                          
         BE    VR1150                                                           
         OC    8(3,R2),SPACES        IF ALL SPACES ... TREAT AS NO              
         CLC   8(3,R2),SPACES        INPUT                                      
         BE    VR1150                                                           
*                                                                               
         LHI   R0,BADFLTRQ                                                      
         MVC   EPROF(3),ESTFLTR                                                 
         LA    R4,EPROF                                                         
         LA    R5,3                  IF INPUT ...                               
VR1130   CLI   0(R4),C' '            FILTER MUST BE A COMBINATION OF 3          
         BE    VR1140                OR LESS ALPHANUMERICS                      
         CLI   0(R4),C'A'                                                       
         BL    SPERR0                WE'RE RELIANT ON R0, SO SPERR0             
         CLI   0(R4),C'9'                                                       
         BH    SPERR0                                                           
VR1140   LA    R4,1(R4)                                                         
         BCT   R5,VR1130                                                        
*                                                                               
VR1150   CLI   SVCLEX+3,C'Y'         IF CLIENT REQUIRES FILTER ...              
         BNE   VR1160                FILTER MUST BE INPUTTED                    
         LHI   R0,ERRNOFLTQ                                                     
         OC    EPROF(3),EPROF                                                   
         JZ    SPERR0                                                           
*                                                                               
VR1160   CLI   SVF0PROF+2,C'Y'       IF FILTER 1 REQUIRED ...                   
         BNE   VR1170                FIRST CHARACTER OF FILTER CANNOT           
         MVC   ERRNUM,=AL2(FLT1MSS)  BE A SPACE                                 
         CLI   EPROF,C' '                                                       
         BNH   SPERREX                                                          
*                                                                               
VR1170   CLI   SVF0PROF+3,C'Y'       IF FILTER 2 REQUIRED ...                   
         BNE   VR1180                FIRST CHARACTER OF FILTER CANNOT           
         MVC   ERRNUM,=AL2(FLT2MSS)  BE A SPACE                                 
         CLI   EPROF+1,C' '                                                     
         BNH   SPERREX                                                          
*                                                                               
VR1180   CLI   SVF0PROF+4,C'Y'       IF FILTER 3 REQUIRED ...                   
         BNE   VR1190                FIRST CHARACTER OF FILTER CANNOT           
         MVC   ERRNUM,=AL2(FLT3MSS)  BE A SPACE                                 
         CLI   EPROF+2,C' '                                                     
         BNH   SPERREX                                                          
VR1190   CLI   POLSW,0                                                          
         BE    VR1209                                                           
*                                                                               
         CLI   SVF0PROF,C'N'         IF (CHANGING OR ADDING A BRAND             
         BE    VR1209                ESTIMATE FOR A CLIENT WITH A POL           
         CLI   POLSW,1               ESTIMATE) ... FILTERS FOR BRAND            
         BNE   VR1200                ESTIMATE MUST MATCH THE POL'S              
         MVC   ERRNUM,=AL2(BRPOLER8) FILTERS UNLESS (FIRST BYTE OF              
         CLC   SVFLTRS,EPROF         SVF0PROF IS N) OR (MEDIA IS                
         BE    VR1209                RADIO AND FIRST BYTE OF SVCLPROF           
         CLI   QMED,C'R'             IS NOT 0)                                  
         BNE   SPERREX                                                          
         CLI   SVCLPROF,C'0'                                                    
         BE    SPERREX                                                          
         B     VR1209                                                           
*                                    IF (ADDING A POL ESTIMATE) OR              
VR1200   MVC   SVFLTRS,EPROF         (CHANGING A POL ESTIMATE) ...              
*                                    SAVE NEW FILTERS INTO SVFLTRS              
*                                    UNLESS (FIRST BYTE OF SVF0PROF             
VR1209   J     XIT                   IS N)                                      
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VR1210 - VALIDATES CONTROL AND RETAIL SCHEME                 *         
***********************************************************************         
VR1210   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ESTECONH           ESTIMATE CONTROL                           
         MVI   ECONTROL,0                                                       
         CLI   5(R2),0               NOT REQUIRED                               
         BE    VR1240                                                           
*                                                                               
         OC    ESTECON,SPACES        'E' IS VALID CONTROL FOR ONLY              
         CLC   ESTECON(2),=C'E '     9 AGENCIES                                 
         BNE   VR1230                                                           
         MVI   ECONTROL,EBILESTQ                                                
         LAY   R1,ECTAGY                                                        
VR1220   CLI   0(R1),X'FF'                                                      
         BE    ERRINV                                                           
         CLC   0(2,R1),AGENCY                                                   
         BE    VR1240                                                           
         LA    R1,2(R1)                                                         
         B     VR1220                                                           
VR1230   LHI   R0,BADCTRLQ           ONLY OTHER VALID CONTROL IS 'NSC'          
         CLC   ESTECON(3),=C'NSC'                                               
         JNE   SPERR0                                                           
         MVI   ECONTROL,ENSEPCMQ                                                
*                                                                               
VR1240   MVC   ERRNUM,=AL2(CONTER1)                                             
         CLI   ACTEQU,ACTADD         CONTROL CANNOT BE CHANGED FOR              
         BE    VR1270                ESTIMATE IF TERMINAL IS NOT                
         GOTO1 =A(CKLINID),RR=RELO   AUTHORIZED AND BILLING RECORDS             
         BE    VR1270                EXIST FOR EST                              
         CLC   SVECON,ECONTROL                                                  
         BE    VR1270                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(9),ESTKEY                                                    
         GOTO1 HIGH                                                             
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         JE    SPERR0                                                           
         CLI   POLSW,0                                                          
         BE    VR1290                                                           
*                                                                               
VR1250   MVC   ERRNUM,=AL2(CONTER2)                                             
         CLC   QPRD,=C'POL'         CONTROL CANNOT BE CHANGED FOR POL           
         BNE   VR1270               ESTIMATE IF TERMINAL IS NOT AUTHOR-         
         XC    KEY,KEY              IZED AND BILLING RECORD EXISTS FOR          
         MVC   KEY(4),ESTKEY        ANY BRAND ESTIMATES                         
VR1260   GOTO1 =A(NEXTPRD),RR=RELO                                              
         BNE   VR1270                                                           
         GOTO1 SEQ                                                              
         CLC   KEY(8),KEYSAVE                                                   
         JE    SPERR0                                                           
         B     VR1260                                                           
*                                                                               
VR1270   CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1280                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER6) ESTIMATE) ... CONTROL FOR BRAND            
         CLC   POLCON,ECONTROL       ESTIMATE MUST MATCH THE POL'S              
         BE    VR1290                CONTROL                                    
         B     SPERREX                                                          
*                                                                               
VR1280   MVC   SVECON,ECONTROL       IF (ADDING A POL ESTIMATE) OR              
*                                    (CHANGING A POL ESTIMATE) ...              
*                                    SAVE NEW CONTROL INTO SVECON               
*                                                                               
***********************************************************************         
*                                                                               
VR1290   LA    R2,ESTERTLH           RETAIL SCHEME                              
         XC    ERTLSCHM,ERTLSCHM                                                
         CLI   5(R2),0                                                          
         BE    *+10                                                             
         MVC   ERTLSCHM,8(R2)                                                   
         CLI   POLSW,0                                                          
         BE    VR1310                                                           
*                                                                               
         CLI   SVF0PROF+1,C'N'       IF (CHANGING OR ADDING A BRAND             
         BE    VR1310                ESTIMATE FOR A CLIENT WITH A               
         CLI   POLSW,1               POL ESTIMATE) ... RETAIL SCHEME            
         BNE   VR1300                FOR BRAND ESTIMATE MUST MATCH THE          
         CLC   POLRTL,ERTLSCHM       POL'S RETAIL SCHEME UNLESS (FIRST          
         BE    VR1310                BYTE OF SVF0PROF IS N) OR (MEDIA           
         MVC   ERRNUM,=AL2(BRPOLER7) IS RADIO AND FIRST BYTE OF                 
         CLI   QMED,C'R'             SVCLPROF IS NOT 0)                         
         BNE   SPERREX                                                          
         CLI   SVCLPROF,C'0'                                                    
         BE    SPERREX                                                          
         B     VR1310                                                           
*                                                                               
*                                    IF (ADDING A POL ESTIMATE) OR              
*                                    (CHANGING A POL ESTIMATE) ...              
VR1300   MVC   SVRTL,ERTLSCHM        SAVE NEW RETAIL SCHEME INTO SVRTL          
*                                    UNLESS (FIRST BYTE OF SVF0PROF IS          
*                                    N)                                         
*                                                                               
***********************************************************************         
*                                                                               
VR1310   XC    USERDATA,USERDATA                                                
         OC    SVE1USER,SVE1USER     ANY "ESTIMATE 1" INFO?                     
         BZ    VR1320                                                           
*                                                                               
         LA    R2,ESTUSR1H           SET UP PARAMETERS FOR EDTUSR CALL          
         ST    R2,AUSR                                                          
         MVC   UTYPE,SVEU1TYP        WITH ESTIMATE 1 INFO - TYPE,LENGTH         
         MVC   LEN,SVEU1LEN          AND FIRST FLAG                             
         MVC   FLAG1,SVEU1FL1                                                   
         MVC   FLAG2,SVEU1FL2                                                   
         MVC   ERRTEXT(5),=C'USER1'  PRESET ERROR TEXT                          
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
VR1320   MVC   EUSER1,USERDATA       SAVE ESTIMATE 1 INFO INTO RECORD           
         MVC   ESTUSR1,USERDATA      CLEAR OR RETRANSMIT EST 1 FIELD            
         OI    ESTUSR1H+6,X'80'                                                 
*                                                                               
         XC    USERDATA,USERDATA                                                
         OC    SVE2USER,SVE2USER     ANY "ESTIMATE 2" INFO?                     
         BZ    VR1330                                                           
*                                                                               
         LA    R2,ESTUSR2H           SET UP PARAMETERS FOR EDTUSR CALL          
         ST    R2,AUSR                                                          
         MVC   UTYPE,SVEU2TYP        WITH ESTIMATE 2 INFO - TYPE,LENGTH         
         MVC   LEN,SVEU2LEN          AND FIRST FLAG                             
         MVC   FLAG1,SVEU2FL1                                                   
         MVC   FLAG2,SVEU2FL2                                                   
         MVC   ERRTEXT(5),=C'USER2'  PRESET ERROR TEXT                          
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
VR1330   MVC   EUSER2,USERDATA       SAVE ESTIMATE 2 INFO INTO RECORD           
         MVC   ESTUSR2,USERDATA      CLEAR OR RETRANSMIT EST 2 FIELD            
         OI    ESTUSR2H+6,X'80'                                                 
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        VR1340                                                                 
***********************************************************************         
*                                                                               
VR1340   NTR1  BASE=*,LABEL=*                                                   
         CLC   QPRD,=C'POL'                                                     
         BNE   VR1507                                                           
VR1342   XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
VR1350   GOTO1 =A(NEXTPRD),RR=RELO                                              
         BNE   VR1507                                                           
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         CLI   POLSW,3                                                          
         BNE   VR1430                                                           
*                                                                               
         MVC   ERRNUM,=AL2(BRPOLER3) IF (ADDING A POL ESTIMATE) ...             
         LA    R2,ESTRBKH            POL MUST AGREE WITH ALL BRAND'S            
         CLC   EBOOK,SVBOOK          RATING BOOKS, HUT ADJUSTMENTS,             
         BNE   SPERREX               DAYPART MENUS AND CONTROLS                 
         MVC   ERRNUM,=AL2(BRPOLER5) ...AND TYPE OPTION                         
         LA    R2,ESTHUTH                                                       
         CLC   EHUTADJ,SVHUT                                                    
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(BRPOLER4)                                            
         LA    R2,ESTMENUH                                                      
         CLC   EDAYMENU,SVDPT                                                   
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(BRPOLER6)                                            
         LA    R2,ESTECONH                                                      
         CLC   ECONTROL,SVECON                                                  
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(BRPOLERA)                                            
         LA    R2,ESTOPTH                                                       
         CLC   ETYPE,SVETYPE                                                    
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(BRPOLER8) IF (ADDING A POL ESTIMATE) ...             
         LA    R2,ESTFLTRH           POL'S FILTERS MUST AGREE WITH              
         CLI   SVF0PROF,C'N'         ALL BRAND'S FILTERS IF (FIRST              
         BE    VR1410                BYTE OF SVF0PROF IS NOT N) AND             
         CLC   EPROF(3),SVFLTRS      (MEDIA IS NOT R OR FIRST BYTE OF           
         BE    VR1410                SVCLPROF IS ZERO)                          
         CLI   QMED,C'R'                                                        
         BNE   SPERREX                                                          
         CLI   SVCLPROF,C'0'                                                    
         BE    SPERREX                                                          
*                                                                               
VR1410   MVC   ERRNUM,=AL2(BRPOLER7) IF (ADDING A POL ESTIMATE) ...             
         LA    R2,ESTERTLH           POL'S RETAIL SCHEME MUST AGREE             
         CLI   SVF0PROF+1,C'N'       WITH ALL BRAND'S RETAIL SCHEMES            
         BE    VR1350                IF (SECOND BYTE OF SVF0PROF IS             
         CLC   ERTLSCHM,SVRTL        NOT N) AND (MEDIA IS R OR FIRST            
         BE    VR1350                BYTE OF SVCLPROF IS ZERO)                  
         CLI   QMED,C'R'                                                        
         BNE   SPERREX                                                          
         CLI   SVCLPROF,C'0'                                                    
         BE    SPERREX                                                          
         B     VR1350                                                           
*                                                                               
***********************************************************************         
*                                                                               
VR1430   CLC   EBOOK,SVBOOK          IF (CHANGING A POL ESTIMATE) ...           
         BNE   VR1480                MUST CHANGE RATING BOOKS, HUT ADJ-         
         CLC   EPWPCT,SVPOLPW        USTMENTS, DEPATMENT MENUS AND CON-         
         BNE   VR1480                TROLS OF ALL BRAND ESTIMATES TO            
         CLC   EHUTADJ,SVHUT         TO MATCH NEW POL VALUES                    
         BNE   VR1480                POL VALUES                                 
         CLC   EDAYMENU,SVDPT                                                   
         BNE   VR1480                                                           
         CLC   ETYPE,SVETYPE                                                    
         BNE   VR1480                                                           
         CLI   SVF0PROF,C'N'         FILTERS MUST BE CHANGED IF FIRST           
         BE    VR1440                BYTE OF SVF0PROF IS NOT N                  
         CLC   EPROF(3),SVFLTRS                                                 
         BNE   VR1480                                                           
VR1440   CLC   ECONTROL,SVECON                                                  
         BNE   VR1480                                                           
         CLI   SVF0PROF+1,C'N'       RETAIL SCHEMES MUST BE CHANGED IF          
         BE    VR1450                SECOND BYTE OF SVF0PROF IS NOT N           
         CLC   ERTLSCHM,SVRTL                                                   
         BNE   VR1480                                                           
*                                                                               
VR1450   LA    R4,4                  IF USER NAMES MATCH ...                    
         LA    RF,EUSRNMS            READ IN NEXT BRAND                         
         LA    RE,SVUSRNMS                                                      
VR1460   CLI   0(RF),C' '                                                       
         BNH   VR1470                                                           
         CLC   0(7,RF),0(RE)                                                    
         BNE   VR1480                                                           
VR1470   LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,VR1460                                                        
         B     VR1350                                                           
*                                                                               
VR1480   MVC   EHUTADJ,SVHUT         IF USER NAMES DON'T MATCH ... OR           
         OC    ECOST2,ECOST2       IF BRD HAS COS2 (POL HAS PW)                 
         BNZ   *+10                DON'T COPY POL PW TO COS2 BRD!               
         MVC   EPWPCT,SVPOLPW        POL AND BRAND DON'T AGREE...SAVE           
         MVC   EBOOK,SVBOOK          POL'S HUT ADJUSTMENT, RATING BOOK,         
         MVC   EDAYMENU,SVDPT        DAYPART MENU, CONTROLS (AND                
         CLI   SVF0PROF,C'N'         FILTERS AND RETAIL SCHEME) INTO            
         BE    *+10                  BRAND ESTIMATE ... THEN MAKE ALL           
         MVC   EPROF(3),SVFLTRS      USER NAMES MATCH AND PUT BRAND             
         MVC   ECONTROL,SVECON       ESTIMATE                                   
         MVC   ETYPE,SVETYPE                                                    
         CLI   SVF0PROF+1,C'N'                                                  
         BE    *+10                                                             
         MVC   ERTLSCHM,SVRTL                                                   
         LA    R4,4                                                             
         LA    RF,EUSRNMS                                                       
         LA    RE,SVUSRNMS                                                      
VR1490   CLI   0(RF),C' '                                                       
         BNH   VR1500                                                           
         MVC   0(7,RF),0(RE)                                                    
VR1500   LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,VR1490                                                        
*                                                                               
VR1502   GOTO1 =A(PTREC),RR=RELO                                                
*                                                                               
         L     R1,AIO                                                           
         MVC   CANKEY,0(R1)        SET KEY TO UPDATE                            
         GOTO1 =A(CANTV),RR=RELO   UPDATE MEDIA N AND C FOR CANADA              
         B     VR1350                                                           
VR1507   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        VR1520 - VALIDATE COPY CODE, SPECIAL REP, CPP EST           *          
*                 TYPE, REQ RANGE, AND RATE TYPE                     *          
**********************************************************************          
*                                                                               
VR1520   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,ESTCOPYH           COPY CODE NOT REQUIRED                     
         MVI   ECOPY,0                                                          
         CLI   5(R2),0                                                          
         BE    VR1540                                                           
*                                                                               
         LHI   R0,BADCPYCDQ      MUST BE ALPHABETIC                             
         TM    4(R2),X'04'                                                      
         JZ    SPERR0                                                           
         MVC   ECOPY,ESTCOPY                                                    
*                                                                               
**********************************************************************          
*                                                                               
VR1540   LA    R2,ESTREPH            SPECIAL REP NOT REQUIRED                   
         XC    EREP,EREP                                                        
         CLI   5(R2),0                                                          
         BE    VR1550                                                           
*                                                                               
         MVC   DMCB+4(3),=X'D9000A'                                             
         MVI   DMCB+7,QRCPACK                                                   
         GOTO1 CALLOV,DMCB           CALL RCPACK                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(C'P',8(R2)),EREP                                      
         BZ    VR541                                                            
         MVC   ERRNUM,=AL2(BADREP)   INVALID REP                                
         B     SPERREX                                                          
*                                                                               
VR541    XC    KEY,KEY               IF SPECIAL REP PRESENT ...                 
         MVI   KEY,C'R'              READ REP RECORD INTO AIO3                  
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(3),8(R2)                                                   
         MVC   KEY+5(2),AGENCY                                                  
         MVC   KEY+7(10),=10C'0'                                                
         MVC   AIO,AIO3                                                         
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         LHI   R0,BADSPREPQ                                                     
         CLC   KEY(7),0(R4)          MATCH?                                     
         JNE   SPERR0                                                           
VR1550   MVC   KEY,ESTKEY                                                       
*                                                                               
**********************************************************************          
*                                                                               
         LA    R2,ESTCPPEH           CPP ESTIMATE NOT REQUIRED                  
         XC    ECPPCLT(L'ECPPCLT+L'ECPPEST),ECPPCLT                             
         CLI   5(R2),0                                                          
         BE    VR1630                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
         TM    4(R2),X'08'           IF NUMERIC ... CANNOT EXCEED 255           
         BZ    VR1560                                                           
         GOTO1 =A(SPACK),RR=RELO                                                
         MVC   ERRNUM,=AL2(CPPERR2)                                             
         CLC   HALF,=H'255'                                                     
         BH    SPERREX                                                          
         LH    R0,HALF                                                          
         B     VR1600                                                           
*                                                                               
VR1560   MVC   ERRNUM,=AL2(CPPERR1)  IF POL ESTIMATE ... CPP ESTIMATE           
         CLC   QPRD,=C'POL'          MUST BE NUMERIC                            
         BE    SPERREX                                                          
*                                                                               
         LA    R5,8(R2)              IF NOT NUMERIC ... FIRST HALF MUST         
         MVC   WORK(3),SPACES        END WITH ',' OR '/' AND ...                
         LLC   R6,5(R2)              MUST BE <= 3 CHARACTERS IN LENGTH          
         LA    R1,WORK                                                          
         LA    R0,0                                                             
VR1570   CLI   0(R5),C','                                                       
         BE    VR1580                                                           
         CLI   0(R5),C'/'                                                       
         BE    VR1580                                                           
         MVC   0(1,R1),0(R5)                                                    
         LA    R1,1(R1)                                                         
         LA    R0,1(R0)                                                         
         LA    R5,1(R5)                                                         
         BCT   R6,VR1570                                                        
         B     ERRINV                                                           
VR1580   CHI   R0,3                                                             
         BH    ERRINV                                                           
*                                                                               
         MVC   DMCB+4(4),=X'D9000A14'                                           
         GOTO1 CALLOV,DMCB,0         CALL SUBROUTINE AND MOVE RESULT            
         CLI   4(R1),X'FF'           INTO ECPPLCLT                              
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),WORK,KEY+2                                             
         CLI   DMCB,0                                                           
         BNE   ERRINV                                                           
         MVC   ECPPCLT,KEY+2                                                    
*                                                                               
         LA    R5,1(R5)              SECOND HALF OF CPP EST MUST BE             
         XC    WORK,WORK             VALID NUMERIC                              
         BCTR  R6,0                                                             
         LTR   R6,R6                                                            
         BZ    ERRINV                                                           
         LA    RE,0                                                             
         LA    R1,WORK                                                          
VR1590   CLI   0(R5),C'0'                                                       
         BL    ERRINV                                                           
         CLI   0(R5),C'9'                                                       
         BH    ERRINV                                                           
         MVC   0(1,R1),0(R5)                                                    
         LA    R1,1(R1)                                                         
         LA    R5,1(R5)                                                         
         LA    RE,1(RE)                                                         
         BCT   R6,VR1590                                                        
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,WORK(0)                                                      
         CVB   R0,DUB                                                           
VR1600   STC   R0,ECPPEST                                                       
*                                                                               
         STC   R0,KEY+7            CPP EST                                      
         MVC   KEY+4(3),=C'POL'      READ IN ESTIMATE RECORD FOR POL            
         GOTO1 READ                  DATE AND RATE TYPE                         
         MVC   AIO,AIO2                                                         
         L     R3,AIO2                                                          
         GOTO1 GETREC                                                           
         MVC   POLDEMOS,EDEMOS                                                  
         MVC   POLDATE,ESTART                                                   
         MVC   POLTYPE,ECPPTYPE                                                 
*                                                                               
         MVC   ERRNUM,=AL2(PRIDERR)                                             
         L     R3,AIO1               RESTORE ESTHDRD TO AIO1                    
         CLC   EDEMOS(3),POLDEMOS    PRIMARY DEMO MUST MATCH                    
         BNE   SPERREX                                                          
*                                                                               
VR1610   CLI   POLTYPE,1             POL TYPE MUST BE 1-5                       
         BL    ERRINV                                                           
         CLI   POLTYPE,5                                                        
         BH    ERRINV                                                           
*                                                                               
         CLC   QPRD,=C'POL'          IF DOING POL EST ...                       
         BNE   VR1620                START DATE MUST MATCH END + 1              
         MVC   ERRNUM,=AL2(ENDOLAP)                                             
         GOTO1 ADDAY,DMCB,EEND,WORK,1                                           
         CLC   WORK(6),POLDATE                                                  
         BE    VR1630                                                           
         B     SPERREX                                                          
*                                                                               
VR1620   MVC   ERRNUM,=AL2(INVES)    IF NOT DOING POL ESTIMATE ...              
         CLC   ESTART,POLDATE        PERIODS MUST OVERLAP                       
         BL    SPERREX                                                          
         CLC   ESTART,POLDATE+6                                                 
         BH    SPERREX                                                          
*                                                                               
**********************************************************************          
*                                                                               
VR1630   LA    R2,ESTTYPEH           TYPE NOT REQUIRED                          
         CLI   ACTEQU,ACTADD                                                    
         BE    VR1640                                                           
*                                                                               
         MVC   ERRNUM,=AL2(ETYPERR)  IF TYPE PREVIOUSLY SAVED ... IT            
         CLI   ECPPTYPE,0            CANNOT BE BLANK                            
         BE    VR1640                                                           
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
         B     VR1650                                                           
*                                                                               
VR1640   MVI   ECPPTYPE,0                                                       
         CLI   5(R2),0                                                          
         BNE   VR1650                                                           
         CLC   QPRD,=C'POL'                                                     
         BNE   VR1700                                                           
         CLI   POLTYPE,0                                                        
         BE    VR1700                                                           
         B     SPERREX                                                          
*                                                                               
VR1650   MVC   ERRNUM,=AL2(TYPERR1)                                             
         CLI   OVSYS,3               FOR NETPAK, TYPE MUST BE 'CUT'             
         BNE   VR1660                                                           
         CLC   ESTTYPE,=C'CUT'                                                  
         BNE   SPERREX                                                          
         MVI   ECPPTYPE,C'C'                                                    
         B     VR1700                                                           
*                                                                               
VR1660   MVC   ERRNUM,=AL2(TYPERR2)  FOR NON-NETPAK, TYPE ONLY VALID            
         CLC   QPRD,=C'POL'          FOR POL PRODUCTS ... MUST BE 2             
         BNE   SPERREX               CHARACTERS LONG                            
         MVC   ERRNUM,=AL2(TYPERR3)                                             
         CLI   5(R2),2                                                          
         BNE   SPERREX                                                          
*        LA    R5,TYPTAB                                                        
         LAY   R5,TYPTAB             OTHERWISE ... IS TYPE IN TYPE              
VR1670   CLC   ESTTYPE(2),0(R5)                                                 
         BE    VR1680                                                           
         LA    R5,3(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   VR1670                                                           
         B     SPERREX               IF M$ STORE 03 INTO ECPPTYP,IF M%          
VR1680   MVC   ECPPTYPE,2(R5)        STORE 04, IF Q$ STORE 05                   
*                                                                               
         MVC   ERRNUM,=AL2(TYPERR4)  POL RECORD TYPE MUST MATCH                 
         CLI   POLTYPE,0             CPP EST TYPE                               
         BE    VR1690                                                           
         CLC   ECPPTYPE,POLTYPE                                                 
         BNE   SPERREX                                                          
*                                                                               
**********************************************************************          
*                                                                               
VR1690   MVC   ERRNUM,=AL2(STRINJAN) START DATE                                 
         LA    R2,ESTSTRDH                                                      
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(225),BLOCK+255                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO GTBRD                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   VR1695                AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    VR1695                                                           
         MVI   DBSELMED,C'C'                                                    
VR1695   MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,ESTART),WORK,GETDAY,ADDAY                           
         DROP  R4                                                               
*                                                                               
         CLC   WORK+8(2),=C'01'      START MONTH MUST BE JAN                    
         BNE   SPERREX                                                          
         CLC   ESTART,WORK           MUST MATCH FIRST BROADCAST DAY             
         BNE   SPERREX               OF JAN                                     
*                                                                               
**********************************************************************          
*                                                                               
         MVC   ERRNUM,=AL2(ENDINDEC) END DATE                                   
         LA    R2,ESTENDDH                                                      
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(225),BLOCK+255                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO GTBRD                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   VR1696                AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    VR1696                                                           
         MVI   DBSELMED,C'C'                                                    
VR1696   MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,EEND),WORK,GETDAY,ADDAY                             
         DROP  R4                                                               
*                                                                               
         CLC   EEND+2(2),=C'12'      END MONTH MUST BE DEC                      
         BNE   SPERREX                                                          
         CLC   EEND,WORK+6           MUST MATCH LAST BROADCAST DAY OF           
         BNE   SPERREX               DEC                                        
*                                                                               
**********************************************************************          
*                                                                               
VR1700   LA    R2,ESTRANH            REQ RANGE NOT REQUIRED                     
         XC    EREQLO(2),EREQLO                                                 
         CLI   5(R2),0                                                          
         BE    VR1760                                                           
*                                                                               
         CLI   5(R2),2               IF INPUT IS LENGTH 2, RANGE MUST           
         BNE   VR1710                BE 'NO'                                    
         CLC   8(2,R2),=C'NO'                                                   
         JNE   VR1750ER                                                         
         MVC   EREQLO(2),8(R2)                                                  
         B     VR1760                                                           
*                                                                               
VR1710   LA    R5,EREQLO             IF INPUT LENGTH NOT 2 ... FIRST            
         LA    R6,2                  HALF OF RANGE MUST BE VALID                
         LLC   R0,5(R2)              NUMERIC BETWEEN 0 AND 255                  
         LA    R2,8(R2)              (DELIMITERS ,-/)                           
VR1720   XR    R4,R4                                                            
         LR    R1,R2                                                            
VR1730   CLI   0(R2),C','                                                       
         BE    VR1740                                                           
         CLI   0(R2),C'-'                                                       
         BE    VR1740                                                           
         CLI   0(R2),C'/'                                                       
         BE    VR1740                                                           
         CLI   0(R2),C'0'                                                       
         JL    VR1750ER                                                         
         CLI   0(R2),C'9'                                                       
         JH    VR1750ER                                                         
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,VR1730                                                        
         B     VR1750                                                           
VR1740   BCTR  R0,0                                                             
VR1750   LTR   R4,R4                                                            
         JZ    VR1750ER                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,255                                                           
         JH    VR1750ER                                                         
         LTR   R1,R1                                                            
         JNP   VR1750ER                                                         
*                                                                               
         STC   R1,0(R5)              TEST SECOND HALF OF RANGE WITH             
         LA    R5,1(R5)              SAME STANDARDS                             
         LA    R2,1(R2)              R5 IS NOW POINTING TO EREQHI               
         BCT   R6,VR1720                                                        
         LTR   R0,R0                                                            
         JZ    VR1752                (SHOULD BE NOTHING LEFT)                   
*                                                                               
VR1750ER LHI   R0,BADRANGEQ                                                     
         J     SPERR0                                                           
*                                                                               
VR1752   LHI   R0,RANERR1            FIRST HALF OF RANGE MUST BE LESS           
         CLC   EREQLO,EREQHI         THAN SECOND HALF                           
         JNL   SPERR0                                                           
*                                                                               
         LA    R2,ESTRANH                                                       
         LHI   R0,RANERR2                                                       
         CLC   BEST,EREQLO           CURRENT ESTIMATE CODE MUST FIT IN          
         JL    SPERR0                INPUTTED RANGE                             
         CLC   BEST,EREQHI                                                      
         JH    SPERR0                                                           
*                                                                               
**********************************************************************          
*                                                                               
VR1760   LA    R2,ESTRTYPH           RATE TYPE NOT REQUIRED                     
         MVI   ERATE,0                                                          
         MVI   ERATECST,0                                                       
         CLI   5(R2),0                                                          
***      BE    VR1784                                                           
         BNE   VR1761                                                           
***                                                                             
* NO RATE INPUT                                                                 
* IF CANADIAN AGENCY AND ESTIMATE START DATE IS AFTER DEC29/19                  
* FORCE N-RATE ON THE ESTIMATE UNLESS CLIENT RATE IS A 1 OR 2                   
***                                                                             
         CLI   SVAPROF+7,C'C'        CANADIAN AGENCY?                           
         BNE   VR1784                NO                                         
         CLC   ESTART,=X'FBF9F1F2F2F9' ESTIMATE START AFTER DEC29/19?           
         BNH   VR1784                  NO - DO NOT FORCE N-RATE ON EST          
         CLI   SVCLPROF+14,C'1'      S-RATE ON THE CLIENT RECORD?               
         BE    VR1784                YES - DO NOT FORCE N-RATE ON EST           
         CLI   SVCLPROF+14,C'2'      F-RATE ON THE CLIENT RECORD?               
         BE    VR1784                YES - DO NOT FORCE N-RATE ON EST           
         MVI   ESTRTYP,C'3'          FORCE N-RATE ON THE ESTIMATE               
         B     VR1782                SET N-RATE ON ESTIMATE RECORD              
*                                                                               
VR1761   LHI   R0,RATER1             IF CLT DOES NOT ALLOW SPECIAL              
         CLI   SVCLPROF+14,C'*'      RATES DON'T ALLOW RATE ON EST              
         JE    SPERR0                                                           
*                                                                               
         CLI   OVSYS,3               FOR NETPAK, IF RATE TYPE INPUT IS          
         BNE   VR1780                2 CHARS LONG ... SECOND CHAR MUST          
         CLI   5(R2),2               BE EITHER A 'T','I' OR 'A'                 
         BNE   VR1780                                                           
         CLI   ESTRTYP+1,C'T'        TIME COVER                                 
         BE    VR1770                                                           
         CLI   ESTRTYP+1,C'I'        INT COVER                                  
         BE    VR1770                                                           
         CLI   ESTRTYP+1,C'A'        ALL COVER                                  
         BE    VR1770                                                           
         LHI   R0,BADRATYPQ                                                     
         J     SPERR0                                                           
VR1770   MVC   ERATECST,ESTRTYP+1                                               
         B     VR1782X                                                          
*                                                                               
VR1780   LHI   R0,BADRATYPQ                                                     
         CLI   5(R2),1                IF NOT NETPAK, RATE TYPE INPUT            
         JNE   SPERR0                 MUST BE 1 CHAR LONG                       
*                                                                               
VR1782   CLI   ESTRTYP,C'0'                                                     
         JL    SPERR0                                                           
         CLI   ESTRTYP,C'9'                                                     
         JH    SPERR0                                                           
*                                                                               
VR1782X  MVC   ERATE,ESTRTYP                                                    
*                                                                               
**********************************************************************          
*                                                                               
VR1784   LA    R2,ESTBTYPH           BOOK TYPE NOT REQUIRED                     
         MVI   EBKTYPE,X'00'                                                    
         CLI   5(R2),0                                                          
         BE    VR1788                                                           
         OC    ESTBTYP,SPACES                                                   
*                                                                               
         L     RF,ACOMFACS                                                      
         ICM   RF,15,CDEMTABS-COMFACSD(RF)                                      
         GOTO1 (RF),DMCB,SPBOOKTB  GET A(BOOK TABLE)                            
         ICM   RF,15,0(R1)         A(TABLE)RETURNED IN P1                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         L     R0,4(R1)            L'TABLE ENTRY RETURNED IN P2                 
         USING SPBKTYPD,RF                                                      
*                                                                               
         LHI   R0,BADBKTYP                                                      
VR1786   CLI   0(RF),X'FF'                                                      
         JE    SPERR0                                                           
         CLC   ESTBTYP,SPBKTYPA                                                 
         BE    *+12                                                             
         AHI   RF,SPBKTYLQ                                                      
         B     VR1786                                                           
         MVC   EBKTYPE,SPBKTYPN                                                 
         DROP  RF                                                               
*                                                                               
VR1788   XC    ECSBKTYP,ECSBKTYP                                                
         MVI   ECSSDTE,0                                                        
*                                                                               
         LHI   R0,BADCSBKTY                                                     
         LA    R2,ESTCBTYH         COMSCORE BOOK TYPE                           
         CLI   5(R2),0                                                          
         JNE   VR1790                                                           
         OC    ENONTDMS,ENONTDMS   IF NON-TRAD DEMOS EXIST                      
         JZ    VR1792                                                           
         TM    4(R2),X'80'                                                      
         JO    SPERR0                                                           
         MVC   8(2,R2),=C'L '        DEFAULT TO LIVE                            
*                                                                               
VR1790   OC    ESTCBTY,SPACES                                                   
         CLC   8(2,R2),=C'L '        LIVE?                                      
         JNE   SPERR0                                                           
         MVC   ECSBKTYP,8(R2)                                                   
*                                                                               
VR1792   LHI   R0,BADCSDTS                                                      
         LA    R2,ESTCSDTH         COMSCORE SURVEY DATES                        
         CLI   5(R2),0             REQUIRED                                     
         JNE   VR1794                                                           
         OC    ENONTDMS,ENONTDMS   IF NON-TRAD DEMOS EXIST                      
         JZ    VR1798                                                           
         TM    4(R2),X'80'                                                      
         JO    SPERR0                                                           
         MVI   8(R2),ECSSDTSQ      DEFAULT TO SWEEP                             
*                                                                               
VR1794   CLI   8(R2),ECSSDTBQ      BROADCAST?                                   
         JE    *+12                                                             
         CLI   8(R2),ECSSDTSQ      SWEEP?                                       
         JNE   SPERR0                                                           
         MVC   ECSSDTE,8(R2)                                                    
*                                                                               
VR1798   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
********************************************************************            
*        VR1800 - ADD OR MODIFY EST AND UPDATE CASH                *            
********************************************************************            
VR1800   NTR1  BASE=*,LABEL=*                                                   
         CLI   WORK,C'S'                                                        
         BE    VR1820                                                           
         MVC   KEY,ESTKEY                                                       
         CLI   ACTEQU,ACTADD                                                    
         BNE   VR1820                                                           
         CLI   EDAILY,C'Y'                                                      
         BNE   VR1810                IF ADDING A DAILY ESTIMATE, MAX-           
         MVC   ERRNUM,=AL2(DLYERR)   IMUM 53 DAYS BETWEEN START AND             
         L     RF,ACOMFACS           END DATE                                   
         USING COMFACSD,RF                                                      
         GOTO1 PERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                      
         LH    R4,8(R1)                                                         
         CHI   R4,53                                                            
         BH    SPERREX                                                          
         DROP  RF                                                               
*                                                                               
VR1810   MVC   KEY(13),ESTKEY        ADD THE ESTIMATE AND UPDATE CASH           
         OI    ECNTRL,X'01'          PRODUCT CODE                               
         MVC   EPRDCD+1(1),BPRD                                                 
         MVC   AIO,AIO1                                                         
*                                  ZAP NEW PACKED ACCUMS                        
         ZAP   ECURPDN,=PL6'0'                                                  
         LA    R1,26                                                            
         LA    R2,EORD                                                          
         ZAP   0(6,R2),=PL6'0'                                                  
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
         LA    R1,13                                                            
         LA    R2,EAUTH                                                         
         ZAP   0(6,R2),=PL6'0'                                                  
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
         LA    R1,26                                                            
         LA    R2,EPAID                                                         
         ZAP   0(6,R2),=PL6'0'                                                  
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         GOTO1 =A(ADREC),RR=RELO                                                
         MVC   ESTKEY,KEY                                                       
*                                                                               
         MVC   CANKEY,KEY            SET KEY TO UPDATE                          
         GOTO1 =A(CANTV),RR=RELO     GO UPDATE N AND C RECS                     
         B     VR1850                                                           
*                                                                               
********************************************************************            
*                                                                               
VR1820   MVC   KEY(13),ESTKEY        REREAD ESTIMATE BEFORE PUTREC              
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1            NEW RECORD NOW IN AIO1                       
         L     R3,AIO                                                           
*                                                                               
         MVI   NEWLEN,C'N'                                                      
         L     RE,AIO3                                                          
         CLC   ELEN-ESTHDR(2,RE),ELEN   ESTHDR SAME LEN                         
         JE    *+8                                                              
         MVI   NEWLEN,C'Y'                                                      
*                                                                               
         MVC   ERRNUM,=AL2(DLYERR)   IF ESTIMATE RECORD IS DAILY ...            
         CLI   EDAILY,C' '           MAXIMUM 53 DAYS BBETWEEN START             
         BNH   VR1830                AND END DATES                              
         CLI   EDAILY,C'N'                                                      
         BE    VR1830                                                           
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 PERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                      
         LH    R4,8(R1)                                                         
         CHI   R4,53                                                            
         BH    SPERREX                                                          
*                                                                               
VR1830   BRAS  RE,PTREC                  PUT RECORD FROM AIO1!                  
*                                                                               
*================================================================               
* IF POL NONT DEMO -->NAMES<--  CHANGED, COPY TO BRAND ESTIMATES                
* IF THEY HAVE ANY NONT DEMOS                                                   
*================================================================               
                                                                                
         L     RE,AIO1               SAVE NEW RECORD IN AIO3                    
         SR    RF,RF                                                            
         ICM   RF,3,ELEN-ESTHDR(RE)  GET ESTHDR LEN                             
         L     R0,AIO3                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   POLSW,3             TEST ADDING POL EST                          
         JE    VR1840                                                           
         CLC   QPRD,=C'POL'                                                     
         JNE   VR1840                                                           
         CLC   ELEN,=AL2(ESTHDRLN)  DOES EST HAVE NONT DEMO NAMES               
         JNH   VR1840                                                           
         CLC   POLNTRDM,ENONTDMS    COMPARE OLD/NEW NONT DEMO NAMES             
         JE    VR1840               SAME                                        
         MVC   POLNTRDM,ENONTDMS    MOVE NEW LIST TO SAVE AREA STUPIDO!         
*                                                                               
         XC    KEY,KEY             LOOP THROUGH ALL BRANDS                      
         MVC   KEY(4),ESTKEY                                                    
*                                                                               
VR1832   BRAS  RE,NEXTPRD                                                       
         BNE   VR1834                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO               AT PRESENT, THIS IS AIO1                    
         USING ESTHDR,R3 <===  CODE ALWAYS USED R3, JUST CLARIFYING             
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)  DOES EST HAVE NONT DEMO NAMES               
         JNH   VR1832                                                           
         CLC   POLNTRDM,ENONTDMS    DO LISTS MATCH                              
         JE    VR1832                                                           
         MVC   ENONTDMS(160),POLNTRDM                                           
         BRAS  RE,PTREC              UPDATE THE RECORD                          
         J     VR1832                                                           
*                                                                               
VR1834   L     RE,AIO3               RESTORE NEW POL RECORD                     
         SR    RF,RF                                                            
         ICM   RF,3,ELEN-ESTHDR(RE)  GET ESTHDR LEN                             
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   KEY,SVKEY            RESTORE POL ESTHDR KEY                      
         GOTO1 READ                                                             
*                                                                               
VR1840   L     R1,AIO                                                           
         MVC   CANKEY,0(R1)          SET KEY TO UPDATE                          
         BRAS  RE,CANTV              UPDATE MEDIA N AND C                       
*                                                                               
********************************************************************            
*                                                                               
         CLI   WORK,C'S'             IF STATUS WAS CHANGED...                   
         BNE   VR1850                READ EST REC...WRITE CNTRL                 
         GOTO1 READ                  BYTE TO KEY                                
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         CLI   SVAPROF+7,C'C'                                                   
         BNE   VR1875                                                           
         CLI   QMED,C'T'                                                        
         BNE   VR1875                                                           
*                                                                               
         CLI   SVCXTRA+8,C'P'        P&G CLIENT?                                
         BE    VR1842                YES - NO MEDIA N!                          
         MVC   KEY,ESTKEY            IF CANADIAN TV WRITE CNTRL BYTE            
         NI    KEY+1,X'F0'           TO KEY IN X'03' AND X'08' RECS             
         OI    KEY+1,X'03'                                                      
         GOTO1 READ                                                             
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
*                                                                               
VR1842   MVC   KEY,ESTKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 READ                                                             
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         B     VR1875                                                           
*                                                                               
********************************************************************            
*                                                                               
VR1850   OC    CASHPRD,CASHPRD                                                  
         BZ    VR1860                                                           
*                                                                               
         XC    KEY,KEY               CASH PRODUCT EXISTS ...                    
         MVC   KEY(8),ESTKEY                                                    
         MVC   KEY+4(3),CASHPRD                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2              READ CASH PRD ESTIMATE INTO AIO2           
         L     R3,AIO                UPDATE TRADE PRODUCT CODE AND PUT          
         GOTO1 GETREC                                                           
         MVC   ETRDPRD-ESTHDR(1,R3),BPRD                                        
         GOTO1 =A(PTREC),RR=RELO                                                
*                                                                               
********************************************************************            
*                                                                               
VR1860   OC    OLDCASH,OLDCASH                                                  
         BZ    VR1865                                                           
         XC    KEY,KEY               ESTIMATES FOR OLD CASH PRODUCTS            
         MVC   KEY(8),ESTKEY         MUST BE DELETED                            
         MVC   KEY+4(3),OLDCASH                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2              READ OLD CASH PRD EST INTO AIO2            
         L     R3,AIO                REMOVE TRADE PRODUCT AND PUT               
         GOTO1 GETREC                                                           
         MVI   ETRDPRD-ESTHDR(R7),0                                             
         GOTO1 =A(PTREC),RR=RELO                                                
*                                                                               
********************************************************************            
*                                                                               
VR1865   CLI   SETCONV,C'Y'          SET CLT COST2 CONVERSION DATE              
         BNE   VR1870                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY         CLIENT RECORD                              
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2              READ OLD CASH PRD EST INTO AIO2            
         L     R2,AIO                                                           
         USING CLTHDR,R2                                                        
         GOTO1 GETREC                                                           
         OC    CC2CONV,CC2CONV     ALREADY MARKED CONVERTED                     
         BNZ   VR1870              THEN SKIP                                    
         GOTO1 DATCON,DMCB,(5,0),(2,CC2CONV)                                    
         OI    COPT3,COP3CONV      SET DATE NOT SET BY SPCW                     
         GOTO1 =A(PTREC),RR=RELO                                                
         DROP  R2                                                               
*                                                                               
********************************************************************            
*                                                                               
VR1870   L     R3,AIO1                                                          
         MVC   AIO,AIO3              BUILD REQUEST RECORD AT AIO3               
         L     R1,AIO                                                           
         XC    0(150,R1),0(R1)                                                  
         MVI   10(R1),132                                                       
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'L2'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   5(3,R1),QCLT                                                     
         MVC   11(3,R1),QPRD                                                    
         LLC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVI   61(R1),C'N'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   65(R1),C'A'                                                      
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    *+8                                                              
         MVI   65(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVI   WORK,0                                                           
*                                                                               
         CLI   ACTEQU,ACTADD                                                    
         BE    VR1875              DONT UPDATE BUYLINE ON ADD                   
*                                                                               
         CLC   ECOST2,SVECOST2     ECOST2 CHANGED?                              
         BE    VR1875                                                           
*                                                                               
         XC    WORK,WORK             CHECK PROFILE - C3 REQUEST NEEDED?         
         MVC   WORK+16(4),=C'S0B0'   B0 PROFILE                                 
         MVC   WORK+20(2),AGENCY     PROFILE NAME                               
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK+1,C'Y'                                                      
         BNE   VR1875                                                           
*                                                                               
         MVC   AIO,AIO3              GENERATE C3 RECORD AT AIO3                 
         L     R1,AIO                                                           
         XC    0(150,R1),0(R1)                                                  
         MVI   10(R1),132          C3 REQ                                       
         MVI   14(R1),106                                                       
         LA    R1,26(R1)                                                        
         MVI   0(R1),X'40'                                                      
         MVC   1(79,R1),0(R1)                                                   
         MVC   0(2,R1),=C'C3'                                                   
         MVC   2(2,R1),14(RA)                                                   
         MVC   4(1,R1),QMED                                                     
         MVC   5(3,R1),QCLT                                                     
         MVC   11(3,R1),QPRD                                                    
         MVC   49(4,R1),SVECOST2                                                
         LLC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVC   68(7,R1),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVI   WORK,0              SO I'LL REFORMAT REC                         
*                                                                               
VR1875   MVC   AIO,AIO1                                                         
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VALPROR - VALIDATE PURCHASE ORDER                            *         
***********************************************************************         
VALPROR  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,EPONUM                                                        
         LA    R1,L'EPONUM                                                      
VPROR10  CLI   0(R3),X'40'    SPACES,                                           
         BE    VPROR20                                                          
         CLI   0(R3),C'A'     ALPHA,                                            
         JL    NEQXIT                                                           
         CLI   0(R3),C'Z'                                                       
         BNH   VPROR20                                                          
         CLI   0(R3),C'0'     AND NUMERIC ARE OK                                
         JL    NEQXIT                                                           
         CLI   0(R3),C'9'                                                       
         JH    NEQXIT                                                           
VPROR20  AHI   R3,1                                                             
         BCT   R1,VPROR10                                                       
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHKSTAT - VALIDATE STATUS FIELD                              *         
***********************************************************************         
*                                                                               
CHKSTAT  NTR1 BASE=*,LABEL=*                                                    
         MVI   WORK,0                FIRST BYTE OF WORK WILL BE SET             
         CLI   5(R2),0               TO 'S' IF STATUS OF ALREADY                
         BNE   CHKS05                EXISTING ESTIMATE IS CHANGED               
         CLI   ACTEQU,ACTADD                                                    
         BNE   CHKS01                                                           
         CLI   OVSYS,3               STATUS ONLY REQUIRED FOR NETPAK            
         BNE   CHKXIT                                                           
         MVC   ERRNUM,=AL2(STATERRC)                                            
         BE    SPERREX               ADDS                                       
*                                                                               
***********************************************************************         
*                                                                               
CHKS01   OC    SVELOCK,SVELOCK       IF LOCK DATES HAVE BEEN ERASED             
         BZ    CHKXIT                SET 'STATUS CHANGED' BYTE AND              
         XC    ELOCKYM,ELOCKYM       EXIT                                       
         B     CHKS60                                                           
*                                                                               
***********************************************************************         
*                                                                               
CHKS05   LHI   R0,STATERR1                                                      
         CLC   ESTSTAT(4),=C'LOCK'   LOCK IS INVALID STATUS FOR ADDS            
         BNE   CHKS10                AND PREVIOUSLY HELD ESTIMATES              
         CLI   ACTEQU,ACTADD                                                    
         JE    SPERR0                                                           
         OI    ECNTRL,X'08'                                                     
         OI    KEY+13,X'08'                                                     
*                                                                               
         LHI   R0,STATERR2                                                      
         TM    SVCNTRL,X'04'         LOCK HAS BEEN VALIDATED ...                
         JNZ   SPERR0                TURN ON X'08' BIT                          
         TM    SVCNTRL,X'08'         IF ESTIMATE WASN'T PREVIOUSLY              
         BO    CHKXIT                LOCKED SET STATUS CHANGED BYTE             
         B     CHKS60                AND EXIT                                   
*                                                                               
***********************************************************************         
*                                                                               
CHKS10   LHI   R0,STATERR3                                                      
         CLC   ESTSTAT(4),=C'HOLD'   HOLD IS INVALID STATUS FOR ADDS            
         BNE   CHKS20                                                           
         CLI   ACTEQU,ACTADD                                                    
         JE    SPERR0                                                           
         OI    ECNTRL,X'0C'          HOLD HAS BEEN VALIDATED ...                
         OI    KEY+13,X'0C'          TUNR ON '0C' BIT                           
         TM    SVCNTRL,X'0C'         IF ESTIMATE WASN'T PREVIOUSLY              
         BO    CHKXIT                HELD SET STATUS CHANGED BYTE               
         B     CHKS60                AND EXIT                                   
*                                                                               
***********************************************************************         
*                                                                               
CHKS20   LHI   R0,STATERR4                                                      
         CLC   ESTSTAT(6),=C'UNLOCK'                                            
         BNE   CHKS30                UNLOCK IS INVALID STATUS FOR ADDS,         
         CLI   ACTEQU,ACTADD         HELD ESTIMATES AND ESTIMATES WHICH         
         JE    SPERR0                WEREN'T PREVIOUSLY LOCKED                  
         NI    ECNTRL,X'F7'                                                     
         NI    KEY+13,X'F7'                                                     
*                                                                               
         LHI   R0,STATERR5                                                      
         TM    SVCNTRL,X'04'                                                    
         JNZ   SPERR0                                                           
         LHI   R0,STATERR6                                                      
         TM    SVCNTRL,X'08'         UNLOCK HAS BEEN VALIDATED ...              
         JZ    SPERR0                TURN OFF X'F7' BIT, SET STATUS             
         B     CHKS60                CHANGED BYTE AND EXIT                      
*                                                                               
***********************************************************************         
*                                                                               
CHKS30   LHI   R0,STATERR7                                                      
         CLC   AGENCY,=C'JW'         FOR AGENCY JWT, DO NOT ALLOW               
         BNE   CHKS40                'REL' ONLY 'DBT'                           
         LHI   R0,STATERR8                                                      
         CLC   ESTSTAT(3),=C'DBT'                                               
         BE    CHKS50                                                           
         B     CHKS55                                                           
CHKS40   CLC   ESTSTAT(3),=C'REL'    REL (OR DBT) ARE INVALID FOR ADDS          
         BNE   CHKS55                                                           
CHKS50   CLI   ACTEQU,ACTADD                                                    
         JE    SPERR0                                                           
*                                                                               
         LHI   R0,STATERR9           REL (OR DBT) MUST BE FOLLOWED BY           
         CLI   5(R2),5               THE 2 DIGITS OF THE CURRENT DAY            
         JNE   SPERR0                                                           
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         CLC   WORK+4(2),11(R2)                                                 
         JNE   SPERR0                                                           
         NI    ECNTRL,X'F3'        RELEASE (OR DBT) HAS BEEN VALIDATED          
         NI    KEY+13,X'F3'        TURN OFF X'F3' BYTE, SET STATUS              
         B     CHKS60              CHANGED BYTE AND EXIT                        
*                                                                               
***********************************************************************         
*                                                                               
CHKS55   MVI   WORK,0                                                           
         LA    R1,WORK+1                                                        
         LR    R4,R1                                                            
         MVC   WORK+1(L'ESTSTAT),ESTSTAT                                        
         LA    R3,L'ESTSTAT                                                     
CHKS55A  CLI   0(R1),C'-'            IF STATUS IS A LOCK DATE ...               
         BNE   *+12                  CHECK IF FORMAT IS M/Y, M/Y-, OR           
         MVI   WORK,X'80'            M/Y+ ... FIRST BYTE X'80' INDICATE         
         B     CHKS55C               PRIOR DATE ... FIRST BYTE X'40'            
         CLI   0(R1),C'+'            INDICATES SUBSEQUENT DATE                  
         BNE   *+12                                                             
         MVI   WORK,X'40'                                                       
         B     CHKS55C                                                          
         CLI   0(R1),C' '                                                       
         BE    CHKS55F                                                          
         CLI   0(R1),X'0'                                                       
         BE    CHKS55F                                                          
         CLI   0(R1),C'/'                                                       
         BE    CHKS55B                                                          
         CLI   0(R1),C'A'                                                       
         BL    CHKS80                                                           
         CLI   0(R1),C'9'                                                       
         BH    CHKS80                                                           
CHKS55B  LA    R1,1(R1)                                                         
         BCT   R3,CHKS55A            IF + OR - FOUND ... REPLACE WITH           
         B     CHKS55F               BLANK FOR DATVAL                           
CHKS55C  MVI   0(R1),C' '                                                       
CHKS55F  GOTO1 DATVAL,DMCB,(2,WORK+1),WORK+20                                   
         OC    DMCB(4),DMCB                                                     
         BZ    CHKS80                                                           
*                                                                               
         LHI   R0,STATERRA                                                      
         TM    SVCNTRL,X'04'         CAN'T USE DATE IF ESTIMATE PREV-           
         JO    SPERR0                IOUSLY HELD                                
*                                                                               
         LHI   R0,STATERRB                                                      
         TM    SVCNTRL,X'08'         CAN'T USE DATE IF ESTIMATE PREV-           
         JNZ   SPERR0                IOUSLY LOCKED                              
*                                                                               
         L     R3,AIO1                                                          
         LHI   R0,ESTERR             MONTH MUST BE IN EST PERIOD                
         CLC   WORK+20(4),SVSTRT                                                
         JL    SPERR0                                                           
         CLC   WORK+20(4),SVEND                                                 
         JH    SPERR0                                                           
         GOTO1 DATCON,DMCB,(0,WORK+20),(3,WORK+30)                              
         OC    WORK+31(1),WORK                                                  
         MVC   ELOCKYM,WORK+30                                                  
         OC    ELOCKMON,WORK                                                    
*                                                                               
         CLC   ELOCKYM,SVELOCK       IF LOCK DATE HAS CHANGED, SET              
         BE    CHKXIT                STATUS CHANGED BYTE AND EXIT               
*                                                                               
***********************************************************************         
*                                                                               
CHKS60   MVI   WORK,C'S'             SET STATUS CHANGED BYTE AND EXIT           
         B     CHKXIT                                                           
*                                                                               
***********************************************************************         
*                                                                               
CHKS80   L     R3,AIO1                                                          
         GOTO1 =A(CKLINID),RR=RELO   OTHERWISE ...                              
         BE    CHKS90                (ACTION MUST BE ADD) OR (TERMINAL          
         CLI   ACTEQU,ACTADD         MUST BE VALID FOR CHANGE) ... AND          
         BNE   ERRINV                (STATUS MUST BE 'OLD') OR (STATUS          
CHKS90   NI    EPRDCD,X'7F'          MUST BE 'NEW' AND MEDIA MUST BE            
         CLC   ESTSTAT(3),=C'OLD'    'NETWORK') ... DON'T SET STATUS            
         BE    CHKXIT                CHANGED BYTE                               
         CLC   ESTSTAT(3),=C'NEW'                                               
         BNE   ERRINV                                                           
         CLI   QMED,C'N'                                                        
         BNE   ERRINV                                                           
         OI    EPRDCD,X'80'                                                     
*                                                                               
CHKXIT   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*        CHECK MASTER TERMINAL/LINE ID                          *               
*****************************************************************               
CKLINID  NTR1 BASE=*,LABEL=*                                                    
         L     R1,=A(LINIDTAB)                                                  
         A     R1,RELO                                                          
*                                                                               
CKL10    CLI   0(R1),X'FF'                                                      
         BE    CKLNO                                                            
         CLC   LINID(8),0(R1)                                                   
         BE    CKLYES                                                           
         LA    R1,8(R1)                                                         
         B     CKL10                                                            
CKLYES   SR    R1,R1                                                            
CKLNO    LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
* CHKEDTS- FOR NETWORK CAN'T HAVE MORE THAN 12 CALENDAR MONTHS      *           
*          FOR SPOT CAN'T HAVE MORE THAN 12 BROADCAST MONTHS        *           
*          OR 53 WEEKS (WE THINK)                                   *           
*********************************************************************           
*                                                                               
CHKEDTS  NTR1 BASE=*,LABEL=*                                                    
         MVC   WORK+6(6),SYR                                                    
         CLI   OVSYS,2                                                          
         BNE   CHKEDT02                                                         
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(225),BLOCK+255                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO GTBRD                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   CHKEDT01              AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    CHKEDT01                                                         
         MVI   DBSELMED,C'C'                                                    
CHKEDT01 MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,SYR),WORK,GETDAY,ADDAY                              
         DROP  R4                                                               
*                                                                               
CHKEDT02 GOTO1 DATCON,DMCB,WORK+6,(3,DUB)                                       
         MVC   WORK+6(6),EYR                                                    
         CLI   OVSYS,2               SPOT?                                      
         BNE   CHKEDT06                                                         
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(225),BLOCK+255                                         
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO GTBRD                       
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   CHKEDT05              AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    CHKEDT05                                                         
         MVI   DBSELMED,C'C'                                                    
CHKEDT05 MVC   DMCB+4(4),=X'D9000A1D'                                           
         GOTO1 CALLOV,DMCB           CALL GETBRD                                
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(1,EYR),WORK,GETDAY,ADDAY                              
         DROP  R4                                                               
*                                                                               
CHKEDT06 GOTO1 DATCON,DMCB,WORK+6,(3,DUB+3)                                     
         LLC   R4,DUB                                                           
         LLC   R5,DUB+3                                                         
         SR    R5,R4                                                            
         BZ    CHKEDTY               SAME YEAR IS ALWAYS OK                     
         CHI   R5,1                                                             
         BH    CHKEDTN               CAN'T EXCEED ONE YEAR (SO INVALID)         
         CLC   DUB+1(2),DUB+4        NELSON SAYS TO ALLOW THIS                  
         BNH   CHKEDTN               SO LONG AS NOT MORE THAN 365 DAYS          
         CLI   OVSYS,2               SPOT?                                      
         BNE   CHKEDTY               DONE FOR NET                               
         CLC   DUB+1(1),DUB+4        INVALID IF SAME BRD MONTH(DIF YRS)         
         BE    CHKEDTN               CAN'T HAVE 13 BRD MONTHS                   
         B     CHKEDTY                                                          
*                                                                               
CHKEDTN  LA    R2,ESTSTRDH           CURSOR TO START DATE                       
         LHI   R0,DSPERR             (INVALID DATE SPREAD)                      
         CLC   =C'SJ',AGENCY         SJ HAS NO TALENT RECORDS                   
         JE    SPERR0                                                           
         GOTO1 =A(CHKTAL),RR=RELO                                               
         JNE   SPERR0                                                           
*                                                                               
CHKEDTY  LHI   R0,DSPERR             (INVALID DATE SPREAD)                      
         LA    R2,ESTSTRDH           CURSOR TO START DATE                       
         CR    R3,R3                                                            
         JNZ   SPERR0                                                           
CHKEDTX  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*************************************************************                   
* NEXTPRD                                                                       
* SUBROUTINE READS ALL BRAND ESTIMATES FOR THIS CLIENT      *                   
* AND PRODUCT SKIPPING ALL POL ESTIMATES                    *                   
* EXIT WITH CC NEQ WHEN NO MORE PRODUCTS                    *                   
*************************************************************                   
*                                                                               
NEXTPRD  NTR1 BASE=*,LABEL=*                                                    
NEXTPRD2 MVC   KEY+7(2),=X'FFFF'        READ NEXT PRDHDR                        
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY           SAME CLIENT                             
         BNE   PRDNEQX                                                          
         CLC   KEY+4(3),=C'POL'                                                 
         BE    NEXTPRD2                                                         
         XC    KEY+7(13),KEY+7                                                  
         MVC   KEY+7(1),ESTKEY+7                                                
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(4),KEY           SAME CLIENT                             
         BNE   PRDNEQX                                                          
         CLC   KEYSAVE(9),KEY           SAME PRD/EST                            
         BE    PRDEQX                                                           
         MVC   KEY,KEYSAVE              NO - RESTORE TO LAST PRD                
         B     NEXTPRD2                                                         
PRDEQX   CR    RE,RE                                                            
         B     *+6                                                              
PRDNEQX  LTR   RE,RE                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SPACK                                                        *         
***********************************************************************         
SPACK    NTR1 BASE=*,LABEL=*                                                    
         SR   R0,R0                                                             
         ZAP  DUB,=P'0'                                                         
         LLC R1,5(R2)                                                           
         LTR  R1,R1                                                             
         BZ   SPACKX                                                            
         TM   4(R2),X'08'                                                       
         BZ   SPACKX                                                            
         BCTR R1,0                                                              
         EX   R1,*+12                                                           
         CVB  R0,DUB                                                            
         B    SPACKX                                                            
         PACK DUB,8(0,R2)                                                       
SPACKX   STH  R0,HALF                                                           
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                       NEXTEL                                       *          
**********************************************************************          
NEXTEL   LLC   R0,1(R5)                                                         
         AR    R5,R0                                                            
         CLI   0(R5),0             END OF REC SET CC NOT EQ                     
         JE    NEXTELX                                                          
         CLC   ELCODE,0(R5)                                                     
         JE    NEXTELX2                                                         
         J     NEXTEL                                                           
*                                                                               
NEXTELX  LTR   R5,R5                                                            
NEXTELX2 BR    RE                  RETURN WITH CC NOT EQ                        
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*                          PARTOSCAN                                  *         
* REFORMAT PARSNIP BLK AT AIO3+500 TO LOOK ALMOST LIKE SCANNER OUTPUT *         
* AT AIO3 - EXCEPT FLD1 IS LENGTH 11 FOR DEMOS LIKE ###.RADRWM2554    *         
***********************************************************************         
PARTOSCN NTR1 BASE=*,LABEL=*                                                    
         L     R2,AIO3                                                          
         AHI   R2,500              500(AIO3)                                    
         USING PSND,R2             PARSNIP BLK                                  
         L     R5,AIO3                                                          
         USING BSCAND,R5           NEW BIG SCANNER-ISH BLK                      
*                                                                               
PAR10    XC    0(BSCANLNQ,R5),0(R5)                                             
         MVC   BFLD1,SPACES                                                     
         MVC   BFLD2,SPACES                                                     
         LTR   R2,R2               ANY MORE?                                    
         BZ    PARX                                                             
PAR12    CLI   PSNTAG,C'F'         FIELD IS LEFT OF '=' IS FLD1                 
         BE    PAR20                                                            
         CLI   PSNTAG,C'V'         VALUE IS RIGHT OF '=' IS FLD2                
         BE    PAR30                                                            
         B     PARX                                                             
*                                                                               
PAR20    MVC   BFLD1LEN,PSNLEN     MOVE COMPONENT                               
         MVC   BFLD1VAL,PSNSTAT                                                 
         MVC   BFLD1B,PSNNUM                                                    
         L     R4,PSNCOMP                                                       
         LLC   R1,PSNLEN                                                        
         CHI   R1,11               CAN'T BE >11                                 
         BNH   *+8                                                              
         LA    R1,11                                                            
         AHI   R1,-1               SUBTRACT 1                                   
         BM    PAR22                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BFLD1(0),0(R4)                                                   
PAR22    L     R6,PSNFLD           A(NEXT FIELD)                                
         L     R2,PSNVAL           A(NEXT VALUE)                                
         B     PAR12                                                            
*                                                                               
PAR30    MVC   BFLD2LEN,PSNLEN     MOVE COMPONENT                               
         MVC   BFLD2VAL,PSNSTAT                                                 
         MVC   BFLD2B,PSNNUM                                                    
         L     R4,PSNCOMP                                                       
         LLC   R1,PSNLEN                                                        
         CHI   R1,10               CAN'T BE >10                                 
         BNH   *+8                                                              
         LA    R1,10                                                            
         AHI   R1,-1               SUBTRACT 1                                   
         BM    PAR32                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BFLD2(0),0(R4)                                                   
PAR32    LR    R2,R6               NEXT FIELD                                   
         LA    R5,BSCANLNQ(R5)                                                  
         B     PAR10                                                            
*                                                                               
PARX     J     XIT                                                              
         LTORG                                                                  
         DROP  R5,R2                                                            
         EJECT                                                                  
*********************************************************************           
*                CKPOLWTS                                           *           
*********************************************************************           
CKPOLWTS NTR1 BASE=*,LABEL=*                                                    
         LA    R3,DMAX                                                          
         LA    R5,POLDEMOS           R2 ---> BRAND DEMOS                        
         LA    R6,PEWGTLST           R4 ---> BRAND WEIGHTS                      
CKPOL1   CLC   0(3,R5),0(R2)         R5 ---> POL DEMOS                          
         BE    CKPOL5                R6 ---> POL WEIGHTS                        
         LA    R5,3(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R3,CKPOL1                                                        
         B     WGHTSPR2                                                         
CKPOL5   CLC   0(1,R6),0(R4)         WHEN BRAND DEMO FOUND ...                  
         BNE   WGHTSPR2              BRAND WEIGHT MUST MATCH POL WEIGHT         
CKPOLX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        EDTUSR                                                       *         
***********************************************************************         
EDTUSR   NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R3,AUSR               ANY INPUT IN USER FIELD?                   
         CLI   5(R3),0                                                          
         BNE   EDTUSR10                                                         
*                                                                               
         TM    FLAG1,CFLGREQQ        NO INPUT ... WAS IT REQUIRED?              
         BZ    EDTXIT                IF NOT, EXIT                               
         LHI   R0,BADUSR1Q                                                      
         J     EDTUSRER              IF YES, ERROR                              
*                                                                               
EDTUSR10 LHI   R0,BADUSR2Q                                                      
         CLC   LEN,5(R3)             CHECK LENGTH OF INPUT                      
         JL    EDTUSRER                                                         
*                                                                               
         CLI   UTYPE,C' '            IS TYPE SUPPOSED TO BE 'WILD'?             
         BNH   EDTUSR80                                                         
*                                                                               
         CLI   UTYPE,C'C'            IF TYPE IS CHARACTER...                    
         BNE   EDTUSR60              INPUT CANNOT BE NUMERIC                    
         LA    R4,8(R3)                                                         
         LLC   R1,5(R3)                                                         
EDTUSR40 LHI   R0,BADUSR3Q           ALPHA INPUT REQD                           
         CLI   0(R4),C'0'                                                       
         BL    EDTUSR50                                                         
         CLI   0(R4),C'9'                                                       
         BNH   EDTUSRER                                                         
EDTUSR50 LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'            IF TYPE IS NUMERIC...                      
         BNE   EDTUSR70                                                         
         GOTO1 =A(CHKNTYP),RR=RELO   INPUT MUST BE ALL NUMERIC                  
         BE    EDTUSR80                                                         
         LHI   R0,BADUSR4Q           NUMERIC INPUT REQD                         
         B     EDTUSRER                                                         
*                                                                               
*                                                                               
EDTUSR70 LHI   R0,BADDATE                                                       
         CLI   UTYPE,C'D'            IS TYPE DATE...                            
         JNE   *+2                   IF NO, BAD TYPE                            
         GOTO1 DATVAL,DMCB,(0,8(R3)),WORK                                       
         OC    DMCB(4),DMCB          INPUT MUST BE VALID DATE                   
         BZ    EDTUSRER                                                         
         L     R1,0(R1)                                                         
         LLC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   EDTUSRER                                                         
*                                                                               
EDTUSR80 LLC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)     MOVE INPUT INTO USERDATA                   
EDTXIT   MVC   ERRTEXT,SPACES      CLEAR ERRTEXT                                
         J     XIT                                                              
*                                                                               
EDTUSRER J     SPERR0                                                           
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       SET EDAILY FIELD SPEC-33510                   *         
***********************************************************************         
SETDLY   NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         MVC   EDAILY,SVCLDLY      INITIALIZE DAILY ESTIMATE INDICATOR          
*                                                                               
         LA    R2,ESTOPTH          USER DECLARED OPTIONS                        
         CLI   5(R2),0             ANY INPUT?                                   
         BE    SETDLYX             NO - DONE                                    
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   DMCB+4,0            VALID INPUT?                                 
         BE    ERRINV              NO - ERROR                                   
*                                                                               
         LA    R5,BLOCK            SCANNER BLOCK                                
         USING SCAND,R5            SCANNER BLOCK DSECT                          
         LLC   R8,DMCB+4           R8 = NUMER OF OPTIONS                        
*                                                                               
SETDLY10 LLC   R1,FLD1LEN          FIELD LENGTH                                 
         BCTR  R1,0                -1                                           
         EX    R1,*+8              EXECUTE COMPARE                              
         B     *+10                                                             
         CLC   FLD1(0),=C'DAILY'   DAILY OPTION?                                
         BNE   SETDLY20            NO - BUMP TO NEXT OPTION                     
         MVC   EDAILY,FLD2         SET DAILY FIELD                              
         CLI   EDAILY,C'N'         DAILY=N?                                     
         BE    SETDLYX             YES - DONE                                   
         LHI   R0,BADDAILYQ        IN CASE OF INVALID INPUT                     
         CLI   EDAILY,C'Y'         DAILY=Y?                                     
         BE    SETDLYX             YES - DONE                                   
         J     SPERR0              NO - DAILY MUST BE Y OR N                    
*                                                                               
SETDLY20 LA    R5,32(R5)           BUMP TO NEXT OPTION                          
         BCT   R8,SETDLY10         PROCESS NEXT OPTION                          
         DROP  R5                  DROP SCANNER BLOCK USING                     
*                                                                               
SETDLYX  MVC   NEWDLY,EDAILY       UPDATED DAILY FLAG                           
         J     XIT                 RETURN                                       
         LTORG                                                                  
***********************************************************************         
*                       VALIDATE PROFILE FIELD                        *         
***********************************************************************         
VALPROF  NTR1 BASE=*,LABEL=*                                                    
         XC    CASHPRD,CASHPRD                                                  
         XC    OLDCASH,OLDCASH                                                  
         MVI   OLDCPRD,0                                                        
*                                                                               
         CLI   SCRNFLAG,0          IF ALL OPTIONS CANNOT BE DISPLAYED           
         BE    VPRF00              BLANK OPTIONS FIELD AND SEND MESS.           
         MVC   ESTOPT,SPACES                                                    
         MVC   ESTOPT(39),=C'OPTIONS CANNOT BE CHANGED - CONTACT DDS'           
         OI    ESTOPTH+6,X'80'                                                  
         B     VPRFXIT                                                          
*                                                                               
***********************************************************************         
*                                                                               
*                                  AUTOMATICALLY SET OPTIONS                    
*                                                                               
VPRF00   DS    0H                                                               
         XC    ECOST2,ECOST2       INITIALIZE COST FACTOR AND PROFIT            
         XC    EPWPCT,EPWPCT       WITHIN PERCENTAGE TO ZERO                    
         XC    ETYPE,ETYPE                                                      
         MVI   SETCONV,C'N'                                                     
*                                                                               
         MVC   EDAILY,SVCLDLY      INITIALIZE DAILY ESTIMATE INDICATOR          
*                                  TO CLIENT DEFAULT                            
         CLI   POLSW,1             ADD/CHA BRD EST WITH POL EXISTING            
         BNE   VPRF00A             OPTION ETYPE SET TO SAME IN BRD              
         MVC   ETYPE,POLETYPE      EST AS POL                                   
         CLI   SVCLPROF+0,C'0'     FOR TPOL                                     
         BNE   VPRF00A                                                          
         TM    POLFLAG1,EF1SDE     SUPERDESK AUTH OPEN ON POL EST               
         BNO   *+8                                                              
         OI    EFLAG1,EF1SDE       THEN FORCE BRD ON TOO                        
*                                                                               
VPRF00A  CLI   POLSW,0                                                          
         BE    VPRF05                                                           
         CLI   POLSW,3             IF ADDING POL EST (POLSW=3)                  
         BNE   VPRF02                                                           
         OC    SVCCOST2,SVCCOST2   COST2 ON CLIENT REC                          
         BZ    VPRF01                                                           
         MVC   ECOST2,SVCCOST2     SET COST2 FROM CLIENT                        
         B     VPRF05                                                           
*                                                                               
VPRF01   CLI   WESTERN,C'Y'        WESTERN AGENCY?                              
         BNE   VPRF05              THEN SKIP                                    
         OC    SVCC2CON,SVCC2CON   OR CLIENT HAS BEEN CONVERTED                 
         BNZ   VPRF01A             THEN CALC COST2 FROM CLIENT PW               
*                                                                               
         CLI   SVCOFF,C'J'         OFFICE J STILL USES PW                       
         BE    *+12                                                             
         CLI   SVCOFF,C'F'         OFFICE F STILL USES PW                       
         BNE   VPRF01A             ELSE FORCE TO COS2                           
         MVC   EPWPCT,SVCLTPW      NO COS2 SET DEFAULT PW FROM CLIENT           
         B     VPRF05                                                           
*                                                                               
VPRF01A  L     R0,=F'1000000'      SET COS2PCT=1.000000                         
         CLC   SVCLTPW,=X'800000'  TEST PW=0                                    
         BE    VPRF01B                                                          
         OC    SVCLTPW,SVCLTPW     NO PW                                        
         BZ    VPRF05              THEN NO COS2                                 
*                                                                               
         SR    R0,R0               ELSE TRANSLATE TO COS2                       
         ICM   R0,7,SVCLTPW                                                     
         N     R0,=X'007FFFFF'                                                  
         L     RF,=F'8500'                                                      
         SR    RF,R0               GET 85 - PW PCT                              
         LR    R0,RF               IN R0                                        
*                                                                               
         L     RF,=F'8500'                                                      
         M     RE,=F'2000000'      X 1000000 X 2                                
         DR    RE,R0                                                            
         SRL   RF,1                                                             
         LR    R0,RF               SAVE FACTOR IN R0                            
VPRF01B  STCM  R0,15,ECOST2        STORE IN REC                                 
*                                                                               
         OC    SVCC2CON,SVCC2CON   CONVERSION DATE ALREADY IN CLT REC           
         BNZ   VPRF05                                                           
         MVI   SETCONV,C'Y'        FLAG TO TURN ON CC2CONV IN CLT               
         MVI   SVCC2CON,X'FF'      SET CONV DATE TO TRIGGER ERRORS              
         B     VPRF05                                                           
*                                                                               
VPRF02   MVC   SVETYPE,ETYPE       IN CASE USER DIDN'T TYPE IN TYPE=...         
         CLI   ACTEQU,ACTADD       ADDING BRD ESTIMATE (POLSW=1)                
         BNE   VPRF05              WITH EXISTING POL ESTIMATE                   
         MVC   EPWPCT,SVPOLPW      DEFAULT BRD TO POL PW %                      
         OC    SVCCOST2,SVCCOST2   UNLESS COST2 CLIENT ?                        
         BNZ   VPRF03                                                           
         OC    SVCC2CON,SVCC2CON   OR CLIENT HAS BEEN CONVERTED                 
         BZ    VPRF05                                                           
VPRF03   MVC   ECOST2,SVCCOST2     DEFAULT TO USE CLIENT COST2                  
         OC    SVPOLC2,SVPOLC2     UNLESS THERE IS A POL COST2                  
         BZ    *+10                                                             
         MVC   ECOST2,SVPOLC2      THEN USE THE POL COST2                       
         XC    EPWPCT,EPWPCT       IF COST2 THEN CLEAR PW                       
*                                                                               
***********************************************************************         
*                                                                               
VPRF05   LA    R2,ESTOPTH          USER DECLARED OPTIONS                        
         CLI   5(R2),0                                                          
         BE    VPRFX                                                            
         OC    ECPPEST,ECPPEST     CANNOT HAVE USER DECLARED OPTIONS IF         
         BNZ   ERRINV              CPP ESTIMATE PRESENT                         
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
         LLC   R8,DMCB+4           R8 = NUMER OF OPTIONS                        
         XC    ECGTPCT,ECGTPCT     WE DON'T NEED SPOOLD IN VALPROF              
*                                                                               
***********************************************************************         
*                                                                               
VPRF10   LLC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD1(0),=C'DAILY'     IF DAILY OPTION ...                        
         BNE   VPRF20                                                           
         MVC   EDAILY,FLD2                                                      
         CLI   EDAILY,C'N'                                                      
         BE    VPRF100                                                          
         LHI   R0,BADDAILYQ                                                     
         CLI   EDAILY,C'Y'           MUST BE Y OR N                             
         JNE   SPERR0                                                           
*                                                                               
*                                    SPREAD BETWEEN START AND END               
         MVC   ERRNUM,=AL2(DAILYERR) DATES MUST BE LESS THAN OR EQUAL           
         L     RF,ACOMFACS           TO 53 DAYS                                 
         USING COMFACSD,RF                                                      
         GOTO1 CPERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                     
         LH    R4,8(R1)                                                         
         CHI   R4,53                                                            
         BH    SPERREX               DAILY ESTIMATE TOO LONG                    
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF20   CLI   FLD1LEN,2           IF PW OPTION ...                             
         BNE   VPRF30                                                           
         CLC   FLD1(2),=C'PW'                                                   
         BNE   VPRF30                                                           
         LHI   R0,NOTPWCLTQ                                                     
         OC    SVCLTPW,SVCLTPW     CLIENT MUST BE A PW CLIENT                   
         JZ    SPERR0                                                           
*                                                                               
         OC    SVCCOST2,SVCCOST2   CLIENT HAS COST2                             
         BNZ   *+14                THEN PW INVALID                              
         OC    SVCC2CON,SVCC2CON   COST2 CLIENT ?                               
         BZ    VPRF22              NO, PW ALLOWED                               
         OC    SVPWPCT,SVPWPCT     PW ON RECORD BEFORE?                         
         BNZ   VPRF22              YES, LEAVE THE WAY IT IS                     
         LHI   R0,PWCOS2ER         NO, ERROR - MUST USE COST2                   
         J     SPERR0                                                           
*                                                                               
VPRF22   LLC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(2,FLD2),(R4)                                       
         LHI   R0,BADPWQ                                                        
         CLI   DMCB,X'00'                                                       
         JNE   SPERR0              MUST BE VALID MONETARY                       
         MVC   EPWPCT,DMCB+5                                                    
         OC    EPWPCT,EPWPCT                                                    
         BNZ   VPRF100                                                          
         OI    EPWPCT,X'80'        SET PROFIT WITHIN PERCENTAGE FLAG            
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF30   CLC   FLD1(3),=C'REQ'       IF REQ OPTION ...                          
         BNE   VPRF31                                                           
*                                                                               
         LHI   R0,BADREQOPQ          MUST BE A POL ESTIMATE                     
         CLC   QPRD,=C'POL'                                                     
         JNE   SPERR0                                                           
*                                                                               
         LHI   R0,REQERR1                                                       
         NI    EFLAG1,X'FF'-EF1REQ   MUST BE A Y OR N                           
         CLI   FLD2,C'N'                                                        
         BE    VPRF100                                                          
         CLI   FLD2,C'Y'                                                        
         BNE   SPERR0                                                           
         OI    EFLAG1,EF1REQ         IF Y ... SET SF JWT REQ=Y FLAG             
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF31   CLC   FLD1(4),=C'TYPE'      IF TYPE OPTION ...                         
         BNE   VPRF35                                                           
*                                                                               
         LHI   R0,TYPERR                                                        
         LLC   R1,FLD2LEN                                                       
         BCTR  R1,0                                                             
         L     R6,=A(ETYPTAB)                                                   
         A     R6,RELO                                                          
*                                                                               
VPRF31A  CLI   0(R6),X'FF'           END OF TABLE?                              
         JE    SPERR0                YES, ERROR                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD2(0),1(R6)         MATCH?                                     
         BE    VPRF31B               YES                                        
         LLC   R4,0(R6)                                                         
         AR    R6,R4                                                            
         B     VPRF31A                                                          
*                                                                               
VPRF31B  MVC   ETYPE,1(R6)                                                      
*                                                                               
VPRF32   CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VPRF34                ESTIMATE FOR A CLIENT WITH A POL           
         LHI   R0,BRPOLERA           ESTIMATE) ... ETYPE FOR                    
         CLC   ETYPE,POLETYPE        BRAND ESTIMATE MUST MATCH THE              
         JNE   SPERR0                POL'S ETYPE                                
*                                                                               
VPRF34   MVC   SVETYPE,ETYPE                                                    
         B     VPRF100                                                          
*                                                                               
VPRF35   CLC   FLD1(3),=C'CGT'       IF CGT OPTION ...                          
         BNE   VPRF37                                                           
         LHI   R0,CGTERR                                                        
         LLC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(2,FLD2),(R4)                                       
         CLI   DMCB,X'00'                                                       
         JNE   SPERR0                MUST BE MONETARY AND EQUAL TO OR           
         CLC   DMCB+4(4),=F'9999'    LESS THAN $9.99                            
         JH    SPERR0                                                           
         MVC   ECGTPCT,DMCB+6        SET CLIENT GROSS TRADE PERCENTAGE          
         B     VPRF100               (TBS)                                      
*                                                                               
***********************************************************************         
*                                                                               
VPRF37   CLC   FLD1(5),=C'DEMOS'     IF DEMOS OPTION ...                        
         BNE   VPRF38                                                           
         LHI   R0,ODEERR                                                        
         NI    EFLAG1,X'FF'-EF1NODEM                                            
         CLI   FLD2,C'Y'             MUST BE Y OR N                             
         BE    VPRF100                                                          
         CLI   FLD2,C'N'                                                        
         JNE   SPERR0                IF N ... SET FLAG FOR NO DEMOS             
         OI    EFLAG1,EF1NODEM       REQUIRED IN BUYS                           
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF38   CLC   FLD1(4),=C'OWPW'      IF OWPW OPTION ...                         
         BNE   VPRF39                                                           
         OI    EFLAG1,EF1OOWPW       SET PW OOW BILLING FEATURE FLAG            
         CLC   FLD2(2),=C'TG'        LET TRACY GLASS TURN IT OFF                
         BNE   *+8                                                              
         NI    EFLAG1,X'FF'-EF1OOWPW                                            
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF39   CLC   FLD1(4),=C'COS2'     IF COS2 OPTION ...                          
         BNE   VPRF40                                                           
*                                                                               
         LHI   R0,NOPWPCT                                                       
         OC    EPWPCT,EPWPCT       DO THEY ALREADY HAVE PW?                     
         JNZ   SPERR0              THEN NO COS2                                 
*                                                                               
         LHI   R0,1405             DON'T ALLOW IF CLT COS2=T/O                  
         TM    SVCLOP3,COP3COSQ    COS2 OPTIONAL (TRADE=O)?                     
         JNZ   SPERR0              THEN NO COS2                                 
         TM    SVCLOP4,COP4TRD     COS2 = TRADE?                                
         JNZ   SPERR0              THEN NO COS2                                 
*                                                                               
         LHI   R0,COS2ERR                                                       
         LLC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(6,FLD2),(R4)                                       
         CLI   DMCB,0                                                           
         JNE   SPERR0               MUST BE MONETARY AND FIT BETWEEN            
         CLC   4(4,R1),=F'9999999'  1 AND $9.99                                 
         JH    SPERR0                                                           
*                                                                               
         MVC   ECOST2,DMCB+4         SET ECOST2 FIELD                           
         OC    ECOST2,ECOST2                                                    
         BNZ   VPRF100                                                          
         OI    ECOST2,X'80'          IF INPUT IS ZERO, SET 'ZERO WAS            
         B     VPRF100               INPUT' BIT                                 
*                                                                               
***********************************************************************         
*                                                                               
VPRF40   CLC   FLD1(3),=C'SLN'       IF SLN OPTION ...                          
         BNE   VPRF45                                                           
*                                                                               
         CLC   =C'DEL',FLD2          IF DEL, REMOVE RESTRICTION FOR             
         BNE   VPRF40A               BUYING ONLY THIS SPOT LENGTH               
         MVI   ESLN,0                                                           
         B     VPRF100                                                          
*                                                                               
VPRF40A  MVC   DMCB+4(4),=X'D9000A57'                                           
         GOTO1 CALLOV,DMCB,0         GET SPSLENTAB                              
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     R1,DMCB             POINT TO START OF PHASE                      
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            EOT DSPL                                     
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
         MVI   BYTE,C'T'                                                        
         CLI   QMED,C'T'                                                        
         BE    VPRF41                                                           
         CLI   QMED,C'N'                                                        
         BE    VPRF41                                                           
         CLI   QMED,C'C'                                                        
         BE    VPRF41                                                           
*                                                                               
         MVI   BYTE,C'R'                                                        
         CLI   QMED,C'R'                                                        
         BE    VPRF41                                                           
         CLI   QMED,C'X'                                                        
         BE    VPRF41                                                           
         DC    H'0'                                                             
*                                                                               
VPRF41   CLC   =C'00',0(R1)        TEST DEFAULT TABLE                           
         BE    VPRF42                                                           
         CLC   0(2,R1),AGENCY      ELSE MATCH AGY                               
         BNE   *+14                                                             
VPRF42   CLC   BYTE,2(R1)          AND MEDIA                                    
         BE    VPRF43                                                           
*                                                                               
         BXLE  R1,RE,VPRF41                                                     
         DC    H'0'                                                             
*                                                                               
VPRF43   LHI   R0,SPTLNERR                                                      
         AHI   R1,4                POINT BEYOND HEADER                          
         ICM   R4,15,FLD2B         GET SLN                                      
         AR    R4,R4               X 2                                          
         AR    R4,R1               POINT TO ENTRY                               
         CLI   1(R4),0             SLN VALID?                                   
         JE    SPERR0              NO                                           
         MVC   ESLN,FLD2B+3        SAVE SPOT LEN THAT USER INPUT                
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF45   CLC   FLD1(4),=C'CASH'      IF CASH OPTION ...                         
         BNE   VPRF50                                                           
         LHI   R0,CASH1                                                         
         CLI   FLD2LEN,3             CASH PRODUCT MUST BE 3 CHARACTERS          
         JNE   SPERR0                LONG                                       
*                                                                               
         LHI   R0,CASHTRD            CASH PRD CANNOT BE TRADE                   
         CLI   FLD2+2,C'#'                                                      
         JE    SPERR0                                                           
*                                                                               
         LHI   R0,NOCSHEST           CASH ESTIMATE MUST BE ON FILE ...          
         XC    KEY,KEY                                                          
         MVC   KEY(8),ESTKEY                                                    
         MVC   KEY+4(3),FLD2                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         JNE   SPERR0                                                           
         L     R6,AIO3               SAVE CASH ESTIMATE'S TRADE PRODUCT         
         MVC   AIO,AIO3              INTO SVTRDPRD                              
         GOTO1 GETREC                                                           
         MVC   SVTRDPRD,ETRDPRD-ESTHDR(R6)                                      
*                                                                               
         LHI   R0,CASH2              CASH PRODUCT MUST BE IN CLIENT'S           
         XC    KEY,KEY               CLIST                                      
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO2                                                          
         LA    R6,CLIST-CLTHDR(0,R6)                                            
VPRF46   CLI   0(R6),0                                                          
         JE    SPERREX                                                          
         CLC   FLD2(3),0(R6)                                                    
         BE    VPRF47                                                           
         LA    R6,4(R6)                                                         
         B     VPRF46                                                           
*                                                                               
VPRF47   LHI   R0,CASHSET                                                       
         CLI   SVTRDPRD,0            IF TRADE PRODUCT IS NOT THE SAME           
         BE    *+14                  AS CURRENT PRODUCT ... ERROR               
         CLC   SVTRDPRD,SVPRD                                                   
         JNE   SPERR0                                                           
*                                                                               
         LHI   R0,CASHCHG                                                       
         CLC   ECASHPRD,3(R6)        IF INPUTTED CASH PRODUCT HAS               
         BE    VPRF48                CHANGED ... SAVE OLD CASH PRODUCT          
         CLI   ECASHPRD,0            INTO OLD CASH                              
         BE    VPRF48                                                           
         MVC   OLDCPRD,ECASHPRD                                                 
         CLI   SONIA,C'C'            UNLESS SONIA=C ... CANNOT CHANGE           
         JNE   SPERR0                CASH PRODUCT                               
         TM    SVCLOP2,COP2DIY       DIY TRADE                                  
         JO    SPERR0                CANNOT CHANGE                              
VPRF48   MVC   ECASHPRD,3(R6)                                                   
         CLI   SVTRDPRD,0            MOVE INPUTTED CASH PRODUCT IN              
         BNE   *+10                                                             
         MVC   CASHPRD,FLD2                                                     
*                                                                               
         CLI   OLDCPRD,0             DELETE CASH PRODUCT FROM CLIENT'S          
         BE    VPRF49                CLIST                                      
         L     R6,AIO2                                                          
         LA    R6,CLIST-CLTHDR(R6)                                              
VPRF48A  CLI   0(R6),0                                                          
         BE    ERRINV                                                           
         CLC   OLDCPRD,3(R6)                                                    
         BE    VPRF48B                                                          
         LA    R6,4(R6)                                                         
         B     VPRF48A                                                          
VPRF48B  MVC   OLDCASH,0(R6)                                                    
VPRF49   MVC   KEY,ESTKEY                                                       
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF50   CLI   FLD1LEN,2                                                        
         BNE   VPRF70                                                           
         CLC   FLD1(2),=C'SD'        SUPERDESK ESTIMATE                         
         BE    VPRF100               NO VALIDATION                              
*                                                                               
VPRF70   CLC   FLD1(3),=C'TRD'                                                  
         BE    VPRF100                                                          
***********************************************************************         
VPRF80   DS    0H                                                               
         CLC   FLD1(5),=C'CPPRS'                                                
         BNE   ERRINV                                                           
*                                                                               
         LHI   R0,INVCPPRS                                                      
         CLI   FLD2LEN,1                                                        
         JNE   SPERR0                                                           
*                                                                               
         CLI   FLD2,C'Y'                                                        
         BNE   *+12                                                             
         MVI   ECPPRS,C'Y'                                                      
         B     VPRF100                                                          
*                                                                               
         CLI   FLD2,C'N'                                                        
         JNE   SPERR0                                                           
         MVI   ECPPRS,C'N'                                                      
***********************************************************************         
*                                                                               
VPRF100  LA    R5,32(R5)             BUMP TO NEXT OPTION                        
         BCT   R8,VPRF10                                                        
         DROP  R5                                                               
*                                                                               
***********************************************************************         
*                                                                               
VPRFX    DS    0H                                                               
*                                                                               
         CLI   T217FFD+1,C'*'      TEST DDS TERMINAL?                           
         BE    VPRFX1              YUP, SKIP COS2 CHECK                         
         MVC   ERRNUM,=AL2(C2DELERR)                                            
         LA    R2,ESTOPTH                                                       
         OC    SVCOST2,SVCOST2                                                  
         BZ    *+14                                                             
         OC    ECOST2,ECOST2                                                    
         BZ    SPERREX              MUST BE MONETARY AND FIT BETWEEN            
*                                                                               
VPRFX1   DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VPRFX1A                                                          
         CLI   ETYPE,C'S'                                                       
         BE    *+12                                                             
         CLI   ETYPE,C'B'                                                       
         BNE   VPRFX1A                                                          
         MVC   ERRNUM,=AL2(TYPEERR1)                                            
         BRAS  RE,CHKTYPE                                                       
         BNE   SPERREX                                                          
*                                                                               
VPRFX1A  DS    0H                                                               
         OC    SVCLTPW,SVCLTPW       IF CLIENT SET FOR PROFIT WITHIN            
         BZ    VPRFX6                PERCENTAGE ...                             
*                                                                               
VPRFX2   CLC   =C'POL',QPRD          IF PRD=POL                                 
         BE    VPRFX2A                                                          
         OC    SVPOLPW,SVPOLPW       IS THERE POL REC WITH PW ?                 
         BZ    VPRFX3                IF YES, NON-POL REC HAS TO HAVE IT         
*                                                                               
VPRFX2A  OC    SVCC2CON,SVCC2CON     NO, CHECK IF COS2 CONVERTED ?              
         BNZ   VPRFX3                IF YES, PW IS NOT REQUIRED                 
         OC    ECOST2,ECOST2         ALREADY HAS COS2 INSTEAD                   
         BNZ   VPRFX3                THEN SKIP PW ERROR                         
         MVC   ERRNUM,=AL2(NOPWPCT)                                             
         OC    EPWPCT,EPWPCT         THEN PW PERCENTAGE IS REQUIRED             
         BZ    SPERREX                                                          
*                                                                               
VPRFX3   CLC   =C'POL',QPRD          IF POL ESTIMATE ... SAVE PROFIT            
         BNE   VPRFX4                WITHIN PERCENTAGE INTO SVPOLPW             
         MVC   SVPOLPW,EPWPCT                                                   
         B     VPRFX6                                                           
*                                                                               
VPRFX4   OC    EPWPCT,EPWPCT         IF THERE IS A BRD PW PCT                   
         BZ    VPRFX6                THEN IT MUST MATCH THE POL PW              
         MVC   ERRNUM,=AL2(NOTPOLPW) IF (CHANGING OR ADDING BRAND EST-          
         CLC   SVPOLPW,EPWPCT        IMATE WITH EXISTING POL ESTIMATE)          
         BNE   SPERREX               ... PWP'S MUST MATCH                       
*                                                                               
VPRFX6   OC    EPWPCT,EPWPCT         IF THERE IS PW - CHECK DATES               
         BZ    VPRFX8                                                           
         GOTO1 ADDAY,DMCB,SYR,WORK,98                                           
         CLC   WORK(6),EYR                                                      
         BH    VPRFX10               CAN'T HAVE PROFIT WITHIN PERCENT-          
         MVC   ERRNUM,=AL2(INVPW)    AGE ON AN ESTIMATE THAT SPANS MORE         
         B     SPERREX               THAN 14 WEEKS                              
*                                                                               
VPRFX8   CLC   =C'POL',QPRD          IF PROFIT WITHIN PERCENTAGE HAS            
         BNE   VPRFX9                BEEN ERASED FROM THE POL RECORD            
         OC    SVPWPCT,SVPWPCT       ... CLEAR SVPOLPW                          
         BZ    VPRFX10                                                          
         MVC   SVPOLPW,EPWPCT                                                   
         B     VPRFX10                                                          
*                                                                               
VPRFX9   MVC   ERRNUM,=AL2(DELPWPOL) CANNOT ERASE PROFIT WITHIN PER-            
         OC    SVPWPCT,SVPWPCT       CENTAGE FROM A BRAND ESTIMATE WITH         
         BZ    VPRFX10               AN EXISTING POL ESTIMATE                   
         CLI   POLSW,1                                                          
         BE    SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRFX10  TM    SVAGYFL1,AGYCOS2Q     IF AGENCY REQUIRES COST FACTOR AND         
         BZ    VPRFX11               NOT INPUTTED ... MOVE IN COST              
         OC    ECOST2,ECOST2         FACTOR FROM CLIENT RECORD ON ADD           
         BNZ   VPRFX11                                                          
         CLI   ACTEQU,ACTADD         ONLY COPY FROM CLIENT ON ADD               
         BNE   VPRFX11                                                          
         MVC   ECOST2,SVCCOST2                                                  
*                                                                               
***********************************************************************         
*                                                                               
VPRFX11  MVC   ERRNUM,=AL2(CASHINV)                                             
         CLI   QPRD+2,C'#'           CASH OPTION INVALID FOR NON-TRADE          
         BE    VPRFX11A              PRODUCTS                                   
         CLI   ECASHPRD,0                                                       
         BNE   SPERREX                                                          
         B     VPRFX12                                                          
*                                                                               
VPRFX11A TM    SVCLOP2,COP2DIY       DIY TRADE                                  
         BNO   VPRFX11C                                                         
         CLI   ACTEQU,ACTADD                                                    
         BNE   VPRFX11C              ONLY NEED TO SET UP ON ADD                 
         MVC   ECASHPRD,BPRD         SET UP LINK WITH CASH/TRD                  
         NI    ECASHPRD,X'FF'-X'80'  CASH=TRD-X'80'                             
         MVC   CASHPRD,QPRD                                                     
         MVI   CASHPRD+2,C'C'        CASH PRD IS TRD WITH C                     
         MVC   ERRNUM,=AL2(NOCSHEST) CASH ESTIMATE MUST BE ON FILE ...          
         XC    KEY,KEY                                                          
         MVC   KEY(8),ESTKEY                                                    
         MVC   KEY+4(3),CASHPRD                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BE    VPRFX11B                                                         
         LA    R2,ESTPRDKH           PRODUCT                                    
         B     SPERREX                                                          
VPRFX11B GOTO1 =A(NODIYEST),RR=RELO  ONLY 1 CSH/TRD PAIR PER EST                
         BE    VPRFX11C                                                         
         MVC   ERRNUM,=AL2(ONLY1DIY)                                            
         LA    R2,ESTPRDKH           PRODUCT FIELD                              
         B     SPERREX                                                          
*                                                                               
VPRFX11C MVC   ERRNUM,=AL2(CASHREQ)  CASH OPTION REQUIRED FOR TRADE             
         CLI   ECASHPRD,0            PRODUCTS                                   
         BE    SPERREX                                                          
*                                                                               
*  AS PER STEVE K ALLOW REMOVAL OF TYPE=BAR/STW                                 
*  SHOULD BE SAFE AS BY DEFINITION THERE SHOULD BE NO BILLED/PAID               
*                                                                               
VPRFX12  DS    0H                                                               
*                                                                               
VPRFXIT  J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
************************************************************                    
* NODIYEST-CHECKS THAT THERE ARE NO OTHER TRADE PRD ESTS   *                    
*          OPEN FOR THIS ESTIMATE                          *                    
*          SETS CC = IF IT DOESN'T FIND ANY                *                    
************************************************************                    
*                                                                               
NODIYEST NTR1 BASE=*,LABEL=*                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY       THRU PRD                                     
         GOTO1 HIGH                                                             
         B     NOD10                                                            
NODSEQ   GOTO1 SEQ                                                              
NOD10    CLC   KEY(4),KEYSAVE                                                   
         BNE   NODXYES                                                          
         CLC   ESTKEY+7(1),KEY+7   SAME ESTIMATE                                
         BNE   NODSEQ                                                           
         CLI   KEY+6,C'#'          IS IT FOR A TRADE PRD                        
         BE    NODXNO                                                           
         B     NODSEQ                                                           
*                                                                               
NODXYES  SR    RC,RC                                                            
NODXNO   LTR   RC,RC                                                            
         J     XIT                                                              
         EJECT                                                                  
************************************************************                    
* CHKTAL - CHECKS IF TALENT FACTOR RECORD EXISTS           *                    
*        - EX DF HAS CHILD SPOT ESTIMATES THAT CAN BE LONG *                    
*          SETS CC = IF IT EXISTS                          *                    
************************************************************                    
*                                                                               
CHKTAL   NTR1 BASE=*,LABEL=*                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D27'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(5),KEYSAVE                                                   
         J     XIT                                                              
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CHKNTYP                                                      *         
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF PASSED VARIABLE IS VALID NUMERIC                
***********************************************************************         
CHKNTYP  NTR1 BASE=*,LABEL=*                                                    
         LA    R4,8(R3)              R4 ---> INPUT                              
         LLC   R1,5(R3)              R1= L(INPUT)                               
CHKN10   LAY   R5,VALDNTBL           R5 = TABLE OF VALID DIGITS                 
CHKN20   CLC   0(1,R4),0(R5)         VALID DIGIT?                               
         BE    CHKN30                                                           
         LA    R5,1(R5)              BUMP TO NEXT DIGIT                         
         CLI   0(R5),EOTBLQ          END OF TABLE?                              
         BE    XCHKN                                                            
         B     CHKN20                NO, TRY AGAIN                              
*                                                                               
CHKN30   LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,CHKN10                                                        
XCHKN    LTR   R1,R1                                                            
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
*                                                                               
PTREC    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,ECHDATE)                                    
*                                                                               
         MVI   USEIO,C'Y'                                                       
*                                                                               
* CODE BELOW IS INTENDED TO KEEP NONT NAME LISTS IN PERMANENT SYNCH!            
*                                                                               
         CLC   QPRD,=C'POL'        IF DOING POL, SKIP THIS                      
         JE    PTREC2                                                           
         CLC   ELEN,=AL2(ESTHDRLN) OR IF NO NONT NAMES, SKIP THIS               
         JNH   PTREC2                                                           
         MVC   ENONTDMS(160),POLNTRDM  THEN KEEP NONT NAMES IN SYNCH            
*                                                                               
PTREC2   CLI   NEWLEN,C'Y'         NEED TO ADD LONGER REC?                      
         JE    PTREC4                                                           
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
         J     PTRECX                                                           
*                                                                               
* ADD LONGER RECORD AND DO NOT DIE ON DUP KEY ON ADD                            
* THEN UPDATE DISK ADDRESS IN ACTIVE AND PASSIVE POINTERS                       
*                                                                               
PTREC4   GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
*                                                                               
         MVC   KEYSAVE,KEY         SAVE KEY WITH NEW DISK ADDRESS               
         GOTO1 (RF),(R1),=C'DMRDHI',=C'SPTDIR',KEY,KEY                          
         CLC   KEY(13),KEYSAVE     MAKE SURE I FOUND IT                         
         JNE   *+2                                                              
         MVC   KEY,KEYSAVE         MOVE KEY WITH NEW D/A                        
         GOTO1 (RF),(R1),=C'DMWRT'                                              
         CLI   8(R1),0                                                          
         JNE   *+2                                                              
PTRECX   MVI   USEIO,C'N'                                                       
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
*                                                                               
ADREC    NTR1  BASE=*,LABEL=*                                                   
***NOPHW MVC   ELEN,=AL2(ESTHDR2Q)                                              
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(2,ECRDATE)                                    
*                                                                               
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         JNE   *+2                                                              
         MVI   USEIO,C'N'                                                       
         J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD OR CHANGE CANADIAN ESTIMATE               *                        
***********************************************************************         
*        IF CANADIAN AGENCY ADDS OR MODIFIES AN ESTINMATE FOR TV -    *         
*        ADD OR MODIFY PRODUCT RECORD FOR MEDIA N(03) & MEDIA C(08)   *         
***********************************************************************         
CANTV    NTR1  BASE=*,LABEL=*                                                   
*  ON ENTRY CANKEY WILL HAVE THE MED T KEY                                      
*  ON ENTRY AIO WILL HAVE CHANGED T RECORD - COULD BE AIO1 OR AIO2              
*                                                                               
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTX                                                              
         CLI   QMED,C'T'             TV?                                        
         BNE   CTX                                                              
*                                                                               
         MVC   CANAIO,AIO         - SAVE TO CANAIO                              
         L     R4,CANAIO                                                        
         LA    R5,ESTHDRLN                                                      
         LA    RE,2000(R4)                                                      
         LR    RF,R5                                                            
         MVCL  (RE),(R4)           SAVE REC IN IO1+2000                         
*                                                                               
         CLI   SVCXTRA+8,C'P'        P&G CLIENT?                                
         BE    CT20                  YES - NO MEDIA N!                          
*                                                                               
         L     R4,CANAIO                                                        
         XC    KEY,KEY               MEDIA N(03)                                
         MVC   KEY,CANKEY            MEDIA T KEY JUST UPDATED                   
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT10                                                             
         L     R5,CANAIO                                                        
         MVC   0(13,R5),KEYSAVE      RECORD DOESN'T EXIST                       
         MVC   KEY(13),KEYSAVE       MUST ADD IT                                
         MVC   AIO,CANAIO                                                       
         BRAS  RE,CLRBCKTS                                                      
         BRAS  RE,ADREC                                                         
         B     CT20                                                             
*                                                                               
CT10     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,CANAIO                                                        
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,CANAIO                                                       
         BRAS  RE,MVBCKTS            MOVE BUCKETS FROM AIO3 TO CANAIO           
         BRAS  RE,PTREC              PUT (03) RECORD                            
*                                                                               
CT20     XC    KEY,KEY               MEDIA C(08)                                
         MVC   KEY,CANKEY            MEDIA T KEY JUST UPDATED                   
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT30                                                             
         L     R5,CANAIO             RECORD DOESN'T EXIST                       
         MVC   0(13,R5),KEYSAVE      MUST ADD IT                                
         MVC   AIO,CANAIO                                                       
         MVC   KEY(13),KEYSAVE                                                  
         BRAS  RE,CLRBCKTS                                                      
         BRAS  RE,ADREC                                                         
         B     CT40                                                             
*                                                                               
CT30     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,CANAIO                                                        
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,CANAIO                                                       
         BRAS  RE,MVBCKTS            MOVE BUCKETS FROM AIO3 TO CANAIO           
         BRAS  RE,PTREC              PUT C(08) RECORD                           
CT40     MVC   KEY,CANKEY            RESTORE MEDIA T KEY JUST UPDATED           
*                                                                               
         L     R4,CANAIO                                                        
         LA    R5,ESTHDRLN                                                      
         LA    RE,2000(R4)                                                      
         LR    RF,R5                                                            
         MVCL  (R4),(RE)           RESTORE REC IN IO1+2000                      
*                                                                               
         MVC   AIO,CANAIO          RESTORE AIO                                  
*                                                                               
CTX      J     XIT                                                              
*                                                                               
REC3     USING ESTHDR,R1                                                        
*                                                                               
MVBCKTS  L     R1,AIO3             FROM AIO3                                    
         L     R3,CANAIO           TO CANAIO                                    
         MVC   EORD(EORDX-EORD),REC3.EORD                                       
         MVC   EORDNET(EORDNETX-EORDNET),REC3.EORDNET                           
         MVC   EPAID(EPAIDX-EPAID),REC3.EPAID                                   
         MVC   EPDNET(EPDNETX-EPDNET),REC3.EPDNET                               
         MVC   EAUTH(EAUTHX-EAUTH),REC3.EAUTH                                   
         MVC   ECURPDN,REC3.ECURPDN                                             
         BR    RE                                                               
         DROP  REC3                                                             
*                                                                               
CLRBCKTS L     R3,CANAIO                                                        
*                                                                               
         ZAP   ECURPDN,=PL6'0'                                                  
         LA    R1,26                                                            
         LA    R2,EORD                                                          
         ZAP   0(6,R2),=PL6'0'                                                  
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         LA    R1,13                                                            
         LA    R2,EAUTH                                                         
         ZAP   0(6,R2),=PL6'0'                                                  
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         LA    R1,26                                                            
         LA    R2,EPAID                                                         
         ZAP   0(6,R2),=PL6'0'                                                  
         LA    R2,6(R2)                                                         
         BCT   R1,*-10                                                          
*                                                                               
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        SPACES TO ZEROS                                                        
**********************************************************************          
SPTOZER  NTR1  BASE=*,LABEL=*                                                   
SPTOZ10  CLI   ESTESTK,C' '          CHANGE LEADING SPACES TO ZEROS             
         BH    SPTOZX                                                           
         MVI   ESTESTK,X'F0'                                                    
         OI    ESTESTKH+6,X'80'      TRANSMIT                                   
         CLI   ESTESTK+1,C' '                                                   
         BH    SPTOZ20                                                          
         MVI   ESTESTK+1,X'F0'                                                  
*                                                                               
SPTOZ20  MVC   ERRNUM,=AL2(ESTERR1)  EST CODE MUST BE NUMERIC                   
         LA    R3,3                                                             
         LA    R4,ESTESTK                                                       
SPTOZ25  CLI   0(R4),X'F9'                                                      
         BH    SPERREX                                                          
         CLI   0(R4),X'F0'                                                      
         BL    SPERREX                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,SPTOZ25                                                       
         OI    ESTESTKH+4,X'08'      SET FOR VALID NUMERIC                      
SPTOZX   J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
         TM    USRIDFLG,USRRNTKQ        ACCESS TO COMSCORE?                     
         BZ    SETUP00                                                          
         CLI   ESTMEDK,C'R'             RADIO?                                  
         JE    SETUP00                                                          
         CLI   ESTMEDK,C'X'             RADIO?                                  
         JE    SETUP00                                                          
         NI    ESTCBTH+1,X'FF'-X'0C'    NORMAL INTENSITY                        
         OI    ESTCBTH+6,X'80'                                                  
         NI    ESTCBTYH+1,X'FF'-X'0C'                                           
         OI    ESTCBTYH+6,X'80'                                                 
         NI    ESTCSDH+1,X'FF'-X'0C'                                            
         OI    ESTCSDH+6,X'80'                                                  
         NI    ESTCSDTH+1,X'FF'-X'0C'                                           
         OI    ESTCSDTH+6,X'80'                                                 
*                                                                               
SETUP00  LA    R2,CONACTH          POINT TO ACTION                              
*                                                                               
* COMMENTED OUT NEXT 4 LINES CAUSE THEY DON'T DO ANYTHING! AKAT 8/30/07         
***      CLI   T217FFD+1,C'*'      TEST DDS TERM                                
***      BE    SETUP01                                                          
***      TM    T217FFD+12,X'10'                                                 
***      BNO   SETUP01             NOT ON = ALL OK                              
*                                                                               
SETUP01  MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OI    ESTREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         CLI   CALLSP,0                                                         
         BE    *+8                                                              
         NI    ESTREH+1,X'FF'-X'04'  LIGHT UP PF12 FIELD                        
         OI    ESTREH+6,X'80'                                                   
*                                                                               
SETUP10  GOTO1 INITPFKY,DMCB,PFTABLE  PF TABLE                                  
*                                                                               
         L     RF,ACOMFACS                                                      
         USING COMFACSD,RF                                                      
         GOTO1 GETFACT,DMCB,(2,0)                                               
         L     R1,0(R1)              SET MEDIA IN USE                           
         USING FACTSD,R1                                                        
         MVC   OVSYS,FAOVSYS         2=SPOT,3=NET                               
         MVC   LINID,FALINE                                                     
         MVC   LINADDR,FAADDR                                                   
         MVC   OVSYS,FAOVSYS         2=SPOT,3=NET                               
         DROP  R1,RF                                                            
*                                                                               
         MVI   WESTERN,C'N'        SET WESTERN AGENCY                           
         CLC   AGENCY,=C'WI'       WESTERN WHO ELSE?                            
         BE    SETUP20                                                          
         CLC   AGENCY,=C'WJ'       WESTERN WHO ELSE?                            
         BE    SETUP20                                                          
         CLC   AGENCY,=C'WT'       WESTERN WHO ELSE?                            
         BNE   SETUP30                                                          
SETUP20  MVI   WESTERN,C'Y'        SET WESTERN AGENCY                           
*                                                                               
SETUP30  LA    RF,SECFLDPO         SECURITY FIELD FOR PO #                      
         LA    R2,ESTPONDH         ESTIMATE PO# FIELD                           
         BRAS  RE,PRCFLD           PROTECT FIELDS IF NECESSARY                  
*                                                                               
         LA    RF,SCFLDEBF         SECURITY FIELD FOR BILL BASIS                
         LA    R2,ESTBBTXH         ESTIMATE BILL BASIS FIELD                    
         BRAS  RE,PRCFLD           PROTECT FIELDS IF NECESSARY                  
*                                                                               
         LA    RF,SCFLDEBF         SECURITY FIELD FOR COMM PCT                  
         LA    R2,ESTCPTXH         ESTIMATE COMM PCT FIELD                      
         BRAS  RE,PRCFLD           PROTECT FIELDS IF NECESSARY                  
*                                                                               
         LA    RF,SCFLDEBF         SECURITY FIELD FOR COMM BASIS                
         LA    R2,ESTCBTXH         ESTIMATE COMM BASIS FIELD                    
         BRAS  RE,PRCFLD           PROTECT FIELDS IF NECESSARY                  
*                                                                               
SETUPX   J     XIT                                                              
*                                                                               
PRCFLD   DS    0H                  CLEAR TITLE & PROTECT FIELDS                 
         LLC   R1,0(R2)            TITLE FIELD LENGTH                           
         CLI   0(RF),0             FULL ACCESS?                                 
         BNE   PRCF20              NO                                           
         AR    R2,R1               BUMP TO INPUT FIELD                          
         NI    1(R2),X'FF'-X'20'   UNPROTECT INPUT FLD                          
         B     PRCFLDX             TRANSMIT CHANGE AND EXIT                     
*                                                                               
PRCF20   CLI   0(RF),C'Y'          READ ONLY?                                   
         BE    PRCF30              YES                                          
         CLI   0(RF),C'N'          NO ACCESS?                                   
         JNE   *+2                 INVALID SECURITY VALUE                       
         SHI   R1,8+1              MINUS OVERHEAD AND ONE FOR EX                
         CHI   R1,0                TITLE FIELD LENGTH > 0?                      
         JL    *+2                 NO - BAD TITLE FLD LENGTH                    
         EX    R1,*+8              CLEAR TITLE FIELD                            
         B     *+10                                                             
         XC    8(0,R2),8(R2)       ** EXECUTED **                               
         OI    6(R2),X'80'         TRANSMIT CLEARED TITLE FIELD                 
*                                                                               
PRCF30   IC    R1,0(R2)            TITLE FIELD LENGTH                           
         AR    R2,R1               BUMP TO INPUT FIELD                          
         OI    1(R2),X'20'         PROTECT INPUT FLD                            
PRCFLDX  OI    6(R2),X'80'         TRANSMIT PROTECTED INPUT FIELD               
         BR    RE                  DONE                                         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PFTABLE  DS   0H                                                                
*        CLIENT MAINT DISPLAY                                                   
         DC   AL1(MPF04X-*,04,PFTCPROG,(MPF04X-MPF04)/KEYLNQ,0)                 
         DC   CL3'CM '                 MAINT                                    
         DC   CL8'CLT'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF04    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
MPF04X   EQU  *                                                                 
*                                                                               
*        CLIENT2 MAINT DISPLAY                                                  
         DC   AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                 
         DC   CL3'CM2'                 MAINT                                    
         DC   CL8'CL2'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF05    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
MPF05X   EQU  *                                                                 
*                                                                               
*        PRODUCT MAINT DISPLAY                                                  
         DC   AL1(MPF02X-*,02,PFTCPROG,(MPF02X-MPF02)/KEYLNQ,0)                 
         DC   CL3'PM '                 DISPLAY                                  
         DC   CL8'PRD'                 RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF02    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
MPF02X   EQU  *                                                                 
*                                                                               
*        ESTIMATE COPY                                                          
         DC   AL1(LPF08X-*,08,PFTCPROG,(LPF08X-LPF08)/KEYLNQ,0)                 
         DC   CL3'EC '                 MAINT                                    
         DC   CL8'EST'                 RECORD                                   
         DC   CL8'COPY'                ACTION                                   
LPF08    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTESTK-1),AL2(ESTESTK-T217FFD)                    
LPF08X   EQU  *                                                                 
*                                                                               
*        ESTIMATE DOLLAR                                                        
         DC   AL1(MPF09X-*,09,PFTCPROG,(MPF09X-MPF09)/KEYLNQ,0)                 
         DC   CL3'ED '                 LIST                                     
         DC   CL8'ESTD'                RECORD                                   
         DC   CL8'DISP'                ACTION                                   
MPF09    DC   AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTCLIK-1),AL2(ESTCLIK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTPRDK-1),AL2(ESTPRDK-T217FFD)                    
         DC   AL1(KEYTYTWA,L'ESTESTK-1),AL2(ESTESTK-T217FFD)                    
MPF09X   EQU  *                                                                 
*                                                                               
*        RETURN CALLER                                                          
         DC    AL1(LPF12X-*,12,PFTRPROG,0,0)                                    
         DC    CL3' ',CL8' ',CL8' '                                             
LPF12X   EQU   *                                                                
         DC    X'FF'                                                            
*                                                                               
*                                                                               
CHKTYPE  NTR1  BASE=*,LABEL=*                                                   
         L     R1,AIO                                                           
         MVC   CHKSVKEY,0(R1)                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(4),CHKSVKEY                                                  
*                                                                               
CHKT05   GOTO1 HIGH                                                             
         B     CHKT20                                                           
CHKT10   GOTO1 SEQ                                                              
CHKT20   CLC   KEYSAVE(4),KEY      SAME A/M, CLT?                               
         BNE   CHKT100                                                          
*                                                                               
         CLC   CHKSVKEY+7(1),KEY+7  SAME ESTIMATE?                              
         BE    *+12                                                             
         MVI   KEY+8,X'FF'                                                      
         B     CHKT05              NO - READ NEXT EST RECORD                    
*                                                                               
         CLC   =5X'00',KEY+8                                                    
         BE    *+12                                                             
         MVI   KEY+8,X'FF'                                                      
         B     CHKT05              NO - READ NEXT EST RECORD                    
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R2,AIO                                                           
         USING ESTHDR,R2                                                        
*                                                                               
         LA    R0,EPAIDX                                                        
         LA    R2,EPAID                                                         
CHKT40   CR    R0,R2                                                            
         BH    *+12                                                             
         MVI   KEY+8,X'FF'                                                      
         B     CHKT05                                                           
*                                                                               
         CLC   =XL6'00',0(R2)                                                   
         BE    *+14                                                             
         CP    0(L'EPAID,R2),=PL6'0'                                            
         BNE   CHKTXNEQ            ERROR EXIT, IF NONZERO                       
         LA    R2,L'EPAID(R2)                                                   
         B     CHKT40                                                           
*                                                                               
CHKT100  DS    0H                  CHECK IF BILL HEADERS EXIST                  
         XC    KEY,KEY                                                          
         MVC   KEY(4),CHKSVKEY                                                  
*                                                                               
CHKT105  GOTO1 HIGH                                                             
         B     CHKT120                                                          
CHKT110  GOTO1 SEQ                                                              
CHKT120  CLC   KEYSAVE(4),KEY      SAME A/M, CLT?                               
         BNE   CHKTXEQ             NO - NO BILL HEADERS FOUND                   
*                                                                               
         CLC   CHKSVKEY+7(1),KEY+7  SAME ESTIMATE?                              
         BE    *+12                                                             
         MVI   KEY+8,X'FF'         READ TO NEXT EST                             
         B     CHKT105                                                          
*                                                                               
         CLC   KEY+8(5),=5X'00'    NULLS ABOVE ESTIMATE?                        
         BE    CHKT110             YES - NOT A BILLING RECORD - SKIP            
*                                                                               
         B     CHKTXNEQ                                                         
*                                                                               
         DROP  R2                                                               
CHKTXEQ  MVI   EXITBYTE,C'E'                                                    
         B     *+8                                                              
CHKTXNEQ MVI   EXITBYTE,C'N'                                                    
*                                                                               
         MVC   KEY,CHKSVKEY         RESTORE KEY                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(13),KEY                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
*                                                                               
         CLI   EXITBYTE,C'E'                                                    
         JE    EQXIT                                                            
         J     NEQXIT                                                           
*                                                                               
CHKSVKEY DS    XL13                                                             
EXITBYTE DS    X                   E=EQXIT,N=NEQXIT                             
         LTORG                                                                  
***********************************************************************         
* ADD THE PASSIVE POINTER                                             *         
***********************************************************************         
                                                                                
XADD     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R2,MEDTAB                                                        
XADD00   L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)        READ THE KEY WE JUST ADDED/CHANGED          
*                                                                               
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   XADD01               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XADD01               NO, ONLY CHANGE 1 RECORD                    
         NI    KEY+1,X'F0'          TURN OFF MEDIA BIT                          
         OC    KEY+1(1),1(R2)       USE THIS MEDIA                              
*                                                                               
XADD01   GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      WE BETTER HAVE IT!!!                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   WORK(4),KEY+14       SAVE THE D/A                                
*                                                                               
         USING ESTHDR,R6                                                        
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING EPKEY,R3                                                         
*                                                                               
         MVI   EPKEYTYP,EPKEYTYQ    X'0D'                                       
         MVI   EPKEYSUB,EPKEYSBQ    X'F2'                                       
         MVC   EPKEYAM,KEYSAVE+1    A/M                                         
         MVC   EPKEYCLT,EKEYCLT     CLIENT                                      
         GOTO1 DATCON,DMCB,(0,ESTART),(2,EPKEYSDT)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,EPKEYEDT)                                
         MVC   EPKEYEST,EKEYEST     EST                                         
         MVC   EPKEYPRD,EKEYPRD     PRD                                         
         MVC   KEY+14(4),WORK       SET DISK ADDRESS                            
*                                                                               
         OI    DMINBTS,X'08'        PASS DELETES                                
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         BNE   XADD10               NO                                          
         CLC   KEY+14(4),KEYSAVE+14   TEST SAME DISK ADDRESS                    
         BE    XADD20                 YES -                                     
*                                                                               
         MVC   KEY,KEYSAVE          RESTORE KEY WITH NEW DISK ADDR              
         NI    KEY+13,X'7F'         YES, UNDELETE                               
         MVC   AIO,AIO2             DO NOT CLOBBER AIO2                         
         GOTO1 WRITE                AND WRITE BACK                              
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
         B     XADD20                                                           
*                                                                               
XADD10   MVC   KEY,KEYSAVE          RESTORE KEY                                 
         MVC   AIO,AIO2             DO NOT CLOBBER AIO2                         
         GOTO1 ADD                                                              
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XADD20   CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   XADDX                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XADDX                NO, WE ARE DONE HERE                        
*                                                                               
XADD30   LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    XADDX                YES                                         
         CLI   SVCXTRA+8,C'P'       TEST P&G                                    
         BE    *+12                 YES - NO MEDIA N!                           
         CLI   0(R2),C'N'           PROCESS MEDIA N?                            
         BE    XADD00               YES                                         
         CLI   0(R2),C'C'           PROCESS MEDIA C?                            
         BE    XADD00               YES                                         
         B     XADD30                                                           
*                                                                               
XADDX    J     XIT                                                              
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*====================================================================           
* ESTIMATE RECORD WAS JUST CHANGED...IF DATES DID NOT CHANGE JUST               
* WRITE THE KEY BACK IF DISK ADDRESS HAS CHANGED                                
* OTHERWISE, DELETE THE OLD PASSIVE AND ADD A NEW ONE                           
*====================================================================           
                                                                                
XCHG     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LAY   R2,MEDTAB                                                        
         XC    KEY,KEY                                                          
*                                                                               
XCHG00   L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         CLI   NEWLEN,C'Y'         DID WE MAKE ESTHDR BIGGER                    
         JE    XCHG05              YES                                          
*                                                                               
         CLC   SVSTRT,ESTART        DID ESTIMATE START CHANGE?                  
         BNE   XCHG05               YES IT DID                                  
         CLC   SVEND,EEND           DID ESTIMATE END CHANGE?                    
         BE    XCHGX                NO, LEAVE THE PASSIVE ALONE                 
*                                                                               
XCHG05   LA    R3,KEY                                                           
         USING EPKEY,R3                                                         
         MVI   EPKEYTYP,EPKEYTYQ    X'0D'                                       
         MVI   EPKEYSUB,EPKEYSBQ    X'F2'                                       
         MVC   EPKEYAM,EKEYAM       A/M                                         
*                                                                               
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   XCHG10               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XCHG10               NO, ONLY CHANGE 1 RECORD                    
         NI    EPKEYAM,X'F0'        TURN OFF MEDIA BIT                          
         OC    EPKEYAM,1(R2)        USE THIS MEDIA                              
*                                                                               
XCHG10   MVC   EPKEYCLT,EKEYCLT     CLT                                         
         GOTO1 DATCON,DMCB,(0,SVSTRT),(2,EPKEYSDT)                              
         GOTO1 DATCON,DMCB,(0,SVEND),(2,EPKEYEDT)                               
         MVC   EPKEYEST,EKEYEST     EST                                         
         MVC   EPKEYPRD,EKEYPRD     PRD                                         
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         BNE   XCHG15               NO                                          
         CLI   NEWLEN,C'Y'          TEST CHANGE OF LEN                          
         BE    XCHG30                                                           
*                                                                               
         OI    KEY+13,X'80'         MARK RECORD FOR DELETION                    
         MVC   AIO,AIO2             DO NOT CLOBBER AIO2                         
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
*                                                                               
XCHG15   CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   XCHG30               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XCHG30               NO, ONLY CHANGE 1 RECORD                    
*                                                                               
XCHG20   LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    XCHG30               YES                                         
         CLI   SVCXTRA+8,C'P'       TEST P&G                                    
         BE    *+12                 YES - NO MEDIA N!                           
         CLI   0(R2),C'N'           PROCESS MEDIA N?                            
         BE    XCHG00               YES                                         
         CLI   0(R2),C'C'           PROCESS MEDIA C?                            
         BE    XCHG00               YES                                         
         B     XCHG20                                                           
*                                                                               
XCHG30   BRAS  RE,XADD              GO AND ADD NEW PASSIVE                      
*                                                                               
         L     R6,AIO                                                           
         CLC   =C'POL',EKEYPRD      POL ESTIMATE?                               
         BNE   XCHGX                NO, DONE                                    
         MVC   ESTKEY,0(R6)         SAVE THE POL KEY                            
         XC    KEY,KEY                                                          
         MVC   KEY(4),0(R6)         START READING WITH FIRST PRODUCT            
*                                                                               
XCHG31   GOTO1 =A(NEXTPRD),RR=RELO  HAVE NEXT PRD/EST?                          
         BNE   XCHGX                NO, DONE                                    
         MVC   SAVEKEY,KEY          SAVE OFF THIS KEY                           
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         BAS   RE,XCHG              RECURSIVE CALL!                             
         MVC   KEY(13),SAVEKEY      ESTIMATE RECORD WE JUST PROCESSED           
         B     XCHG31               AND READ THE NEXT ESTIMATE                  
*                                                                               
XCHGX    J     XIT                                                              
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
*                                                                               
         USING ESTHDR,R3                                                        
DISPDEM  NTR1  BASE=*,LABEL=*                                                   
         OC    EDEMLST(3),EDEMLST                                               
         JZ    XIT                                                              
* BUILD DBLOCK IN ELEM                                                          
         BRAS  RE,SETDBLOCK                                                     
         L     RF,DMCB             GET DEMOCON ADDRESS                          
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(224),BLOCK+255                                         
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JH    *+10                                                             
         XC    ENONTDMS,ENONTDMS                                                
         GOTO1 (RF),(R1),(20,EDEMLST),(13,BLOCK),(C'S',ELEM),EUSRNMS,  *        
               ENONTDMS                                                         
*                                                                               
DR115    LA    R2,EDEMLST                                                       
         LA    RF,BLOCK                                                         
*                                                                               
         LA    R5,ESTDEMSH         FIRST DEMO FLDHDR                            
         ST    R5,SVDEMADR                                                      
         LA    R5,8(R5)            FIRST OUTPUT POSITION                        
         LA    R6,L'ESTDEMS(R5)                                                 
*                                                                               
DR120    CLI   0(RF),C' '                                                       
         BNH   DR140                 IF NOT THE LAST DEMO ...                   
         BRAS  RE,FMTDEMO            CALL FMTDEMO                               
*                                                                               
         LLC   R1,WORK               LENGTH OF DEMO RETURNED IN WORK            
         LR    R0,R5                 SEE IF IT WILL FIT ON THIS LINE            
         AR    R0,R1                                                            
         CR    R0,R6                                                            
         BNH   DR130                                                            
*                                                                               
         BCTR  R5,0                  WILL NOT FIT ... PUT IT ON NEXT            
         CLI   0(R5),C','            LINE AND BLANK OUT LAST COMMA              
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
*                                                                               
         L     R5,SVDEMADR         GET FLDHDR ADDRESS                           
         SR    R0,R0                                                            
         IC    R0,0(R5)            POINT TO 'DEMOS' TITLE                       
         AR    R5,R0                                                            
         IC    R0,0(R5)                                                         
         AR    R5,R0               POINT TO NEXT DEMO FIELD                     
         ST    R5,SVDEMADR                                                      
         LA    R5,8(R5)            FIRST OUTPUT POSITION                        
         LA    R6,L'ESTDEMS(R5)    R6 HAS END ADDRESS                           
*                                                                               
DR130    BCTR  R1,0                  DEMO RETURNED IN WORK+1 ... COPY           
         EX    R1,*+8                IT TO SCREEN                               
         B     *+10                                                             
         MVC   0(0,R5),WORK+1                                                   
         AR    R5,R1                 IF NOT AT END OF LINE, MOVE COMMA          
         LA    R5,1(R5)              TO THE END OF COPIED DEMO                  
         CR    R5,R6                                                            
         BNL   *+8                                                              
         MVI   0(R5),C','                                                       
*                                                                               
         LA    R5,1(R5)              BUMP UP OUTPUT LINE, BUMP TO               
         LA    RF,11(RF)             NEXT DEMO  IN BLOCK AND RECORD             
         LA    R2,3(R2)              BUMP TO NEXT DEMO IN RECORD                
         LA    R0,EDEMLST+L'EDEMLST-1                                           
         CR    R2,R0                                                            
         BNH   DR120                                                            
*                                                                               
DR140    BCTR  R5,0                  LAST DEMO HAS BEEN OUTPUTTED ...           
         CLI   0(R5),C','            ELIMINATE LAST COMMA                       
         BNE   DR150                                                            
         MVI   0(R5),C' '                                                       
*                                                                               
DR150    J     XIT                                                              
         DROP  R3                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* THIS CODE WILL DELETE THE DIR/FIL AND PASSIVE                       *         
***********************************************************************         
DEL      NTR1  BASE=*,LABEL=*                                                   
         LAY   R2,MEDTAB                                                        
         MVC   SVAIO,AIO                                                        
*                                                                               
DEL00    L     R6,AIO1              RIGHT ESTIMATE IS IN AIO1!!                 
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)                                                    
         USING EKEY,R3                                                          
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   DEL01                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   DEL01                NO, ONLY CHANGE 1 RECORD                    
         NI    EKEYAM,X'F0'         TURN OFF MEDIA BIT                          
         OC    EKEYAM,1(R2)         USE THIS MEDIA                              
         DROP  R3                                                               
*                                                                               
DEL01    MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3             USE THIS AIO AREA (1 & 2 ARE TAKEN)         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         OI    15(R6),X'80'         MARK RECORD FOR DELETION                    
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      DELETING THE RIGHT KEY?                     
         BE    *+6                  YES                                         
         DC    H'0'                 NO, GO BACK AND RESTORE THE FIL!            
         OI    KEY+13,X'80'         MARK KEY FOR DELETION                       
         GOTO1 WRITE                                                            
         MVC   AIO,SVAIO            RESTORE AIO                                 
*                                                                               
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   DEL05                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   DEL05                NO, ONLY CHANGE 1 RECORD                    
*                                                                               
DEL02    LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    DEL03                YES                                         
         CLI   SVCXTRA+8,C'P'       TEST P&G                                    
         BE    *+12                 YES - NO MEDIA N!                           
         CLI   0(R2),C'N'           PROCESS MEDIA N?                            
         BE    DEL00                YES                                         
         CLI   0(R2),C'C'           PROCESS MEDIA C?                            
         BE    DEL00                YES                                         
         B     DEL02                                                            
*                                                                               
DEL03    LAY   R2,MEDTAB                                                        
***                                                                             
* NOW DELETE PASSIVE KEY(S) IF IT EXISTS                                        
***                                                                             
DEL05    L     R6,AIO1                                                          
         USING ESTHDR,R6                                                        
         LA    R3,KEY                                                           
         XC    KEY,KEY                                                          
         USING EPKEY,R3                                                         
         MVI   EPKEYTYP,EPKEYTYQ    X'0D'                                       
         MVI   EPKEYSUB,EPKEYSBQ    X'F2'                                       
         MVC   EPKEYAM,EKEYAM       A/M                                         
*                                                                               
         CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   DEL10                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   DEL10                NO, ONLY CHANGE 1 RECORD                    
         NI    EPKEYAM,X'F0'        TURN OFF MEDIA BIT                          
         OC    EPKEYAM,1(R2)        USE THIS MEDIA                              
*                                                                               
DEL10    MVC   EPKEYCLT,EKEYCLT     CLT                                         
         GOTO1 DATCON,DMCB,(0,SVSTRT),(2,EPKEYSDT)                              
         GOTO1 DATCON,DMCB,(0,SVEND),(2,EPKEYEDT)                               
         MVC   EPKEYEST,EKEYEST     EST                                         
         MVC   EPKEYPRD,EKEYPRD     PRD                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         BNE   DEL15                NO                                          
         OI    KEY+13,X'80'         MARK RECORD FOR DELETION                    
         GOTO1 WRITE                                                            
*                                                                               
DEL15    CLI   SVAPROF+7,C'C'       CANADIAN?                                   
         BNE   DELX                 NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   DELX                 NO, ONLY CHANGE 1 RECORD                    
*                                                                               
DEL20    LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    DELX                 YES                                         
         CLI   SVCXTRA+8,C'P'       TEST P&G                                    
         BE    *+12                 YES - NO MEDIA C OR N                       
         CLI   0(R2),C'N'           PROCESS MEDIA N?                            
         BE    DEL05                YES                                         
         CLI   0(R2),C'C'           PROCESS MEDIA C?                            
         BE    DEL05                YES                                         
         B     DEL20                                                            
*                                                                               
DELX     J     XIT                                                              
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* SEE IF NEED TO ADD 0D29 AND 0D2A SEQNUM RECORDS FOR NONT DEMOS                
* SINCE ALL DEMOS MUST BE IN POL ESTHDR, ONLY  DO THIS FOR POL EST              
*===================================================================            
                                                                                
ADDNONT  NTR1  BASE=*,LABEL=*                                                   
         L     R3,AIO1                                                          
         USING ESTHDR,R3                                                        
*                                                                               
         CLC   EKEYPRD,=C'POL'                                                  
         JNE   XIT                                                              
*                                                                               
         CLC   ENONTDMS(20*L'ENONTDMS),POLNTRDM   ANY NEW NONT DEMOS ?          
         JE    XIT                 NO                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NTSQKEY,R6                                                       
         MVI   NTSQKTYP,NTSQKTYPQ  0D                                           
         MVI   NTSQKSUB,NTSQKSUBQ  2A                                           
         MVI   NTSQRTGSV,C'C'                                                   
         MVC   NTSQKAGMD,EKEY+1                                                 
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         JE    *+10                                                             
         MVC   NTSQKSEQ,=X'FF00'   SET COMPLEMENT OF 256                        
*                                                                               
         MVC   HALF,NTSQKSEQ                                                    
         XC    HALF,=X'FFFF'       GIVES CURRENT HIGH SEQNUM                    
                                                                                
* NOW SEE IF EACH OF THE DEMOS IS ALREADY ON FILE                               
                                                                                
         LA    R5,ENONTDMS                                                      
         LA    R0,20                                                            
*                                                                               
ADDNONT2 CLI   0(R5),0                                                          
         JE    ADDNONT6                                                         
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING NTDKEY,R6                                                        
*                                                                               
         MVI   NTDKTYP,NTDKTYPQ    0D                                           
         MVI   NTDKSUB,NTDKSUBQ    29                                           
         MVI   NTDKRTGSV,C'C'      FOR NOW, ONLY COMSCORE                       
         MVC   NTDKAGMD,EKEY+1     AGY/MD                                       
*                                                                               
         LA    R1,0(R5)                                                         
         CLI   0(R1),C'X'                                                       
         JNE   *+12                                                             
         LA    R1,1(R1)                                                         
         J     ADDNONT4                                                         
         CLC   =C'RX',0(R1)                                                     
         JNE   *+8                                                              
         LA    R1,2(R1)                                                         
*                                                                               
ADDNONT4 MVC   NTDKALPH(6),0(R1)   MOVE ALPHA DEMO W/O X OR RX                  
         MVC   FAKEFLD,0(R1)       SAVE ACTUAL ALPHA                            
*                                                                               
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(11),KEYSAVE     COMPARE THRU ALPHA DEMO                      
         JE    ADDNONT6                                                         
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE KEY                                  
         SR    RE,RE                                                            
         ICM   RE,3,HALF                                                        
         LA    RE,1(RE)                                                         
         STH   RE,HALF                                                          
         STCM  RE,3,NTDKSEQ        DO NOT COMPLEMENT 0D29 SEQNUM                
*                                                                               
         MVC   KEY+14(3),=X'FFFFFF'  SET DIRECTORY ONLY FLAG                    
         GOTO1 ADD                   ADD NEW 0D29 POINTER                       
*                                                                               
         XC    KEY,KEY             UPDATE HIGH SEQNUM POINTER                   
         LA    R6,KEY                                                           
         USING NTSQKEY,R6                                                       
         MVI   NTSQKTYP,NTSQKTYPQ  0D                                           
         MVI   NTSQKSUB,NTSQKSUBQ  2A                                           
         MVI   NTSQRTGSV,C'C'                                                   
         MVC   NTSQKAGMD,EKEY+1                                                 
*                                                                               
         MVC   NTSQKSEQ,HALF                                                    
         XC    NTSQKSEQ,=X'FFFF'                                                
*                                                                               
         MVC   NTSQKALPH(6),FAKEFLD                                             
         MVC   KEY+14(3),=X'FFFFFF'  SET DIRECTORY ONLY FLAG                    
         GOTO1 ADD                                                              
         DROP  R6                                                               
*                                                                               
ADDNONT6 LA    R5,8(R5)                                                         
         JCT   R0,ADDNONT2                                                      
         J     EQXIT                                                            
         LTORG                                                                  
         EJECT                                                                  
*=================================================================              
* SET UP DBLOCK TO CALL DEMOCON                                                 
*=================================================================              
                                                                                
SETDBLOCK NTR1 BASE=*,LABEL=*                                                   
         XC    ELEM,ELEM             DEMOS EXIST ...                            
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO DEMOCON                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TP '        SET DBFILE = NAD FOR NETWORK               
         CLI   OVSYS,3               SET DBFILE = TP  OTHERWISE                 
         BNE   *+10                                                             
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'                                                        
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   SETD10                AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    SETD10                                                           
         MVI   DBSELMED,C'C'                                                    
SETD10   MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 CALLOV,DMCB           CALL DEMOCON                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         XIT1                                                                   
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
PRODUCT DSECT                                                                   
       ++INCLUDE SPGENPRD          PRODUCT RECORD                               
         EJECT                                                                  
CLIENT  DSECT                                                                   
       ++INCLUDE SPGENCLT          CLIENT  RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENAGY          AGENCY  RECORD                               
         EJECT                                                                  
       ++INCLUDE SPGENEST          <----  E RECORDS                             
         EJECT                                                                  
       ++INCLUDE SPGENBILL         BILLING RECORDS                              
         EJECT                                                                  
       ++INCLUDE SPGENESTD         PASSIVE ESTIMATE KEY DSECT                   
         EJECT                                                                  
       ++INCLUDE SPGENNTDEM        NONT DEMO POINTERS                           
         EJECT                                                                  
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
       ++INCLUDE SCSFM72D          EST MAINTENANCE SCREEN                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
       ++INCLUDE DDPSTBLK          PROVINCIAL TAX VALIDATION                    
         EJECT                                                                  
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
         EJECT                                                                  
       ++INCLUDE DEDEMTABD                                                      
         EJECT                                                                  
       ++INCLUDE FAFACTS                                                        
         EJECT                                                                  
       ++INCLUDE DDPARSNIPD                                                     
         EJECT                                                                  
       ++INCLUDE DDFLDIND                                                       
         EJECT                                                                  
       ++INCLUDE SPGENAPY          AUTOPAY RECORD DSECT                         
         EJECT                                                                  
       ++INCLUDE DDSCANBLKD        FOR SCANNER                                  
         EJECT                                                                  
       ++INCLUDE DDOFFICED                                                      
         EJECT                                                                  
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
         EJECT                                                                  
       ++INCLUDE DEDEMOVALD                                                     
*                                                                               
SCAND    DSECT                       DSECT TO COVER SCANNER LINES               
FLD1LEN  DS    CL1                                                              
FLD2LEN  DS    CL1                                                              
FLD1VAL  DS    CL1                                                              
FLD2VAL  DS    CL1                                                              
FLD1B    DS    CL4                                                              
FLD2B    DS    CL4                                                              
FLD1     DS    CL10                                                             
FLD2     DS    CL10                                                             
*                                                                               
BSCAND   DSECT                     SCANNER LIKE OUTPUT                          
BFLD1LEN DS    CL1                                                              
BFLD2LEN DS    CL1                                                              
BFLD1VAL DS    CL1                                                              
BFLD2VAL DS    CL1                                                              
BFLD1B   DS    CL4                                                              
BFLD2B   DS    CL4                                                              
BFLD1    DS    CL11                EXCEPT FLD 1 = LEN 11                        
BFLD2    DS    CL10                                                             
BSCANLNQ EQU   *-BSCAND                                                         
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
PROKEY   DS    CL13                                                             
ESTKEY   DS    CL13                                                             
SAVEKEY  DS    XL13                                                             
CANKEY   DS    CL13                PASS KEY FOR UPDATING CANADIAN RECS          
CANAIO   DS    A                   IO FOR T RECORD TO EXPLODE TO N+C            
SVAIO    DS    A                                                                
WTEND    DS    A                                                                
*                                                                               
CASHPRD  DS    CL3                                                              
OLDCASH  DS    CL3                                                              
OLDCPRD  DS    XL1                                                              
*                                                                               
SVPRD    DS    X                                                                
SVPOLPW  DS    XL3                                                              
SVPOLC2  DS    XL4                                                              
*                                                                               
SVCLTPOL DS    CL1                                                              
SVCLPROF DS    CL15                                                             
SVCLEX   DS    CL15                                                             
SVCLDLY  DS    CL1                                                              
SVCLTPW  DS    XL3                                                              
SVCCOST2 DS    XL4                                                              
SVCC2CON DS    XL2                                                              
SVCOFF   DS    CL1                                                              
SETCONV  DS    CL1                                                              
WESTERN  DS    CL1                                                              
*                                                                               
SVCLTDA  DS    XL4                                                              
SVPRDDA  DS    XL4                                                              
SVAGYFL1 DS    XL1                                                              
FADDR    DS    A                                                                
ERRAREA  DS    X                                                                
SVE1USER DS    CL20                                                             
SVEU1TYP DS    CL1                                                              
SVEU1LEN DS    XL1                                                              
SVEU1FL1 DS    XL1                                                              
SVEU1FL2 DS    XL1                                                              
*                                                                               
SVE2USER DS    CL20                                                             
SVEU2TYP DS    CL1                                                              
SVEU2LEN DS    XL1                                                              
SVEU2FL1 DS    XL1                                                              
SVEU2FL2 DS    XL1                                                              
*                                                                               
SVCLOP1  DS    XL1                                                              
SVCLOP2  DS    XL1                                                              
SVCLOP3  DS    XL1                                                              
SVCLOP4  DS    XL1                                                              
SVF0PROF DS    CL16                                                             
*                                                                               
SVPPST   DS    CL10                                                             
*                                                                               
SV00PROF DS    CL16                                                             
*                                                                               
SCRNFLAG DS    X                                                                
AUSR     DS    A                                                                
PSTOUT   DS    CL64                                                             
NEWLEN   DS    CL1                 C'Y'=ESTHDR LENGTH EXTENDED                  
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
USERDATA DS    CL32                                                             
*                                                                               
SVADVLST DS    CL30                                                             
FAKEFLD  DS    XL11                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
WORK2    DS    CL48                                                             
SVADVDA  DS    CL4                                                              
SVADVAGY DS    XL1                                                              
SVADVKEY DS    XL13                                                             
BYTE2    DS    XL1                                                              
CCOUNT   DS    XL1                                                              
SONIA    DS    CL1                                                              
SYR      DS    CL2                                                              
SMN      DS    CL2                                                              
SDY      DS    CL2                                                              
EYR      DS    CL2                                                              
EMN      DS    CL2                                                              
EDY      DS    CL2                                                              
MAKEGDDT DS    CL2                                                              
WTSW     DS    CL1                                                              
UDSW     DS    CL1                 SET TO X'01' IF USER DEMOS USED              
HMSW     DS    CL1                 SET TO X'01' IF TOTAL HMS INPUT              
NHMSW    DS    CL1                 SET TO X'01' IF A DEMO OTHER THAN            
*                                  64 OR 3 IS INPUT                             
SVTRDPRD DS    XL1                                                              
POLCHG   DS    CL1                                                              
OVSYS    DS    CL1                                                              
LINID    DS    CL4                                                              
LINADDR  DS    CL4                                                              
OWSDAY   DS    CL1                                                              
CLIPRO   DS    CL10                                                             
SVDEMADR DS    A                                                                
*                                                                               
SVESTIM  DS    0CL162                                                           
SVCNTRL  DS    XL1                                                              
SVSTRT   DS    CL6                                                              
SVEND    DS    CL6                                                              
SVELOCK  DS    XL2                                                              
SVOWS    DS    XL1                                                              
SVECOST2 DS    F                                                                
SVDEMOS  DS    0CL124                                                           
SVDEMLST DS    CL60                                                             
SVWGTLST DS    XL20                                                             
SVUSRNMS DS    CL28                                                             
SVWGTNM  DS    CL7                                                              
         DS    CL9                                                              
         DS    CL1                (EOWSDAY IN EDEMOS+124)                       
         DS    CL1                (ERATE NOW IN EDEMOS+125)                     
SVBOOK   DS    CL2                                                              
SVHUT    DS    CL1                                                              
SVDPT    DS    CL1                                                              
SVETYPE  DS    CL1                                                              
SVPWPCT  DS    XL3                                                              
SVCOST2  DS    XL4                                                              
SVECON   DS    CL1                 ECONTROL                                     
SVEMGD   DS    CL2                                                              
SVCPP    DS    CL1                                                              
SVFLTRS  DS    CL3                 FILTERS                                      
SVRTL    DS    CL2                 RETAIL SCHEME                                
*                                                                               
SVNETYM  DS    XL2                                                              
SVBDSTM  DS    XL3                                                              
SVBDENDM DS    XL3                                                              
SVDATAX  EQU   *                                                                
*                                                                               
POLDATA  DS    0D                                                               
POLSTRT  DS    CL6                                                              
POLEND   DS    CL6                                                              
POLOWDAY DS    XL1                                                              
POLDEMOS DS    0CL124                                                           
PEDEMLST DS    CL60                CONVERTED DEMO FIELDS                        
PEWGTLST DS    XL20                                                             
PEUSRNMS DS    CL28                                                             
PEWGTNM  DS    CL7                                                              
         DS    CL9                                                              
         DS    CL1                (EOWSDAY IN EDEMOS+124)                       
         DS    CL1                (ERATE NOW IN EDEMOS+125)                     
*                                                                               
POLBOOK  DS    CL2                                                              
POLHUT   DS    CL1                                                              
POLDPT   DS    CL1                                                              
POLETYPE DS    CL1                                                              
POLCON   DS    XL1                                                              
POLSW    DS    CL1                 SET TO X'01' IF POL HDR EXISTS               
*                                  SET TO X'02' IF CHGING POL                   
POLDATE  DS    CL12                FOR CPP POL ESTS                             
POLTYPE  DS    CL1                 FOR CPP POL ESTS                             
POLFLTRS DS    CL3                                                              
POLRTL   DS    CL2                 RETAIL SCHEME NEW 9/18/92                    
POLFLAG1 DS    XL1                 EFLAG1                                       
*                                                                               
POLNTRDM DS    CL160               CHAR OF NON-TRAD DEMO CATS (20*8)            
POLDATAX EQU   *                                                                
         DS    CL100               SPARE                                        
*                                                                               
SVDLY    DS    CL1                 ORIGINAL DAILY FLAG                          
NEWDLY   DS    CL1                 DAILY FLAG AFTER VALIDATION                  
POLDLY   DS    CL1                 POL DAILY FLAG                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122SPSFM52   11/04/20'                                      
         END                                                                    
