*          DATA SET NESFM03    AT LEVEL 095 AS OF 09/02/20                      
*PHASE T31C03B                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C03  -- ESTIMATE MAINTENANCE                      *         
*                                                                     *         
*  COMMENTS:     MAINTAINS ESTIMATE RECORDS                           *         
*                                                                     *         
*  CALLED FROM:  SFM CONTROLLER (T31C00), WHICH CALLS                 *         
*                GEGENCON (T00A30), WHICH CALLS THIS.                 *         
*                                                                     *         
*  INPUTS:       SCREEN NESFMBE (MAINT)                               *         
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
         TITLE 'T31C03 - ESTIMATE MAINTENANCE'                                  
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* AKAT 092 15OCT19 SPEC-38512   FCONTROL SUPPORT                      *         
* JSAY 091 16APR18 <SPEC-22503> CREATE CLEAR ERROR MESSAGES FOR API   *         
***********************************************************************         
T31C03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C03**,R7,RR=R3                                              
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
         GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
*                                                                               
         BRAS  RE,SETUP                                                         
         BRAS  RE,CHKPFKEY                                                      
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,XRECADD        RECORD JUST ADDED?                           
         BNE   *+8                                                              
         BRAS  RE,XADD             YES - ADD PASSIVE                            
         CLI   MODE,XRECPUT        RECORD JUST CHANGED?                         
         BNE   *+8                                                              
         BRAS  RE,XCHG             YES - CHANGE/ADD PASSIVE AS NEEDED           
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
MAXPRD   EQU   252                                                              
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
***********************************************************************         
*                                                                               
         LA    R2,ESTMEDKH         MEDIA                                        
         GOTO1 VALIMED             VALIDATE MEDIA CODE AND TRANSMIT             
         MVC   ESTMEDN,MEDNM       MEDIA NAME                                   
         OI    ESTMEDNH+6,X'80'                                                 
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
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
*                                                                               
         MVC   ESTCLIN,CLTNM         CLIENT NAME                                
         OI    ESTCLINH+6,X'80'                                                 
*                                                                               
         L     RE,AIO2                                                          
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
         MVC   SVCLTPOL,CPOLONLY                                                
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         MVC   SVCLDLY,CDAILY                                                   
         MVC   SVCLTPW,CPWPCT                                                   
         MVC   SVCCOST2,CCOST2                                                  
         DROP  RE                                                               
*                                                                               
         MVC   AIO,AIO1                                                         
*                                                                               
         MVC   ESTDSC1,SVE1USER       DESCRIPTION                               
         OI    ESTDSC1H+6,X'80'                                                 
         OI    ESTUSR1H+1,X'20'       INPUT FIELD                               
         OI    ESTUSR1H+6,X'80'                                                 
         CLC   SVE1USER,SPACES                                                  
         BNH   *+8                                                              
         NI    ESTUSR1H+1,X'FF'-X'20' UNPROTECT                                 
         MVC   ESTDSC2,SVE2USER                                                 
         OI    ESTDSC2H+6,X'80'                                                 
         OI    ESTUSR2H+1,X'20'       INPUT FIELD                               
         OI    ESTUSR2H+6,X'80'                                                 
         CLC   SVE2USER,SPACES                                                  
         BNH   *+8                                                              
         NI    ESTUSR2H+1,X'FF'-X'20' UNPROTECT                                 
*                                                                               
         MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTPRDKH           PRODUCT                                    
         GOTO1 VALIPRD                                                          
         MVC   ESTPRDN,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    ESTPRDNH+6,X'80'      PRODUCT NAME                               
*                                                                               
         L     RE,AIO                SAVE PRODUCT RECORD DATA                   
         USING PRDHDR,RE                                                        
         MVC   SVPRD,PCODE+1                                                    
         DROP  RE                                                               
*                                                                               
         CLC   CTAGYSV,=C'CK'        FOR AGENCY CK...                           
         BNE   VK10                  NO ADD OF PRODUCT TO CLIENTS               
         CLC   QCLT,=C'CC '          OTHER THAN CC                              
         BE    VK10                                                             
         MVC   ERRNUM,=AL2(VKERR4)                                              
         CLI   ACTNUM,ACTADD                                                    
         BE    SPERREX                                                          
*                                                                               
VK10     MVC   EKEYPRD,QPRD          COPY PRODUCT INTO KEY                      
         OC    EKEYPRD,SPACES                                                   
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTESTKH           ESTIMATE                                   
*                                                                               
         CLI   5(R2),0               DK ACTED AS INPUT TRANSLATOR AND           
         BE    ERRMIS                SET EST HEADER FIELD                       
*                                                                               
*        CLI   ACTNUM,ACTDIS                                                    
*        BNE   VK15                                                             
         BAS   RE,SPTOZER            CHANGE SPACES TO 0'S                       
*                                                                               
VK15     CLI   ACTNUM,ACTADD                                                    
         BE    VK20                                                             
         GOTO1 VALIEST               ACTION IS NOT ADD SO...                    
         MVC   ESTDESC,ESTNAME       VALIDATE ESTIMATE CODE AND                 
         OI    ESTDESCH+6,X'80'      TRANSMIT ESTIMATE DESCRIPTION              
*                                                                               
         L     R3,AIO                                                           
         MVC   SVECOST2,ECOST2       SAVE CLIENT RECORD DATA                    
         LA    R3,KEY                                                           
*                                                                               
         B     VK25                                                             
*                                                                               
VK20     MVC   ERRNUM,=AL2(ESTERR1)  ESTIMATE CODE MUST BE NUMERIC              
         TM    ESTESTKH+4,X'08'      AND HAVE A LENGTH <=3                      
         BZ    SPERREX                                                          
*                                                                               
VK25     ZIC   RE,5(R2)              CONVERT ESTIMATE CODE TO BINARY            
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RE,DUB                                                           
         MVC   ERRNUM,=AL2(ESTERR2)  EST MUST BE BETWEEN 1 - 255                
         CHI   RE,1                                                             
         BL    SPERREX                                                          
         CHI   RE,255                                                           
         BH    SPERREX                                                          
         STC   RE,BEST               STORE BINARY ESTIMATE                      
*                                                                               
         MVC   EKEYEST,BEST          SAVE ESTIMATE CODE INTO KEY                
         MVC   ESTKEY,KEY            SAVE ESTIMATE RECORD KEY                   
*                                                                               
**********************************************************************          
*                                                                               
         XC    SVPOLPW,SVPOLPW                                                  
         MVC   KEY(13),ESTKEY         READ ESTIMATE RECORD FOR POL              
         MVC   KEY+4(3),=C'POL'       PRODUCT                                   
         GOTO1 HIGH                                                             
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK60                                                             
*                                                                               
         CLI   SVCLTPOL,C'Y'          SOME CLIENTS MUST ADD POL EST             
         BE    VK30                   BEFORE ADDING BRAND ESTIMATES!            
*                                                                               
         CLI   SVCLPROF+0,C'0'        ATTEMPTS TO ADD BRAND ESTIMATE            
         BE    VK40                   BEFORE POL ... ERROR                      
*                                                                               
VK30     CLI   BPRD,X'FF'                                                       
         BE    VKX                                                              
         MVC   ERRNUM,=AL2(ESTERR3)                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPERREX                                                          
*                                                                               
VK40     MVC   ERRNUM,=AL2(ESTERR3)   IN NETPAK, IF ANY CLIENTS ATT-            
         CLI   BPRD,X'FF'             EMPTS TO ADD BRAND ESTIMATE               
         BE    VKX                    BEFORE POL ... ERROR                      
         CLC   KEY(13),KEYSAVE                                                  
         BE    VK50                                                             
         CLI   QMED,C'N'                                                        
         BE    SPERREX                                                          
         CLI   SVCLTPOL,C'Y'                                                    
         BNE   VK60                                                             
         B     SPERREX                                                          
*                                                                               
VK50     MVC   ERRNUM,=AL2(ESTERR4)   IF EXISTING POL ESTIMATE IS A             
         MVC   AIO,AIO3               MASTER OR SUB-ESTIMATE ... CANNOT         
         GOTO1 GETREC                 ADD BRAND ESTIMATE                        
         L     R3,AIO                                                           
         CLI   EMSTRIND,0                                                       
         BNE   SPERREX                                                          
*                                                                               
VK60     MVC   ERRNUM,=AL2(ESTERR3)   FOR PROFIT WITHIN PERCENTAGE              
         CLI   BPRD,X'FF'             CLIENTS, POL ESTIMATE MUST BE             
         BE    VKX                    ADDED BEFORE ANY BRANDS                   
         OC    SVCLTPW,SVCLTPW                                                  
         BZ    VKX                                                              
         CLI   ACTNUM,ACTADD                                                    
         BNE   VK65                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   SPERREX                                                          
         B     VK70                                                             
*                                                                               
VK65     CLC   KEY(13),KEYSAVE                                                  
         BNE   VKX                                                              
VK70     MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                 SAVE PROFIT WITHIN PERCENTAGE             
         MVC   SVPOLPW,EPWPCT         FROM POL RECORD                           
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
*                                                                               
DR01     ZIC   R1,0(R2)             LENGTH OF FIRST FIELD                       
         AHI   R1,-9                MINUS HEADER AND 1 FOR EX                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),SPACES       BLANK CURRENT FIELD                         
         OI    6(R2),X'80'          TRANSMIT                                    
DR02     ZIC   R1,0(R2)             R1 = LENGTH OF FIELD + HEADER               
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
         MVC   ESTDESC,EDESC         DESCRIPTION                                
         OI    ESTDESCH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         TM    ECNTRL,X'04'          STATUS  (ESTIMATE LOCK)                    
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
         CLI   SECEBF,C'N'                                                      
         BE    DR100                                                            
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
DR50     MVC   ESTBBAS,=CL5'CNET'    X'10' AND X'40' BIT ON ...                 
         MVI   ESTBBASH+5,4                                                     
         TM    EBILLBAS,X'50'        SET BILL BASIS FIELD TO 'CNET'             
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=CL5'NET'     X'10' BIT ON ...                           
         MVI   ESTBBASH+5,3                                                     
         TM    EBILLBAS,X'10'        SET BILL BASIS FIELD TO 'NET'              
         BO    DR60                                                             
*                                                                               
         MVC   ESTBBAS,=C'CGROS'     X'40' BIT ON ...                           
         MVI   ESTBBASH+5,5                                                     
         TM    EBILLBAS,X'40'        SET BILL BASIS FIELD TO 'CGROS'            
         BO    DR60                                                             
*                                                                               
*                                    OTHERWISE ...                              
         MVC   ESTBBAS,=C'GROSS'     SET BILL BASIS FIELD TO 'GROSS'            
         MVI   ESTBBASH+5,5                                                     
*                                                                               
**********************************************************************          
*                                                                               
DR60     ICM   R4,15,EBILLCOM        COM.PERCENTAGE                             
         LTR   R4,R4                                                            
         BNZ   DR70                                                             
*                                                                               
         MVC   ESTCPCT,SPACES        IF COM BASIS IS ZERO ...                   
         MVC   ESTCBAS,SPACES        MOVE BLANK COMMISSION PERCENTAGE           
         B     DR100                 AND BLANK COM BASIS TO SCREEN              
*                                                                               
DR70     LPR   RF,R4                                                            
         C     RF,=F'1000000'        IF COM BASIS IS 100% MOVE 100              
         BNE   DR80                  TO SCREEN                                  
         MVC   ESTCPCT+1(3),=C'100'                                             
         B     DR90                                                             
*                                                                               
*                                    OTHERWISE ...                              
*                                    EDIT COM.% INTO SCREEN FIELD               
DR80     EDIT  (R4),(8,ESTCPCT),4,FLOAT=+,ALIGN=LEFT                            
*                                                                               
DR90     LTR   R4,R4                                                            
         BNM   *+8                   COM.% NEGATIVE, MINUS SIGN                 
         MVI   ESTCPCT,C'-'                                                     
*                                                                               
         MVI   ESTCPCTH+5,7                                                     
         CLI   ESTCPCT+7,0                                                      
         BE    DR95                                                             
         CLI   ESTCPCT+7,C' '                                                   
         BE    DR95                                                             
         MVI   ESTCPCTH+5,8                                                     
*                                                                               
**********************************************************************          
*                                                                               
*                                    COMM.BASIS                                 
DR95     MVC   ESTCBAS,=C'GROSS'     X'01' BIT ON ...                           
         MVI   ESTCBASH+5,5                                                     
         TM    EBILLBAS,X'01'        SET COMM.BASIS FIELD TO 'GROSS'            
         BZ    DR100                 OTHERWISE ...                              
         MVC   ESTCBAS,=CL5'NET'     SET COMM.BASIS FIELD TO 'NET'              
         MVI   ESTCBASH+5,3                                                     
*                                                                               
**********************************************************************          
*                                                                               
DR100    DS    0H                                                               
**********************************************************************          
*                                                                               
         XC    ESTREP,ESTREP         SPECIAL REP                                
         OC    EREP,EREP                                                        
         BZ    DR250                                                            
*                                                                               
         MVC   HALF,EREP                                                        
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTREP,DUB                                                       
DR250    OI    ESTREPH+6,X'80'                                                  
**********************************************************************          
*                                                                               
         MVC   ESTFLTR,EPROF         FILTERS                                    
         OI    ESTFLTRH+6,X'80'                                                 
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
         LA    R4,TYPTAB             OTHERWISE ... IS TYPE IN TYPE              
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
         ZIC   R0,EREQLO             IF NOT 'NO' COPY FIRST # OF RANGE          
         CVD   R0,DUB                INTO SCREEN FIELD                          
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRAN(3),DUB                                                    
         MVI   ESTRAN+3,C'-'         MOVE '-' BETWEEN RANGE NUMBERS             
         ZIC   R0,EREQHI                                                        
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
         CLI   SECFLDPO,C'N'         USER HAS READ OR WRITE ACCESS?             
         BE    DR371                 NO - DO NOT DISPLAY FIELD VALUE            
         MVC   ESTPON,EPONUM         PURCHASE ORDER NUMBER                      
         OI    ESTPONH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*                                                                               
DR371    LA    RF,ESTUSR1H           FIRST USER                                 
         LA    RE,ESTDSC1H           FIRST DESCRIPTION                          
*                                                                               
         OI    ESTUSR1H+1,X'20'      FIRST DESCRIPTION                          
         MVC   8(L'SVE1USER,RE),SVE1USER                                        
         CLC   SPACES(L'SVE1USER),8(RE)                                         
         BL    DR375                                                            
*                                                                               
         LR    R0,RE                 FIRST DESCRIPTION ADDRESS                  
         ZIC   RE,0(RF)              RE = L(HEADER) + L(INPUT)                  
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
         ZIC   RE,0(RF)              RE = L(HEADER) + L(INPUT)                  
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
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR379                 NO                                         
         CLI   ETYPE,0               IS ETYPE REGULAR?                          
         BNE   *+8                   NO                                         
         MVI   ETYPE,C'R'                                                       
         CLI   ETYPE,C'R'            IS ETYPE REGULAR?                          
         BE    DR379                 YES...DON'T DISPLAY                        
*                                                                               
         MVC   0(5,R1),=C'TYPE='                                                
*                                                                               
         MVC   5(3,R1),=C'STW'                                                  
         B     DR378B                                                           
*                                                                               
DR378B   MVI   8(R1),C','            COMMA TO SCREEN                            
         LA    R1,9(R1)              BUMP TO NEXT AVAILIBLE SPACE               
*                                                                               
DR379    CLI   SVCLDLY,C'Y'          DAILY OPTION SET?                          
         BE    DR380                                                            
         CLI   EDAILY,C' '                                                      
         BNH   DR390                                                            
DR380    LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR390                                                            
         CLI   EDAILY,C' '                                                      
         BH    *+8                                                              
         MVI   EDAILY,C'N'                                                      
         MVC   0(6,R1),=C'DAILY='    YES ... COPY DAILY OPTION AND              
         MVC   6(1,R1),EDAILY        COMMA TO SCREEN                            
         MVI   7(R1),C','                                                       
         LA    R1,8(R1)                                                         
*                                                                               
*                                                                               
DR390    TM    EFLAG1,EF1REQ         REQUEST OPTION SET?                        
         BNO   DR400                                                            
         LA    R0,6                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR400                                                            
         MVC   0(6,R1),=C'REQ=Y,'    YES ... COPY REQUEST OPTION TO             
         LA    R1,6(R1)              SCREEN                                     
*                                                                               
*                                                                               
DR400    TM    EFLAG1,EF1NMG         NEW MAKEGOODS OPTION SET?                  
         BNO   DR410                                                            
         LA    R0,6                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR410                                                            
         MVC   0(6,R1),=C'NMG=Y,'    YES ... COPY NMG OPTION TO                 
         LA    R1,6(R1)              SCREEN                                     
*                                                                               
*                                                                               
DR410    TM    EFLAG1,EF1NODEM       NO DEMOS REQUIRED FOR BUY OPTION           
         BNO   DR420                 SET?                                       
         LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR420                                                            
         MVC   0(8,R1),=C'DEMOS=N,'  YES ... COPY DEMOS OPTION TO               
         LA    R1,8(R1)              SCREEN                                     
*                                                                               
*                                                                               
DR420    OC    ECGTPCT,ECGTPCT       CLIENT GROSS TRADE OPTION SET?             
         BZ    DR430                                                            
         LA    R0,10                 MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
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
*                                                                               
DR430    TM    EFLAG1,EF1OOWPW       PW OOW BILLING FEAUTURE OPTION             
         BZ    DR470                 SET?                                       
         LA    R0,5                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR470                                                            
         MVC   0(4,R1),=C'OWPW'      YES ... COPY OWPW OPTION AND COMMA         
         MVI   4(R1),C','            TO SCREEN                                  
         LA    R1,5(R1)                                                         
*                                                                               
*                                                                               
*                                                                               
DR470    CLI   ESLN,0                RESTRICTED SPOT LEN OPTION SET?            
         BE    DR480                                                            
         LA    R0,7                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR480                                                            
         MVC   0(4,R1),=C'SLN='      YES ... COPY RESTRICTED SPOT LEN           
         LA    R1,4(R1)              AND COMMA TO SCREEN                        
         LR    R6,R1                                                            
         EDIT  ESLN,(3,(R6)),0,ALIGN=LEFT                                       
         AR    R6,R0                                                            
         MVI   0(R6),C','                                                       
         LA    R1,1(R6)                                                         
*                                                                               
*  BYPASS CASHPRD LOGIC NO LONGER SUPPORTED IN NET                              
DR480    B     DR540                                                            
*                                                                               
***** DR480    CLI   ECASHPRD,0            CASH PRODUCT OPTION SET?             
         BE    DR510                                                            
         LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR510                                                            
         MVC   0(5,R1),=C'CASH='     YES ... COPY CASH PRODUCT AND              
         LA    R1,5(R1)              COMMA TO SCREEN                            
*                                                                               
         LHI   R0,MAXPRD                                                        
         LA    RF,SVCLIST                                                       
DR490    CLI   0(RF),0                                                          
         BE    DR510                                                            
         CLC   3(1,RF),ECASHPRD                                                 
         BE    DR500                                                            
         LA    RF,4(RF)                                                         
         BCT   R0,DR490                                                         
         B     DR510                                                            
*                                                                               
DR500    MVC   0(3,R1),0(RF)                                                    
         LA    R1,3(R1)                                                         
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR510    CLI   ETRDPRD,0             TRADE PRODUCT OPTION SET?                  
         BE    DR540                                                            
         LA    R0,7                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR540                                                            
         MVC   0(4,R1),=C'TRD='      YES ... COPY TRADE PRODUCT OPTION          
         LA    R1,4(R1)              AND COMMA TO SCREEN                        
*                                                                               
         LHI   R0,MAXPRD                                                        
         LA    RF,SVCLIST                                                       
DR520    CLI   0(RF),0                                                          
         BE    DR540                                                            
         CLC   3(1,RF),ETRDPRD                                                  
         BE    DR530                                                            
         LA    RF,4(RF)                                                         
         BCT   R0,DR520                                                         
         B     DR540                                                            
*                                                                               
DR530    MVC   0(3,R1),0(RF)                                                    
         LA    R1,3(R1)                                                         
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
DR540    OC    EPWPCT,EPWPCT         PROFIT WITHIN PERCENTAGE OPTION            
         BZ    DR582                 SET?                                       
         LA    R0,11                 MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
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
         BZ    DR589                                                            
         LA    R0,14                 MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR589                                                            
         TM    EFLAG1,EF1COS2I                                                  
         BZ    DR585                                                            
         MVC   0(6,R1),=C'ICOS2='    INTEGRATION COS2 DISPLAY                   
         LA    R4,6(R1)              SCREEN                                     
         B     *+14                                                             
DR585    MVC   0(5,R1),=C'COS2='     YES ... COPY COST FACTOR TO                
         LA    R4,5(R1)              SCREEN                                     
         CLI   ECOST2,X'80'                                                     
         BNE   DR586                                                            
         MVC   0(3,R4),=C'0.0'                                                  
         LA    R0,3                                                             
         B     DR587                                                            
DR586    EDIT  ECOST2,(8,0(R4)),6,ALIGN=LEFT,FILL=0,DROP=5                      
DR587    AR    R4,R0                                                            
*                                                                               
DR589    BCTR  R1,0                                                             
         CLI   0(R1),C','                                                       
         BNE   *+8                                                              
         MVI   0(R1),C' '                                                       
         CLI   SCRNFLAG,0            IF ALL OPTIONS DIDN'T FIT ON               
         BE    DR600                 SCREEN - COPY 'DIDN'T FIT' FLAG            
         MVC   0(2,R1),=C',*'                                                   
DR600    OI    ESTOPTH+6,X'80'                                                  
*                                                                               
         OC    EBOOK,EBOOK           IF EBOOK IS EMPTY, DEFAULT TO              
         BZ    DR610                 'LATEST'                                   
*                                                                               
         CLI   QMED,C'N'                                                        
         BNE   DR605                                                            
         XC    ESTRBK,ESTRBK         IF NETWORK MEDIA ...                       
         ZIC   R0,EBOOK+1            RATING BOOK WILL BE IN BINARY              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK(2),DUB                                                    
         MVI   ESTRBK+2,C'/'                                                    
         ZIC   R0,EBOOK                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTRBK+3(2),DUB                                                  
         B     DR610                                                            
*                                    IF NOT NETWORK ... CALL DATCON             
DR605    GOTO1 DATCON,DMCB,(3,EBOOK),(6,ESTRBK)                                 
*                                                                               
DR610    MVC   ESTHUT,=CL7'AUTO'     HUTADJ                                     
         OI    ESTHUTH+6,X'80'                                                  
         CLI   EHUTADJ,0             IF ESTHUT IS EMPTY,DEFAULT TO AUTO         
         BE    DRX                                                              
*                                                                               
DR620    ZIC   R4,EHUTADJ            OTHERWISE ... CALL DATCON                  
         XC    ESTHUT,ESTHUT                                                    
         SRL   R4,4                                                             
         STC   R4,WORK+1                                                        
         MVI   WORK,77                                                          
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+10)                                 
         MVC   ESTHUT(3),WORK+10                                                
*                                                                               
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
         LA    R5,MEDTAB             FIND MEDIA CODE USING MEDIA TABLE          
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
         GOTO1 CLUNPK,DMCB,EKEYCLT,ESTCLIK                                      
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
         BH    DK30                                                             
         MVI   ESTESTKH+5,2                                                     
         CLI   ESTESTK+1,C' '                                                   
         BH    DK30                                                             
         MVI   ESTESTKH+5,1                                                     
*                                                                               
DK30     CLI   THISLSEL,C'C'         SELECT FOR CHANGE                          
         BNE   DKX                                                              
*                                                                               
         CLI   T31CFFD+1,C'*'        TEST DDS TERM                              
         BE    DKX                                                              
         TM    T31CFFD+12,X'10'                                                 
         BO    ERRSEC2               ON = NO CHANGE                             
*                                                                               
DKX      B     VK                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
*                                                                               
VR       CLC   CLIPRO(4),=C'S0F0'    SAVE CLIENT'S F0 PROFILE                   
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
         GOTO1 GETPROF,DMCB,CLIPRO,WORK,DATAMGR                                 
         MVC   SVF0PROF,WORK                                                    
*                                                                               
***********************************************************************         
*                                                                               
VR02     XC    SVESTIM,SVESTIM                                                  
         CLI   ACTNUM,ACTSEL                                                    
         BE    *+12                                                             
         CLI   ACTNUM,ACTCHA         IF CHANGING ESTIMATE, SAVE OLD             
         BNE   VR03                  ESTIMATE DATA                              
         L     R3,AIO1                                                          
         USING ESTHDR,R3                                                        
         MVC   SVCNTRL,ECNTRL                                                   
         MVC   SVSTRT,ESTART                                                    
         MVC   SVEND,EEND                                                       
         MVC   SVELOCK,ELOCKYM                                                  
         MVC   SVOWS,EOWSDAY                                                    
         MVC   SVUSRNMS,EUSRNMS                                                 
         MVC   SVPWPCT,EPWPCT                                                   
         MVC   SVEMGD,EMGDTE                                                    
         MVC   SVCPP,ECPPEST                                                    
         MVC   SVDEMOS,EDEMOS                                                   
         MVC   SVDEMLST(60),EDEMLST  NET HAS 21 DEMOS                           
         MVC   SVWGTLST(20),EWGTLST                                             
         MVC   TEMPDEM1,SVDEMLST     STORE THEM IN BLOCK                        
         MVC   TEMPDEM1+60(3),EDEM21                                            
         MVC   TEMPWGT1(20),EWGTLST                                             
         MVC   TEMPWGT1+20(1),EDEM21WT                                          
         MVC   SVBOOK,EBOOK                                                     
         MVC   SVHUT,EHUTADJ                                                    
         MVC   SVDPT,EDAYMENU                                                   
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
VR03     XC    POLDATA(POLDATAX-POLDATA),POLDATA                                
         MVI   POLSW,0                                                          
         CLC   QPRD,=C'POL'                                                     
         BNE   VR04                  IF (CHANGING OR ADDING A BRAND             
         CLI   ACTNUM,ACTADD         ESTIMATE FOR A CLEINT WITH A POL           
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
*                                                                               
         MVC   TEMPDEM2(60),EDEMLST  NET HAS 21 DEMOS                           
         MVC   TEMPDEM2+60(3),EDEM21                                            
         MVC   TEMPWGT2(20),EWGTLST                                             
         MVC   TEMPWGT2+20(1),EDEM21WT                                          
*                                                                               
         MVC   POLBOOK,EBOOK                                                    
         MVC   POLHUT,EHUTADJ                                                   
         MVC   POLDPT,EDAYMENU                                                  
         MVC   POLETYPE,ETYPE                                                   
         MVC   POLCON,ECONTROL                                                  
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
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR07                                                             
         MVC   ELEN,=Y(ESTHDRLN)                                                
         TM    USRIDFLG,USRRNTKQ     TEST ACCESS TO COMSCORE?                   
         BZ    VR07                    NO                                       
         MVC   ELEN,=AL2(ESTHDR2Q)    YES, SET EXTENDED ELEN                    
         XC    ENONTDMS(200),ENONTDMS                                           
         XC    ENONTDMS+200(200),ENONTDMS                                       
*                                                                               
VR07     CLI   ESTOPTH+5,0           IF NO OPTIONS INPUTTED AND CLIENT          
         BNE   VR30                  NOT SET FOR DAILY, BLANK OPTIONS           
         MVC   ESTOPT,SPACES         FIELD                                      
*                                                                               
         CLI   SVCLDLY,C'Y'          IF NO OPTIONS INPUTTED AND CLIENT          
         BNE   VR10                  SET FOR DAILY, MOVE IN DAILY=Y             
         MVI   ESTOPTH+5,7                                                      
         MVC   ESTOPT(7),=C'DAILY=Y'                                            
VR10     OI    ESTOPTH+6,X'80'                                                  
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
VR32     GOTO1 =A(VR40),RR=RELO      VAL DESCRIPTION AND START DATE             
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
         GOTO1 =A(VR1090),RR=RELO    DEPT MENUS AND FILTERS                     
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 =A(VR1210),RR=RELO    CONTROL AND RETAIL SCHEME                  
*                                                                               
***********************************************************************         
*                                                                               
         CLI   SECFLDPO,0            FULL ACCESS?                               
         BNE   VR361                 NO - CANNOT CHANGE THIS FIELD              
         LA    R2,ESTPONH                                                       
         MVC   EPONUM,ESTPON         VALIDATE PURCHASE ORDER NUMBER             
         OC    EPONUM,SPACES                                                    
         BRAS  RE,VALPROR            MAKE SURE PO IS ALPHANUMERIC               
         JNE   ERRINV                                                           
*                                                                               
***********************************************************************         
VR361    GOTO1 =A(VR1900),RR=RELO    VALIDATE RATING BOOK & HUT ADJUST          
         GOTO1 =A(VR2000),RR=RELO    VALIDATE RATING BOOK & HUT ADJUST          
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
         EJECT                                                                  
***********************************************************************         
*                                                                               
CHECKOPT NTR1                                                                   
         AR    R0,R1               A(NEW FIELD POSITION)                        
         LA    R3,ESTOPT           A(SCREEN FIELD)                              
         LA    R3,L'ESTOPT-1(R3)   A(LAST POSTITION IN FIELD)                   
         CR    R0,R3               WILL CURRENT OPTION FIT IN FIELD?            
         BH    CKOPERR             NO - SO ERROR                                
*                                                                               
         XR    R0,R0               SET GOOD CC                                  
         B     CKOPEXIT                                                         
*                                                                               
CKOPERR  ZIC   R3,SCRNFLAG         SAVE OLD COUNT OF MISSED OPTS                
         LA    R3,1(R3)            INC COUNT                                    
         STC   R3,SCRNFLAG         STORE IT                                     
         LTR   RC,RC               SET ERROR CC                                 
CKOPEXIT XIT1                                                                   
         EJECT                                                                  
*                                                                               
**********************************************************************          
*        SPACES TO ZEROS                                                        
**********************************************************************          
SPTOZER  NTR1                                                                   
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
SPTOZX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
ERRBKLN  MVC   ERRNUM,=AL2(BKLNINV)                                             
         B     SPERREX                                                          
ERRBRPOL MVC   ERRNUM,=AL2(BRPOLER1)                                            
         B     SPERREX                                                          
ERRDMPOL MVC   ERRNUM,=AL2(DEMERR2)                                             
         B     SPERREX                                                          
ERRNOCHG MVC   ERRNUM,=AL2(CHGERR)                                              
         B     SPERREX                                                          
NODEMDEL MVC   ERRNUM,=AL2(NODEMODE)                                            
         B     SPERREX                                                          
ERREXC   MVC   ERRNUM,=AL2(DEMOEXC)                                             
         B     SPERREX                                                          
*                                                                               
ERRSTAT  MVC   ERRNUM,=AL2(INVSTAT)                                             
         B     SPERREX                                                          
ERRMEDC  MVC   ERRNUM,=AL2(INVMEDC)                                             
         B     SPERREX                                                          
ERRBILB  MVC   ERRNUM,=AL2(INVBILB)                                             
         B     SPERREX                                                          
ERRCOMB  MVC   ERRNUM,=AL2(INVCOMB)                                             
         B     SPERREX                                                          
ERRSREP  MVC   ERRNUM,=AL2(INVSREP)                                             
         B     SPERREX                                                          
ERRANGE  LA    R2,ESTRANH                                                       
ERRREQR  MVC   ERRNUM,=AL2(INVREQR)                                             
         B     SPERREX                                                          
ERRRTYP  MVC   ERRNUM,=AL2(INVRTYP)                                             
         B     SPERREX                                                          
ERRCNTL  MVC   ERRNUM,=AL2(INVCNTL)                                             
         B     SPERREX                                                          
ERRHTBK  MVC   ERRNUM,=AL2(INVHTBK)                                             
         B     SPERREX                                                          
ERRHUTA  MVC   ERRNUM,=AL2(INVHUTA)                                             
         B     SPERREX                                                          
ERRFLTR  MVC   ERRNUM,=AL2(INVFLTR)                                             
         B     SPERREX                                                          
ERRMONT  MVC   ERRNUM,=AL2(INVMONT)                                             
         B     SPERREX                                                          
*                                                                               
         USING SCAND,R5                                                         
ERROPTN  XC    CONHEAD,CONHEAD       INIT                                       
         CLI   FLD1LEN,0             CHECK FOR FLD1 LENGTH >0 & <=10            
         BNH   ERROPTN6              NO, DEFAULT ERROR MESSAGE                  
         CLI   FLD1LEN,L'FLD1                                                   
         BH    ERROPTN6                                                         
         LLC   RF,FLD1LEN            YES,                                       
         MVC   CONHEAD(0),FLD1       MOVE FLD1 DATA                             
         EX    RF,*-6                                                           
         LA    RF,CONHEAD(RF)        BUMP TO NEXT AVAIALBLE POSITION            
         CLI   FLD2LEN,0             CHECK FOR FLD2 LENGTH >0 & <=10            
         BNH   ERROPTN4              NO, DEFAULT ERROR MESSAGE                  
         CLI   FLD2LEN,L'FLD1                                                   
         BH    ERROPTN6                                                         
         MVI   0(RF),C'='            POPULATE '='                               
         LA    RF,1(RF)              BUMP TO NEXT AVAIALBLE POSITION            
         LLC   RE,FLD2LEN                                                       
         MVC   0(0,RF),FLD2          MOVE FLD2 DATA                             
         EX    RE,*-6                                                           
         LA    RF,0(RE,RF)           BUMP TO NEXT AVAIALBLE POSITION            
ERROPTN4 MVC   0(22,RF),=CL22' IS NOT A VALID OPTION'                           
         B     ERROPTNX                                                         
ERROPTN6 XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(28),=CL28'INVALID DATA IN OPTION FIELD'                  
ERROPTNX GOTO1 ERREX2                                                           
         DROP  R5                                                               
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
         SPACE 2                                                                
*        ERRORMESSAGES                                                          
*                                  SHORT DESP OF ERROR MSGS                     
INVHADT  EQU   1466                INVALID DATE IN HUT ADJ FIELD                
INVHTDT  EQU   1465                INVALID DATE IN HUT FORMULA FIELD            
INVSRDA  EQU   1464                ONLY NUMERIC DATA IS ALLOWED IN REP          
INVSREP  EQU   1461                INVALID SPECIAL REP FIELD                    
INVCOMB  EQU   1460                INVALID COMM BASIS FIELD                     
INVBILB  EQU   1459                INVALID BILL BASIS FIELD                     
INVMONT  EQU   1458                INVALID MONETARY                             
INVRTYP  EQU   1449                RATE TYPE IS NOT VALID                       
INVCNTL  EQU   1436                CONTROL ENTRY IS NOT VALID                   
INVHUTA  EQU   1432                HUT FIELD SHOULD BE AUTO OR A MONTH          
INVREQR  EQU   1429                INVALID REQUEST RANGE                        
INVSTAT  EQU   1426                INVALID STATUS FIELD                         
INVOPTN  EQU   0603                INVALID OPTION                               
INVFLTR  EQU   0604                INVALID FILTER                               
INVENDT  EQU   0153                END DATE NOT VALID                           
INVSTDT  EQU   0152                START DATE NOT VALID                         
INVHTBK  EQU   0031                BOOK-HUT INVALID                             
INVMEDC  EQU   0013                INVALID MEDIA                                
NUMERR   EQU   3                   NUMERIC ONLY                                 
ALPHAERR EQU   4                   ALPHABETIC ONLY                              
NOTAUTH  EQU   175                 NOT AUTHRORIZED FOR THIS FUNCTION            
BKLNINV  EQU   449                 BREAK LN MUST BE 1-4                         
GMIERR   EQU   486                 GMI NOT SET UP FOR CLIENT                    
BIGERR   EQU   487                 OTHER AGENCY FEE TOO LARGE                   
TALERR   EQU   488                 NTP NOT VALID FOR PRODUCT                    
TALREQ   EQU   489                 NTP REQUIRED FOR THIS CLIENT                 
CLTFULL  EQU   485                 TOO MANY PRODUCTS                            
PRDERR   EQU   491                 PRD ERROR CHECK TRAFFIC MASTER PRD           
SCRTYERR EQU   492                 ACCESS TO CLIENT NOT AUTHORIZED              
ERRTRADE EQU   493                 TRADE PRD CODE ALREAD IN CLIST               
ERRTRAD2 EQU   494                 THIRD CHAR IN PRD CODE MUST BE 'C'           
CLNTERR  EQU   40                  CLIENT NOT FOUND                             
PRODERR  EQU   41                  PRODUCT NOT FOUND                            
NTPERR   EQU   495                 NTP MUST FIT BETWEEN 0 AND 2                 
BADDATE  EQU   20                  INVALID DATE                                 
PNREQER  EQU   496                 PRD NAME REQUIRED                            
PNDELER  EQU   497                 PRD NAME CANNOT BE DELETE                    
CPERR1   EQU   498                 CLT/PRD CODE REQUIRED                        
CPERR2   EQU   499                 CLT/PRD CODE MUST BE 4 ALPHANUMER            
CPERR3   EQU   503                 CLT/PRD CODE MUST BE 4 NUMERICS              
CPERR4   EQU   505                 CLT/PRD CODE MUST BE 5 NUMERICS              
PCLERR1  EQU   506                 CLASSES MUST BE A - I                        
PCLERR2  EQU   507                 CLASSES CANNOT BE EQUAL                      
BTNERR   EQU   508                 BILL-TO-NAME REQUIRED                        
OAFERR   EQU   509                 OAF CANNOT EXCEED 9.99                       
BBERR    EQU   510                 BILL-BASIS IS CGROSS OR CNET                 
COMPERR1 EQU   511                 COM % REQUIRED                               
COMPERR2 EQU   515                 COM % VALID NUMERIC                          
COMPERR3 EQU   512                 100% IS MAX COM %                            
COMPERR4 EQU   513                 0% IS MIN COM %                              
COMPERR5 EQU   514                 1ST CHAR IS + OR -                           
CBASERR1 EQU   522                 COMM BASIS REQUIRED                          
CBASERR2 EQU   521                 COMM BASIS GROSS OR NET                      
EDERR1   EQU   542                 DATE REQUIRES BILL BASIS                     
EDERR2   EQU   523                 DATE REQUIRED FOR BILL BASIS                 
GSTERR   EQU   524                 INVALID GST CODE                             
PSTERR1  EQU   528                 INVALID PST CODE                             
EDTERR1  EQU   530                 INPUT CANNOT BE NUMERIC                      
EDTERR2  EQU   529                 INPUT MUST BE NUMERIC                        
OPTERR1  EQU   531                 OPTION MUST BE NTP                           
OPTERR2  EQU   532                 NTP MUST BE 0-2                              
VKERR1   EQU   533                 MUST BE 3 LONG                               
VKERR2   EQU   534                 MUST BE VALID HEX                            
VKERR3   EQU   535                 ZZZ INVALID PRD CODE                         
VKERR4   EQU   536                 CLIENT MUST BE 'CC'                          
VKERR5   EQU   537                 1ST CHAR MUST BE ALPHA                       
VKERR6   EQU   538                 MUST BE 2 OR 3 CHARS LONG                    
VKERR7   EQU   539                 2ND AND 3RD CHARS ALPHANUMERIC               
VKERR8   EQU   540                 ALL IS INVALID PRD CODE                      
VKERR9   EQU   541                 NO IS INVALID PRD CODE                       
CHGERR   EQU   546                 CANNOT CHANGE FIELD                          
NTPERR3  EQU   547                 NTP CAN ONLY BE SET ONCE                     
DELERR1  EQU   548                 PRODUCT EXISTS FOR GROUP IN ID               
DELERR2  EQU   549                 MASTER TRAFFIC CLT ALREADY EXISTS            
DELERR3  EQU   550                 BILL RECS SHOULD SUM TO ZERO                 
DELERR4  EQU   551                 ORDERED OR PAID $ ON EST                     
DELERR5  EQU   552                 BILL ON FILE- CANNOT DEL PRD                 
DELERR6  EQU   553                 NO GOALS-- GO CHECK FOR BUYS                 
DELERR7  EQU   554                 HAS BUYS, CANNOT DELETE                      
DELERR8  EQU   555                 CANNOT DELETE POL PRODUCT                    
DELERR9  EQU   560                 'DELETE' MUST BE IN PROD NAME                
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
TV1DEM   EQU   573                 TV MUST HAVE AT LEAST 1 DEMO                 
NET1DEM  EQU   721                 NETPAK MUST HAVE AT LEAST 1 DEMO             
TOTHERR  EQU   576                 TOTAL HMS MUST BE INPUT FOR NETWORK          
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
SPTLNERR EQU   673                 SPOT LENGTH NOT VALID                        
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
MENERR1  EQU   698                 DEPT MENU REQUIRED                           
MENERR2  EQU   699                 DEPT MENU MUST BE 1 CHAR LONG                
BRPOLER2 EQU   701                 BRAND & POL OOWR MUST AGREE                  
BRPOLER3 EQU   702                 BRAND & POL RATING BOOK MUST AGREE           
BRPOLER4 EQU   703                 BRAND & POL DEPT MENY MUST AGREE             
BRPOLER5 EQU   704                 BRAND & POL HUT ADJ'S MUST AGREE             
BRPOLER6 EQU   705                 BRAND & POL CONTROL'S MUST AGREE             
BRPOLER7 EQU   706                 BRAND & POL RET SCHEME MUST AGREE            
BRPOLER8 EQU   707                 BRAND & POL FILTERS MUST AGREE               
BRPOLER9 EQU   708                 BRAND & POL WEIGHTS MUST AGREE               
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
DEMOEXC  EQU   797                 EXCEEDS MAXIMUM DEMOS.                       
MULTCOS2 EQU   903                 MORE THEN 1 COS2 ENTRY                       
TYPERR   EQU   985                 TYPE OPT MUST BE STW,REG,BAR(TER)            
BRPOLERA EQU   990                 BRAND & POL TYPE=OPTS MUST AGREE             
TYPEERR1 EQU   1145                TYPE=STW/BAR NOT ALLOWED                     
WTSTERRS EQU   1211                MUST ENTER T1 AND T2 DEMOS                   
POLWIDE  EQU   1300                BRAND EST DATES WIDER THEN POL EST           
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLES                                                                        
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
EOTBLQ   DC    C'A'                                                             
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
*                                                                               
***********************************************************************         
*        AGENCY TABLE                                                 *         
***********************************************************************         
AGYTAB   DC   C'GY'                                                             
         DC   C'DR'                                                             
         DC   C'GN'                                                             
         DC   C'CE'                                                             
         DC   C'FM'                                                             
         DC   C'RE'                                                             
         DC   X'FF'                                                             
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
*        SLNTABC                                                      *         
***********************************************************************         
*                                                                               
SLNTABC  DC    AL1(10,15,20,30,40,45,50,60,75,90,120,5)                         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
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
*NOP*    DC    C'DX03901T'                                                      
         DC    C'DDNY916T'                                                      
*NOP*    DC    C'DDNY720T'                                                      
*NOP*    DC    C'DDNYF11T'          DDS                                         
         DC    C'HDTO847T'          HDTO (WAS HDTO823T)                         
         DC    C'HDTO829T'          HDTO (WAS HDTO830T)                         
         DC    C'XDDSC84A'                                                      
*NOP*    DC    C'DDNYD26T'                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R2,CONACTH          POINT TO ACTION                              
         CLI   T31CFFD+1,C'*'      TEST DDS TERM                                
         BE    SETUP01                                                          
         TM    T31CFFD+12,X'10'                                                 
         BNO   SETUP01             NOT ON = ALL OK                              
         CLI   ACTNUM,ACTCHA                                                    
         BE    ERRSEC2             CHANGE NOT ALLOWED                           
         CLI   ACTNUM,ACTADD                                                    
         BE    ERRSEC2             ADD NOT ALLOWED                              
*                                                                               
SETUP01  MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
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
SETUP30  LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
         L     R9,ASYSD            A(SYSD)                                      
*                                                                               
SETUP45  XR    R4,R4               CLEAR R4                                     
         ICM   R4,3,0(R2)          R4=DISPLACEMENT TO SECURITY VALUE            
         AR    R4,R9               A(SECURITY FIELD)                            
         MVI   0(R4),0             INIT TO FULL ACCESS                          
         OC    TWASAGN,TWASAGN     ON NEW SECURITY?                             
         BNZ   *+14                YES                                          
         OC    TWAACCS(2),TWAACCS  LIMIT ACCESS?                                
         BZ    SETUP50             NO - LEAVE FULL ACCESS                       
*                                                                               
         GOTO1 SECRET,DMCB,('SECPFLDP',ASECBLK),2(R2)                           
         BE    SETUP50             CC EQU IF FULL READ/WRITE ACCESS             
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                 CC HIGH IF READ ONLY                         
         LA    RF,C'N'             C'N'=NO ACCESS                               
         STC   RF,0(R4)            SET LIMIT FCONTROL ACCESS                    
SETUP50  AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,SETUP45          PROCESS NEXT SECURITY FIELD                  
*                                                                               
         LA    RF,SECFLDPO         SECURITY FIELD                               
         LA    R2,ESTPONDH         ESTIMATE PO# FIELD                           
         BRAS  RE,PRCFLD           PROTECT FIELDS IF NECESSARY                  
*                                                                               
* SPEC-43698: EST BILL FORMULA FIELD LEVEL SECURITY                             
*                                                                               
         LA    RF,SECEBF                                                        
         CLI   0(RF),C'N'          NO ACCESS?                                   
         BNE   SETUP52                                                          
         XC    ESTBFD,ESTBFD                                                    
         OI    ESTBFDH+6,X'80'                                                  
*                                                                               
SETUP52  LA    RF,SECEBF                                                        
         LA    R2,ESTBBH                                                        
         BRAS  RE,PRCFLD                                                        
         LA    RF,SECEBF                                                        
         LA    R2,ESTCPH                                                        
         BRAS  RE,PRCFLD                                                        
         LA    RF,SECEBF                                                        
         LA    R2,ESTCBH                                                        
         BRAS  RE,PRCFLD                                                        
         J     SETUPX                                                           
*                                                                               
SETUPX   XIT1                                                                   
*                                                                               
SECFLDS  DS    0XL3                ** DISPS. TO SECURITY VALUES **              
         DC    AL2(SECFLDPO-SYSD),AL1(01)  PURCHASE ORDER #                     
         DC    AL2(SECEBF-SYSD),AL1(02)    EST BILL FORMULA                     
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
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
         BE    *+6                 YES                                          
         DC    H'0'                INVALID SECURITY VALUE                       
         SHI   R1,8+1              MINUS OVERHEAD AND ONE FOR EX                
         CHI   R1,0                TITLE FIELD LENGTH > 0?                      
         BNL   *+6                 YES                                          
         DC    H'0'                NO - BAD TITLE FLD LENGTH                    
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
***********************************************************************         
*        VR40 - VALIDATE DESCRIPTION, START DATE, END DATE AND        *         
*               OUT OF WEEK ROTATOR                                             
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
*                                                                               
VR60     MVC   EDESC,ESTDESC         DESCRIPTION VALIDATION COMPLETE            
         OC    EDESC,SPACES          ... SAVE TO RECORD                         
*                                                                               
***********************************************************************         
*                                                                               
VR70     MVC   ERRNUM,=AL2(DATERR1)                                             
         LA    R2,ESTSTRDH           START DATE REQUIRED                        
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
*                                    MUST BE VALID M/D/Y (US)                   
         MVC   ERRNUM,=AL2(INVSTDT)  VALID D/M/Y (UK)                           
         GOTO1 DATVAL,DMCB,(0,ESTSTRD),SYR                                      
         OC    DMCB(4),DMCB          YY RETURNED IN SYR, MM RETURNED            
         BZ    SPERREX               IN SMN, DD RETURNED IN SDY                 
*                                                                               
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR5)                                             
         CLI   ACTNUM,ACTADD                                                    
         BE    VR100                                                            
         CLI   WORK2,C'D'            MASTER OR SUB-ESTIMATE'S START             
         BE    VR100                 DATE CANNOT BE CHANGED UNLESS              
         CLI   EMSTRIND,0            DATES CAN BE CHANGED W/0 VALID-            
         BE    VR80                  ATING                                      
         CLC   SVSTRT,SYR                                                       
         BE    VR100                                                            
         B     SPERREX                                                          
*                                                                               
VR80     MVC   ERRNUM,=AL2(ESTERR6)  NON-MASTER OR NON-SUB ESTIMATE'S           
         OC    SVCPP,SVCPP           START DATE CANNOT BE ADVANCED UN-          
         BNZ   VR90                  LESS CPP EST. IS 0 AND TERMINAL            
         GOTO1 =A(CKLINID),RR=RELO   IS AUTHORIZED (OR IF DATES CAN BE          
         BE    VR100                 CHANGED W/O VALIDATING)                    
VR90     CLC   SVSTRT,SYR                                                       
         BL    SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR100    MVC   ERRNUM,=AL2(DATERR2)                                             
         LA    R2,ESTENDDH           END DATE REQUIRED                          
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
*                                    MUST BE VALID M/D/Y (US)                   
         MVC   ERRNUM,=AL2(INVENDT)  VALID D/M/Y (UK)                           
         GOTO1 DATVAL,DMCB,(0,ESTENDD),EYR                                      
         OC    DMCB(4),DMCB          YY RETURNED IN EYR, MM RETURNED            
         BZ    SPERREX               IN EMN, DD RETURNED IN EDY                 
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR5)                                             
         CLI   ACTNUM,ACTADD         MASTER OR SUB-ESTIMATE'S END DATE          
         BE    VR130                 CANNOT BE CHANGED UNLESS DATES CAN         
         CLI   WORK2,C'D'            BE CHANGED W/O VALIDATING                  
         BE    VR130                                                            
         CLI   EMSTRIND,0            SINCE NEITHER START OR END DATE            
         BE    VR110                 FOR MASTER OR SUB-ESTIMATE HAVE            
         CLC   SVEND,EYR             CHANGED ... DATE VALIDATION COMP-          
         BNE   SPERREX               LETE                                       
         B     VR350                                                            
*                                                                               
*                                                                               
VR110    MVC   ERRNUM,=AL2(ESTERR6)  NON-MASTER OR NON-SUB ESTIMATE'S           
         OC    ECPPEST,ECPPEST       END DATES CANNOT BE CUT BACK               
         BNZ   VR120                 UNLESS CPP EST IS 0 AND TERMINAL           
         GOTO1 =A(CKLINID),RR=RELO   IS AUTHORIZED (OR IF DATES CAN BE          
         BE    VR130                 CHANGED W/0 VALIDATING)                    
VR120    CLC   SVEND,EYR                                                        
         BH    SPERREX                                                          
*                                                                               
VR130    MVC   ERRNUM,=AL2(ESTERR7)  END DATE MUST BE LATER OR EQUAL TO         
         CLC   SYR(6),EYR            START DATE                                 
         BH    SPERREX                                                          
*                                                                               
         CLC   QPRD,=C'POL'          BYPASS DATE CHECK IF FOR POL               
         BE    VR132                                                            
         MVC   ERRNUM,=AL2(POLWIDE)  DATES MUST EQUAL POL DATES                 
         CLC   SYR(6),POLSTRT        START DATE                                 
         BNE   SPERREX                                                          
         CLC   EYR(6),POLEND         END DATE                                   
         BNE   SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR132    MVC   ERRNUM,=AL2(ESTERR8)  UNLESS START AND END DATE HAVE NOT         
         CLI   OVSYS,3               BEEN MODIFIED FOR A MASTER OR              
         BNE   VR140                 SUB-ESTIMATE ... NETPAK ESTIMATES          
         LA    R2,ESTMEDKH           MUST BE MEDIA N                            
         CLI   QMED,C'N'                                                        
         BNE   SPERREX                                                          
         B     VR150                                                            
*                                                                               
VR140    LA    R2,ESTSTATH           UNLESS START AND END DATE HAVE NOT         
         CLC   ESTSTAT(3),=C'OLD'    BEEN MODIFIED FOR A MASTER OR              
         BE    ERRSTAT               SUB-ESTIMATE ... 'OLD' AND 'NEW'           
         CLC   ESTSTAT(3),=C'NEW'    INVALID STATUS FOR NON-NETPAK              
         BE    ERRSTAT               ESTIMATES                                  
*                                                                               
***********************************************************************         
*                                                                               
VR150    CLI   ACTNUM,ACTADD         ON AN (ADD) OR (START DATE CHANGE)         
         BE    VR190                 OR (END DATE CHANGE)  ...                  
         CLC   SVSTRT,SYR                                                       
         BNE   VR190                                                            
         CLC   SVEND,EYR                                                        
         BE    VR220                                                            
*                                                                               
VR190    MVC   ERRNUM,=AL2(ESTERR9)     NETWORK CAN'T HAVE MORE THAN 12         
         GOTO1 =A(CHKEDTS),RR=RELO    CALENDAR MONTHS BETWEEN EST DATES         
         LA    R2,ESTSTRDH           - FOR SPOT CAN'T HAVE MORE THAN 12         
         OC    SVPWPCT,SVPWPCT         BROADCAST MONTHS OR 53 WEEKS BE-         
         BZ    VR220                     TWEEN EST DATES (UNLESS PROFIT         
         GOTO1 ADDAY,DMCB,SYR,WORK,98        WITHIN PERCENTAGE IS ZERO)         
         CLC   WORK(6),EYR                                                      
         BNH   SPERREX                                                          
*                                                                               
VR220    GOTO1 DATCON,DMCB,EYR,(2,EMGDTE)  SAVE LATEST MAKEGOOD DATES           
*                                                                               
***********************************************************************         
*                                                                               
VR310    XC    KEY,KEY               IF (ADDING A POL ESTIMATE) ... ALL         
         MVC   KEY(4),ESTKEY         BRAND ESTIMATES FOR PRODUCT MUST           
VR320    GOTO1 =A(NEXTPRD),RR=RELO   MATCH THE POL'S NEW START DATE,            
         BNE   VR350                 END DATE AND OUT OF WEEK ROTATOR           
         MVC   AIO,AIO3              ... OR ERROR                               
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR330                                                            
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   ERRBRPOL                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   ERRBRPOL                                                         
         MVC   ERRNUM,=AL2(BRPOLER2)                                            
*                                                                               
* SINCE I DON'T THINK ESTIMATE BUCKETS ARE USED IN NET, THERE'S NOT             
* MUCH POINT TO SEEING IF THERE'S ANYTHING IN THEM AFTER ALL                    
* (EJOR 26MAR02)                                                                
*                                                                               
         B     VR320                                                            
*&&DO                                                                           
         MVC   ERRNUM,=AL2(EDOLERR)                                             
* NOP ON 25MAR02                                                                
**NOP    OC    EORDN(208),EORDN      IF ORDERED OR PAID $ ON ANY OF THE         
**NOP    BZ    VR320                 BRAND ESTIMATES ... CANNOT ADD POL         
*                                                                               
         LA    R0,26               IF ANY ORDERED OR PAID $ ON ANY OF           
         LA    RF,EORD              THE BRAND ESTS, CANNOT ADD POL              
*                                                                               
VR322    CP    0(6,RF),=P'0'                                                    
         BNE   VR328                                                            
         AHI   RE,6                                                             
         BCT   R0,VR322                                                         
*                                                                               
         LA    R0,26                                                            
         LA    RF,EPAID                                                         
*                                                                               
VR324    CP    0(6,RF),=P'0'                                                    
         BNE   VR328                                                            
         AHI   RE,6                                                             
         BCT   R0,VR324                                                         
*                                                                               
VR328    CLI   ESTDESC,C'@'          ESTIMATE (UNLESS @ IN DESCRIPTION          
         BE    VR320                 FIELD)                                     
         B     SPERREX                                                          
*&&                                                                             
*                                                                               
***********************************************************************         
*                                                                               
VR330    CLC   SVSTRT,SYR            IF (CHANGING A POL ESTIMATE) ...           
         BNE   VR340                                                            
         CLC   SVEND,EYR                                                        
         BNE   VR340                                                            
         CLC   SVOWS,OWSDAY                                                     
         BE    VR320                                                            
*                                                                               
VR340    MVC   ESTART,SYR            CHANGE THE START DATE, END DATE,           
         MVC   EEND,EYR              MAKE GOOD DATE AND OUT OF WEEK             
         MVC   EMGDTE,MAKEGDDT       ROTATOR FOR ALL THE PRODUCT'S              
         MVC   EOWSDAY,OWSDAY        BRAND ESTIMATES TO MATCH THE               
         GOTO1 =A(PTREC),RR=RELO     NEW POL DATA                               
         B     VR320                                                            
*                                                                               
***********************************************************************         
*                                                                               
VR350    MVI   WORK2,C' '            RESET DATE CHANGE W/0 VALIDATION           
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
         MVC   ESTART,SYR            STORE START DATE, END DATE AND             
         MVC   EEND,EYR              OUT OF WEEK ROTATOR INTO RECORD            
         MVC   EOWSDAY,OWSDAY                                                   
VR358    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VR360 - VALIDATE BILL BASIS, COMM PCT AND COMM BASIS         *         
***********************************************************************         
*                                                                               
VR360    NTR1  BASE=*,LABEL=*                                                   
         CLI   SECEBF,C'N'          NO ACCESS?                                  
         BE    VR489                                                            
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR362                                                            
         XC    EBILLBAS,EBILLBAS                                                
         XC    EBILLCOM,EBILLCOM                                                
         CLI   SECEBF,0            WRITE ACCESS?                                
         BNE   VR489                                                            
*                                                                               
VR362    LA    R2,ESTBBASH                                                      
         XC    EBILLBAS(5),EBILLBAS  BILL BASIS IS NOT REQUIRED                 
         CLI   5(R2),0                                                          
         BE    VR400                                                            
*                                                                               
VR370    XC    WORK,WORK                                                        
         MVC   WORK+16(4),=C'SB1X'   DOES PROFILE EXIST FOR THIS                
*        MVI   WORK+16,X'A2'         AGENCY, MEDIA, CLIENT?                     
         NI    WORK+16,X'BF'       LOWER CASE                                   
         MVC   WORK+20(2),AGENCY                                                
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         MVI   WORK+26,C'*'                                                     
         MVC   WORK+27(1),BOFFICE   OFFICE                                      
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
*                                                                               
         CLI   WORK+11,C' '          BILL FORMULA NOT ALLOWED IF                
         BNH   VR380                 OPTION 12 IS SET                           
         CLI   WORK+11,C'N'                                                     
         BNE   ERRBILB                                                          
*                                                                               
VR380    ZIC   RE,5(R2)                                                         
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
VR400    LA    R2,ESTCPCTH                                                      
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
         ZIC   R0,5(R2)              (EXCEPT 1ST CHAR)                          
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
VR430    LA    R2,ESTCBASH                                                      
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
         BNE   ERRCOMB                                                          
VR450    MVC   ERRNUM,=AL2(CBASERR2) COM.BASIS MUST BE EITHER "GROSS"           
         ZIC   RE,5(R2)              OR "NET"                                   
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
VR489    XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        VR1090 - VALIDATE DAYPART MENU AND FILTERS                *            
***********************************************************************         
VR1090   NTR1  BASE=*,LABEL=*                                                   
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
         MVC   EPROF(3),ESTFLTR                                                 
         LA    R4,EPROF                                                         
         LA    R5,3                  IF INPUTTED ...                            
VR1130   CLI   0(R4),C' '            FILTER MUST BE A COMBINATION OF 3          
         BE    VR1140                OR LESS ALPHANUMERICS                      
         CLI   0(R4),C'A'                                                       
         BL    ERRFLTR                                                          
         CLI   0(R4),C'9'                                                       
         BH    ERRFLTR                                                          
VR1140   LA    R4,1(R4)                                                         
         BCT   R5,VR1130                                                        
*                                                                               
VR1150   CLI   SVCLEX+3,C'Y'         IF CLIENT REQUIRES FILTER ...              
         BNE   VR1160                FILTER MUST BE INPUTTED                    
         OC    EPROF(3),EPROF                                                   
         BZ    ERRMIS                                                           
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
VR1209   XIT1                        IS N)                                      
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
         LA    R1,ECTAGY                                                        
VR1220   CLI   0(R1),X'FF'                                                      
         BE    ERRCNTL                                                          
         CLC   0(2,R1),AGENCY                                                   
         BE    VR1240                                                           
         LA    R1,2(R1)                                                         
         B     VR1220                                                           
VR1230   CLC   ESTECON(3),=C'NSC'    ONLY OTHER VALID CONTROL IS 'NSC'          
         BNE   ERRCNTL                                                          
         MVI   ECONTROL,ENSEPCMQ                                                
*                                                                               
VR1240   MVC   ERRNUM,=AL2(CONTER1)                                             
         CLI   ACTNUM,ACTADD         CONTROL CANNOT BE CHANGED FOR              
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
         BE    SPERREX                                                          
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
         BE    SPERREX                                                          
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
         GOTO1 =A(EDTUSR),RR=RELO                                               
*                                                                               
VR1330   MVC   EUSER2,USERDATA       SAVE ESTIMATE 2 INFO INTO RECORD           
         MVC   ESTUSR2,USERDATA      CLEAR OR RETRANSMIT EST 2 FIELD            
         OI    ESTUSR2H+6,X'80'                                                 
         XIT1                                                                   
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
         MVC   ERRNUM,=AL2(BRPOLER6)                                            
         LA    R2,ESTECONH                                                      
         CLC   ECONTROL,SVECON                                                  
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
         GOTO1 =A(PTREC),RR=RELO                                                
*                                                                               
         NI    KEY+13,X'FF'-ESTSTW                                              
         CLI   ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                       
         BNE   *+8                                                              
         OI    KEY+13,ESTSTW                                                    
         GOTO1 WRITE                                                            
*                                                                               
         B     VR1350                                                           
VR1507   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*        VR1520 - VALIDATE COPY CODE, SPECIAL REP, CPP EST           *          
*                 TYPE, REQ RANGE, AND RATE TYPE                     *          
**********************************************************************          
*                                                                               
VR1520   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
VR1540   LA    R2,ESTREPH            SPECIAL REP NOT REQUIRED                   
         XC    EREP,EREP                                                        
         CLI   5(R2),0                                                          
         BE    VR1550                                                           
*                                                                               
         MVC   ERRNUM,=AL2(INVSRDA)  MUST BE NUMERIC                            
         TM    4(R2),X'08'                                                      
         BZ    SPERREX                                                          
         GOTO1 =A(SPACK),RR=RELO                                                
         MVC   EREP,HALF                                                        
*                                                                               
         XC    KEY,KEY               IF SPECIAL REP PRESENT ...                 
         MVI   KEY,C'R'              READ STATION RECORD INTO AIO3              
         MVC   KEY+1(1),QMED                                                    
         UNPK  KEY+2(3),DUB+5(3)                                                
         OI    KEY+4,X'F0'                                                      
         MVC   KEY+5(2),AGENCY                                                  
         MVC   KEY+7(10),=10C'0'                                                
         MVC   AIO,AIO3                                                         
         L     R4,AIO                                                           
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
*                                                                               
         TM    8(R1),X'FD'           TEST STATION RECORD (EXCEPT IF             
         BZ    VR1545                DELETED)                                   
         CLI   COMMAND+2,C'R'                                                   
         BE    VR1545                                                           
         CLI   8(R1),X'20'                                                      
         BNE   *+18                                                             
         L     RF,4(R1)                                                         
         CLC   =C'STATION',0(RF)                                                
         BE    VR1550                                                           
         DC    H'0'                                                             
*                                                                               
VR1545   MVC   BYTE,DMOUTBTS                                                    
         NC    BYTE,8(R1)                                                       
         BNZ   ERRSREP                                                          
*                                                                               
VR1550   MVC   KEY,ESTKEY                                                       
**********************************************************************          
*                                                                               
VR1630   LA    R2,ESTTYPEH           TYPE NOT REQUIRED                          
         CLI   ACTNUM,ACTADD                                                    
         BE    VR1640                                                           
*                                                                               
         MVC   ERRNUM,=AL2(ETYPERR)  IF TYPE PREVIOUSLY SAVED ... IT            
         CLI   ECPPTYPE,0               CANNOT BE BLANK                         
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
         LA    R5,TYPTAB                                                        
VR1670   CLC   ESTTYPE(2),0(R5)                                                 
         BE    VR1680                                                           
         LA    R5,3(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   VR1670                                                           
         B     SPERREX               IF M$ STORE 03 INTO ETYPE, IF M%           
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
         BNE   ERRREQR                                                          
         MVC   EREQLO(2),8(R2)                                                  
         B     VR1760                                                           
*                                                                               
VR1710   LA    R5,EREQLO             IF INPUT LENGTH NOT 2 ... FIRST            
         LA    R6,2                  HALF OF RANGE MUST BE VALID                
         ZIC   R0,5(R2)              NUMERIC BETWEEN 0 AND 255                  
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
         BL    ERRANGE                                                          
         CLI   0(R2),C'9'                                                       
         BH    ERRANGE                                                          
         LA    R4,1(R4)                                                         
         LA    R2,1(R2)                                                         
         BCT   R0,VR1730                                                        
         B     VR1750                                                           
VR1740   BCTR  R0,0                                                             
VR1750   LTR   R4,R4                                                            
         BZ    ERRANGE                                                          
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R1)                                                      
         CVB   R1,DUB                                                           
         CH    R1,=H'255'                                                       
         BH    ERRANGE                                                          
         CH    R1,=H'0'                                                         
         BNH   ERRANGE                                                          
*                                                                               
         STC   R1,0(R5)              TEST SECOND HALF OF RANGE WITH             
         LA    R5,1(R5)              SAME STANDARDS                             
         LA    R2,1(R2)              R5 IS NOW POINTING TO EREQHI               
         BCT   R6,VR1720                                                        
         LTR   R0,R0                                                            
         BNZ   ERRANGE               (SHOULD BE NOTHING LEFT)                   
*                                                                               
         MVC   ERRNUM,=AL2(RANERR1)  FIRST HALF OF RANGE MUST BE LESS           
         CLC   EREQLO,EREQHI         THAN SECOND HALF                           
         BNL   SPERREX                                                          
*                                                                               
         LA    R2,ESTRANH                                                       
         MVC   ERRNUM,=AL2(RANERR2)                                             
         CLC   BEST,EREQLO           CURRENT ESTIMATE CODE MUST FIT IN          
         BL    SPERREX               INPUTTED RANGE                             
         CLC   BEST,EREQHI                                                      
         BH    SPERREX                                                          
*                                                                               
**********************************************************************          
*                                                                               
VR1760   LA    R2,ESTRTYPH           RATE TYPE NOT REQUIRED                     
         MVI   ERATE,0                                                          
         MVI   ERATECST,0                                                       
         CLI   5(R2),0                                                          
         BE    VR1798                                                           
*                                                                               
         MVC   ERRNUM,=AL2(RATER1)   IF CLT DOES NOT ALLOW SPECIAL              
         CLI   SVCLPROF+14,C'*'      RATES DON'T ALLOW RATE ON EST              
         BE    SPERREX                                                          
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
         BNE   ERRRTYP                                                          
VR1770   MVC   ERATECST,ESTRTYP+1                                               
         B     *+12                                                             
*                                                                               
VR1780   CLI   5(R2),1                IF NOT NETPAK, RATE TYPE INPUT            
         BNE   ERRRTYP                MUST BE 1 CHAR LONG                       
*                                                                               
         CLI   ESTRTYP,C'*'           FIRST CHAR OF RATE TYPE MUST BE           
         BE    VR1790                 * OR 0-9                                  
         CLI   ESTRTYP,C'0'                                                     
         BL    ERRRTYP                                                          
         CLI   ESTRTYP,C'9'                                                     
         BH    ERRRTYP                                                          
VR1790   MVC   ERATE,ESTRTYP                                                    
VR1798   XIT1                                                                   
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
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR1820                                                           
         CLI   EDAILY,C'Y'                                                      
         BNE   VR1810                IF ADDING A DAILY ESTIMATE, MAX-           
         MVC   ERRNUM,=AL2(DLYERR)   IMUM 53 DAYS BETWEEN START AND             
         L     RF,ACOMFACS           END DATE                                   
         USING COMFACSD,RF                                                      
         GOTO1 PERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                      
         LH    R4,8(R1)                                                         
         CH    R4,=H'53'                                                        
         BH    SPERREX                                                          
         DROP  RF                                                               
*                                                                               
VR1810   MVC   KEY(13),ESTKEY        ADD THE ESTIMATE AND UPDATE CASH           
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   VR1815                                                           
         MVC   ELEN,=Y(ESTHDRLN)     ESTIMATE RECORD WITH TRADE                 
         TM    USRIDFLG,USRRNTKQ     TEST ACCESS TO COMSCORE?                   
         BZ    *+10                    NO                                       
         MVC   ELEN,=AL2(ESTHDR2Q)    YES, SET EXTENDED ELEN                    
*                                                                               
VR1815   OI    ECNTRL,X'01'          PRODUCT CODE                               
         MVC   EPRDCD+1(1),BPRD                                                 
         MVC   AIO,AIO1                                                         
         GOTO1 DATCON,DMCB,(5,0),(2,ECRDATE)       DATE OF CREATION             
*                                  ZAP NEW PACKED ACCUMS                        
         ZAP   ECURPDN,=PL6'0'                                                  
         LA    RE,26                                                            
         LA    RF,EORD                                                          
         ZAP   0(6,RF),=PL6'0'                                                  
         AHI   RF,6                                                             
         BCT   RE,*-10                                                          
         LA    RE,13                                                            
         LA    RF,EAUTH                                                         
         ZAP   0(6,RF),=PL6'0'                                                  
         AHI   RF,6                                                             
         BCT   RE,*-10                                                          
         LA    RE,26                                                            
         LA    RF,EPAID                                                         
         ZAP   0(6,RF),=PL6'0'                                                  
         AHI   RF,6                                                             
         BCT   RE,*-10                                                          
*                                                                               
         MVC   EDEMLST(4),=X'00C901FF'     ALWAYS GIVE HOME CATEGORY            
*                                          ON ADD SINCE DEMOS IS ON             
*                                          DIFFERENT SCREEN                     
         GOTO1 =A(ADREC),RR=RELO                                                
         MVC   ESTKEY,KEY                                                       
         B     VR1840                                                           
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
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
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
         CH    R4,=H'53'                                                        
         BH    SPERREX                                                          
*                                                                               
VR1830   GOTO1 DATCON,DMCB,(5,0),(2,ECHDATE)     DATE OF CHANGE                 
         GOTO1 =A(PTREC),RR=RELO         PUT THE ESTIMATE                       
*                                                                               
********************************************************************            
*                                                                               
         CLC   KEY+13(1),ECNTRL                                                 
         BNE   *+12                                                             
         CLI   WORK,C'S'             IF STATUS WAS CHANGED...                   
         BNE   VR1840                READ EST REC...WRITE CNTRL                 
         GOTO1 READ                  BYTE TO KEY                                
*                                                                               
         NI    ECNTRL,X'FF'-ESTSTW                                              
         CLI   ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                       
         BNE   *+8                                                              
         OI    ECNTRL,ESTSTW                                                    
*                                                                               
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         B     VR1865                                                           
********************************************************************            
*                                                                               
*  BYPASS CASH PROD LOGIC NO LONGER SUPPORTED IN NET                            
VR1840   B     VR1860                                                           
*                                                                               
***  VR1840   OC    CASHPRD,CASHPRD                                             
         BZ    VR1850                                                           
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
VR1850   OC    OLDCASH,OLDCASH                                                  
         BZ    VR1860                                                           
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
VR1860   L     R3,AIO1                                                          
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
*  MAKE SURE EVERYTHING IS FILLED IN                                            
         CLC   QCLT,=C'000'        BUG CATCHER                                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   QMED,SPACES                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   QCLT,SPACES                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   QPRD,SPACES                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVI   61(R1),C'N'                                                      
         MVC   68(7,R1),=C'CONTROL'                                             
         MVI   65(R1),C'A'                                                      
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   65(R1),C'C'                                                      
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVI   WORK,0                                                           
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR1865              DONT UPDATE BUYLINE ON ADD                   
*                                                                               
         CLC   ECOST2,SVECOST2     ECOST2 CHANGED?                              
         BE    VR1865                                                           
*                                                                               
         XC    WORK,WORK             CHECK PROFILE - C3 REQUEST NEEDED?         
         MVC   WORK+16(4),=C'S0B0'   B0 PROFILE                                 
         MVC   WORK+20(2),AGENCY     PROFILE NAME                               
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK+1,C'Y'                                                      
         BNE   VR1865                                                           
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
         ZIC   R0,BEST                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  23(3,R1),DUB                                                     
         MVC   68(7,R1),=C'CONTROL'                                             
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'REQUEST',AIO,AIO                       
         MVI   WORK,0              SO I'LL REFORMAT REC                         
*                                                                               
VR1865   MVC   AIO,AIO1                                                         
         XIT1                                                                   
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
         CLI   ACTNUM,ACTADD                                                    
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
CHKS05   MVC   ERRNUM,=AL2(STATERR1)                                            
         CLC   ESTSTAT(4),=C'LOCK'   LOCK IS INVALID STATUS FOR ADDS            
         BNE   CHKS10                AND PREVIOUSLY HELD ESTIMATES              
         CLI   ACTNUM,ACTADD                                                    
         BE    SPERREX                                                          
         OI    ECNTRL,X'08'                                                     
         OI    KEY+13,X'08'                                                     
*                                                                               
         MVC   ERRNUM,=AL2(STATERR2)                                            
         TM    SVCNTRL,X'04'         LOCK HAS BEEN VALIDATED ...                
         BNZ   SPERREX               TURN ON X'08' BIT                          
         TM    SVCNTRL,X'08'         IF ESTIMATE WASN'T PREVIOUSLY              
         BO    CHKXIT                LOCKED SET STATUS CHANGED BYTE             
         B     CHKS60                AND EXIT                                   
*                                                                               
***********************************************************************         
*                                                                               
CHKS10   MVC   ERRNUM,=AL2(STATERR3)                                            
         CLC   ESTSTAT(4),=C'HOLD'   HOLD IS INVALID STATUS FOR ADDS            
         BNE   CHKS20                                                           
         CLI   ACTNUM,ACTADD                                                    
         BE    SPERREX                                                          
         OI    ECNTRL,X'0C'          HOLD HAS BEEN VALIDATED ...                
         OI    KEY+13,X'0C'          TUNR ON '0C' BIT                           
         TM    SVCNTRL,X'0C'         IF ESTIMATE WASN'T PREVIOUSLY              
         BO    CHKXIT                HELD SET STATUS CHANGED BYTE               
         B     CHKS60                AND EXIT                                   
*                                                                               
***********************************************************************         
*                                                                               
CHKS20   MVC   ERRNUM,=AL2(STATERR4)                                            
         CLC   ESTSTAT(6),=C'UNLOCK'                                            
         BNE   CHKS30                UNLOCK IS INVALID STATUS FOR ADDS,         
         CLI   ACTNUM,ACTADD         HELD ESTIMATES AND ESTIMATES WHICH         
         BE    SPERREX               WEREN'T PREVIOUSLY LOCKED                  
         NI    ECNTRL,X'F7'                                                     
         NI    KEY+13,X'F7'                                                     
*                                                                               
         MVC   ERRNUM,=AL2(STATERR5)                                            
         TM    SVCNTRL,X'04'                                                    
         BNZ   SPERREX                                                          
         MVC   ERRNUM,=AL2(STATERR6)                                            
         TM    SVCNTRL,X'08'         UNLOCK HAS BEEN VALIDATED ...              
         BZ    SPERREX               TURN OFF X'F7' BIT, SET STATUS             
         B     CHKS60                CHANGED BYTE AND EXIT                      
*                                                                               
***********************************************************************         
*                                                                               
CHKS30   MVC   ERRNUM,=AL2(STATERR7)                                            
         CLC   AGENCY,=C'JW'         FOR AGENCY JWT, DO NOT ALLOW               
         BNE   CHKS40                'REL' ONLY 'DBT'                           
         MVC   ERRNUM,=AL2(STATERR8)                                            
         CLC   ESTSTAT(3),=C'DBT'                                               
         BE    CHKS50                                                           
         B     CHKS55                                                           
CHKS40   CLC   ESTSTAT(3),=C'REL'    REL (OR DBT) ARE INVALID FOR ADDS          
         BNE   CHKS55                                                           
CHKS50   CLI   ACTNUM,ACTADD                                                    
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(STATERR9) REL (OR DBT) MUST BE FOLLOWED BY           
         CLI   5(R2),5               THE 2 DIGITS OF THE CURRENT DAY            
         BNE   SPERREX                                                          
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         CLC   WORK+4(2),11(R2)                                                 
         BNE   SPERREX                                                          
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
         MVC   ERRNUM,=AL2(STATERRA)                                            
         TM    SVCNTRL,X'04'         CAN'T USE DATE IF ESTIMATE PREV-           
         BNZ   SPERREX               IOUSLY HELD                                
*                                                                               
         MVC   ERRNUM,=AL2(STATERRB)                                            
         TM    SVCNTRL,X'08'         CAN'T USE DATE IF ESTIMATE PREV-           
         BNZ   SPERREX               IOUSLY LOCKED                              
*                                                                               
         L     R3,AIO1                                                          
         MVC   ERRNUM,=AL2(ESTERR)   MONTH MUST BE IN EST PERIOD                
         CLC   WORK+20(4),SVSTRT                                                
         BL    SPERREX                                                          
         CLC   WORK+20(4),SVEND                                                 
         BH    SPERREX                                                          
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
         CLI   ACTNUM,ACTADD         MUST BE VALID FOR CHANGE) ... AND          
         BNE   ERRSTAT               (STATUS MUST BE 'OLD') OR (STATUS          
CHKS90   NI    EPRDCD,X'7F'          MUST BE 'NEW' AND MEDIA MUST BE            
         CLC   ESTSTAT(3),=C'OLD'    'NETWORK') ... DON'T SET STATUS            
         BE    CHKXIT                CHANGED BYTE                               
         CLC   ESTSTAT(3),=C'NEW'                                               
         BNE   ERRSTAT                                                          
         CLI   QMED,C'N'                                                        
         BNE   ERRMEDC                                                          
         OI    EPRDCD,X'80'                                                     
*                                                                               
CHKXIT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*****************************************************************               
*        CHECK MASTER TERMINAL/LINE ID                          *               
*****************************************************************               
CKLINID  NTR1 BASE=*,LABEL=*                                                    
         LA    R1,LINIDTAB                                                      
CKL10    CLI   0(R1),X'FF'                                                      
         BE    CKLNO                                                            
         CLC   LINID(8),0(R1)                                                   
         BE    CKLYES                                                           
         LA    R1,8(R1)                                                         
         B     CKL10                                                            
CKLYES   SR    R1,R1                                                            
CKLNO    LTR   R1,R1                                                            
         XIT1                                                                   
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
         ZIC   R4,DUB                                                           
         ZIC   R5,DUB+3                                                         
         SR    R5,R4                                                            
         BZ    CHKEDTY               SAME YEAR IS ALWAYS OK                     
         CH    R5,=H'1'                                                         
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
         MVC   ERRNUM,=AL2(DSPERR)   (INVALID DATE SPREAD)                      
*******  CLC   =C'SJ',AGENCY         SJ HAS NO TALENT RECORDS                   
*******  BE    SPERREX                                                          
*******  GOTO1 =A(CHKTAL),RR=RELO                                               
*******  BNE   SPERREX                                                          
         B     SPERREX                                                          
*                                                                               
CHKEDTY  MVC   ERRNUM,=AL2(DSPERR)   (INVALID DATE SPREAD)                      
         LA    R2,ESTSTRDH           CURSOR TO START DATE                       
         CR    R3,R3                                                            
         BNZ   SPERREX                                                          
CHKEDTX  XIT1                                                                   
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
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        SPACK                                                        *         
***********************************************************************         
SPACK    NTR1 BASE=*,LABEL=*                                                    
         SR   R0,R0                                                             
         ZAP  DUB,=P'0'                                                         
         ZIC  R1,5(R2)                                                          
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
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
*                       NEXTEL                                       *          
**********************************************************************          
NEXTEL   ZIC   R0,1(R5)                                                         
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
         ZIC   R1,PSNLEN                                                        
         CH    R1,=H'11'           CAN'T BE >11                                 
         BNH   *+8                                                              
         LA    R1,11                                                            
         SH    R1,=H'1'                                                         
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
         ZIC   R1,PSNLEN                                                        
         CH    R1,=H'10'           CAN'T BE >10                                 
         BNH   *+8                                                              
         LA    R1,10                                                            
         SH    R1,=H'1'                                                         
         BM    PAR32                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BFLD2(0),0(R4)                                                   
PAR32    LR    R2,R6               NEXT FIELD                                   
         LA    R5,BSCANLNQ(R5)                                                  
         B     PAR10                                                            
*                                                                               
PARX     XIT1                                                                   
         LTORG                                                                  
         DROP  R5,R2                                                            
         EJECT                                                                  
***********************************************************************         
*        EDTUSR                                                       *         
***********************************************************************         
EDTUSR   NTR1 BASE=*,LABEL=*                                                    
         L     R3,AUSR               ANY INPUT IN USER FIELD?                   
         CLI   5(R3),0                                                          
         BNE   EDTUSR10                                                         
*                                                                               
         TM    FLAG1,CFLGREQQ        NO INPUT ... WAS IT REQUIRED?              
         BZ    EDTXIT                IF NOT, EXIT                               
         B     ERRMIS                IF YES, ERROR                              
*                                                                               
EDTUSR10 MVC   ERRNUM,=AL2(TOOBIG)                                              
         CLC   LEN,5(R3)             CHECK LENGTH OF INPUT                      
         BL    SPERREX                                                          
*                                                                               
         CLI   UTYPE,C' '            IS TYPE SUPPOSED TO BE 'WILD'?             
         BNH   EDTUSR80                                                         
*                                                                               
*                                                                               
         CLI   UTYPE,C'C'            IF TYPE IS CHARACTER...                    
         BNE   EDTUSR60              INPUT CANNOT BE NUMERIC                    
         LA    R4,8(R3)                                                         
         ZIC   R1,5(R3)                                                         
EDTUSR40 MVC   ERRNUM,=AL2(EDTERR1)                                             
         CLI   0(R4),C'0'                                                       
         BL    EDTUSR50                                                         
         CLI   0(R4),C'9'                                                       
         BNH   SPERREX                                                          
EDTUSR50 LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,EDTUSR40                                                      
         B     EDTUSR80                                                         
*                                                                               
*                                                                               
EDTUSR60 CLI   UTYPE,C'N'            IF TYPE IS NUMERIC...                      
         BNE   EDTUSR70                                                         
         GOTO1 =A(CHKNTYP),RR=RELO   INPUT MUST BE ALL NUMERIC                  
         BE    EDTUSR80                                                         
         MVC   ERRNUM,=AL2(EDTERR2)                                             
         B     SPERREX                                                          
*                                                                               
*                                                                               
EDTUSR70 MVC   ERRNUM,=AL2(BADDATE)                                             
         CLI   UTYPE,C'D'            IS TYPE DATE...                            
         BE    *+6                                                              
         DC    H'0'                  IF NO, BAD TYPE                            
         GOTO1 DATVAL,DMCB,(0,8(R3)),WORK                                       
         OC    DMCB(4),DMCB          INPUT MUST BE VALID DATE                   
         BZ    SPERREX                                                          
         L     R1,0(R1)                                                         
         ZIC   R4,5(R3)                                                         
         SR    R1,R4                                                            
         BNZ   SPERREX                                                          
*                                                                               
EDTUSR80 ZIC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,USERDATA,8(R3)     MOVE INPUT INTO USERDATA                   
EDTXIT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE RATING BOOK & HUT ADJUST             *         
***********************************************************************         
VR1900   NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R3                                                        
         L     R3,AIO                                                           
         LA    R2,ESTRBKH            RATING BOOK REQUIRED                       
         MVC   ERRNUM,=AL2(RATERR1)                                             
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         XC    EBOOK,EBOOK           IF NETPAK, RATING BOOK MUST BE             
         CLI   OVSYS,3               GREATER THAN ZERO                          
         BNE   VR1940                                                           
         CLI   8(R2),C'0'                                                       
         BL    ERRHTBK                                                          
         B     VR1945                                                           
*                                                                               
VR1940   CLC   8(6,R2),=C'LATEST'    IF NOT NETPAK AND RATING BOOK IS           
         BE    VR1950                'LATEST' DO NOT VALIDATE FOR DATE          
*                                                                               
VR1945   MVC   ERRNUM,=AL2(INVHTDT)           VALIDATE RATING BOOK FOR          
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK     M/Y AND SAVE INTO RECORD          
         OC    DMCB(4),DMCB                                                     
         BZ    SPERREX                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   EBOOK,WORK+10                                                    
*                                                                               
VR1950   CLI   EMSTRIND,0            RATING BOOK CANNOT BE CHANGED ON           
         BE    VR1955                MASTER OR SUB-ESTIMATE, NON-POL            
         CLI   ACTNUM,ACTADD         ESTIMATES                                  
         BE    VR1955                                                           
         CLC   SVBOOK,EBOOK                                                     
         BE    VR1955                                                           
         CLC   QPRD,=C'POL'                                                     
         BNE   ERRNOCHG                                                         
VR1955   CLI   POLSW,0                                                          
         BE    VR1965                                                           
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1960                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER3) ESTIMATE) ... RATING BOOK FOR              
         CLC   POLBOOK,EBOOK         BRAND ESTIMATES MUST MATCH THE             
         BNE   SPERREX               POL'S RATING BOOK                          
         B     VR1965                                                           
*                                                                               
VR1960   MVC   SVBOOK,EBOOK          IF (ADDING A POL ESTIMATE) OR              
*                                    (CHANGING A POL ESTIMATE) ...              
*                                    SAVE NEW RATING BOOK INTO SVBOOK           
*                                                                               
***********************************************************************         
*                                                                               
VR1965   LA    R2,ESTHUTH            HUT ADJUSTMENT REQUIRED                    
         CLI   5(R2),0                                                          
         BNE   *+14                  IF NOT THERE, FILL IN AUTO                 
         MVC   8(4,R2),=C'AUTO'                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVI   EHUTADJ,0             IF HUT ADJUSTMENT NOT 'AUTO' ...           
         CLC   8(4,R2),=C'AUTO'      MUST BE 3 CHARACTERS LONG AND A            
         BE    VR1970                VALID MONTH                                
         CLI   5(R2),3                                                          
         BNE   ERRHUTA                                                          
*                                                                               
         MVC   ERRNUM,=AL2(INVHADT)                                             
         MVC   WORK(3),8(R2)                                                    
         MVC   WORK+3(3),=C'/77'                                                
         GOTO1 DATVAL,DMCB,(2,WORK),WORK+10                                     
         OC    DMCB(4),DMCB                                                     
         BZ    SPERREX                                                          
         PACK  DUB,WORK+12(2)                                                   
         CVB   R0,DUB                                                           
         SLL   R0,4                                                             
         STC   R0,EHUTADJ                                                       
*                                                                               
VR1970   CLI   EMSTRIND,0            HUT ADJUSTMENT CANNOT BE CHANGED           
         BE    VR1975                ON MASTER OR SUB-ESTIMATE, NON-            
         CLI   ACTNUM,ACTADD         POL ESTIMATES                              
         BE    VR1975                                                           
         CLC   SVHUT,EHUTADJ                                                    
         BE    VR1975                                                           
         CLC   QPRD,=C'POL'                                                     
         BNE   ERRNOCHG                                                         
VR1975   CLI   POLSW,0                                                          
         BE    VR1900X                                                          
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1980                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER5) ESTIMATE) ... HUT ADJUSTMENT FOR           
         CLC   POLHUT,EHUTADJ        BRAND ESTIMATE MUST MATCH POL'S            
         BNE   SPERREX               HUT ADJUST.                                
         B     VR1900X                                                          
*                                                                               
*                                    IF (ADDING A POL ESTIMATE) OR              
VR1980   MVC   SVHUT,EHUTADJ         (CHANGING A POL ESTIMATE) ...              
VR1900X  XIT1                        SAVE NEW HUT ADJUSTMENT INTO SVHUT         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE ADJUSTMENT                           *         
***********************************************************************         
VR2000   NTR1 BASE=*,LABEL=*                                                    
         CLC   QPRD,=C'POL'                                                     
         BNE   VR2000X                                                          
VR2042   XC    KEY,KEY                                                          
         MVC   KEY(4),ESTKEY                                                    
VR2050   GOTO1 =A(NEXTPRD),RR=RELO                                              
         BNE   VR2000X                                                          
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         CLI   POLSW,3                                                          
         BNE   VR2130                                                           
*                                                                               
         MVC   ERRNUM,=AL2(BRPOLER3) IF (ADDING A POL ESTIMATE) ...             
         LA    R2,ESTRBKH            POL MUST AGREE WITH ALL BRAND'S            
         CLC   EBOOK,SVBOOK          RATING BOOKS, HUT ADJUSTMENTS,             
         BNE   SPERREX               DAYPART MENUS AND CONTROLS                 
         MVC   ERRNUM,=AL2(BRPOLER5)                                            
         LA    R2,ESTHUTH                                                       
         CLC   EHUTADJ,SVHUT                                                    
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(BRPOLER4)                                            
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   SPERREX                                                          
         CLI   SVCLPROF,C'0'                                                    
         BE    SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR2130   CLC   EBOOK,SVBOOK          IF (CHANGING A POL ESTIMATE) ...           
         BNE   VR2180                MUST CHANGE RATING BOOKS, HUT ADJ-         
         CLC   EPWPCT,SVPOLPW        USTMENTS, DEPATMENT MENUS AND CON-         
         BNE   VR2180                TROLS OF ALL BRAND ESTIMATES TO            
         CLC   EHUTADJ,SVHUT         TO MATCH NEW POL VALUES                    
         BNE   VR2180                POL VALUES                                 
         CLC   EDAYMENU,SVDPT                                                   
         BNE   VR2180                                                           
         CLC   ETYPE,SVETYPE                                                    
         BNE   VR2180                                                           
         CLI   SVF0PROF,C'N'         FILTERS MUST BE CHANGED IF FIRST           
         BE    VR2140                BYTE OF SVF0PROF IS NOT N                  
         CLC   EPROF(3),SVFLTRS                                                 
         BNE   VR2180                                                           
VR2140   CLC   ECONTROL,SVECON                                                  
         BNE   VR2180                                                           
         CLI   SVF0PROF+1,C'N'       RETAIL SCHEMES MUST BE CHANGED IF          
         BE    VR2150                SECOND BYTE OF SVF0PROF IS NOT N           
         CLC   ERTLSCHM,SVRTL                                                   
         BNE   VR2180                                                           
*                                                                               
VR2150   LA    R4,4                  IF USER NAMES MATCH ...                    
         LA    RF,EUSRNMS            READ IN NEXT BRAND                         
         LA    RE,SVUSRNMS                                                      
VR2160   CLI   0(RF),C' '                                                       
         BNH   VR2170                                                           
         CLC   0(7,RF),0(RE)                                                    
         BNE   VR2180                                                           
VR2170   LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,VR2160                                                        
         B     VR2050                                                           
*                                                                               
VR2180   MVC   EHUTADJ,SVHUT         IF USER NAMES DON'T MATCH ... OR           
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
VR2190   CLI   0(RF),C' '                                                       
         BNH   VR2200                                                           
         MVC   0(7,RF),0(RE)                                                    
VR2200   LA    RF,7(RF)                                                         
         LA    RE,7(RE)                                                         
         BCT   R4,VR2190                                                        
         GOTO1 =A(PTREC),RR=RELO                                                
*                                                                               
         NI    KEY+13,X'FF'-ESTSTW                                              
         CLI   ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                       
         BNE   *+8                                                              
         OI    KEY+13,ESTSTW                                                    
         GOTO1 WRITE                                                            
*                                                                               
         B     VR2050                                                           
VR2000X  XIT1                        SAVE NEW HUT ADJUSTMENT INTO SVHUT         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       VALIDATE PROFILE FIELD                        *         
***********************************************************************         
VALPROF  NTR1 BASE=*,LABEL=*                                                    
         XC    CASHPRD,CASHPRD                                                  
         XC    OLDCASH,OLDCASH                                                  
         MVI   OLDCPRD,0                                                        
         XC    SVETYPE,SVETYPE                                                  
         NI    ECNTRL,X'FF'-ESTSTW                                              
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
VPRF00   XC    ECOST2,ECOST2       INITIALIZE COST FACTOR AND PROFIT            
         XC    EPWPCT,EPWPCT       WITHIN PERCENTAGE TO ZERO                    
         MVI   ETYPE,0                                                          
*                                                                               
         MVC   EDAILY,SVCLDLY      INITIALIZE DAILY ESTIMATE INDICATOR          
*                                  TO CLIENT DEFAULT                            
*                                                                               
         TM    SVCLOP1,COP1NMG     IF CLIENT USES NEW MAKEGOOD DATES            
         BNO   *+8                 TURN ON BIT IN ESTIMATE RECORD               
         OI    EFLAG1,EF1NMG                                                    
*                                                                               
         CLI   POLSW,1             ADD/CHA BRD EST WITH POL EXISTING            
         BNE   *+10                OPTION ETYPE SET TO SAME IN BRD              
         MVC   ETYPE,POLETYPE      EST AS POL                                   
*                                                                               
         CLI   POLSW,0                                                          
         BE    VPRF05              IF (ADDING POL ESTIMATE) ... DE-             
         CLI   POLSW,3             FAULT PROFIT WITHIN PERCENTAGE TO            
         BNE   VPRF04              CLIENT'S PROFIT WITHIN PERCENTAGE            
         MVC   EPWPCT,SVCLTPW                                                   
         B     VPRF05              IF (ADDING BRAND ESTIMATE WITH EX-           
VPRF04   CLI   ACTNUM,ACTADD       ISTING POL ESTIMATE) ... DEFAULT             
         BNE   VPRF05              BRAND PROFIT WITHIN PERCENTAGE TO            
         MVC   EPWPCT,SVPOLPW      POL'S PROFIT WITHIN PERCENTAGE               
*                                                                               
***********************************************************************         
*                                                                               
VPRF05   NI    EFLAG1,X'FF'-EF1COS2I   INITIALIZE INTEG COS2 BIT                
         XC    ECOST2,ECOST2       CLEAR COS2 FIELD                             
         LA    R2,ESTOPTH          USER DECLARED OPTIONS                        
         CLI   5(R2),0                                                          
         BE    VPRFX                                                            
         OC    ECPPEST,ECPPEST     CANNOT HAVE USER DECLARED OPTIONS IF         
         BNZ   ERROPTN             CPP ESTIMATE PRESENT                         
*                                                                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   DMCB+4,0                                                         
         BE    ERROPTN                                                          
         LA    R5,BLOCK                                                         
         USING SCAND,R5                                                         
         ZIC   R0,DMCB+4             NUMER OF OPTIONS                           
         XC    ECGTPCT,ECGTPCT                                                  
*                                                                               
***********************************************************************         
*                                                                               
VPRF10   ZIC   R1,FLD1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FLD1(0),=C'DAILY'     IF DAILY OPTION ...                        
         BNE   VPRF20                                                           
         MVC   EDAILY,FLD2                                                      
         CLI   EDAILY,C'N'                                                      
         BE    VPRF100                                                          
         CLI   EDAILY,C'Y'           MUST BE Y OR N                             
         BNE   ERROPTN                                                          
*                                                                               
*                                    SPREAD BETWEEN START AND END               
         MVC   ERRNUM,=AL2(DAILYERR) DATES MUST BE LESS THAN OR EQUAL           
         L     RF,ACOMFACS           TO 53 DAYS                                 
         USING COMFACSD,RF                                                      
         GOTO1 CPERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                     
         LH    R4,8(R1)                                                         
         CH    R4,=H'53'                                                        
         BH    SPERREX               SET DAILY ESTIMATE INDICATOR               
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF20   CLI   FLD1LEN,2           IF PW OPTION ...                             
         BNE   VPRF30                                                           
         CLC   FLD1(2),=C'PW'                                                   
         BNE   VPRF30                                                           
         OC    SVCLTPW,SVCLTPW     CLIENT MUST BE A PW CLIENT                   
         BZ    ERROPTN                                                          
         ZIC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(2,FLD2),(R4)                                       
         CLI   DMCB,X'00'                                                       
         BNE   ERRMONT             MUST BE VALID MONETARY                       
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
         MVC   ERRNUM,=AL2(POLRQERR) MUST BE A POL ESTIMATE                     
         CLC   QPRD,=C'POL'                                                     
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(REQERR1)                                             
         NI    EFLAG1,X'FF'-EF1REQ   MUST BE A Y OR N                           
         CLI   FLD2,C'N'                                                        
         BE    VPRF100                                                          
         CLI   FLD2,C'Y'                                                        
         BNE   SPERREX                                                          
         OI    EFLAG1,EF1REQ         IF Y ... SET SF JWT REQ=Y FLAG             
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF31   CLC   FLD1(4),=C'TYPE'      IF TYPE OPTION ...                         
         BNE   VPRF35                                                           
*                                                                               
         MVC   ERRNUM,=AL2(TYPERR)                                              
*                                                                               
         CLC   FLD2(3),=C'STW'                                                  
         BNE   VPRF32                                                           
         MVI   ETYPE,C'S'                                                       
         CLI   ACTNUM,ACTADD                                                    
         BE    *+8                                                              
         MVI   WORK,C'S'                                                        
         B     VPRF33                                                           
*                                                                               
VPRF32   CLC   FLD2(3),=C'REG'                                                  
         BNE   SPERREX                                                          
         MVI   ETYPE,C'R'                                                       
*                                                                               
VPRF33   CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VPRF34                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLERA) ESTIMATE) ... ETYPE FOR                    
         CLC   ETYPE,POLETYPE        BRAND ESTIMATE MUST MATCH THE              
         BNE   SPERREX               POL'S ETYPE                                
*                                                                               
VPRF34   MVC   SVETYPE,ETYPE                                                    
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF35   CLC   FLD1(3),=C'NMG'       IF NMG OPTION ...                          
         BNE   VPRF36                                                           
         OI    EFLAG1,EF1NMG         IF NOT A DDS TERMINAL, MUST BE Y           
         CLI   FLD2,C'Y'             IF DDS TERMINAL CAN BE N OR Y              
         BE    VPRF100                                                          
         CLI   TWAOFFC,C'*'                                                     
         BNE   ERROPTN                                                          
         CLI   FLD2,C'N'                                                        
         BNE   ERROPTN                                                          
         NI    EFLAG1,X'FF'-EF1NMG   IF N ... TURN OFF NEW MAKEGOODS            
         B     VPRF100               FLAG                                       
*                                                                               
***********************************************************************         
*                                                                               
VPRF36   CLC   FLD1(3),=C'CGT'       IF CGT OPTION ...                          
         BNE   VPRF37                                                           
         MVC   ERRNUM,=AL2(CGTERR)                                              
         ZIC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(2,FLD2),(R4)                                       
         CLI   DMCB,X'00'                                                       
         BNE   SPERREX               MUST BE MONETARY AND EQUUAL TO OR          
         CLC   DMCB+4(4),=F'9999'    LESS THAN $9.99                            
         BH    SPERREX                                                          
         MVC   ECGTPCT,DMCB+6        SET CLIENT GROSS TRADE PERCENTAGE          
         B     VPRF100               (TBS)                                      
*                                                                               
***********************************************************************         
*                                                                               
VPRF37   CLC   FLD1(5),=C'DEMOS'     IF DEMOS OPTION ...                        
         BNE   VPRF38                                                           
         MVC   ERRNUM,=AL2(ODEERR)                                              
         NI    EFLAG1,X'FF'-EF1NODEM                                            
         CLI   FLD2,C'Y'             MUST BE Y OR N                             
         BE    VPRF100                                                          
         CLI   FLD2,C'N'                                                        
         BNE   SPERREX               IF N ... SET FLAG FOR NO DEMOS             
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
         BE    VPRF39B                                                          
         CLC   FLD1(4),=C'ICOS2'    IF COS2 OPTION WITH INTEGRATION...          
         BNE   VPRF40                                                           
         OI    EFLAG1,EF1COS2I                                                  
VPRF39B  MVC   ERRNUM,=AL2(MULTCOS2)                                            
         OC    ECOST2,ECOST2        WAS COS2 INPUTTED PREVIOSLY                 
         BNZ   SPERREX              YES ERROR                                   
         MVC   ERRNUM,=AL2(COS2ERR)                                             
         ZIC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(6,FLD2),(R4)                                       
         CLI   DMCB,0                                                           
         BNE   SPERREX              MUST BE MONETARY AND FIT BETWEEN            
         CLC   4(4,R1),=F'9999999'  0 AND $9.99                                 
         BH    SPERREX                                                          
         CLC   4(4,R1),=F'0'                                                    
         BL    SPERREX                                                          
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
******** BNE   VPRF45                                                           
*  BYPASS CASH AND TRADE CHECK NOT SUPPORTED IN NET                             
         BNE   ERROPTN                                                          
*                                                                               
         CLC   =C'DEL',FLD2          IF DEL, REMOVE RESTRICTION FOR             
         BNE   VPRF41                BUYING ONLY THIS SPOT LENGTH               
         MVI   ESLN,0                                                           
         B     VPRF100                                                          
*                                                                               
VPRF41   MVC   ERRNUM,=AL2(SPTLNERR)                                            
         LA    R1,SLNTABC                                                       
         ICM   R4,15,FLD2B                                                      
VPRF42   CLI   0(R1),X'FF'                                                      
         BE    SPERREX               IF NOT DEL, CHECK IF VALID SPOT            
         ZIC   RE,0(R1)              LENGTH (5,10,15,20,30,40,45,50,            
         CR    RE,R4                 60,75,90,120) ... AND SAVE INTO            
         BE    VPRF43                SLN FIELD                                  
         LA    R1,1(R1)                                                         
         B     VPRF42                                                           
VPRF43   STC   R4,ESLN                                                          
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF45   CLC   FLD1(4),=C'CASH'      IF CASH OPTION ...                         
         BNE   VPRF50                                                           
         MVC   ERRNUM,=AL2(CASH1)                                               
         CLI   FLD2LEN,3             CASH PRODUCT MUST BE 3 CHARACTERS          
         BNE   ERROPTN               LONG                                       
*                                                                               
         MVC   ERRNUM,=AL2(CASHTRD)  CASH PRD CANNOT BE TRADE                   
         CLI   FLD2+2,C'#'                                                      
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(NOCSHEST) CASH ESTIMATE MUST BE ON FILE ...          
         XC    KEY,KEY                                                          
         MVC   KEY(8),ESTKEY                                                    
         MVC   KEY+4(3),FLD2                                                    
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(8),KEY                                                   
         BNE   SPERREX                                                          
*                                                                               
         LHI   R1,MAXPRD                                                        
         LA    R6,SVCLIST                                                       
VPRF46   CLI   0(R6),0                                                          
         BE    SPERREX                                                          
         CLC   FLD2(3),0(R6)                                                    
         BE    VPRF47                                                           
         LA    R6,4(R6)                                                         
         BCT   R1,VPRF46                                                        
         B     SPERREX                                                          
*                                                                               
VPRF47   MVC   ERRNUM,=AL2(CASHSET)                                             
         CLI   SVTRDPRD,0            IF TRADE PRODUCT IS NOT THE SAME           
         BE    *+14                  AS CURRENT PRODUCT ... ERROR               
         CLC   SVTRDPRD,SVPRD                                                   
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(CASHCHG)                                             
         CLC   ECASHPRD,3(R6)        IF INPUTTED CASH PRODUCT HAS               
         BE    VPRF48                CHANGED ... SAVE OLD CASH PRODUCT          
         CLI   ECASHPRD,0            INTO OLD CASH                              
         BE    VPRF48                                                           
         MVC   OLDCPRD,ECASHPRD                                                 
         CLI   SONIA,C'C'            UNLESS SONIA=C ... CANNOT CHANGE           
         BNE   SPERREX               CASH PRODUCT                               
         TM    SVCLOP2,COP2DIY       DIY TRADE                                  
         BO    SPERREX               CANNOT CHANGE                              
VPRF48   MVC   ECASHPRD,3(R6)                                                   
         CLI   SVTRDPRD,0            MOVE INPUTTED CASH PRODUCT IN              
         BNE   *+10                                                             
         MVC   CASHPRD,FLD2                                                     
*                                                                               
         CLI   OLDCPRD,0             DELETE CASH PRODUCT FROM CLIENT'S          
         BE    VPRF49                CLIST                                      
         LHI   R1,MAXPRD                                                        
         LA    R6,SVCLIST                                                       
VPRF48A  CLI   0(R6),0                                                          
         BE    ERROPTN                                                          
         CLC   OLDCPRD,3(R6)                                                    
         BE    VPRF48B                                                          
         LA    R6,4(R6)                                                         
         BCT   R1,VPRF48A                                                       
         B     ERROPTN                                                          
VPRF48B  MVC   OLDCASH,0(R6)                                                    
VPRF49   MVC   KEY,ESTKEY                                                       
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF50   CLC   FLD1(3),=C'TRD'       TRD IS ONLY OTHER VALID OPTION ...         
         BNE   ERROPTN               NO VALIDATION                              
*                                                                               
***********************************************************************         
*                                                                               
VPRF100  LA    R5,32(R5)             BUMP TO NEXT OPTION                        
         BCT   R0,VPRF10                                                        
         DROP  R5                                                               
*                                                                               
***********************************************************************         
*                                                                               
VPRFX    DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VPRFX1                                                           
         CLI   ETYPE,C'S'                                                       
         BNE   VPRFX1                                                           
         MVC   ERRNUM,=AL2(TYPEERR1)                                            
         BRAS  RE,CHKTYPE                                                       
         BNE   SPERREX                                                          
*                                                                               
VPRFX1   OC    SVCLTPW,SVCLTPW       IF CLIENT SET FOR PROFIT WITHIN            
         BZ    VPRFX6                PERCENTAGE ...                             
*                                                                               
         CLC   =C'WI',AGENCY         FOR AGENCY WI, IF END DATE IS              
         BNE   VPRFX2                BEFORE APR/95 ... CLEAR PROFIT             
         CLC   EEND,=C'950326'       WITHIN PERCENTAGE                          
         BH    VPRFX2                                                           
         XC    EPWPCT,EPWPCT                                                    
         B     VPRFX10                                                          
*                                                                               
VPRFX2   MVC   ERRNUM,=AL2(NOPWPCT)  IF AGENCY IS NOT WI, THEN PROFIT           
         OC    EPWPCT,EPWPCT         WITHIN PERCENTAGE IS REQUIRED              
         BZ    SPERREX                                                          
*                                                                               
         CLC   =C'POL',QPRD          IF POL ESTIMATE ... SAVE PROFIT            
         BNE   VPRFX4                WITHIN PERCENTAGE INTO SVPOLPW             
         MVC   SVPOLPW,EPWPCT                                                   
         B     VPRFX6                                                           
*                                                                               
VPRFX4   MVC   ERRNUM,=AL2(NOTPOLPW) IF (CHANGING OR ADDING BRAND EST-          
         CLC   SVPOLPW,EPWPCT        IMATE WITH EXISTING POL ESTIMATE)          
         BNE   SPERREX               ... PWP'S MUST MATCH                       
*                                                                               
VPRFX6   OC    EPWPCT,EPWPCT                                                    
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
         OC    ECOST2,ECOST2         FACTOR FROM CLIENT RECORD                  
         BNZ   VPRFX11                                                          
         NI    EFLAG1,X'FF'-EF1COS2I                                            
         MVC   ECOST2,SVCCOST2                                                  
         TM    SVCLOP3,COP3CS2I      DOES COS2 COVER INTEGRATION                
         BZ    VPRFX11                                                          
         OI    EFLAG1,EF1COS2I                                                  
*                                                                               
***********************************************************************         
*                                                                               
VPRFX11  MVC   ERRNUM,=AL2(CASHINV)                                             
         CLI   QPRD+2,C'#'           CASH OPTION INVALID FOR NON-TRADE          
         BE    VPRFX11A              PRODUCTS                                   
         CLI   ECASHPRD,0                                                       
         BNE   SPERREX                                                          
         B     VPRFXIT                                                          
*                                                                               
VPRFX11A TM    SVCLOP2,COP2DIY       DIY TRADE                                  
         BNO   VPRFX11C                                                         
         CLI   ACTNUM,ACTADD                                                    
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
VPRFXIT  XIT1                                                                   
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
         XIT1                                                                   
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
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
***********************************************************************         
*        CHKNTYP                                                      *         
***********************************************************************         
* THIS ROUTINE CHECKS TO SEE IF PASSED VARIABLE IS VALID NUMERIC                
***********************************************************************         
CHKNTYP  NTR1 BASE=*,LABEL=*                                                    
         LA    R4,8(R3)              R4 ---> INPUT                              
         ZIC   R1,5(R3)              R1= L(INPUT)                               
CHKN10   LA    R5,VALDNTBL           R5 = TABLE OF VALID DIGITS                 
CHKN20   CLC   0(1,R4),0(R5)         VALID DIGIT?                               
         BE    CHKN30                                                           
         LA    R5,1(R5)              BUMP TO NEXT DIGIT                         
         CLC   0(1,R5),EOTBLQ        END OF TABLE?                              
         BE    XCHKN                                                            
         B     CHKN20                NO, TRY AGAIN                              
*                                                                               
CHKN30   LA    R4,1(R4)              CHECK NEXT CHAR IN INPUT                   
         BCT   R1,CHKN10                                                        
XCHKN    LTR   R1,R1                                                            
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                       PUT RECORD                                    *         
***********************************************************************         
*                                                                               
PTREC    NTR1 BASE=*,LABEL=*                                                    
         NI    ECNTRL,X'FF'-ESTSTW                                              
         CLI   ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                       
         BNE   *+8                                                              
         OI    ECNTRL,ESTSTW                                                    
*                                                                               
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                       ADD RECORD                                    *         
***********************************************************************         
*                                                                               
ADREC    NTR1 BASE=*,LABEL=*                                                    
         NI   ECNTRL,X'FF'-ESTSTW                                               
         CLI  ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                        
         BNE  *+8                                                               
         OI   ECNTRL,ESTSTW                                                     
*                                                                               
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
* CHECK IF ESTIMATE HAS BILLED OR PAID UNITS UNDER IT                           
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
         L     R4,AIO                                                           
         USING ESTHDR,R4                                                        
*                                                                               
         LA    R0,EPAIDX                                                        
         LA    R4,EPAID                                                         
         DROP  R4                                                               
CHKT40   CR    R0,R4                                                            
         BH    *+12                                                             
         MVI   KEY+8,X'FF'                                                      
         B     CHKT05                                                           
*                                                                               
         CLC   =XL6'00',0(R4)                                                   
         BE    *+14                                                             
         CP    0(L'EPAID,R4),=PL6'0'                                            
         BNE   CHKTXNEQ            ERROR EXIT, IF NONZERO                       
         LA    R4,L'EPAID(R4)                                                   
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
***********************************************************************         
* ADD THE PASSIVE POINTER                                             *         
***********************************************************************         
XADD     NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MEDTAB                                                        
XADD00   L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(13),0(R6)        READ THE KEY WE JUST ADDED/CHANGED          
         CLI   USERPROF+7,C'C'      CANADIAN?                                   
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
         USING ESTHDR,R6                                                        
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING EPKEY,R3                                                         
         MVI   EPKEYTYP,EPKEYTYQ    X'0D'                                       
         MVI   EPKEYSUB,EPKEYSBQ    X'F2'                                       
         MVC   EPKEYAM,KEYSAVE+1    A/M                                         
         MVC   EPKEYCLT,EKEYCLT     CLIENT                                      
         GOTO1 DATCON,DMCB,(0,ESTART),(2,EPKEYSDT)                              
         GOTO1 DATCON,DMCB,(0,EEND),(2,EPKEYEDT)                                
         MVC   EPKEYEST,EKEYEST     EST                                         
         MVC   EPKEYPRD,EKEYPRD     PRD                                         
*                                                                               
         MVC   KEY+14(4),WORK       SET DISK ADDRESS                            
         OI    DMINBTS,X'08'        PASS DELETES                                
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         BNE   XADD10               NO                                          
         NI    KEY+13,X'7F'         YES, UNDELETE                               
*                                                                               
         NI    EPKCNTRL,X'FF'-EPKSTSTW                                          
         CLI   ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                       
         BNE   *+8                                                              
         OI    EPKCNTRL,EPKSTSTW                                                
*                                                                               
         MVC   AIO,AIO2             DO NOT CLOBBER AIO2                         
         GOTO1 WRITE                AND WRITE BACK                              
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
         B     XADD20                                                           
*                                                                               
XADD10   MVC   KEY,KEYSAVE          RESTORE KEY                                 
*                                                                               
         CLI   ETYPE,ETYPSTW        STEWARDSHIP ESTIMATE?                       
         BNE   *+8                                                              
         OI    EPKCNTRL,EPKSTSTW                                                
*                                                                               
         MVC   AIO,AIO2             DO NOT CLOBBER AIO2                         
         GOTO1 ADD                                                              
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
XADD20   CLI   USERPROF+7,C'C'      CANADIAN?                                   
         BNE   XADDX                NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XADDX                NO, WE ARE DONE HERE                        
*                                                                               
XADD30   LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    XADDX                YES                                         
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
***********************************************************************         
* ESTIMATE RECORD WAS JUST CHANGED...IF DATES DID NOT CHANGE, EXIT    *         
* OTHERWISE, DELETE THE OLD PASSIVE AND ADD A NEW ONE                 *         
***********************************************************************         
XCHG     NTR1  BASE=*,LABEL=*                                                   
         LA    R2,MEDTAB                                                        
         XC    KEY,KEY                                                          
*                                                                               
XCHG00   L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
*!!      CLC   SVSTRT,ESTART        DID ESTIMATE START CHANGE?                  
*!!      BNE   XCHG05               YES IT DID                                  
*!!      CLC   SVEND,EEND           DID ESTIMATE END CHANGE?                    
*!!      BE    XCHGX                NO, LEAVE THE PASSIVE ALONE                 
*                                                                               
XCHG05   LA    R3,KEY                                                           
         USING EPKEY,R3                                                         
         MVI   EPKEYTYP,EPKEYTYQ    X'0D'                                       
         MVI   EPKEYSUB,EPKEYSBQ    X'F2'                                       
         MVC   EPKEYAM,EKEYAM       A/M                                         
*                                                                               
         CLI   USERPROF+7,C'C'      CANADIAN?                                   
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
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE      FOUND?                                      
         BNE   XCHG15               NO                          CC              
         OI    KEY+13,X'80'         MARK RECORD FOR DELETION                    
         MVC   AIO,AIO2             DO NOT CLOBBER AIO2                         
         GOTO1 WRITE                                                            
         MVC   AIO,AIO1             RESTORE NEWEST ESTIMATE RECORD              
*                                                                               
XCHG15   CLI   USERPROF+7,C'C'      CANADIAN?                                   
         BNE   XCHG30               NO                                          
         CLI   QMED,C'T'            MEDIA T?                                    
         BNE   XCHG30               NO, ONLY CHANGE 1 RECORD                    
*                                                                               
XCHG20   LA    R2,2(R2)             BUMP TO NEXT ENTRY IN MED TABLE             
         CLI   0(R2),X'FF'          END OF TABLE?                               
         BE    XCHG30               YES                                         
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
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        PROCESS PFKEY                                                *         
***********************************************************************         
CHKPFKEY NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PFAID,5                                                          
         BNE   CPFK10                                                           
         GOTO1 VINITPF,DMCB,MPFTABLE                                            
         J     XIT                                                              
*                                                                               
CPFK10   CLI   PFAID,6                                                          
         BNE   CPFK20                                                           
         GOTO1 VINITPF,DMCB,MPFTABLE                                            
         J     XIT                                                              
*                                                                               
CPFK20   CLI   PFAID,12                                                         
         BNE   CPFK30                                                           
         GOTO1 VINITPF,DMCB,MPFTABLE                                            
         J     XIT                                                              
*                                                                               
CPFK30   DS    0H                                                               
         J     XIT                                                              
***********************************************************************         
*        PF TABLE                                                     *         
***********************************************************************         
MPFTABLE DS    0X                                                               
         DC    AL1(MPF05X-*,05,PFTCPROG,(MPF05X-MPF05)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'ESTDEMOS'       RECORD: OFFICE                               
MPF05ACT DC    CL8'DISPLAY '       ACTION:                                      
MPF05    DC    AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T31CFFD)                   
MPF05X   EQU   *                                                                
*                                                                               
         DC    AL1(MPF06X-*,06,PFTCPROG,(MPF06X-MPF06)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'ESTDEMOS'       RECORD: OFFICE                               
MPF06ACT DC    CL8'CHANGE  '       ACTION:                                      
MPF06    DC    AL1(KEYTYTWA,L'ESTMEDK-1),AL2(ESTMEDK-T31CFFD)                   
MPF06X   EQU   *                                                                
*                                                                               
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
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
       ++INCLUDE SPGENEST          ESTIMATE RECORDS                             
       ++INCLUDE SPGENESTD         ESTIMATE RECORDS PASSIVE KEY                 
         EJECT                                                                  
       ++INCLUDE SPGENBILL         BILLING RECORDS                              
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFMBED          EST MAINTENANCE SCREEN                       
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
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
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
       ++INCLUDE NESFMWORKD                                                     
         EJECT                                                                  
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE+220                                                     
RELO     DS    A                   RELOCATION FACTOR                            
PROKEY   DS    CL13                                                             
ESTKEY   DS    CL13                                                             
WTEND    DS    A                                                                
*                                                                               
CASHPRD  DS    CL3                                                              
OLDCASH  DS    CL3                                                              
OLDCPRD  DS    XL1                                                              
*                                                                               
SVPRD    DS    X                                                                
SVPOLPW  DS    XL3                                                              
*                                                                               
SVCLTPOL DS    CL1                                                              
SVCLPROF DS    CL15                                                             
SVCLEX   DS    CL15                                                             
SVCLDLY  DS    CL1                                                              
SVCLTPW  DS    XL3                                                              
SVCCOST2 DS    XL4                                                              
*                                                                               
SVCLTDA  DS    XL4                                                              
SVPRDDA  DS    XL4                                                              
*VAGYFL1 DS    XL1                                                              
FADDR    DS    A                                                                
ERRAREA  DS    X                                                                
*VE1USER DS    CL20                                                             
SVEU1TYP DS    CL1                                                              
SVEU1LEN DS    XL1                                                              
SVEU1FL1 DS    XL1                                                              
SVEU1FL2 DS    XL1                                                              
*                                                                               
*VE2USER DS    CL20                                                             
SVEU2TYP DS    CL1                                                              
SVEU2LEN DS    XL1                                                              
SVEU2FL1 DS    XL1                                                              
SVEU2FL2 DS    XL1                                                              
*                                                                               
SVCLOP1  DS    XL1                                                              
SVCLOP2  DS    XL1                                                              
SVCLOP3  DS    XL1                                                              
SVF0PROF DS    CL16                                                             
*                                                                               
SVPPST   DS    CL10                                                             
*                                                                               
SCRNFLAG DS    X                                                                
AUSR     DS    A                                                                
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
USERDATA DS    CL32                                                             
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
WORK2    DS    CL48                                                             
SVADVDA  DS    CL4                                                              
SVADVAGY DS    XL1                                                              
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
*                                                                               
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
SVECON   DS    CL1                 ECONTROL                                     
SVEMGD   DS    CL2                                                              
SVCPP    DS    CL1                                                              
SVFLTRS  DS    CL3                 FILTERS                                      
SVRTL    DS    CL2                 RETAIL SCHEME                                
*                                                                               
SVNETYM  DS    XL2                                                              
SVBDSTM  DS    XL3                                                              
SVBDENDM DS    XL3                                                              
POLDATA  DS    0D                                                               
POLSTRT  DS    CL6                                                              
POLEND   DS    CL6                                                              
POLOWDAY DS    XL1                                                              
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
*                                                                               
POLDATAX EQU   *                                                                
CLIPRO   DS    CL10                                                             
SVDEMADR DS    A                                                                
*                                                                               
WTSFLAG  DS    XL1                                                              
WTST1    EQU   X'01'               ENTERED T1 DEMO WEIGHT                       
WTST2    EQU   X'02'               ENTERED T2 DEMO WEIGHT                       
*                                                                               
TEMPDEM1 DS    XL63                USED TO SAVE DEMO FROM RECORD                
TEMPDEM2 DS    XL63                USED TO SAVE POL DEMOS                       
TEMPDEM3 DS    XL63                USED TO SAVE THE NEW DEMOS                   
TEMPWGT1 DS    XL21                                                             
TEMPWGT2 DS    XL21                                                             
TEMPWGT3 DS    XL21                                                             
*                                                                               
* FOLLOWING FIELDS HAD TP BE MOVED TO KEEP                                      
* NESFM03 AND NESFM54 STORAGE IN SYNC                                           
*                                                                               
SAVEKEY  DS    XL13                                                             
PSTOUT   DS    CL64                                                             
SVADVLST DS    CL30                                                             
FAKEFLD  DS    XL11                                                             
SVADVKEY DS    XL13                                                             
SVESTIM  DS    0CL162                                                           
POLDEMOS DS    0CL124                                                           
PEDEMLST DS    CL60                CONVERTED DEMO FIELDS                        
PEWGTLST DS    XL20                                                             
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'095NESFM03   09/02/20'                                      
         END                                                                    
