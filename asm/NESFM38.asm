*          DATA SET NESFM38    AT LEVEL 001 AS OF 08/09/99                      
*PHASE T31C38A                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C38  -- ESTIMATE MAINTENANCE                      *         
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
         TITLE 'T31C38 - ESTIMATE MAINTENANCE'                                  
T31C38   CSECT                                                                  
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
         BAS    RE,SETUP                                                        
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
*                                                                               
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
***********************************************************************         
*                                                                               
         LA    R2,ESTMEDKH           MEDIA                                      
         GOTO1 VALIMED               VALIDATE MEDIA CODE AND TRANSMIT           
         MVC   ESTMEDN,MEDNM         MEDIA NAME                                 
         OI    ESTMEDNH+6,X'80'                                                 
*                                                                               
         L     RE,AIO                SAVE AGENCY RECORD DATA                    
         USING AGYHDR,RE                                                        
         MVC   SVAGYFL1,AGYFLAG1                                                
         DROP  RE                                                               
*                                                                               
         MVC   EKEYAM,BAGYMD         COPY MEDIA INTO KEY                        
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTCLIKH           CLIENT                                     
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
         MVC   SVCLTPOL,CPOLONLY                                                
         MVC   SVCLPROF,CPROF                                                   
         MVC   SVCLEX,CEXTRA                                                    
         MVC   SVCLDLY,CDAILY                                                   
         MVC   SVCLTPW,CPWPCT                                                   
         MVC   SVCCOST2,CCOST2                                                  
         DROP  RE                                                               
*                                                                               
         MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTPRDKH           PRODUCT                                    
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
         CLC   SVCTAGY,=C'CK'        FOR AGENCY CK...                           
         BNE   VK10                  NO ADD OF PRODUCT TO CLIENTS               
         CLC   QCLT,=C'CC '          OTHER THAN CC                              
         BE    VK10                                                             
         MVC   ERRNUM,=AL2(VKERR4)                                              
         CLI   ACTEQU,ACTADD                                                    
         BE    SPERREX                                                          
*                                                                               
VK10     MVC   EKEYPRD,QPRD          COPY PRODUCT INTO KEY                      
         OC    EKEYPRD,SPACES                                                   
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESTESTKH           ESTIMATE                                   
*                                                                               
         CLI   ACTEQU,ACTDIS                                                    
         BNE   VK13                                                             
         CLI   ESTESTKH+5,0                                                     
         BE    ERRMIS                                                           
         BAS   RE,SPTOZER                                                       
*                                                                               
VK13     MVI   ESTESTKH+5,3          LENGTH                                     
         CLI   ESTESTK+2,C' '                                                   
         BH    VK15                                                             
         MVI   ESTESTKH+5,2                                                     
         CLI   ESTESTK+1,C' '                                                   
         BH    VK15                                                             
         MVI   ESTESTKH+5,1                                                     
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
VK20     CLI   5(R2),0               ACTION IS ADD SO... ESTIMATE               
         BE    ERRMIS                CODE IS REQUIRED                           
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR1)  ESTIMATE CODE MUST BE NUMERIC              
         TM    ESTESTKH+4,X'08'      AND HAVE A LENGTH <=3                      
         BZ    SPERREX                                                          
*                                                                               
         ZIC   RE,5(R2)              CONVERT ESTIMATE CODE TO BINARY            
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
VK25     MVC   EKEYEST,BEST          SAVE ESTIMATE CODE INTO KEY                
         MVC   ESTKEY,KEY            SAVE ESTIMATE RECORD KEY                   
*                                                                               
**********************************************************************          
*                                                                               
         XC    SVPOLPW,SVPOLPW                                                  
         MVC   KEY(13),ESTKEY         READ ESTIMATE RECORD FOR POL              
         MVC   KEY+4(3),=C'POL'       PRODUCT                                   
         GOTO1 HIGH                                                             
         CLI   ACTEQU,ACTADD                                                    
         BNE   VK60                                                             
*                                                                               
         CLI   SVCLTPOL,C'Y'          IF (CLIENT MUST ADD POL ESTIMATE          
         BE    VK30                   BEFORE ADDING BRAND ESTIMATES)            
         CLI   SVCLPROF+0,C'0'        ATTEMPTS TO ADD BRAND ESTIMATE            
         BE    VK40                   BEFORE POL ... ERROR                      
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
         CLI   ACTEQU,ACTADD                                                    
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
         TM    ECNTRL,X'04'          STATUS                                     
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
DR50     MVC   ESTBBAS,=CL5'CNET'    X'10' AND X'40' BIT ON ...                 
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
**********************************************************************          
*                                                                               
*                                    COMM.BASIS                                 
         MVC   ESTCBAS,=C'GROSS'     X'01' BIT ON ...                           
         TM    EBILLBAS,X'01'        SET COMM.BASIS FIELD TO 'GROSS'            
         BZ    *+10                  OTHERWISE ...                              
         MVC   ESTCBAS,=CL5'NET'     SET COMM.BASIS FIELD TO 'NET'              
*                                                                               
**********************************************************************          
*                                                                               
DR100    XC    ESTDEMS,ESTDEMS       DEMOS                                      
         XC    ESTDEM2,ESTDEM2                                                  
         OC    EDEMLST(3),EDEMLST                                               
         BZ    DR150                                                            
*                                                                               
         XC    BLOCK(255),BLOCK                                                 
         XC    BLOCK+255(224),BLOCK+255                                         
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
         BNE   DR110                 AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    DR110                                                            
         MVI   DBSELMED,C'C'                                                    
DR110    MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 CALLOV,DMCB           CALL DEMOCON                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(14,EDEMLST),(13,BLOCK),(C'S',ELEM),EUSRNMS            
         DROP  R4                                                               
*                                                                               
         LA    R5,ESTDEMS                                                       
         LA    R2,EDEMLST                                                       
         LA    R6,L'ESTDEMS(R5)                                                 
         LA    RF,BLOCK                                                         
*                                                                               
DR120    CLI   0(RF),C' '                                                       
         BNH   DR140                 IF NOT THE LAST DEMO ...                   
         BAS   RE,FMTDEMO            CALL FMTDEMO                               
*                                                                               
         ZIC   R1,WORK               LENGTH OF DEMO RETURNED IN WORK            
         LR    R0,R5                 SEE IF IT WILL FIT ON THIS LINE            
         AR    R0,R1                                                            
         CR    R0,R6                                                            
         BNH   DR130                                                            
*                                                                               
         BCTR  R5,0                  WILL NOT FIT ... PUT IT ON NEXT            
         CLI   0(R5),C','            LINE AND BLANK OUT LAST COMMA              
         BNE   *+8                                                              
         MVI   0(R5),C' '                                                       
         LA    R5,ESTDEM2                                                       
         LA    R6,L'ESTDEM2(R5)                                                 
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
         LA    RF,11(RF)             NEXT DEMON IN BLOCK AND RECORD             
         LA    R2,3(R2)              BUMP TO NEXT DEMO IN RECORD                
         B     DR120                                                            
*                                                                               
DR140    BCTR  R5,0                  LAST DEMO HAS BEEN OUTPUTTED ...           
         CLI   0(R5),C','            ELIMINATE LAST COMMA                       
         BNE   DR150                                                            
         MVI   0(R5),C' '                                                       
*                                                                               
DR150    OI    ESTDEMSH+6,X'80'      TRANSMIT DEMO LINES                        
         OI    ESTDEM2H+6,X'80'                                                 
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
         BAS   RE,FMTDEMO            BLOCK                                      
*                                                                               
         CLI   1(R2),X'21'           IF USER DEMO ... ONLY UN=NN WILL           
         BNE   DR170                 APPEAR ON WEIGHTS LINE                     
         MVI   WORK,2                                                           
*                                                                               
DR170    ZIC   R1,WORK               LENGTH OF DEMO RETURNED IN WORK            
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
         LA    R4,ONETWO             TARGET NUMBER LIST                         
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
DR220    BAS   RE,FMTDEMO                                                       
         CLI   1(R2),X'21'           IF USER DEMO ... ONLY UN=NN                
         BNE   *+8                   WILL APPEAR ON WEIGHTS LINE                
         MVI   WORK,2                                                           
*                                                                               
         ZIC   RF,WORK               LENGTH OF DEMO RETURNED IN WORK            
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
         MVC   HALF,EREP                                                        
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESTREP,DUB                                                       
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
DR300    ZIC   R4,EHUTADJ            OTHERWISE ... CALL DATCON                  
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
DR310    MVC   ESTMENU,EDAYMENU      DEPARTMENT MENU                            
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
         ZIC   R0,ECPPEST            CPP ESTIMATE EXISTS ... COPY               
         CVD   R0,DUB                IT TO SCREEN AND TRANSMIT                  
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R6),DUB                                                      
DR330    FOUT  ESTCPPEH                                                         
*R330    OI    ESTCPPEH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESTTYPE,ESTTYPE       TYPE                                       
         CLI   ETYPE,0                                                          
         BE    DR360                                                            
*                                                                               
         MVC   ESTTYPE(3),=C'CUT'    IF TYPE IS 'C' MOVE 'CUT' SCREEN           
         CLI   ETYPE,C'C'                                                       
         BE    DR360                                                            
*                                                                               
         XC    ESTTYPE,ESTTYPE                                                  
         LA    R4,TYPTAB             OTHERWISE ... IS TYPE IN TYPE              
DR340    CLC   ETYPE,2(R4)           TABLE?                                     
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
         MVC   ESTOWD(4),=C'NO  '    OUT OF WEEK ROTATOR                        
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
         CLI   SVCLDLY,C'Y'          DAILY OPTION SET?                          
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
*                                                                               
DR480    CLI   ECASHPRD,0            CASH PRODUCT OPTION SET?                   
         BE    DR510                                                            
         LA    R0,8                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
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
         LA    R1,3(R1)                                                         
         DROP  RF                                                               
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
*                                                                               
DR510    CLI   ETRDPRD,0             TRADE PRODUCT OPTION SET?                  
         BE    DR540                                                            
         LA    R0,7                  MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
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
         LA    R1,3(R1)                                                         
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
*                                                                               
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
         LA    R0,13                 MAX LENGTH OF OUTPUT DATA                  
         BAS   RE,CHECKOPT           WILL IT FIT IN FIELD?                      
         BNZ   DR589                                                            
         MVC   0(5,R1),=C'COS2='     YES ... COPY COST FACTOR TO                
         LA    R4,5(R1)              SCREEN                                     
         CLI   ECOST2,X'80'                                                     
         BNE   DR583                                                            
         MVC   0(3,R4),=C'0.0'                                                  
         LA    R0,3                                                             
         B     DR584                                                            
DR583    EDIT  ECOST2,(8,0(R4)),6,ALIGN=LEFT,FILL=0,DROP=5                      
DR584    AR    R4,R0                                                            
*                                                                               
*                                                                               
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
         MVI   ESTESTKH+5,3                                                     
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
         CLI   ACTEQU,ACTSEL                                                    
         BE    *+12                                                             
         CLI   ACTEQU,ACTCHA         IF CHANGING ESTIMATE, SAVE OLD             
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
         CLI   ACTEQU,ACTADD         ESTIMATE FOR A CLEINT WITH A POL           
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
         MVC   ELEN,=H'640'                                                     
*                                                                               
         CLI   ESTOPTH+5,0           IF NO OPTIONS INPUTTED AND CLIENT          
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
         MVC   ERRNUM,=AL2(TV1DEM)   AT LEAST 1 DEMO REQUIRED FOR MEDIA         
         CLI   5(R2),0               TV                                         
         BNE   VR500                                                            
         CLI   QMED,C'T'                                                        
         BE    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(NET1DEM)  AT LEAST 1 DEMO REQUIRED FOR               
         CLI   OVSYS,3               NETPAK                                     
         BNE   VR800                                                            
         B     SPERREX                                                          
*                                                                               
***********************************************************************         
*                                                                               
VR500    MVC   ERRNUM,=AL2(DEMERR)                                              
         CLC   8(5,R2),=C'MENU='                                                
         BNE   VR530                                                            
         LA    R2,ESTDEM2H           IF "MENU=" DEMO ...                        
         CLI   5(R2),0               2ND DEMO LINE MUST BE BLANK                
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(DEMERR3)                                             
         LA    R2,ESTDEMSH           MENU OPTION MUST BE 1-4 CHARACTERS         
         ZIC   R1,5(R2)              LONG                                       
         AHI   R1,-5                                                            
         BNP   SPERREX                                                          
         CHI   R1,4                                                             
         BH    SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(DEMERR4)                                             
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
         BNE   SPERREX               MENU OPTION INVALID                        
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
         L     R5,AIO2                                                          
         USING DBLOCK,R5             SET UP CALL TO DEMOVAL                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'TPT'        SET DBFILE = TPT                           
         MVI   DBSELMED,C'T'                                                    
         CLI   QMED,C'R'             SET DBSELMED = R FOR RADIO                 
         BNE   *+8                                                              
         MVI   DBSELMED,C'R'                                                    
         CLI   SVAPROF+7,C'C'        SET DBSELMED = C IF CANADIAN               
         BNE   VR540                 AGENCY USING US DEMOS                      
         CLI   SVCLEX,C'U'           SET DBSELMED = R OTHERWISE                 
         BE    VR540                                                            
         MVI   DBSELMED,C'C'                                                    
VR540    MVC   DMCB+4(4),=X'D9000AD9'                                           
         GOTO1 CALLOV,DMCB,0         CALL DEMOVAL                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),(2,ESTDEMSH),(14,BLOCK),(C'S',(R5)),EUSRNMS            
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
*                                                                               
         ZIC   R4,DMCB+4             NUMBER OF DEMOS                            
         MHI   R4,3                  EACH DEMO REQUIRES 3 BYTES                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                  STORE DEMOS INTO RECORD (EDEMLST)          
         MVC   EDEMLST(0),BLOCK                                                 
*                                                                               
***********************************************************************         
*                                                                               
VR550    LA    R4,DMAX-1             CHECK FOR DUPLICATE DEMOS                  
         LA    R5,EDEMLST            (R5 HOLDS STEADY AS R6 BUMPS               
VR560    LA    R6,3(R5)              THROUGH)                                   
         LA    R1,DMAX-1                                                        
*                                                                               
VR570    CLI   1(R6),0               END OF DEMOS?                              
         BE    VR580                                                            
         CLC   0(3,R5),0(R6)         DUPLICATE DEMO?                            
         BE    DUPPERR                                                          
         LA    R6,3(R6)              NOT A DUPLICATE, KEEP GOING                
         BCT   R1,VR570              TILL THE END OF DEMOLIST                   
*                                                                               
VR580    LA    R5,3(R5)              BUMP UP R5                                 
         CLI   1(R5),0               END OF DEMOS?                              
         BE    VR590                                                            
         BCT   R4,VR560              NO, KEEP CHECKING FOR DUPLICATES           
VR590    CLI   POLSW,0                                                          
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
         BE    NODEMDEL                                                         
         BCT   RF,VR660                                                         
         B     NODEMDEL                                                         
VR670    LA    R4,3(R4)                                                         
         CLC   0(3,R4),=3X'00'                                                  
         BE    VR760                                                            
         BCT   RE,VR650                                                         
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
*                                                                               
VR750    MVC   ERRNUM,=AL2(USWDMER)                                             
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
VR780    MVC   ERRNUM,=AL2(TOTHERR)                                             
         CLI   HMSW,1                                                           
         BNE   SPERREX                                                          
         MVC   ERRNUM,=AL2(DEMERR)                                              
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
         GOTO1 SCANNER,DMCB,(R2),(15,(R4))                                      
         CLI   DMCB+4,0                                                         
         BE    ERRINV                # OF DEMOS CANNOT EXCEED 14 OR BE          
         CLI   DMCB+4,DMAX           ZERO                                       
         BH    ERRINV                                                           
         LA    R2,ESTDEM2H           IF DEMOS EXIST ON 2ND DEMO LINE            
         CLI   5(R2),0               CONTINUE SCANNING                          
         BE    VR820                                                            
         ZIC   R0,DMCB+4                                                        
         MHI   R0,32                                                            
         L     R5,AIO3                                                          
         AHI   R5,1000                                                          
         AR    R5,R0                                                            
         ZIC   RE,DMCB+4                                                        
         LA    R0,DMAX+1                                                        
         SR    R0,RE                                                            
         GOTO1 SCANNER,DMCB,(R2),((R0),(R5))                                    
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
*                                                                               
VR820    LA    R2,ESTWTSH            PARSNIP BUILDS TABLE OF WEIGHTS            
         L     R4,AIO3               AT 500 BYTES BEYOND AIO3                   
         AHI   R4,500                                                           
         L     RF,ACOMFACS                                                      
         L     RF,CPARSNIP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(15,(R4)),0                                       
         MVC   ERRNUM,=AL2(WGHTERR)                                             
         CLI   DMCB+4,0              # OF WEIGHTS CANNOT EXCEED 28 OR           
         BE    SPERREX               BE ZERO                                    
         CLI   DMCB+4,DMAX*2                                                    
         BH    SPERREX               CHANGE PARSNIP BLOCK TO BE SCANNER         
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
VR950    CLI   EMSTRIND,0            MASTER AND SUB-ESTIMATES CANNOT            
         BE    VR970                 CHANGE THEIR DEMOS OR USER NAMES           
         CLI   ACTEQU,ACTADD                                                    
         BE    VR970                                                            
         CLC   SVDEMOS(60),EDEMLST                                              
         BNE   ERRNOCHG                                                         
         CLC   SVDEMOS+80(28),EUSRNMS                                           
         BNE   ERRNOCHG                                                         
*                                                                               
VR970    CLI   POLSW,2               IF (CHANGING POL ESTIMATE) ...             
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
         GOTO1 =A(VR1090),RR=RELO    DEPT MENUS AND FILTERS                     
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 =A(VR1210),RR=RELO    CONTROL AND RETAIL SCHEME                  
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
*        SETUP                                                        *         
***********************************************************************         
SETUP    NTR1                                                                   
         MVI   USEIO,C'N'                                                       
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    GENSTAT4,CONFDEL                                                 
         OI    CONSERVH+1,X'01'      MODIFY SERVICE REQUEST                     
         OI    CONSERVH+6,X'80'      TRANSMIT TO GET CONTROL                    
         MVI   IOOPT,C'Y'                                                       
*                                                                               
         OI    ESTREH+1,X'0C'        HIDE PF12=RETURN FIELD                     
         LR    RE,RA                                                            
         AH    RE,=Y(TWAENDLQ-2)                                                
         CLI   1(RE),0                                                          
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
SETUPX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
***********************************************************************         
*         FMTDEMO                                                     *         
***********************************************************************         
* ROUTINE TO FORMAT DEMOS ... RF POINTS TO 10 CHARACTER DESCRIPTION   *         
* ... R2 POINTS TO 3 BYTE DEMO ... WORK(1) RETURNS LENGTH ...         *         
* WORK+1 RETURNS DESCRIPTION                                          *         
***********************************************************************         
FMTDEMO  NTR1                                                                   
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
         ZIC   R0,2(R2)              USER NAME NUMBER                           
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
FMTDEMOX XIT1                                                                   
         DROP  R3                                                               
*                                                                               
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
ERRANGE  LA    R2,ESTRANH                                                       
         B     ERRINV                                                           
ERRINV2  LA    R2,ESTWTSH            RESET R2 FOR WEIGHTS ERRORS                
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
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
*                                                                               
DUPPERR1 LA    R2,ESTWTSH            RESET R2 FOR ERROR MESS                    
DUPPERR  MVC   ERRNUM,=AL2(DUPERR)                                              
         B     SPERREX                                                          
*                                                                               
WGHTSPR2 LA    R2,ESTWTSH            BRAND AND POL WEIGHTS MUST AGREE           
         MVC   ERRNUM,=AL2(BRPOLER9)                                            
         B     SPERREX                                                          
*                                                                               
WGHTSPER LA    R2,ESTWTSH                                                       
         MVC   ERRNUM,=AL2(WGHTERR3)                                            
         B     SPERREX              WEIGHT INVALID OR MISSING                   
*                                                                               
RATNOWT  LA    R2,ESTWTSH                                                       
         MVC   ERRNUM,=AL2(WGHTERR2)                                            
         B     SPERREX                                                          
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
NUMERR   EQU   3                   NUMERIC ONLY                                 
ALPHAERR EQU   4                   ALPHABETIC ONLY                              
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
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLES                                                                        
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
EOTBLQ   DC    C'A'                                                             
DMAX     EQU   14                                                               
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
***********************************************************************         
*        PFKEYS TABLES                                                *         
***********************************************************************         
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
         EJECT                                                                  
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
         MVC   ERRNUM,=AL2(BADDATE)  VALID D/M/Y (UK)                           
         GOTO1 DATVAL,DMCB,(0,ESTSTRD),SYR                                      
         OC    DMCB(4),DMCB          YY RETURNED IN SYR, MM RETURNED            
         BZ    SPERREX               IN SMN, DD RETURNED IN SDY                 
*                                                                               
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR5)                                             
         CLI   ACTEQU,ACTADD                                                    
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
         MVC   ERRNUM,=AL2(BADDATE)  VALID D/M/Y (UK)                           
         GOTO1 DATVAL,DMCB,(0,ESTENDD),EYR                                      
         OC    DMCB(4),DMCB          YY RETURNED IN EYR, MM RETURNED            
         BZ    SPERREX               IN EMN, DD RETURNED IN EDY                 
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR5)                                             
         CLI   ACTEQU,ACTADD         MASTER OR SUB-ESTIMATE'S END DATE          
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
***********************************************************************         
*                                                                               
         MVC   ERRNUM,=AL2(ESTERR8)  UNLESS START AND END DATE HAVE NOT         
         CLI   OVSYS,3               BEEN MODIFIED FOR A MASTER OR              
         BNE   VR140                 SUB-ESTIMATE ... NETPAK ESTIMATES          
         LA    R2,ESTMEDKH           MUST BE MEDIA N                            
         CLI   QMED,C'N'                                                        
         BNE   SPERREX                                                          
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
         LA    R2,ESTOWDH            OUT OF WEEK ROTATOR                        
         CLI   5(R2),0               (START DATE'S DAY OF THE WEEK)             
         BNE   VR230                                                            
*                                                                               
         MVI   DMCB,0                IF NOT INPUTTED, BUT CLIENT CON-           
         CLI   SVCLEX+10,C'Y'        TROLLED ... GO VALIDATE START DATE         
         BE    VR250                                                            
         B     VR260                                                            
*                                                                               
VR230    MVC   ERRNUM,=AL2(OWRERR1)                                             
         CLI   8(R2),C'N'            IF INPUTTED ...                            
         BE    VR260                 OUT OF WEEK ROTATOR MUST START             
*&&DO                                WITH AN 'N' OR 'Y' ... UNLESS IN           
         MVC   ERRNUM,=AL2(OWRERR2)  ???, WHERE ADDS TO MEDIA R REQ-            
         CLI   QMED,C'R'             UIRE OUT OF WEEK ROTATOR STARTING          
         BNE   VR240                 WITH 'N'                                   
         CLI   ACTEQU,ACTADD                                                    
         BE    SPERREX                                                          
*&&                                                                             
VR240    CLI   8(R2),C'Y'                                                       
         BNE   SPERREX                                                          
*                                                                               
*                                                                               
VR250    GOTO1 GETDAY,DMCB,(0,SYR),DUB                                          
         MVC   ERRNUM,=AL2(BADDATE)                                             
         CLC   DUB(3),SPACES                                                    
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(MONDERR)                                             
         CLI   0(R1),1               COPY START DATE'S DAY OF THE WEEK          
         BE    SPERREX               INTO OWSDAY                                
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
         MVC   ERRNUM,=AL2(BRPOLER2)                                            
         LA    R2,ESTOWDH                                                       
         CLC   POLOWDAY,OWSDAY                                                  
         BNE   SPERREX                                                          
         B     VR350                                                            
*                                                                               
***********************************************************************         
*                                                                               
VR290    CLI   POLSW,2               IF (CHANGING THE POL ESTIMATE) ...         
         BNE   VR310                 AND NO BRANDS EXIST, ALLOW POL             
         XC    KEY,KEY               DEMO DELETE                                
         MVC   KEY(4),ESTKEY                                                    
         GOTO1 =A(NEXTPRD),RR=RELO                                              
*                                                                               
***********************************************************************         
*                                                                               
VR300    CLC   SVSTRT,SYR            IF WHILE (CHANGING THE POL EST-            
         BNE   VR310                 IMATE) START DATE, END DATE AND            
         CLC   SVEND,EYR             OUT OF WEEK ROTATOR WERE NOT               
         BNE   VR310                 CHANGED ... DATES AND ROTATOR              
         CLC   SVOWS,OWSDAY          VALIDATION COMPLETE                        
         BE    VR350                                                            
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
         CLI   ACTEQU,ACTADD                                                    
         BNE   VR330                                                            
         LA    R2,ESTSTRDH                                                      
         CLC   ESTART,SYR                                                       
         BNE   ERRBRPOL                                                         
         LA    R2,ESTENDDH                                                      
         CLC   EEND,EYR                                                         
         BNE   ERRBRPOL                                                         
         MVC   ERRNUM,=AL2(BRPOLER2)                                            
         LA    R2,ESTOWDH                                                       
         CLC   EOWSDAY,OWSDAY                                                   
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(EDOLERR)                                             
         OC    EORDN(208),EORDN      IF ORDERED OR PAID $ ON ANY OF THE         
         BZ    VR320                 BRAND ESTIMATES ... CANNOT ADD POL         
         CLI   ESTDESC,C'@'          ESTIMATE (UNLESS @ IN DESCRIPTION          
         BE    VR320                 FIELD)                                     
         B     SPERREX                                                          
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
         GOTO1 =A(CANTV),RR=RELO                                                
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
         BNE   ERRINV                                                           
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
VR1000   MVC   ERRNUM,=AL2(BADDATE)           VALIDATE RATING BOOK FOR          
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK     M/Y AND SAVE INTO RECORD          
         OC    DMCB(4),DMCB                                                     
         BZ    SPERREX                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+10)                                 
         MVC   EBOOK,WORK+10                                                    
*                                                                               
VR1010   CLI   EMSTRIND,0            RATING BOOK CANNOT BE CHANGED ON           
         BE    VR1020                MASTER OR SUB-ESTIMATE, NON-POL            
         CLI   ACTEQU,ACTADD         ESTIMATES                                  
         BE    VR1020                                                           
         CLC   SVBOOK,EBOOK                                                     
         BE    VR1020                                                           
         CLC   QPRD,=C'POL'                                                     
         BNE   ERRNOCHG                                                         
VR1020   CLI   POLSW,0                                                          
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
         MVC   ERRNUM,=AL2(HUTERR1)                                             
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         MVI   EHUTADJ,0             IF HUT ADJUSTMENT NOT 'AUTO' ...           
         CLC   8(4,R2),=C'AUTO'      MUST BE 3 CHARACTERS LONG AND A            
         BE    VR1060                VALID MONTH                                
         CLI   5(R2),3                                                          
         BNE   ERRINV                                                           
*                                                                               
         MVC   ERRNUM,=AL2(BADDATE)                                             
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
VR1060   CLI   EMSTRIND,0            HUT ADJUSTMENT CANNOT BE CHANGED           
         BE    VR1070                ON MASTER OR SUB-ESTIMATE, NON-            
         CLI   ACTEQU,ACTADD         POL ESTIMATES                              
         BE    VR1070                                                           
         CLC   SVHUT,EHUTADJ                                                    
         BE    VR1070                                                           
         CLC   QPRD,=C'POL'                                                     
         BNE   ERRNOCHG                                                         
VR1070   CLI   POLSW,0                                                          
         BE    VR1081                                                           
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1080                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER5) ESTIMATE) ... HUT ADJUSTMENT FOR           
         CLC   POLHUT,EHUTADJ        BRAND ESTIMATE MUST MATCH POL'S            
         BNE   SPERREX               HUT ADJUST.                                
         B     VR1081                                                           
*                                                                               
*                                    IF (ADDING A POL ESTIMATE) OR              
VR1080   MVC   SVHUT,EHUTADJ         (CHANGING A POL ESTIMATE) ...              
VR1081   XIT1                        SAVE NEW HUT ADJUSTMENT INTO SVHUT         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        VR1090 - VALIDATE DEPARTMENT MENU AND FILTERS                *         
***********************************************************************         
VR1090   NTR1  BASE=*,LABEL=*                                                   
         MVC   ERRNUM,=AL2(MENERR1)                                             
         LA    R2,ESTMENUH           DEPARTMENT MENU                            
         CLI   5(R2),0               REQUIRED                                   
         BE    SPERREX                                                          
         MVC   ERRNUM,=AL2(MENERR2)  MUST BE ONE CHARACTER LONG                 
         CLI   5(R2),1                                                          
         BNE   SPERREX                                                          
*                                                                               
         MVC   EDAYMENU,8(R2)        IS DEPARTMENT MENU ON FILE?                
         MVC   DMCB(2),AGENCY        AGENCY                                     
         MVC   DMCB+2(1),QMED        MEDIA                                      
         MVC   DMCB+3(1),EDAYMENU    DEPARTMENT MENU                            
         GOTO1 DPTRD,DMCB,,BLOCK,DATAMGR                                        
         MVC   ERRNUM,=AL2(FNDERR)                                              
         CLI   DMCB+8,X'FF'          IF MENU NOT ON FILE ... ERROR              
         BE    SPERREX                                                          
*                                                                               
         CLI   EMSTRIND,0            DEPARTMENT MENU CANNOT BE CHANGED          
         BE    VR1100                ON MASTER OR SUB-ESTIMATE                  
         CLI   ACTEQU,ACTADD                                                    
         BE    VR1100                                                           
         CLC   SVDPT,EDAYMENU                                                   
         BNE   ERRNOCHG                                                         
VR1100   CLI   POLSW,0                                                          
         BE    VR1120                                                           
*                                                                               
         CLI   POLSW,1               IF (CHANGING OR ADDING A BRAND             
         BNE   VR1110                ESTIMATE FOR A CLIENT WITH A POL           
         MVC   ERRNUM,=AL2(BRPOLER4) ESTIMATE) ... DEPARTMENT MENU FOR          
         CLC   POLDPT,EDAYMENU       BRAND ESTIMATE MUST MATCH THE              
         BNE   SPERREX               POL'S DEPARTMENT MENU                      
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
         MVC   EPROF(3),ESTFLTR                                                 
         LA    R4,EPROF                                                         
         LA    R5,3                  IF INPUTTED ...                            
VR1130   CLI   0(R4),C' '            FILTER MUST BE A COMBINATION OF 3          
         BE    VR1140                OR LESS ALPHANUMERICS                      
         CLI   0(R4),C'A'                                                       
         BL    ERRINV                                                           
         CLI   0(R4),C'9'                                                       
         BH    ERRINV                                                           
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
         CLC   POLFLTRS,EPROF        SVF0PROF IS N) OR (MEDIA IS                
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
         BE    ERRINV                                                           
         CLC   0(2,R1),AGENCY                                                   
         BE    VR1240                                                           
         LA    R1,2(R1)                                                         
         B     VR1220                                                           
VR1230   CLC   ESTECON(3),=C'NSC'    ONLY OTHER VALID CONTROL IS 'NSC'          
         BNE   ERRINV                                                           
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
         MVC   ERRNUM,=AL2(BRPOLER3) IF (ADDING A POL ESTIMATE) ...             
         LA    R2,ESTRBKH            POL MUST AGREE WITH ALL BRAND'S            
         CLC   EBOOK,SVBOOK          RATING BOOKS, HUT ADJUSTMENTS,             
         BNE   SPERREX               DEPARTMENT MENUS AND CONTROLS              
         MVC   ERRNUM,=AL2(BRPOLER5)                                            
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
         MVC   EDAYMENU,SVDPT        DEPARTMENT MENU, CONTROLS (AND             
         CLI   SVF0PROF,C'N'         FILTERS AND RETAIL SCHEME) INTO            
         BE    *+10                  BRAND ESTIMATE ... THEN MAKE ALL           
         MVC   EPROF(3),SVFLTRS      USER NAMES MATCH AND PUT BRAND             
         MVC   ECONTROL,SVECON       ESTIMATE                                   
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
         GOTO1 =A(CANTV),RR=RELO                                                
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
         LA    R2,ESTCOPYH           COPY CODE NOT REQUIRED                     
         MVI   ECOPY,0                                                          
         CLI   5(R2),0                                                          
         BE    VR1540                                                           
*                                                                               
         MVC   ERRNUM,=AL2(ALPHAERR)  MUST BE ALPHABETIC                        
         TM    4(R2),X'04'                                                      
         BZ    SPERREX                                                          
         MVC   ECOPY,ESTCOPY                                                    
*                                                                               
**********************************************************************          
*                                                                               
VR1540   LA    R2,ESTREPH            SPECIAL REP NOT REQUIRED                   
         XC    EREP,EREP                                                        
         CLI   5(R2),0                                                          
         BE    VR1550                                                           
*                                                                               
         MVC   ERRNUM,=AL2(NUMERR)   MUST BE NUMERIC                            
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
         BNZ   ERRINV                                                           
*                                                                               
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
         ZIC   R6,5(R2)              MUST BE <= 3 CHARACTERS IN LENGTH          
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
*                                                                               
         MVC   KEY+4(3),=C'POL'      READ IN ESTIMATE RECORD FOR POL            
         MVC   KEY+7(1),HALF+1       PRODUCT ... SAVE DEMOS, START              
         GOTO1 READ                  DATE AND RATE TYPE                         
         MVC   AIO,AIO2                                                         
         L     R3,AIO2                                                          
         GOTO1 GETREC                                                           
         MVC   POLDEMOS,EDEMOS                                                  
         MVC   POLDATE,ESTART                                                   
         MVC   POLTYPE,ETYPE                                                    
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
         CLI   ETYPE,0               CANNOT BE BLANK                            
         BE    VR1640                                                           
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
         B     VR1650                                                           
*                                                                               
VR1640   MVI   ETYPE,0                                                          
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
         MVI   ETYPE,C'C'                                                       
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
VR1680   MVC   ETYPE,2(R5)           STORE 04, IF Q$ STORE 05                   
*                                                                               
         MVC   ERRNUM,=AL2(TYPERR4)  POL RECORD TYPE MUST MATCH                 
         CLI   POLTYPE,0             CPP EST TYPE                               
         BE    VR1690                                                           
         CLC   ETYPE,POLTYPE                                                    
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
         BNE   ERRINV                                                           
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
         BNE   ERRINV                                                           
VR1770   MVC   ERATECST,ESTRTYP+1                                               
         B     *+12                                                             
*                                                                               
VR1780   CLI   5(R2),1                IF NOT NETPAK, RATE TYPE INPUT            
         BNE   ERRINV                 MUST BE 1 CHAR LONG                       
*                                                                               
         CLI   ESTRTYP,C'*'           FIRST CHAR OF RATE TYPE MUST BE           
         BE    VR1790                 * OR 0-9                                  
         CLI   ESTRTYP,C'0'                                                     
         BL    ERRINV                                                           
         CLI   ESTRTYP,C'9'                                                     
         BH    ERRINV                                                           
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
         CLI   ACTEQU,ACTADD                                                    
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
         MVC   ELEN,=H'640'          ESTIMATE RECORD WITH TRADE                 
         OI    ECNTRL,X'01'          PRODUCT CODE                               
         MVC   EPRDCD+1(1),BPRD                                                 
         MVC   AIO,AIO1                                                         
         GOTO1 =A(ADREC),RR=RELO                                                
         MVC   ESTKEY,KEY                                                       
         GOTO1 =A(CANTV),RR=RELO                                                
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
VR1830   GOTO1 =A(PTREC),RR=RELO         PUT THE ESTIMATE                       
         GOTO1 =A(CANTV),RR=RELO                                                
*                                                                               
********************************************************************            
*                                                                               
         CLI   WORK,C'S'             IF STATUS WAS CHANGED...                   
         BNE   VR1840                READ EST REC...WRITE CNTRL                 
         GOTO1 READ                  BYTE TO KEY                                
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         CLI   SVAPROF+7,C'C'                                                   
         BNE   VR1865                                                           
         CLI   QMED,C'T'                                                        
         BNE   VR1865                                                           
*                                                                               
         MVC   KEY,ESTKEY            IF CANADIAN TV WRITE CNTRL BYTE            
         NI    KEY+1,X'F0'           TO KEY IN X'03' AND X'08' RECS             
         OI    KEY+1,X'03'                                                      
         GOTO1 READ                                                             
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         MVC   KEY,ESTKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 READ                                                             
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         B     VR1865                                                           
*                                                                               
********************************************************************            
*                                                                               
VR1840   OC    CASHPRD,CASHPRD                                                  
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
         ZIC   R0,BEST                                                          
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
         BE    SPERREX               ADDS                                       
         B     CHKXIT                                                           
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
         CLI   ACTEQU,ACTADD                                                    
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
         CLI   ACTEQU,ACTADD                                                    
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
         CLI   ACTEQU,ACTADD         HELD ESTIMATES AND ESTIMATES WHICH         
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
CHKS50   CLI   ACTEQU,ACTADD                                                    
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
         CLC   =C'SJ',AGENCY         SJ HAS NO TALENT RECORDS                   
         BE    SPERREX                                                          
         GOTO1 =A(CHKTAL),RR=RELO                                               
         BNE   SPERREX                                                          
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
CKPOLX   XIT1                                                                   
         LTORG                                                                  
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
VPRF00   XC    ECOST2,ECOST2       INITIALIZE COST FACTOR AND PROFIT            
         XC    EPWPCT,EPWPCT       WITHIN PERCENTAGE TO ZERO                    
*                                                                               
         MVC   EDAILY,SVCLDLY      INITIALIZE DAILY ESTIMATE INDICATOR          
*                                  TO CLIENT DEFAULT                            
*                                                                               
         TM    SVCLOP1,COP1NMG     IF CLIENT USES NEW MAKEGOOD DATES            
         BNO   *+8                 TURN ON BIT IN ESTIMATE RECORD               
         OI    EFLAG1,EF1NMG                                                    
*                                                                               
         CLI   POLSW,0                                                          
         BE    VPRF05              IF (ADDING POL ESTIMATE) ... DE-             
         CLI   POLSW,3             FAULT PROFIT WITHIN PERCENTAGE TO            
         BNE   VPRF04              CLIENT'S PROFIT WITHIN PERCENTAGE            
         MVC   EPWPCT,SVCLTPW                                                   
         B     VPRF05              IF (ADDING BRAND ESTIMATE WITH EX-           
VPRF04   CLI   ACTEQU,ACTADD       ISTING POL ESTIMATE) ... DEFAULT             
         BNE   VPRF05              BRAND PROFIT WITHIN PERCENTAGE TO            
         MVC   EPWPCT,SVPOLPW      POL'S PROFIT WITHIN PERCENTAGE               
*                                                                               
***********************************************************************         
*                                                                               
VPRF05   LA    R2,ESTOPTH          USER DECLARED OPTIONS                        
         CLI   5(R2),0                                                          
         BE    VPRFX                                                            
         OC    ECPPEST,ECPPEST     CANNOT HAVE USER DECLARED OPTIONS IF         
         BNZ   ERRINV              CPP ESTIMATE PRESENT                         
*                                                                               
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(10,BLOCK)                                     
         CLI   DMCB+4,0                                                         
         BE    ERRINV                                                           
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
         BNE   ERRINV                                                           
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
         BZ    ERRINV                                                           
         ZIC   R4,FLD2LEN                                                       
         GOTO1 CASHVAL,DMCB,(2,FLD2),(R4)                                       
         CLI   DMCB,X'00'                                                       
         BNE   ERRINV              MUST BE VALID MONETARY                       
         MVC   EPWPCT,DMCB+5                                                    
         OC    EPWPCT,EPWPCT                                                    
         BNZ   VPRF100                                                          
         OI    EPWPCT,X'80'        SET PROFIT WITHIN PERCENTAGE FLAG            
         B     VPRF100                                                          
*                                                                               
***********************************************************************         
*                                                                               
VPRF30   CLC   FLD1(3),=C'REQ'       IF REQ OPTION ...                          
         BNE   VPRF35                                                           
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
VPRF35   CLC   FLD1(3),=C'NMG'       IF NMG OPTION ...                          
         BNE   VPRF36                                                           
         OI    EFLAG1,EF1NMG         IF NOT A DDS TERMINAL, MUST BE Y           
         CLI   FLD2,C'Y'             IF DDS TERMINAL CAN BE N OR Y              
         BE    VPRF100                                                          
         CLI   TWAOFFC,C'*'                                                     
         BNE   ERRINV                                                           
         CLI   FLD2,C'N'                                                        
         BNE   ERRINV                                                           
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
         BNE   VPRF40                                                           
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
         BNE   VPRF45                                                           
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
         BNE   ERRINV                LONG                                       
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
         L     R6,AIO3               SAVE CASH ESTIMATE'S TRADE PRODUCT         
         MVC   AIO,AIO3              INTO SVTRDPRD                              
         GOTO1 GETREC                                                           
         MVC   SVTRDPRD,ETRDPRD-ESTHDR(R6)                                      
*                                                                               
         MVC   ERRNUM,=AL2(CASH2)    CASH PRODUCT MUST BE IN CLIENT'S           
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
         BE    SPERREX                                                          
         CLC   FLD2(3),0(R6)                                                    
         BE    VPRF47                                                           
         LA    R6,4(R6)                                                         
         B     VPRF46                                                           
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
VPRF50   CLC   FLD1(3),=C'TRD'       TRD IS ONLY OTHER VALID OPTION ...         
         BNE   ERRINV                NO VALIDATION                              
*                                                                               
***********************************************************************         
*                                                                               
VPRF100  LA    R5,32(R5)             BUMP TO NEXT OPTION                        
         BCT   R0,VPRF10                                                        
         DROP  R5                                                               
*                                                                               
***********************************************************************         
*                                                                               
VPRFX    OC    SVCLTPW,SVCLTPW       IF CLIENT SET FOR PROFIT WITHIN            
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
         MVC   ECOST2,SVCCOST2                                                  
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
         MVI   USEIO,C'Y'                                                       
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTFIL',KEY+14,AIO                     
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   USEIO,C'N'                                                       
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        ADD OR CHANGE CANADIAN ESTIMATE               *                        
***********************************************************************         
*        IF CANADIAN AGENCY ADDS OR MODIFIES AN ESTINMATE FOR TV -    *         
*        ADD OR MODIFY PRODUCT RECORD FOR MEDIA N(03) & MEDIA C(08)   *         
***********************************************************************         
CANTV    NTR1  BASE=*,LABEL=*                                                   
         L     R4,AIO1                                                          
         CLI   SVAPROF+7,C'C'        CANADIAN?                                  
         BNE   CTX                                                              
         CLI   QMED,C'T'             TV?                                        
         BNE   CTX                                                              
*                                                                               
         XC    KEY,KEY               MEDIA N(03)                                
         MVC   KEY,ESTKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'03'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT10                                                             
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEYSAVE      RECORD DOESN'T EXIST                       
         MVC   KEY(13),KEYSAVE       MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         GOTO1 =A(ADREC),RR=RELO                                                
         B     CT20                                                             
*                                                                               
CT10     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         GOTO1 =A(PTREC),RR=RELO     PUT (03) RECORD                            
*                                                                               
*                                                                               
*                                                                               
CT20     XC    KEY,KEY               MEDIA C(08)                                
         MVC   KEY,ESTKEY                                                       
         NI    KEY+1,X'F0'                                                      
         OI    KEY+1,X'08'                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    CT30                                                             
         L     R5,AIO1               RECORD DOESN'T EXIST                       
         MVC   0(13,R5),KEYSAVE      MUST ADD IT                                
         MVC   AIO,AIO1                                                         
         MVC   KEY(13),KEYSAVE                                                  
         GOTO1 =A(ADREC),RR=RELO                                                
         B     CT40                                                             
*                                                                               
CT30     MVC   AIO,AIO3              RECORD EXISTS, GET IT                      
         GOTO1 GETREC                INTO AIO3                                  
         L     R5,AIO1                                                          
         MVC   0(13,R5),KEY          COPY KEY                                   
         MVC   AIO,AIO1                                                         
         GOTO1 =A(PTREC),RR=RELO     PUT C(08) RECORD                           
CT40     MVC   KEY,ESTKEY            RESTORE KEY                                
         NI    1(R5),X'F0'                                                      
         OI    1(R5),X'01'                                                      
CTX      XIT1                                                                   
         DROP  R3                                                               
***********************************************************************         
*        LTORG                                                        *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
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
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFM72D          EST MAINTENACE SCREEN                        
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
SVF0PROF DS    CL16                                                             
*                                                                               
SVPPST   DS    CL10                                                             
*                                                                               
SCRNFLAG DS    X                                                                
AUSR     DS    A                                                                
PSTOUT   DS    CL64                                                             
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
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
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001NESFM38   08/09/99'                                      
         END                                                                    
