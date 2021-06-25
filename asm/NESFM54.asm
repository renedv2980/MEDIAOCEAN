*          DATA SET NESFM54    AT LEVEL 118 AS OF 11/09/18                      
*PHASE T31C54B                                                                  
*                                                                               
***********************************************************************         
*                                                                     *         
*  TITLE:        T31C54  -- ESTIMATE DEMO MAINTENANCE                 *         
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
         TITLE 'T31C54 - ESTIMATE DEMO MAINTENANCE'                             
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 117 16APR18 <SPEC-22503> CREATE CLEAR ERROR MESSAGES FOR API   *         
***********************************************************************         
T31C54   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1C54**,R7,RR=R3                                              
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
         CLC   =C'DIS',CONACT                                                   
         BE    MAIN10                                                           
         CLC   =C'SELE',CONACT                                                  
         BE    MAIN10                                                           
         CLC   =C'CHA',CONACT                                                   
         BNE   ERRIACT                                                          
*                                                                               
MAIN10   GOTO1 VTERMACC            CHECK FOR DISP/LIST ONLY TERMINALS           
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
         MVC   ESDMEDN,SPACES        CLEAR MEDIA NAME AND CLIENT NAME           
         OI    ESDMEDNH+6,X'80'      AND PRODUCT NAME                           
         MVC   ESDCLIN,SPACES                                                   
         OI    ESDCLINH+6,X'80'                                                 
         MVC   ESDPRDN,SPACES                                                   
         OI    ESDPRDNH+6,X'80'                                                 
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESDMEDKH         MEDIA                                        
         GOTO1 VALIMED             VALIDATE MEDIA CODE AND TRANSMIT             
         MVC   ESDMEDN,MEDNM       MEDIA NAME                                   
         OI    ESDMEDNH+6,X'80'                                                 
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
         LA    R2,ESDCLIKH           CLIENT                                     
*                                                                               
         MVC   AIO,AIO2                                                         
         MVI   BYTE,C'A'                                                        
         GOTO1 VALICLT               VALIDATE CLIENT CODE AND TRANSMIT          
*                                                                               
         MVC   ESDCLIN,CLTNM         CLIENT NAME                                
         OI    ESDCLINH+6,X'80'                                                 
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
         MVC   EKEYCLT,BCLT          COPY CLIENT INTO KEY                       
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESDPRDKH           PRODUCT                                    
         GOTO1 VALIPRD                                                          
         MVC   ESDPRDN,PRDNM         VALIDATE PRODUCT CODE AND TRANSMIT         
         OI    ESDPRDNH+6,X'80'      PRODUCT NAME                               
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
         LA    R2,ESDESTKH           ESTIMATE                                   
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
*                                                                               
         L     R3,AIO                                                           
         MVC   SVECOST2,ECOST2       SAVE CLIENT RECORD DATA                    
         LA    R3,KEY                                                           
*                                                                               
         B     VK25                                                             
*                                                                               
VK20     MVC   ERRNUM,=AL2(ESTERR1)  ESTIMATE CODE MUST BE NUMERIC              
         TM    ESDESTKH+4,X'08'      AND HAVE A LENGTH <=3                      
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
DR       DS    0H                                                               
         USING ESTHDR,R3                                                        
         L     R3,AIO                                                           
*                                                                               
         LA    R2,ESDDEMSH                                                      
DR10     LA    RF,ESDDEMLH                                                      
         CR    R2,RF                                                            
         BH    DR20                                                             
         XC    8(L'ESDDEMS,R2),8(R2)                                            
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     DR10                                                             
*                                                                               
DR20     LA    R2,ESDWTSH                                                       
DR25     LA    RF,ESDWTLH                                                       
         CR    R2,RF                                                            
         BH    DR50                                                             
         XC    8(L'ESDWTS,R2),8(R2)                                             
         OI    6(R2),X'80'                                                      
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         ZIC   RF,0(R2)                                                         
         AR    R2,RF                                                            
         B     DR25                                                             
*                                                                               
DR50     DS    0H                                                               
         MVC   SVDEMLST(60),EDEMLST           NET NOW HAS 50 DEMOS              
*                                                                               
         OC    EDEM21,EDEM21                IF THERE'S A 21ST DEMO,             
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EDEMLST1(3),EDEM21           LIST AND CLEAR THIS OUT             
         XC    EDEM21,EDEM21                                                    
*                                                                               
         MVC   SVDEMLST+60(60),EDEMLST1                                         
         MVC   SVDEMLST+120(30),EDEMLST2                                        
*                                                                               
         MVC   SVWGTLST(20),EWGTLST                                             
*                                                                               
         MVC   TEMPDEM1,SVDEMLST STORE ALL DEMOS INTO ONE BLOCK                 
*                                                                               
         MVC   TEMPWGT1(20),EWGTLST  AND ALL THE WEIGHT LISTS.                  
*                                                                               
         OC    EDEM21WT,EDEM21WT            IF THERE'S A 21ST WGT,              
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EWGTLST2(1),EDEM21WT         LIST AND CLEAR THIS OUT             
         XC    EDEM21WT,EDEM21WT                                                
*                                                                               
         MVC   TEMPWGT1+20(30),EWGTLST2                                         
*                                                                               
         OC    SVDEMLST(3),SVDEMLST                                             
         BZ    DR150                                                            
*                                                                               
         LA    R4,MYBLOCK                                                       
         XCEF  (R4),560                                                         
*                                                                               
         XC    ELEM,ELEM             DEMOS EXIST ...                            
         LA    R4,ELEM                                                          
         USING DBLOCK,R4             SET UP CALL TO DEMOCON                     
         MVC   DBCOMFCS,ACOMFACS                                                
         MVC   DBFILE,=C'NAD'                                                   
         MVI   DBSELMED,C'N'                                                    
DR110    MVC   DMCB+4(4),=X'D9000AE0'                                           
         GOTO1 CALLOV,DMCB           CALL DEMOCON                               
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         LAY   RE,DEMOCON                                                       
         MVC   0(L'DMCB,RE),DMCB                                                
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JH    DR112                                                            
         GOTO1 (RF),(R1),(50,TEMPDEM1),(13,MYBLOCK),(C'S',ELEM),EUSRNMS         
         J     DR115                                                            
DR112    GOTO1 (RF),(R1),(50,TEMPDEM1),(13,MYBLOCK),(C'S',ELEM),       *        
               EUSRNMS,ENONTDMS                                                 
         DROP  R4                                                               
*                                                                               
DR115    LA    R2,TEMPDEM1                                                      
         LA    RF,MYBLOCK                                                       
*                                                                               
         LA    R5,ESDDEMSH         FIRST DEMO FLDHDR                            
         ST    R5,SVDEMADR                                                      
         LA    R5,8(R5)            FIRST OUTPUT POSITION                        
         LA    R6,L'ESDDEMS(R5)                                                 
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
*                                                                               
         L     R5,SVDEMADR         GET FLDHDR ADDRESS                           
         SR    R0,R0                                                            
         IC    R0,0(R5)            POINT TO 'DEMOS' TITLE                       
         AR    R5,R0                                                            
         IC    R0,0(R5)                                                         
         AR    R5,R0               POINT TO NEXT DEMO FIELD                     
         ST    R5,SVDEMADR                                                      
         LA    R5,8(R5)            FIRST OUTPUT POSITION                        
         LA    R6,L'ESDDEMS(R5)    R6 HAS END ADDRESS                           
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
         LA    R0,TEMPDEM1+L'TEMPDEM1-1                                         
         CR    R2,R0                                                            
         BNH   DR120                                                            
*                                                                               
DR140    BCTR  R5,0                  LAST DEMO HAS BEEN OUTPUTTED ...           
         CLI   0(R5),C','            ELIMINATE LAST COMMA                       
         BNE   DR150                                                            
         MVI   0(R5),C' '                                                       
*                                                                               
DR150    OI    ESDDEMSH+6,X'80'      TRANSMIT DEMO LINES                        
         OI    ESDDEM2H+6,X'80'                                                 
         OI    ESDDEM3H+6,X'80'                                                 
         OI    ESDDEM4H+6,X'80'                                                 
         OI    ESDDEM5H+6,X'80'                                                 
         OI    ESDDEM6H+6,X'80'                                                 
         OI    ESDDEM7H+6,X'80'                                                 
         OI    ESDDEM8H+6,X'80'                                                 
         OI    ESDDEMLH+6,X'80'                                                 
*                                                                               
**********************************************************************          
*                                                                               
         XC    ESDWTS,ESDWTS         WEIGHTS                                    
         LA    R5,ESDWTS                                                        
         LA    R0,L'ESDWTS(R5)                                                  
         ST    R0,WTEND                                                         
*                                                                               
         CLI   EWGTNM,C' '           IF NO WEIGHTED DEMO NAME, TEST             
         BNH   DR190                 FOR TARGETS                                
*                                                                               
         LA    R6,DMAX               WEIGHTED DEMO NAME EXISTS ...              
         CLI   OVSYS,3               NET HAS 21 DEMOS                           
         BNE   *+8                                                              
         AHI   R6,1                                                             
         LA    R2,TEMPDEM1                                                      
         LA    R4,TEMPWGT1                                                      
         LA    RF,MYBLOCK                                                       
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
DR190    OC    ETRGLST,ETRGLST       ANY TARGETS?                               
         BZ    DR240                                                            
         LA    R1,ETRGLST            YES ... TARGET LIST                        
         LA    R4,ONETWO             TARGET NUMBER LIST                         
*                                                                               
DR200    OC    0(3,R1),0(R1)                                                    
         BZ    DR230                                                            
         LA    RF,MYBLOCK                                                       
         LA    R2,TEMPDEM1                                                      
         LA    R6,DMAX                                                          
         CLI   OVSYS,3              20+1 DEMOS FOR NET                          
         BNE   *+8                                                              
         AHI   R6,1                                                             
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
         OI    ESDWTSH+6,X'80'                                                  
*                                                                               
**********************************************************************          
*&&DO                                                                           
*                                                                               
         OC    EBOOK,EBOOK           IF EBOOK IS EMPTY, DEFAULT TO              
         BZ    DR290                 'LATEST'                                   
*                                                                               
DR260    CLI   QMED,C'N'                                                        
         BNE   DR270                                                            
         XC    ESDRBK,ESDRBK         IF NETWORK MEDIA ...                       
         ZIC   R0,EBOOK+1            RATING BOOK WILL BE IN BINARY              
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESDRBK(2),DUB                                                    
         MVI   ESDRBK+2,C'/'                                                    
         ZIC   R0,EBOOK                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  ESDRBK+3(2),DUB                                                  
         B     DR290                                                            
*                                    IF NOT NETWORK ... CALL DATCON             
DR270    GOTO1 DATCON,DMCB,(3,EBOOK),(6,ESDRBK)                                 
*                                                                               
**********************************************************************          
DR290    MVC   ESDHUT,=CL7'AUTO'     HUTADJ                                     
         OI    ESDHUTH+6,X'80'                                                  
         CLI   EHUTADJ,0             IF ESTHUT IS EMPTY,DEFAULT TO AUTO         
         BE    DR310                                                            
*                                                                               
DR300    ZIC   R4,EHUTADJ            OTHERWISE ... CALL DATCON                  
         XC    ESDHUT,ESDHUT                                                    
         SRL   R4,4                                                             
         STC   R4,WORK+1                                                        
         MVI   WORK,77                                                          
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(3,WORK),(6,WORK+10)                                 
         MVC   ESDHUT(3),WORK+10                                                
*&&                                                                             
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
DK20     MVC   ESDMEDK,0(R5)                                                    
         OI    ESDMEDKH+6,X'80'                                                 
         MVI   ESDMEDKH+5,1          TRANSMIT MEDIA CODE TO SCREEN              
*                                                                               
***********************************************************************         
*                                                                               
         GOTO1 CLUNPK,DMCB,EKEYCLT,ESDCLIK                                      
         OI    ESDCLIKH+6,X'80'                                                 
         MVI   ESDCLIKH+5,3          TRANSMIT CLIENT CODE TO SCREEN             
*                                                                               
***********************************************************************         
*                                                                               
         MVC   ESDPRDK,EKEYPRD                                                  
         MVI   ESDPRDKH+5,3                                                     
         OI    ESDPRDKH+6,X'80'      TRANSMIT PRODUCT CODE TO SCREEN            
*                                                                               
**********************************************************************          
*                                                                               
         EDIT  EKEYEST,ESDESTK,ALIGN=LEFT                                       
         OI    ESDESTKH+6,X'80'      TRANSMIT ESTIMATE CODE TO SCREEN           
         OI    ESDESTKH+4,X'08'      NUMERIC CODE                               
         MVI   ESDESTKH+5,3          LENGTH                                     
         CLI   ESDESTK+2,C' '                                                   
         BH    DK30                                                             
         MVI   ESDESTKH+5,2                                                     
         CLI   ESDESTK+1,C' '                                                   
         BH    DK30                                                             
         MVI   ESDESTKH+5,1                                                     
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
         XC    SVDEMOS,SVDEMOS                                                  
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
*                                                                               
         MVC   SVDEMLST(60),EDEMLST         NET NOW HAS 50 DEMOS                
*                                                                               
         OC    EDEM21,EDEM21                IF THERE'S A 21ST DEMO,             
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EDEMLST1(3),EDEM21           LIST AND CLEAR THIS OUT             
         XC    EDEM21,EDEM21                                                    
*                                                                               
         MVC   SVDEMLST+60(60),EDEMLST1                                         
         MVC   SVDEMLST+120(30),EDEMLST2                                        
*                                                                               
         MVC   SVWGTLST(20),EWGTLST                                             
         MVC   TEMPDEM1,SVDEMLST     STORE THEM IN BLOCK                        
         MVC   TEMPWGT1(20),EWGTLST                                             
*                                                                               
         OC    EDEM21WT,EDEM21WT            IF THERE'S A 21ST WGT,              
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EWGTLST2(1),EDEM21WT         LIST AND CLEAR THIS OUT             
         XC    EDEM21WT,EDEM21WT                                                
*                                                                               
         MVC   TEMPWGT1+20(30),EWGTLST2                                         
*                                                                               
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
VR03     DS    0H                                                               
         LA    RE,POLDATA                                                       
         LA    RF,(POLDATAX-POLDATA)                                            
         XCEF                                                                   
         LA    RE,SVCLIST1         USE SVCLIST1 AS POLNTRDM                     
         LA    RF,400                                                           
         XCEF                                                                   
*                                                                               
         LA    R3,POLDATA                                                       
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
         MVC   PEUSRNMS,EUSRNMS                                                 
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   VR04A                                                            
         LA    RE,ENONTDMS         SAVE POL COMSCORE DEMOS                      
         LA    RF,400                                                           
         LA    R0,SVCLIST1         USE SVCLIST1 AS POLNTRDM                     
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
******** MVC   PEDEMLST(60),EDEMLST         NET NOW HAS 50 DEMOS                
*                                                                               
VR04A    OC    EDEM21,EDEM21                IF THERE'S A 21ST DEMO,             
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EDEMLST1(3),EDEM21           LIST AND CLEAR THIS OUT             
         XC    EDEM21,EDEM21                                                    
*                                                                               
******** MVC   PEDEMLST+60(60),EDEMLST1                                         
******** MVC   PEDEMLST+120(30),EDEMLST2                                        
*                                                                               
******** MVC   PEWGTLST(20),EWGTLST                                             
*                                                                               
         OC    EDEM21WT,EDEM21WT            IF THERE'S A 21ST WGT,              
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EWGTLST2(1),EDEM21WT         LIST AND CLEAR THIS OUT             
         XC    EDEM21WT,EDEM21WT                                                
*                                                                               
******** MVC   PEWGTLST+20(30),EWGTLST2                                         
*                                                                               
         MVC   TEMPDEM2(60),EDEMLST           NET HAS 21 DEMOS                  
*                                                                               
         OC    EDEM21,EDEM21                IF THERE'S A 21ST DEMO,             
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EDEMLST1(3),EDEM21           LIST AND CLEAR THIS OUT             
         XC    EDEM21,EDEM21                                                    
*                                                                               
         MVC   TEMPDEM2+60(60),EDEMLST1                                         
         MVC   TEMPDEM2+120(30),EDEMLST2                                        
*                                                                               
         MVC   TEMPWGT2(20),EWGTLST                                             
*                                                                               
         OC    EDEM21WT,EDEM21WT            IF THERE'S A 21ST WGT,              
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EWGTLST2(1),EDEM21WT         LIST AND CLEAR THIS OUT             
         XC    EDEM21WT,EDEM21WT                                                
*                                                                               
         MVC   TEMPWGT2+20(30),EWGTLST2                                         
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
         MVC   ELEN,=Y(ESTHDRLN)                                                
         TM    USRIDFLG,USRRNTKQ     TEST ACCESS TO COMSCORE?                   
         BZ    *+10                    NO                                       
         MVC   ELEN,=AL2(ESTHDR2Q)    YES, SET EXTENDED ELEN                    
*                                                                               
***********************************************************************         
*                                                                               
         LA    R2,ESDDEMSH           DEMOS                                      
         XC    EDEMOS(124),EDEMOS                                               
         XC    EDEMLST1(60),EDEMLST1   DEMOS 21-40                              
         XC    EDEMLST2(30),EDEMLST2   DEMOS 41-50                              
         XC    EDEM21,EDEM21                                                    
         XC    EDEM21WT,EDEM21WT                                                
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
         LA    R2,ESDDEM2H           IF "MENU=" DEMO ...                        
         CLI   5(R2),0               2ND DEMO LINE MUST BE BLANK                
         BNE   SPERREX                                                          
         LA    R2,ESDDEM3H           IF "MENU=" DEMO ...                        
         CLI   5(R2),0               3RD DEMO LINE MUST BE BLANK                
         BNE   SPERREX                                                          
*                                                                               
         MVC   ERRNUM,=AL2(DEMERR3)                                             
         LA    R2,ESDDEMSH           MENU OPTION MUST BE 1-4 CHARACTERS         
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
         CLI   OVSYS,3               FOR NET, SEE IF THERE IS ANOTHER           
         BNE   VR520                 ELEMENT FOR THEM                           
         BRAS  RE,NEXTEL                                                        
         BNE   VR520                                                            
         MVC   EDEM21,2(R5)                                                     
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
*                                                                               
VR540    BRAS  RE,GETTOKEN           SEE IS WE HAVE COMSCORE USER TOKN          
*                                                                               
         MVC   DMCB+4(4),=X'D9000AD9'                                           
         GOTO1 CALLOV,DMCB,0         CALL DEMOVAL                               
         CLI   4(R1),X'FF'                                                      
         JE    *+2                                                              
         L     RF,DMCB                                                          
*                                                                               
VR540A   XC    DMCB,DMCB                                                        
*                                                                               
         L     R1,AIO2              DBLOCK IS USING AIO2                        
         AHI   R1,1024              DEMOVAL PARAM5 USING AIO2+1024              
         USING P5XD,R1                                                          
         MVC   P5XID,=C'P5X '                                                   
         LR    RE,R9                                                            
         AHI   RE,CSUTOKEN-SYSD                                                 
         ST    RE,P5XLICNS          A(32 BYTE COMSCORE LICENSE)                 
         ST    R1,DMCB+16           EXTENDED PARAM5                             
         OI    DMCB+16,X'80'        SO DEMOVAL KNOWS EXTENDED BLOCK             
*                                                                               
*                                   CHG/ADD BRD W/ OR W/O POL?                  
         IF  (CLI,POLSW,EQ,0),OR,                                               
             (CLI,POLSW,EQ,1),OR,                                               
             (OC,SVCLIST1(200),SVCLIST1,NZ),OR,                                 
             (OC,SVCLIST1+200(200),SVCLIST+200,NZ)                              
           LA    RE,SVCLIST1                                                    
           ST    RE,P5XINNTD          A(INPUT NON-TRAD INDX DEMO LIST)          
         ENDIF ,                                                                
         DROP  R1                                                               
*                                                                               
VR540B   GOTO1 (RF),DMCB,(9,ESDDEMSH),(50,BLOCK),(C'S',(R5)),EUSRNMS,, *        
               ENONTDMS                                                         
VR540C   CLI   DMCB+4,0                                                         
         BNE   *+12                                                             
         L     R2,DMCB             POINT TO WHERE THE PROBLEM IS                
         B     ERRDEMO                                                          
*                                                                               
         MVC   EDEMLST,BLOCK         FIRST 20 DEMOS GO HERE                     
         MVC   EDEMLST1,BLOCK+60       LAST ONE HERE                            
         MVC   EDEMLST2,BLOCK+120      LAST ONE HERE                            
*                                                                               
         OC    ENONTDMS,ENONTDMS     ANY NON-TRAD EMOS?                         
         JZ    VR550                  NO                                        
         TM    USRIDFLG,USRRNTKQ     ACCESS TO COMSCORE?                        
         BZ    ERRCOMS                NO                                        
         CLI   ESDMEDK,C'R'           RADIO?                                    
         JE    ERRDEMO                                                          
         CLI   ESDMEDK,C'X'           NTWK RADIO?                               
         JE    ERRDEMO                                                          
*                                                                               
         SR    R1,R1               TEST IF MORE THAN 20 COMSCORE DEMOS          
         LA    RE,50               MAX # OF DEMOS                               
         LA    RF,BLOCK                                                         
VR542    CLI   0(RF),X'FF'         END OF LIST?                                 
         JE    VR543                                                            
         CLI   2(RF),0             COMSCORE DEMO?                               
         BNE   *+8                                                              
         AHI   R1,1                                                             
         AHI   RF,3                                                             
         BCT   RE,VR542                                                         
VR543    CHI   R1,20                                                            
         BH    ERCSMAX                                                          
*                                     YES, IF ADDING A NEW REC, THEN            
*                                     ELEN SHOULD BE EXTENDED, O/W              
         CLC   ELEN,=AL2(ESTHDRLN)    IF CHANGE, WAS REC PREV EXTENDED?         
         BNH   ERRRCEX                 NO                                       
*                                                                               
         MVC   ECSBKTYP,=C'L '       DEFAULT TO LIVE FOR NOW                    
         B     VR550                                                            
*                                                                               
VR545    ZIC   R4,DMCB+4             NUMBER OF DEMOS < 21                       
         MHI   R4,3                  EACH DEMO REQUIRES 3 BYTES                 
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                  STORE DEMOS INTO RECORD (EDEMLST)          
         MVC   EDEMLST(0),BLOCK                                                 
*                                                                               
***********************************************************************         
*                                                                               
VR550    MVC   TEMPDEM3(60),EDEMLST  SAVE THE NEW DEMOS                         
         MVC   TEMPDEM3+60(60),EDEMLST1                                         
         MVC   TEMPDEM3+120(30),EDEMLST2                                        
*                                                                               
         LA    R4,DMAX               CHECK FOR DUPLICATE DEMOS                  
         LA    R5,BLOCK              (R5 HOLDS STEADY AS R6 BUMPS               
VR560    LA    R6,3(R5)              THROUGH)                                   
         LA    R1,DMAX                                                          
*                                                                               
VR570    CLI   1(R6),0               END OF DEMOS?                              
         BE    VR580                                                            
         CLC   0(3,R5),0(R6)         DUPLICATE DEMO?                            
         BE    ERRDUPD                                                          
         LA    R6,3(R6)              NOT A DUPLICATE, KEEP GOING                
         BCT   R1,VR570              TILL THE END OF DEMOS                      
*                                                                               
VR580    LA    R5,3(R5)              BUMP UP R5                                 
         CLI   1(R5),0               END OF DEMOS?                              
         BE    *+8                                                              
         BCT   R4,VR560              NO, KEEP CHECKING FOR DUPLICATES           
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)                                              
         JNH   VR590D                                                           
*                                                                               
         LA    R4,DMAX-1             CHECK FOR DUPLICATE NON-TRAD DEMOS         
         LA    R5,ENONTDMS           (R5 HOLDS STEADY AS R6 BUMPS               
VR590A   LA    R6,L'ENTRDDMO(R5)     THROUGH)                                   
         LA    R1,DMAX-1                                                        
*                                                                               
VR590B   CLI   0(R6),0               END OF DEMOS?                              
         BE    VR590C                                                           
         CLC   0(L'ENTRDDMO,R5),0(R6)   DUPLICATE DEMO?                         
         BE    ERRDUPD                                                          
         LA    R6,L'ENTRDDMO(R6)     NOT A DUPLICATE, KEEP GOING                
         BCT   R1,VR590B             TILL THE END OF DEMOLIST                   
*                                                                               
VR590C   LA    R5,L'ENTRDDMO(R5)     BUMP UP R5                                 
         CLI   0(R5),0               END OF DEMOS?                              
         BE    VR590C10                                                         
         BCT   R4,VR590A             NO, KEEP CHECKING FOR DUPLICATES           
*                                                                               
VR590C10 LA    R5,ENONTDMS           SET NONT DEMO POINTER                      
         BRAS  RE,ADDNONT            GO ADD NONT SEQNUM POINTERS                
*                                                                               
VR590D   CLI   POLSW,0                                                          
         BE    VR760                                                            
*                                                                               
***********************************************************************         
*                                                                               
         CLI   POLSW,3               IF (CHANGING POL RECORD) SEE IF            
         BE    VR595                 ANY DEMOS DELETED FROM DEMO LIST           
         CLI   POLSW,2                                                          
         BNE   VR680                                                            
*                                                                               
         CLC   TEMPDEM2,TEMPDEM3     WITH THIS TEST, OLD AND NEW DEMO           
         BE    VR760                 LIST WILL ONLY MATCH IF THEY CON-          
*                                    TAIN SAME DEMOS IN SAME ORDER              
*                                                                               
*                                                                               
VR591A   LA    R5,TEMPDEM2           BUT IT'S ALSO OK,(IF ALL OLD DEMOS         
         LA    RE,DMAX+1             ARE PRESENT BUT IN A DIFFERENT OR-         
VR591B   LA    R6,TEMPDEM3           DER) OR (IF ALL OLD DEMOS PRESENT          
         LA    RF,DMAX+1             WITH 1 OR MORE ADDITION)                   
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
VR595    XC    TEMPDEM2,TEMPDEM2     IF (ADDING POL RECORD) OR WHILE            
         XC    KEY,KEY               (CHANGING POL RECORD) ANY DEMOS            
         MVC   KEY(4),ESTKEY         WERE DELETED ... MUST LOOP THROUGH         
VR600    GOTO1 =A(NEXTPRD),RR=RELO   ALL BRAND ESTIMATES AND BUILD LIST         
         BNE   VR640                 OF ALL BRAND DEMOS IN POLDEMOS             
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R3,AIO                                                           
         MVC   TEMPDEM3(60),EDEMLST                                             
         MVC   TEMPDEM3+60(60),EDEMLST1                                         
         MVC   TEMPDEM3+120(30),EDEMLST2                                        
         LA    R4,TEMPDEM3                                                      
         LA    R1,DMAX+1                                                        
VR610    LA    R5,TEMPDEM2                                                      
         LA    R6,DMAX+1                                                        
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
         CLC   0(3,R4),=X'FF0000'   READ IN NEXT BRAND ESTIMATE                 
         BE    VR600                                                            
         BCT   R1,VR610                                                         
         B     VR600                                                            
*                                                                               
VR640    CLC   TEMPDEM2(3),=3X'00'                                              
         BE    VR760                                                            
         L     R3,AIO1               COMPARE THE BRAND-BUILT POLDEMOS           
         MVC   TEMPDEM3(60),EDEMLST                                             
         MVC   TEMPDEM3+60(60),EDEMLST1                                         
         MVC   TEMPDEM3+120(30),EDEMLST2                                        
         LA    R4,TEMPDEM2           WITH CHANGED OR ADDED POL DEMOS            
         LA    RE,DMAX+1                                                        
VR650    LA    R5,TEMPDEM3                                                      
         LA    RF,DMAX+1                                                        
VR660    CLC   0(3,R4),0(R5)         IF ANY DEMOS IN POLDEMOS MISSING           
         BE    VR670                 FROM POL RECORD RETURN ERROR               
         LA    R5,3(R5)                                                         
         CLC   0(3,R5),=3X'00'                                                  
         BE    NODEMDEL                                                         
         CLC   0(3,R5),=X'FF0000'                                               
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
         CLC   TEMPDEM2(3),=3X'00'   ESTIMATE FOR A CLIENT WITH A POL           
         BE    SPERREX               ESTIMATE) ... DEMOS MUST BE A              
         MVI   UDSW,0                SUBSET OF POL DEMOS                        
*                                                                               
         LA    R4,TEMPDEM3                                                      
         LA    R1,DMAX+1                                                        
VR690    LA    R5,TEMPDEM2                                                      
         LA    R6,DMAX+1                                                        
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
         CLC   0(3,R4),=X'FF0000'                                               
         BE    VR720                                                            
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
VR752    LA    R5,SVCLIST1                                                      
         LA    R6,DMAX                                                          
VR754    CLC   0(L'ENTRDDMO,R4),0(R5)    IF BRAND DEMO NOT IN POL DEMOS         
         BE    VR756                     ERROR                                  
         AHI   R5,L'ENTRDDMO                                                    
         BCT   R6,VR754                                                         
         J     SPERREX                                                          
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
         CLI   OVSYS,3                                                          
         BNE   VR800                                                            
*                                                                               
         LA    R4,TEMPDEM3           SEE IF TOTAL HMS (C901) IS SOME-           
         LA    R0,DMAX+1             WHERE IN DEMO LIST ...                     
VR770    CLC   0(3,R4),=3X'00'                                                  
         BE    VR780                                                            
         CLC   0(3,R4),=X'FF0000'                                               
         BE    VR780                                                            
         CLC   0(3,R4),=X'00C901'    WHEN TOTAL HMS FOUND SET HMSW              
         BNE   *+12                                                             
         MVI   HMSW,1                                                           
         B     *+8                   WHEN ANY OTHER DEMO FOUND SET              
         MVI   NHMSW,1               NHMSW                                      
         LA    R4,3(R4)                                                         
         BCT   R0,VR770                                                         
VR780    MVC   ERRNUM,=AL2(TOTHERR)                                             
         CLI   HMSW,1                ONLY HOMES IS A VALID INPUT                
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
         LA    R2,ESDWTSH            DEMO WEIGHTS                               
         CLI   5(R2),0                                                          
         BE    VR925                                                            
*                                                                               
         L     R4,AIO3               SCANNER BUILDS TABLE OF DEMOS AT           
         AHI   R4,1000               1000 BYTES BEYOND AIO3                     
         LA    R2,ESDDEMSH                                                      
         ST    R2,SVDEMADR                                                      
         GOTO1 SCANNER,DMCB,(R2),(15,(R4))                                      
         CLI   DMCB+4,0                                                         
         BE    ERR1DEM               # OF DEMOS CANNOT EXCEED 14 OR BE          
         CLI   DMCB+4,DMAX           ZERO                                       
         BH    ERREXC                                                           
         L     R2,SVDEMADR                                                      
         L     R5,AIO3                                                          
         AHI   R5,1000                                                          
VR815    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO NEXT DEMO INPUT LINE                
         LA    R0,ESDDEMLH                                                      
         CR    R2,R0                                                            
         BH    VR820                                                            
         CLI   5(R2),0             TEST FOR MORE INPUT                          
         BE    VR820               NO                                           
         ZIC   R0,DMCB+4                                                        
         MHI   R0,32                                                            
         AR    R5,R0                                                            
         ZIC   RE,DMCB+4                                                        
         LA    R0,DMAX+1                                                        
         SR    R0,RE                                                            
         GOTO1 SCANNER,DMCB,(R2),((R0),(R5))                                    
         CLI   DMCB+4,0                                                         
         BE    ERRDEMO                                                          
         B     VR815                                                            
*                                                                               
VR820    LA    R2,ESDWTSH            PARSNIP BUILDS TABLE OF WEIGHTS            
         L     R4,AIO3               AT 500 BYTES BEYOND AIO3                   
         AHI   R4,500                                                           
         L     RF,ACOMFACS                                                      
         L     RF,CPARSNIP-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(R2),(15,(R4)),0                                       
         MVC   ERRNUM,=AL2(WGHTERR)                                             
         CLI   DMCB+4,0              # OF WEIGHTS CANNOT EXCEED 50 OR           
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
         BE    VR925                 DEMO LIST   <---   R6                      
         ST    R6,FULL                                                          
         LA    R0,DMAX                                                          
         MVC   TEMPDEM3(60),EDEMLST                                             
         MVC   TEMPDEM3+60(60),EDEMLST1                                         
         MVC   TEMPDEM3+60(30),EDEMLST2                                         
         MVC   TEMPWGT3(20),EWGTLST                                             
*                                                                               
         OC    EDEM21WT,EDEM21WT            IF THERE'S A 21ST WGT,              
         BZ    *+16                         MOVE IT TO THE NEW DEMO             
         MVC   EWGTLST2(1),EDEM21WT         LIST AND CLEAR THIS OUT             
         XC    EDEM21WT,EDEM21WT                                                
*                                                                               
         MVC   TEMPWGT3+20(30),EWGTLST2                                         
*                                                                               
         LA    R4,TEMPWGT3           RECORD'S WEIGHT FIELD <--- R4              
         LA    R2,TEMPDEM3           RECORD'S DEMO FIELD   <--- R2              
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
         LA    R2,ESDWTSH            IF WEIGHT NOT FOUND IN DEMO LIST           
         B     SPERREX               RETURN ERROR                               
*                                                                               
***********************************************************************         
*                                                                               
VR860    CLI   BFLD2,C'T'            IF WEIGHT IS A TARGET ... MUST             
         BNE   VR890                 HAVE A LENGTH OF 2, MUST BE EITHER         
         CLI   BFLD2LEN,2            'T1' OR 'T2' AND BE INPUTTED ONLY          
         BNE   ERRDUPT               ONCE                                       
         LA    R1,ETRGLST                                                       
         MVI   WTSFLAG,0                                                        
*                                                                               
         CLC   BFLD2(2),=C'T1'                                                  
         BNE   *+12                                                             
         OI    WTSFLAG,WTST1                                                    
         B     VR870                                                            
*                                                                               
         LA    R1,ETRGLST+3                                                     
         CLC   BFLD2(2),=C'T2'                                                  
         BNE   ERRDUPT                                                          
         OI    WTSFLAG,WTST2                                                    
*                                                                               
VR870    OC    0(3,R1),0(R1)                                                    
         BNZ   ERRDUPT                                                          
VR880    MVC   0(3,R1),0(R2)         MOVE DEMO FROM RECORD INTO TARGET          
         B     VR920                 LIST                                       
*                                                                               
***********************************************************************         
*                                                                               
VR890    CLI   0(R4),0               IF NOT A TARGET ...                        
         BNE   ERRDUPT               MAY ONLY INPUT EACH WEIGHT ONCE            
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
******** CLC   POLDEMOS(3),=3X'00'                                              
         CLC   TEMPDEM2(3),=3X'00'                                              
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
VR925    DS    0H                                                               
         CLC   AGENCY,=C'SJ'                                                    
         BE    *+14                                                             
         CLC   AGENCY,=C'MC'         ONLY FOR MC CANN                           
         BNE   VR930                                                            
         TM    WTSFLAG,WTST1         ENTERED T1 WEIGHT?                         
         BZ    VR930                                                            
         TM    WTSFLAG,WTST2         THEN T2 MUST BE THERE                      
         BZ    WGHTTERR                                                         
*                                                                               
***********************************************************************         
*                                                                               
VR930    LA    R2,ESDDEMSH                                                      
         MVC   ERRNUM,=AL2(WGHTERR4) IF WEIGHT WAS ENTERED WITH NO              
         OC    TEMPWGT3,TEMPWGT3     WEIGHTED DEMO ... RETURN ERROR             
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
         CLI   ACTNUM,ACTADD                                                    
         BE    VR970                                                            
         CLC   TEMPDEM1,TEMPDEM3                                                
         BNE   ERRNOCHG                                                         
         CLC   SVUSRNMS,EUSRNMS                                                 
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
VR979    DS    0H                                                               
**979    GOTO1 =A(VR980),RR=RELO     VALIDATE RATING BOOK & HUT ADJUST          
*                                                                               
***********************************************************************         
*                                                                               
**       GOTO1 =A(VR1340),RR=RELO                                               
*                                                                               
***********************************************************************         
*                                                                               
VR1510   MVC   AIO,AIO1                                                         
***********************************************************************         
*                                                                               
VR1799   GOTO1 =A(VR1800),RR=RELO    RECORD HAS BEEN VALIDATED ...              
VRX      OI    GENSTAT2,RETEQSEL     PUT IT AND REDISPLAY                       
         B     DR                                                               
         DROP  R3                                                               
         EJECT                                                                  
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
**********************************************************************          
*        SPACES TO ZEROS                                                        
**********************************************************************          
SPTOZER  NTR1                                                                   
SPTOZ10  CLI   ESDESTK,C' '          CHANGE LEADING SPACES TO ZEROS             
         BH    SPTOZX                                                           
         MVI   ESDESTK,X'F0'                                                    
         OI    ESDESTKH+6,X'80'      TRANSMIT                                   
         CLI   ESDESTK+1,C' '                                                   
         BH    SPTOZ20                                                          
         MVI   ESDESTK+1,X'F0'                                                  
*                                                                               
SPTOZ20  MVC   ERRNUM,=AL2(ESTERR1)  EST CODE MUST BE NUMERIC                   
         LA    R3,3                                                             
         LA    R4,ESDESTK                                                       
SPTOZ25  CLI   0(R4),X'F9'                                                      
         BH    SPERREX                                                          
         CLI   0(R4),X'F0'                                                      
         BL    SPERREX                                                          
         LA    R4,1(R4)                                                         
         BCT   R3,SPTOZ25                                                       
         OI    ESDESTKH+4,X'08'      SET FOR VALID NUMERIC                      
SPTOZX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
ERRIACT  LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     VSFMERR                                                          
ERR1DEM  MVC   ERRNUM,=AL2(NET1DEM)                                             
         B     SPERREX                                                          
ERRHUTA  MVC   ERRNUM,=AL2(INVHUTA)                                             
         B     SPERREX                                                          
ERRHTBK  MVC   ERRNUM,=AL2(INVHTBK)                                             
         B     SPERREX                                                          
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
DUPPERR1 LA    R2,ESDWTSH            RESET R2 FOR ERROR MESS                    
DUPPERR  MVC   ERRNUM,=AL2(DUPERR)                                              
         B     SPERREX                                                          
*                                                                               
WGHTTERR LA    R2,ESDWTSH            RESET R2 FOR ERROR MESS                    
         MVC   ERRNUM,=AL2(WTSTERRS)                                            
         B     SPERREX                                                          
*                                                                               
WGHTSPR2 LA    R2,ESDWTSH            BRAND AND POL WEIGHTS MUST AGREE           
         MVC   ERRNUM,=AL2(BRPOLER9)                                            
         B     SPERREX                                                          
*                                                                               
WGHTSPER LA    R2,ESDWTSH                                                       
         MVC   ERRNUM,=AL2(WGHTERR3)                                            
         B     SPERREX              WEIGHT INVALID OR MISSING                   
*                                                                               
RATNOWT  LA    R2,ESDWTSH                                                       
         MVC   ERRNUM,=AL2(WGHTERR2)                                            
         B     SPERREX                                                          
*                                                                               
ERRCOMS  LA    R2,ESDDEMSH                                                      
         MVC   ERRNUM,=AL2(COMSERR)                                             
         B     SPERREX                                                          
*                                                                               
ERRRCEX  LA    R2,ESDDEMSH         RECORD MUST BE EXTENDED FIRST                
         MVC   ERRNUM,=AL2(1416)                                                
         B     SPERREX                                                          
*                                                                               
ERCSMAX  LA    R2,ESDDEMSH         CAN'T HAVE MORE THAN 20 COMSCORE             
         MVC   ERRNUM,=AL2(1419)   DEMOS                                        
         B     SPERREX                                                          
*                                                                               
ERRDEMO  XC    CONHEAD,CONHEAD       INIT MYERROR                               
         MVC   CONHEAD(L'FLD1),BLOCK PASS INVALID DEMO                          
         LA    RF,CONHEAD            POINT TO INVALID DEMO                      
ERRDEMO2 CLI   0(RF),C' '            CHECK FOR SPACES                           
         JNH   ERRDEMO4              NO,                                        
         LA    RF,1(RF)              BUMP IT                                    
         B     ERRDEMO2              CHECK FR SPACES                            
ERRDEMO4 MVC   0(18,RF),=C' IS A INVALID DEMO' APPEND ERROR MESSAGE             
         GOTO1 ERREX2                EXIT                                       
*                                                                               
         USING BSCAND,R5                                                        
ERRDUPT  LA    R2,ESDWTSH                                                       
         XC    CONHEAD,CONHEAD       INIT MYERROR                               
         LLC   RF,BFLD1LEN           GET LENGTH OF FLD1                         
         MVC   CONHEAD(0),BFLD1      PASS FLD1 DATA                             
         EX    RF,*-6                                                           
         LA    RF,CONHEAD(RF)        BUMP TO NEXT AVAILABLE POSITION            
         MVI   0(RF),C'='            PASS '='                                   
         LA    RF,1(RF)              BUMP TO NEXT AVAILABLE POSITION            
         LLC   RE,BFLD2LEN           GET LENGTH OF FLD2                         
         MVC   0(0,RF),BFLD2         PASS FLD2 DATA                             
         EX    RE,*-6                                                           
         LA    RF,0(RE,RF)           BUMP TO NEXT AVAILABLE POSITION            
         CLI   BFLD2LEN,2            CHECK FOR FLD2 LENGTH                      
         BNE   ERRDUPT4              NOT 2, ERROUT                              
         CLC   BFLD2(2),=C'T1'       YES, NOT T1                                
         BE    ERRDUPT2              ERROR OUT                                  
         CLC   BFLD2(2),=C'T2'       YES, NOT T2                                
         BNE   ERRDUPT4              ERROR OUR                                  
ERRDUPT2 MVC   0(22,RF),=C' IS A DUPLICATE TARGET'                              
         B     ERRDUPTX              ERROR EXIT                                 
ERRDUPT4 MVC   0(22,RF),=C' IS NOT A VALID TARGET'                              
ERRDUPTX GOTO1 ERREX2                EXIT                                       
         DROP  R5                                                               
*                                                                               
ERRDUPD  XC    CONHEAD,CONHEAD       INIT MYERROR                               
         XC    ELEM,ELEM             INIT ELEM                                  
         USING DBLOCK,ELEM                                                      
         MVC   DBCOMFCS,ACOMFACS     PASS ADDRESS OF COMFACS                    
         MVC   DBFILE,=C'NAD'        DBFILE 'NAD'                               
         MVI   DBSELMED,C'N'         MEDIA NET                                  
         LAY   RF,DEMOCON                                                       
         L     RF,0(RF)              GET ADDRESS OF DEMOCON                     
         GOTO1 (RF),DMCB,(1,0(R5)),(10,CONHEAD),ELEM                            
         LA    RF,CONHEAD            POINT TO MY ERROR                          
ERRDUPD2 CLI   0(RF),C' '            CHECK FOR SPACE                            
         BNH   ERRDUPD4              YES, APPEND ERROR MESSAGE                  
         LA    RF,1(RF)              BUMP IT                                    
         B     ERRDUPD2                                                         
ERRDUPD4 MVC   0(20,RF),=C' IS A DUPLICATE DEMO'                                
         GOTO1 ERREX2                ERROR                                      
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
INVDEMO  EQU   1462                INVALID DEMOS FIELD                          
INVHUTA  EQU   1432                HUT FIELD SHOULD BE AUTO OR A MONTH          
INVHTBK  EQU   0031                BOOK-HUT INVALID                             
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
COMSERR  EQU   1415                UNAUTHORIZED TO USE COMSCORE                 
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
* TABLES                                                                        
***********************************************************************         
*        CONSTANTS                                                    *         
***********************************************************************         
EOTBLQ   DC    C'A'                                                             
DMAX     EQU   49                                                               
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
SETUPX   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
*        VR980 - VALIDATE RATING BOOK AND HUT ADJUSTMENT              *         
***********************************************************************         
VR980    NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R3                                                        
         L     R3,AIO                                                           
         LA    R2,ESDRBKH            RATING BOOK REQUIRED                       
         MVC   ERRNUM,=AL2(RATERR1)                                             
         CLI   5(R2),0                                                          
         BE    SPERREX                                                          
*                                                                               
         XC    EBOOK,EBOOK           IF NETPAK, RATING BOOK MUST BE             
         CLI   OVSYS,3               GREATER THAN ZERO                          
         BNE   VR990                                                            
         CLI   8(R2),C'0'                                                       
         BL    ERRHTBK                                                          
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
         CLI   ACTNUM,ACTADD         ESTIMATES                                  
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
VR1050   LA    R2,ESDHUTH            HUT ADJUSTMENT REQUIRED                    
         CLI   5(R2),0                                                          
         BNE   *+14                  IF NOT THERE, FILL IN AUTO                 
         MVC   8(4,R2),=C'AUTO'                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         MVI   EHUTADJ,0             IF HUT ADJUSTMENT NOT 'AUTO' ...           
         CLC   8(4,R2),=C'AUTO'      MUST BE 3 CHARACTERS LONG AND A            
         BE    VR1060                VALID MONTH                                
         CLI   5(R2),3                                                          
         BNE   ERRHUTA                                                          
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
         CLI   ACTNUM,ACTADD         POL ESTIMATES                              
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
         LA    R2,ESDRBKH            POL MUST AGREE WITH ALL BRAND'S            
         CLC   EBOOK,SVBOOK          RATING BOOKS, HUT ADJUSTMENTS,             
         BNE   SPERREX               DAYPART MENUS AND CONTROLS                 
         MVC   ERRNUM,=AL2(BRPOLER5)                                            
         LA    R2,ESDHUTH                                                       
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
         B     VR1350                                                           
VR1507   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
********************************************************************            
*        VR1800 - ADD OR MODIFY EST AND UPDATE CASH                *            
********************************************************************            
VR1800   NTR1  BASE=*,LABEL=*                                                   
         USING ESTHDR,R3                                                        
         L     R3,AIO                                                           
***************************                                                     
         B     VR1820                                                           
***************************                                                     
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
         MVC   ELEN,=Y(ESTHDRLN)     ESTIMATE RECORD WITH TRADE                 
         OI    ECNTRL,X'01'          PRODUCT CODE                               
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
         GOTO1 =A(ADREC),RR=RELO                                                
         MVC   ESTKEY,KEY                                                       
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
         MVC   AIO,AIO1                                                         
         L     R3,AIO                                                           
*                                                                               
         MVI   NEWLEN,C'N'                                                      
         L     RE,AIO3                                                          
         CLC   ELEN-ESTHDR(2,RE),ELEN   ESTHDR SAME LEN                         
         JE    *+8                                                              
         MVI   NEWLEN,C'Y'                                                      
*                                                                               
******   MVC   ERRNUM,=AL2(DLYERR)   IF ESTIMATE RECORD IS DAILY ...            
******   CLI   EDAILY,C' '           MAXIMUM 53 DAYS BBETWEEN START             
******   BNH   VR1830                AND END DATES                              
******   CLI   EDAILY,C'N'                                                      
******   BE    VR1830                                                           
******   L     RF,ACOMFACS                                                      
******   USING COMFACSD,RF                                                      
******   GOTO1 PERVERT,DMCB,ESTART,EEND,WORK,WORK+4,WORK+8                      
******   LH    R4,8(R1)                                                         
******   CH    R4,=H'53'                                                        
******   BH    SPERREX                                                          
*                                                                               
VR1830   GOTO1 DATCON,DMCB,(5,0),(2,ECHDATE)     DATE OF CHANGE                 
         GOTO1 =A(PTREC),RR=RELO         PUT THE ESTIMATE                       
***************************                                                     
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
         JE    VR1850                                                           
         CLC   QPRD,=C'POL'                                                     
         JNE   VR1850                                                           
         CLC   ELEN,=AL2(ESTHDRLN)  DOES EST HAVE NONT DEMO NAMES               
         JNH   VR1850                                                           
*                                                                               
         CLC   SVCLIST(200),ENONTDMS  COMPARE OLD/NEW NONT DEMO NAMES           
         JNE   *+14                                                             
         CLC   SVCLIST+200(200),ENONTDMS+200                                    
         JE    VR1850                                                           
         LA    RE,ENONTDMS          MOVE NEW LIST TO SAVE AREA STUPIDO!         
         LA    RF,400                                                           
         LA    R0,SVCLIST1                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    KEY,KEY             LOOP THROUGH ALL BRANDS                      
         MVC   KEY(4),ESTKEY                                                    
*                                                                               
VR1832   GOTO1 =A(NEXTPRD),RR=RELO                                              
         BNE   VR1834                                                           
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R3,AIO               AT PRESENT, THIS IS AIO1                    
         USING ESTHDR,R3 <===  CODE ALWAYS USED R3, JUST CLARIFYING             
*                                                                               
         CLC   ELEN,=AL2(ESTHDRLN)  DOES EST HAVE NONT DEMO NAMES               
         JNH   VR1832                                                           
         CLC   SVCLIST(200),ENONTDMS  DO LISTS MATCH?                           
         JNE   *+14                                                             
         CLC   SVCLIST+200(200),ENONTDMS+200                                    
         JE    VR1832                                                           
         LA    RE,SVCLIST1                                                      
         LA    RF,400                                                           
         LA    R0,ENONTDMS                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 =A(PTREC),RR=RELO     UPDATE THE RECORD                          
         J     VR1832                                                           
*                                                                               
VR1834   L     RE,AIO3               RESTORE NEW POL RECORD                     
         SR    RF,RF                                                            
         ICM   RF,3,ELEN-ESTHDR(RE)  GET ESTHDR LEN                             
         L     R0,AIO1                                                          
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   KEY,ESTKEY           RESTORE POL ESTHDR KEY                      
         GOTO1 READ                                                             
***************************                                                     
         B     VR1865                                                           
***************************                                                     
********************************************************************            
*                                                                               
         CLI   WORK,C'S'             IF STATUS WAS CHANGED...                   
         BNE   VR1850                READ EST REC...WRITE CNTRL                 
         GOTO1 READ                  BYTE TO KEY                                
         MVC   KEY+13(1),ECNTRL                                                 
         GOTO1 WRITE                                                            
         B     VR1885                                                           
********************************************************************            
*                                                                               
VR1850   B     VR1860                                                           
*VR1850   OC    CASHPRD,CASHPRD                                                 
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
         BNE   VR1865                                                           
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVC   AIO,AIO2              READ OLD CASH PRD EST INTO AIO2            
         L     R3,AIO                REMOVE TRADE PRODUCT AND PUT               
         GOTO1 GETREC                                                           
         MVI   ETRDPRD-ESTHDR(R7),0                                             
         GOTO1 =A(PTREC),RR=RELO                                                
*                                                                               
********************************************************************            
*                                                                               
VR1865   L     R3,AIO1                                                          
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
         CLC   QMED,SPACES                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   QCLT,SPACES                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
         CLC   QPRD,SPACES                                                      
         BH    *+6                                                              
         DC    H'0'                                                             
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
***************************                                                     
         B     VR1885                                                           
***************************                                                     
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    VR1885              DONT UPDATE BUYLINE ON ADD                   
*                                                                               
         CLC   ECOST2,SVECOST2     ECOST2 CHANGED?                              
         BE    VR1885                                                           
*                                                                               
         XC    WORK,WORK             CHECK PROFILE - C3 REQUEST NEEDED?         
         MVC   WORK+16(4),=C'S0B0'   B0 PROFILE                                 
         MVC   WORK+20(2),AGENCY     PROFILE NAME                               
         MVC   WORK+22(1),QMED                                                  
         MVC   WORK+23(3),QCLT                                                  
         GOTO1 GETPROF,DMCB,WORK+16,WORK,DATAMGR                                
         CLI   WORK+1,C'Y'                                                      
         BNE   VR1885                                                           
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
VR1885   MVC   AIO,AIO1                                                         
         XIT1                                                                   
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
         LA    R5,TEMPDEM2           R2 ---> BRAND DEMOS                        
         LA    R6,TEMPWGT2           R4 ---> BRAND WEIGHTS                      
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
*                                                                               
* CODE BELOW IS INTENDED TO KEEP NONT NAME LISTS IN PERMANENT SYNCH!            
*                                                                               
         CLC   QPRD,=C'POL'        IF DOING POL, SKIP THIS                      
         JE    PTREC2                                                           
         CLC   ELEN,=AL2(ESTHDRLN) OR IF NO NONT NAMES, SKIP THIS               
         JNH   PTREC2                                                           
*        MVC   ENONTDMS(160),POLNTRDM  THEN KEEP NONT NAMES IN SYNC             
*                                                                               
         LA    RE,SVCLIST1             THEN KEEP NONT NAMES IN SYNC             
         LA    RF,400                                                           
         LA    R0,ENONTDMS                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
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
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* GET COMSCORE USER TOKEN                                                       
***********************************************************************         
         USING GTTOKD,R2                                                        
GETTOKEN NTR1  BASE=*,LABEL=*,WORK=(R2,GTTOKL)                                  
         LR    RE,R9                                                            
         AHI   RE,CSUTOKEN-SYSD                                                 
         XC    0(L'CSUTOKEN,RE),0(RE)  CLEAR THE COMSCORE USER TOKEN            
*                                                                               
         TM    USRIDFLG,USRRNTKQ    DID WE FIND THE TOKEN EARLIER?              
         JZ    GTOKENX              NO WE DIDN'T, THEN NO NEED                  
         L     RA,ATWA                                                          
         USING T31CFFD,RA                                                       
         MVC   HALF,TWAAGY          SAVE OFF THE AGENCY ALPHA                   
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING CT5KEY,R4                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,TWAAGY                                                  
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD '),=C'CTFILE ',KEY,GTTOKIO             
         LA    R4,GTTOKIO                                                       
         LA    R5,CT5DATA          POINT TO THE FIRST ELEMENT IN REC            
         USING CTSEAD,R5                                                        
         CLI   0(R5),CTSEAELQ      ALREADY ON SECURITY ALPHA ELEM?              
         JE    GTOKEN10            YES                                          
         MVI   ELCODE,CTSEAELQ     NO, FIND SECURITY ALPHA ELEM                 
         BRAS  RE,NEXTEL                                                        
         JNE   *+10                                                             
GTOKEN10 MVC   HALF,CTSEAAID       GET SECURITY AGENCY ALPHA AS WELL            
         DROP  R5                                                               
                                                                                
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING TOKRECD,R4          BUILD TOKEN RECORD KEY                       
         MVI   TOKKMIN,TOKKMINQ                                                 
         MVI   TOKKTYP,TOKKRTRK                                                 
         MVC   TOKKSAGY,HALF       SECURITY AGENCY ALPHA                        
         MVC   TOKKAAGY,TWAAGY     AND AGENCY ALPHA                             
         MVI   TOKKSYS,X'03'       NET SYSTEM                                   
         MVC   KEYSAVE,KEY                                                      
                                                                                
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI '),=C'GENDIR ',KEY,KEY                 
         CLC   KEY(L'TOKKEY),KEYSAVE                                            
         JNE   GTOKENX                                                          
                                                                                
         LA    R4,GTTOKIO                                                       
         GOTO1 DATAMGR,DMCB,=C'GETREC',=C'GENFIL ',KEY+36,(R4),WORK             
                                                                                
         AHI   R4,TOKFIRST                                                      
         USING RTAUTHD,R4                                                       
         CLI   0(R4),RTAUTELQ      X'0A' ELEMENT                                
         JNE   GTOKENX                                                          
         OC    RTAUTID(L'RTAUTID+L'RTAUTSEC),RTAUTID                            
         JZ    GTOKENX                                                          
         OI    USRIDFLG,USRRNTKQ   HAS ACCESS TO RENTRAK DEMOS                  
         LR    RE,R9                                                            
         AHI   RE,CSUTOKEN-SYSD                                                 
         MVC   0(L'CSUTOKEN,RE),RTAUTID  SAVE USER TOKEN FOR DEMOVAL            
*                                                                               
GTOKENX  DS    0H                                                               
         J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
GTTOKD   DSECT                                                                  
GTTOKIO  DS    XL2000                                                           
GTTOKL   EQU   *-GTTOKD                                                         
T31C54   CSECT                                                                  
*                                                                               
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
         LA    R4,EPAID                                                         
         LA    R0,EPAIDX                                                        
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
         DROP  R4                                                               
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
         DC    CL8'ESTIMATE'       RECORD:                                      
MPF05ACT DC    CL8'DISPLAY '       ACTION:                                      
MPF05    DC    AL1(KEYTYTWA,L'ESDMEDK-1),AL2(ESDMEDK-T31CFFD)                   
MPF05X   EQU   *                                                                
*                                                                               
         DC    AL1(MPF06X-*,06,PFTCPROG,(MPF06X-MPF06)/KEYLNQ,0)                
         DC    CL3'   '                                                         
         DC    CL8'ESTIMATE'       RECORD:                                      
MPF06ACT DC    CL8'CHANGE  '       ACTION:                                      
MPF06    DC    AL1(KEYTYTWA,L'ESDMEDK-1),AL2(ESDMEDK-T31CFFD)                   
MPF06X   EQU   *                                                                
*                                                                               
         DC    AL1(RETCALL-*,12,PFTRPROG,0,0)                                   
         DC    CL3' ',CL8' ',CL8' '                                             
RETCALL  EQU   *                                                                
         DC    X'FF'                                                            
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
         CLC   ENONTDMS(200),SVCLIST1 ANY NEW NONT DEMOS?                       
         JNE   *+14                                                             
         CLC   ENONTDMS+200(200),SVCLIST1+200                                   
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
         LA    R0,50                                                            
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
ADDNONT4 LR    R2,R1               R2 = A(ALPHA DEMO W/O X OR RX)               
         MVC   NTDKALPH(6),0(R1)   MOVE ALPHA DEMO W/O X OR RX                  
         MVC   DUB,0(R1)           SAVE ACTUAL ALPHA                            
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
         MVC   NTSQKALPH(6),0(R2)                                               
         MVC   KEY+14(3),=X'FFFFFF'  SET DIRECTORY ONLY FLAG                    
         GOTO1 ADD                   ADD NEW 0D2A POINTER                       
         DROP  R6                                                               
*                                                                               
ADDNONT6 LA    R5,8(R5)                                                         
         JCT   R0,ADDNONT2                                                      
         J     EQXIT                                                            
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
       ++INCLUDE NESFMFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE NESFM97D          EST DEMO MAINTENANCE SCREEN                  
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
       ++INCLUDE CTGENFILE                                                      
         EJECT                                                                  
       ++INCLUDE GEGENTOK                                                       
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
       ++INCLUDE SPGENNTDEM                                                     
       ++INCLUDE DEDEMOVALD                                                     
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
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
PROKEY   DS    CL13                                                             
ESTKEY   DS    CL13                                                             
****************SAVEKEY  DS    XL13                                             
WTEND    DS    A                                                                
*                                                                               
CASHPRD  DS    CL3                                                              
OLDCASH  DS    CL3                                                              
OLDCPRD  DS    XL1                                                              
*                                                                               
SVPRD    DS    X                                                                
SVPOLPW  DS    XL3                                                              
NEWLEN   DS    CL1                                                              
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
**********************PSTOUT   DS    CL64                                       
OPTNFLAG DS    XL1                                                              
TALOPTN  EQU   X'80'                                                            
FLAG1    DS    XL1                                                              
FLAG2    DS    XL1                                                              
LEN      DS    XL1                                                              
UTYPE    DS    CL1                                                              
USERDATA DS    CL32                                                             
*                                                                               
**********************SVADVLST DS    CL30                                       
**********************FAKEFLD  DS    XL11                                       
*                                                                               
ERRNUM   DS    XL2                                                              
SAVESEL  DS    CL1                                                              
WORK2    DS    CL48                                                             
SVADVDA  DS    CL4                                                              
SVADVAGY DS    XL1                                                              
**********************SVADVKEY DS    XL13                                       
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
***********   SVESTIM  DS    0CL162                                             
SVESTIM  DS    0CL39                                                            
SVCNTRL  DS    XL1                                                              
SVSTRT   DS    CL6                                                              
SVEND    DS    CL6                                                              
SVELOCK  DS    XL2                                                              
SVOWS    DS    XL1                                                              
SVECOST2 DS    XL4    ** CHANGED FROM DS  F **                                  
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
****                                                                            
SVDEMOS  DS    0CL244                                                           
SVDEMLST DS    CL150                                                            
SVWGTLST DS    XL50                                                             
SVUSRNMS DS    CL28                                                             
SVWGTNM  DS    CL7                                                              
         DS    CL9                                                              
****                                                                            
SVNETYM  DS    XL2                                                              
SVBDSTM  DS    XL3                                                              
SVBDENDM DS    XL3                                                              
*                                                                               
POLDATA  DS    0D                                                               
POLSTRT  DS    CL6                                                              
POLEND   DS    CL6                                                              
POLOWDAY DS    XL1                                                              
POLDEMOS DS    0CL44                                                            
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
*                                                                               
CLIPRO   DS    CL10                                                             
SVDEMADR DS    A                                                                
*                                                                               
WTSFLAG  DS    XL1                                                              
WTST1    EQU   X'01'               ENTERED T1 DEMO WEIGHT                       
WTST2    EQU   X'02'               ENTERED T2 DEMO WEIGHT                       
*                                                                               
TEMPDEM1 DS    XL150               USED TO SAVE DEMO FROM RECORD                
TEMPDEM2 DS    XL150               USED TO SAVE POL DEMOS                       
TEMPDEM3 DS    XL150               USED TO SAVE THE NEW DEMOS                   
TEMPWGT1 DS    XL50                                                             
TEMPWGT2 DS    XL50                                                             
TEMPWGT3 DS    XL50                                                             
*                                                                               
MYBLOCK  DS    XL560                                                            
*                                                                               
DEMOCON  DS    F                                                                
*                                                                               
CSUTOKEN DS    CL32                COMSCORE USER TOKEN                          
****                                                                            
*  RELOCATED FIELDS TO KEEP NESFM03 AND NESFM54 IN SYN                          
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'118NESFM54   11/09/18'                                      
         END                                                                    
