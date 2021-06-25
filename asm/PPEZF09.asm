*          DATA SET PPEZF09    AT LEVEL 064 AS OF 09/26/97                      
*PHASE T43009A,+0,NOAUTO                                                        
*        TITLE 'PPEZF09 - EPIC - INVOICE REPORT'                                
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT'                                
***********************************************************************         
*                                                                     *         
*  REGISTER USAGE                                                     *         
*                                                                     *         
*  R0 - OS & WORK                                                     *         
*  R1 - OS & WORK                                                     *         
*  R2 - WORK                                                          *         
*  R3 - WORK                                                          *         
*  R4 - WORK                                                          *         
*  R5 - WORK                                                          *         
*  R6 - EZBLOCKD                                                      *         
*  R7 - SUBROUTS BASE                                                 *         
*  R8 - SPOOLD                                                        *         
*  R9 - SYSD                                                          *         
*  RA - TWAD                                                          *         
*  RB - FIRST BASE REG                                                *         
*  RC - GEND                                                          *         
*  RD - STD REG SAVE CHAIN                                            *         
*  RE - OS & WORK                                                     *         
*  RF - OS & WORK                                                     *         
*                                                                     *         
*  AIO1  WORKER INDEX                                                 *         
*  AIO2  WORKER BUFFER                                                *         
*  AIO3  EZBLOCK                                                      *         
*                                                                     *         
***********************************************************************         
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - CHANGE LOG'                   
***********************************************************************         
*                                                                     *         
*        CHANGE LOG                                                   *         
*                                                                     *         
***********************************************************************         
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - INITIALIZATION'               
***********************************************************************         
*                                                                     *         
*        INITIALIZATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         PRINT NOGEN                                                            
T43009   CSECT                                                                  
         NMOD1 0,T43009,RR=R2                                                   
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO09                                                        
         MVC   AIO,AIO1                                                         
         L     R6,AIO3                                                          
         USING EZBLOCKD,R6                                                      
*                                                                               
         L     R1,=A(WRKFBUFR-SYSD)   SET UP WORKER BUFFER ADDRESS              
         LA    R1,SYSD(R1)                                                      
         ST    R1,WRKFBUFA                                                      
*                                                                               
         L     R7,=A(SUBROUTS)     ESTABLISH COMMON SUBROUTINES                 
         A     R7,RELO09           RELOCATE ADDRESS                             
         USING SUBROUTS,R7                                                      
*                                                                               
         L     RF,=A(MYREGS)                                                    
         A     RF,RELO09                                                        
         STM   R6,RC,0(RF)         SAVE REGS FOR MYPRINT                        
*                                                                               
         ST    RC,VGEND            SAVE AGEND                                   
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - MODE'                         
***********************************************************************         
*                                                                     *         
*        MODE EVALUATION                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT RECORDS                                
         BE    LRR                                                              
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - VKEY'                         
***********************************************************************         
*                                                                     *         
*        KEY VALIDATION                                               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
VKEY     MVI   ERROR,INVACT        ONLY VALID ACTION IS LIST                    
         LA    R2,CONACTH                                                       
         CLI   ACTNUM,ACTCHA                                                    
         BE    VKERRX                                                           
         CLI   ACTNUM,ACTADD                                                    
         BE    VKERRX                                                           
         CLI   ACTNUM,ACTDEL                                                    
         BE    VKERRX                                                           
         CLI   ACTNUM,ACTREST                                                   
         BE    VKERRX                                                           
         CLI   ACTNUM,ACTDIS                                                    
         BE    VKERRX                                                           
*                                                                               
         LA    R2,IRPMEDH          POINT TO MEDIA FIELD                         
*                                                                               
         MVI   CURSYST,C'M'        INDICATE WE ARE IN MEDIA SYS-PRINT           
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA FILE                         
*                                                                               
         GOTO1 VALIMED             VALIDATE MEDIA                               
*                                                                               
         LA    R2,IRPPUBH          PUB (REQUIRED) - ALL IS OK                   
         XC    SVPUB,SVPUB                                                      
*                                                                               
         GOTO1 ANY                                                              
*                                                                               
         CLC   =C'ALL',WORK                                                     
         BNE   VK100                                                            
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VK100                                                            
*                                                                               
         MVC   SVPUB(3),=C'ALL'                                                 
*                                                                               
         B     VK120                                                            
*                                                                               
VK100    GOTO1 VALIPUB                                                          
*                                                                               
         MVC   SVPUB,QPUB                                                       
*                                                                               
VK120    DS    0H                                                               
*                                                                               
*                                  OPTIONS LINE                                 
         LA    R2,IRPOPTH                                                       
         XC    RQOPTS,RQOPTS       CLEAR OPTS                                   
*                                                                               
         CLI   5(R2),0                                                          
         BE    VK140                                                            
*                                                                               
         BAS   RE,VOPT                                                          
*                                                                               
VK140    LA    R2,IRPBDTH          BATCH DATE (REQUIRED)                        
         XC    SVDTES,SVDTES                                                    
*                                                                               
         CLI   5(R2),0             ANY ENTRY                                    
         BNE   VK144                                                            
*                                                                               
*  OPTION TODAYC - LOADED TODAY  OR OPTION TODAY LOADED TODAY                   
*                                                                               
         TM    OPTNSW,OPTTDYC+OPTTDYL                                           
         BNZ   VK200                                                            
*                                                                               
VK144    GOTO1 ANY                                                              
*                                                                               
         GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
*                                                                               
         ICM   R3,15,DMCB                                                       
         BZ    VKBADDT                                                          
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDTESTR)                                
*                                                                               
         CLM   R3,1,5(R2)          ONLY 1 DATE ENTERED                          
         BE    VK160                YES                                         
         LA    R3,1+8(R2,R3)                                                    
*                                                                               
         GOTO1 DATVAL,DMCB,(0,(R3)),WORK                                        
*                                                                               
         OC    DMCB,DMCB                                                        
         BZ    VKBADDT                                                          
*                                                                               
         GOTO1 DATCON,(R1),(0,WORK),(2,SVDTEEND)                                
*                                                                               
         B     VK200                                                            
*                                                                               
VK160    MVC   SVDTEEND,SVDTESTR                                                
*                                                                               
VK200    LA    R2,IRPBSQH          BATCH SEQ (OPTIONAL)                         
         XC    SVSEQ,SVSEQ                                                      
         XC    SVBSEQ,SVBSEQ                                                    
         CLI   5(R2),0             ANY INPUT                                    
         BE    VK300                                                            
*                                                                               
         MVI   ERROR,NOTNUM                                                     
*                                                                               
         MVC   WORK(8),=8C'0'                                                   
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,VKMVN                                                         
         EX    R1,VKCLC                                                         
         BNE   VKERRX                                                           
         EX    R1,VKPK                                                          
         OI    DUB+7,X'0F'                                                      
         CVB   R0,DUB                                                           
         UNPK  SVSEQ,DUB                                                        
*                                                                               
         STCM  R0,3,SVBSEQ                                                      
         B     VK300                                                            
*                                                                               
VKMVN    MVN   WORK(0),8(R2)                                                    
VKCLC    CLC   WORK(0),8(R2)                                                    
VKPK     PACK  DUB,8(0,R2)                                                      
*                                                                               
VK300    LA    R2,IRPINVH          INVOICE (OPTIONAL)                           
         XC    SVINV,SVINV                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BE    VKXIT                                                            
*                                                                               
         MVC   SVINV,SPACES        INIT INVOICE NUMBER SAVEAREA                 
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   SVINV(0),8(R2)      SAVE INVOICE NUMBER FILTER                   
*                                                                               
VKXIT    DS    0H                                                               
         XC    KEY,KEY                                                          
*                                                                               
         CLI   OFFLINE,C'Y'                                                     
         BE    VKEYX                                                            
*                                                                               
         MVI   CURSYST,C'M'        INDICATE WE ARE IN MEDIA SYS-PRINT           
*                                                                               
         GOTO1 VALIFAS             SWITCH TO MEDIA SYSTEM                       
*                                                                               
         MVC   SYSDIR(3),=C'PRT'                                                
         MVC   SYSFIL(3),=C'PRT'                                                
*                                                                               
VKEYX    DS    0H                                                               
*                                                                               
         B     EXIT                                                             
*                                                                               
VKNOTNUM MVI   ERROR,NOTNUM        NOT NUMERIC                                  
         B     VKERRX                                                           
*                                                                               
VKBADDT  MVI   ERROR,INVDATE       INVALID DATE                                 
         B     VKERRX                                                           
*                                                                               
VKERRX   DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - LRR'                          
***********************************************************************         
*                                                                     *         
*        PRINT INVOICE REPORT                                         *         
*           INITIALIZATION                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
LRR      DS    0H                                                               
*                                                                               
*        INITIALIZE PZMOD                                                       
*                                                                               
         L     RF,=A(HDHK)         HEADLINE ROUTINE                             
         A     RF,RELO09                                                        
         ST    RF,HEADHOOK                                                      
*                                                                               
         GOTO1 =A(INITPZB),RR=RELO09  INITIALIZE EZBLOCKD                       
*                                                                               
*        OPEN SORT IF NEEDED                                                    
*                                                                               
         CLI   SRTOPT,0            SKIP IF NOT A SORTED REQUEST                 
         BE    LRROPNX                                                          
*                                                                               
         LM    R4,R5,=A(SORTCARD,RECCARD)                                       
         A     R4,RELO09                                                        
         A     R5,RELO09                                                        
*                                                                               
         GOTO1 SORTER,DMCB,(R4),(R5)                                            
*                                                                               
LRROPNX  DS    0H                                                               
*                                                                               
*        SET OPTIONS FOR EZMOD IN EZBLOCK                                       
*        --------------------------------                                       
*                                                                               
LRRTRC   DS    0H                                                               
*                                                                               
         MVI   EZTRACE,0           TRACE OPTION                                 
*                                                                               
         CLI   TRCOPT,C'N'                                                      
         BE    LRRTRCX                                                          
*                                                                               
         CLI   TRCOPT,C' '                                                      
         BE    LRRTRCX                                                          
*                                                                               
         CLI   TRCOPT,C'A'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'FF'       TRACE EVERYTHING                             
*                                                                               
         CLI   TRCOPT,C'Y'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'FF'       TRACE EVERYTHING                             
*                                                                               
         CLI   TRCOPT,C'E'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'20'       TRACE ERRORS                                 
*                                                                               
         CLI   TRCOPT,C'M'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'60'       TRACE MODES AND ERRORS                       
*                                                                               
         CLI   TRCOPT,C'F'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'E0'       TRACE FIELDS AND ERRORS AND MODES            
*                                                                               
         CLI   TRCOPT,C'R'                                                      
         BNE   *+8                                                              
         MVI   EZTRACE,X'70'       TRACE MODES, ERRORS, AND ADDED RECS          
*                                                                               
LRRTRCX  DS    0H                                                               
*                                                                               
         XC    EZWKRIND,EZWKRIND                                                
*                                                                               
*        SET UP BOXES PARAMETERS                                      *         
*                                                                               
         ICM   R3,15,ABOX          IS ABOX ZEROS                                
         BZ    LRRBOXX             YES/ ON-LINE SKIP BOXES                      
         USING BOXD,R3                                                          
*                                                                               
         MVI   BOXWT,1                                                          
         MVI   BOXINIT,0                                                        
         MVI   BOXOFF,0           (ONLY FOR SPOOF)                              
*                                                                               
         MVC   BOXWIDTH,=F'165'    SET LINE WIDTH                               
         MVI   BOXFONT,1                                                        
*                                                                               
***      =================  ***                                                 
         L     R4,BOXAWIDE         ESTABLISH WIDE LINES                         
         USING WIDED,R4                                                         
***      =================  ***                                                 
*                                                                               
         LA    R1,XHEAD1                                                        
         ST    R1,AH1                                                           
*                                                                               
         LA    R1,XMID1                                                         
         ST    R1,AM1                                                           
*                                                                               
         LA    R1,XP                                                            
         ST    R1,AP1                                                           
*                                                                               
         LA    R1,XSPACES                                                       
         ST    R1,ASPACES                                                       
*                                                                               
LRRBOXX  DS    0H                                                               
*                                                                               
         DROP  R3                                                               
         DROP  R4                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* READ THROUGH WORKER FILE, LOOKING FOR SELECTED INVOICES *                     
*                                                                               
LRRBATLP DS    0H                                                               
*                                                                               
*        POINT TO NEXT WORKER RECORD                                            
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,EZWKRIND,AIO1,WRKFBUFA             
*                                                                               
         TM    DMCB+8,X'80'        TEST EOF                                     
         BO    LRRBATDN             YES - ALL DONE                              
*                                                                               
         LA    R4,EZWKRIND         ESTABLISH WORKER FILE INDEX RECORD           
         USING EZWKRIXD,R4                                                      
*                                                                               
         CLI   EZWIDAY,X'98'       SKIP IF NOT AN EPIC FILE                     
         BNE   LRRBATCN                                                         
*                                                                               
         CLC   EZWIUID,TWAORIG     SKIP IF NOT RIGHT ID                         
         BNE   LRRBATCN                                                         
*                                                                               
         OC    QMED,QMED           IF SPECIFIC MEDIA ASKED FOR                  
         BZ    *+14                                                             
         CLC   EZWIMED,QMED           SKIP IF NOT RIGHT MEDIA                   
         BNE   LRRBATCN                                                         
*                                                                               
*****    OC    RQPZPUB,RQPZPUB     IF SPECIFIC PUB ASKED FOR                    
*****    BZ    *+14                                                             
*****    CLC   EZWIPUB,RQPZPUB        SKIP IF NOT RIGHT PUB                     
*****    BNE   LRRBATCN                                                         
*                                                                               
         OC    QPUB,QPUB           IF SPECIFIC PUB ASKED FOR                    
         BZ    LRRBATPX                                                         
*                                                                               
*        FIND PUB IN PUB TRANSLATE TABLE                                        
*                                                                               
         LA    RF,PUBPTRTB         PUB ENTRY IN PZTABS                          
         USING PTRTABD,RF                                                       
*                                                                               
         CLC   EZWIPUB,PTRPZPUB    AND PUB (EXTERNAL CODE)                      
         BNE   LRRBATCN                                                         
*                                                                               
         DROP  RF                                                               
*                                                                               
LRRBATPX DS    0H                                                               
*                                                                               
*        READ FIRST WORKER RECORD                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'READ',EPICWK,EZWKRIND,AIO1,WRKFBUFA              
         TM    DMCB+8,X'80'        IF EOF ON FIRST READ                         
         BNZ   LRRBATDN            SKIP- GET NEXT INDEX                         
*                                                                               
         L     R5,WRKFBUFA                                                      
         USING W_RECD,R5                                                        
*                                                                               
*        APPLY SECONDARY FILTERS THAT DEPEND ON DATA IN RECORD                  
*                                                                               
* SEE IF IN DATE RANGE                                                          
*                                                                               
         OC    SVDTES,SVDTES       WERE DATES ENTERED                           
         BZ    LRRDTSX                                                          
*                                                                               
         CLC   W_DATEL,SVDTESTR                                                 
         BL    LRRBATCN                                                         
*                                                                               
         CLC   W_DATEL,SVDTEEND                                                 
         BH    LRRBATCN                                                         
*                                                                               
LRRDTSX  DS    0H                                                               
*                                                                               
*        APPLY FILTERS FROM OPTIONS                                             
*                                                                               
         OC    SVBSEQ,SVBSEQ         SEQ                                        
         BZ    *+14                                                             
         CLC   W_FILENO,SVBSEQ                                                  
         BL    LRRBATCN                                                         
*                                                                               
         OC    OPTSRCE,OPTSRCE       SOURCE                                     
         BZ    LRRSRCX                                                          
*                                                                               
         LA    RE,W_DESC                                                        
         CLC   OPTSRCE,EZWCSRCE-EZWKRCMD(RE)                                    
         BNE   LRRBATCN                                                         
*                                                                               
LRRSRCX  DS    0H                                                               
*                                                                               
         DROP  R5                                                               
*                                                                               
         L     R3,EZWKRBUF         SAVE 64 BYTE DESCRIPTOR REC                  
         MVC   SVWFREC0,0(R3)                                                   
*                                                                               
         USING W_RECD,R3                                                        
*                                                                               
         MVC   EZPYFRST(EZPYDLEN),SPACES  INIT PAYEE DATA                       
*                                                                               
         OI    EZLOOKSW,EZLWRGOP   SET TO IGNORE WSJ REGIONAL COMBO             
*                                  (OPTION- SO Z9 WILL DO ALL INSERTS)          
*                                                                               
         GOTO1 VPZMOD,DMCB,EZBLOCKD  CALL PZMOD TO PROCESS INVOICE              
*                                                                               
LRRBATCN DS    0H                                                               
*                                                                               
         B     LRRBATLP            NEXT INDEX                                   
*                                                                               
LRRBATDN DS    0H                                                               
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*        IF SORTING, NOW CLOSE SORT AND REPEAT ABOVE FOR SORTED RECS            
*                                                                               
         CLI   SRTOPT,C'Y'         SORT REQUIRED                                
         BNE   LRRSORTX             NO                                          
*                                                                               
         MVI   SRTOPT,C'Z'         SET TO OUTPUT PASS FOR SORT                  
*                                                                               
         L     R5,VPZMOD                                                        
         GOTO1 =A(INITPZB),RR=RELO09 RE-INITIALIZE PZBLOCKD                     
*                                                                               
*        RETRIEVE RECORDS FROM SORT                                             
*                                                                               
LRRSRTLP DS    0H                                                               
*                                                                               
         GOTO1 SORTER,DMCB,=C'GET'                                              
*                                                                               
         L     RF,4(,R1)                                                        
         LTR   RF,RF               END OF SORTED RECS                           
         BZ    LRRSRTDN              YES                                        
*                                                                               
         MVC   ELEM(L'SORTENT),0(RF)                                            
         LA    R5,ELEM                                                          
         USING SORTRECD,R5                                                      
*                                                                               
         MVC   EZWKRIND,SORTINDX                                                
*                                                                               
         L     R1,AIO1                                                          
         XC    0(256,R1),0(R1)                                                  
*                                                                               
         L     R1,WRKFBUFA                                                      
         XC    0(256,R1),0(R1)                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'INDEX',EPICWK,EZWKRIND,AIO1,WRKFBUFA             
*                                                                               
         CLI   DMCB+8,0            ALL OK                                       
         BE    *+6                  YES                                         
         DC    H'0'                                                             
*                                                                               
         CLC   EZWKRIND,SORTINDX                                                
         BE    *+6                 NEXT INDEX                                   
         DC    H'0'                                                             
*                                                                               
*        READ FIRST WORKER RECORD                                               
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'READ',EPICWK,EZWKRIND,AIO1,WRKFBUFA              
         TM    DMCB+8,X'80'        IF EOF ON FIRST READ                         
         BNZ   LRRSRTCN            SKIP- GET NEXT INDEX                         
*                                                                               
         L     R5,WRKFBUFA                                                      
         USING W_RECD,R5                                                        
*                                                                               
*        APPLY SECONDARY FILTERS THAT DEPEND ON DATA IN RECORD                  
*                                                                               
*        SEE IF IN DATE RANGE                                                   
*                                                                               
         OC    SVDTES,SVDTES       WERE DATES ENTERED                           
         BZ    LRSDTSX                                                          
*                                                                               
         CLC   W_DATEL,SVDTESTR                                                 
         BL    LRRSRTCN                                                         
*                                                                               
         CLC   W_DATEL,SVDTEEND                                                 
         BH    LRRSRTCN                                                         
*                                                                               
LRSDTSX  DS    0H                                                               
*                                                                               
*        APPLY FILTERS FROM OPTIONS                                             
*                                                                               
         OC    SVBSEQ,SVBSEQ         SEQ                                        
         BZ    *+14                                                             
         CLC   W_FILENO,SVBSEQ                                                  
         BL    LRRSRTCN                                                         
*                                                                               
         OC    OPTSRCE,OPTSRCE       SOURCE                                     
         BZ    LRSSRCX                                                          
*                                                                               
         LA    RE,W_DESC                                                        
         CLC   OPTSRCE,EZWCSRCE-EZWKRCMD(RE)                                    
         BNE   LRRSRTCN                                                         
         DROP  R5                                                               
*                                                                               
LRSSRCX  DS    0H                                                               
*                                                                               
         L     R3,EZWKRBUF         SAVE 64 BYTE DESCRIPTOR REC                  
         MVC   SVWFREC0,0(R3)                                                   
*                                                                               
         USING W_RECD,R3                                                        
*                                                                               
         MVC   EZPYFRST(EZPYDLEN),SPACES  INIT PAYEE DATA                       
*                                                                               
         GOTO1 VPZMOD,DMCB,EZBLOCKD  CALL PZMOD TO PROCESS INVOICE              
*                                                                               
LRRSRTCN DS    0H                                                               
*                                                                               
         B     LRRSRTLP            NEXT INDEX                                   
*                                                                               
LRRSRTDN DS    0H                                                               
*                                                                               
LRRSORTX DS    0H                                                               
*                                                                               
LRRX     DS    0H                                                               
         XIT1                                                                   
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,17,A),FORMAT=BI,WORK=1 '                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=33 '                                   
*                                                                               
         LTORG                                                                  
*                                                                               
PLINSMAX EQU   20                  MAX DETAIL LINES FOR PRINTING                
PLINSWID EQU   198                 LENGTH OF PRINT LINE                         
*                                                                               
         DROP  R3,R4                                                            
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - VOPT'                         
***********************************************************************         
*                                                                     *         
*        VALIDATE OPTIONS                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
VOPT     NMOD1 0,**#VOPT                                                        
         L     RC,VGEND            RE-ESTABLISH GEND                            
*                                                                               
         CLI   8(R2),C'?'          HELP OPTION                                  
         BE    VKHLP                                                            
         MVI   BYTE,1                                                           
         LA    R4,BLOCK                                                         
         XC    BLOCK(256),BLOCK                                                 
         GOTO1 SCANNER,DMCB,(R2),(2,(R4)),0                                     
         CLI   4(R1),0                                                          
         BE    MYERR                                                            
*                                                                               
VOPT100  CLI   0(R4),0             END                                          
         BE    VOPTX                                                            
         ZIC   R1,0(R4)                                                         
         BCTR  R1,0                                                             
*                                                                               
* GET ADDRESS OF OPTION VALIDATION RTN                                          
*                                                                               
         LA    RF,OPTABLE                                                       
         EX    R1,VOPTCLC                                                       
         BE    VOPTGO                                                           
         LA    RF,L'OPTABLE(,RF)                                                
         CLI   0(RF),X'FF'                                                      
         BNE   *-16                                                             
         B     VOPTERR                                                          
*                                                                               
VOPTGO   L     RE,8(,RF)                                                        
         A     RE,RELO09                                                        
         BR    RE                                                               
VOPTCLC  CLC   12(0,R4),0(RF)                                                   
         EJECT                                                                  
*                                  TRACE OPTION                                 
*                                                                               
VOPTTR   CLI   OFFLINE,C'Y'        IF OFFLINE, OK                               
         BE    *+12                                                             
         CLI   1(RA),C'*'          THIS A DDS TERMINAL                          
         BNE   VOPTERR                                                          
         MVC   TRCOPT,22(R4)                                                    
         B     VOPT200                                                          
*                                                                               
*                                  SORT OPTION                                  
*                                                                               
VOPTSRT  TM    WHEN,X'38'          ONLY SORT SOON, OV, DDS                      
         BZ    SORTERR                                                          
         MVI   SRTOPT,C'Y'                                                      
         B     VOPT200                                                          
*                                                                               
*                                  DONE - CONVERTED + DELETED                   
*                                                                               
VOPTDN   TM    OPTNSW,OPTCONV+OPTDEL+OPTUNCV                                    
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTDONE                                                   
         B     VOPT200                                                          
*                                                                               
*                                  CONVERTED BATCHES                            
*                                                                               
VOPTCN   TM    OPTNSW,OPTDONE+OPTDEL+OPTUNCV                                    
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTCONV                                                   
         B     VOPT200                                                          
*                                                                               
*                                  DELETED BATCHES                              
*                                                                               
VOPTDL   TM    OPTNSW,OPTDONE+OPTCONV+OPTUNCV                                   
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTDEL                                                    
         B     VOPT200                                                          
*                                                                               
*                                  MOS/MONTH OF SERVICE                         
*                                                                               
VOPTMOS  DS   0H                                                                
         LA    R5,22(,R4)                                                       
         GOTO1 DATVAL,DMCB,(0,(R5)),WORK                                        
         ICM   RE,15,DMCB          WAS MO/DA/YR DATE VALID                      
         BNZ   MOSERR               YES, ERROR                                  
*                                                                               
         GOTO1 (RF),(R1),(2,(R5)),WORK                                          
         ICM   RE,15,DMCB          WAS MO/YR DATE VALID                         
         BZ    MOSERR               NO, ERROR                                   
         GOTO1 DATCON,(R1),(0,WORK),(6,OPTMOS)                                  
         B     VOPT200                                                          
*                                                                               
*                                  UNCONVERTED BATCHES                          
*                                                                               
VOPTUC   TM    OPTNSW,OPTDEL+OPTCONV+OPTUNCV                                    
         BNZ   TOOMANER                                                         
         OI    OPTNSW,OPTUNCV                                                   
         B     VOPT200                                                          
*                                                                               
*                                  SHOW ALL COST/RATES AS ZERO                  
*                                                                               
VOPTCS   OI    OPTNSW,OPTNOCS                                                   
         B     VOPT200                                                          
*                                                                               
*                                  SHOW ALL SOURCE XXXX                         
*                                                                               
VOPTSRCE MVC   OPTSRCE,22(R4)                                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(3,OPTODAYL)                              
         B     VOPT200                                                          
*                                                                               
*                                  LOADED TODAY                                 
*                                                                               
VOPTTDY  OI    OPTNSW,OPTTDYL                                                   
         GOTO1 DATCON,DMCB,(3,BTODAY),(3,OPTODAYL)                              
         B     VOPT200                                                          
*                                                                               
*                                  CONVERTED TODAY                              
*                                                                               
VOPTTDC  OI    OPTNSW,OPTTDYC                                                   
*                                                                               
         GOTO1 DATCON,DMCB,(3,BTODAY),(1,OPTODAYC)                              
*                                                                               
* UP ERROR FIELD & NEXT BLOCK                                                   
*                                                                               
VOPT200  ZIC   RE,BYTE                                                          
         LA    RE,1(RE)                                                         
         STC   RE,BYTE                                                          
         LA    R4,32(,R4)                                                       
         B     VOPT100                                                          
VOPTX    DS    0H                                                               
         XIT1                                                                   
VOPTERR  DS    0H                                                               
         GOTO1 ERREX                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
         DS    0D                                                               
OPTABLE  DS    CL12                                                             
         DC    CL8'HELP    ',A(VKHLP)                                           
         DC    CL8'CONV    ',A(VOPTCN)                                          
         DC    CL8'DELETED ',A(VOPTDL)                                          
         DC    CL8'DONE    ',A(VOPTDN)   CONVERTED/DELETED                      
         DC    CL8'MONTH   ',A(VOPTMOS)                                         
         DC    CL8'MOS     ',A(VOPTMOS)                                         
         DC    CL8'NOCOST  ',A(VOPTCS)                                          
         DC    CL8'SORT    ',A(VOPTSRT)                                         
         DC    CL8'SOURCE  ',A(VOPTSRCE)                                        
         DC    CL8'TODAY   ',A(VOPTTDY)                                         
         DC    CL8'TODAYC  ',A(VOPTTDC)                                         
         DC    CL8'TRACE   ',A(VOPTTR)                                          
         DC    CL8'UNCONV  ',A(VOPTUC)                                          
         DC    X'FF'                                                            
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - PZMPROC'                      
***********************************************************************         
*                                                                     *         
*        PZMOD PROCESSING ROUTINE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*                                                                               
* ORDER OF RECORDS WILL BE INVOICE             (31)                             
*                          INVOICE TOP COMMENT (32)                             
*                          SCHEDULE LINE       (41)                             
*                          BROADCAST DETAIL    (51)                             
*                          INVOICE TOP COMMENT (32)                             
*                          INVOICE BOTTOM CMT  (33)                             
*                          INVOICE TOTAL       (34)                             
*                                                                               
         DS    0H                                                               
PZMPROC  NMOD1 0,**#PZM                                                         
*                                                                               
         L     RC,VGEND            RE-ESTABLISH GEND                            
*                                                                               
         CLI   SRTOPT,C'Y'         THIS INPUT PASS FOR SORT                     
         BE    PZMPROCX             YES, BYPASS ALL                             
*                                                                               
         CLI   EZMODE,EZSPTP       (29) PROCESS INSERTION                       
         BE    SPTPRTN                                                          
         CLI   EZMODE,EZAAP        (32) PROCESS ADDITIONAL ADJUSTMENT           
         BE    AAPRTN                                                           
         CLI   EZMODE,EZRECP       (30) PROCESS RECONCILIATION REMARK           
         BE    RECPRTN                                                          
         CLI   EZMODE,EZSPTL       (37) PROCESS LAST FOR INSERTION              
         BE    SPTLRTN                                                          
         CLI   EZMODE,EZINVP       (25) PROCESS INVOICE                         
         BE    INVPRTN                                                          
         CLI   EZMODE,EZICCOMP     (26) PROCESS INVOICE COMMENT-TOP             
         BE    INVCRTN                                                          
         CLI   EZMODE,EZIBCOMP     (33) PROCESS INVOICE COMMENT-BOTTOM          
         NOP   INVBRTN                                                          
         CLI   EZMODE,EZINVL       (34) LAST FOR INVOICE                        
         BE    INVLRTN                                                          
*                                                                               
         B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - SPTPRTN'                      
***********************************************************************         
*                                                                     *         
*        PROCESS INSERTION DETAIL                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPTPRTN  DS    0H                                                               
*                                                                               
         GOTO1 =A(SPTPRTNR),RR=RELO09                                           
*                                                                               
         B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - AAPRTN'                       
***********************************************************************         
*                                                                     *         
*        PROCESS ADDITIOANL ADJUSTMENT TO INSERTION                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
AAPRTN   DS    0H                                                               
*                                                                               
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
         L     R4,ASPACES          POINT TO BLANK LINE                          
*                                                                               
         ICM   R3,15,ABOX          ESTABLISH BOXES                              
         BZ    AAPBOXX                                                          
         USING BOXD,R3                                                          
*                                                                               
         CLI   BOXSTAT,C'I'        SKIP IF INSIDE A BOX                         
         BE    AAPROWX                                                          
*                                                                               
         MVI   BOXREQ,C'O'            FORCE BOXES TO BE OPENED                  
*                                                                               
         MVC   0(PLINSWID,R5),0(R4) CLEAR LINE                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)        PRINT BLANK LINE FOR TOP OF BOXES         
*                                                                               
AAPROWX  DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
AAPBOXX  DS    0H                                                               
*                                                                               
         LA    R5,DTLLINES         ESTABLISH PRINT LINE                         
         USING PLINED,R5                                                        
*                                                                               
         L     RE,ASPACES          POINT TO FIELD OF SPACES                     
         LA    R0,PLINSMAX         MAX NUMBER OF DETAIL LINES                   
*                                                                               
         CLC   0(PLINSWID,R5),0(RE)  FIND FIRST UN-USED LINE                    
         BNH   *+12                                                             
         LA    R5,PLINSWID(R5)     NEXT PRINT LINE                              
         BCT   R0,*-14                                                          
*                                                                               
         USING PLINED,R5           ESTABLISH DETAIL LINE FOR REFERENCE          
*                                                                               
         MVC   PAAPDESC,EZAADESC    PRINT ADJUSTMENT DESCRIPTION                
*                                                                               
         ICM   R2,15,EZAABGRS                                                   
         BZ    AAPGRSX                                                          
*                                                                               
         EDIT  (R2),(10,PGRS),2                                                 
*                                                                               
AAPGRSX  DS    0H                                                               
*                                                                               
         ICM   R2,15,EZAABNET                                                   
         BZ    AAPNETX                                                          
*                                                                               
         EDIT  (R2),(10,PNET),2                                                 
*                                                                               
AAPNETX  DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT ADJUSTMENT                             
*                                                                               
AAPRTNX  DS    0H                                                               
         B     PZMPROCX                                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - SPTLRTN'                      
***********************************************************************         
*                                                                     *         
*        LAST FOR A SPOT                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPTLRTN  DS    0H                                                               
*                                                                               
*        CHOP RECONCILIATION REMARKS INTO DETAILS                               
*                                                                               
         LA    R1,NXTRMK           POINT TO FIRST REMARK                        
         L     RF,NXTRMKA          POINT TO NEXT AVAILABLE SAVEAREA             
         SR    RF,R1               LENGTH OF REMARKS                            
         BNP   SPTLRMKX            IGNORE IF NONE PRESENT                       
*                                                                               
         LA    R5,DTLLINES         START OF DETAIL LINES                        
         USING PLINED,R5           ESTABLISH DETAIL LINES                       
*                                                                               
         GOTO1 CHOPPER,DMCB,((RF),NXTRMK),(L'PRMKS,PRMKS),             X        
               ('PLINSWID',PLINSMAX),0,0,0                                      
*                                                                               
         LA    RE,NXTRMK           SET A(NEXT RECON REMARK SAVE)                
         ST    RE,NXTRMKA                                                       
         MVC   NXTRMK(L'EZRRREM),SPACES  INIT START OF SAVEAREA                 
*                                                                               
SPTLRMKX DS    0H                                                               
*                                                                               
         LA    R0,PLINSMAX         MAX NUMBER OF DETAIL LINES                   
         LA    R5,DTLLINES         START OF DETAIL LINES                        
*                                                                               
SPTLLP   DS    0H                                                               
*                                                                               
         LA    R4,4                MAX NUMBER OF SPOOL DETAIL LINES             
         L     RE,AP1              START OF SPOOL PRINT LINES                   
         L     RF,ASPACES          A WHOLE LOT OF SPACES                        
*                                                                               
SPTLLP1  DS    0H                                                               
*                                                                               
         OC    0(PLINSWID,R5),0(RF) FORCE TO UPPERCASE                          
*                                                                               
         CLC   0(PLINSWID,R5),0(RF) DONE AT FIRST BLANK LINE                    
         BNH   SPTLDN1                                                          
*                                                                               
         MVC   0(PLINSWID,RE),0(R5) MOVE TO SPOOL DETAIL LINE                   
*                                                                               
SPTLCN1  DS    0H                                                               
*                                                                               
         LA    RE,PLINSWID(RE)     NEXT SPOOL DETAIL LINE                       
         LA    R5,PLINSWID(R5)     NEXT DETAIL LINE TO PRINT                    
         BCT   R0,*+8              DECREMENT DETAIL LINES COUNTER               
         B     SPTLDN1             NONE LEFT                                    
         BCT   R4,SPTLLP1                                                       
                                                                                
*                                                                               
SPTLDN1  DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT DETAILS                                
*                                                                               
         LTR   R4,R4               CHECK IF BLANK LINE FOUND                    
         BNZ   SPTLDN                                                           
*                                                                               
SPTLCN   DS    0H                                                               
*                                                                               
         B     SPTLLP              NEXT BATCH OF LINES                          
*                                                                               
SPTLDN   DS    0H                                                               
*                                                                               
SPTLRTNX DS    0H                                                               
         B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - RECPRTN'                      
***********************************************************************         
*                                                                     *         
*        PROCESS A RECONCILIATION REMARK                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RECPRTN  DS    0H                                                               
*                                                                               
         CLC   EZRRREM,SPACES      IGNORE IF ITS SPACES                         
         BNH   RECPRTNX                                                         
*                                                                               
         L     RE,NXTRMKA          POINT TO START OF NEXT REMARK SAVE           
         LA    RF,NXTRMKX          POINT TO END OF REMARKS SAVEAREA             
*                                                                               
         CR    RE,RF               IF SAVEAREA IS FULL                          
         BL    RECP10                                                           
*                                                                               
         SH    RE,=Y(L'EZRRREM)       BACK UP TO LAST REMARK                    
         MVC   0(L'EZRRREM,RE),SPACES                                           
         MVC   0(12,RE),=CL12'* OVERFLOW *'   MARK END                          
*                                                                               
         B     RECPRTNX            DONE                                         
*                                                                               
RECP10   DS    0H                                                               
*                                                                               
         MVC   0(L'EZRRREM,RE),EZRRREM   SAVE REMARK                            
*                                                                               
         LA    RE,L'EZRRREM(RE)    RESET NEXT SAVEAREA ADDRESS                  
         ST    RE,NXTRMKA                                                       
*                                                                               
RECPRTNX DS    0H                                                               
         B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - INVPRTN'                      
***********************************************************************         
*                                                                     *         
*        PROCESS INVOICE HEADER                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVPRTN  DS    0H                                                               
*                                                                               
         LA    RE,NXTRMK           SET A(NEXT RECON REMARK SAVE)                
         ST    RE,NXTRMKA                                                       
         MVC   NXTRMK(L'EZRRREM),SPACES  INIT START OF SAVEAREA                 
*                                                                               
         ICM   R3,15,ABOX          ESTABLISH BOXES                              
         BZ    INVPBOXX                                                         
         USING BOXD,R3                                                          
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
         MVC   BOXROWS(58),=60C'-'                                              
         MVI   BOXYORN,C'Y'                                                     
         MVI   BOXINIT,0                                                        
*                                                                               
         LA    R5,BOXCOLS                                                       
         USING PLINED,R5                                                        
*                                                                               
         MVI   B1,C'L'                                                          
         MVI   B2,C'C'                                                          
         MVI   B3,C'C'                                                          
         MVI   B4,C'C'                                                          
         MVI   B5,C'C'                                                          
         MVI   B6,C'C'                                                          
         CLI   QMED,C'M'                                                        
         BE    *+8                                                              
         MVI   B7,C'C'                                                          
         MVI   B8,C'C'                                                          
         MVI   B8A,C'C'                                                         
         MVI   B9,C'C'                                                          
         MVI   B10,C'C'                                                         
         MVI   B11,C'C'                                                         
         MVI   B12,C'C'                                                         
         MVI   B13,C'C'                                                         
         MVI   B14,C'R'                                                         
*                                                                               
         DROP  R5                                                               
*                                                                               
         MVI   BOXROWS+14,C'T'                                                  
         MVI   BOXROWS+14+3,C'M'                                                
         MVI   BOXROWS+14+44,C'B'                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
INVPBOXX DS    0H                                                               
*                                                                               
         TM    OPTNSW,OPTDEL       REQUESTING DELETED ONLY                      
         BZ    IP120                                                            
         TM    EZIHCVST,EZIHCDEL   THIS A DELETED INVOICE                       
         BO    IP200                YES                                         
*                                                                               
IP100    MVI   EZMODE,EZINVL       SET TO BYPASS INVOICE                        
         B     EPXIT                                                            
*                                                                               
IP120    TM    OPTNSW,OPTDONE      REQUESTING DONE (DEL/CONVERT)                
         BZ    IP130                                                            
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    IP100                                                            
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL                                        
         BZ    IP100                                                            
         B     IP200                                                            
*                                                                               
IP130    TM    OPTNSW,OPTUNCV      ONLY UNCONVERTED                             
         BZ    IP140                                                            
*                                                                               
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    IP200                                                            
         TM    EZIHCVST,EZIHCVQ+EZIHCDEL                                        
         BNZ   IP100                                                            
         B     IP200                                                            
*                                                                               
IP140    TM    OPTNSW,OPTCONV       ONLY CONVERTED                              
         BZ    IP150                                                            
*                                                                               
         TM    EZIHCVST,EZIHCVQ                                                 
         BZ    IP100                                                            
         TM    EZIHCVST,EZIHRCVQ   RECONVERT                                    
         BO    IP100                                                            
*                                                                               
IP150    TM    OPTNSW,OPTTDYL      OPTION LOADED TODAY                          
         BZ    IP160                                                            
         CLC   EZBTBRDT,OPTODAYL   WAS THIS LOADED TODAY                        
         BNE   IP100                                                            
*                                                                               
IP160    TM    OPTNSW,OPTTDYC      OPTION CONVERTED TODAY                       
         BZ    IP170                                                            
         CLC   EZIHCVDT,OPTODAYC   WAS THIS CONVERTED TODAY                     
         BNE   IP100                                                            
*                                                                               
IP170    OC    OPTMOS,OPTMOS       OPTION MONTH OF SERVICE                      
         BZ    IP200                                                            
         CLC   EZIHDMOS,OPTMOS     WAS THIS REQUESTED MOS                       
         BNE   IP100                                                            
IP200    DS    0H                                                               
*                                                                               
         CLI   SRTOPT,C'Y'         SKIP UNLESS BUILDING A SORT RECORD           
         BNE   INVPRTSX                                                         
*                                                                               
*        BUILD SORT RECORD AND PASS TO SORT                                     
*                                                                               
         LA    R5,ELEM                                                          
         XC    ELEM,ELEM                                                        
         USING SORTRECD,R5         ESTABLISH SORT RECORD                        
*                                                                               
         MVC   SORTMONS,EZIHBMOS                                                
         MVC   SORTPUB,EZSNPUB     ****  PRINTPAK ID                            
         MVC   SORTINV,EZIHINV                                                  
         MVC   SORTINDX,EZWKRIND                                                
*                                                                               
         GOTO1 SORTER,DMCB,=C'PUT',(R5)                                         
*                                                                               
         MVI   EZMODE,EZINVL       FORCE END OF INVOICE                         
*                                                                               
         B     PZMPROCX                                                         
*                                                                               
INVPRTSX DS    0H                                                               
*                                  SET VARIOUS SWITCHES                         
         MVI   TCOMSW,C'N'         TOP STND COMS NOT DONE                       
         MVI   IBCSW,C'N'          NO INVOICE BOTTOM COMS YET                   
*                                                                               
         MVI   PAGE+1,1                                                         
         MVI   NEWINV,C'N'                                                      
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
EPXIT    B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - INVCRTN'                      
***********************************************************************         
*                                                                     *         
*        PROCESS INVOICE COMMENT (TOP)                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVCRTN  DS    0H                                                               
*                                                                               
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
         L     R4,ASPACES          POINT TO BLANK LINE                          
*                                                                               
         ICM   R3,15,ABOX          ESTABLISH BOXES                              
         BZ    INVCBOXX            NONE BEING USED                              
         USING BOXD,R3                                                          
*                                                                               
         CLI   TCOMSW,C'Y'         SKIP IF ONE COMMENT ALREADY DONE             
         BE    INVCBOXX                                                         
*                                                                               
         MVI   TCOMSW,C'Y'         INDICATE A TOP COMMENT DONE                  
*                                                                               
         MVI   BOXROWS+14+3,C'B'   CLOSE TITLES BOX                             
*                                                                               
         MVC   0(PLINSWID,R5),0(R4) CLEAR LINE                                  
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT A BLANK LINE AND HEADERS               
*                                                                               
         MVI   BOXROWS+14+3,C'M'   RESET BOXROWS                                
*                                                                               
         DROP  R3                                                               
*                                                                               
INVCBOXX DS    0H                                                               
*                                                                               
         LA    R0,1                MAX ONE COMMENTS                             
         LA    R2,17(R5)           STARTING POINT FOR COMMENTS                  
         LA    R3,EZICFRST         FIRST LINE OF COMMENT                        
*                                                                               
INVCLP   DS    0H                                                               
*                                                                               
         CLC   0(L'EZSTCOM,R3),0(R4)  DONE IF NOT GREATER THAN SPACES           
         BNH   INVCDN                                                           
*                                                                               
         MVC   0(L'EZSTCOM,R2),0(R3)  MOVE COMMENT TO PRINT AREA                
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD      PRINT COMMENT                             
*                                                                               
INVCCN   DS    0H                                                               
*                                                                               
         LA    R3,L'EZSTCOM(R3)    BUMP TO NEXT COMMENT                         
         BCT   R0,INVCLP                                                        
*                                                                               
INVCDN   DS    0H                                                               
*                                                                               
INVCRTNX DS    0H                                                               
         B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - INVBRTN'                      
***********************************************************************         
*                                                                     *         
*        PROCESS INVOICE COMMENT ((BOTTOM)                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVBRTN  DS    0H                                                               
*                                                                               
         CLI   IBCSW,C'Y'          TEST THIS IS FIRST                           
         BE    INVB1STX                                                         
*                                                                               
         MVI   IBCSW,C'Y'          INDICATE FIRST COMMENT PRINTED               
*                                                                               
         ICM   R3,15,ABOX          ESTABLISH BOXES                              
         BZ    INVBBOXX                                                         
         USING BOXD,R3                                                          
*                                                                               
         MVI   BOXREQ,C'C'         CLOSE BOXES                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     YES, SKIP A LINE                             
*                                                                               
         MVI   BOXYORN,C'N'        TURN OFF BOXES                               
*                                                                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
INVBBOXX DS    0H                                                               
*                                                                               
INVB1STX DS    0H                                                               
*                                                                               
         MVI   ALLOWLIN,5          ALL COMMENTS TO PRINT TOGETHER               
*                                                                               
         LA    R0,1                MAX ONE COMMENTS                             
         LA    R2,17(R5)           STARTING POINT FOR COMMENTS                  
         LA    R3,EZIBCOM          FIRST LINE OF COMMENT                        
         L     R4,ASPACES          POINT TO BLANK LINE                          
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
*                                                                               
INVBLP   DS    0H                                                               
*                                                                               
         CLC   0(L'EZSBCOM,R3),0(R4)  DONE IF NOT GREATER THAN SPACES           
         BNH   INVBDN                                                           
*                                                                               
         MVC   0(L'EZSBCOM,R2),0(R3) MOVE COMMENT TO PRINT AREA                 
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT COMMENT                                
*                                                                               
INVBCN   DS    0H                                                               
*                                                                               
         LA    R3,L'EZSBCOM(R3)    BUMP TO NEXT COMMENT                         
         BCT   R0,INVBLP                                                        
*                                                                               
INVBDN   DS    0H                                                               
*                                                                               
INVBRTNX DS    0H                                                               
         B     PZMPROCX                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - INVLRTN'                      
***********************************************************************         
*                                                                     *         
*        PROCESS LAST FOR INVOICE                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
INVLRTN  DS    0H                                                               
*                                                                               
*        SET UP BOXES PARAMETERS *                                              
*                                                                               
         USING BOXD,R3                                                          
         ICM   R3,15,ABOX          IS ABOX ZEROS                                
         BZ    INVLBOXX            YES/ ON-LINE SKIP BOXES                      
*                                                                               
         CLI   BOXYORN,C'Y'        SKIP IF BOXES NOT ON                         
         BNE   INVLBOXX                                                         
*                                                                               
         MVI   BOXREQ,C'C'         CLOSE BOXES                                  
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   BOXYORN,C'N'        TURN OFF BOXES                               
         MVC   BOXCOLS,SPACES                                                   
         MVC   BOXCOLSR,SPACES                                                  
         MVC   BOXROWS,SPACES                                                   
*                                                                               
         DROP  R3                                                               
*                                                                               
INVLBOXX DS    0H                                                               
*                                                                               
         MVI   ALLOWLIN,11         DONT START WITHOUT 11 LINES                  
*                                                                               
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
         USING PLINED,R5           ESTABLISH DETAIL LINE FOR REFERENCE          
*                                                                               
         CLC   EZIHCVAD,SPACES     SKIP IF CLIENT NOT CONVERTED                 
         BH    *+10                                                             
         CLC   EZIHLADC,SPACES     NOR LOOKED UP                                
         BNH   IL280                                                            
*                                                                               
         MVC   0(22,R5),=C'PRINTPAK CLT/PRD CODES'                              
*                                                                               
         LA    R1,24(R5)                                                        
*                                                                               
         MVC   0(L'EZIHCVAD,R1),EZIHCVAD   CONVERTED CLIENT CODE                
*                                                                               
         CLC   EZIHCVAD,SPACES     IF CLIENT NOT CONVERTED                      
         BH    *+10                                                             
         MVC   0(L'EZIHLADC,R1),EZIHLADC  USE LOOKED UP CODE                    
*                                                                               
         LA    R1,2(,R1)                                                        
*                                                                               
         CLI   0(R1),C' '          ADJUST FOR 2 CH CODES                        
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
         CLC   EZIHCVPR,SPACES     SKIP IF CLIENT NOT CONVERTED                 
         BH    *+10                                                             
         CLC   EZIHLPRC,SPACES     NOR LOOKED UP                                
         BNH   IL264                                                            
*                                                                               
         MVI   0(R1),C'/'          SET SEPARATOR                                
*                                                                               
         MVC   1(L'EZIHCVPR,R1),EZIHCVPR   CONVERTED PRODUCT CODE               
*                                                                               
         CLC   EZIHCVPR,SPACES     IF PRODUCT NOT CONVERTED                     
         BH    *+10                                                             
         MVC   1(L'EZIHLPRC,R1),EZIHLPRC   USE LOOKED UP CODE                   
*                                                                               
         LA    R1,3(,R1)                                                        
*                                                                               
         CLI   0(R1),C' '          ADJUST FOR 2 CH CODES                        
         BE    *+8                                                              
         LA    R1,1(R1)                                                         
*                                                                               
IL264    OC    EZIHCVES,EZIHCVES   CONVERTED ESTIMATE                           
         BZ    IL266                                                            
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,EZIHCVES                                                    
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         MVI   0(R1),C'/'           PRODUCT ESTIMATE SEPARATOR                  
         UNPK  1(3,R1),DUB                                                      
         LA    R1,4(,R1)                                                        
*                                                                               
IL266    MVC   3(8,R1),=C'OVERRIDE'                                             
*                                                                               
IL280    DS    0H                                                               
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    ILLIN1X              YES                                         
*                                                                               
*        PRINT TOTAL ORDERED IF GIVEN                                           
*                                                                               
         ICM   R2,15,EZITBSCH                                                   
         BZ    ILLIN1X                                                          
*                                                                               
         MVC   PGRS-20(20),=CL20'TOTAL ORDERED'                                 
*                                                                               
         EDIT  (R2),(10,PGRS),2                                                 
*                                                                               
ILORDX   DS    0H                                                               
*                                                                               
ILLIN1X  DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    ILLIN2X              YES                                         
*                                                                               
         USING PLINED,R5           ESTABLISH DETAIL LINE FOR REFERENCE          
*                                                                               
         MVC   PGRS-20(20),=CL20'TOTAL GROSS'                                   
*                                                                               
         ICM   R2,15,EZITBACT                                                   
         BZ    ILACTX                                                           
*                                                                               
         EDIT  (R2),(10,PGRS),2                                                 
*                                                                               
ILACTX   DS    0H                                                               
*                                                                               
         MVC   PRMKS(15),=CL15'RECONCILIATION'                                  
*                                                                               
         ICM   R2,15,EZITBRTO                                                   
         BP    IL120                                                            
*                                                                               
         EDIT  (R2),(10,PDBCR),2,FLOAT=-                                        
         B     IL130                                                            
*                                                                               
IL120    EDIT  (R2),(9,PDBCR),2,FLOAT=+                                         
*                                                                               
IL130    DS    0H                                                               
*                                                                               
ILLIN2X  DS    0H                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT LINE                                   
*                                                                               
*        FORMAT NEXT 4 LINES TOGETHER                                           
*                                                                               
         CLI   EZIHACF,C'Y'                                                     
         BNE   *+16                                                             
         MVC   0(21,R5),=C'AGENCY COMMISSION=YES'                               
         MVC   3*PLINSWID(12,R5),=CL12'STATUS'                                  
*                                                                               
         TM    EZIHCVST,EZIHCVQ    CONVERTED                                    
         BNO   IL190                                                            
*                                                                               
         MVC   14+3*PLINSWID(13,R5),=C'* CONVERTED *'                           
*                                                                               
         LA    RF,32+3*PLINSWID(R5)                                             
         GOTO1 DATCON,DMCB,(1,EZIHCVDT),(5,(RF))                                
         B     *+10                                                             
IL190    MVC   14+3*PLINSWID(15,R5),=C'* UNCONVERTED *'                         
*                                                                               
         TM    EZIHCVST,EZIHCDEL   DELETED                                      
         BNO   *+10                                                             
         MVC   40+3*PLINSWID(11,R5),=C'* DELETED *'                             
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL182                YES                                         
*                                                                               
         ICM   R2,15,EZITBGST      CANADIAN GST TAX                             
         BZ    IL180                                                            
*                                                                               
         MVC   60+PLINSWID(20,R5),=C'CANADIAN GST TAX'                          
         LA    RF,82+PLINSWID(R5)                                               
         EDIT  (R2),(11,(RF)),2,FLOAT=-                                         
*                                                                               
IL180    DS    0H                                                               
*                                                                               
         ICM   R2,15,EZITBQST      QUEBEC SALES TAX                             
         BZ    IL181                                                            
*                                                                               
         MVC   60+2*PLINSWID(20,R5),=C'QUEBEC   QST TAX'                        
         LA    RF,82+2*PLINSWID(R5)                                             
         EDIT  (R2),(11,(RF)),2,FLOAT=-                                         
*                                                                               
IL181    DS    0H                                                               
*                                                                               
         MVC   PGRS-20(20),=CL20'AGENCY COMMISSION'                             
         ICM   R2,15,EZITBAGC                                                   
         EDIT  (R2),(10,PGRS),2,FLOAT=-                                         
*                                                                               
         MVC   PGRS-20+PLINSWID(20),=CL20'STATE TAX'                            
         ICM   R2,15,EZITBSTX                                                   
         LA    RF,PGRS+PLINSWID                                                 
         EDIT  (R2),(10,(RF)),2,FLOAT=-                                         
*                                                                               
         MVC   PGRS-20+2*PLINSWID(20),=CL20'LOCAL TAX'                          
         ICM   R2,15,EZITBLTX                                                   
         LA    RF,PGRS+2*PLINSWID                                               
         EDIT  (R2),(10,(RF)),2,FLOAT=-                                         
*                                                                               
         MVC   PGRS-20+3*PLINSWID(20),=CL20'NET DUE'                            
         ICM   R2,15,EZITBDUE                                                   
         LA    RF,PGRS+3*PLINSWID                                               
         EDIT  (R2),(10,(RF)),2,FLOAT=-                                         
         B     IL184                                                            
*                                                                               
IL182    MVI   P3,0                FORCE P4 TO PRINT                            
*                                                                               
IL184    DS    0H                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         OC    EZITBPNT,EZITBPNT                                                
         BNZ   IL204                                                            
         OC    EZITBPGR,EZITBPGR                                                
         BZ    IL206                                                            
*                                                                               
IL204    DS    0H                                                               
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL206                YES                                         
*                                                                               
         MVC   PGRS-20(20),=CL20'PRIOR GROSS BALANCE'                           
         ICM   R2,15,EZITBPGR                                                   
         LA    RF,PGRS                                                          
         EDIT  (R2),(10,(RF)),2,FLOAT=-                                         
*                                                                               
         MVC   PGRS-20+PLINSWID(20),=CL20'PRIOR NET BALANCE'                    
         ICM   R2,15,EZITBPNT                                                   
         LA    RF,PGRS+PLINSWID                                                 
         EDIT  (R2),(10,(RF)),2,FLOAT=-                                         
*                                                                               
IL206    DS    0H                                                               
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    IL208                YES                                         
*                                                                               
         ICM   R2,15,EZIHBSTP                                                   
         BZ    IL208                                                            
*                                                                               
         MVC   0(20,R5),=CL20'SALES TAX PERCENT'                                
         EDIT  (R2),(10,22(R5)),3,ALIGN=LEFT,ZERO=BLANK,DROP=3                  
*                                                                               
IL208    DS    0H                  PRINT LINE                                   
*                                                                               
         GOTO1 SPOOL,DMCB,SPOOLD                                                
*                                                                               
         MVC   0(10,R5),=C'BATCH DATE'                                          
         GOTO1 DATCON,DMCB,(3,EZBTBRDT),(5,12(R5))                              
*                                                                               
         MVC   34(8,R5),=C'SOURCE ='                                            
*                                                                               
         MVC   43(4,R5),EZSRCE                                                  
*                                                                               
         OC    EZSNSYS,SPACES                                                   
         MVC   50(L'EZSNSYS,R5),EZSNSYS                                         
*                                                                               
IL300    MVC   66(20,R5),=C'NUMBER OF INSERTIONS'                               
         ICM   R2,15,EZIHTSPN                                                   
         LA    RF,88(R5)                                                        
         EDIT  (R2),(5,(RF)),ALIGN=LEFT                                         
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   NEWINV,C'Y'         NEW INVOICE NEXT                             
*                                                                               
ILXIT    DS    0H                                                               
*                                                                               
         USING BOXD,R3                                                          
         ICM   R3,15,ABOX          IS ABOX ZEROS                                
         BZ    *+8                 YES/ ON-LINE SKIP BOXES                      
         MVI   BOXYORN,C'Y'        TURN ON BOXES                                
*                                                                               
         B     PZMPROCX                                                         
*                                                                               
         EJECT                                                                  
*                                                                               
PZMPROCX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - SPTPRTNR'                     
***********************************************************************         
*                                                                     *         
*        PROCESS INSERTION DETAIL                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SPTPRTNR NMOD1 0,**#SPTP                                                        
*                                                                               
         L     RC,VGEND            RE-ESTABLISH WORKING STORAGE                 
*                                                                               
         MVI   TCOMSW,0            INIT TOP COMMENT SWITCH                      
         MVI   IBCSW,0             INIT BOTTOM COMMENT SWITCH                   
*                                                                               
         ICM   R3,15,ABOX          ESTABLISH BOXES                              
         BZ    SPTPBOXX                                                         
         USING BOXD,R3                                                          
*                                                                               
         MVI   BOXYORN,C'Y'        MAKE SURE BOXES ARE ON                       
*                                                                               
         CLI   BOXSTAT,C'I'        SKIP IF INSIDE BOX                           
         BE    SPTPROWX                                                         
*                                                                               
         MVI   BOXREQ,C'O'         FORCE BOXES TO BE OPENED                     
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)     PRINT BLANK LINE FOR TOP OF BOXES            
*                                                                               
SPTPROWX DS    0H                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
SPTPBOXX DS    0H                                                               
*                                                                               
         LA    R5,DTLLINES         ESTABLISH PRINT LINE                         
         USING PLINED,R5                                                        
*                                                                               
         LA    R0,PLINSMAX         MAX NUMBER OF DETAIL LINES                   
         L     RE,ASPACES          POINT TO FIELD OF SPACES                     
         LR    RF,R5               START OF DETAIL LINES                        
*                                                                               
         MVC   0(PLINSWID,RF),0(RE)  INIT DETAIL PRINT LINES                    
         LA    RF,PLINSWID(RF)     NEXT PRINT LINE                              
         BCT   R0,*-10                                                          
*                                                                               
         MVC   PDATE,EZSPDDT       INSERTION DATE                               
*                                                                               
*        PRINT EDITION NAME, CHOPPING IF NEEDED                                 
*                                                                               
SPTEDN   DS    0H                                                               
*                                                                               
         LA    R2,PEDN             POINT TO START OF EDITION                    
         LA    R0,PLINSMAX         MAX DETAIL LINES FOR PRINTING                
*                                                                               
         CLC   EZSPEDNM,SPACES     SKIP IF NONE AVAILABLE                       
         BNH   SPTEDNX                                                          
*                                                                               
         LA    RF,PLINSWID         LINE LENGTH                                  
*                                                                               
         GOTO1 CHOPPER,DMCB,(L'EZSPEDNM,EZSPEDNM),(L'PEDN,0(R2)),      X        
               ((RF),(R0)),0,0,0                                                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,8(R1)            NUMBER OF LINES USED                         
         SR    R0,RF               NUMBER OF LINES LEFT                         
         BNP   SPTEDNX             NO ROOM TO PRINT ANYMORE                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(PLINSWID)                                                  
         LA    R2,0(RF,R2)         NEXT AVAILABLE PRINT POSITION                
*                                                                               
SPTEDNX  DS    0H                                                               
*                                                                               
*        PRINT IO#, PRODUCT, ESTIMATE IN SAME FIELD                             
*                                                                               
SPTCPE   DS    0H                                                               
*                                                                               
         LA    R2,PIO#             POINT TO START OF IO#                        
         LA    R0,PLINSMAX         MAX DETAIL LINES FOR PRINTING                
*                                                                               
*        PRINT INSERTION ORDER NUMBER ON FIRST LINE IF PRESENT                  
*                                                                               
SPTIO#   DS    0H                                                               
*                                                                               
         CLC   EZSPIORD,SPACES     SKIP IF NO IO# AVAILABLE                     
         BNH   SPTIO#X                                                          
*                                                                               
         MVC   0(L'EZSPIORD,R2),EZSPIORD  INSERTION ORDER NUMBER                
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DECREMENT AVAILABLE LINES COUNTER            
         BNP   SPTCPEX             NO MORE ROOM                                 
*                                                                               
SPTIO#X  DS    0H                                                               
*                                                                               
*        PRINT PRODUCT NAME - MAY HAVE TO CHOP IT DOWN                          
*                                                                               
SPTPRD   DS    0H                                                               
*                                                                               
         CLC   EZSPPRNM,SPACES     SKIP IF NO PRODUCT NAME AVAILABLE            
         BNH   SPTPRDX                                                          
*                                                                               
         MVC   0(4,R2),=C'PRD='    SET IDENTIFIER                               
*                                                                               
         LA    RF,PLINSWID         LINE LENGTH                                  
*                                                                               
         GOTO1 CHOPPER,DMCB,(L'EZSPPRNM,EZSPPRNM),(L'PIO#-4,4(R2)),    X        
               ((RF),(R0)),0,0,0                                                
*                                                                               
         SR    RF,RF                                                            
         IC    RF,8(R1)            NUMBER OF LINES USED                         
         SR    R0,RF               NUMBER OF LINES LEFT                         
         BNP   SPTCPEX             NO ROOM TO PRINT ANYMORE                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(PLINSWID)                                                  
         LA    R2,0(RF,R2)         NEXT AVAILABLE PRINT POSITION                
*                                                                               
SPTPRDX  DS    0H                                                               
*                                                                               
*        PRINT ESTIMATE DESCRIPTION                                             
*                                                                               
SPTEM#   DS    0H                                                               
*                                                                               
         CLC   EZSPEST,SPACES      SKIP IF NO ESTIMATE AVAILABLE                
         BNH   SPTENMX                                                          
*                                                                               
         MVC   0(4,R2),=C'EST='    SET IDENTIFIER                               
*                                                                               
         MVC   4(L'EZSPEST,R2),EZSPEST  ESTIMATE NUMBER/DESCRIPTION             
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DECREMENT AVAILABLE LINES COUNTER            
         BNP   SPTCPEX             NO MORE ROOM                                 
*                                                                               
SPTENMX  DS    0H                                                               
*                                                                               
*        ADD DDS PRODUCT AND ESTIMATE CODES                                     
*                                                                               
SPTDDS   DS    0H                                                               
*                                                                               
         LA    R4,4(R2)            START OF PRINT AREA                          
*                                                                               
         CLC   EZSPCVPR,SPACES     IF CONVERTED PRODUCT AVAILABLE               
         BNH   *+14                                                             
         MVC   0(L'EZSPCVPR,R4),EZSPCVPR PRINT IT                               
         B     SPTDDS1                                                          
*                                                                               
         CLC   EZSPLPRC,SPACES     IF LOOKED UP PRODUCT AVAILABLE               
         BNH   SPTDDSX                ELSE SKIP                                 
*                                                                               
         MVC   0(L'EZSPLPRC,R4),EZSPLPRC PRINT IT                               
*                                                                               
SPTDDS1  DS    0H                                                               
*                                                                               
         MVC   0(4,R2),=C'DDS='    SET LINE IDENTIFIER                          
*                                                                               
         LA    R4,2(R4)            BUMP POINTER                                 
*                                                                               
         CLI   0(R4),C' '          SKIP IF CODE IS ONLY 2 LONG                  
         BNH   *+8                                                              
         LA    R4,1(R4)            BUMP POINTER TO NEXT AVAILABLE AREA          
*                                                                               
         OC    EZSPCVES,EZSPCVES   SKIP IF NO ESTIMATE AVAILABLE                
         BZ    SPTDDS2                                                          
*                                                                               
         MVI   0(R4),C'/'          SET SEPARATOR                                
         EDIT  (B2,EZSPCVES),(3,1(R4)),0,ALIGN=LEFT  PRINT EST NO.              
*                                                                               
SPTDDS2  DS    0H                                                               
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DECREMENT AVAILABLE LINES COUNTER            
         BNP   SPTCPEX             NO MORE ROOM                                 
*                                                                               
SPTDDSX  DS    0H                                                               
*                                                                               
SPTCPEX  DS    0H                                                               
*                                                                               
*        PRINT CAPTION, AD CODE AND COPY CODE IN SAME FIELD                     
*                                                                               
SPTCAC   DS    0H                                                               
*                                                                               
*        PRINT CAPTION - MAY HAVE TO CHOP IT DOWN                               
*        MAX 4 LINES FOR PRINTING                                               
*                                                                               
         LA    R2,PCAP             POINT TO START OF CAPTION                    
         LA    R0,PLINSMAX         MAX DETAIL LINES FOR PRINTING                
*                                                                               
         CLC   EZSPCAPT,SPACES     SKIP IF NO CAPTION AVAILABLE                 
         BNH   SPTCAPX                                                          
*                                                                               
         LA    RF,PLINSWID         LINE LENGTH                                  
*                                                                               
         GOTO1 CHOPPER,DMCB,(L'EZSPCAPT,EZSPCAPT),(L'PCAP,(R2)),       X        
               ((RF),4),0,0,0                                                   
*                                                                               
         SR    RF,RF                                                            
         IC    RF,8(R1)            NUMBER OF LINES USED                         
         SR    R0,RF               NUMBER OF LINES LEFT                         
         BNP   SPTCACX             NO ROOM TO PRINT ANYMORE                     
*                                                                               
         BCTR  RF,0                DECREMENT FOR INDEXING                       
         MH    RF,=Y(PLINSWID)                                                  
         LA    R2,0(RF,R2)         NEXT AVAILABLE PRINT POSITION                
*                                                                               
SPTCAPX  DS    0H                                                               
*                                                                               
*        PRINT AD CODE IF PRESENT                                               
*                                                                               
SPTADC   DS    0H                                                               
*                                                                               
         CLC   EZSPADCD,SPACES     SKIP IF NOT PRESENT                          
         BNH   SPTADCX                                                          
*                                                                               
         MVC   0(4,R2),=C'ADC='    SET IDENTIFIER                               
         MVC   4(L'EZSPADCD,R2),EZSPADCD   PRINT AD CODE                        
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTCACX             NO MORE ROOM                                 
*                                                                               
SPTADCX  DS    0H                                                               
*                                                                               
*        PRINT COPY CODE IF PRESENT                                             
*                                                                               
SPTCPY   DS    0H                                                               
*                                                                               
         CLC   EZSPCOPY,SPACES     SKIP IF NOT PRESENT                          
         BNH   SPTCPYX                                                          
*                                                                               
         MVC   0(4,R2),=C'CPY='    SET IDENTIFIER                               
         MVC   4(L'EZSPCOPY,R2),EZSPCOPY   PRINT COPY CODE                      
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTCACX             NO MORE ROOM                                 
*                                                                               
SPTCPYX  DS    0H                                                               
*                                                                               
SPTCACX  DS    0H                                                               
*                                                                               
         MVC   PPAGE,EZSPPAGE      SECTION/PAGE                                 
*                                                                               
*        PRINT SPACE DESCRIPTION AND/OR LINES/COLUMNS/INCHES                    
*                                                                               
SPTSPC   DS    0H                                                               
*                                                                               
         LA    R2,PSPACE           POINT TO START OF SPACE                      
         LA    R0,PLINSMAX         MAX DETAIL LINES FOR PRINTING                
*                                                                               
         CLC   EZSPSPCE,SPACES     SKIP IF NO SPACE AVAILABLE                   
         BNH   SPTSPC1                                                          
*                                                                               
         MVC   PSPACE,EZSPSPCE     SPACE                                        
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DECREMEMNT AVAILABLE LINES COUNTER           
         BNP   SPTSPCX             NO MORE ROOM                                 
*                                                                               
SPTSPC1  DS    0H                                                               
*                                                                               
*        PRINT TOTAL NUMBER OF LINES IF PRESENT                                 
*                                                                               
SPTLNS   DS    0H                                                               
*                                                                               
         OC    EZSPBLNS,EZSPBLNS   SKIP IF NO LINES AVAILABLE                   
         BZ    SPTLNSX                                                          
*                                                                               
         MVC   0(6,R2),=C'LINES='  SET LINES INDICATOR                          
*                                                                               
         LR    RF,R0               SAVE LINES COUNTER                           
         EDIT  EZSPBLNS,(11,6(R2)),0,ALIGN=LEFT   PRINT LINES                   
         LR    R0,RF               RESTORE LINES COUNTER                        
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTSPCX             NO MORE ROOM                                 
*                                                                               
SPTLNSX  DS    0H                                                               
*                                                                               
*        PRINT NUMBER OF COLUMNS IF PRESENT                                     
*                                                                               
SPTCLS   DS    0H                                                               
*                                                                               
         OC    EZSPBCLS,EZSPBCLS   SKIP IF NO COLUMNS AVAILABLE                 
         BZ    SPTCLSX                                                          
*                                                                               
         MVC   0(6,R2),=C'COLS ='  SET COLUMNS INDICATOR                        
*                                                                               
         LR    RF,R0               SAVE LINES COUNTER                           
         EDIT  EZSPBCLS,(11,6(R2)),0,ALIGN=LEFT   PRINT COLUMNS                 
         LR    R0,RF               RESTORE LINES COUNTER                        
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTSPCX             NO MORE ROOM                                 
*                                                                               
SPTCLSX  DS    0H                                                               
*                                                                               
*        PRINT NUMBER OF INCHES IF PRESENT                                      
*                                                                               
SPTICS   DS    0H                                                               
*                                                                               
         OC    EZSPBICS,EZSPBICS   SKIP IF NO INCHES AVAILABLE                  
         BZ    SPTICSX                                                          
*                                                                               
         MVC   0(6,R2),=C'INCHS='  SET INCHES INDICATOR                         
*                                                                               
         LR    RF,R0               SAVE LINES COUNTER                           
         EDIT  EZSPBICS,(11,6(R2)),3,ALIGN=LEFT   PRINT COLUMNS                 
         LR    R0,RF               RESTORE LINES COUNTER                        
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTSPCX             NO MORE ROOM                                 
*                                                                               
SPTICSX  DS    0H                                                               
*                                                                               
SPTSPCX  DS    0H                                                               
*                                                                               
*        PRINT PREMIUMS AS PREMIUM/COST                                         
*                                                                               
SPTPRM   DS    0X                                                               
*                                                                               
         LA    R2,PSPACE           POINT TO START OF SPACE                      
         LA    R0,PLINSMAX         MAX DETAIL LINES FOR PRINTING                
*                                                                               
         LA    R3,EZSPPOS1         POSITION PREMIUM 1                           
         LA    R4,EZSPBPM1         COST FOR PREMIUM                             
         LA    RE,3                NUMBER OF PREMIUMS                           
*                                                                               
SPTPRMLP DS    0H                                                               
*                                                                               
         LA    R1,X                START OF OUTPUT WORKAREA                     
         MVC   X,SPACES            START OF OUTPUT WORKAREA                     
*                                                                               
         CLC   0(L'EZSPPOS1,R3),SPACES  SKIP IF NO PREMIUM                      
         BNH   SPTPRMCN                                                         
*                                                                               
         MVC   0(L'EZSPPOS1,R1),0(R3)   PRINT PREMIUM                           
*                                                                               
         LA    R1,L'EZSPPOS1-1(R1) POINT TO LAST BYTE OF PREMIUM                
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-SPACE                          
         BE    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         OC    0(L'EZSPBPM1,R4),0(R4) SKIP IF NO COST FOR POSITION              
         BZ    SPTPRM10                                                         
*                                                                               
         MVI   1(R1),C'/'          SET SEPARATOR                                
*                                                                               
         LR    RF,R0               SAVE COUNTER                                 
         EDIT  (B4,0(R4)),(11,2(R1)),2,ALIGN=LEFT  PRINT COST                   
         LR    R0,RF               RESTORE COUNTER                              
*                                                                               
SPTPRM10 DS    0H                                                               
*                                                                               
         MVC   0(L'PPREM,R2),X     PRINT PREMIUM                                
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTPRMX             NO MORE ROOM                                 
*                                                                               
SPTPRMCN DS    0H                                                               
*                                                                               
         BCT   RE,SPTPRMLP                                                      
*                                                                               
SPTPRMDN DS    0H                                                               
*                                                                               
*        COLOR PREMIUM                                                          
*                                                                               
         LA    R1,X                START OF OUTPUT WORKAREA                     
         MVC   X,SPACES            START OF OUTPUT WORKAREA                     
*                                                                               
         CLC   EZSPCLPR,SPACES     SKIP IF NO COLOR PREMIUM                     
         BNH   SPTPRMCX                                                         
*                                                                               
         MVC   0(L'EZSPCLPR,R1),EZSPCLPR   PRINT PREMIUM                        
*                                                                               
         LA    R1,L'EZSPCLPR-1(R1) POINT TO LAST BYTE OF PREMIUM                
*                                                                               
         CLI   0(R1),C' '          FIND LAST NON-SPACE                          
         BE    *+8                                                              
         BCT   R1,*-8                                                           
*                                                                               
         CLC   EZSPCLAM,SPACES     SKIP IF NO COST FOR COLOR                    
         BZ    SPTPRMC1                                                         
*                                                                               
         MVI   1(R1),C'/'          SET SEPARATOR                                
*                                                                               
         PACK  DUB,EZSPCLAM        PACK AMOUNT                                  
*                                                                               
         LR    RF,R0               SAVE COUNTER                                 
         EDIT  (P8,DUB),(11,2(R1)),2,ALIGN=LEFT  PRINT COST                     
         LR    R0,RF               RESTORE COUNTER                              
*                                                                               
SPTPRMC1 DS    0H                                                               
*                                                                               
         MVC   0(L'PPREM,R2),X     PRINT PREMIUM                                
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTPRMX             NO MORE ROOM                                 
*                                                                               
SPTPRMCX DS    0H                                                               
*                                                                               
SPTPRMX  DS    0H                                                               
*                                                                               
         MVC   PTS(L'EZSPTEAR),EZSPTEAR INDICATE TEARSHEET STATUS               
*                                                                               
         TM    OPTNSW,OPTNOCS      BLANK ALL COST/RATES                         
         BO    SPTPRTNX             YES                                         
*                                                                               
SPTRATE  DS    0H                                                               
*                                                                               
         OC    EZSPBRAT,EZSPBRAT   SKIP IF NO RATE GIVEN                        
         BZ    SPTRATEX                                                         
*                                                                               
         EDIT  EZSPBRAT,(11,PRATE),5,DROP=3   UNIT RATE                         
*                                                                               
SPTRATEX DS    0H                                                               
*                                                                               
SPTGRS   DS    0H                                                               
*                                                                               
         OC    EZSPBGRS,EZSPBGRS   SKIP IF NO GROSS GIVEN                       
         BZ    SPTGRSX                                                          
*                                                                               
         EDIT  EZSPBGRS,(10,PGRS),2    GROSS                                    
*                                                                               
SPTGRSX  DS    0H                                                               
*                                                                               
SPTNET   DS    0H                                                               
*                                                                               
         OC    EZSPBNET,EZSPBNET   SKIP IF NO NET GIVEN                         
         BZ    SPTNETX                                                          
*                                                                               
         EDIT  EZSPBNET,(10,PNET),2    NET                                      
*                                                                               
SPTNETX  DS    0H                                                               
*                                                                               
SPTPRTEX DS    0H                                                               
*                                                                               
*        PRINT DEBITS/CREDITS                                                   
*                                                                               
SPTDBCR  DS    0H                                                               
*                                                                               
         ICM   RE,15,EZSPBDR       DEBT CREDIT                                  
         ICM   RF,15,EZSPBCR                                                    
         AR    RE,RF               COMBINE                                      
         BZ    SPTDBCRX                                                         
         BM    SPTDBCR1                                                         
*                                                                               
         LR    R4,RE                                                            
         EDIT  (R4),(9,X),2,FLOAT=+                                             
*                                                                               
         B     SPTDBCR2                                                         
*                                                                               
SPTDBCR1 DS    0H                                                               
*                                                                               
         LR    R4,RE                                                            
         EDIT  (R4),(9,X),2,FLOAT=-                                             
*                                                                               
SPTDBCR2 DS    0H                                                               
*                                                                               
         LA    R2,PLINSWID(R2)     BUMP TO NEXT LINE                            
         SH    R0,=H'1'            DROP AVAILABLE LINES COUNTER                 
         BNP   SPTPRMX             NO MORE ROOM                                 
*                                                                               
SPTDBCRX DS    0H                                                               
*                                                                               
SPTPRTNX DS   0H                                                                
*                                                                               
         XIT1                                                                   
*                                                                               
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - SUBROUTS'                     
***********************************************************************         
*                                                                     *         
*        COMMON SUROUTINES                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SUBROUTS DS    0D                                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         DC    H'0'                                                             
*                                                                               
SORTERR  XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'SORTERMS),SORTERMS                                     
         B     MYERR                                                            
VKHLP    XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTHLPMS),OPTHLPMS                                     
         B     MYERR                                                            
MOSERR   XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MOSERMS),MOSERMS                                       
         B     MYERR                                                            
TOOMANER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOOMANMS),TOOMANMS                                     
         B     MYERR                                                            
VKERR    OI    BYTE,X'F0'                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'OPTERRMS),OPTERRMS                                     
         MVC   CONHEAD+16(1),BYTE                                               
MYERR    LA    R2,IRPOPTH                                                       
MYERR2   MVI   GENSTAT2,USMYOK                                                  
         MVI   GCMODE,C' '         IF ERROR TURN OFF SLAVE & EXIT               
         GOTO1 ERREX2                                                           
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
OPTERRMS DC    C'* ERROR * FIELD X INVALID OPTION *'                            
SORTERMS DC    C'* ERROR * SORT ONLY ALLOWED SOON, OV, OR DDS *'                
MOSERMS  DC    C'* ERROR * ENTER MOS MON/YR OR MONYR *'                         
OPTHLPMS DC    C'OPTIONS=CONV/DELETE/DONE/MOS/NOCOST/SORT/SOURCE/UNCONVC        
               '                                                                
TOOMANMS DC    C'* ERROR * ENTER ONLY ONE OF DONE, DELETE, CONV, UNCONVC        
                *'                                                              
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - HDHK'                         
***********************************************************************         
*                                                                     *         
*        HDHK - HEADHOOK ROUTINE                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
HDHK     NMOD1 0,**#HDHK                                                        
*                                                                               
         L     RC,VGEND            RE-ESTABLISH GEND                            
*                                                                               
         L     R5,ABOX             ESTABLISH XHEADLINES                         
         L     R5,BOXAWIDE-BOXD(R5)                                             
         USING WIDED,R5                                                         
*                                                                               
         MVC   XHEAD1+00(11),=C'AGENCY    -' AGENCY                             
         MVC   XHEAD1+12(30),EZAGNAM  NAME                                      
         MVC   XHEAD2+12(30),EZAGLIN1 ADDRESS LINE 1                            
         MVC   XHEAD3+12(30),EZAGLIN2 ADDRESS LINE 2                            
         MVC   XHEAD4+12(30),EZAGLIN3 ADDRESS LINE 3                            
         MVC   XHEAD5+12(30),EZAGLIN4 ADDRESS LINE 4                            
         MVC   XHEAD6+00(11),=C'ACCT. NO. -'                                    
         MVC   XHEAD6+12(10),EZIHAACC                                           
         MVC   XHEAD7+00(11),=C'CONTACT   -'                                    
         MVC   XHEAD7+12(25),EZIHACON                                           
*                                                                               
         MVC   XHEAD9+00(11),=C'ADVERTISER-'                                    
         MVC   XHEAD9+12(25),EZIHAVNM                                           
*                                                                               
         LA    R1,XHEAD9+12+L'EZIHAVNM-1   END OF ADVERTISER'S NAME             
         LA    R0,L'EZIHAVNM       LENGTH OF NAME                               
*                                                                               
         CLI   0(R1),C' '          FIND LAST BLANK                              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,1(R1)            NEXT AVAILABLE PRINT POSITION                
*                                                                               
         CLC   EZIHAVCD,SPACES     SKIP IF NO ID FOR ADVERTISER                 
         BNH   HDADV10                                                          
*                                                                               
         MVC   0(4,R1),=C'(ID='    SET INDICATOR                                
         LA    R1,4(R1)            BUMP POINTER                                 
         MVC   0(L'EZIHAVCD,R1),EZIHAVCD   PRINT ID                             
*                                                                               
         LA    R1,L'EZIHAVCD-1(R1) END OF ID                                    
         LA    R0,L'EZIHAVCD       LENGTH OF ID                                 
*                                                                               
         CLI   0(R1),C' '          FIND LAST BLANK                              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         MVI   1(R1),C','          SET END OF ID                                
         LA    R1,1(R1)            BUMP POINTER                                 
*                                                                               
HDADV10  DS    0H                                                               
*                                                                               
         CLC   EZIHCVAD,SPACES     SKIP IF NO CONVERTED CODE                    
         BH    *+10                                                             
         CLC   EZIHLADC,SPACES     OR LOOKED UP CODE                            
         BNH   HDADV20                                                          
*                                                                               
         CLI   0(R1),C' '          IF NO PREVIOUS ID                            
         BH    *+8                                                              
         MVI   0(R1),C'('             OPEN BRACKETS                             
*                                                                               
         LA    R1,1(R1)            BUMP POINTER                                 
*                                                                               
         MVC   0(4,R1),=C'DDS='    SET ID                                       
*                                                                               
         MVC   4(L'EZIHCVAD,R1),EZIHCVAD   PRINT CONVERTED CLIENT CODE          
*                                                                               
         CLC   EZIHCVAD,SPACES     IF NO CONVERTED CODE                         
         BH    *+10                                                             
         MVC   4(L'EZIHLADC,R1),EZIHLADC   PRINT LOOKED UP CLIENT CODE          
*                                                                               
         LA    R1,4+L'EZIHCVAD(R1) NEXT PRINT POSITION                          
*                                                                               
         MVI   0(R1),C')'          CLOSE BRACKETS                               
*                                                                               
HDADV20  DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          CONVERT TRAILING COMMA                       
         BNE   *+8                                                              
         MVI   0(R1),C')'             TO CLOSING BRACKET                        
*                                                                               
HDADVX   DS    0H                                                               
*                                                                               
         MVC   XHEAD10+00(11),=C'ACCT. NO. -'                                   
         MVC   XHEAD10+12(08),EZIHSADC                                          
*                                                                               
         MVC   XHEAD11+00(11),=C'PRODUCT   -'                                   
         MVC   XHEAD11+12(25),EZIHPRNM                                          
*                                                                               
         LA    R1,XHEAD11+L'EZIHPRNM-1   END OF PRODUCT'S NAME                  
         LA    R0,L'EZIHPRNM       LENGTH OF NAME                               
*                                                                               
         CLI   0(R1),C' '          FIND LAST BLANK                              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,1(R1)            NEXT AVAILABLE PRINT POSITION                
*                                                                               
         CLC   EZIHPRCD,SPACES     SKIP IF NO ID FOR PRODUCT                    
         BNH   HDPRD10                                                          
*                                                                               
         MVC   0(4,R1),=C'(ID='    SET INDICATOR                                
         LA    R1,4(R1)            BUMP POINTER                                 
         MVC   0(L'EZIHPRCD,R1),EZIHPRCD   PRINT ID                             
*                                                                               
         LA    R1,L'EZIHPRCD-1(R1) END OF ID                                    
         LA    R0,L'EZIHPRCD       LENGTH OF ID                                 
*                                                                               
         CLI   0(R1),C' '          FIND LAST BLANK                              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         MVI   1(R1),C','          SET END OF ID                                
         LA    R1,1(R1)            BUMP POINTER                                 
*                                                                               
HDPRD10  DS    0H                                                               
*                                                                               
         CLC   EZIHCVPR,SPACES     SKIP IF NO CONVERTED CODE                    
         BH    *+10                                                             
         CLC   EZIHLPRC,SPACES     OR LOOKED UP CODE                            
         BNH   HDPRD20                                                          
*                                                                               
         CLI   0(R1),C' '          IF NO PREVIOUS ID                            
         BH    *+8                                                              
         MVI   0(R1),C'('             OPEN BRACKETS                             
*                                                                               
         LA    R1,1(R1)            BUMP POINTER                                 
*                                                                               
         MVC   0(4,R1),=C'DDS='    SET ID                                       
*                                                                               
         MVC   4(L'EZIHCVPR,R1),EZIHCVPR   PRINT CONVERTED CLIENT CODE          
*                                                                               
         CLC   EZIHCVPR,SPACES     IF NO CONVERTED CODE                         
         BH    *+10                                                             
         MVC   4(L'EZIHLPRC,R1),EZIHLPRC   PRINT LOOKED UP CLIENT CODE          
*                                                                               
         LA    R1,4+L'EZIHCVPR(R1) NEXT PRINT POSITION                          
*                                                                               
         MVI   0(R1),C')'          CLOSE BRACKETS                               
*                                                                               
HDPRD20  DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          CONVERT TRAILING COMMA                       
         BNE   *+8                                                              
         MVI   0(R1),C')'             TO CLOSING BRACKET                        
*                                                                               
HDPRDX   DS    0H                                                               
*                                                                               
         MVC   XHEAD12+00(11),=C'ESTIMATE  -'                                   
         MVC   XHEAD12+12(10),EZIHEST                                           
*                                                                               
         LA    R1,XHEAD9+12+L'EZIHEST-1   END OF ESTIMATE                       
         LA    R0,L'EZIHEST        LENGTH OF NAME                               
*                                                                               
         CLI   0(R1),C' '          FIND LAST BLANK                              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         LA    R1,1(R1)            NEXT AVAILABLE PRINT POSITION                
*                                                                               
         OC    EZIHCVES,EZIHCVES   SKIP IF NO CONVERTED ESTIMATE NUMBER         
         BZ    HDEST10                                                          
*                                                                               
         MVC   0(5,R1),=C'(DDS='   SET INDICATOR                                
         LA    R1,5(R1)            BUMP POINTER                                 
         EDIT  (B2,EZIHCVES),(3,0(R1)),0,ALIGN=LEFT  CONVERTED EST#             
*                                                                               
         LA    R1,2(R1)            END OF ID                                    
         LA    R0,3                LENGTH OF ID                                 
*                                                                               
         CLI   0(R1),C' '          FIND LAST BLANK                              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
*                                                                               
         MVI   1(R1),C')'          SET END OF ID                                
         LA    R1,1(R1)            BUMP POINTER                                 
*                                                                               
HDEST10  DS    0H                                                               
*                                                                               
HDESTX   DS    0H                                                               
*                                                                               
         MVC   XHEAD1+70(15),=C'PUBLICATION   -' PUBLICATION                    
         MVC   XHEAD1+86(30),EZSNNAME NAME                                      
         MVC   XHEAD2+86(30),EZSNLIN1 ADDRESS LINE 1                            
         MVC   XHEAD3+86(30),EZSNLIN2 ADDRESS LINE 2                            
         MVC   XHEAD4+86(30),EZSNLIN3 ADDRESS LINE 3                            
         MVC   XHEAD5+86(30),EZSNLIN4 ADDRESS LINE 4                            
         MVC   XHEAD6+70(15),=C'CONTACT       -'                                
         MVC   XHEAD6+86(25),EZIHSCON                                           
         MVC   XHEAD7+70(15),=C'ORDER TYPE    -'                                
         MVC   XHEAD7+86(15),EZIHORD                                            
         MVC   XHEAD8+70(15),=C'PUB CONTRACT  -'                                
         MVC   XHEAD8+86(10),EZIHSORD                                           
         MVC   XHEAD9+70(15),=C'REPRESENTATIVE-'                                
         MVC   XHEAD9+086(25),EZIHREP                                           
         MVC   XHEAD10+70(15),=C'SALESPERSON   -'                               
         MVC   XHEAD10+086(25),EZIHSLSP                                         
         MVC   XHEAD11+70(15),=C'REP CONTRACT  _'                               
         MVC   XHEAD11+86(10),EZIHRORD                                          
         MVC   XHEAD12+70(15),=C'CONTRACT DATES-'                               
         OC    EZIHDCSD,EZIHDCSD                                                
         BZ    HDHK040                                                          
         MVC   XHEAD12+86(8),EZIHDCSD                                           
         MVI   XHEAD12+94,C'-'                                                  
         MVC   XHEAD12+95(8),EZIHDCED                                           
*                                                                               
HDHK040  DS    0H                                                               
*                                                                               
         MVC   XHEAD13+70(15),=C'RATE CARD     -'                               
         MVC   XHEAD13+86(10),EZIHRC                                            
*                                                                               
         MVC   XHEAD1+120(09),=C'INVOICE -' INVOICE                             
*                                                                               
         MVC   XHEAD1+130(09),=C'NUMBER  -'                                     
         MVC   XHEAD1+140(10),EZIHINV NUMBER                                    
*                                                                               
         MVC   XHEAD2+130(09),=C'DATE    -'                                     
         MVC   XHEAD2+140(8),EZIHDIDT ISSUE DATE                                
*                                                                               
         MVC   XHEAD3+130(09),=C'PERIOD  -' PERIOD                              
         OC    EZIHISDT,EZIHISDT   SKIP IF NO DATES                             
         BZ    HDHK100                                                          
*                                                                               
         MVC   XHEAD3+140(8),EZIHDISD                                           
         MVI   XHEAD3+148,C'-'                                                  
         MVC   XHEAD3+149(8),EZIHDIED                                           
*                                                                               
HDHK100  DS    0H                                                               
*                                                                               
         MVC   XHEAD4+130(09),=C'MONTH   -' MONTH OF SERVICE                    
         MVC   XHEAD4+140(6),EZIHDMOS                                           
*                                                                               
         MVC   XHEAD5+130(09),=C'DUE DATE-' DUE DATE                            
         MVC   XHEAD5+140(08),EZIHDUDT                                          
*                                                                               
         MVC   XHEAD6+130(09),=C'TERMS   -' TERMS                               
         MVC   XHEAD6+140(25),EZIHINST                                          
*                                                                               
         MVC   XHEAD8+120(09),=C'PAYEE   -' PAYEE                               
         MVC   XHEAD8+130(30),EZPYNAM  NAME                                     
         MVC   XHEAD9+130(30),EZPYLIN1 ADDRESS LINE 1                           
         MVC   XHEAD10+130(30),EZPYLIN2 ADDRESS LINE 2                          
         MVC   XHEAD11+130(30),EZPYLIN3 ADDRESS LINE 3                          
         MVC   XHEAD12+130(30),EZPYLIN4 ADDRESS LINE 4                          
*                                                                               
         LA    R2,XHEAD13+150                                                   
         MVC   0(4,R2),=C'PAGE'                                                 
*                                                                               
         EDIT  (B2,PAGE),(4,6(R2)),ALIGN=LEFT                                   
*                                                                               
         SR    RF,RF               BUMP PAGE COUNTER                            
         ICM   RF,3,PAGE                                                        
         LA    RF,1(RF)                                                         
         STCM  RF,3,PAGE                                                        
*                                                                               
         CLI   EZMODE,EZINVL       IF AT END OF INVOICE                         
         BE    HDHKMIDX            SKIP MIDS                                    
*                                                                               
         MVC   XHEAD14+30(9),=C'INSERTION'                                      
         MVC   XHEAD14+142(14),=C'RECONCILIATION'                               
*                                                                               
         L     R5,AM1              ESTABLISH DETAIL LINE ON TITLES              
         LA    R5,PLINSWID(R5)                                                  
         USING PLINED,R5                                                        
*                                                                               
*        MOVE IN TITLES                                                         
*                                                                               
         MVC   PDATE+((L'PDATE-L'CTDATE)/2)(L'CTDATE),CTDATE                    
         MVC   PIO#+((L'PIO#-L'CTIO#)/2)(L'CTIO#),CTIO#                         
         MVC   PCAP+((L'PCAP-L'CTCAP)/2)(L'CTCAP),CTCAP                         
         MVC   PPAGE+((L'PPAGE-L'CTPAGE)/2)(L'CTPAGE),CTPAGE                    
         MVC   PEDN+((L'PEDN-L'CTEDN)/2)(L'CTEDN),CTEDN                         
         MVC   PSPACE+((L'PSPACE-L'CTSPACE)/2)(L'CTSPACE),CTSPACE               
         MVC   PPREM+((L'PPREM-L'CTPREM)/2)(L'CTPREM),CTPREM                    
         MVC   PTS+((L'PTS-L'CTTS)/2)(L'CTTS),CTTS                              
         MVC   PRATE+((L'PRATE-L'CTRATE)/2)(L'CTRATE),CTRATE                    
         MVC   PGRS+((L'PGRS-L'CTGRS)/2)(L'CTGRS),CTGRS                         
         MVC   PNET+((L'PNET-L'CTNET)/2)(L'CTNET),CTNET                         
         MVC   PRMKS+((L'PRMKS-L'CTRMKS)/2)(L'CTRMKS),CTRMKS                    
         MVC   PDBCR+((L'PDBCR-L'CTDBCR)/2)(L'CTDBCR),CTDBCR                    
*                                                                               
HDHKMIDX DS    0H                  AT END OF INVOICE KILL BOXES                 
*                                                                               
HDHKX    XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*        COLUMN TITLES                                                          
*                                                                               
CTDATE   DC    C'DATE'                                                          
CTEDN    DC    C'EDITION'                                                       
CTIO#    DC    C'I/O#'                                                          
CTCAP    DC    C'CAPTION'                                                       
CTPAGE   DC    C'PAGE'                                                          
CTSPACE  DC    C'SPACE'                                                         
CTPREM   DC    C'PREMIUM'                                                       
CTTS     DC    C'TS'                                                            
CTRATE   DC    C'RATE'                                                          
CTGRS    DC    C'GROSS'                                                         
CTNET    DC    C'NET'                                                           
CTRMKS   DC    C'REMARKS'                                                       
CTDBCR   DC    C'DB/CR'                                                         
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - INITPZB'                      
***********************************************************************         
*                                                                     *         
*        INITPZB - INITIALIZE PZBLOCK FOR PZMOD                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
INITPZB  NMOD1 0,**#IPZB                                                        
*                                                                               
         L     RC,VGEND            RE-ESTABLISH GENCON WORKING STORAGE          
*                                                                               
         LA    R0,EZBLOCKD         CLEAR PZBLOCK                                
         LH    R1,=Y(EZBLOCKL)                                                  
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   NEWINV,C'Y'         1ST TIME NEW INVOICE                         
*                                                                               
         MVC   EZWKRFIL,EPICWK                                                  
         MVC   EZWKRBUF,WRKFBUFA                                                
*                                                                               
         L     RF,AIO1                                                          
         ST    RF,EZWKRREC                                                      
*                                                                               
         L     RF,=A(IOA4-(CONHEADH-64)) EXTRA IOAREA                           
         LA    RF,0(RF,RA)                                                      
         ST    RF,EZAREC                                                        
*                                                                               
         MVC   EZCOMFCS,ACOMFACS                                                
         MVI   EZTEST,C'Y'                                                      
         MVI   EZWRITE,C'N'                                                     
         MVC   EZAGY,AGENCY                                                     
         MVC   EZUTL,UTL                                                        
         MVC   EZOFF,C'Y'          INDICATE OFF-LINE                            
         MVC   EZSELINV,SVINV                                                   
*                                                                               
         L     RF,=A(PZMPROC)                                                   
         A     RF,RELO09                                                        
         ST    RF,EZHOOK                                                        
*                                                                               
         L     RF,=A(MYPRINT)      PRINT ROUTINE FOR TRACE                      
         A     RF,RELO09            RELOCATE ADDRESS                            
         ST    RF,EZPRINT                                                       
*                                                                               
INITPZBX DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - MYPRINT'                      
***********************************************************************         
*                                                                     *         
*        MYPRINT - PRINT ROUTINE FOR EZMOD TRACE                      *         
*        ENTERED WITH NO SAVED REGISTERS                              *         
*        SO MUST BE STAND ALONE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
MYPRINT  NMOD1 0,**#MYPRT                                                       
*                                                                               
         LM    R6,RA,MYREGS        RESTORE REGISTERS                            
         L     RC,MYREGS+24                                                     
         USING GEND,RC             RE-ESTABLISH GENCON WORKING STORAGE          
*                                                                               
         LA    R0,PLINSMAX         NUMBER OF DEATIL LINES                       
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
         L     RE,=A(SAVEP1)                                                    
         A     RE,RELO09                                                        
*                                                                               
         MVC   0(PLINSWID,RE),0(R5)  SAVE PRINT LINES                           
         LA    RE,PLINSWID(RE)                                                  
         LA    R5,PLINSWID(R5)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
*                                                                               
         L     RF,ASPACES                                                       
         MVC   PLINSWID(PLINSWID,R5),0(RF)  CLEAR SECOND LINE                   
*                                                                               
         L     R1,0(,R1)                                                        
         MVC   0(PLINSWID,R5),1(R1)       EZMODS LINE IS AT +1                  
*                                                                               
         L     R2,HEADHOOK                                                      
         XC    HEADHOOK,HEADHOOK                                                
         ZIC   R3,LINE                                                          
         MVI   LINE,1                                                           
         ZIC   R4,FORCEHED                                                      
         MVI   FORCEHED,0                                                       
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
         ST    R2,HEADHOOK                                                      
         STC   R3,LINE                                                          
         STC   R4,FORCEHED                                                      
*                                                                               
         LA    R0,PLINSMAX         NUMBER OF DEATIL LINES                       
         L     R5,AP1              POINT TO FIRST DETAIL LINE                   
         L     RE,=A(SAVEP1)                                                    
         A     RE,RELO09                                                        
*                                                                               
         MVC   0(PLINSWID,R5),0(RE)  RESTORE PRINT LINES                        
         LA    RE,PLINSWID(RE)                                                  
         LA    R5,PLINSWID(R5)                                                  
         BCT   R0,*-14                                                          
*                                                                               
         XIT1                                                                   
*                                                                               
MYREGS   DS    7F                  REGISTER SAVEAREA                            
         LTORG                                                                  
*                                                                               
*        OFF-LINE WORKAREAS                                                     
*                                                                               
*        RECONCILLIATION REMARKS SAVEAREA                                       
*                                                                               
NXTRMKA  DS    A                   A(NEZT REMARK SAVEAREA)                      
NXTRMK   DS    CL(PLINSMAX*L'EZRRREM)   REMARKS SAVEAREA                        
NXTRMKX  EQU   *-1                 END OF REMARKS                               
*                                                                               
DTLLINES DS    CL(PLINSMAX*198)    DETAIL LINES BUILD AREA                      
*                                                                               
SAVEPS   DS    0C                                                               
SAVEP1   DS    CL(PLINSWID)                                                     
         ORG   *+(PLINSWID*(PLINSMAX-1))                                        
*                                                                               
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - WORKAREAS'                    
***********************************************************************         
*                                                                     *         
*        VARIOUS WORKAREAS                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
         DS    0D                                                               
*PPEZFWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE PPEZFWORKD                                                     
         PRINT ON                                                               
*PPEZFCNVWD                                                                     
         PRINT OFF                                                              
       ++INCLUDE PPEZFCNVWD                                                     
         PRINT ON                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - WORKING STORAGE'              
***********************************************************************         
*                                                                     *         
*        WORKING STORAGE                                              *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSAPPLW                                                         
RELO09   DS    A                                                                
VGEND    DS    V                   A(GENCON WORKING STORAGE)                    
TCOMSW   DS    CL1                                                              
IBCSW    DS    CL1                                                              
NEWINV   DS    CL1                                                              
PBCNT    DS    CL1                                                              
PRNTSW   DS    CL1                                                              
RCCNT    DS    XL1                                                              
SVDTES   DS   0XL4                                                              
SVDTESTR DS    XL2                                                              
SVDTEEND DS    XL2                                                              
SVSEQ    DS    CL4                                                              
SVBSEQ   DS    XL2                                                              
SVINV    DS    CL10                                                             
*                                                                               
RQOPTS   DS    0CL16                                                            
TRCOPT   DS    CL1                                                              
SRTOPT   DS    CL1                                                              
OPTNSW   DS    XL1                                                              
OPTCONV  EQU   X'80'                                                            
OPTDEL   EQU   X'40'                                                            
OPTUNCV  EQU   X'20'                                                            
OPTDONE  EQU   X'10'                                                            
OPTTDYC  EQU   X'08'               OPTION CONVERTED TODAY                       
OPTNOCS  EQU   X'04'               OPTION NOCOST - BLANK ALL RATES              
OPTTDYL  EQU   X'02'               OPTION LOADED TODAY                          
OPTODAYL DS    XL3                 BINARY LOAD DATE                             
OPTODAYC DS    XL3                 PACKED CONVERT DATE                          
OPTSRCE  DS    CL4                 OPTION SOURCE                                
OPTMOS   DS    CL6                 MONTH OF SERVICE                             
*                                                                               
SRTOPN   DS    CL1                 SORT IS OPEN SWITCH                          
         DS    0D                                                               
SVWFREC0 DS    XL64                                                             
*                                  SCHEDULE LINES                               
AH1      DS    A                   A(HEAD   LINE 1)                             
AM1      DS    A                   A(MID    LINE 1)                             
AP1      DS    A                   A(DETAIL LINE 1)                             
ASPACES  DS    A                   A(SPACES)                                    
         DS    0F                                                               
X        DS    XL64                                                             
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - PLINED'                       
***********************************************************************         
*                                                                     *         
*        DETAIL LINE DSECT                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PLINED   DSECT                                                                  
PLINE    DS    0C                                                               
*                                                                               
B1A      DS    0CL1                                                             
*                                                                               
B1       DS    CL1                                                              
PDATE    DS    CL5                                                              
B2       DS    CL1                                                              
PEDN     DS    CL10                                                             
B3       DS    CL1                                                              
PIO#     DS    CL20                                                             
B4       DS    CL1                                                              
PCAP     DS    CL21                                                             
B5       DS    CL1                                                              
PPAGE    DS    CL5                                                              
B6       DS    CL1                                                              
PSPACE   DS    CL17                                                             
B7       DS    CL1                                                              
PPREM    DS    CL10                                                             
B8       DS    CL1                                                              
PTS      DS    CL2                                                              
B8A      DS    CL1                                                              
PRATE    DS    CL11                                                             
B9       DS    CL1                                                              
PGRS     DS    CL10                                                             
B10      DS    CL1                                                              
PNET     DS    CL10                                                             
B11      DS    CL1                                                              
B12      DS    CL1                                                              
PRMKS    DS    CL18                                                             
B13      DS    CL1                                                              
PDBCR    DS    CL11                                                             
         ORG   PLINE+164                                                        
B14      DS    CL1                                                              
*                                                                               
         ORG   PSPACE                                                           
PAAPDESC DS    CL35                ADJUSTMENT DESCRIPTION                       
         ORG                                                                    
*                                                                               
PLINEL   EQU   *-PLINE             LENGTH OF PRINT LINE                         
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - SORTRECD'                     
***********************************************************************         
*                                                                     *         
*        SORT RECORD DSECT                                            *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SORTRECD DSECT                                                                  
SORTENT  DS    0XL33                                                            
SORTSORT DS    0XL17                                                            
SORTMONS DS    XL2                 MONTH OF SERVICE BINARY YYMM                 
SORTPUB  DS    CL4                                                              
SORTINV  DS    CL10                                                             
SORTINDX DS    XL16                                                             
SORTNEXT EQU   *                                                                
*                                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - SCREENS'                      
***********************************************************************         
*                                                                     *         
*        SCREENS                                                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* PPEZFFFD                                                                      
       ++INCLUDE PPEZFFFD                                                       
         ORG   CONTAGH                                                          
         EJECT                                                                  
* PPEZFE6D                                                                      
       ++INCLUDE PPEZFE6D                                                       
*                                                                               
         ORG   CONHEADH-64+X'2000'                                              
HELPSAVE DS    XL512                                                            
IOA4     DS    XL4096              EXTRA IOAREA                                 
*                                                                               
* DDGENTWA                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
         PRINT ON                                                               
         TITLE 'PPEZF09 - EPIC - INVOICE REPORT - HIDDEN DSECTS'                
***********************************************************************         
*                                                                     *         
*        HIDDEN DSECTS                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
* PPGENPZ                                                                       
         PRINT OFF                                                              
       ++INCLUDE PPGENPZ                                                        
         PRINT ON                                                               
* PRGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE PRGENFILE                                                      
         PRINT ON                                                               
* PZGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE PZGLOBEQUS                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* DDBIGBOX                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
* DDWIDED                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDWIDED                                                        
         PRINT ON                                                               
* PZBLOCK                                                                       
         PRINT OFF                                                              
       ++INCLUDE PZBLOCK                                                        
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FASSB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FASSB                                                          
         PRINT ON                                                               
         PRINT ON                                                               
* DMWRKRD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DMWRKFD                                                        
         PRINT ON                                                               
* PRHELPCB                                                                      
         PRINT OFF                                                              
       ++INCLUDE PRHELPCB                                                       
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064PPEZF09   09/26/97'                                      
         END                                                                    
