*          DATA SET PPMAT25    AT LEVEL 046 AS OF 04/07/15                      
*PHASE T40225A                                                                  
***********************************************************************         
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* BPLA 04/07/15 FIX A TRYFF STATEMENT                                           
*                                                                               
* SMYE 04/19/04 RELINKED AND LOADED TO USE ENLARGED CRTABLE                     
*               IN PPMATPRTD BUT STILL DOES NOT HANDLE MULTIPLE                 
*               DISCREPANCIES IN ONE INVOICE PROPERLY                           
*               PF11 OPTION REPORT FROM "CHECK" SCREENS SHOULD BE               
*               USED INSTEAD OF REPORT "ACTION"                                 
*                                                                               
* SMYE 06/02    NEW LIMIT ACCESS SECURITY                                       
*                                                                               
* SMYE 06/01    ADD OPTIONS FIELD FOR AN ENTRY (M=N) TO EXCLUDE                 
*               "*" MATCHED BUYS FROM REPORT                                    
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                                                                               
*  TITLE: T40225 - REPORTING OF PRINT INVOICES -- REPORT ACTION                 
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS THIS.                             
*                                                                               
*  CALLS TO:    PPMAT07 (SPOOLING OVERLAY), DATAMGR                             
*                                                                               
*  SCREENS:     PPMATDE (T402DE)                                                
*                                                                               
*  LOCALS: REGISTER USAGE                                                       
*          R0 - WORK                                                            
*          R1 - WORK                                                            
*          R2 - WORK (SCREEN FIELD HEADER)                                      
*          R3 - WORK                                                            
*          R4 - OVERLAY SAVED STORAGE    (MYAREAD)                              
*          R5 - MINIO CONTROL BLOCK      (MINBLKD)                              
*          R6 - MINELEM                                                         
*          R7 - SECOND BASE                                                     
*          R8 - SPOOLD                                                          
*          R9 - SYSD                                                            
*          RA - TWA                                                             
*          RB - FIRST BASE                                                      
*          RC - GEND                                                            
*          RD - SYSTEM                                                          
*          RE - SYSTEM/WORK                                                     
*          RF - SYSTEM/WORK                                                     
*                                                                               
***********************************************************************         
T40225   TITLE 'PPMAT25 - PRINT INVOICE REPORT - REPORT ACTION'                 
T40225   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40225*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         LA    R5,MINBLOCK         MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         LA    R4,SYSSPARE         OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         ST    R3,RELO                                                          
*                                                                               
         LA    R1,REC1             ANOTHER IO AREA                              
         ST    R1,AREC1                                                         
*                                                                               
CMODE    CLI   MODE,VALKEY                                                      
         BE    VK                                                               
         CLI   MODE,PRINTREP                                                    
         BE    PR                                                               
         B     EXIT                                                             
*                                                                               
XIT      DS    0H                                                               
         OC    PQID,PQID                                                        
         BZ    EXIT                                                             
         MVI   SPMODE,X'FF'        CLOSE THE PRINTQ                             
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     EXIT                                                             
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                     VALIDATE THE KEY                                          
***********************************************************************         
VK       DS    0H                                                               
*----------------------------------------------------------------               
*                    VALIDATE THE MEDIA                                         
*----------------------------------------------------------------               
         LA    R2,CKRMEDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED             PUTS MEDIA IN QMED                           
*----------------------------------------------------------------               
*                    VALIDATE THE CLIENT                                        
*----------------------------------------------------------------               
         LA    R2,CKRCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   5(R2),3             CAN ENTER ALL                                
         BNE   VKCLT10                                                          
         CLC   CKRCLT(3),=C'ALL'                                                
         BNE   VKCLT10                                                          
         XC    OFFVAL,OFFVAL                                                    
         BAS   RE,VALOFF                                                        
         XC    KCLT,KCLT           CLEAR IF ALL                                 
         XC    QCLT,QCLT                                                        
         B     VKPRD                                                            
*                                                                               
VKCLT10  DS    0H                                                               
         CLI   5(R2),2             CAN ENTER *?                                 
         BNE   VKCLT                                                            
         CLI   CKRCLT,C'*'         OFFICE LIST                                  
         BNE   VKCLT                                                            
         MVC   OFFVAL,CKRCLT+1                                                  
         BAS   RE,VALOFF                                                        
         XC    KCLT,KCLT           CLEAR IF ALL                                 
         XC    QCLT,QCLT                                                        
         B     VKPRD                                                            
*                                                                               
VKCLT    DS    0H                                                               
         GOTO1 VALICLT             PUTS CLIENT IN QCLT                          
         MVC   KCLT,QCLT                                                        
*---------------------------------------------------------------                
*                    VALIDATE THE PRODUCT                                       
*---------------------------------------------------------------                
VKPRD    LA    R2,CKRPRDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   5(R2),3             CAN ENTER ALL                                
         BNE   VKPRD10                                                          
         CLC   CKRPRD(3),=C'ALL'   IF ALL                                       
         BE    *+14                                                             
         CLC   =C'***',CKRPRD      OR IF VARIOUS                                
         BNE   VKPRD10                                                          
         XC    KPRD,KPRD           THEN CLEAR                                   
         XC    QPRD,QPRD                                                        
         B     VKPUB                                                            
*                                                                               
VKPRD10  OC    KCLT,KCLT           WAS A CLIENT GIVEN?                          
         BNZ   VKPRD20                                                          
         MVC   KPRD,CKRPRD                                                      
         MVC   QPRD,CKRPRD                                                      
         B     VKPUB                                                            
*                                                                               
VKPRD20  GOTO1 VALIPRD             PUTS PRODUCT CODE IN QPRD                    
         MVC   KPRD,QPRD                                                        
*---------------------------------------------------------------                
*                    VALIDATE THE PUB                                           
*---------------------------------------------------------------                
VKPUB    LA    R2,CKRPUBH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLI   8(R2),C'='          PUB NAME SEARCH                              
         BNE   VKPUB10                                                          
         SR    R2,RA                                                            
         LA    R3,WORK                                                          
         USING DSPARM,R3                                                        
         XC    DSPARM(DSPARML),DSPARM                                           
         MVC   DSMEDCOD,QMED                                                    
         GOTO1 VSRCHCAL,DMCB,(3,(R2)),(X'80',(RA)),ACOMFACS,           X        
               ('DSPARML',WORK),(1,=CL8'PUB'),0                                 
         DROP  R3                                                               
*                                                                               
VKPUB10  CLI   5(R2),3             CAN ENTER ALL                                
         BNE   VKPUB20                                                          
         CLC   CKRPUB(3),=C'ALL'                                                
         BNE   VKPUB20                                                          
         XC    KPUB,KPUB           CLEAR PUB                                    
         XC    BPUB,BPUB                                                        
         B     VKPER10                                                          
*                                                                               
VKPUB20  DS    0H                  CHECK FOR PUB NUM,ALL                        
         GOTO1 SCANNER,DMCB,(R2),SCANBLK                                        
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VPUB30                                                           
         MVI   COMP4,C'Y'          COMPARE PUBS FOR LEN=4                       
         MVI   ALLZONE,C'Y'        ALL ZONES AND EDITIONS                       
         ZIC   R3,0(R1)                                                         
         STC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R2),12(R1)                                                   
VPUB30   GOTO1 VALIPUB                                                          
         MVC   KPUB,BPUB                                                        
         XC    PUBEXP,PUBEXP                                                    
         XC    PUBNUMH,PUBNUMH                                                  
         GOTO1 VPUBEDIT,DMCB,(8,KPUB),(C'S',PUBEXP)                             
         MVC   PUBNUMH,PUBEXP                                                   
         MVC   PUBINFO,PUBNM                                                    
*------------------------------------------------------------                   
*                  VALIDATE THE PERIOD                                          
*------------------------------------------------------------                   
VKPER10  LA    R2,CKRPERH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         ZIC   R6,5(R2)                                                         
         GOTO1 PERVAL,DMCB,((R6),8(R2)),(X'20',PVOUT)                           
         CLI   DMCB+4,X'04'        ONLY ONE FIELD INPUT?                        
         BE    VKPER20                                                          
         CLI   DMCB+4,X'00'        BOTH FIELDS VALID                            
         LA    R2,CKRPERH                                                       
         BNE   INVLFLD                                                          
VKPER15  LA    R6,PVOUT                                                         
         USING PERVALD,R6                                                       
         CLC   PVALBSTA(2),PVALBEND   CAN ONLY REQUEST PERIOD OF UP TO          
         LA    R2,CKRPERH                                                       
         BNE   INVLFLD                ONE MONTH                                 
         MVC   FLTSTDT,PVALBSTA    BIN YYMMDD START DATE                        
         MVC   FLTNDDT,PVALBEND    BIN YYMMDD END DATE                          
         MVC   QYEAR,PVALCPER+7    CHARAC YEAR FOR MINIOINIT                    
         MVC   8(17,R2),PVALCPER                                                
         OI    6(R2),X'80'                                                      
         LA    R1,17                                                            
         STC   R1,5(R2)                                                         
         B     VKREP10                                                          
*                                                                               
VKPER20  DS    0H                                                               
         CLI   DMCB+4,X'01'        SINGLE DATE VALID?                           
         BNE   *+12                                                             
         LA    R2,CKRPERH                                                       
         B     INVLFLD                                                          
         LA    R6,PVOUT                                                         
         USING PERVALD,R6          START AND END DATE SAME FOR 1 DATE           
         MVC   FLTSTDT,PVALBSTA    BIN YYMMDD START DATE                        
         MVC   FLTNDDT,PVALBSTA    BIN YYMMDD END DATE                          
         MVC   QYEAR,PVALCPER+7    CHARAC YEAR FOR MINIOINIT                    
         MVC   8(8,R2),PVALCPER                                                 
         MVI   16(R2),C'-'                                                      
         MVC   17(8,R2),PVALCPER                                                
         OI    6(R2),X'80'                                                      
         LA    R1,17                                                            
         STC   R1,5(R2)                                                         
*---------------------------------------------------------------                
*                 VALIDATE THE REP IF GIVEN                                     
*---------------------------------------------------------------                
VKREP10  LA    R2,CKRREPH                                                       
         CLI   5(R2),0                                                          
         BE    VKOPT10                                                          
         GOTO1 VALIREP                                                          
         MVC   FLTSREP,8(R2)                                                    
*                                                                               
*---------------------------------------------------------------                
*                 VALIDATE THE OPTIONS IF GIVEN                                 
*---------------------------------------------------------------                
VKOPT10  DS    0H                                                               
         MVI   MYRPTOPT,C' '       CLEAR OPTIONS                                
         LA    R2,CKROPTH                                                       
         CLI   5(R2),0                                                          
         BE    VKX                                                              
         CLC   8(3,R2),=C'M=N'     ONLY ALLOWABLE ENTRY                         
         BNE   INVLFLD                                                          
         MVC   MYRPTOPT,10(R2)                                                  
*                                                                               
VKX      DS    0H                                                               
         DROP  R6                                                               
         EJECT                                                                  
*******************************************************************             
*        BUILD FIRST KEY  --  WHERE TO START                                    
*******************************************************************             
BLDKEY25 DS    0H                                                               
*                                                                               
         XC    KEY,KEY             SET UP KEY TO READ BUYS                      
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         OC    KCLT,KCLT           CLIENT GIVEN?                                
         BZ    BLDK25X                                                          
         MVC   PBUYKCLT,QCLT                                                    
         OC    KPRD,KPRD           PRODUCT GIVEN?(NONE IF *** OR ALL)           
         BZ    BLDK25X                                                          
         MVC   PBUYKPRD,QPRD                                                    
         OC    KPUB,KPUB           PUB GIVEN?                                   
         BZ    BLDK25X                                                          
         MVC   PBUYKPUB(L'BPUB),BPUB                                            
*                                                                               
BLDK25X  XC    FRSTKEY,FRSTKEY                                                  
         MVC   FRSTKEY(16),KEY         SAVE KEY                                 
         B     EXIT                                                             
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
********************************************************************            
*     PRINT REPORT MODE -- GET FIRST BUY                                        
********************************************************************            
PR       DS    0H                                                               
*                                                                               
PRRHI    XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         MVC   KEY(L'FRSTKEY),FRSTKEY         RESTORE KEY                       
         GOTO1 HIGH                GET FIRST MATCH                              
         MVC   FRSTKEY,KEY         SAVE KEY IN CASE MINIO MESSES IT UP          
         XC    KEY,KEY                                                          
         MVC   KEY(16),FRSTKEY                                                  
         MVC   KEY+16(3),FLTSTDT      START DATE                                
         GOTO1 HIGH                                                             
         MVC   FRSTKEY,KEY         SAVE KEY IN CASE MINIO MESSES IT UP          
         B     CHKEY                                                            
*                                                                               
PRRSEQ   XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         MVC   KEY(L'FRSTKEY),FRSTKEY         RESTORE KEY                       
         MVI   PBUYKDAT,X'FF'       BUMP TO NEXT PUB,PRD ETC                    
         GOTO1 HIGH                RESTORE SEQUENCE                             
         CLC   KEY(4),KEYSAVE      SAME UP TO BEFORE CLT                        
         BNE   XIT                 NO, ALL DONE                                 
         MVC   FRSTKEY,KEY         SAVE KEY IN CASE MINIO MESSES IT UP          
         CLC   DUMBKEY(16),KEY                                                  
         BNE   PUTDAT                                                           
         GOTO1 SEQ                 GET NEXT                                     
         CLC   KEY(4),KEYSAVE      SAME UP TO BEFORE CLT                        
         BNE   XIT                 NO, ALL DONE                                 
PUTDAT   XC    KEY,KEY                                                          
         MVC   KEY(16),FRSTKEY                                                  
         MVC   KEY+16(3),FLTSTDT      START DATE                                
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE      SAME UP TO BEFORE CLT                        
         BNE   XIT                 NO, ALL DONE                                 
         MVC   FRSTKEY,KEY         SAVE KEY IN CASE MINIO MESSES IT UP          
         CLC   PBUYKDAT,FLTSTDT                                                 
         BL    PUTDAT                                                           
         B     CHKEY                                                            
*                                                                               
PRRSEQ2  GOTO1 SEQ                 WANT NEXT ONE                                
         MVC   FRSTKEY,KEY         SAVE KEY IN CASE MINIO MESSES IT UP          
*                                                                               
CHKEY    DS    0H                                                               
         CLC   DUMBKEY(14),KEY                                                  
         BE    SKPINIT                                                          
         MVI   TRYFF,C'N'                                                       
         MVI   ALLZONE,C'N'                                                     
SKPINIT  MVC   DUMBKEY,KEY                                                      
         CLC   KEY(4),KEYSAVE      SAME UP TO BEFORE CLT                        
         BNE   XIT                 NO, ALL DONE                                 
*                                                                               
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                  WITHIN PERIOD                                
         CLC   FLTSTDT,PBUYKDAT    BIN YYMMDD START DATE                        
         BH    PRRSEQ                                                           
         CLC   FLTNDDT,PBUYKDAT    BIN YYMMDD END DATE                          
         BNL   *+14                                                             
         MVC   PBUYKPUB,BPUB                                                    
         B     PRRSEQ                                                           
*                                                                               
         DROP  R3                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    PRRSEQ2                                                          
*                                                                               
         DROP  R6                                                               
*                                                                               
         LA    R3,KEY                                                           
         USING PBUYREC,R3         RESSET USING TO KEY                           
*                                                                               
*--------------------------------------------------------------                 
*                VALIDATE THE CLIENT                                            
*--------------------------------------------------------------                 
         OC    KCLT,KCLT           CLIENT SPECIFIED                             
         BZ    GETACLT                                                          
         CLC   PBUYKCLT,KCLT       DOES IT MATCH                                
         BNE   XIT                 NO, ALL DONE                                 
         B     GPRD10                                                           
GETACLT  DS    0H                                                               
         MVC   FAKEFLD(3),PBUYKCLT                                              
         MVI   FAKEFLDH+5,3        INPUT LENGTH                                 
         LA    R2,FAKEFLDH                                                      
         MVC   KEYSV,KEY           SAVE KEY                                     
         GOTO1 VALICLT,DMCB,(X'80',0)        RETURN TO ME WITH ERROR            
         CLI   ERROR,SECLOCK                                                    
         BE    PRRSEQ                                                           
         OC    OFFVAL,OFFVAL                                                    
         BZ    GC10                                                             
         CLC   CLTOFICE,OFFVAL                                                  
         BNE   PRRSEQ                                                           
GC10     MVC   KEY(33),KEYSV                                                    
******   DROP  R6                                                               
******   L     R6,AIO2                                                          
*******  USING PBUYREC,R6                                                       
*--------------------------------------------------------------                 
*              VALIDATE THE PRODUCT                                             
*--------------------------------------------------------------                 
GPRD10   OC    KPRD,KPRD           PRODUCT GIVEN?                               
         BZ    VALP                THEN GET ONE                                 
         CLC   KPRD,PBUYKPRD       SAME AS GIVEN?                               
         BNE   PRRSEQ              NO, THEN GET NEXT BUY                        
         B     VPUB20                                                           
VALP     MVC   KEYSV,KEY           SAVE KEY                                     
         MVC   QPRD,PBUYKPRD                                                    
         BAS   RE,GTPRDNM          GET PRD NAME                                 
         MVC   KEY(33),KEYSV                                                    
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*---------------------------------------------------------------                
*                 VALIDATE THE PUB                                              
*---------------------------------------------------------------                
VPUB20   OC    KPUB,KPUB           PUB GIVEN?                                   
         BZ    VALPB               THEN GET ONE                                 
         CLI   COMP4,C'Y'          COMPARE ONLY 4?                              
         BNE   VPUB20A                                                          
         CLC   BPUB(4),PBUYKPUB                                                 
         BNE   PRRSEQ                                                           
         MVC   BPUB,PBUYKPUB                                                    
         B     MNINIT                                                           
VPUB20A  CLC   BPUB,PBUYKPUB       SAME AS GIVEN?                               
         BNE   PRRSEQ              NO, THEN NEXT                                
         B     MNINIT                                                           
VALPB    MVC   KEYSV,KEY           SAVE KEY                                     
         MVC   BPUB,PBUYKPUB                                                    
****************************************************************                
*              GET DETAIL HEADER FROM MINIO                                     
****************************************************************                
MNINIT   DS    0H                                                               
         OC    KPUB,KPUB                                                        
         BZ    NOPUB                                                            
         CLI   COMP4,C'Y'          COMPARE 4 OF PUB? PUB,ALL                    
         BNE   DOMNIO                                                           
NOPUB    CLI   TRYFF,C'Y'          ALREADY CHECKED FOR ZN/EDT=FFFF              
         BE    DOMNIO              YES                                          
         MVI   TRYFF,C'Y'                                                       
ACRZN    MVC   SVBPUB,BPUB                                                      
         MVC   BPUB+4(2),=X'FFFF'  =ACROSS ZONES AND EDITIONS                   
DOMNIOZ  CLC   CKRPRD,=C'***'                                                   
         BNE   *+10                                                             
         MVC   QPRD,CKRPRD                                                      
         GOTO1 MNIOINIT            INITIALIZE MINIO                             
         MVC   BPUB,SVBPUB                                                      
*                                                                               
         XC    MINEKEY,MINEKEY     READ THE INVOICE HEADER                      
         MVI   MINEKEY,PIMHDREQ                                                 
         BAS   RE,MINIOHI                                                       
         BNE   DOMNIO                                                           
         MVI   ALLZONE,C'Y'                                                     
         XC    BPUB+4(2),BPUB+4                                                 
         B     GHDR10                                                           
*                                                                               
DOMNIO   CLC   CKRPRD,=C'***'                                                   
         BNE   *+10                                                             
         MVC   QPRD,CKRPRD                                                      
         GOTO1 MNIOINIT            INITIALIZE MINIO                             
         LA    R6,MINMKEY                                                       
         USING PINVKEYD,R6                                                      
         XC    MINEKEY,MINEKEY     READ THE INVOICE HEADER                      
         MVI   MINEKEY,PIMHDREQ                                                 
         BAS   RE,MINIOHI                                                       
         BE    GHDR10                                                           
         B     PRRSEQ              GET NEXT SET OF BUYS                         
*                                                                               
FROM07   XC    MINEKEY,MINEKEY                                                  
         XC    SPECREP,SPECREP                                                  
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(1),LSTHDRSQ  LAST HDR SEQ NUM USED                     
         BAS   RE,MINIOHI          RESTORE SEQUENCE                             
         BNE   PRRSEQ                                                           
         BAS   RE,MINIOSEQ         GET NEXT                                     
         BNE   PRRSEQ                                                           
*                                                                               
GHDR10   L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         MVI   TRYFF,C'N'                                                       
*                                                                               
         CLI   PIMHDREL,PIMHDREQ   STILL HEADER ELEMENT?                        
         BNE   PRRSEQ                                                           
*                                                                               
         MVC   LSTHDRSQ,PIMHDRSQ   SAVE THE HEADER SEQUENCE USED                
         CLC   FLTSTDT,PIMSTDT     WITHIN PERIOD                                
         BH    FROM07                                                           
         CLC   FLTNDDT,PIMENDDT                                                 
         BL    FROM07                                                           
*------------------------------------------------------------                   
*            GET SOME INFO FROM HEADER                                          
*------------------------------------------------------------                   
         MVC   INVSTDT,PIMSTDT     COPY THE PERIOD                              
         MVC   INVENDDT,PIMENDDT                                                
*                                                                               
         MVI   AMGRSNET,C'N'       GROSS OR NET RATES                           
         TM    PIMSTAT,X'40'                                                    
         BZ    *+8                                                              
         MVI   AMGRSNET,C'G'                                                    
*                                                                               
         MVC   INVNUMH,PIMINVNO    INVOICE NUMBER FOR HEADING                   
*                                                                               
         XC    PERIOD,PERIOD       FORMAT PERIOD FOR HEADING                    
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(11,PERIOD)                              
         MVC   PERIOD+9(4),=C'THRU'                                             
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(11,PERIOD+14)                          
*                                  INVOICE DATE FOR HEADING                     
         GOTO1 DATCON,DMCB,(3,PIMINVDT),(11,INVDTH)                             
*                                                                               
*                                  INVOICE GROSS TOTAL                          
         EDIT  (P5,PIMAMT),(13,GROSSH),2,ALIGN=LEFT,FLOAT=$                     
         ZAP   INVGRTOT,PIMAMT                                                  
*-----------------------------------------------------------------              
*              CHECK IF ESTIMATE GIVEN                                          
*-----------------------------------------------------------------              
         OC    PIMEST,PIMEST       GET ESTIMATE IF ANY                          
         BNZ   YEST                                                             
         XC    BEST,BEST                                                        
         XC    HEST,HEST                                                        
         XC    HESTNM,HESTNM                                                    
         XC    HESTPERD,HESTPERD                                                
         XC    ESTWORD,ESTWORD                                                  
         B     SPREP                                                            
*                                                                               
YEST     PACK  DUB(8),PIMEST(3)    STORE BINARY EST IN BEST                     
         CVB   R1,DUB                                                           
         STH   R1,BEST                                                          
*---------------------------------------------------------------                
*            CHECK IF SPECIAL REP GIVEN                                         
*---------------------------------------------------------------                
SPREP    XC    SPECREP,SPECREP     SPECIAL REP FILTER?                          
         XC    SREPCODE,SREPCODE                                                
         XC    SREPNAME,SREPNAME                                                
         XC    SREPADR1,SREPADR1                                                
         XC    SREPADR2,SREPADR2                                                
         OC    FLTSREP,FLTSREP                                                  
         BZ    SRHERE                                                           
         CLC   FLTSREP,PIMSREP                                                  
         BNE   FROM07                                                           
SRHERE   OC    PIMSREP,PIMSREP     SPECIAL REP FOR THIS INVOICE HEADER          
         BZ    GTEST                                                            
         MVC   SPECREP,PIMSREP                                                  
         MVC   SREPCODE,PIMSREP                                                 
         XC    KEY,KEY             YES, THEN GET INFO                           
         LA    R6,KEY                                                           
         USING PREPKEY,R6                                                       
         MVC   PREPKAGY,AGENCY                                                  
         MVC   PREPKMED,QMED                                                    
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,SPECREP                                                 
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AREC1                                                        
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         USING PREPELEM,R6                                                      
         MVC   SREPNAME,PREPNAME                                                
         MVC   SREPADR1,PREPLIN1                                                
         MVC   SREPADR2,PREPLIN2                                                
*-------------------------------------------------------------                  
*           GET ESTIMATE INFO IF BEST IS GIVEN                                  
*-------------------------------------------------------------                  
GTEST    DS    0H                                                               
         OC    BEST,BEST                                                        
         BZ    NOEST                                                            
         XC    KEY,KEY             GET ESTIMATE RECORD                          
         LA    R6,KEY                                                           
         USING PESTKEY,R6                                                       
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'                                                   
         MVC   PESTKCLT,QCLT                                                    
         MVC   PESTKPRD,QPRD                                                    
         CLC   =C'***',CKRPRD                                                   
         BNE   *+10                                                             
         MVC   PESTKPRD,PBUYKPRD                                                
         MVC   PESTKEST,BEST                                                    
         DROP  R6                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE                                                  
         BNE   NOEST                                                            
         MVC   AIO,AREC1                                                        
         GOTO1 GETREC                                                           
*                                                                               
         USING PESTELEM,R6                                                      
         MVI   ELCODE,X'07'                                                     
         BAS   RE,GETEL                                                         
         BNE   NOEST                                                            
         MVC   HESTNM,PESTNAME                                                  
         MVC   ESTSTDT,PESTST                                                   
         MVC   ESTNDDT,PESTEND                                                  
         GOTO1 DATCON,DMCB,(0,ESTSTDT),(11,HESTPERD)                            
         MVI   HESTPERD+8,C'-'                                                  
         LA    R2,HESTPERD                                                      
         GOTO1 DATCON,DMCB,(0,ESTNDDT),(11,9(R2))                               
         EDIT  (B2,BEST),(3,HEST),FILL=0                                        
         MVC   ESTWORD,=C'ESTIMATE'                                             
*-------------------------------------------------------------------            
*       FORMAT PUB NAME AND GET ADDRESS                                         
*-------------------------------------------------------------------            
NOEST    DS    0H                  FORMAT PUB STUFF                             
         MVC   KEYSV,KEY           SAVE KEY                                     
         XC    PUBEXP,PUBEXP                                                    
         GOTO1 VPUBEDIT,DMCB,(8,BPUB),(C'S',PUBEXP)                             
         MVC   PUBNUMH,PUBEXP      EXPANDED PUB,ZONE,EDITION                    
         CLI   ALLZONE,C'Y'                                                     
         BNE   *+10                                                             
         MVC   PUBNUMH+8(4),=C',ALL'                                            
         MVC   PUBINFO,PUBNM                                                    
         BAS   RE,GTPUBADR                                                      
         BAS   RE,GTPUBNM                                                       
         MVC   KEY(33),KEYSV                                                    
*                                                                               
         DROP  R6,R3                                                            
         EJECT                                                                  
*********************************************************************           
* CORRECTED INVOICE DETAILS  -- SAVE IN TABLE                                   
*********************************************************************           
CR       DS    0H                                                               
*                                                                               
         LA    R3,CRTABLE                                                       
         USING CORRTBLD,R3                                                      
         LA    R2,EOTABLE          MAX NUMBER OF CORRECTIONS????                
         ST    R2,EOTABLE                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   CRX                                                              
         B     CR100LP                                                          
*                                                                               
CR100NXH XC    MINEKEY,MINEKEY     RESTORE SEQUENCE                             
         MVC   MINEKEY(L'MINEKSV),MINEKSV                                       
         BAS   RE,MINIOHI                                                       
         BNE   CRX                                                              
CR100NXS BAS   RE,MINIOSEQ                                                      
         BNE   CRX                                                              
         MVC   MINEKEY(L'MINEKSV),MINEKSV                                       
*                                                                               
CR100LP  L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ   DETAIL ELEMENT STILL?                        
         BNE   CRX                 NO, LEAVE                                    
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ   SAME HEADER SEQ #?                           
         BNE   CRX                                                              
*                                                                               
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2                                   
         MVC   MINEKSV,MINEKEY     SAVE PLACE IN X'20' SEQUENCE                 
*                                                                               
         CLC   PIMIDATE,INVSTDT    DATE WITHIN INVOICE PERIOD                   
         BL    CR100NXS            NO                                           
         CLC   PIMIDATE,INVENDDT                                                
         BH    CR100NXS            NO                                           
*                                                                               
         CLI   PIMBLINE,0          UNMATCHED?                                   
         BE    CR100NXS            YES --  ONLY WANT CORRECTIONS                
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BNZ   CR100NXS            YES, NOTHING TO SAVE                         
*                                                                               
         CLC   =C'***',CKRPRD     PRODUCT "VARIOUS"                             
         BNE   CR111              NO                                            
*                                                                               
         L     RE,AIO2                                                          
         USING PBUYREC,RE                                                       
         CLC   PIMSPRD,PBUYKPRD   SPECIFIC PROD SAME AS BUY PROD ?              
         BNE   CR100NXS           NO - DO NOT SAVE                              
         DROP  RE                                                               
*                                                                               
CR111    MVC   CORRLINE,PIMBLINE   SAVE TABLE ENTRY                             
         MVC   CORRDATE,PIMIDATE                                                
         MVC   CORBDATE,PIMBDATE                                                
         MVC   CORREST,PIMIEST                                                  
         MVC   CORBEST,PIMBEST                                                  
         MVC   CORRZONE,PIMBZONE                                                
         MVC   CORREDTN,PIMBEDTN                                                
         MVC   CORRSPCE,PIMSPACE                                                
         MVC   CORRUIND,PIMUIND                                                 
         MVC   CORRUNIT,PIMUNITS                                                
         MVC   CORRCLMS,PIMCLMS                                                 
         MVC   CORRCOST,PIMCOST                                                 
         MVC   CORRPREM,PIMPREM                                                 
         MVC   CORRCLRS,PIMCLRS                                                 
         MVC   CORRSTAT,PIMDSTAT                                                
         MVC   CORRCSIN,PIMCSIND                                                
         XC    CORRCMDT,CORRCMDT                                                
*                                                                               
         MVC   CORRPRD,PIMSPRD                                                  
*                                                                               
         TM    PIMDSTAT,X'04'      TEST FOR COMMENTS                            
         BNZ   CR120                                                            
         LA    R3,L'CORRTBL(R3)                                                 
         C     R3,EOTABLE                                                       
         BL    CR100NXS                                                         
         B     CRX                                                              
*                                                                               
CR120    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'30'       COMMENT ELEMENT                              
         MVC   MINEKEY+1(L'PIMDTLS1),PIMDTLS1   HEADER SEQ NUM                  
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2   DETAIL SEQ NUM                  
         MVC   DTLSEQN,PIMDTLS2                                                 
         BAS   RE,MINIOHI                                                       
         BNE   CR150                                                            
*                                                                               
CR130    L     R6,MINELEM                                                       
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ   COMMENT ELEMENT?                             
         BNE   CR150                                                            
         CLC   PIMCOMS1,LSTHDRSQ   SAME HEADER SEQ #?                           
         BNE   CR150                                                            
         CLC   PIMCOMS2,DTLSEQN    SAME DETAIL SEQ NUM?                         
         BNE   CR150                                                            
         MVC   CORRCMDT,PIMCOMS2   SAVE DETAIL SEQ NUM                          
*                                                                               
CR150    LA    R3,L'CORRTBL(R3)                                                 
         C     R3,EOTABLE                                                       
         BL    CR100NXH                                                         
*                                                                               
CRX      MVI   0(R3),X'FF'                                                      
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE KEY  AND CALLS THE PRINT ENGINE OVERLAY               
***********************************************************************         
CALL25   DS    0H                                                               
*                                                                               
         ZAP   MYPUBAC,PUBAGYCM                                                 
         MVC   REPCODE,RREPCODE    REP INFO                                     
         MVC   REPNAME,RREPNAME                                                 
         MVC   REPADR1,RREPADR1                                                 
         MVC   REPADR2,RREPADR2                                                 
         OC    SPECREP,SPECREP     SPECIAL REP IS MORE IMPORTANT                
         BZ    CALL25A                                                          
         MVC   REPCODE,SREPCODE                                                 
         MVC   REPNAME,SREPNAME                                                 
         MVC   REPADR1,SREPADR1                                                 
         MVC   REPADR2,SREPADR2                                                 
CALL25A  DS    0H                                                               
*                                                                               
         XC    KEY,KEY             SET UP KEY TO READ SAME BUY                  
         MVC   KEY(16),DUMBKEY                                                  
         MVC   KEY+16(3),FLTSTDT      START DATE                                
*-----------------------------------------------------------------              
*          CALL PPMAT07 PRINT ENGINE OVERLAY                                    
*-----------------------------------------------------------------              
CALL07   DS    0H                                                               
         CLC   =C'ALL',CKRPRD                                                   
         BE    *+10               DO NOT MOVE TO QPRD                           
         MVC   QPRD,CKRPRD                                                      
         GOTO1 CALLOV,DMCB,(X'07',0),(0,0)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*                                                                               
         B     FROM07                                                           
         EJECT                                                                  
*                                                                               
********************************************************************            
*        OFFICE LIST STUFF                                                      
********************************************************************            
VALOFF   NTR1                                                                   
         L     RF,ATWA                                                          
*                                                                               
         OC    6(2,RF),6(RF)     CHK FOR LIMIT ACCESS                           
         BZ    VOFF10                                                           
         CLI   6(RF),C'$'                                                       
         BNE   NODOL                                                            
         BAS   RE,PPCLIVER                                                      
         BE    VOFF10                                                           
*                                                                               
ACCERR   MVI   ERROR,SECLOCK        SECURITY LOCK-OUT                           
         GOTO1 ERREX                                                            
*                                                                               
NODOL    DS    0H                                                               
         CLI   6(RF),C'*'        CHK OFFICE LIMIT ACCESS                        
         BNE   VOFF5                                                            
         CLC   7(1,RF),OFFVAL                                                   
         BNE   ACCERR                                                           
         B     VOFF10                                                           
*                                                                               
VOFF5    CLC   6(3,RF),QCLT                                                     
         BNE   ACCERR                                                           
*                                                                               
VOFF10   DS    0H                                                               
         B     XIT                                                              
         SPACE 2                                                                
*       *************************                                               
******  TEST OFFICE LIST SECURITY  ******                                       
*       *************************                                               
         SPACE 2                                                                
*                  / **************************\                                
PPCLIVER NTR1 ***** NOTE- I/O AREA M/B IN AREC ******                           
*                  \ **************************/                                
         SPACE 2                                                                
         XC    DMCB(12),DMCB                                                    
         MVC   DMCB+4(4),=X'D9000A38'  GET OFFICER ADDRESS                      
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),255                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         XC    WORK,WORK                                                        
         LA    R1,WORK                                                          
         USING OFFICED,R1                                                       
         MVI   OFCSYS,C'P'                                                      
         L     RF,ATWA                                                          
         MVC   OFCAUTH,6(RF)                                                    
         MVC   OFCAGY,AGENCY                                                    
         MVC   OFCOFC,OFFVAL                                                    
         DROP  R1                                                               
         L     RF,DMCB                                                          
         LA    R8,DATAMGR                                                       
         GOTO1  (RF),DMCB,WORK,(R8)                                             
         CLI   0(R1),0                                                          
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE PRD NAME USING QPRD FROM A BUY RECORD                   
***********************************************************************         
GTPRDNM  NTR1                                                                   
         XC    KEY,KEY             GET PRODUCT RECORD                           
         LA    R3,KEY                                                           
         USING PPRDKEY,R3                                                       
         MVC   PPRDKAGY,AGENCY                                                  
         MVC   PPRDKMED,QMED                                                    
         MVI   PPRDKRCD,X'06'                                                   
         MVC   PPRDKCLT,QCLT                                                    
         MVC   PPRDKPRD,QPRD                                                    
         DROP  R3                                                               
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC1                                                         
         ST    R6,AIO                                                           
         GOTO1 GETREC                                                           
*                                                                               
         USING PPRDELEM,R6                                                      
         MVI   ELCODE,X'06'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   PRDNM,PPRDNAME                                                   
*                                                                               
         CLC   =C'***',CKRPRD                                                   
         BNE   *+16                                                             
         XC    PRDNM,PRDNM                                                      
         MVC   PRDNM(7),=C'VARIOUS'                                             
*                                                                               
         L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         B     EXIT                                                             
         DROP  R6                                                               
***********************************************************************         
* THIS ROUTINE GETS THE PUB NAME AND CITY, AND THE PAYING REP NAME,             
* AND ADDRESS                                                                   
***********************************************************************         
GTPUBNM  NTR1                                                                   
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PUBRECD,R3                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),BPUB     MOVE PUB/ZONE/EDTN                           
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR'                                            
         GOTO1 HIGH                                                             
         XC    FILENAME,FILENAME                                                
*                                                                               
         CLC   KEY(25),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AREC1                                                         
         ST    R6,AIO                                                           
         MVC   FILENAME,=CL8'PUBFILE'                                           
         GOTO1 GETREC                                                           
         XC    FILENAME,FILENAME                                                
         XC    REPCODE,REPCODE                                                  
*                                                                               
         GOTO1 VPUBFLT,DMCB,(R6),PUBINFO                                        
*                                                                               
         MVI   PUBGSTAX,0                                                       
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   GP50                                                             
         USING PUBNAMEL,R6                                                      
         MVC   PUBGSTAX,PUBGST                                                  
*                                                                               
GP50     DS    0H                                                               
         XC    MAGFREQ,MAGFREQ                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    GP100                                                            
         ZAP   PUBCASHD,=P'20'                                                  
         ZAP   PUBAGYCM,=P'15000'                                               
         B     GP200                                                            
*                                                                               
GP100    DS    0H                                                               
         USING PUBGENEL,R6                                                      
         ZAP   PUBCASHD,PUBCD                                                   
         ZAP   PUBAGYCM,PUBAC                                                   
         MVC   MAGFREQ,PUBMFREQ                                                 
*                                                                               
GP200    DS    0H                                                               
         L     R6,AIO                                                           
         XC    PUBTAXES,PUBTAXES                                                
         MVI   ELCODE,X'22'        GET THE TAX ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   GP205                                                            
         USING PUBTAXEL,R6                                                      
         MVC   PUBTAXES,PUBTAX1    COPY THE TAX RATES AND DATES                 
*                                                                               
GP205    DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'14'                                                     
         BAS   RE,GETEL                                                         
         BNE   GTPUBX                                                           
         USING PUBREPEL,R6                                                      
GP210    MVC   RREPCODE,PUBPAREP                                                
         OC    PUBPAREP,PUBPAREP                                                
         BNZ   GP215                                                            
         BAS   RE,NEXTEL                                                        
         CLI   0(R6),X'14'                                                      
         BNE   GTPUBX                                                           
         B     GP210                                                            
*                                                                               
GP215    XC    KEY,KEY             GET PAYING REP INFO                          
         LA    R6,KEY                                                           
         USING PREPKEY,R6                                                       
         MVC   PREPKAGY,AGENCY                                                  
         MVC   PREPKMED,QMED                                                    
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,RREPCODE                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AIO,AREC1                                                        
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PREPREC,R6                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PREPELEM,R6                                                      
         MVC   RREPNAME,PREPNAME                                                
         MVC   RREPADR1,PREPLIN1                                                
         MVC   RREPADR2,PREPLIN2                                                
*                                                                               
GTPUBX   L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         B     EXIT                                                             
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE PUB PAYING ADDRESS, WHAT A BIG PAIN IN THE BUTT         
*    PAYING REPS AND SPECIAL REPS OVERRIDE THIS ADDRESS ANYWAY                  
***********************************************************************         
GTPUBADR NTR1                                                                   
*                                                                               
         MVC   SVBPUB,BPUB                                                      
         MVI   RDUPDATE,C'N'                                                    
         L     R6,AREC1                                                         
         ST    R6,AIO                                                           
         XC    REPCODE,REPCODE                                                  
         XC    RREPCODE,RREPCODE                                                
         XC    RREPNAME,RREPNAME                                                
         XC    RREPADR1,RREPADR1                                                
         XC    RREPADR2,RREPADR2                                                
*                                                                               
         CLI   ALLZONE,C'Y'                                                     
         BNE   GPA4                                                             
         MVC   BPUB+4(2),=X'FFFF'  =ACROSS ALL ZONES AND EDITIONS               
GPA4     DS    0H                                                               
*--------------------------------------------------------------                 
*                READ PUB REC                                                   
*--------------------------------------------------------------                 
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING PUBRECD,R3                                                       
         MVC   PUBKMED,QMED                                                     
         MVC   PUBKPUB(6),BPUB     MOVE PUB/ZONE/EDTN                           
         LA    R5,6                COMPARE LEN - 1                              
         CLI   BPUB+4,X'FF'        TEST ALL ZONES/EDTS                          
         BNE   GPA4C                                                            
GPA4B    DS    0H                                                               
         LA    R5,4                COMPARE LEN - 1                              
         XC    KEY+5(2),KEY+5                                                   
GPA4C    DS    0H                                                               
         MVC   PUBKAGY,AGENCY                                                   
         MVI   PUBKCOD,X'81'                                                    
GPA5     DS    0H                                                               
         MVC   FILENAME,=CL8'PUBDIR  '                                          
         GOTO1 HIGH                                                             
GPA5C    DS    0H                                                               
         EX    R5,*+12                                                          
         BNE   GPAX                                                             
         BE    *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         CLC   KEY+7(3),KEYSAVE+7                                               
         BNE   GPAX                                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
GPA2     DS    0H                                                               
         MVC   FILENAME,=CL8'PUBFILE '                                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PUBRECD,R6                                                       
         GOTO1 VPUBFLT,DMCB,(R6),PUBINFO                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PUBNAMEL,R6                                                      
         MVC   RREPNAME,=CL30'PAY PUB DIRECT'                                   
         MVC   RREPADR1,PUBLINE1                                                
         MVC   RREPADR2,PUBLINE2                                                
GPA2B    DS    0H                                                               
         CH    R5,=H'4'                                                         
         BE    GPA200                                                           
         CLI   SVAGPROF+13,C'0'    IF COMBINED PAYMENTS                         
         BNE   GPA4B               GO BACK AND GET BASE PUB FOR ADDR            
*                                                                               
GPA200   DS    0H                                                               
         SPACE 2                                                                
*                                                                               
         MVC   FULL(3),=3X'FF'                                                  
*****    L     R6,AIO                                                           
*****    MVI   ELCODE,X'08'                                                     
*****    BAS   RE,GETEL                                                         
*****    CLI   0(R6),X'08'                                                      
*****    BE    P4A                                                              
*****P4A0     DS    0H                                                          
*****    BAS   RE,NEXTEL                                                        
*****    CLI   0(R6),X'08'                                                      
*****    BNE   GPAX                                                             
*****    USING PUBAOVEL,R6                                                      
*****    CLC   PUBAOFF,QCLT                                                     
*****    BE    P4A                                                              
*****    CLI   PUBAOFF,X'FF'                                                    
*****    BNE   P4A0                                                             
*****    CLC   PUBAOFF+1(1),CLTOFICE                                            
*****    BE    P4A                                                              
*****    CLC   PUBAOFF,=3X'FF'                                                  
*****    BNE   P4A0                                                             
*****P4A      MVC   RREPNAME,PUBAONAM                                           
*****    MVC   RREPADR1,PUBAOLN1                                                
*****    MVC   RREPADR2,PUBAOLN2                                                
*****    MVC   FULL,PUBAOFF                                                     
*                                                                               
         DROP  R6                                                               
         L     R6,AIO                                                           
         USING PUBRECD,R6                                                       
*                                       PAY ADDRESS OVERRIDE                    
         MVC   CLTAGY,PUBKAGY                                                   
         MVC   CLTMED,PUBKMED                                                   
         MVC   CLTCODE,QCLT                                                     
         MVC   CLTOFF,CLTOFICE                                                  
*                                                                               
         GOTO1 VPGETADR,DMCB,(=C'P',CLTDATA),PUBREC,DATAMGR,0                   
*                                                                               
         CLI   0(R1),X'FF'         ERROR IN CALL ?                              
         BNE   *+6                 NO                                           
         DC    H'0'                                                             
*                                                                               
         CLI   0(R1),0             ADDRESS RECORD FOUND ?                       
         BE    GPAX                NO - DONE                                    
*                                  PAY ADDRESS REC FOUND                        
         L     R6,4(R1)            A(ADDRESS INFO FROM CALL)                    
         USING PGETADRD,R6                                                      
         MVC   REPNAME,PGADNAME                                                 
         MVC   REPADR1,PGADLIN1                                                 
         MVC   REPADR2,PGADLIN2                                                 
         MVC   FULL(3),1(R1)       ADDRESS 'LEVEL'                              
*                                                                               
GPAX     L     R1,AIO1                                                          
         ST    R1,AIO                                                           
         XC    FILENAME,FILENAME                                                
         MVC   BPUB,SVBPUB                                                      
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
CLTDATA  DS    0CL7         USED TO PASS KEY INFO TO PPGETADR MODULE            
CLTAGY   DS    CL2                                                              
CLTMED   DS    CL1                                                              
CLTCODE  DS    CL3                                                              
CLTOFF   DS    CL1                                                              
*                                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS A MINIO ELEMENT.  MINEKEY MUST BE SET BY CALLER            
***********************************************************************         
MINIORD  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINRD',(R5))                                        
         CLI   MINERR,0                                                         
         BE    EXIT                                                             
         DC    H'0'                DIE ON ANY ERROR                             
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ HIGH A MINIO ELEMENT.  MINEKEY MUST BE SET BY               
* THE CALLER.                                                                   
***********************************************************************         
MINIOHI  NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINHI',(R5))                                        
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         CLI   MINERR,MINESNF      RETURN 'NO' IF SET DOES NOT EXIST            
         BE    NO                                                               
         CLI   MINERR,MINERNF      RETURN 'NO' IF RECORD NOT FOUND              
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
***********************************************************************         
* THIS ROUTINE READ SEQUENTIAL FOR A MINIO ELEMENT.                             
***********************************************************************         
MINIOSEQ NTR1                                                                   
         GOTO1 MINIO,DMCB,('MINSEQ',(R5))                                       
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    YES                                                              
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    NO                                                               
         CLI   MINERR,MINESNF      RETURN 'NO' IF SET DOES NOT EXIST            
         BE    NO                                                               
         CLI   MINERR,MINERNF      RETURN 'NO' IF RECORD NOT FOUND              
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
RELO     DS    A                                                                
***********************************************************************         
* ERROR MESSAGES                                                                
***********************************************************************         
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* DDREMOTED                                                                     
* DDREPMASTD                                                                    
* DMPRTQL                                                                       
* CTGENFILE                                                                     
* FATIOB                                                                        
* SCREENS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE DMPRTQL                                                        
       ++INCLUDE DDREMOTED                                                      
       ++INCLUDE DDREPMASTD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR CHECK SCREEN FOR NEWSPAPER)             
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR CHECK SCREEN FOR MAGAZINE)              
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATDED          (OUR CHECK SCREEN FOR PRINT ACTION)          
       ++INCLUDE DDGENTWA                                                       
* DDGENTWA                                                                      
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* DDBIGBOX                                                                      
* DDOFFICED                                                                     
* FAFACTS                                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE DDOFFICED                                                      
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
PUBRECD  DSECT                                                                  
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBGENEL                                                       
       ++INCLUDE PUBNAMEL                                                       
       ++INCLUDE PUBREPEL                                                       
       ++INCLUDE PUBAOVEL                                                       
       ++INCLUDE PUBTAXEL                                                       
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
         EJECT                                                                  
PPRDRECD DSECT                                                                  
       ++INCLUDE PPRDREC                                                        
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
       ++INCLUDE PESTREC                                                        
         EJECT                                                                  
       ++INCLUDE PPSRCHPARM                                                     
         EJECT                                                                  
* MY STORAGE AREA                                                               
       ++INCLUDE PPMATPRTD                                                      
*******************                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'046PPMAT25   04/07/15'                                      
         END                                                                    
