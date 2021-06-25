*          DATA SET PPMAT06    AT LEVEL 013 AS OF 01/02/06                      
*PHASE T40206A                                                                  
***********************************************************************         
*                                                                               
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 04/19/04 RELINKED AND LOADED TO USE ENLARGED CRTABLE                     
*               IN PPMATPRTD                                                    
*                                                                               
* SMYE 03/02    FIX BUG FOR PRODUCT VARIOUS (***) - WAS                         
*               ONLY TESTING FOR BUYDATE AND BUYLINE TO DISTINGUISH             
*               DISCREPANCIES, THUS IF DIFFERENT PRODUCTS HAD THE SAME          
*               BUYDATE, THE DISCREPANCY ENTERED FOR ONE PRODUCT WOULD          
*               BE TOTALLED AND LISTED FOR EVERY PRODUCT WITH THIS DATE         
*                                                                               
*                                                                               
*  TITLE: T40206 - REPORTING OF PRINT INVOICES -- VALKEY FOR PF11               
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS PPMAT02 (T40202)                  
*               WHICH CALLS THIS.                                               
*                                                                               
*  CALLS TO:    PPMAT07 (SPOOLING OVERLAY), DATAMGR                             
*                                                                               
*  SCREENS:     NONE FOR REPORT -- GET INFO FROM CHECK SCREEN (T402FB)          
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
T40206   TITLE 'PPMAT06 - PRINT INVOICE REPORT -- PF11 REPORT OPTION'           
T40206   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T40206*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     RA,4(R1)                                                         
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,8(R1)                                                         
         USING SYSD,R9                                                          
         L     R8,12(R1)                                                        
         USING SPOOLD,R8                                                        
         L     R5,16(R1)           MINIO CONTROL BLOCK                          
         USING MINBLKD,R5                                                       
         L     R4,20(R1)           OVERLAY SAVED STORAGE                        
         USING MYAREAD,R4                                                       
         ST    R3,RELO                                                          
*                                                                               
         B     VK                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*                   VALIDATE THE KEY                                            
***********************************************************************         
VK       DS    0H                                                               
         LA    R0,MYSTART          CLEARS MY WORK STORAGE                       
         LA    R1,MYLENGTH                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         OI    GENSTAT3,MULTFILS   ALLOWS US TO CLOSE PRINT QUEUE               
*-----------------------------------------------------------------              
*                  VALIDATE THE MEDIA                                           
*-----------------------------------------------------------------              
         LA    R2,CHKMEDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALIMED                                                          
*-----------------------------------------------------------------              
*                  VALIDATE THE CLIENT                                          
*-----------------------------------------------------------------              
         LA    R2,CHKCLTH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         GOTO1 VALICLT                                                          
*-----------------------------------------------------------------              
*                  VALIDATE THE PRODUCT                                         
*-----------------------------------------------------------------              
         LA    R2,CHKPRDH                                                       
         CLI   5(R2),0                                                          
         BE    MISSFLD                                                          
         CLC   =C'***',CHKPRD                                                   
         BE    VPUB10                                                           
         GOTO1 VALIPRD                                                          
*-----------------------------------------------------------------              
*                  VALIDATE THE PUBLICATION                                     
*-----------------------------------------------------------------              
VPUB10   MVC   SVBPUB,BPUB                                                      
         LA    R2,CHKPUBH                                                       
         GOTO1 SCANNER,DMCB,(R2),SCANBLK,C',=,,'                                
         CLI   DMCB+4,0                                                         
         BE    INVLFLD                                                          
         LA    R1,SCANBLK                                                       
         CLC   =C'ALL',44(R1)                                                   
         BNE   VPUB100                                                          
         MVI   ALLZONE,C'Y'                                                     
         MVC   BPUB+4(2),=X'FFFF'                                               
VPUB100  DS    0H                                                               
*-----------------------------------------------------------------              
*                  INITIALIZE MINIO                                             
*-----------------------------------------------------------------              
         GOTO1 MNIOINIT                                                         
         MVC   BPUB,SVBPUB                                                      
         MVC   KPUB,BPUB                                                        
*-----------------------------------------------------------------              
*           VALIDATE INVOICE NUMBER AND GET INFO                                
*-----------------------------------------------------------------              
         CLI   CHKPERH+5,0        SHOULD DEFINATELY BE SOMETHING HERE           
         BE    GETOUT                                                           
         BAS   RE,GTHDR                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
*                                                                               
         MVC   INVSTDT,PIMSTDT     COPY THE PERIOD                              
         MVC   INVENDDT,PIMENDDT                                                
*                                                                               
         XC    PERIOD,PERIOD                                                    
         GOTO1 DATCON,DMCB,(3,PIMSTDT),(11,PERIOD)                              
         MVC   PERIOD+9(4),=C'THRU'                                             
         GOTO1 DATCON,DMCB,(3,PIMENDDT),(11,PERIOD+14)                          
*                                                                               
         MVI   AMGRSNET,C'N'       NET OR GROSS RATES FOR 07                    
         TM    PIMSTAT,X'40'                                                    
         BZ    *+8                                                              
         MVI   AMGRSNET,C'G'                                                    
*                                                                               
         CLI   CHKINVNH+5,0        SHOULD DEFINATELY BE SOMETHING HERE          
         BE    GETOUT                                                           
         MVC   INVNUMH,CHKINVN     SAVE INVOICE NUMBER                          
*                                                                               
         GOTO1 DATCON,DMCB,(3,PIMINVDT),(11,INVDTH)                             
         MVC   INVYRMON,PIMINVDT   SAVE INVOICE YEAR AND MONTH                  
*                                                                               
         EDIT  (P5,PIMAMT),(13,GROSSH),2,ALIGN=LEFT,FLOAT=$                     
         ZAP   INVGRTOT,PIMAMT                                                  
*-----------------------------------------------------------------              
*           IF ESTIMATE IN HEADER GET INFO FOR HEADLINES                        
*-----------------------------------------------------------------              
         XC    BEST,BEST                                                        
         OC    PIMEST,PIMEST       SHOW ESTIMATE IF ANY                         
         BZ    SPREP                                                            
         LA    R2,CHKESTMH                                                      
         MVC   8(L'CHKESTM,R2),PIMEST                                           
         MVI   5(R2),L'CHKESTM                                                  
         MVC   ESTIMATE,PIMEST                                                  
         GOTO1 VALIEST                                                          
         MVC   HESTNM,ESTNM                                                     
         GOTO1 DATCON,DMCB,(0,ESTSTDT),(11,HESTPERD)                            
         MVI   HESTPERD+8,C'-'                                                  
         LA    R2,HESTPERD                                                      
         GOTO1 DATCON,DMCB,(0,ESTNDDT),(11,9(R2))                               
         EDIT  (B2,BEST),(3,HEST),FILL=0                                        
         MVC   ESTWORD,=C'ESTIMATE'                                             
*-----------------------------------------------------------------              
*         IF SPECIAL REP IN HEADER GET INFO FOR HEADLINES                       
*-----------------------------------------------------------------              
SPREP    XC    SPECREP,SPECREP                                                  
         XC    KEY,KEY                                                          
         OC    PIMSREP,PIMSREP                                                  
         BZ    PAYREP                                                           
         XC    REPNAME,REPNAME                                                  
         XC    REPADR1,REPADR1                                                  
         XC    REPADR2,REPADR2                                                  
         LA    R2,CHKREPNH                                                      
         MVC   8(L'CHKREPN,R2),PIMSREP                                          
         MVI   5(R2),L'CHKREPN                                                  
         NI    4(R2),X'08'                                                      
         MVC   SPECREP,PIMSREP                                                  
         MVC   REPCODE,PIMSREP                                                  
         GOTO1 VALIREP                                                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         USING PREPELEM,R6                                                      
         MVC   REPNAME,PREPNAME                                                 
         MVC   REPADR1,PREPLIN1                                                 
         MVC   REPADR2,PREPLIN2                                                 
         B     VKX                 DON'T NEED PAY REP IF HAVE SPEC REP          
*-----------------------------------------------------------------              
*         IF NO SPECIAL REP CHECK FOR PAYING REP                                
*-----------------------------------------------------------------              
PAYREP   DS    0H                  GET PAYING REP INFO                          
         OC    PUBPYREP,PUBPYREP                                                
*        BZ    VKX                                                              
         BZ    PAYADR                                                           
         CLC   =X'40404040',PUBPYREP                                            
*        BE    VKX                                                              
         BE    PAYADR                                                           
         XC    REPCODE,REPCODE                                                  
         XC    REPNAME,REPNAME                                                  
         XC    REPADR1,REPADR1                                                  
         XC    REPADR2,REPADR2                                                  
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING PREPKEY,R6                                                       
         MVC   PREPKAGY,AGENCY                                                  
         MVC   PREPKMED,QMED                                                    
         MVI   PREPKRCD,X'11'                                                   
         MVC   PREPKREP,PUBPYREP                                                
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'PREPKEY),KEYSAVE                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   REPCODE,PREPKREP                                                 
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PREPREC,R6                                                       
         MVI   ELCODE,X'11'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PREPELEM,R6                                                      
         MVC   REPNAME,PREPNAME                                                 
         MVC   REPADR1,PREPLIN1                                                 
         MVC   REPADR2,PREPLIN2                                                 
         B     VKX                                                              
*-----------------------------------------------------------------              
*         IF NO PAYING REP CHECK FOR LOWER ADDRESS                              
*-----------------------------------------------------------------              
PAYADR   DS    0H                                                               
         BAS   RE,GTPUBADR                                                      
*                                                                               
*-----------------------------------------------------------------              
*         FORMAT PUB NUMBER                                                     
*-----------------------------------------------------------------              
VKX      DS    0H                                                               
         XC    PUBEXP,PUBEXP                                                    
         XC    PUBNUMH,PUBNUMH                                                  
         GOTO1 VPUBEDIT,DMCB,(8,KPUB),(C'S',PUBEXP)                             
         MVC   PUBNUMH,PUBEXP                                                   
         MVC   PUBINFO,PUBNM                                                    
         CLI   ALLZONE,C'Y'                                                     
         BNE   CR                                                               
         MVC   PUBNUMH+8(7),=C',ALL   '                                         
         B     CR                  CORRECTED INVOICE DETAILS                    
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
*           CORRECTED INVOICE DETAILS  -- SAVE IN TABLE                         
*********************************************************************           
CR       DS    0H                                                               
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
CR100NX1 XC    MINEKEY,MINEKEY     RESTORE SEQUENCE                             
         MVC   MINEKEY(L'MINEKSV),MINEKSV                                       
         BAS   RE,MINIOHI                                                       
         BNE   CRX                                                              
CR100NX2 BAS   RE,MINIOSEQ                                                      
         BNE   CRX                                                              
         MVC   MINEKEY(L'MINEKSV),MINEKSV                                       
*                                                                               
CR100LP  L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
*                                                                               
         CLI   PIMDTLEL,PIMDTLEQ   DETAIL ELEMENT STILL?                        
         BNE   CRX                 NO, LEAVE                                    
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ   SAME HEADER SEQ #                            
         BNE   CRX                 NO                                           
*                                                                               
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2                                   
         MVC   MINEKSV,MINEKEY     SAVE PLACE IN X'20' SEQUENCE                 
*                                                                               
         CLC   PIMIDATE,INVSTDT    BEFORE PERIOD START?                         
         BL    CR100NX2            YES, NEXT                                    
         CLC   PIMIDATE,INVENDDT   AFTER PERIOD END?                            
         BH    CR100NX2            YES,NEXT                                     
*                                                                               
         CLI   PIMBLINE,0          UNMATCHED?                                   
         BE    CR100NX2            YES --  ONLY WANT CORRECTIONS                
*                                                                               
         TM    PIMDSTAT,X'10'      MATCHED?                                     
         BNZ   CR100NX2            YES, NOTHING TO SAVE                         
*-----------------------------------------------------------------              
*                     GOT ONE                                                   
*-----------------------------------------------------------------              
         MVC   CORRLINE,PIMBLINE   SAVE TABLE ENTRY                             
         MVC   CORRDATE,PIMIDATE                                                
         MVC   CORREST,PIMIEST                                                  
         MVC   CORBDATE,PIMBDATE                                                
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
         XC    CORRPRD,CORRPRD                                                  
         CLC   =C'***',CHKPRD      PRODUCT VARIOUS ?                            
         BNE   CR110               NO                                           
         MVC   CORRPRD,PIMSPRD                                                  
*                                                                               
*-----------------------------------------------------------------              
*                     CHECK FOR COMMENTS                                        
*-----------------------------------------------------------------              
CR110    TM    PIMDSTAT,X'04'      TEST FOR COMMENTS                            
         BNZ   CR120                                                            
         LA    R3,L'CORRTBL(R3)                                                 
         C     R3,EOTABLE                                                       
         BL    CR100NX2                                                         
         B     CRX                                                              
*                                                                               
CR120    XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,X'30'       COMMENT ELEMENT                              
         MVC   MINEKEY+1(L'PIMDTLS1),PIMDTLS1   HEADER SEQ NUM                  
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2   DETAIL SEQ NUM                  
         MVC   DTLSEQN,PIMDTLS2                                                 
         BAS   RE,MINIOHI                                                       
         BNE   CR150                                                            
         B     CR130                                                            
*                                                                               
CR130    L     R6,MINELEM                                                       
         USING PIMCOMEL,R6                                                      
         CLI   PIMCOMEL,PIMCOMEQ   COMMENT ELEMENT?                             
         BNE   CR150                                                            
         CLC   PIMCOMS1,LSTHDRSQ   SAME HEADER?                                 
         BNE   CR150                                                            
         CLC   PIMCOMS2,DTLSEQN    SAME DETAIL SEQ NUM?                         
         BNE   CR150                                                            
         MVC   CORRCMDT,PIMCOMS2   SAVE DETAIL SEQ NUM                          
*                                                                               
CR150    LA    R3,L'CORRTBL(R3)                                                 
         C     R3,EOTABLE                                                       
         BL    CR100NX1                                                         
         B     CRX                                                              
*                                                                               
CRX      MVI   0(R3),X'FF'                                                      
         B     BLDKEY                                                           
         DROP  R6,R3                                                            
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE BUILDS THE KEY  AND CALLS THE PRINT ENGINE OVERLAY               
***********************************************************************         
BLDKEY   DS    0H                                                               
*                                  TO SET UP AS REPORT                          
         MVI   WHEN,X'40'          SET REPORT TO NOW                            
         MVI   TWAWHEN,0                                                        
         MVC   REMUSER,SPACES      SET UP INITIALS                              
         CLI   CHKINITH+5,0        SHOULD DEFINATELY BE SOMETHING HERE          
         BE    GETOUT                                                           
         ZIC   R1,CHKINITH+5                                                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   REMUSER(0),CHKINIT                                               
         GOTO1 OPENPQ              OPENS PRINT QUEUE                            
*                                                                               
PRTB20   DS    0H                                                               
         XC    KEY,KEY             SET UP KEY TO READ BUYS                      
         LA    R3,KEY              SET KEY TO SAME YEAR AND                     
         USING PBUYKEY,R3              MONTH AS THE INVOICE                     
         MVC   PBUYKAGY,AGENCY                                                  
         MVC   PBUYKMED,QMED                                                    
         MVI   PBUYKRCD,X'20'                                                   
         MVC   PBUYKCLT,QCLT                                                    
         MVC   PBUYKPRD,QPRD                                                    
         CLC   =C'***',CHKPRD                                                   
         BNE   *+10                                                             
         XC    PBUYKPRD,PBUYKPRD                                                
         CLI   ALLZONE,C'Y'                                                     
         BNE   FULLPUB                                                          
         MVC   PBUYKPUB(4),BPUB                                                 
         B     OK07                                                             
FULLPUB  MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKDAT(L'INVSTDT),INVSTDT    USE INVOICE START DATE            
         DROP  R3                                                               
*-----------------------------------------------------------------              
*             CALL PPMAT07 PRINT ENGINE OVERLAY                                 
*-----------------------------------------------------------------              
OK07     DS    0H                                                               
         MVC   QPRD,CHKPRD                                                      
         GOTO1 CALLOV,DMCB,(X'07',0),(0,0)                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         L     RF,DMCB                                                          
         GOTO1 (RF),DMCB,(RC),(RA),(R9),(R8),(R5),(R4)                          
*-----------------------------------------------------------------              
*             CLOSE THE PRINT QUEUE AND GET OUT                                 
*-----------------------------------------------------------------              
         DS    0H                                                               
         MVI   SPMODE,X'FF'        CLOSES PRINT QUEUE                           
         GOTO1 SPOOL,DMCB,(R8)                                                  
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE PUB PAYING ADDRESS, WHAT A BIG PAIN IN THE BUTT         
***********************************************************************         
GTPUBADR NTR1                                                                   
         MVI   RDUPDATE,C'N'                                                    
         L     R6,AIO3                                                          
         ST    R6,AIO                                                           
         XC    REPCODE,REPCODE                                                  
         XC    SPECREP,SPECREP                                                  
         XC    REPCODE,REPCODE                                                  
         XC    REPNAME,REPNAME                                                  
         XC    REPADR1,REPADR1                                                  
         XC    REPADR2,REPADR2                                                  
*                                                                               
         CLI   ALLZONE,C'Y'                                                     
         BNE   GPA4                                                             
         MVC   BPUB+4(2),=X'FFFF'                                               
GPA4     DS    0H                                                               
*----------------------------------------------------------                     
*                READ PUB REC                                                   
*----------------------------------------------------------                     
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
         BE    GPA2                                                             
         B     GPAX                                                             
*                                                                               
         DROP  R3                                                               
*                                                                               
GPA2     DS    0H                                                               
         MVC   FILENAME,=CL8'PUBFILE '                                          
         GOTO1 GETREC                                                           
******   L     R6,AIO                                                           
******   USING PUBRECD,R6                                                       
*******  GOTO1 VPUBFLT,DMCB,(R6),PUBINFO                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PUBNAMEL,R6                                                      
         MVC   REPNAME,=CL30'PAY PUB DIRECT'                                    
         MVC   REPADR1,PUBLINE1                                                 
         MVC   REPADR2,PUBLINE2                                                 
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
*****P4A      MVC   REPNAME,PUBAONAM                                            
*****    MVC   REPADR1,PUBAOLN1                                                 
*****    MVC   REPADR2,PUBAOLN2                                                 
*****    MVC   FULL,PUBAOFF                                                     
*                                                                               
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
         B     XIT                                                              
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
         BE    XIT                                                              
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
         CLI   MINERR,MINESNF      RETURN 'NO' SET DOESN'T EXIST                
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
         CLI   MINERR,MINESNF      RETURN 'NO' SET DOESN'T EXIST                
         BE    NO                                                               
         CLI   MINERR,MINERNF      RETURN 'NO' IF RECORD NOT FOUND              
         BE    NO                                                               
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE INVOICE HEADER BASED ON THE INVOICE NUMBER.             
***********************************************************************         
GTHDR    NTR1                                                                   
         XC    MINEKEY,MINEKEY     READ THE INVOICE HEADER                      
         MVI   MINEKEY,PIMHDREQ                                                 
         MVC   MINEKEY+1(1),MYHDRSQ                                             
         BAS   RE,MINIOHI                                                       
         BNE   GHDRNO                                                           
*                                                                               
GHDR10   L     R6,MINELEM                                                       
         USING PIMHDREL,R6                                                      
         CLC   PIMHDRSQ,MYHDRSQ                                                 
         BE    GHDRYES                                                          
*                                                                               
GHDRNO   B     NO                                                               
*                                                                               
GHDRYES  MVC   LSTHDRSQ,PIMHDRSQ   SAVE LAST HEADER SEQUENCE USED               
         B     YES                                                              
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
*                ERROR MESSAGES AND LEFTOVERS                                   
***********************************************************************         
RELO     DS    A                                                                
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
GETOUT   MVI   GERROR1,ERNOPUSH                                                 
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
* FATIOB                                                                        
* SCREENS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATEBD          (OUR CHECK SCREEN FOR NEWSPAPER)             
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATDBD          (OUR CHECK SCREEN FOR MAGAZINE)              
         EJECT                                                                  
* DDGENTWA                                                                      
* DDPERVALD                                                                     
* DDMINBLK                                                                      
* PUBGENEL                                                                      
* DDBIGBOX                                                                      
* AND MORE ...                                                                  
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDMINBLK                                                       
       ++INCLUDE DDBIGBOX                                                       
       ++INCLUDE PPMATWORKD        (SYSTEM AREAS)                               
         EJECT                                                                  
PUBRECD DSECT                                                                   
       ++INCLUDE PUBREC                                                         
       ++INCLUDE PUBGENEL                                                       
       ++INCLUDE PUBREPEL                                                       
       ++INCLUDE PUBNAMEL                                                       
       ++INCLUDE PUBAOVEL                                                       
PGETADRD DSECT                                                                  
       ++INCLUDE PPGETADRD                                                      
         EJECT                                                                  
PBUYRECD DSECT                                                                  
       ++INCLUDE BUYDSECTS                                                      
         EJECT                                                                  
       ++INCLUDE PREPREC                                                        
         EJECT                                                                  
       ++INCLUDE PPMATPRTD                                                      
*******************                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPMAT06   01/02/06'                                      
         END                                                                    
