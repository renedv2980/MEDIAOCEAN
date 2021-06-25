*          DATA SET PPMAT05    AT LEVEL 059 AS OF 01/02/06                      
*PHASE T40205A                                                                  
***********************************************************************         
*   CHANGE LOG                                                                  
*                                                                               
* SMYE 04/19/04 RELINKED AND LOADED TO USE ENLARGED CRTABLE                     
*               IN PPMATPRTD                                                    
*                                                                               
* SMYE 03/02    FIX BUG FOR PRODUCT VARIOUS (***) - WAS                         
*               ONLY TESTING FOR BUYDATE AND BUYLINE TO DISTINGUISH             
*               DISCREPANCIES, THUS IF DIFFERENT PRODUCTS HAD THE SAME          
*               BUYDATE, THE DISCREPANCY ENTERED FOR ONE PRODUCT                
*               WOULD BE TOTALLED FOR EVERY PRODUCT WITH THIS DATE              
*                                                                               
*                                                                               
*                                                                               
*  TITLE: T40205 - INVOICE MATCH TOTALS SCREEN -- PF10                          
*                                                                               
*  CALLED FROM: PRINT INVOICE CONTROLLER (T40200), WHICH CALLS                  
*               DDGENCON (T00A30) WHICH CALLS PPMAT02 (T40202)                  
*               WHICH CALLS THIS.                                               
*                                                                               
*  SCREENS:     PPMATCC (T402CC)-- GET INFO FROM CHECK SCREEN (T402FB)          
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
T40205   TITLE 'PPMAT05 - PRINT INVOICE MATCH -- PF10 TOTALS SCREEN'            
T40205   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**0205**,R7,RR=R3                                              
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
         EJECT                                                                  
*----------------------------------------------------------------------         
*  CALL SCREEN AND CLEAR STORAGE                                                
*----------------------------------------------------------------------         
VK       DS    0H                                                               
         LA    R1,CHKTAGH                                                       
         ST    R1,DMCB                                                          
         MVI   DMCB,X'CC'                                                       
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   VK05                                                             
         DC    H'0'                                                             
*                                                                               
VK05     DS    0H                                                               
         L     R2,ATWA             MUST SET INDICTOR TO XMIT ALL FIELDS         
         LA    R2,64(R2)               OR SCREEN WILL BE MESSED UP              
         CLI   0(R2),0                                                          
         BE    *+16                FIND END OF TWA                              
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         B     *-16                                                             
         MVC   1(2,R2),=X'0101'    SET INDICATOR TO XMIT ALL FIELDS             
*                                                                               
         LA    R0,MYSTART          CLEARS MY WORK STORAGE                       
         LA    R1,MYLENGTH                                                      
         LR    RE,R0                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
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
         CLC   =C'***',8(R2)                                                    
         BE    VK10                                                             
         GOTO1 VALIPRD                                                          
*-----------------------------------------------------------------              
*                  VALIDATE THE PUBLICATION                                     
*-----------------------------------------------------------------              
VK10     MVC   SVBPUB,BPUB                                                      
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
         EDIT  (B2,BEST),(3,HEST),FILL=0                                        
*-----------------------------------------------------------------              
*         IF SPECIAL REP IN HEADER GET INFO FOR HEADLINES                       
*-----------------------------------------------------------------              
SPREP    XC    SPECREP,SPECREP                                                  
         XC    KEY,KEY                                                          
         OC    PIMSREP,PIMSREP                                                  
         BZ    PAYREP                                                           
         MVC   SPECREP,PIMSREP                                                  
         MVC   REPCODE,PIMSREP                                                  
         B     VKX                 DON'T NEED PAY REP IF HAVE SPEC REP          
*-----------------------------------------------------------------              
*         IF NO SPECIAL REP CHECK FOR PAYING REP                                
*-----------------------------------------------------------------              
PAYREP   DS    0H                  GET PAYING REP INFO                          
         OC    PUBPYREP,PUBPYREP                                                
         BZ    VKX                                                              
         CLC   =X'40404040',PUBPYREP                                            
         BE    VKX                                                              
         XC    REPCODE,REPCODE                                                  
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
VKX      B     CR                  CORRECTED INVOICE DETAILS                    
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
         MVC   CORRPRD,PIMSPRD                                                  
*                                                                               
         LA    R3,L'CORRTBL(R3)                                                 
         C     R3,EOTABLE                                                       
         BL    CR100NX2                                                         
         B     CRX                                                              
*                                                                               
CRX      MVI   0(R3),X'FF'                                                      
         B     BLDKEY                                                           
         DROP  R6,R3                                                            
         EJECT                                                                  
*                                                                               
BLDKEY   DS    0H                                                               
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
         CLC   =C'***',CHKPRD      PRD VARIOUS -> TOTAL FOR ALL PRDS            
         BNE   *+10                                                             
         XC    PBUYKPRD,PBUYKPRD                                                
         CLI   ALLZONE,C'Y'                                                     
         BNE   FULLPUB                                                          
         MVC   PBUYKPUB(4),BPUB                                                 
         B     PRTINIT                                                          
FULLPUB  MVC   PBUYKPUB(L'BPUB),BPUB                                            
         MVC   PBUYKDAT(L'INVSTDT),INVSTDT    USE INVOICE START DATE            
         DROP  R3                                                               
*--------------------------------------------------------------                 
*         INITIALIZE SOME FIELDS                                                
*--------------------------------------------------------------                 
PRTINIT  ZAP   ORDGR,=P'0'         GROSS TOTAL FOR ALL ORDERED                  
         ZAP   BUYGRTOT,=P'0'      GROSS TOTAL FOR ALL BUYS                     
         ZAP   CRINVGR,=P'0'       GROSS TOTAL FOR CORRECTIONS                  
         ZAP   UMINVGR,=P'0'       GROSS TOTAL FOR UNMATCHED INVOICES           
         ZAP   UMBUYGR,=P'0'       GROSS TOTAL FOR UNMATCHED BUYS               
         ZAP   INVGR,=P'0'         GROSS TOTAL FOR ALL INVOICE ITEMS            
         ZAP   MINVGR,=P'0'        GROSS TOTAL FOR MATCHED INVOICES             
         XC    MINVNUM,MINVNUM     COUNTS OF ITEMS                              
         XC    CINVNUM,CINVNUM                                                  
         XC    UMBUYNUM,UMBUYNUM                                                
         XC    UMINVNUM,UMINVNUM                                                
         XC    ORDNUM,ORDNUM                                                    
         XC    MNET,MNET           NET TOTALS                                   
         XC    CNET,CNET                                                        
         XC    ANET,ANET                                                        
         XC    ORDNET,ORDNET                                                    
         XC    UMBUYNET,UMBUYNET                                                
         XC    UMINVNET,UMINVNET                                                
         XC    MCD,MCD             CASH DISC TOTALS                             
         XC    CCD,CCD                                                          
         XC    ORDCD,ORDCD                                                      
         XC    UMBUYCD,UMBUYCD                                                  
         XC    UMINVCD,UMINVCD                                                  
         XC    ACSHDSC,ACSHDSC                                                  
         XC    MTAX,MTAX           TAX TOTALS                                   
         XC    CTAX,CTAX                                                        
         XC    ORDTAX,ORDTAX                                                    
         XC    UMBUYTAX,UMBUYTAX                                                
         XC    UMINVTAX,UMINVTAX                                                
         XC    MGSTAX,MGSTAX           TAX TOTALS                               
         XC    CGSTAX,CGSTAX                                                    
         XC    UMBGSTAX,UMBGSTAX                                                
         XC    BUYGSTAX,BUYGSTAX                                                
         XC    UMIGSTAX,UMIGSTAX                                                
         XC    ORDGSTAX,ORDGSTAX                                                
*----------------------------------------------------------                     
*          WHAT TYPE OF MEDIA?                                                  
*----------------------------------------------------------                     
         XC    MEDTYPE,MEDTYPE     WHAT MEDIA TYPE                              
         CLI   QMED,C'N'                                                        
         BNE   *+8                                                              
         MVI   MEDTYPE,C'N'                                                     
         CLI   QMED,C'S'           SUPPLEMENTS ARE LIKE NEWS PAPER              
         BNE   *+8                                                              
         MVI   MEDTYPE,C'N'                                                     
*----------------------------------------------------------                     
*          GET BUYS AND TOTALS THEM UP                                          
*----------------------------------------------------------                     
PRTBHI   DS    0H                                                               
         GOTO1 HIGH                                                             
         CLI   DMCB+8,0                                                         
         BE    PRTBLP                                                           
         DC    H'0'                                                             
*                                                                               
PRTBNXTH DS    0H                                                               
         GOTO1 HIGH                                                             
PRTBNXTS GOTO1 SEQ                                                              
         CLI   DMCB+8,0                                                         
         BE    PRTBLP                                                           
         DC    H'0'                                                             
*                                                                               
PRTBLP   DS    0H                                                               
         CLC   KEY(PBUYKPRD-PBUYKEY),KEYSAVE   SAME BASICS?                     
         BNE   PRTUNMAT            PRINT UNMATCHED ITEMS                        
         LA    R3,KEY                                                           
         USING PBUYKEY,R3                                                       
*                                                                               
         CLC   =C'***',CHKPRD                                                   
         BE    *+14                                                             
         CLC   PBUYKPRD,QPRD                                                    
         BNE   PRTUNMAT                                                         
*                                                                               
         CLC   PBUYKPUB(4),BPUB        SAME PUB (ACROSS ZONE,EDT)               
         BE    PRTB05                                                           
         BL    PRTBNXTS                                                         
*                                                                               
         CLC   =C'***',CHKPRD                                                   
         BNE   PRTUNMAT                                                         
         MVI   PBUYKPUB,X'FF'                                                   
         B     PRTBHI                                                           
*                                                                               
PRTB05   CLI   ALLZONE,C'Y'                                                     
         BE    *+14                                                             
         CLC   PBUYKPUB(6),BPUB                                                 
         BNE   PRTBNXTS                                                         
         XC    PUBEXP,PUBEXP                                                    
         GOTO1 VPUBEDIT,DMCB,(8,PBUYKPUB),(C'S',PUBEXP)                         
         B     BUYOK                                                            
*                                                                               
BUYOK    DS    0H                                                               
         CLC   PBUYKDAT,INVSTDT    WITHIN PERIOD?                               
         BNL   PRTB30                                                           
         CLI   ALLZONE,C'Y'                                                     
         BE    *+14                                                             
         CLC   =C'***',CHKPRD                                                   
         BNE   PRTUNMAT                                                         
         MVC   PBUYKDAT,INVSTDT                                                 
         XC    PBUYKEST(PBUYKLIN+L'PBUYKLIN-PBUYKEST),PBUYKEST                  
         B     PRTBHI                                                           
*                                                                               
PRTB30   CLC   PBUYKDAT,INVENDDT                                                
         BNH   PRTB35                                                           
         CLI   ALLZONE,C'Y'                                                     
         BNE   *+12                                                             
         MVI   PBUYKDAT,X'FF'                                                   
         B     PRTBHI                                                           
         CLC   =C'***',CHKPRD                                                   
         BNE   PRTUNMAT                                                         
         MVI   PBUYKPUB,X'FF'                                                   
         B     PRTBHI                                                           
*                                                                               
PRTB35   OC    BEST,BEST           NO ESTIMATE GIVEN?                           
         BZ    PRTB100             NONE                                         
         CLC   PBUYKEST,BEST       DOES GIVEN EST MATCH EST IN KEY?             
         BNE   PRTBNXTS            NO, GET NEXT BUY RECORD                      
*                                                                               
PRTB100  OC    PBUYKACT,PBUYKACT   IF SOMETHING HERE  (ACTIVE PRODUCT)          
         BNZ   PRTBNXTS            THEN SKIP THE RECORD   (ASK MEL)             
         DROP  R3                                                               
*                                                                               
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
*                                                                               
         CLI   PBDBFD,C'T'         IGNORE TEST BUYS                             
         BE    PRTBNXTS                                                         
*                                                                               
         OC    SPECREP,SPECREP     SPECIAL REP GIVEN                            
         BZ    PRTB102                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,X'80'        SPECIAL REPS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PRTBNXTS                                                         
         USING PBSREPEL,R6                                                      
         CLC   SPECREP,PBSREP                                                   
         BNE   PRTBNXTS                                                         
         B     PRTB103                                                          
*                                                                               
PRTB102  DS    0H                                                               
         L     R6,AIO              IF NO SPEC REP GIVEN                         
         MVI   ELCODE,X'80'        SPECIAL REPS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   PRTB103                                                          
         USING PBSREPEL,R6                                                      
         OC    PBSREP,PBSREP       THEN SKIP IF THERE IS A SPEC REP             
         BNZ   PRTBNXTS                                                         
         B     PRTB103                                                          
*                                                                               
PRTB103  L     R6,AIO              RESET                                        
         USING PBUYREC,R6                                                       
         CLI   MEDTYPE,C'N'        NEWSPAPER TYPE?                              
         BNE   PRTB211                                                          
*--------------------------------------------------------------------           
*       GROSS, NET, CD, AND PAY/BILL DATES  (NEWS ONLY)                         
*--------------------------------------------------------------------           
PRTB161  CLC   =C'***',CHKPRD                                                   
         BNE   *+10                                                             
         MVC   QPRD,PBUYKPRD                                                    
         GOTO1 GETINS,DMCB,AIO,GETINSA,QPRD,INVSTDT,=C'GST'                     
         MVC   QPRD,CHKPRD                                                      
         L     R3,16(R1)                                                        
         USING GVALUES,R3                                                       
         XC    BUYGSTAX,BUYGSTAX                                                
         XC    BUYTAX,BUYTAX                                                    
         MVC   BUYGSTAX,GSTTAX                                                  
*                                                                               
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
         MVC   BUYTAX,TAX          FOR TAX TOTAL                                
*                                                                               
         EDIT  (B4,GROSS),(11,ANNSPACE),FILL=0  JUST TO GET IN EBCIDIC          
         PACK  WKSPACE(8),ANNSPACE(11)                                          
         ZAP   DUMMY(8),WKSPACE(8)                                              
         AP    ORDGR(8),WKSPACE(8)                                              
         L     R1,GROSS                                    FOR NOW              
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         MVC   TEMPBNET,FULL                                                    
*                                  CASH DISCOUNT                                
         MVC   TEMPBCD,CSHDSC                                                   
*                                                                               
         B     PRTB300                                                          
         DROP  R3                                                               
*--------------------------------------------------------------------           
*        GROSS, NET, CD, PAY/BILL DATES (MAGS ONLY)                             
*--------------------------------------------------------------------           
PRTB211  CLC   =C'***',CHKPRD                                                   
         BNE   *+10                                                             
         MVC   QPRD,PBUYKPRD                                                    
         GOTO1 GETINS,DMCB,AIO,GETINSA,QPRD,INVSTDT,=C'GST'                     
         MVC   QPRD,CHKPRD                                                      
         XC    BUYGSTAX,BUYGSTAX                                                
         XC    BUYTAX,BUYTAX                                                    
         L     R3,16(R1)                                                        
         USING GVALUES,R3                                                       
         MVC   BUYGSTAX,GSTTAX                                                  
         LA    R3,GETINSA                                                       
         USING PVALUES,R3                                                       
*                                                                               
         MVC   BUYTAX,TAX          FOR TAX TOTAL                                
*                                                                               
         EDIT  (B4,GROSS),(11,ANNSPACE),FILL=0                                  
         PACK  WKSPACE(8),ANNSPACE(11)                                          
         ZAP   DUMMY(8),WKSPACE(8)                                              
         AP    ORDGR(8),WKSPACE(8)                                              
         L     R1,GROSS                                    FOR NOW              
         L     R0,AGYCOM                                                        
         SR    R1,R0                                                            
         ST    R1,FULL                                                          
         MVC   TEMPBNET,FULL                                                    
*                                  CASH DISCOUNT                                
         MVC   TEMPBCD,CSHDSC                                                   
*                                                                               
*                                                                               
         B     PRTB300                                                          
         DROP  R3,R6                                                            
********************************************************************            
         EJECT                                                                  
*--------------------------------------------------------------------           
*        INDICATE IF MATCHED (BOTH NEWS AND MAGS)                               
*--------------------------------------------------------------------           
PRTB300  DS    0H                                                               
*                                  KEEP ORDERED TOTALS                          
         L     R1,ORDNUM           ORDERED COUNT                                
         LA    R1,1(R1)                                                         
         ST    R1,ORDNUM                                                        
*                                                                               
         L     R1,ORDCD              ORDERED CD  TOTAL                          
         A     R1,TEMPBCD                                                       
         ST    R1,ORDCD                                                         
*                                                                               
         L     R1,ORDNET             ORDERED NET TOTAL                          
         A     R1,TEMPBNET                                                      
         ST    R1,ORDNET                                                        
*                                                                               
         L     R1,ORDTAX             ORDERED TAX TOTAL                          
         A     R1,BUYTAX                                                        
         ST    R1,ORDTAX                                                        
*                                                                               
         L     R1,ORDGSTAX             ORDERED GST TAX TOTAL                    
         A     R1,BUYGSTAX                                                      
         ST    R1,ORDGSTAX                                                      
*                                                                               
         L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         TM    PBDSTAT,X'40'       IF MATCHED                                   
         BZ    PRTC400             NOT , CHECK FOR CORRECTION                   
*                                                                               
         L     R1,MINVNUM          MATCHED INVOICE COUNT                        
         LA    R1,1(R1)                                                         
         ST    R1,MINVNUM                                                       
*                                                                               
         L     R1,MCD              MATCHED CD  TOTAL                            
         A     R1,TEMPBCD                                                       
         ST    R1,MCD                                                           
*                                                                               
         L     R1,MNET             MATCHED NET TOTAL                            
         A     R1,TEMPBNET                                                      
         ST    R1,MNET                                                          
*                                                                               
         L     R1,MTAX             MATCHED TAX TOTAL                            
         A     R1,BUYTAX                                                        
         ST    R1,MTAX                                                          
*                                                                               
         L     R1,MGSTAX             MATCHED GST TAX TOTAL                      
         A     R1,BUYGSTAX                                                      
         ST    R1,MGSTAX                                                        
*                                                                               
         AP    MINVGR(8),DUMMY(8)  MATCHED GROSS TOTAL                          
*                                                                               
         B     PRTBANN             NO CORRECTIONS IF MATCHED                    
*                                                                               
********************************************************************            
         EJECT                                                                  
*--------------------------------------------------------------------           
*        UNMATCHED BUY, CHECK FOR ASSOCIATED CORRECTION                         
*--------------------------------------------------------------------           
PRTC400  DS    0H                                                               
         MVI   GOTCORR,C'N'        INIT                                         
*                                                                               
         LA    R2,CRTABLE                                                       
         USING CORRTBLD,R2                                                      
*                                                                               
PRTC405  L     R6,AIO                                                           
         USING PBUYREC,R6                                                       
         CLC   PBUYKLIN,CORRLINE   MUST HAVE SAME BUYLINE                       
         BNE   PRTC490                                                          
         CLC   PBUYKDAT,CORBDATE   SAME DATE                                    
         BNE   PRTC490                                                          
         CLC   PBUYKEST,CORBEST    AND SAME ESTIMATE                            
         BNE   PRTC490                                                          
         CLC   PBUYKZON,CORRZONE   AND SAME ZONE                                
         BNE   PRTC490                                                          
         CLC   PBUYKEDT,CORREDTN   AND SAME EDITION                             
         BNE   PRTC490                                                          
         CLC   =C'***',CHKPRD      PRODUCT VARIOUS ?                            
         BNE   PRTC406             NO                                           
         CLC   PBUYKPRD,CORRPRD    MUST HAVE SAME PRODUCT CODE                  
         BNE   PRTC490                                                          
*                                  PRINT CORRESPONDING CORRECTION               
PRTC406  MVC   PR2NIND,=CL2'->'                                                 
*                                                                               
         MVC   VALUIND,PBDUIND     SAVE BUY VALUES                              
         ZAP   VALUNITS,PBDUNITS                                                
         ZAP   VALCLMS,PBDCLMS                                                  
         MVC   VALCOSTY,PBDCOSTY                                                
         ZAP   VALCOST,PBDCOS                                                   
         MVC   VALCL,PBDCL                                                      
         ZAP   VALPRCOS,PBDPRCOS                                                
         MVC   VALSPACE,PBDSPACE                                                
         CLI   QMED,C'O'           OUTDOORS?                                    
         BNE   PRTC409                                                          
         CP    PBDSHOW,=P'0'                                                    
         BNE   PRTC409                                                          
         CP    PBDREG,=P'0'                                                     
         BNE   PRTC409                                                          
         CP    PBDILLUM,=P'0'                                                   
         BNE   PRTC409                                                          
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'66'                                                     
         BAS   RE,GETEL                                                         
         BE    PRTC408                                                          
*                                                                               
         USING PCOMELEM,R6                                                      
PRTC408  ZIC   R1,PCOMELEM+1                                                    
         SH    R1,=H'2'                                                         
         CH    R1,=Y(L'VALSPACE)                                                
         BNH   *+14                                                             
         MVC   VALSPACE,PCOMELEM+2    SEE PPBUY05, LABEL FMTCOM                 
         B     PRTC409                                                          
*                                                                               
         CH    R1,=H'2'                                                         
         BNH   PRTC409                                                          
         XC    VALSPACE,VALSPACE                                                
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   VALSPACE(0),PCOMELEM+2                                           
         B     PRTC409                                                          
*                                                                               
         DROP  R6                                                               
         EJECT                                                                  
*-------------------------------------------------------------------            
*           GOT ONE, PRINT IT OUT                                               
*-------------------------------------------------------------------            
PRTC409  DS    0H                                                               
         GOTO1 =A(DCORR),DMCB,(RC),(RA),(R9),(R8),(R5),(R4),RR=RELO             
         MVI   GOTCORR,C'Y'        FOUND A CORRECTION                           
*                                                                               
         L     R1,CNET             CORRECTED INV NET TOTAL                      
         A     R1,ANET                                                          
         ST    R1,CNET                                                          
         L     R1,CTAX             CORRECTED INV TAX TOTAL                      
         A     R1,BUYTAX                                                        
         ST    R1,CTAX                                                          
         L     R1,CGSTAX           CORRECTED INV GST TAX TOTAL                  
         A     R1,BUYGSTAX                                                      
         ST    R1,CGSTAX                                                        
         L     R1,CCD              CORRECTED INV CASH DISC TOTAL                
         A     R1,ACSHDSC                                                       
         ST    R1,CCD                                                           
         L     R1,CINVNUM          CORRECTED INVOICE COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,CINVNUM                                                       
         B     PRTBPRT                                                          
****                                                                            
PRTC490  LA    R2,L'CORRTBL(R2)    CK NEXT CORRECTION IN TABLE                  
         CLI   0(R2),X'FF'         END OF TABLE?                                
         BE    PRTBPRT             PRINT OUT LINE(S)                            
         C     R2,EOTABLE                                                       
         BNL   PRTBPRT                                                          
         B     PRTC405                                                          
*-----------------------------------------------------------------              
*                      PRINT OUT LINE(S)                                        
*-----------------------------------------------------------------              
PRTBPRT  DS    0H                                                               
*                                                                               
         CLI   GOTCORR,C'Y'        FOUND A CORRECTION                           
         BE    PRTBANN             IF NOT UPDATE TOTALS                         
         L     R1,TEMPBNET         UNMATCHED BUY NET TOTAL                      
         A     R1,UMBUYNET                                                      
         ST    R1,UMBUYNET                                                      
*                                                                               
         L     R1,UMBUYCD          UNMATCHED BUY CD  TOTAL                      
         A     R1,TEMPBCD                                                       
         ST    R1,UMBUYCD                                                       
*                                                                               
         L     R1,UMBUYTAX         UNMATCHED BUY TAX TOTAL                      
         A     R1,BUYTAX                                                        
         ST    R1,UMBUYTAX                                                      
*                                                                               
         L     R1,UMBGSTAX         UNMATCHED BUY GST TAX TOTAL                  
         A     R1,BUYGSTAX                                                      
         ST    R1,UMBGSTAX                                                      
*                                                                               
         L     R1,UMBUYNUM         UNMATCHED BUY COUNT                          
         LA    R1,1(R1)                                                         
         ST    R1,UMBUYNUM                                                      
*                                                                               
         AP    UMBUYGR(8),DUMMY(8) UNMATCHED BUY GROSS TOTAL                    
PRTBANN  DS    0H                                                               
         B     PRTBNXTS            NEXT BUY                                     
*                                                                               
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*            PRINT OUT ALL UNMATCHED INVOICE ITEMS                              
***********************************************************************         
PRTUNMAT DS    0H                                                               
*                                                                               
         GOTO1 =A(UNMAT),DMCB,(RC),(RA),(R9),(R8),(R5),(R4),RR=RELO             
*                                                                               
**********************************************************************          
*                    PRINT OUT TOTALS AND STUFF                                 
**********************************************************************          
         CVB   R1,MINVGR                                                        
         S     R1,MCD                                                           
         ST    R1,MGCD             GROSS LESS CD                                
         L     R1,MNET                                                          
         S     R1,MCD                                                           
         ST    R1,MNCD             NET LESS CD                                  
*                                                                               
         LA    R2,TOTMNUM          MATCHED NUMBER                               
         EDIT  (B4,MINVNUM),(2,0(R2)),ALIGN=RIGHT,ZERO=NOBLANK                  
*                                                                               
         LA    R2,TOTGMAT          MATCHED GROSS                                
         EDIT  (P8,MINVGR),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
         LA    R2,TOTNMAT          MATCHED NET                                  
         EDIT  (B4,MNET),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTBMAT          MATCHED GROSS LESS CD                        
         EDIT  (B4,MGCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTAMAT          MATCHED NET LESS CD                          
         EDIT  (B4,MNCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTCMAT          MATCHED CD                                   
         EDIT  (B4,MCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES              
*                                                                               
         LA    R2,TOTTMAT          MATCHED TAX                                  
         EDIT  (B4,MTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTSMAT          MATCHED GST TAX                              
         EDIT  (B4,MGSTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
*-------------------------------------------------------------------            
         CVB   R1,CRINVGR                                                       
         S     R1,CCD                                                           
         ST    R1,CGCD             GROSS LESS CD                                
         L     R1,CNET                                                          
         S     R1,CCD                                                           
         ST    R1,CNCD             NET LESS CD                                  
*                                                                               
         LA    R2,TOTCNUM          DISCREPANT NUMBER                            
         EDIT  (B4,CINVNUM),(2,0(R2)),ALIGN=RIGHT,ZERO=NOBLANK                  
*                                                                               
         LA    R2,TOTGDIS          DISCREPANT GROSS                             
         EDIT  (P8,CRINVGR),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
*                                                                               
         LA    R2,TOTNDIS          DISCREPANT NET                               
         EDIT  (B4,CNET),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTBDIS          DISCREPANT GROSS LESS CD                     
         EDIT  (B4,CGCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTADIS          DISCREPANT NET LESS CD                       
         EDIT  (B4,CNCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTCDIS          DISCREPANT CD                                
         EDIT  (B4,CCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES              
*                                                                               
         LA    R2,TOTTDIS          DISCREPANT TAX                               
         EDIT  (B4,CTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES             
*                                                                               
         LA    R2,TOTSDIS          DISCREPANT GST TAX                           
         EDIT  (B4,CGSTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
*-------------------------------------------------------------------            
         CVB   R1,UMBUYGR                                                       
         S     R1,UMBUYCD                                                       
         ST    R1,UMBUYGCD         GROSS LESS CD                                
         L     R1,UMBUYNET                                                      
         S     R1,UMBUYCD                                                       
         ST    R1,UMBUYNCD         NET LESS CD                                  
*                                                                               
         LA    R2,TOTONUM          ORDERED NOT RUN                              
         EDIT  (B4,UMBUYNUM),(2,0(R2)),ALIGN=RIGHT,ZERO=NOBLANK                 
*                                                                               
         LA    R2,TOTGORD          ORDERED NOT RUN GROSS                        
         EDIT  (P8,UMBUYGR),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
*                                                                               
         LA    R2,TOTNORD          ORDERED NOT RUN NET                          
         EDIT  (B4,UMBUYNET),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTBORD          ORDERED NOT RUN GROSS LESS CD                
         EDIT  (B4,UMBUYGCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTAORD          ORDERED NOT RUN NET LESS CD                  
         EDIT  (B4,UMBUYNCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTCORD          ORDERED NOT RUN CD                           
         EDIT  (B4,UMBUYCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
*                                                                               
         LA    R2,TOTTORD          ORDERED NOT RUN TAX                          
         EDIT  (B4,UMBUYTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTSORD          ORDERED NOT RUN GSTTAX                       
         EDIT  (B4,UMBGSTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
*--------------------------------------------------------------------           
         CVB   R1,UMINVGR                                                       
         S     R1,UMINVCD                                                       
         ST    R1,UMINVGCD         GROSS LESS CD                                
         L     R1,UMINVNET                                                      
         S     R1,UMINVCD                                                       
         ST    R1,UMINVNCD         NET LESS CD                                  
*                                                                               
         LA    R2,TOTRNUM          RUN NOT ORDERED                              
         EDIT  (B4,UMINVNUM),(2,0(R2)),ALIGN=RIGHT,ZERO=NOBLANK                 
*                                                                               
         LA    R2,TOTGNOT          RUN NOT ORDERED GROSS                        
         EDIT  (P8,UMINVGR),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
*                                                                               
         LA    R2,TOTNNOT          RUN NOT ORDERED NET                          
         EDIT  (B4,UMINVNET),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTBNOT          RUN NOT ORDERED GROSS LESS CD                
         EDIT  (B4,UMINVGCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTANOT          RUN NOT ORDERED NET LESS CD                  
         EDIT  (B4,UMINVNCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTCNOT          RUN NOT ORDERED CD                           
         EDIT  (B4,UMINVCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES          
*                                                                               
         LA    R2,TOTTNOT          RUN NOT ORDERED TAX                          
         EDIT  (B4,UMINVTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
         LA    R2,TOTSNOT          RUN NOT ORDERED GST TAX                      
         EDIT  (B4,UMIGSTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
*--------------------------------------------------------------------           
*                                                                               
         CVB   R1,ORDGR                                                         
         S     R1,ORDCD                                                         
         ST    R1,ORDGCD           GROSS LESS CD                                
         L     R1,ORDNET                                                        
         S     R1,ORDCD                                                         
         ST    R1,ORDNCD           NET LESS CD                                  
*                                                                               
         LA    R2,TOTTNUM          TOTAL ORDERED                                
         EDIT  (B4,ORDNUM),(2,0(R2)),ALIGN=RIGHT,ZERO=NOBLANK                   
*                                                                               
         LA    R2,TOTGTOT          TOTAL ORDERED GROSS                          
         EDIT  (P8,ORDGR),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES            
*                                                                               
         LA    R2,TOTNTOT          TOTAL ORDERED NET                            
         EDIT  (B4,ORDNET),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
         LA    R2,TOTBTOT          TOTAL ORDERED GROSS LESS CD                  
         EDIT  (B4,ORDGCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
         LA    R2,TOTATOT          TOTAL ORDERED NET LESS CD                    
         EDIT  (B4,ORDNCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
         LA    R2,TOTCTOT          TOTAL ORDERED CD                             
         EDIT  (B4,ORDCD),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES            
*                                                                               
         LA    R2,TOTTTOT          TOTAL ORDERED TAX                            
         EDIT  (B4,ORDTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES           
*                                                                               
         LA    R2,TOTSTOT          TOTAL ORDERED GSTTAX                         
         EDIT  (B4,ORDGSTAX),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,MINUS=YES         
*                                                                               
*---------------------------------------------------------------------          
         LA    R2,TOTITOT                                                       
         EDIT  (P8,INVGRTOT),(13,0(R2)),2,ALIGN=LEFT,FLOAT=$,          X        
               MINUS=YES                                                        
*                                                                               
         LA    R2,TOTDNUM          # OF INVOICE DETAILS                         
         L     R1,MINVNUM                                                       
         A     R1,CINVNUM                                                       
         A     R1,UMINVNUM                                                      
         ST    R1,DETNUM                                                        
         EDIT  (B4,DETNUM),(3,0(R2)),ALIGN=LEFT,ZERO=NOBLANK                    
*                                                                               
         LA    R2,TOTDTOT                                                       
         CLI   MYGRSNET,C'G'                                                    
         BNE   NETTOT                                                           
         AP    INVGR(8),MINVGR(8)                                               
         AP    INVGR(8),UMINVGR(8)                                              
         AP    INVGR(8),CRINVGR(8)                                              
         EDIT  (P8,INVGR),(13,0(R2)),2,ALIGN=LEFT,FLOAT=$,MINUS=YES             
         B     XIT                                                              
*                                                                               
NETTOT   L     R1,MNET                                                          
         A     R1,UMINVNET                                                      
         A     R1,CNET                                                          
         EDIT  (R1),(13,0(R2)),2,ALIGN=LEFT,FLOAT=$,MINUS=YES                   
         B     XIT                                                              
*                                                                               
*        EDIT  (P8,BUYGRTOT),(13,0(R2)),2,ALIGN=RIGHT,FLOAT=$,                  
*              MINUS=YES                                                        
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*                         GROSS TO NET                                          
***********************************************************************         
GRTONET  NTR1                                                                   
         ZAP   AMNETAMT,=P'100000'       NET = GROSS * (1-%AGE)                 
         SP    AMNETAMT,AMPERCTG                                                
         ZAP   AMPERCTG,AMNETAMT                                                
         MP    AMGRSAMT,AMPERCTG                                                
         DP    AMGRSAMT,=P'100000'       KEEP ALL DECIMALS INTACT               
         ZAP   AMNETAMT,AMGRSAMT(L'AMGRSAMT-4)                                  
* DO WE ROUND UP ONE?                                                           
         CP    AMGRSAMT+L'AMGRSAMT-4(4),=P'50000'                               
         BL    *+10                                                             
         AP    AMNETAMT,=P'1'       YES                                         
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE GETS THE PUB PAYING ADDRESS, WHAT A BIG PAIN IN THE BUTT         
***********************************************************************         
GTPUBADR NTR1                                                                   
         MVI   RDUPDATE,C'N'                                                    
         MVC   FILENAME,=CL8'PUBDIR  '                                          
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
         CH    R5,=H'4'                                                         
         BE    GPA2B                                                            
         CLI   SVAGPROF+13,C'0'    IF COMBINED PAYMENTS                         
         BNE   GPA4B               GO BACK AND GET BASE PUB FOR ADDR            
GPA2B    DS    0H                                                               
         MVC   FILENAME,=CL8'PUBFILE '                                          
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING PUBRECD,R6                                                       
         GOTO1 VPUBFLT,DMCB,(R6),PUBINFO                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         USING PUBNAMEL,R6                                                      
         MVC   REPNAME,=CL30'PAY PUB DIRECT'                                    
         MVC   REPADR1,PUBLINE1                                                 
         MVC   REPADR2,PUBLINE2                                                 
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
*----------------------------------------------------------------------         
*                         ERROR MESSAGES (AND STUFF)                            
*----------------------------------------------------------------------         
RELO     DS    A                                                                
MISSFLD  MVI   GERROR1,MISSING                                                  
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         B     ERREXIT                                                          
*                                                                               
ERREXIT  MVI   GMSGTYPE,C'E'                                                    
         GOTO1 MYERR                                                            
*                                                                               
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*  THIS ROUTINE DISPLAYS THE CORRECTION DETAIL                                  
*********************************************************************           
DCORR    NMOD1 0,**DCORR*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     R8,12(R1)                                                        
         L     R5,16(R1)                                                        
         L     R4,20(R1)                                                        
         USING CORRTBLD,R2                                                      
PRTC410  DS    0H                                                               
*------------------------------------------------------------------             
*          GROSS, NET AND CASH DISCOUNT (BOTH)                                  
*------------------------------------------------------------------             
*                                  CALCDTLG NEED MINELEM                        
         L     R6,MINELEM                                                       
         USING PIMDTLEL,R6                                                      
         MVC   PIMUIND,CORRUIND                                                 
         MVC   PIMUNITS,CORRUNIT                                                
         MVC   PIMCLMS,CORRCLMS                                                 
         MVC   PIMCOST,CORRCOST                                                 
         MVC   PIMPREM,CORRPREM                                                 
         MVC   PIMCLRS,CORRCLRS                                                 
         MVC   PIMDSTAT,CORRSTAT                                                
         MVC   PIMCSIND,CORRCSIN                                                
*                                  GROSS, NET, AND CASH DISCOUNT                
         XC    ANET,ANET                                                        
         XC    BUYTAX,BUYTAX                                                    
         XC    BUYGSTAX,BUYGSTAX                                                
         XC    ACSHDSC,ACSHDSC                                                  
         GOTO1 CALCDTLG,DMCB,GETINSA                                            
         LA    R1,GETINSA                                                       
         USING PVALUES,R1                                                       
*                                                                               
         MVC   BUYGSTAX,GSTTAX                                                  
         MVC   BUYTAX,TAX                                                       
         MVC   FULL,GROSS          GROSS                                        
         EDIT  (B4,FULL),(11,ANNSPACE),FILL=0                                   
         PACK  WKSPACE(8),ANNSPACE(11)                                          
         AP    CRINVGR(8),WKSPACE(8)                                            
*                                                                               
         CLI   CORRCSIN,C'S'                                                    
         BNE   *+10                                                             
         XC    AGYCOM,AGYCOM                                                    
*                                                                               
         TM    PIMDSTAT,X'40'      CASH DISC                                    
         BNZ   PRTC464                                                          
         OC    CSHDSC,CSHDSC                                                    
         BNZ   PRTC462                                                          
*                                                                               
PRTC462  DS    0H                                                               
         MVC   ACSHDSC,CSHDSC                                                   
*                                                                               
PRTC464  DS    0H                  NET                                          
         L     R3,GROSS                                                         
         L     R0,AGYCOM                                                        
         SR    R3,R0                                                            
         ST    R3,FULL                                                          
         ST    R3,ANET                                                          
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
********************************************************************            
* ROUTINE FOR PRINTING UNMATCHED ITEMS                                          
********************************************************************            
UNMAT    NMOD1 0,**UNMAT*                                                       
         L     RC,0(R1)                                                         
         L     RA,4(R1)                                                         
         L     R9,8(R1)                                                         
         L     R8,12(R1)                                                        
         L     R5,16(R1)                                                        
         L     R4,20(R1)                                                        
*----------------------------------------------------------------               
*        GET DETAILS  FROM MINIO                                                
*----------------------------------------------------------------               
PRTU500  DS    0H                                                               
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,PIMDTLEQ                                                 
         MVC   MINEKEY+1(L'LSTHDRSQ),LSTHDRSQ                                   
*                                                                               
         BAS   RE,MINIOHI                                                       
         BNE   PRTUX                                                            
         B     PRTU507                                                          
*                                                                               
PRTU505H DS    0H                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'MINEKSV),MINEKSV     RESTORE X'20' SEQUENCE            
         BAS   RE,MINIOHI                                                       
         BNE   PRTUX                                                            
PRTU505S BAS   RE,MINIOSEQ         GET NEXT MINIO ELEMENT                       
         BNE   PRTUX                                                            
         MVC   MINEKSV,MINEKEY                                                  
*                                                                               
PRTU507  L     R6,MINELEM          DETAIL ELEMENT STILL?                        
         USING PIMDTLEL,R6                                                      
         CLI   PIMDTLEL,PIMDTLEQ                                                
         BNE   PRTUX                                                            
*                                                                               
         CLC   PIMDTLS1,LSTHDRSQ   SAME HEADER SEQ NUM?                         
         BNE   PRTUX                                                            
*                                                                               
         MVC   MINEKEY+2(L'PIMDTLS2),PIMDTLS2                                   
         MVC   MINEKSV,MINEKEY     TO RESTORE X'20' SEQUENCE                    
*                                                                               
         CLC   PIMIDATE,INVSTDT   WITHIN PERIOD?                                
         BL    PRTU505S           NO, NEXT                                      
         CLC   PIMIDATE,INVENDDT                                                
         BH    PRTU505S                                                         
*                                                                               
         CLI   PIMBLINE,0          IF THERE IS A BUYLINE                        
         BE    PRTU510                                                          
*                                                                               
         B     PRTU505S            NO,NEXT                                      
*                                                                               
*----------------------------------------------------------------               
*           GROSS, NET AND CASH DISC  (BOTH)                                    
*----------------------------------------------------------------               
PRTU510  XC    ANET,ANET                                                        
         XC    BUYTAX,BUYTAX                                                    
         XC    BUYGSTAX,BUYGSTAX                                                
         XC    ACSHDSC,ACSHDSC                                                  
         GOTO1 CALCDTLG,DMCB,GETINSA                                            
         LA    R1,GETINSA                                                       
         USING PVALUES,R1                                                       
*                                                                               
         MVC   BUYGSTAX,GSTTAX                                                  
         MVC   BUYTAX,TAX                                                       
         MVC   FULL,GROSS          GROSS                                        
         EDIT  (B4,FULL),(11,ANNSPACE),FILL=0                                   
         PACK  WKSPACE(8),ANNSPACE(11)                                          
         AP    UMINVGR(8),WKSPACE(8)                                            
*                                                                               
         CLI   PIMCSIND,C'S'                                                    
         BNE   *+10                                                             
         XC    AGYCOM,AGYCOM                                                    
*                                                                               
         TM    PIMDSTAT,X'40'      CASH DISC                                    
         BNZ   PRTU564                                                          
         OC    CSHDSC,CSHDSC                                                    
         BNZ   PRTU562                                                          
*                                                                               
PRTU562  DS    0H                                                               
         MVC   ACSHDSC,CSHDSC                                                   
*                                                                               
PRTU564  DS    0H                  NET                                          
         L     R3,GROSS                                                         
         L     R0,AGYCOM                                                        
         SR    R3,R0                                                            
         ST    R3,ANET                                                          
*-------------------------------------------------------------------            
*             UNMATCHED DETAILS TOTALS                                          
*-------------------------------------------------------------------            
         L     R1,UMINVNET         UNMATCHED INV NET TOTAL                      
         A     R1,ANET                                                          
         ST    R1,UMINVNET                                                      
         L     R1,UMINVCD          UNMATCHED INV CASH DISC TOTAL                
         A     R1,ACSHDSC                                                       
         ST    R1,UMINVCD                                                       
         L     R1,UMINVTAX         UNMATCHED INV TAX TOTAL                      
         A     R1,BUYTAX                                                        
         ST    R1,UMINVTAX                                                      
         L     R1,UMINVNUM         UNMATCHED INVOICE COUNT                      
         LA    R1,1(R1)                                                         
         ST    R1,UMINVNUM                                                      
         B     PRTU505S                                                         
****                                                                            
PRTUX    DS    0H                                                               
         B     XIT                                                              
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
******************************************************************              
*               INCLUDES                                                        
******************************************************************              
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FATIOB                                                                        
* SCREENS                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
       ++INCLUDE PPMATFFD          (BASE SCREEN FOR SYSTEM)                     
         ORG   CONTAGH                                                          
       ++INCLUDE PPMATFBD          (OUR CHECK SCREEN)                           
         ORG   CHKTAGH                                                          
       ++INCLUDE PPMATCCD          (OUR TOTALS SCREEN)                          
         EJECT                                                                  
         PRINT OFF                                                              
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
**PAN#1  DC    CL21'059PPMAT05   01/02/06'                                      
         END                                                                    
