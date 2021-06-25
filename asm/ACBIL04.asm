*          DATA SET ACBIL04    AT LEVEL 047 AS OF 03/18/15                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 045076.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE T60E04A                                                                  
         TITLE 'ACBIL04 - CREATIVE BILLING - BILL/UNBILL'                       
ACBIL04  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWSD,**BIL4**,RA,R7,CLEAR=YES                               
*                                                                               
         USING GWS,R9              MAP GLOBAL WORKING STORAGE                   
         USING TWAD,R8             MAP TWA                                      
         USING LWSD,RC             MAP LOCAL WORKING STORAGE                    
*                                                                               
         GOTO1 ALDADTRN            LOAD ADDTRN AND ACBIL40                      
*                                                                               
         CLI   BILDATE,0                                                        
         BNE   *+10                THEY INPUT AN OVERRIDE                       
         MVC   BILDATE,TODAYP      ELSE USE TODAY                               
         CLI   BILMOS,0                                                         
         BNE   *+10                THEY INPUT AN OVERRIDE                       
         MVC   BILMOS,MOS          ELSE USE TODAY                               
*                                                                               
         GOTO1 AVALMLK,TRNTCLBL                                                 
         BNE   ERRXIT                                                           
*                                                                               
         CLI   ACTION,UNB                                                       
         BE    UNBL                                                             
         TM    SAVHSTAT,MANBILL    MANUAL BILL?                                 
         BO    BIL05               YES, DO NOT GET CHARGES                      
         GOTO1 ABLDCHR             BUILD TBL OF ALLOCATED CHARGES               
         CLI   CHRCNT,0                                                         
         BE    NOCHR               NO ALLOCATED CHARGES                         
*                                                                               
BIL05    GOTO1 ABILCHR             GET TOTAL OF PARAGRAPHS                      
*                                                                               
         USING CHRD,R5             MAP ALLOCATED CHARGES TABLE                  
         LA    R5,CHRTOT                                                        
         TM    SAVHSTAT,MANBILL    MANUAL BILL?                                 
         BO    BIL20               YES, DO NOT CHECK TOTALS                     
         ZAP   DUB,CHRANET         ALLOCATED AMOUNTS                            
         AP    DUB,CHRACSD                                                      
         AP    DUB,CHRANON                                                      
*                                                                               
         SP    DUB,CHRBNET         LESS PARAGRAPH AMOUNTS                       
         SP    DUB,CHRBCSD                                                      
         SP    DUB,CHRBNON                                                      
*                                                                               
         CP    DUB,=P'0'           IS THERE A DIFFERENCE?                       
         BNE   BADALLOC            YES, ERROR                                   
*                                                                               
BIL20    ZAP   DUB,CHRBGST         SAVE GST                                     
         ZAP   DUB1,CHRBPST        AND  PST                                     
         ZAP   CHRBGST,=P'0'       CLEAR GST IN CASE NOT USED                   
         ZAP   CHRBPST,=P'0'       AND PST FOR THE SAME REASON                  
         TM    RUNOPT,NEEDGST      ARE WE USING GST?                            
         BZ    BIL25               NO                                           
         ZAP   CHRBGST,DUB         RESTORE GST AMOUNT                           
         TM    SAVHSTAT,MANBILL    IS THIS A MANUAL BILL?                       
         BO    BIL22               YES, LEAVE AS PARAGRAPH TOTAL                
         ZAP   CHRBGST,CHRAGST     NO, GET CURRENT GST & COMMISSION             
         ZAP   CHRBCOM,CHRACOM                                                  
*                                                                               
BIL22    TM    RUNOPT,NEEDPST      ARE WE USING PST?                            
         BZ    BIL25               NO                                           
         ZAP   CHRBPST,DUB1        RESTORE PST AMOUNT                           
         TM    SAVHSTAT,MANBILL    IS THIS A MANUAL BILL?                       
         BO    BIL25               YES, LEAVE AS PARAGRAPH TOTAL                
         ZAP   CHRBPST,CHRAPST     NO, GET CURRENT PST                          
*                                                                               
BIL25    ZAP   XINCAM,=P'0'                                                     
         ZAP   TOTAMT,CHRBNET                                                   
         AP    TOTAMT,CHRBNON                                                   
         ZAP   TOTCOMM,CHRBCOM                                                  
         ZAP   TOTCSD,CHRBCSD                                                   
         ZAP   TOTGST,CHRBGST                                                   
         ZAP   TOTPST,CHRBPST                                                   
*                                                                               
         ZAP   TOTGRS,TOTAMT                                                    
         AP    TOTGRS,TOTCOMM                                                   
*                                                                               
         ZAP   TOTVGRS,TOTGRS                                                   
         AP    TOTVGRS,TOTGST                                                   
         SP    TOTVGRS,TOTCSD                                                   
         AP    TOTVGRS,TOTPST                                                   
*                                                                               
BIL30    GOTO1 VDATCON,DMCB,(1,BILDATE),(2,BILDATEC)                            
         MVI   OFFPOSG,0                                                        
         MVI   OFFPOSI,0                                                        
         MVC   PROKEY,SPACES                                                    
         ZIC   R1,PRODHEIR+1                                                    
         LA    R1,2(,R1)                                                        
         EXMVC R1,PROKEY,JOBKEY                                                 
*                                                                               
         XC    SALESAC,SALESAC                                                  
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'PROKEY),PROKEY                                             
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT                                                           
*                                                                               
*                                  GET SALES ANALYSIS ELEMENT                   
         GOTO1 VHELLO,DMCB,(C'G',ACCOUNT),('SANELQ',AIOAREA1),0,0               
         CLI   DMCB+12,0                                                        
         BNE   BIL50                                                            
*                                                                               
         USING SANELD,R1           MAP SALES ANALYSIS ELEMENT                   
         L     R1,DMCB+12                                                       
         MVC   SALESAC,SANCODE                                                  
         MVC   SALESNM,SANNAME                                                  
         DROP  R1                                                               
*                                                                               
         USING JOBD,R1             MAP CLIENT/PROD/JOB DATA                     
BIL50    LA    R1,JOBINFO                                                       
         MVC   OFF2,JOFFC                                                       
         MVC   XTRAMESS,SPACES                                                  
         DROP  R1,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* VALIDATE POSTING ACCOUNTS                                           *         
***********************************************************************         
                                                                                
POST     DS    0H                                                               
         TM    SAVHSTAT,MANBILL    IS THIS A MANUAL BILL ?                      
         BZ    POST02              NO, SKIP THIS                                
         GOTO1 AGETMAN             YES, GET SVTABLE SETUP                       
*                                                                               
POST02   CLI   ACTION,DRA          DRAFT?                                       
         BE    OKEND               YES, EXIT NOW                                
         MVC   KEY,SPACES                                                       
*                                                                               
         USING JOBD,R1             MAP CLIENT/PROD/JOB DATA                     
         LA    R1,JOBINFO                                                       
         MVC   KEY(15),JRECV       RECEIVABLE ACCOUNT                           
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         DROP  R1                                                               
*                                                                               
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT                                                           
         BAS   RE,POSTEST                                                       
         BNE   ERRXIT                                                           
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   RECVN,WORK                                                       
*                                                                               
*                                  INCOME ACCOUNT -                             
         MVC   KEY(15),INCOME           FROM MEDIA RECORD                       
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1      OR INPUT OVERRIDE                            
         BNE   ERRXIT                                                           
         BAS   RE,POSTEST                                                       
         BNE   ERRXIT                                                           
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   INCNAME,WORK                                                     
*                                                                               
         CLI   COSTING,C'N'                                                     
         BE    POST20              NO COST ACCOUNTING POSTINGS                  
*                                                                               
         USING JOBD,R1             MAP CLIENT/PROD/JOB DATA                     
         LA    R1,JOBINFO                                                       
         MVC   KEY(15),JCOST       CLIENT COSTING                               
         MVI   FNDX,0                                                           
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         DROP  R1                                                               
*                                                                               
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT                                                           
         BAS   RE,POSTEST                                                       
         BNE   ERRXIT                                                           
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   ACC1CN,WORK                                                      
*                                                                               
         MVC   KEY+3(12),SPACES    COSTING BILLING                              
         MVI   KEY+2,C'1'                                                       
         MVC   KEY+3(L'ANALACCT),ANALACCT                                       
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT                                                           
         BAS   RE,POSTEST                                                       
         BNE   ERRXIT                                                           
         MVC   ACC11,KEY                                                        
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   ACC11N,WORK                                                      
*                                                                               
         MVI   KEY+2,C'2'          COSTING INCOME                               
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BNE   ERRXIT                                                           
         BAS   RE,POSTEST                                                       
         BNE   ERRXIT                                                           
         MVC   ACC12,KEY                                                        
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   ACC12N,WORK                                                      
         EJECT ,                                                                
***********************************************************************         
* BUILD POSTING ELEMENTS                                              *         
***********************************************************************         
                                                                                
POST20   MVC   XTRAMESS,SPACES                                                  
         BAS   RE,GETINV           GET INVOICE NUMBER                           
         BNE   ERRXIT              ON ERROR, EXIT                               
         MVI   BTTYPE,TRNTCLBL     CLIENT BILL (TYPE 7)                         
         GOTO1 ABBLDKHD            BUILD KEY FOR BATCH HEADER                   
*                                                                               
         BAS   RE,ADD2SUB          ADD POSTING ACCOUNTS TO SUBTAB               
         BAS   RE,BUILDDB          BUILD THE WORKCODE ELEMENT                   
         BAS   RE,BUILDDB2         BUILD THE long invoice element               
*                                                                               
         USING TRNELD,R3           MAP TRANSACTION ELEMENT                      
         LA    R3,WKTRNL           BUILD TRANSACTION ELEMENT                    
         XC    WKTRNL,WKTRNL                                                    
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
         MVC   TRNDATE,BILDATE     BILL DATE                                    
         MVC   TRNREF,INVOICE      INVOICE                                      
         MVI   TRNTYPE,TRNTCLBL    CLIENT BILL(TYPE 7)                          
         MVC   TRNOFFC,OFF2        OFFICE                                       
         MVC   TRNBTCH,BTCHREF     BATCH REFERENCE                              
         DROP  R3                                                               
*                                                                               
         ZAP   SKAMNTSV,=P'0'      CLEAR SK ACCUM                               
         TM    SAVHSTAT,MANBILL                                                 
         BO    *+8                                                              
         BAS   RE,SKREV            REVERSE SK POSTINGS                          
*                                                                               
         USING MDTELD,R3           MAP MEDIA TRANSFER ELEMENT                   
         LA    R3,WKMDTL           BUILD MEDIA TRANSFER  EL                     
         XC    WKMDTL,WKMDTL                                                    
         MVI   MDTEL,MDTELQ        X'1A' - MEDIA TRANSFER TO SI                 
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,C'J'         SYSTEM = PROD                                
         MVC   MDTMED,LJOB         MEDIA CODE                                   
         MVC   MDTCLI(12),JOBKEY+3 C/P/J                                        
         MVC   MDTMOS,PMOS         MOS (PACKED)                                 
         OC    PBILMOS,PBILMOS                                                  
         BZ    *+10                                                             
         MVC   MDTMOS,PBILMOS      MOS OVERRIDE                                 
         MVC   MDTDSCP,BILJOBN     JOB NAME                                     
*                                                                               
         ZAP   DUB,TOTGRS                                                       
         CVB   R0,DUB              BILLING (GROSS)                              
         STCM  R0,15,MDTGRS                                                     
*                                                                               
         ZAP   DUB,TOTCOMM                                                      
         CVB   R0,DUB              INCOME                                       
         STCM  R0,15,MDTCOM                                                     
*                                                                               
         ZAP   DUB,TOTGRS                                                       
         CVB   R0,DUB              NET PAYABLE = GROSS-COMM-INTNL               
         ZAP   DUB,SKAMNTSV                                                     
         CVB   R1,DUB                                                           
         SR    R0,R1                                                            
         ZAP   DUB,TOTCOMM                                                      
         CVB   R1,DUB                                                           
         SR    R0,R1                                                            
         STCM  R0,15,MDTNET                                                     
*                                                                               
         ZAP   DUB,TOTCSD                                                       
         CVB   R0,DUB              CD                                           
         STCM  R0,15,MDTCD                                                      
*                                                                               
         ZAP   DUB,SKAMNTSV                                                     
         CVB   R0,DUB              INTERNAL                                     
         STCM  R0,15,MDTINTL                                                    
*                                                                               
         ZAP   DUB,TOTVGRS                                                      
         CVB   R0,DUB              RECEIVABLE = GROSS-CSD                       
         STCM  R0,15,MDTRECV                                                    
*                                                                               
         ZAP   DUB,TOTGST                                                       
         CVB   R0,DUB              GST                                          
         STCM  R0,15,MDTVAT                                                     
*                                                                               
         DROP  R3                                                               
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* CREDIT TRANSACTION TO THE JOB                                       *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6          MAP TRANSACTION RECORD                       
                                                                                
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR I/O AREA                               
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING JOBD,R1             MAP CLIENT/PROD/JOB DATA                     
         LA    R1,JOBINFO                                                       
         MVC   TRNKCULA,JOBKEY     CREDIT JOB (NET)                             
         MVC   TRNKWORK,=C'99'     WORK CODE 99 (BILLING)                       
         MVC   TRNKCULC,JRECV      CONTRA IS RECEIVABLE                         
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE NUMBER)                   
         MVC   TRNCACNM,RECVN      CONTRA NAME                                  
         DROP  R1                                                               
*                                                                               
         USING TRNELD,R3           MAP TRANSACTION ELEMENT                      
         LA    R3,WKTRNL           ADD VARIABLE PORTION OF TRNEL                
         MVI   TRNLN,TRNLN2Q                                                    
         MVI   TRNSTAT,0           TRANSACTION IS A CREDIT                      
         MVC   TRNANAL,=C'99'                                                   
         ZAP   TRNAMNT,TOTAMT      AMOUNT LESS:                                 
         SP    TRNAMNT,TOTCSD      CASH DISCOUNT                                
         MVC   TRNBLTYP,=CL15'CLIENT WP BILL'                                   
         TM    SAVHSTAT,MANBILL                                                 
         BZ    *+10                                                             
         MVC   TRNBLTYP,=CL15'MANUAL BILLING'                                   
         LA    R0,13                                                            
         LA    R1,TRNBLCOM         CLEAR BUCKETS                                
         ZAP   0(L'TRNBLCOM,R1),=P'0'                                           
         LA    R1,L'TRNBLCOM(R1)                                                
         BCT   R0,*-10                                                          
*                                                                               
         ZAP   TRNBLCOM,TOTCOMM    COMMISSION                                   
         ZAP   TRNBLCD,TOTCSD      CASH DISCOUNT                                
         ZAP   TRNBLPAY,TOTGRS     AND                                          
         SP    TRNBLPAY,TOTCSD     NET PAYABLE                                  
*                                                                               
         MVI   TRNBTYPE,TRNBTCLI   SET TYPE - CLIENT                            
         TM    SAVHSTAT,MANBILL                                                 
         BZ    *+8                                                              
         MVI   TRNBTYPE,TRNBTMAN   OR MANUAL                                    
         GOTO1 ABADDLST,(R3)       ADD BATCH TRANSACTION EL                     
*                                                                               
         TM    RUNOPT,NEEDGST      ARE WE DOING GST LOGIC ?                     
         BZ    *+8                 NO, SKIP THIS                                
         BAS   RE,BLDVBI                                                        
         TM    RUNOPT,NEEDPST      ARE WE DOING PST LOGIC ?                     
         BZ    *+8                 NO, SKIP THIS                                
         BAS   RE,BLDPBI                                                        
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         USING PIDELD,R3           ADD PIDEL                                    
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   PIDEL,PIDELQ                                                     
         MVI   PIDLN,PIDLNQ                                                     
         MVC   PIDNO,FACPID                                                     
         GOTO1 ABADDLST,(R3)       ADD THE X'D8' ELEMENT                        
*                                                                               
         BAS   RE,BLDFFT                                                        
*                                                                               
         LA    R3,SVDBELM2                                                      
         GOTO1 ABADDLST,(R3)       ADD LONG INVOICE ELEMENT                     
*                                                                               
         USING BSCELD,R3           MAP BILLING SOURCE ELEMENT (X'E3')           
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   BSCEL,BSCELQ                                                     
         MVI   BSCLN,BSCLNQ                                                     
         MVC   BSCBSRC,MEDNAME                                                  
         OC    BSCBSRC,SPACES                                                   
         MVC   BSCBOFF,OFF2                                                     
         GOTO1 ABADDLST,(R3)       ADD THE X'E3' ELEMENT                        
*                                                                               
         GOTO1 ABADDRFT            ADD DRAFT TRANSACTION                        
*                                                                               
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* CREDIT TRANSACTION TO THE INCOME ACCOUNT                            *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6          MAP TRANSACTION RECORD                       
                                                                                
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR I/O                                    
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TRNKCULA,INCOME     CREDIT INCOME ACCOUNT                        
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNKCULC,PROKEY     CONTRA IS CLIENT/PROD                        
         MVC   TRNCACNM,BILPRON    CONTRA NAME                                  
         OC    SALESAC,SALESAC     OVERRIDE PRODUCT CONTRA A/C                  
         BZ    *+16                WITH SALES ACCOUNT                           
         MVC   TRNKCULC,SALESAC                                                 
         MVC   TRNCACNM,SALESNM                                                 
*                                                                               
         USING TRNELD,R3           MAP TRANSACTION ELEMENT                      
         LA    R3,WKTRNL           ADD VARIABLE PORTION OF TRNEL                
         MVI   TRNLN,TRNLN1Q                                                    
         MVI   TRNSTAT,0           TRANSACTION IS A CREDIT                      
         MVC   TRNOFFC,OFF2        OFFICE                                       
         XC    TRNNARR,TRNNARR                                                  
         ZAP   TRNAMNT,TOTCOMM     COMMISSION                                   
         GOTO1 ABADDLST,(R3)       ADD BATCH  TRANSACTION EL                    
*                                                                               
         LA    R1,WKMDTL                                                        
         GOTO1 ABADDLST,(R1)       ADD BATCH MEDIA ELEMENT                      
*                                                                               
         USING SCIELD,R3           MAP SUBSIDIARY CASH INFO EL                  
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCIEL,SCIELQ        BUILD SUBSIDIARY CASH EL                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,C'G'                                                     
         ZAP   SCIADMN,=P'0'                                                    
         ZAP   SCIAMNT,TOTCOMM     GROSS ELEMENT FOR INCOME                     
         AP    SCIAMNT,TOTAMT                                                   
         SP    SCIAMNT,TOTCSD                                                   
         CLI   PFNOTXAM,C'Y'       EXCLUDE TAX FROM BILLING AMT HERE            
         BE    POST33              YES                                          
         AP    SCIAMNT,TOTGST                                                   
         AP    SCIAMNT,TOTPST                                                   
*                                                                               
POST33   GOTO1 ABADDLST,(R3)       ADD BATCH SCIEL ELEMENT                      
*                                                                               
         TM    RUNOPT,NEEDGST      ARE WE DOING GST LOGIC ?                     
         BZ    *+8                 NO, ALL DONE                                 
         BAS   RE,POSTBASE         WRITE GST/PST CASH ELS TO SI                 
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         LA    R3,SVDBELM2                                                      
         GOTO1 ABADDLST,(R3)       ADD LONG INVOICE ELEMENT                     
*                                                                               
         GOTO1 ABADDRFT            ADD INCOME POSTING                           
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* DEBIT TRANSACTION TO THE RECEIVABLE ACCOUNT                         *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
                                                                                
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING JOBD,R1             MAP  CLIENT/PROD/JOB     DATA                
         LA    R1,JOBINFO                                                       
         MVC   TRNKCULA,JRECV      DEBIT     RECEIVABLE    (GROSS)              
         DROP  R1                                                               
*                                                                               
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNKCULC,SPACES                                                  
         MVC   TRNKCACT,MEDNAME    CONTRA    IS   MEDIA     NAME                
         MVC   TRNCACNM,SPACES     CONTRA    NAME IS   BLANK                    
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL                                                        
         MVI   TRNSTAT,TRNSDR      TRANSACTION    IS   A    DEBIT               
         ZAP   TRNAMNT,TOTVGRS     AMOUNT    IS   GROSS                         
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         LA    R1,WKMDTL                                                        
         GOTO1 ABADDLST,(R1)       ADD  BATCH     MEDIA     ELEMENT             
*                                                                               
         USING OTHELD,R3           OTHERS    ELEMENT   FOR  RECEIVABLE          
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(13),SPACES                                                
         MVI   OTHPROF,C'J'                                                     
         LA    RE,JOBKEY                                                        
         ZIC   RF,PRODHEIR         LEVEL     A    LENGTH                        
         AR    RE,RF                                                            
         MVC   OTHNUM,3(RE)        PROD/JOB                                     
         GOTO1 ABADDLST,(R3)       ADD  BATCH     OTHER     ELEMENT             
*                                                                               
         USING SCIELD,R3           MAP  SUBSIDIARY     CASH INFO EL             
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCIEL,SCIELQ        BUILD     SUBSIDIARY     CASH EL             
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,C'D'                                                     
         ZAP   SCIADMN,=P'0'                                                    
         ZAP   SCIAMNT,TOTCSD      CASH DISCOUNT                                
         CP    TOTCSD,=P'0'                                                     
         BE    POST38              NO   ELEMENT   IF   ZERO                     
         GOTO1 ABADDLST,(R3)       ADD  BATCH     CASH DISCOUNT  EL             
*                                                                               
POST38   MVI   SCITYPE,C'I'                                                     
         ZAP   SCIAMNT,TOTCOMM     ADD  INCOME    TO RECEIVABLE POSTING         
         GOTO1 ABADDLST,(R3)       ADD  BATCH     CASH DISCOUNT  EL             
*                                                                               
         TM    RUNOPT,NEEDGST      ARE  WE   DOING     GST  LOGIC ?             
         BZ    *+8                 NO,  ALL  DONE                               
         BAS   RE,POSTBASE         WRITE     GST/PST   BASE CASH ELS            
*                                                                               
         USING DUEELD,R3           MAP  DUE  DATE ELEMENT                       
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         SR    R0,R0                                                            
         IC    R0,DUEDAYS                                                       
         GOTO1 VDATCON,DMCB,(1,BILDATE),(0,WORK)                                
         GOTO1 VADDAY,DMCB,WORK,WORK+6,(R0)                                     
         GOTO1 VDATCON,DMCB,WORK+6,(2,DUEDATE)                                  
         GOTO1 ABADDLST,(R3)       ADD  BATCH     DUE  DATE ELEMENT             
*                                                                               
         BAS   RE,GETUSR           ADD  USER FIELD     ELEMENTS                 
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         LA    R3,SVDBELM2                                                      
         GOTO1 ABADDLST,(R3)       ADD LONG INVOICE ELEMENT                     
*                                                                               
         GOTO1 ABADDRFT            ADD  RECEIVABLE     POSTING                  
*                                                                               
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* COST POSTING                                                        *         
*                          DEBIT  1C  WITH CONTRA 11  AND             *         
*                          CREDIT 11  WITH CONTRA 1C                  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
                                                                                
         CLI   COSTING,C'N'                                                     
         BE    POST50              NO   COST ACCOUNTING     POSTINGS            
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING JOBD,R1             MAP  CLIENT/PROD/JOB     DATA                
         LA    R1,JOBINFO                                                       
         MVC   TRNKCULA,JCOST      1C   ACCOUNT   IS   DEBIT                    
         DROP  R1                                                               
*                                                                               
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKCULC,ACC11      CONTRA    IS   11                            
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNCACNM,ACC11N     CONTRA    NAME                               
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL                                                        
         MVI   TRNSTAT,TRNSDR      TRANSACTION    IS   A    DEBIT               
         ZAP   TRNAMNT,TOTCOMM     AMOUNT    IS   COMMISSION                    
         AP    TRNAMNT,TOTAMT      PLUS           NET                           
         SP    TRNAMNT,TOTCSD      LESS           CD                            
         CLI   PFNOTXAM,C'Y'       EXCLUDE   TAX  FROM BILLING AMT HERE         
         BE    *+16                YES                                          
         AP    TRNAMNT,TOTGST                                                   
         AP    TRNAMNT,TOTPST                                                   
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         LA    R3,SVDBELM2                                                      
         GOTO1 ABADDLST,(R3)       ADD LONG INVOICE ELEMENT                     
*                                                                               
         GOTO1 ABADDRFT            ADD  DEBIT     TO   1C   CONTRA   11         
*                                                                               
         XC    TRNKCULA,TRNKCULC   SWITCH    ACCOUNT\CONTRA                     
         XC    TRNKCULC,TRNKCULA                                                
         XC    TRNKCULA,TRNKCULC                                                
         MVI   TRNKSBR,0                                                        
         MVC   TRNCACNM,ACC1CN     CONTRA    NAME                               
*                                                                               
         LA    R3,TRNRFST                                                       
         MVI   TRNSTAT,0           TRANSACTION    IS   A    CREDIT              
         GOTO1 ABADDRFT            ADD  CREDIT    TO   11   CONTRA   1C         
*                                                                               
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* COST POSTING                                                        *         
*                          DEBIT  1C  WITH CONTRA 12  AND             *         
*                          CREDIT 12  WITH CONTRA 1C                  *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
                                                                                
         CLI   COSTING,C'N'                                                     
         BE    POST50              NO   COST ACCOUNTING     POSTINGS            
         L     R6,ATIO             ->   TRANSACTION    I/O  AREA                
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING JOBD,R1             MAP  CLIENT/PROD/JOB     DATA                
         LA    R1,JOBINFO                                                       
         MVC   TRNKCULA,JCOST      1C   ACCOUNT   IS   DEBIT                    
         DROP  R1                                                               
*                                                                               
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKCULC,ACC12      CONTRA    IS   12                            
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNCACNM,ACC12N     CONTRA    NAME                               
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL                                                        
         MVI   TRNSTAT,TRNSDR      TRANSACTION    IS   A    DEBIT               
         ZAP   TRNAMNT,TOTCOMM     AMOUNT    IS   COMMISSION                    
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         LA    R3,SVDBELM                                                       
         GOTO1 ABADDLST,(R3)       ADD WORKCODE ELEMENT                         
*                                                                               
         LA    R3,SVDBELM2                                                      
         GOTO1 ABADDLST,(R3)       ADD LONG INVOICE ELEMENT                     
*                                                                               
         GOTO1 ABADDRFT            ADD  DEBIT     TO   1C   CONTRA   12         
*                                                                               
         XC    TRNKCULA,TRNKCULC   SWITCH    ACCOUNT\CONTRA                     
         XC    TRNKCULC,TRNKCULA                                                
         XC    TRNKCULA,TRNKCULC                                                
         MVI   TRNKSBR,0                                                        
         MVC   TRNCACNM,ACC1CN     CONTRA    NAME                               
*                                                                               
         LA    R3,TRNRFST                                                       
         MVI   TRNSTAT,0           TRANSACTION    IS   A    CREDIT              
*                                                                               
         GOTO1 ABADDRFT            ADD  CREDIT    TO   12   CONTRA   1C         
*                                                                               
POST50   TM    RUNOPT,NEEDGST      ARE  WE   DOING     GST  LOGIC ?             
         BZ    *+8                 NO,  ALL  DONE                               
         BAS   RE,POSTGST          POST TO   SG                                 
         TM    RUNOPT,NEEDPST      ARE  WE   DOING     PST  LOGIC ?             
         BZ    *+8                 NO,  ALL  DONE                               
         BAS   RE,POSTPST          POST TO   SG                                 
*                                                                               
         GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
*                                                                               
         GOTO1 ABADDHDR            ADD  THE  BATCH     ITEMS AND HEADER         
*                                                                               
         TM    SAVHSTAT,MANBILL    MANUAL    BILL ?                             
         BO    JOBUP05             YES, DO   NOT  MARK ANY  CHARGES             
*                                                                               
         CLI   EPREVSW,C'Y'                                                     
         BNE   *+8                                                              
         BAS   RE,EPREV            REVERSE   E.P                                
         B     JOBUP                                                            
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* HANDLE UPDATING OF JOB RECORD                                       *         
***********************************************************************         
JOBUP    DS    0H                                                               
         BAS   RE,MKTRS            Mark the transacions                         
*                                                                               
JOBUP05  MVI   WRKPARA,0           Read worker header record                    
         GOTO1 AWRKLGTL,AIOAREA1                                                
         BE    *+6                                                              
         DC    H'0'                Something is wrong                           
*                                                                               
         USING HEADERD,RF                                                       
         L     RF,AWRKIO                                                        
         MVI   HBILL,C'Y'          Mark as billed                               
         MVC   HINVNUM,INVOICE                                                  
         DROP  RF                                                               
*                                                                               
         GOTO1 AWRKLPUT,AIOAREA1                                                
*                                                                               
         OI    TRNINDS2,TRNIUPDG   Update the GL postings                       
         OI    TRNINDS,TRNILAST    Last time call to ADDTRNS                    
         GOTO1 ADDTRN,TRNBLK                                                    
         BE    OKEND                                                            
         DC    H'0'                Unwind                                       
         EJECT ,                                                                
***********************************************************************         
* FIND SK POSTING THAT NEED TO BE REVERSED                            *         
*                                                                     *         
*   ON ANY ERROR DIE TO ENABLE RECOVERY                               *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
                                                                                
SKREV    NTR1                                                                   
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY      READ JOB  RECORD                             
         GOTO1 AREADL,AIOAREA3                                                  
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
SKREV3   GOTO1 ASEQL,AIOAREA3                                                   
         L     R2,AIOAREA3                                                      
         CLC   JOBKEY,TRNKCULA     SAME ACCOUNT ?                               
         BNE   SKREVEX             END  OF   JOB  RECORDS                       
         LA    R4,ACCORFST(,R2)    FIND 1ST  ELEMENT                            
         CLI   0(R4),TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   SKREV3              ONLY WANT TRANSACTION    RECORDS             
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   DRAFT ?                                      
         BO    SKREV3              YES, READ NEXT RECORD                        
         TM    TRNRSTAT,TRNSREVS   REVERSAL ?                                   
         BO    SKREV3              YES, READ NEXT RECORD                        
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BZ    SKREV3              NO,  READ NEXT RECORD                        
*                                                                               
*                                  SAVE CONTRA    ACCOUNT                       
         MVC   SKACCNT,TRNKCULC         IN   CASE IT   IS   SK                  
         ZAP   SKAMNT,=P'0'                                                     
*                                                                               
SKREV5   ZIC   R0,1(,R4)                                                        
         AR    R4,R0                                                            
         CLI   0(R4),SPDELQ        X'4C' -   SUBSIDIARY     POSTING  EL         
         BE    SKREV7                                                           
         CLI   0(R4),0             END  OF   RECORD ?                           
         BE    SKREV9              YES, GET  PRORATA   BLOCK                    
         B     SKREV5                                                           
*                                                                               
*                                  OVERRIDE  CONTRA    ACCOUNT                  
         USING SPDELD,R4           MAP  SUBSIDIARY     POSTING   EL             
SKREV7   MVC   SKACCNT+1(14),SPACES                                             
         ZIC   R1,SPDLN            GET  ELEMENT   LENGTH                        
         SH    R1,=AL2(SPDLN1Q+1)  MINUS     BASE EL   LENGTH                   
         BM    SKREV5              INVALID   EL,  SKIP IT                       
         EXMVC R1,SKACCNT+1,SPDACCS     POST TO   THIS ACCOUNT                  
         B     SKREV5              GET  NEXT ELEMENT                            
*                                                                               
         USING PRORATAD,R4         MAP  PRORATA   DATA AREA                     
SKREV9   GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         L     R4,APROBLK                                                       
         ZAP   SKAMNT,PP$AALLO     GET  ALLOCATED TOTAL                         
*                                                                               
SKREV11  CLC   SKACCNT+1(2),=C'SK'                                              
         BNE   SKREV3              NOT  SK   ACCOUNT                            
         CP    SKAMNT,=P'0'        NO   POSTING   AMOUNT                        
         BE    SKREV3                                                           
*                                                                               
         AP    SKAMNTSV,SKAMNT     ADD  TO   SK   ACCUMULATOR                   
         BAS   RE,SKPOST           ADD  POSTINGS                                
         B     SKREV3              GET  NEXT                                    
*                                                                               
SKREVEX  MVC   XTRAMESS,SPACES     NO   ERRORS    FOUND                         
         B     EXIT                RETURN                                       
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* REVERSE SK POSTINGS -  DEBIT TO SK                                  *         
*                                                                     *         
*   ON ANY ERROR DIE TO ENABLE RECOVERY                               *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
                                                                                
SKPOST   NTR1  ,                                                                
         MVC   KEY,SPACES          BUILD     SK   KEY                           
         MVC   KEY(15),SKACCNT                                                  
*                                                                               
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   SKNAME,WORK         SK   ACCOUNT   NAME                          
         MVI   KEY+2,C'I'                                                       
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   SINAME,WORK         SI   ACCOUNT   NAME                          
         CLI   COSTING,C'N'                                                     
         BE    SKPOST7                                                          
*                                                                               
         MVC   XTRACST,SPACES                                                   
*                                  GET  COSTING   BYTE                          
         L     RF,AIOAREA               FROM THIS ACCOUNT                       
         AH    RF,DATADISP                                                      
*                                                                               
SKPOST1  CLI   0(RF),0             TEST EOR                                     
         BE    SKPOST5                                                          
         CLI   0(RF),SPAELQ                                                     
         BE    SKPOST3                                                          
         CLI   0(RF),RSTELQ                                                     
         BE    SKPOST4                                                          
*                                                                               
SKPOST2  ZIC   RE,1(,RF)                                                        
         AR    RF,RE                                                            
         B     SKPOST1                                                          
*                                                                               
         USING SPAELD,RF           MAP  SPECIAL   POSTING   A/C  EL             
SKPOST3  CLI   SPATYPE,SPATANAL    TEST FOR  ANALYSIS  POINTER                  
         BNE   SKPOST2             NO                                           
         MVC   XTRACST,SPAAULA                                                  
         B     SKPOST5             STOP SEARCH    NOW                           
*                                                                               
         USING RSTELD,RF           MAP  RECORD    STATUS    ELEMENT             
SKPOST4  MVC   XTRACST(L'RSTCOSTG),RSTCOSTG                                     
         B     SKPOST2                                                          
         DROP  RF                                                               
*                                                                               
SKPOST5  CLI   XTRACST,C' '        TEST FOR  A    COST ACCOUNT                  
         BNE   SKPOST6             OKAY,     CONTINUE                           
         MVI   FERN,INVPOST                                                     
         DC    H'0'                ELSE,     DIE                                
*                                                                               
SKPOST6  MVC   KEY+1(2),=C'11'                                                  
         MVC   KEY+3(12),XTRACST   AND  SAVE NAMES/KEYS                         
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   XTRA1,KEY                                                        
         MVC   XTRA1N,WORK                                                      
         MVI   KEY+2,C'2'                                                       
         MVC   XTRAMESS,SPACES                                                  
         MVC   XTRAMESS(LULACT),KEY+1                                           
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   XTRA2,KEY                                                        
         MVC   XTRA2N,WORK                                                      
*                                                                               
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
SKPOST7  L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   NEW.TRNKCULA,SKACCNT     DEBIT     SK                            
         MVC   NEW.TRNKCULC,JOBKEY      CONTRA    JOB                           
         MVC   NEW.TRNKOFF,SPACES                                               
         MVC   NEW.TRNKDATE,TRNKDATE    USE  ORIGINAL  DATE                     
         MVC   NEW.TRNKREF,TRNKREF      AND  REFERENCE                          
         LA    R1,ACCORFST(,R2)         ADD  OLD  X'44'     TO NEW RCD          
         GOTO1 ABADDLST,(R1)       ADD  OLD  TRANSACTION    EL                  
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      R3 = NEW  TRANSACTION    ELEMENT             
         MVI   TRNTYPE,TRNTCLBL    CLIENT    BILL     (TYPE 7)                  
         MVI   TRNSTAT,TRNSDR      TRANSACTION    IS   A    DEBIT               
         MVC   TRNBTCH,BTCHREF     BATCH     REFERENCE                          
         MVC   TRNOFFC,OFF2                                                     
         ZAP   TRNAMNT,SKAMNT                                                   
         MVC   TRNCACNM,BILJOBN    CONTRA    NAME                               
*                                                                               
         GOTO1 ABADDRFT            ADD  TRANSACTION                             
*                                                                               
         GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
*                                                                               
         DROP  NEW,R2,R3                                                        
         EJECT ,                                                                
***********************************************************************         
* CREDIT SI ACCOUNT                                                   *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
                                                                                
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TRNKCULA,SKACCNT    CREDIT    INCOME    ACCOUNT                  
         MVI   TRNKLDG,C'I'                                                     
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNKCULC,PROKEY     CONTRA    IS   CLIENT/PROD                   
         MVC   TRNCACNM,BILPRON    CONTRA    NAME (TO  ADDTRN    BLOCK)         
         OC    SALESAC,SALESAC     OVERRIDE  PRODUCT   CONTRA    A/C            
         BZ    *+16                WITH SALES     ACCOUNT                       
         MVC   TRNKCULC,SALESAC                                                 
         MVC   TRNCACNM,SALESNM                                                 
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL           ADD  VARIABLE  PORTION   OF   TRNEL          
         MVI   TRNSTAT,0           TRANSACTION    IS   A    CREDIT              
         ZAP   TRNAMNT,SKAMNT                                                   
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         L     R2,AIOAREA3         ->   OLD  TRANSACTION    RECORD              
         LA    R3,ACCORFST(,R2)    FIND OLD  TRANSACTION    ELEMENT             
         CLI   TRNTYPE,TRNTCLTM    CLIENT    TIME     (TYPE 49) ?               
         BNE   SKPOST8             NO   INCOME    IF   TYPE 49                  
*                                                                               
         USING SCIELD,R3           MAP  SUBSIDIARY     CASH INFO EL             
         LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   SCIEL,SCIELQ        BUILD     SUBSIDIARY     CASH EL             
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'G'                                                     
         ZAP   SCIAMNT,=P'0'       GROSS     ZERO                               
         GOTO1 ABADDLST,(R3)       ADD  BATCH     SUBSIDIARY  CASH EL           
*                                                                               
         USING MDTELD,R3           MAP  MEDIA     TRANSFER  ELEMENT             
SKPOST8  LA    R3,ELEMENT                                                       
         XC    ELEMENT,ELEMENT                                                  
         MVI   MDTEL,MDTELQ        X'1A'     MEDIA     TRANSFER   TO SI         
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,C'J'         SYSTEM =  PROD                               
         MVC   MDTMED,LJOB         MEDIA     CODE=1ST  POSITION  OF JOB         
         MVC   MDTCLI(12),JOBKEY+3 C/P/J                                        
         MVC   MDTMOS,PMOS         MOA  (PACKED)                                
         MVC   MDTDSCP,BILJOBN     JOB  NAME                                    
         XC    MDTGRS,MDTGRS                                                    
         ZAP   DUB,SKAMNT                                                       
         CVB   R0,DUB              INTERNAL  GOES INTO INCOME                   
         STCM  R0,15,MDTCOM                                                     
         XC    MDTNET,MDTNET                                                    
         XC    MDTCD,MDTCD                                                      
         XC    MDTINTL,MDTINTL                                                  
         XC    MDTRECV,MDTRECV                                                  
         XC    MDTVAT,MDTVAT                                                    
         GOTO1 ABADDLST,(R3)       ADD  BATCH     MEDIA     TRANSFER EL         
*                                                                               
         LA    R0,SUBTAB           SAVE OFF SUBTAB                              
         LA    RE,SUBTABS                                                       
         LA    RF,(SUBTABL*SUBTABM)                                             
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVC   SUBTABNS,SUBTABN    AND COUNTER                                  
*                                                                               
         BAS   RE,ADDSUB2          ADD POSTING ACCOUNTS TO SUBTAB2              
         BAS   RE,BUILDC0          BUILD X'C0' ELEMENT                          
                                                                                
         LA    R0,SUBTABS          RESTORE SUBTAB                               
         LA    RE,SUBTAB                                                        
         LA    RF,(SUBTABL*SUBTABM)                                             
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVC   SUBTABN,SUBTABNS    AND COUNTER                                  
                                                                                
         GOTO1 ABADDRFT            ADD  TRANSACTION                             
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
* COST POSTING                                                        *         
*                          DEBIT  1C  WITH CONTRA 12  AND             *         
*                          CREDIT 12  WITH CONTRA 1C                  *         
***********************************************************************         
                                                                                
         CLI   COSTING,C'N'                                                     
         BE    SKPOSTX             NO   COST ACCOUNTING     POSTING             
*                                                                               
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         L     R6,ATIO                                                          
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         USING JOBD,R1             MAP  CLIENT/PROD/JOB     DATA                
         LA    R1,JOBINFO                                                       
         MVC   TRNKCULA,JCOST      1C   ACCOUNT   IS   DEBIT                    
         DROP  R1                                                               
*                                                                               
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKCULC,XTRA2      CONTRA    IS   12                            
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNCACNM,XTRA2N     CONTRA    NAME                               
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL                                                        
         MVI   TRNSTAT,TRNSDR      TRANSACTION    IS   A    DEBIT               
         ZAP   TRNAMNT,SKAMNT      AMOUNT    IS   COMMISSION                    
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         GOTO1 ABADDRFT            ADD  DEBIT     TO   1C   CONTRA   12         
*                                                                               
         XC    TRNKCULA,TRNKCULC   SWITCH    ACCOUNT\CONTRA                     
         XC    TRNKCULC,TRNKCULA                                                
         XC    TRNKCULA,TRNKCULC                                                
         MVI   TRNKSBR,0                                                        
         MVC   TRNCACNM,ACC1CN     CONTRA    NAME                               
*                                                                               
         LA    R3,TRNRFST                                                       
         MVI   TRNSTAT,0           TRANSACTION    IS   A    CREDIT              
         GOTO1 ABADDRFT            ADD  CREDIT    TO   12   CONTRA   1C         
*                                                                               
SKPOSTX  GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
         MVC   KEY,0(R2)                                                        
         GOTO1 AREAD,AIOAREA3      RE-READ   LAST TRANSACTION                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
         DROP  R3,R6                                                            
         EJECT ,                                                                
***********************************************************************         
*              ADD CONTRA SK FLIP TO SUB-ACCOUNT TABLE                *         
***********************************************************************         
*                                                                               
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
ADDSUB2  NTR1                                                                   
         LA    RE,SUBTAB           CLEAR SUBTAB                                 
         LA    RF,(SUBTABL*SUBTABM)                                             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVI   SUBTABN,0           CLEAR SUBTAB COUNTER                         
                                                                                
         L     R6,ATIO                                                          
         LA    R1,TRNKCULA         SK FIRST                                     
         MVI   BYTE,X'80'                                                       
         BRAS  RE,ADD2TAB                                                       
                                                                                
         MVC   WORK,SPACES         SI NEXT                                      
         MVC   WORK(L'ACTKULA),TRNKCULA                                         
         MVI   WORK+2,C'I'                                                      
         MVI   BYTE,0                                                           
         LA    R1,WORK                                                          
         BRAS  RE,ADD2TAB                                                       
                                                                                
         CLI   COSTING,C'N'                                                     
         BZ    ADDSUB2X                                                         
         USING JOBD,RE             MAP CLIENT/PROD/JOB DATA                     
         LA    RE,JOBINFO                                                       
         LA    R1,JCOST            COSTING                                      
         MVI   BYTE,X'80'                                                       
         BRAS  RE,ADD2TAB                                                       
                                                                                
         LA    R1,XTRA2            SUSPENSE                                     
         MVI   BYTE,0                                                           
         BRAS  RE,ADD2TAB                                                       
                                                                                
ADDSUB2X XIT1                                                                   
         DROP  R6,RE                                                            
         EJECT                                                                  
***********************************************************************         
* FIND ESTIMATED PRODUCTION CHARGES TO BE REVERSED                    *         
*                                                                     *         
*   ON ANY ERROR DIE TO ENABLE RECOVERY                               *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
                                                                                
EPREV    NTR1  ,                                                                
         MVI   BTTYPE,0                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY      READ JOB  RECORD                             
         GOTO1 AREADL,AIOAREA3                                                  
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
EPREV3   GOTO1 ASEQL,AIOAREA3                                                   
         L     R2,AIOAREA3         ->   RECORD                                  
         CLC   JOBKEY,TRNKCULA     ACCOUNT                                      
         BNE   EPREV20             END  OF   JOB  RECORDS                       
         LA    R4,ACCORFST(,R2)    FIND 1ST  ELEMENT                            
         CLI   0(R4),TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   EPREV3              ONLY WANT TRANSACTION    RECORDS             
         CLI   TRNTYPE,TRNTEPRD    ESTIMATE  PRODUCTION    (TYPE 47)            
         BNE   EPREV3                                                           
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   DRAFT ?                                      
         BO    EPREV3              YES, READ NEXT RECORD                        
         TM    TRNRSTAT,TRNSREVS   REVERSAL ?                                   
         BO    EPREV3              YES, READ NEXT RECORD                        
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BZ    EPREV3              NO,  READ NEXT RECORD                        
*                                                                               
         USING PRORATAD,R4         MAP  PRORATA   DATA AREA                     
         GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         L     R4,APROBLK                                                       
         ZAP   EPRAMNT,PP$AALLO    GET  ALLOCATED TOTAL                         
         BZ    EPREV3              ZERO,     NOTHING   TO   POST                
         MP    EPRAMNT,=P'-1'                                                   
*                                                                               
         CLI   BTTYPE,TRNTEPRV     TEST 1ST  TIME     (TYPE 48)                 
         BE    EPREV7              NO,  SKIP                                    
         MVI   BTTYPE,TRNTEPRV     ESTIMATE  PRODUCTION     REVERSAL            
         GOTO1 ABBLDKHD            BUILD     KEY  FOR  BATCH     HEADER         
*                                                                               
EPREV7   BAS   RE,EPPOST           ADD  POSTINGS                                
         B     EPREV3              GET  NEXT                                    
*                                                                               
EPREV20  GOTO1 ABADDHDR            ADD  THE  BATCH     ITEMS AND HEADER         
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* MAKE EP REVERSE POSTINGS - DEBIT JOB                                *         
*                                                                     *         
*   ON ANY ERROR DIE TO ENABLE RECOVERY                               *         
*                                                                     *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
                                                                                
EPPOST   NTR1  ,                                                                
         L     R2,AIOAREA3         CURRENT   TRANSACTION                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),TRNKCULC                                                 
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   SKNAME,WORK         CONTRA    ACCOUNT   NAME                     
*                                                                               
NEW      USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         L     R6,ATIO             ->   TRANSACTION    I/O  AREA                
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O                                
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   NEW.TRNKEY(TRNKSBR-TRNKEY),TRNKEY                                
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,EPPOSTEL         NOW  FIX  WITH NEW  AMOUNT                   
         XC    EPPOSTEL,EPPOSTEL   CLEAR     BUILD     AREA                     
*                                  COPY CURR TRANSACTION    ELEMENT             
         MVC   TRNEL(TRNLN1Q),ACCORFST(R2)                                      
         MVI   TRNSTAT,TRNSDR      TRANSACTION    IS   A    DEBIT               
         MVC   TRNBTCH,BTCHREF     BATCH     REFERENCE                          
*                                                                               
OLD      USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
         LA    R4,ACCORFST(,R2)    ->   CURR TRANSACTION   ELEMENT              
*                                  WAS  TYPE 47   NON-COMMISSIONABLE ?          
         TM    OLD.TRNSTAT,TRNSNOCM                                             
         BZ    *+8                 NO,  SKIP                                    
         OI    TRNSTAT,TRNSNOCM    MAKE TYPE 48   NON-COMMISSIONABLE            
         DROP  OLD                                                              
*                                                                               
*                                  ESTIMATE  PRODUCTION     REVERSAL            
         MVI   TRNTYPE,TRNTEPRV        (TYPE 48)                                
         ZAP   TRNAMNT,EPRAMNT     ALLOCATED TOTAL     *    -1                  
         MVC   TRNCACNM,SKNAME     VENDOR NAME                                  
*                                  NOW FIX THE NARRATIVE TO:                    
*                                       **AUTO REVERSE E.P.                     
         MVC   TRNNARR(L'EPRCMT),EPRCMT                                         
         MVI   TRNLN,TRNLN1Q+L'EPRCMT   AND FIX EL LENGTH                       
*                                  END OF RECORD INDICATOR                      
         MVI   TRNNARR+L'EPRCMT,X'00'                                           
*                                                                               
         CLI   PF47NARR,C'Y'       WANT THE NARRATIVE FROM THE 47?              
         BNE   EPPOST4             NO                                           
         LA    R1,TRNNARR          YES                                          
         AHI   R1,L'EPRCMT                                                      
         SR    RF,RF                                                            
         IC    RF,1(R4)                                                         
         SH    RF,=YL2(TRNLN1Q)    LESS STANDARD TRANSACTION LENGTH             
         SH    RF,=YL2(L'EPCMT)    LESS STANDARD 47 NARRATIVE                   
         BZ    EPPOST2                                                          
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TRNNARR-TRNELD+L'EPCMT(R4)                               
*                                                                               
EPPOST2  AH    RF,=YL2(L'EPRCMT)                                                
         LA    R1,TRNLN1Q          TRANSACTION LENGTH                           
         LA    R1,1(R1,RF)         PLUS COMMENT PLUS 1                          
         STC   R1,TRNLN            IS ELEMENT LENGTH                            
*                                                                               
EPPOST4  GOTO1 ABADDLST,(R3)       ADD THIS TRANSACTION ELEMENT                 
*                                                                               
         GOTO1 ABADDRFT            ADD TRANSACTION                              
*                                                                               
         DROP  NEW,R2,R3                                                        
         EJECT ,                                                                
***********************************************************************         
* CREDIT CONTRA VENDOR                                                *         
***********************************************************************         
                                                                                
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
NEW      USING TRNRECD,R6          NEW  TRANSACTION    RECORD                   
                                                                                
         L     R2,AIOAREA3         ->   CURRENT   TRANSACTION                   
         L     R6,ATIO             ->   TRANSACTION    I/O  AREA                
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   NEW.TRNKEY(TRNKSBR-TRNKEY),TRNKEY                                
*                                  VENDOR                                       
         MVC   NEW.TRNKOFF,SPACES                                               
         MVC   NEW.TRNKCULA,TRNKCULC                                            
         MVC   NEW.TRNKCULC,PROKEY CONTRA    PRODUCT                            
         LA    R4,EPPOSTEL         ->   EPPOST    ELEMENT                       
         GOTO1 ABADDLST,(R4)       ADD  CURRENT   ELEMENT                       
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,NEW.TRNRFST      ->   1ST  ELEMENT                            
*                                  TRANSACTION    IS   A    CREDIT              
         NI    TRNSTAT,X'FF'-TRNSDR     SO   TURN OFF  DEBIT                    
*                                                                               
         MVC   TRNOFFC,OFF2        OFFICE                                       
         MVC   TRNCACNM,BILPRON    PRODUCT   NAME                               
*                                                                               
*                                  LOOK FOR  REAL CONTRA    ACCOUNT             
         LA    R4,TRNKEY+ACCORFST                                               
         SR    R0,R0                                                            
*                                                                               
EPPOST5  IC    R0,1(,R4)                                                        
         AR    R4,R0                                                            
         CLI   0(R4),0                                                          
         BE    EPPOST9                                                          
         CLI   0(R4),SPDELQ                                                     
         BNE   EPPOST5                                                          
*                                                                               
         USING SPDELD,R4           MAP  SUBSIDIARY     POSTING   EL             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY      READ CONTRA    OF   VENDOR                   
         ZIC   R1,SPDLN                                                         
         SH    R1,=H'3'                                                         
         EXMVC R1,KEY+1,SPDACCS                                                 
         GOTO1 AREAD,AIOAREA1                                                   
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         GOTO1 AGETNAME,AIOAREA1                                                
         MVC   NEW.TRNKCULA,KEY                                                 
*                                                                               
EPPOST9  GOTO1 ABADDRFT            ADD  TRANSACTION                             
*                                                                               
         GOTO1 ABITMADD            BATCH     ITEM RCD  ADDREC                   
*                                                                               
         MVC   KEY,0(R2)                                                        
         GOTO1 AREAD,AIOAREA3      RE-READ   LAST TRANSACTION                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         DROP  NEW,R2,R3,R4                                                     
         EJECT ,                                                                
***********************************************************************         
* GET USER FIELDS NEEDED FOR RECEIVABLES                              *         
***********************************************************************         
                                                                                
         USING UFSELD,R3           MAP  USER FLD  SELECT    ELEMENT             
                                                                                
GETUSR   NTR1                                                                   
         L     R3,ADJOB                                                         
         AH    R3,DATADISP                                                      
         LA    R0,2                MAXIMUM   OF   2    ELEMENTS                 
*                                                                               
GETU02   CLI   0(R3),0             END  OF   RECORD                             
         BE    GETUSRX             YES, EXIT                                    
         CLI   0(R3),X'A2'         NO,  ANY  USER ELEMENTS ?                    
         BE    GETU06              YES                                          
*                                                                               
GETU04   SR    R1,R1               NO,  KEEP LOOKING                            
         IC    R1,1(,R3)                                                        
         AR    R3,R1                                                            
         B     GETU02                                                           
*                                                                               
GETU06   TM    UFSSTAT,X'08'       NEED FOR  RECEIVABLE ?                       
         BZ    GETU04                                                           
         GOTO1 ABADDLST,(R3)       ADD  BATCH     USER ELEMENT                  
         BCT   R0,GETU04                                                        
*                                                                               
GETUSRX  B     EXIT                RETURN    TO   CALLER                        
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* BUILD VAT BILLED ELEMENT                                            *         
***********************************************************************         
                                                                                
         USING VBIEL,R3            MAP  VAT  BILLED    ELEMENT                  
         USING TWA1D,RF            MAP  TWA1 DSECT                              
                                                                                
BLDVBI   NTR1  ,                                                                
         LA    R0,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
         USING SVTABD,R4           MAP  SAVE TABLE     ENTRY                    
BLDVBI2  CLI   SVTYPE,X'00'        END  OF   TABLE ?                            
         BE    BLDVBIX             YES                                          
         CLI   SVTYPE,SVT_GST      IS   THIS A    GST  ENTRY ?                  
         BNE   BLDVBI5                                                          
         LA    R3,ELEMENT                                                       
         XC    VBIEL(VBILNQ),VBIEL                                              
         MVI   VBIEL,VBIELQ                                                     
         MVI   VBILN,VBILNQ                                                     
         MVC   VBITYPE,SVCODE                                                   
         MVC   VBIRATE,SVRATE                                                   
         MVC   VBIINDS,SVINDS                                                   
         MVC   VBIDATE,SVDATE                                                   
         MVC   VBIACCT,SVACCT                                                   
*                                                                               
         ZAP   VBIVAT,SVGST        SAVE VAT                                     
         ZAP   VBICOMM,SVCOMM      AND  COMMISSION                              
*                                                                               
         ZAP   VBIGROSS,SVBASE     GROSS =   BASE +    GST                      
         AP    VBIGROSS,SVGST                                                   
*                                                                               
         GOTO1 ABADDLST,(R3)       ADD  BATCH     VAT  BILLED    EL             
*                                                                               
BLDVBI5  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R0,BLDVBI2                                                       
*                                                                               
BLDVBIX  B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* BUILD PST BILLED ELEMENT                                            *         
***********************************************************************         
                                                                                
         USING PBIEL,R3            MAP  PST  BILLED    ELEMENT                  
         USING TWA1D,RF            MAP  TWA1 DSECT                              
                                                                                
BLDPBI   NTR1                                                                   
         LA    R0,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
         USING SVTABD,R4           MAP  SAVE TABLE     ENTRY                    
BLDPBI2  CLI   SVTYPE,X'00'        END  OF   TABLE ?                            
         BE    BLDPBIX             YES                                          
         CLI   SVTYPE,SVT_PST      IS   THIS A    PST  ENTRY ?                  
         BNE   BLDPBI5                                                          
         LA    R3,ELEMENT                                                       
         XC    PBIEL(PBILNQ),PBIEL                                              
         MVI   PBIEL,PBIELQ                                                     
         MVI   PBILN,PBILNQ                                                     
         MVC   PBITYPE,SVCODE                                                   
         MVC   PBIRATE,SVRATE                                                   
         MVC   PBIINDS,SVINDS                                                   
         MVC   PBIPRV,SVPRVNCE                                                  
         MVC   PBIDATE,SVDATE                                                   
         MVC   PBIACCT,SVACCT                                                   
*                                                                               
         ZAP   PBIPST,SVPST        SAVE VAT                                     
         ZAP   PBICOMM,SVCOMM      AND  COMMISSION                              
*                                                                               
         ZAP   PBIGROSS,SVBASE     BASE INCLUDES  GST                           
         AP    PBIGROSS,SVPST                                                   
*                                                                               
         GOTO1 ABADDLST,(R3)       ADD  BATCH     PST  BILLED    EL             
*                                                                               
BLDPBI5  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R0,BLDPBI2                                                       
*                                                                               
BLDPBIX  B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* BUILD TYPE 47 ESTIMATE PRODUCTION ELEMENT                           *         
***********************************************************************         
                                                                                
         USING TRNRECD,R6                                                       
BLDFFT   NTR1                                                                   
         ZAP   NET47,=P'0'                                                      
         ZAP   COM47,=P'0'                                                      
         MVI   BTTYPE,0                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY      READ JOB  RECORD                             
         GOTO1 AREADL,AIOAREA3                                                  
         BE    *+6                 OKAY, CONTINUE                               
         DC    H'0'                ELSE,DIE                                     
                                                                                
         USING TRNELD,R4                                                        
BLDFFT2  GOTO1 ASEQL,AIOAREA3                                                   
         L     R6,AIOAREA3                                                      
         CLC   JOBKEY,TRNKCULA     DO WE HAVE THE RIGHT JOB?                    
         BNE   BLDFFT4             NO ,DONE                                     
         LA    R4,ACCORFST(,R6)    FIND 1ST  ELEMENT                            
         CLI   0(R4),TRNELQ        X'44'- TRANSACTION ELEMENT?                  
         BNE   BLDFFT2             NO, GET NEXT                                 
         CLI   TRNTYPE,TRNTEPRD    TYPE 47?                                     
         BNE   BLDFFT2             NO, GET NEXT                                 
         TM    TRNRSTAT,TRNSDRFT   DRAFT?                                       
         BO    BLDFFT2             YES, GET NEXT                                
         TM    TRNRSTAT,TRNSREVS   REVERSAL?                                    
         BO    BLDFFT2             YES, GET NEXT                                
         TM    TRNSTAT,TRNSDR      DEBIT?                                       
         BZ    BLDFFT2             NO, GET NEXT                                 
                                                                                
         USING PRORATAD,R4         CALL PRORATA                                 
         GOTO1 VPRORATA,DMCB,(X'C0',(R6)),0,ACOMFACS,0,APROBLK,APROLST          
                                                                                
         L     R4,APROBLK                                                       
         AP    NET47,PP$AALLO      GET ALLOCATED NET                            
         AP    COM47,PP$ACOMM      GET ALLOCATED COMMISSION                     
         B     BLDFFT2             AND GET THE NEXT                             
                                                                                
         USING FFTELD,R3                                                        
BLDFFT4  CP    NET47,=P'0'                                                      
         BNE   BLDFFT6                                                          
         CP    COM47,=P'0'                                                      
         BE    BLDFFTX                                                          
                                                                                
BLDFFT6  LA    R3,ELEMENT                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTESTA                                
         MVI   FFTTYPE,FFTTESTP                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTESTA                                                
         ZAP   FFTESTN,NET47                                                    
         ZAP   FFTESTC,COM47                                                    
         GOTO1 ABADDLST,(R3)       ADD ESTIMATED PRODUCTION ELEMENT             
                                                                                
BLDFFTX  B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* MAKE POSTING(S) TO GST ACCOUNT(S)                                   *         
***********************************************************************         
                                                                                
         USING SVTABD,R4           MAP  SAVE TABLE     ENTRY                    
         USING TWA1D,RF            MAP  TWA1 DSECT                              
                                                                                
POSTGST  NTR1                                                                   
         LA    R0,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
POSTG02  CLI   SVTYPE,X'00'        END  OF   TABLE ?                            
         BE    POSTGX              YES                                          
*                                                                               
         CLI   SVTYPE,SVT_GST      IS   THIS A    GST  ENTRY                    
         BNE   POSTG10                                                          
*                                                                               
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         L     R6,ATIO             ->   TRANSACTION    I/O  AREA                
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TRNKCULA,SVACCT     GST  ACCOUNT                                 
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKCULC,JOBKEY     CONTRA    IS   JOB                           
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNCACNM,BILJOBN    CONTRA    NAME                               
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL           ->   WORK TRANSACTION    ELEMENT             
         MVI   TRNSTAT,0           TRANSACTION    IS   A    CREDIT              
         ZAP   TRNAMNT,SVGST       AMOUNT    IS   GST                           
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         USING SCIELD,R3           MAP  SUBSIDIARY     CASH INFO EL             
         LA    R3,ELEMENT          ADD  SUBSIDIARY     CASH ELEMENT             
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'N'                                                     
         ZAP   SCIAMNT,SVBASE      BASE AMOUNT                                  
         GOTO1 ABADDLST,(R3)       ADD  BATCH     SUBSIDIARY    CASH EL         
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         GOTO1 ABADDRFT            ADD  CREDIT    TO   GST  ACCOUNT             
*                                                                               
POSTG10  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R0,POSTG02                                                       
*                                                                               
POSTGX   B     EXIT                                                             
         DROP  R3,R4,R6                                                         
         EJECT ,                                                                
***********************************************************************         
* MAKE POSTING(S) TO PST ACCOUNT(S)                                   *         
***********************************************************************         
                                                                                
         USING SVTABD,R4           MAP  SAVE TABLE     ENTRY                    
         USING TWA1D,RF            MAP  TWA1 DSECT                              
                                                                                
POSTPST  NTR1                                                                   
         LA    R0,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
POSTP02  CLI   SVTYPE,X'00'        END  OF   TABLE ?                            
         BE    POSTPX              YES, EXIT                                    
         CLI   SVTYPE,SVT_PST      IS   THIS A    PST  ENTRY                    
         BNE   POSTP10                                                          
*                                                                               
         USING TRNRECD,R6          MAP  TRANSACTION    RECORD                   
         L     R6,ATIO             ->   TRANSACTION    I/O  AREA                
*                                                                               
         LR    RE,R6                                                            
         LA    RF,RECLNQ           CLEAR     I/O  AREA                          
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         MVC   TRNKCULA,SVACCT     PST  ACCOUNT                                 
         MVC   TRNKOFF,SPACES                                                   
         MVC   TRNKCULC,JOBKEY     CONTRA    IS   JOB                           
         MVC   TRNKDATE,BILDATE    DATE                                         
         MVC   TRNKREF,INVOICE     REFERENCE (INVOICE  NUMBER)                  
         MVC   TRNCACNM,BILJOBN    CONTRA    NAME                               
*                                                                               
         USING TRNELD,R3           MAP  TRANSACTION    ELEMENT                  
         LA    R3,WKTRNL                                                        
         MVI   TRNSTAT,0           TRANSACTION    IS   A    CREDIT              
         ZAP   TRNAMNT,SVPST       AMOUNT    IS   PST                           
         GOTO1 ABADDLST,(R3)       ADD  BATCH     TRANSACTION    EL             
*                                                                               
         USING SCIELD,R3           MAP  SUBSIDIARY     CASH INFO EL             
         LA    R3,ELEMENT          ADD  SUBSIDIARY     CASH ELEMENT             
         XC    SCIEL(SCILN1Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'N'                                                     
         ZAP   SCIAMNT,SVBASE      BASE AMOUNT                                  
         GOTO1 ABADDLST,(R3)       ADD  BATCH     SUBSIDIARY    CASH EL         
*                                                                               
         BAS   RE,BUILDC0                                                       
*                                                                               
         GOTO1 ABADDRFT            ADD  CREDIT    TO   PST  ACCOUNT             
*                                                                               
POSTP10  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R0,POSTP02                                                       
*                                                                               
POSTPX   B     EXIT                                                             
         DROP  R3,R4,R6                                                         
                                                                                
         EJECT ,                                                                
***********************************************************************         
* ADD THE GST AND PST BASE CASH ELEMENTS                              *         
***********************************************************************         
                                                                                
         USING SVTABD,R4           MAP  SAVE TABLE     ENTRY                    
         USING TWA1D,RF            MAP  TWA1 DSECT                              
                                                                                
POSTBASE NTR1  ,                                                                
         LA    R0,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
         DROP  RF                                                               
*                                                                               
PB02     CLI   SVTYPE,X'00'        END  OF   TABLE ?                            
         BE    PBX                 YES                                          
         CLI   SVTYPE,SVT_GST      IS   THIS A    GST  ENTRY                    
         BNE   PB10                                                             
*                                                                               
         USING SCIELD,R3           MAP  SUBSIDIARY     CASH INFO EL             
         LA    R3,ELEMENT          SUBSIDIARY     CASH                          
         XC    ELEMENT,ELEMENT     FOR  GST  &    BASE                          
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTAXP                                                 
         ZAP   SCIAMNT,SVGST                                                    
         ZAP   SCIBASE,SVBASE                                                   
         MVC   SCISUBPT,SVCODE                                                  
         GOTO1 ABADDLST,(R3)       ADD  BATCH     SUBSIDIARY    CASH EL         
         B     PB20                                                             
*                                                                               
PB10     CLI   SVTYPE,SVT_PST      IS   THIS A    PST  ENTRY                    
         BNE   PB20                                                             
*                                                                               
         USING SCIELD,R3           MAP  SUBSIDIARY     CASH INFO EL             
         LA    R3,ELEMENT          SUBSIDIARY     CASH                          
         XC    ELEMENT,ELEMENT     PST  AND  BASE                               
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q       PST  NEEDS     EXTENDED  LENGTH              
         MVI   SCITYPE,SCITTQST                                                 
         ZAP   SCIAMNT,SVPST                                                    
         ZAP   SCIBASE,SVBASE                                                   
         MVC   SCISUBPR,SVPRVNCE                                                
         MVC   SCISUBPT,SVCODE                                                  
         GOTO1 ABADDLST,(R3)       ADD  BATCH     SUBSIDIARY    CASH EL         
*                                                                               
PB20     LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R0,PB02                                                          
*                                                                               
PBX      B     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT ,                                                                
***********************************************************************         
*              ADD TO SUB-ACCOUNT TABLE                               *         
***********************************************************************         
*                                                                               
ADD2SUB  NTR1                                                                   
         LA    RE,SUBTAB           CLEAR SUBTAB                                 
         LA    RF,(SUBTABL*SUBTABM)                                             
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
                                                                                
         MVI   SUBTABN,0           CLEAR SUBTAB COUNTER                         
                                                                                
         LA    R1,JOBKEY           GET JOB FIRST                                
         MVI   BYTE,0                                                           
         BAS   RE,ADD2TAB                                                       
                                                                                
         LA    R1,INCOME           COMMISSION ACCOUNT                           
         BAS   RE,ADD2TAB                                                       
                                                                                
         USING JOBD,R1                                                          
         LA    R1,JOBINFO                                                       
         LA    R1,JRECV            RECEIVABLE                                   
         MVI   BYTE,X'80'                                                       
         BAS   RE,ADD2TAB                                                       
                                                                                
         LA    R1,JCOST            COSTING                                      
         CLI   COSTING,C'N'                                                     
         BE    ADD2SUB2                                                         
         BAS   RE,ADD2TAB                                                       
         DROP  R1                                                               
                                                                                
         MVI   BYTE,0                                                           
         LA    R1,ACC11            SUSPENSE                                     
         BAS   RE,ADD2TAB                                                       
                                                                                
         LA    R1,ACC12                                                         
         BAS   RE,ADD2TAB                                                       
                                                                                
ADD2SUB2 MVI   TAXTYP,SVT_GST      SET THE TYPE                                 
         TM    RUNOPT,NEEDGST      ARE WE POSTING GST?                          
         BZ    *+8                 NO, CHECK PST                                
         BAS   RE,ADDTAX           YES, ADD THE ACCOUNTS                        
                                                                                
         MVI   TAXTYP,SVT_PST      SET THE TYPE                                 
         TM    RUNOPT,NEEDPST      ARE WE POSTING PST?                          
         BZ    *+8                 NO, ALL DONE                                 
         BAS   RE,ADDTAX           YES, ADD THE ACCOUNTS                        
                                                                                
ADD2SUBX XIT1                                                                   
         EJECT                                                                  
         USING SVTABD,R4                                                        
         USING TWA1D,RF                                                         
ADDTAX   NTR1                                                                   
         LA    R0,VATNUM                                                        
         L     RF,ATWA1                                                         
         LA    R4,SVTABLE                                                       
                                                                                
ADDTAX2  CLI   SVTYPE,X'00'                                                     
         BE    ADDTAXX             END OF TABLE                                 
                                                                                
         CLC   SVTYPE,TAXTYP       IS THIS THE ENTRY WE WANT?                   
         BNE   ADDTAX4             NO, GET NEXT                                 
                                                                                
         LA    R1,SVACCT           ADD ACCOUNT TO X'C0'                         
         MVI   BYTE,X'00'                                                       
         BAS   RE,ADD2TAB                                                       
                                                                                
ADDTAX4  LA    R4,SVTABLNQ(,R4)                                                 
         BCT   R0,ADDTAX2                                                       
                                                                                
ADDTAXX  XIT1                                                                   
         DROP  RF                                                               
         EJECT                                                                  
         USING SUBTABD,RF                                                       
ADD2TAB  LA    RF,SUBTAB             RF=A(TABLE OF PRIMARY ACCOUNTS)            
         LHI   R0,SUBTABM            R0=MAX NUMBER OF ENTRIES IN TABLE          
                                                                                
ADD2TAB2 OC    0(SUBTABL,RF),0(RF)   VACANT SPOT?                               
         BZ    ADD2TAB6              YES                                        
         CLC   SUBSTA,BYTE                                                      
         BNE   ADD2TAB4                                                         
         CLC   SUBACC,1(R1)                                                     
         BER   RE                    YES, LEAVE IT                              
                                                                                
ADD2TAB4 AHI   RF,SUBTABL            BUMP TO NEXT TABLE ENTRY                   
         BCT   R0,ADD2TAB2                                                      
         BR    RE                                                               
                                                                                
ADD2TAB6 MVC   SUBSTA,BYTE                                                      
         MVC   SUBACC,1(R1)                                                     
         IC    RF,SUBTABN          INCREMENT NUMBER OF TABLE ENTRIES            
         AHI   RF,1                                                             
         STC   RF,SUBTABN                                                       
         BR    RE                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
*              FORMAT ELEMENT TO CARRY WORKCODE AND AMOUNT            *         
***********************************************************************         
*                                                                               
*              BUILD X'DB' ELEMENT WITH WORKCODE AND SJ AMOUNT                  
*                                                                               
         USING FFTELD,R5                                                        
BUILDDB  NTR1                                                                   
         LA    R5,SVDBELM                                                       
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTWORK+L'FFTWAMT                      
         MVI   FFTTYPE,FFTTWRKC                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTWORK+L'FFTWAMT                                      
         MVC   FFTWORK,=C'99'                                                   
         ZAP   FFTWAMT,TOTAMT                                                   
         SP    FFTWAMT,TOTCSD                                                   
*                                                                               
BLDDBX   XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              FORMAT ELEMENT TO CARRY long invoice number            *         
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
BUILDDB2 NTR1                                                                   
         LA    R5,SVDBELM2                                                      
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+9                                        
         MVI   FFTTYPE,FFTTINVN                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,9                                                        
         MVC   FFTDATA(1),LJOB         MEDIA                                    
         MVI   FFTDATA+1,C'-'                                                   
         MVC   FFTDATA+2(2),INVOICE    BILL NUMBER                              
         MVI   FFTDATA+4,C'-'                                                   
         MVC   FFTDATA+5(4),INVOICE+2                                           
*                                                                               
BLDDBX2  XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
*              BUILD X'C0' ELEMENT AND ADD IT                         *         
***********************************************************************         
*                                                                               
         USING APEELD,R3                                                        
         USING TRNRECD,R6                                                       
BUILDC0  NTR1                                                                   
         LA    R3,ELEMENT                                                       
         L     R6,ATIO                                                          
                                                                                
         LA    R1,SUBTAB           R1=A(TABLE OF ANALYSIS ACCOUNTS)             
         SR    R0,R0                                                            
         ICM   R0,1,SUBTABN        R0=NUMBER OF TABLE ENTRIES                   
         BZ    BLDC0X                                                           
                                                                                
         MVI   APEEL,APEELQ                                                     
         MVI   APELN,APELN1Q                                                    
         MVI   APENUM,0                                                         
                                                                                
         USING SUBTABD,R1                                                       
BLDC02   CLC   SUBACC,TRNKULA                                                   
         BE    BLDC04                                                           
         CLC   SUBACC,TRNKULC                                                   
         BE    BLDC04                                                           
                                                                                
         SR    RE,RE                                                            
         IC    RE,APELN                                                         
         LA    RE,APEELD(RE)                                                    
         USING APENTRY,RE          RE=A(APEEL ENTRY)                            
         MVC   APENSTAT,SUBSTA                                                  
         MVC   APENACT,SUBACC                                                   
         LA    RF,APENACT+L'APENACT-1                                           
         CLI   0(RF),C' '          LOCATE END OF ACCOUNT CODE                   
         BH    *+12                                                             
         MVI   0(RF),0             AND DROP TRAILING SPACES                     
         BCT   RF,*-12                                                          
         AHI   RF,1                                                             
         SR    RF,RE               RF=L'SUB-ELEMENT                             
         STC   RF,APENLEN                                                       
         DROP  RE                                                               
                                                                                
         SR    RE,RE               INCREMENT ELEMENT LENGTH                     
         IC    RE,APELN                                                         
         AR    RE,RF                                                            
         STC   RE,APELN                                                         
         IC    RE,APENUM           INCREMENT NUMBER OF SUB-ELEMENTS             
         AHI   RE,1                                                             
         STC   RE,APENUM                                                        
                                                                                
BLDC04   AHI   R1,SUBTABL          BUMP TO NEXT TABLE ENTRY                     
         BCT   R0,BLDC02                                                        
         GOTO1 ABADDLST,(R3)                                                    
                                                                                
BLDC0X   XIT1                                                                   
         DROP  R1,R3,R6                                                         
         EJECT                                                                  
***********************************************************************         
* UNBILLING                                                           *         
***********************************************************************         
                                                                                
UNBL     MVI   WRKPARA,0           READ WORKER    HEADER    RECORD              
         GOTO1 AWRKLGET,AIOAREA1                                                
         BNE   ERRXIT                                                           
*                                                                               
         USING HEADERD,RF          MAP  HEADER    RECORD                        
         L     RF,AWRKIO                                                        
         MVC   INVOICE,HINVNUM     GET  INVOICE                                 
         MVC   MEDIA,HJOB          AND  MEDIA                                   
         DROP  RF                                                               
*                                                                               
         MVI   BTTYPE,TRNTCLBL     CLIENT    BILL     (TYPE 07)                 
         GOTO1 ABDELDPT            DELETE    BATCH     DIR  POINTERS            
         BE    UNBL3                                                            
         MVI   FERN,OK                                                          
         MVC   FVMSGNO,=AL2(104)   NO   ACTIVITY  TODAY     ON THIS JOB         
         MVI   FVMTYPE,FVMINFO                                                  
         B     OKEND                                                            
*                                                                               
UNBL3    MVI   BTTYPE,TRNTEPRV     LOOK FOR  EST. PROD REVERSAL (TP 48)         
         GOTO1 ABDELDPT            DELETE    BATCH     DIR  POINTERS            
*                                                                               
         MVI   WRKPARA,0           READ WORKER    HEADER    RECORD              
         GOTO1 AWRKLGTL,AIOAREA1                                                
         BE    *+6                 OKAY,     CONTINUE                           
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         USING HEADERD,RF          MAP  HEADER    RECORD                        
         L     RF,AWRKIO                                                        
         MVC   INVOICE,HINVNUM                                                  
         MVI   HBILL,C'N'                                                       
         MVC   HINVNUM,SPACES                                                   
         DROP  RF                                                               
*                                                                               
         GOTO1 AWRKLPUT,AIOAREA1                                                
*                                                                               
         TM    SAVHSTAT,MANBILL    DO   NOT  UNMARK    TRANSACTIONS             
         BO    *+8                 IF   MANUAL    BILL                          
         BAS   RE,MKTRS                                                         
         MVI   FERN,OK                                                          
*                                  JOB  UNBILLED -                              
         MVC   FVMSGNO,=AL2(103)        ENTER     NEXT ACTION                   
         MVI   FVMTYPE,FVMINFO                                                  
         B     OKEND                                                            
         EJECT ,                                                                
***********************************************************************         
* MARK/UNMARK TRANSACTIONS                                            *         
*                                                                     *         
*   ON ANY ERROR DIE TO ENABLE RECOVERY                               *         
*                                                                     *         
***********************************************************************         
                                                                                
MKTRS    NTR1                                                                   
         MVC   MATCH,SPACES        IF   IT   IS   BILL FIND BLANK               
         MVC   REPLACE,INVOICE     REPLACE   WITH REAL                          
*                                                                               
         XC    LASTANAL,LASTANAL                                                
         ZAP   SCIUNET,=P'0'                                                    
         ZAP   SCIUCOM,=P'0'                                                    
         MVI   DATASW,C'N'         INDICATE  NO   DATA YET                      
*                                                                               
         CLI   ACTION,BIL          IF   IT   IS   BILL                          
         BE    MKTR02                                                           
         MVC   MATCH,INVOICE       FOR  UNBILL    FIND INVOICE                  
         MVC   REPLACE,SPACES      REPLACE   WITH SPACES                        
*                                                                               
MKTR02   MVC   KEY,SPACES                                                       
         MVC   KEY(15),JOBKEY      READ JOB  RECORD                             
         GOTO1 AREADL,AIOAREA3                                                  
         BE    *+6                                                              
         DC    H'0'                CAN  NOT  READ JOB                           
*                                                                               
         USING TRNRECD,R2          MAP  TRANSACTION    RECORD                   
         USING TRNELD,R4           MAP  TRANSACTION    ELEMENT                  
MKTR04   GOTO1 ASEQL,AIOAREA3                                                   
         L     R2,AIOAREA3         ->   RECORD                                  
         CLC   JOBKEY,TRNKCULA     ACCOUNT                                      
         BNE   MKTR28              END  OF   JOB  RECORDS                       
         LA    R4,ACCORFST(,R2)    FIND 1ST  ELEMENT                            
         CLI   0(R4),TRNELQ        X'44' -   TRANSACTION    ELEMENT ?           
         BNE   MKTR04              ONLY WANT TRANSACTION    RECORDS             
*                                                                               
         CLC   TRNANAL,=C'99'      ELIMINATE BILLING                            
         BE    MKTR04                                                           
         CLC   TRNANAL,=C'**'      AND  OTHER     FUNNIES                       
         BE    MKTR04                                                           
*                                                                               
         CLC   TRNANAL,SPACES                                                   
         BE    MKTR04                                                           
*                                                                               
         TM    TRNRSTAT,TRNSDRFT   DRAFT ?                                      
         BO    MKTR04              YES, READ NEXT RECORD                        
         TM    TRNRSTAT,TRNSREVS   REVERSAL ?                                   
         BO    MKTR04              YES, READ NEXT RECORD                        
*                                                                               
         TM    TRNSTAT,TRNSDR      DEBIT ?                                      
         BZ    MKTR04              NO,  READ NEXT RECORD                        
*                                                                               
         ZAP   NALLNET,=P'0'                                                    
         ZAP   NALLNETF,=P'0'                                                   
         ZAP   NALLCOM,=P'0'                                                    
         ZAP   NALLDSC,=P'0'                                                    
         XC    NALLHRS,NALLHRS                                                  
*                                                                               
         MVC   BYTE,TRNTYPE        SAVE TRANSACTION    TYPE                     
         MVI   FLAG1,C'Y'          COMMISSIONABLE                               
         TM    TRNSTAT,TRNSNOCM                                                 
         BZ    *+8                                                              
         MVI   FLAG1,C'N'          OR   NON-COMMISSIONABLE                      
         CLI   ACTION,UNB          FOR  UNBILL    DO   NOT  NEED RATES          
         BE    MKTR10                                                           
         CLC   LASTANAL,TRNANAL                                                 
         BE    MKTR10              HAVE RATE FOR  THIS WORK CODE                
         ZAP   RATE,=P'0'          COMMISSION     RATE                          
*                                                                               
         USING TWA1D,RF            MAP  TWA1 DSECT                              
         USING CHRD,R5             MAP  ALLOCATED CHARGES   TABLE               
         L     RF,ATWA1                                                         
         LA    R5,CHARGES                                                       
         ZIC   R3,CHRCNT                                                        
         DROP  RF                                                               
*                                                                               
MKTR06   CLC   TRNANAL,CHRWK                                                    
         BE    MKTR08                                                           
         LA    R5,CHRLEN(,R5)                                                   
         BCT   R3,MKTR06                                                        
         B     MKTR10                                                           
*                                                                               
MKTR08   ZAP   RATE,CHRRULE                                                     
         MVC   LASTANAL,TRNANAL                                                 
*                                                                               
MKTR10   GOTO1 VPRORATA,DMCB,(X'C0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
*                                                                               
         USING PTAELD,R3           MAP  PROD TRANSACTION    ACTIVITY EL         
         L     R3,APROLST                                                       
         CLI   0(R3),PTAELQ                                                     
         B     MKTR12+8                                                         
*                                                                               
MKTR12   MVI   ELCODE,PTAELQ       LOOK FOR  77'S TO   BILL                     
         BAS   RE,NEXTEL3                                                       
         BNE   MKTR04                                                           
*                                                                               
         CLI   PTATYPE,PTATRAL     IS   THIS AN   ALLOCATION?                   
         BNE   MKTR12              NO,  KEEP LOOKING                            
         CLI   ACTION,UNB          ARE  WE   UNBILLING?                         
         BNE   MKTR20              NO,  LOOK FOR  PENDING                       
*                                                                               
MKTR14   TM    PTASTAT1,PTASPEND   IF UNBILLING                                 
         BZ    MKTR16                ADD ALL PENDING ALLOCATIONS                
         AP    NALLNET,PTANET                                                   
         AP    NALLNETF,PTANETF                                                 
         AP    NALLCOM,PTARCOM                                                  
         AP    NALLDSC,PTACDSC                                                  
         LH    RF,NALLHRS                                                       
         AH    RF,PTAHOURS                                                      
         STH   RF,NALLHRS                                                       
         MVI   PTAEL,X'FF'         THEN MARK DELETED                            
*                                                                               
MKTR16   MVI   ELCODE,PTAELQ       GET NEXT                                     
         BAS   RE,NEXTEL3                                                       
         BE    MKTR14                                                           
*                                                                               
         USING PTAELD,R3           FOUND ALL PENDING                            
         L     R3,APROLST                                                       
         CLI   0(R3),PTAELQ                                                     
         B     MKTR18+8            LOOK FOR ITEM TO UNBILL NOW                  
*                                                                               
MKTR18   MVI   ELCODE,PTAELQ       LOOK FOR  77'S TO   BILL                     
         BAS   RE,NEXTEL3                                                       
         BNE   MKTR04                                                           
*                                                                               
         CLI   PTATYPE,PTATRAL     IS THIS AN ALLOCATION?                       
         BNE   MKTR18              NO, KEEP LOOKING                             
         TM    PTASTAT1,PTASPEND   SKIP PENDING ONES NOW                        
         BO    MKTR18                                                           
         CLC   MATCH,PTARBLNO      YES, IS THIS THE ONE WE WANT?                
         BNE   MKTR18              NO, SKIP IT                                  
*                                                                               
         CLC   PTARDATE,TODAYC     DOUBLE CHECK THE DATE                        
         BNE   MKTR18                                                           
*                                                                               
         XC    PTARBLNO,PTARBLNO   YES, CLEAR THE BILL NUMBER                   
         XC    PTAMOA,PTAMOA       THE MOA                                      
         XC    PTARBLDT,PTARBLDT   THE BILL DATE                                
         MVI   PTARGSTC,0          THE GST  CODE                                
         MVI   PTARPSTC,0          THE PST  CODE                                
         XC    PTARPRVC,PTARPRVC   AND PROVINCE                                 
         MVC   PTARDATE,TODAYC     SET TODAY'S DATE                             
         OI    PTASTAT1,PTASPEND   THE STATUS                                   
*                                                                               
         AP    SCIUNET,PTANET      ADD UP NET AND COMMISSION                    
         AP    SCIUCOM,PTARCOM     BEFORE ADDING ALLOCATED                      
*                                                                               
         AP    PTANET,NALLNET      UPDATE WITH EXISTING PENDING                 
         AP    PTANETF,NALLNETF                                                 
         AP    PTACDSC,NALLDSC                                                  
         AP    PTARCOM,NALLCOM                                                  
         LH    RE,PTAHOURS                                                      
         AH    RE,NALLHRS                                                       
         STH   RE,PTAHOURS                                                      
         GOTO1 VPRORATA,DMCB,(X'A0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
         TM    0(R1),X'10'         ANY  ERRORS ?                                
         BNO   *+6                 NO                                           
         DC    H'0'                PRORATA   TRANSACTION    TOO  BIG            
*                                                                               
         GOTO1 ATRXEL,AIOAREA3     UPDATE    X'75'     ELEM                     
         GOTO1 AWRITE,AIOAREA3                                                  
         MVI   DATASW,C'Y'         INDICATE  WE   HAVE DATA                     
         B     MKTR04                                                           
*                                                                               
MKTR20   TM    PTASTAT1,PTASPEND   IS   IT   PENDING ?                          
         BZ    MKTR12              NO,  SKIP IT                                 
         MVC   PTARBLNO,REPLACE    YES, UPDATE    BILL NUMBER                   
         MVC   PTARDATE,TODAYC     DATE AND  STATUS                             
         MVC   PTARBLDT,BILDATEC   BILL DATE COMPRESSED                         
         MVC   PTAMOA,PMOS                                                      
         NI    PTASTAT1,X'FF'-PTASPEND                                          
         MVI   PTARCODE,TRNBTCLI   SET BILLING TYPE - CLIENT                    
         TM    SAVHSTAT,MANBILL                                                 
         BZ    *+8                                                              
         MVI   PTARCODE,TRNBTMAN   OR   MANUAL                                  
*                                                                               
         TM    RUNOPT,NEEDGST      ARE  WE   CALCULATING    GST ?               
         BZ    *+10                NO                                           
         MVC   PTARGSTC,CHRCODE    YES, SAVE GST  CODE                          
         TM    RUNOPT,NEEDPST      ARE  WE   CALCULATING    PST ?               
         BZ    MKTR22              NO                                           
         MVC   PTARPSTC,CHRPCODE   SAVE PST  CODE                               
         MVC   PTARPRVC,CHRPRV     AND  PROVINCE                                
*                                                                               
MKTR22   ZAP   PTARCORT,=P'0'                                                   
         ZAP   PTARCOM,=P'0'                                                    
         CLI   FLAG1,C'N'                                                       
         BE    MKTR24              NON-COMMISSIONABLE                           
         ZAP   PTARCORT,RATE       RATE                                         
         ZAP   PK16,PTANET         NET                                          
         AP    PK16,PTACDSC        DISCOUNT                                     
         MP    PK16,RATE+2(6)      NET  * RATE                                  
         SRP   PK16,64-6,5         RATE HAS 4DP SO ROUND                        
         ZAP   PTARCOM,PK16                                                     
         DROP  R5                                                               
*                                                                               
MKTR24   GOTO1 VPRORATA,DMCB,(X'A0',(R2)),0,ACOMFACS,0,APROBLK,APROLST          
         TM    0(R1),X'10'         ANY  ERRORS ?                                
         BNO   MKTR26              NO                                           
         DC    H'0'                PRORATA   TRANSACTION    TOO  BIG            
*                                                                               
MKTR26   GOTO1 ATRXEL,AIOAREA3     UPDATE    X'75'     ELEM                     
         GOTO1 AWRITE,AIOAREA3                                                  
         AP    SCIUNET,PTANET      ADD  UP   NET  AND  COMMISSION               
         AP    SCIUCOM,PTARCOM                                                  
         MVI   DATASW,C'Y'         INDICATE  WE   HAVE DATA                     
         B     MKTR04                                                           
*                                                                               
MKTR28   CLI   DATASW,C'Y'         DO   WE   HAVE ANY  DATA ?                   
         BNE   EXIT                NO                                           
         MVC   KEY,SPACES          YES, READ JOB  AGAIN                         
         MVC   KEY(15),JOBKEY                                                   
         GOTO1 AREADL,AIOAREA3                                                  
         BE    *+6                                                              
         DC    H'0'                CAN  NOT  READ JOB                           
*                                                                               
         L     R2,AIOAREA3                                                      
         MVI   ELCODE,SCIELQ                                                    
         XC    ELFLAG,ELFLAG       CLEAR     ELEMENT   INDICATOR                
         BAS   RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
MKTR30   BAS   RE,NEXTEL                                                        
         BNE   MKTR34                                                           
*                                                                               
         USING SCIELD,R2           MAP  SUBSIDIARY     CASH INFO EL             
         CLI   SCITYPE,SCITCBAP    ALLOCATION     ELEMENT ?                     
         BE    MKTR32                                                           
         CLI   SCITYPE,SCITT99S    TODAY'S   BILLING   ELEMENT ?                
         BNE   MKTR30                                                           
         BAS   RE,UPT99S           UPDATE    ELEMENT   AMOUNT                   
                                                                                
         OI    ELFLAG,SCITT99S     INDICATE  WE   FOUND     A    99             
         B     MKTR30                                                           
*                                                                               
MKTR32   BAS   RE,UPCBAP           UPDATE    ELEMENT   AMOUNT(S)                
         OI    ELFLAG,SCITCBAP     INDICATE  FOUND     AN   ALLOCATION          
         B     MKTR30                                                           
*                                                                               
MKTR34   TM    ELFLAG,SCITT99S     DID  WE   FIND A    TYPE 99 ?                
         BO    MKTR36              YES                                          
*                                                                               
         CLI   ACTION,UNB          ARE  WE   UNBILLING?                         
         BE    MKTR36              YES, NO   NEED TO   ADD  ELEMENT             
*                                                                               
         LA    R2,TEMP             NO,  CREATE    IT   WITH BILL AMOUNT         
         XC    TEMP,TEMP                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITT99S                                                 
         ZAP   SCIAMNT,=P'0'                                                    
         BAS   RE,UPT99S                                                        
         GOTO1 AADDELM,AIOAREA3    ADD  THE  NEW  ELEMENT                       
*                                                                               
MKTR36   TM    ELFLAG,SCITCBAP     DID  WE   FIND AN   ALLOCATED ?              
         BO    MKTR38              YES                                          
*                                                                               
         CLI   ACTION,UNB          ARE  WE   UNBILLING?                         
         BNE   MKTR38              NO,  NO   NEED TO   ADD  IT   THEN           
*                                                                               
         LA    R2,TEMP             CREATE    IT   WITH ALLOCATE  AMOUNT         
         XC    TEMP,TEMP                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN2Q                                                    
         MVI   SCITYPE,SCITCBAP                                                 
         ZAP   SCIAMNT,=P'0'                                                    
         ZAP   SCIADMN,=P'0'                                                    
         BAS   RE,UPCBAP                                                        
         GOTO1 AADDELM,AIOAREA3    ADD  THE  NEW  ELEMENT                       
*                                                                               
MKTR38   GOTO1 AWRITE,AIOAREA3     UPDATE    THE  RECORD                        
         B     EXIT                                                             
         DROP  R2,R3,R4                                                         
         EJECT ,                                                                
***********************************************************************         
* UPDATE SUBSIDIARY CASH INFO ELEMENTS                                *         
***********************************************************************         
                                                                                
         USING SCIELD,R2           MAP  SUBSIDIARY     CASH INFO EL             
                                                                                
UPT99S   LA    R3,SCIAMNT          GET  NET  BUCKET                             
         LA    R4,SCIUNET          GET  ALLOCATED AMOUNT                        
         LA    R5,SUBSCI           ADDRESS   TODAY'S   BILLING                  
         CLI   ACTION,UNB          ARE  WE   UNBILLING?                         
         BE    *+8                 YES                                          
         LA    R5,ADDSCI           NO, CHANGE     INSTRUCTION                   
         EX    0,0(,R5)                                                         
         BR    RE                                                               
                                                                                
UPCBAP   LA    R3,SCIAMNT          GET  NET  BUCKET                             
         LA    R4,SCIUNET          GET  ALLOCATED AMOUNT                        
         LA    R5,SUBSCI           ADDRESS   TODAY'S   BILLING                  
         CLI   ACTION,UNB          ARE  WE   UNBILLING ?                        
         BNE   *+8                 NO                                           
         LA    R5,ADDSCI           YES, CHANGE INSTRUCTION                      
         EX    0,0(,R5)                                                         
*                                                                               
         CLI   SCILN,SCILN2Q       CHECK       THE  LENGTH                      
         BL    MKTR30                                                           
         LA    R3,SCIADMN                                                       
         LA    R4,SCIUCOM                                                       
         EX    0,0(,R5)                                                         
         BR    RE                                                               
                                                                                
ADDSCI   AP    0(6,R3),0(PLAMTLNQ,R4)                                           
SUBSCI   SP    0(6,R3),0(PLAMTLNQ,R4)                                           
*                                                                               
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* TEST POSTING STATUS OF ACCOUNTS                                     *         
***********************************************************************         
                                                                                
POSTEST  NTR1                                                                   
         L     RF,AIOAREA1                                                      
         AH    RF,DATADISP                                                      
         MVI   FLAG,C'N'                                                        
         MVI   FERN,INVPOST                                                     
*                                                                               
PT2      CLI   0(RF),0                                                          
         BE    PT100                                                            
         CLI   0(RF),X'30'         STATUS    ELEMENT                            
         BE    PT10                                                             
         CLI   0(RF),X'32'         BALANCE   ELEMENT                            
         BE    PT20                                                             
*                                                                               
PT4      ZIC   RE,1(,RF)                                                        
         AR    RF,RE                                                            
         B     PT2                                                              
*                                                                               
         USING RSTELD,RF           MAP  RECORD    STATUS    ELEMENT             
PT10     TM    RSTSTAT,X'60'       CLOSED/LOCKED                                
         BNZ   ERRXIT                                                           
         B     PT4                                                              
         DROP  RF                                                               
                                                                                
PT20     MVI   FLAG,C'Y'           BALANCE   ELEMENT   PRESENT                  
         B     PT4                                                              
                                                                                
PT100    CLI   FLAG,C'Y'                                                        
         BNE   ERRXIT                                                           
         B     OKXIT                                                            
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* GET INVOICE NUMBER AND UPDATE MEDIA/LEDGER RECORD TO NEXT SEQUENCE  *         
* NUMBER                                                              *         
*                                                                     *         
*   ON ERROR BEFORE ANY WRITE:                                        *         
*      RETURN WITH CONDITION CODE OF NE                               *         
*                                                                     *         
*   ON ANY WRITE ERROR DIE TO ENABLE RECOVERY                         *         
*                                                                     *         
***********************************************************************         
                                                                                
GETINV   NTR1                                                                   
         CLC   LEDGBILL,SPACES     HANDLE    AUTO BILL NO.S VIA  LEDGER         
         BNE   GETINV4                                                          
         GOTO1 AREAMEDL,AIOAREA2   OR   VIA  MEDIA     REC                      
         BNE   ERRXIT                                                           
         CLC   TODAY+2(2),MEDBILL  RESET     ON   CHANGE    OF   MONTH          
         BE    GETINV1                                                          
         MVC   MEDBILL(2),TODAY+2                                               
         MVC   MEDBILL+2(4),MEDRSET                                             
*                                                                               
GETINV1  MVC   INVOICE,MEDBILL                                                  
         PACK  DUB,MEDBILL                                                      
         AP    DUB,=P'1'                                                        
         UNPK  MEDBILL,DUB                                                      
         OI    MEDBILL+5,X'F0'                                                  
         L     RE,AIOAREA          UPDATE    MEDIA     REC                      
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
GETINV2  CLI   0(RE),PMDELQ        ANY  PRODUCTION     MEDIA    EL ?            
         BE    GETINV3                                                          
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     GETINV2                                                          
*                                                                               
         USING PMDELD,RE           MAP  PRODUCTION     MEDIA    EL              
GETINV3  MVC   PMDLBILL,MEDBILL                                                 
         GOTO1 AWRITE,AIOAREA                                                   
         BE    GETINV8             OKAY,     UNLOCK    RECORDS                  
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         DROP  RE                                                               
*                                                                               
GETINV4  GOTO1 AREALDGL,AIOAREA2   READ LEDGER    RECORD                        
         BNE   ERRXIT                                                           
         CLC   TODAY+2(2),LEDGBILL RESET     ON   CHANGE   OF   MONTH           
         BE    GETINV5                                                          
         MVC   LEDGBILL(2),TODAY+2                                              
         MVC   LEDGBILL+2(4),LEDGRSET                                           
*                                                                               
GETINV5  MVC   INVOICE,LEDGBILL                                                 
         PACK  DUB,LEDGBILL        BUMP NUMBER    FOR  NEXT                     
         AP    DUB,=P'1'                                                        
         UNPK  LEDGBILL,DUB                                                     
         OI    LEDGBILL+5,X'F0'                                                 
         L     RE,AIOAREA          UPDATE    LEDGER    RECORD                   
         AH    RE,DATADISP                                                      
         SR    RF,RF                                                            
*                                                                               
GETINV6  CLI   0(RE),PMDELQ        ANY  PRODUCTION     MEDIA    EL ?            
         BE    GETINV7             YES, CONTINUE                                
         IC    RF,1(,RE)           TEST NEXT ELEMENT                            
         AR    RE,RF                                                            
         B     GETINV6                                                          
*                                                                               
         USING PMDELD,RE           MAP  PRODUCTION     MEDIA    EL              
GETINV7  MVC   PMDLBILL,LEDGBILL                                                
         GOTO1 AWRITE,AIOAREA                                                   
         BE    GETINV8             OKAY,     UNLOCK    RECORDS                  
         DC    H'0'                ELSE,     DIE                                
*                                                                               
         DROP  RE                                                               
*                                                                               
*                                  UNLOCK    ALL  RCDS ON ACCOUNT               
GETINV8  GOTO1 VDATAMGR,DMCB,DMUNLK,ACCOUNT,AIOAREA                             
         CLI   PRPREFIX,0          CHANGE THE PREFIX?                           
         BE    GETINVA             NO, DONE                                     
         MVC   INVOICE(2),YM       SET BILL# TO YMNNNN                          
         CLI   PRPREFIX,2          CHECK PROFILE                                
         BE    GETINVA             OK, DONE                                     
         MVC   INVOICE(2),MY       NO, MUST BE MYNNNN                           
*                                  UNLOCK    ALL  RCDS ON ACCOUNT               
GETINVA  B     OKXIT                                                            
         EJECT ,                                                                
***********************************************************************         
* ERROR ROUTINES - THE TEXT OF ALL OF THESE MESSAGES IS: ERROR - ...  *         
***********************************************************************         
                                                                                
BADALLOC MVC   FVMSGNO,=AL2(2210)  ALLOCATED AMOUNT NOT EQUAL TO BILL           
         LA    R1,BILACTH                                                       
         B     SETERR1                                                          
*                                                                               
NOCHR    MVC   FVMSGNO,=AL2(2202)  NO   ALLOCATED CHARGES   ON   JOB            
*                                                                               
SETERR   LA    R1,BILJOBH                                                       
*                                                                               
SETERR1  ST    R1,FADR                                                          
         MVI   WRKSTAT,0                                                        
         MVI   FNDX,0                                                           
         GOTO1 AERRORX2                                                         
         EJECT ,                                                                
                                                                                
         GETEL R2,DATADISP,ELCODE                                               
         GETELN R3,DATADISP,ELCODE,3                                            
*                                                                               
OKEND    MVI   FERN,OK                                                          
         B     EXIT                                                             
*                                                                               
OKXIT    SR    RB,RB               CC = EQU                                     
*                                                                               
ERRXIT   LTR   RB,RB               CC = NEQ                                     
*                                                                               
EXIT     XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
* DATA CONSTANTS                                                      *         
***********************************************************************         
EPRCMT   DC    C'**AUTO REVERSE E.P.'                                           
EPCMT    DC    C'**ESTIMATED PRODUCTION'                                        
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
LWSD     DSECT                                                                  
EPRAMNT  DS    D                                                                
*                                                                               
ELFLAG   DS    X                   ELEMENT FLAG FOR SCIEL                       
DATASW   DS    C                   INDICATE DATA MARKED                         
TAXTYP   DS    C                   GST/PST TAX TYPE                             
*                                                                               
BILDATEC DS    CL2                 BILL DATE COMPRESSED                         
*                                                                               
RECVN    DS    CL36                                                             
INCNAME  DS    CL36                                                             
SKNAME   DS    CL36                                                             
SINAME   DS    CL36                                                             
SKSAVE   DS    CL15                                                             
VATNAME  DS    CL36                                                             
ACC1CN   DS    CL36                                                             
ACC11    DS    CL15                                                             
ACC11N   DS    CL36                                                             
ACC12    DS    CL15                                                             
ACC12N   DS    CL36                                                             
SALESAC  DS    CL15                                                             
SALESNM  DS    CL36                                                             
PROKEY   DS    CL15                                                             
OFF2     DS    CL2                 OFFICE CODE                                  
*                                                                               
TOTGRS   DS    PL(PLAMTLNQ)                                                     
TOTVGRS  DS    PL(PLAMTLNQ)                                                     
TOTCOMM  DS    PL(PLAMTLNQ)                                                     
TOTAMT   DS    PL(PLAMTLNQ)                                                     
TOTCSD   DS    PL(PLAMTLNQ)                                                     
TOTGST   DS    PL(PLAMTLNQ)                                                     
TOTPST   DS    PL(PLAMTLNQ)                                                     
*                                                                               
SKAMNT   DS    PL(PLAMTLNQ)                                                     
SKAMNTSV DS    PL(PLAMTLNQ)        SAVE AMOUNT POSTED TO SK                     
*                                                                               
NALLNET  DS    PL(L'PTANET)                                                     
NALLNETF DS    PL(L'PTANET)                                                     
NALLCOM  DS    PL(L'PTANET)                                                     
NALLDSC  DS    PL(L'PTANET)                                                     
NALLHRS  DS    XL(L'PTAHOURS)                                                   
*                                                                               
SISKSAVE DS    CL15                                                             
SISKNAME DS    CL36                                                             
OFFPOSG  DS    XL1                                                              
OFFPOSI  DS    XL1                                                              
XTRACST  DS    CL12                                                             
XTRA1    DS    CL15                                                             
XTRA1N   DS    CL36                                                             
XTRA2    DS    CL15                                                             
XTRA2N   DS    CL36                                                             
*                                                                               
XINCAM   DS    PL(PLAMTLNQ)        TOTAL EXTRA INCOME AMOUNT                    
XINCNMES DS    3CL36                                                            
*                                                                               
MATCH    DS    CL6                                                              
REPLACE  DS    CL6                                                              
RATE     DS    PL(PLAMTLNQ)                                                     
LASTANAL DS    CL2                                                              
*                                                                               
SKACCNT  DS    CL15                                                             
*                                                                               
WKMDTL   DS    XL(MDTLNQ)          MEDIA TRANSFER ELEMENT                       
WKTRNL   DS    XL255               TRANSACTION    ELEMENT                       
EPPOSTEL DS    XL(TRNLN1Q+L'EPRCMT+1)    EPPOST   ELEMENT                       
*                                                                               
SVDBELM  DS    CL(FFTLN1Q+L'FFTDLEN+L'FFTWORK+L'FFTWAMT)                        
SVDBELM2 DS    CL(FFTLN1Q+L'FFTDLEN+9)                                          
*                                  TABLE OF ACCOUNTS FOR APEEL ELEMENT          
SUBTAB   DS    0D                                                               
         DS    CL(SUBTABL*SUBTABM)                                              
SUBTABS  DS    0D                                                               
         DS    CL(SUBTABL*SUBTABM) SAVE AREA FOR SUBTAB                         
                                                                                
SUBTABM  EQU   32                  MAXIMUM # OF SUBTAB ENTRIES                  
SUBTABN  DS    X                   ACTUAL # OF SUBTAB ENTRIES                   
SUBTABNS DS    X                   SAVE AREA FOR SUBTABN         \              
                                                                                
NET47    DS    PL6                 TOTAL NET OF TYPE 47 POSTINGS                
COM47    DS    PL6                 TOTAL COM OF TYPE 47 POSTINGS                
LWSX     DS    0C                                                               
         EJECT ,                                                                
SUBTABD  DSECT                                                                  
SUBACT   DS    0C                                                               
SUBSTA   DS    XL(L'APENSTAT)      STATUS                                       
SUBACC   DS    CL(L'APENACT)       ACCOUNT                                      
SUBTABL  EQU   *-SUBTABD                                                        
         EJECT                                                                  
* ACBILDSECT                                                                    
       ++INCLUDE ACBILDSECT                                                     
* ACBILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACBILWORK                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047ACBIL04   03/18/15'                                      
         END                                                                    
