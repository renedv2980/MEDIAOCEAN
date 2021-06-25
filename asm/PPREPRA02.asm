*          DATA SET PPREPRA02  AT LEVEL 174 AS OF 01/05/12                      
*PHASE PPRA02A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE GETCOST                                                                
         PRINT NOGEN                                                            
         TITLE 'PPRA02 - REBATE ANALYSIS'                                       
*                                                                               
*        CHANGE LOG                                                             
*                                                                               
* BPLA  01/05/2012   RELINK WITH NEW GETCOST                                    
*                                                                               
* KWAN 08/00     NEW PBILLREC DSECT                                             
*                                                                               
* BPLA  2/99     AT FBUYCLI - IF NOT FINANCIAL CLIENT SKIP                      
*                BY SETTING MODE TO LBUYCLI                                     
*                                                                               
* BPLA 5/95      AT FBUYCLI READ B2 PROFILE                                     
*                AND USE IT WHEN CALLING GETCOST                                
*                                                                               
* BPLA 6/22/94   GOTO PRNTCLNT FOR OFFICE REQUESTA AS WELL                      
*                  AS 'ALL' CLIENT REQUESTS                                     
*                                                                               
* BPLA  7/26/93  ADD LINE NUMBER TO SORT AND DISPLAY                            
*                                                                               
*  BPLA 7/21/93  IF QOPT 4 = C - READ BILLREC TO GET FORMULA                    
*                AND USE GETCOST TO APPLY IT TO AMOUNTS                         
*                SO REPORT WILL SHOW "COST" RATHER THAN GROSS                   
*                                                                               
PPRA02   CSECT                                                                  
         NMOD1 0,PPRA02                                                         
*                                                                               
**********************************************************************          
* QOPT1: N= WILL FILTER OUT ALL ITEMS WITH A DIFFERENCE OF ZERO      *          
*        DEFAULT = N                                                 *          
*        Y= WILL SHOW ALL ZERO DIFFERENCES                           *          
*                                                                    *          
* QOPT2: N=DEFAULT                                                   *          
*        Y=SHOW ONLY REBATABLE ITEMS/NO REBATED ITEMS                *          
* QOPT3: N=DEFAULT                                                   *          
*        Y=SHOW ONLY REBATED ITEMS/NO REBATABLE ITEMS                *          
*                                                                    *          
* QOPT6  Y=TRACE SORT CALLS                                                     
*                                                                    *          
**********************************************************************          
         SPACE 2                                                                
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,1(RB)                                                         
         LA    R8,4095(R8)                                                      
         USING PPRA02+4096,R8                                                   
*                                                                               
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   CKM3                                                             
         OI    DMINBTS,X'08'                                                    
         OI    DMOUTBTS,X'FD'                                                   
         ZAP   REPTOPN,=P'0'             ZAP REPORT TOTALS FOR                  
         ZAP   REPTCON,=P'0'             ACCUMULATING                           
         ZAP   REPTDIF,=P'0'                                                    
         B     CKM11                                                            
*                                                                               
*                                                                               
CKM3     CLI   MODE,RUNLAST                                                     
         BNE   CKM4                                                             
         GOTO1 SORTER,DMCB,=C'END'       AT RUNLAST CLOSE SORT INCASE           
         B     CKM11                     IT WAS MULTIMEDIA REQUEST              
*                                                                               
CKM4     CLI   MODE,FBUYCLI                                                     
         BNE   CKM5                                                             
*                                                                               
         CLI   PCLTFIN,C'Y'                                                     
         BE    CKM4C                                                            
         MVI   MODE,LBUYCLI        TO SKIP THIS CLIENT                          
         B     CKM11                                                            
*                                                                               
CKM4C    DS    0H                                                               
*            FIRST TRY TO READ B2 PROFILE (NEEDED FOR GETCOST)                  
         XC    B2PROF,B2PROF                                                    
         CLI   QOPT4,C'C'     SEE IF REPORTINF CLIENT COST                      
         BNE   CKM11                                                            
         XC    WORK,WORK                                                        
         MVC   WORK(12),=CL12'P000'                                             
         MVC   WORK+2(2),=C'B2'                                                 
         MVC   WORK+4(2),QAGENCY                                                
         MVC   WORK+6(1),QMEDIA                                                 
         MVC   WORK+7(3),PCLTKCLT                                               
         CLI   PCLTOFF,C' '                                                     
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),PCLTOFF                                               
         GOTO1 GETPROF,DMCB,WORK,B2PROF,DATAMGR                                 
         B     CKM11                                                            
*                                                                               
CKM5     CLI   MODE,FBUYREQ                                                     
         BNE   CKM7                                                             
         ZAP   COUNTIN,=P'0'                  INIT RECORD COUNT                 
         MVC   QSORT,=C'NO'                   MUST RESET TO NO                  
         GOTO1 SORTER,DMCB,SORTCARD,RECCARD,(60,ASORTC)                         
*                                                                               
         CLC   QEND,SPACES                    IF NO QEND SPECIFIED SET          
         BNE   CKM5B                          START DATE EQUAL                  
         MVC   QEND,QSTART                    TO END DATE.                      
CKM5B    DS    0H                                                               
         CLI   RCMULTIQ,C'Y'                                                    
         BNE   CKM5D                                                            
**                                                                              
         CLI   QSTART,C' '                 SEE IF QSTART PRESENT                
         BH    CKM5D                       WILL BE FOR FIRST MEDIA              
         B     CKM5E                                                            
****     CLI   QMEDIA,C'M'                                                      
****     BNE   CKM5E                                                            
CKM5D    MVC   SVQSTRT(12),QSTART             SAVE OFF QSTART AND QEND          
         GOTO1 DATCON,DMCB,(0,SVQSTRT),(3,SBQSTRT)                              
         GOTO1 DATCON,DMCB,(0,SVQEND),(3,SBQEND)                                
CKM5E    MVC   BQSTART(6),=C'000000FFFFFF'    NEED TO GET INSRTNS BY            
         MVC   QSTART(12),SPACES              BILLING DATE NOT INS DATE         
*                                                                               
         XC    ADCDLAB,ADCDLAB                                                  
         CLC   QREGION,=C'ALL'         SET APPROPRIATE ADCODE LABEL             
         BE    CKM5F                   FOR LATER MOVING TO HEADLINES            
         CLC   QREGION(6),=6C' '       WHEN NEEDED                              
         BE    CKM5F                                                            
         MVC   ADCDLAB(6),QREGION                                               
         B     CKM5G                                                            
CKM5F    MVC   ADCDLAB,=C'ALL CODES'                                            
CKM5G    XC    HDLAB,HDLAB                                                      
         CLI   QOPT2,C'Y'                                                       
         BNE   CKM5J                                                            
         MVC   HDLAB,=C'** REBATABLE ITEMS ONLY **'                             
         B     CKM11                                                            
CKM5J    CLI   QOPT3,C'Y'                                                       
         BNE   CKM11                                                            
         MVC   HDLAB,=C' ** REBATED ITEMS ONLY ** '                             
         B     CKM11                                                            
*                                                                               
*                                                                               
CKM7     CLI   MODE,PROCBUY                                                     
         BNE   CKM9                                                             
CKM7C    BAS   RE,PRCSBUY                                                       
         B     CKM11                                                            
*                                                                               
CKM9     CLI   MODE,LBUYREQ                                                     
         BNE   CKM10                                                            
*                                                                               
         MVC   PAGE,=H'1'                     INIT PAGE                         
         MVI   LINE,0                         INIT LINE                         
*                                                                               
         MVI   RCSUBPRG,11                    SET SPROG                         
         CLC   QPRODUCT,=C'   '                                                 
         BE    CKM9C                                                            
         MVI   RCSUBPRG,10                                                      
*                                                                               
CKM9C    MVI   FORCEHED,C'Y'                  START ON NEW PAGE                 
*                                                                               
         CLI   QOPT4,C'C'             SEE IF SHOWING COST                       
         BNE   CKM9C5                                                           
         ZIC   R0,RCSUBPRG                                                      
         AH    R0,=H'10'                                                        
         STC   R0,RCSUBPRG                                                      
*                                                                               
CKM9C5   ZAP   ADCTOPN,=P'0'           ZAP ADCODE TOTALS FOR                    
         ZAP   ADCTCON,=P'0'           ACCUMULATING                             
         ZAP   ADCTDIF,=P'0'                                                    
         ZAP   MEDTOPN,=P'0'           ZAP MEDIA TOTALS FOR                     
         ZAP   MEDTCON,=P'0'           ACCUMULATING                             
         ZAP   MEDTDIF,=P'0'                                                    
         ZAP   PRDTOPN,=P'0'           ZAP PRODUCT TOTALS FOR                   
         ZAP   PRDTCON,=P'0'           ACCUMULATING                             
         ZAP   PRDTDIF,=P'0'                                                    
         ZAP   CLITOPN,=P'0'           ZAP CLIENT TOTALS FOR                    
         ZAP   CLITCON,=P'0'           ACCUMULATING                             
         ZAP   CLITDIF,=P'0'                                                    
*                                                                               
         MVC   QSTART(12),SVQSTRT      FOR PRINTING IN HEADLINES                
CKM9D    BAS   RE,PRNTSRT              PRINT ALL SORT RECS                      
*                                                                               
CKM9F    XC    OLDADCD,OLDADCD                                                  
         GOTO1 SORTER,DMCB,=C'END'                                              
         MVI   FORCEHED,C'Y'                                                    
         B     CKM11                                                            
*                                                                               
*                                                                               
CKM10    CLI   MODE,LBUYXRQ                                                     
         BNE   CKM11                                                            
         GOTO1 SORTER,DMCB,=C'END'                                              
         CLI   LINE,62                                                          
         BNL   CKM10D                                                           
         MVI   FORCEHED,C'N'                                                    
CKM10D   BAS   RE,PRNTRPT                                                       
         XC    P(132),P                                                         
         ZAP   REPTOPN,=P'0'                                                    
         ZAP   REPTCON,=P'0'                                                    
         ZAP   REPTDIF,=P'0'                                                    
         B     CKM11                                                            
*                                                                               
CKM11    XMOD1 1                                                                
         EJECT                                                                  
*                                                                               
         DS    0H                                                               
PRCSBUY  NTR1                                                                   
*                                                                               
         LA    R3,PRNTCSC                                                       
         USING PRNTDSC,R3                                                       
*                                                                               
         CLI   PCLTFIN,C'Y'                 ONLY PROCESS BUYS FOR               
         BNE   PRCSXIT                      FINANCIAL CLIENTS                   
*                                                                               
         BAS   RE,CHKRBT         CHK QOPT2 OR QOPT3 = Y, IF SO FILTER           
         CLI   USEBUY,C'Y'                                                      
         BNE   PRCSXIT                                                          
*                                OUT APPROPRIATE BUYS                           
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
PRCS20   BAS   RE,NEXTEL                                                        
         BNE   PRCS100                                                          
*                                                                               
         USING PBILELEM,R2                                                      
         CLC   PBLDATE,SBQSTRT                                                  
         BL    PRCS20                                                           
         CLC   PBLDATE,SBQEND                                                   
         BH    PRCS20                                                           
*                                                                               
PRCS25   XC    PRNTCSC,PRNTCSC                                                  
*                                                                               
         CLI   QOPT4,C'C'            SEE IF REPORTING COST                      
         BNE   PRCS28                                                           
         BAS   RE,FINDBILL           FIND BILL RECORD                           
         CLI   B2PROF+12,C'Y'                                                   
         BNE   PRCS27                                                           
         GOTO1 =V(GETCOST),DMCB,(C'T',BFORM),PBGROSS,PBUYREC                    
         B     PRCS27X                                                          
*                                                                               
PRCS27   GOTO1 =V(GETCOST),DMCB,BFORM,PBGROSS,PBUYREC                           
*                                                                               
PRCS27X  MVC   PBGROSS,DMCB+4        RETURNED COST                              
*                                                                               
PRCS28   MVC   PCONTRCT,PBGROSS                                                 
         MVC   SV26EL,PBPRD                                                     
         ST    R2,ELADD                                                         
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'28'                                                     
PRCS40   BAS   RE,NEXTEL                                                        
         BE    PRCS45                                                           
         L     R2,ELADD                 IF NO MATCHING '28' EL TREAT AS         
         B     PRCS48C                  OPEN BILLED GROSS = 0                   
*                                       LEAVE POPEN = 0.                        
PRCS45   CLC   SV26EL,PBPRD             IS IT MATCHING '28' FOR '26'.           
         BNE   PRCS40                   IF NOT GET NEXT '28' EL.                
*                                                                               
         MVI   0(R2),X'FF'              IF MATCH FOUND MARK EL PRCSD.           
         CLI   QOPT4,C'C'                                                       
         BNE   PRCS46                                                           
* NOTE THAT SINCE IT MATCHES BFORM WILL BE THE SAME AS FOR X'26' ELEM           
         CLI   B2PROF+12,C'Y'                                                   
         BNE   PRCS45C                                                          
         GOTO1 =V(GETCOST),DMCB,(C'T',BFORM),PBGROSS,PBUYREC                    
         B     PRCS45X                                                          
*                                                                               
PRCS45C  GOTO1 =V(GETCOST),DMCB,BFORM,PBGROSS,PBUYREC                           
*                                                                               
PRCS45X  MVC   PBGROSS,DMCB+4        RETURNED COST                              
*                                                                               
PRCS46   CLC   PBGROSS,PCONTRCT         IF CONTRACT = GROSS AND QOPT1           
         BNE   PRCS48                   NOT SET TO SHOW EQUALS SKIP             
         CLI   QOPT1,C'Y'               AND GET NEXT '26' EL.                   
         BNE   PRCS70                                                           
*                                                                               
PRCS48   MVC   POPEN,PBGROSS                                                    
PRCS48C  GOTO1 DATCON,DMCB,(3,PBLDATE),(0,DUB+2)                                
         MVC   PDOCNUM(2),DUB+4                                                 
         EDIT  PBINVNO,(4,FULL),0,FILL=0                                        
         MVC   PDOCNUM+2(4),FULL                                                
*                                                                               
         BAS   RE,COMPROC                                                       
*                                                                               
PRCS70   L     R2,ELADD                                                         
         MVI   ELCODE,X'26'                                                     
         B     PRCS20                                                           
*                                                                               
PRCS100  LA    R2,PBUYREC+33              AFTER GOING THROUGH ALL '26'          
         MVI   ELCODE,X'28'               ELS FIND ALL UNPROCESSED '28'         
PRCS110  BAS   RE,NEXTEL                  ELS.                                  
         BNE   PRCS200                                                          
         ST    R2,ELADD                                                         
*                                                                               
         USING PBILELEM,R2                                                      
         CLC   PBLDATE,SBQSTRT                                                  
         BL    PRCS110                                                          
         CLC   PBLDATE,SBQEND                                                   
         BH    PRCS110                                                          
*                                                                               
         XC    PRNTCSC,PRNTCSC                                                  
         CLI   QOPT4,C'C'      SEE IF REPORTING COST                            
         BNE   PRCS112                                                          
         BAS   RE,FINDBILL           FIND BILL RECORD                           
         CLI   B2PROF+12,C'Y'                                                   
         BNE   PRCS111                                                          
         GOTO1 =V(GETCOST),DMCB,(C'T',BFORM),PBGROSS,PBUYREC                    
         B     PRCS111X                                                         
*                                                                               
PRCS111  GOTO1 =V(GETCOST),DMCB,BFORM,PBGROSS,PBUYREC                           
*                                                                               
PRCS111X DS    0H                                                               
         MVC   PBGROSS,DMCB+4        RETURNED COST                              
*                                                                               
PRCS112  MVC   POPEN,PBGROSS                                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBLDATE),(0,DUB+2)                                
         MVC   PDOCNUM(2),DUB+4                                                 
         EDIT  PBINVNO,(4,FULL),0,FILL=0                                        
         MVC   PDOCNUM+2(4),FULL                                                
*                                                                               
         BAS   RE,COMPROC                                                       
         L     R2,ELADD                                                         
         MVI   ELCODE,X'28'               MUST RESET ELCODE                     
         B     PRCS110                                                          
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
PRCS200  DS    0H                                                               
*****    CLI   QOPT2,C'Y'                  IF REBATABLE ITEMS ONLY              
*****    BE    PRCSXIT                     SKIP THIS SECTION                    
*****                                  MUST STILL SHOW REBATE BILLINGS          
*                                                                               
         LA    R3,PRNTCSC                                                       
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'29'                                                     
PRCS205  BAS   RE,NEXTEL                                                        
         BNE   PRCSXIT                                                          
*                                                                               
         USING PBILELEM,R2                                                      
         CLC   PBLDATE,SBQSTRT                                                  
         BL    PRCS205                                                          
         CLC   PBLDATE,SBQEND                                                   
         BH    PRCS205                                                          
*                                                                               
         XC    PRNTCSC,PRNTCSC                                                  
         CLI   QOPT4,C'C'          SEE IF DOING COST                            
         BNE   PRCS207                                                          
         BAS   RE,FINDBILL           FIND BILL RECORD                           
         GOTO1 =V(GETCOST),DMCB,BFORM,PBGROSS,,PBUYREC                          
         MVC   PBGROSS,DMCB+4        RETURNED COST                              
*                                                                               
PRCS207  DS    0H                                                               
         MVC   PPUBCODE,PBUYKPUB                                                
         MVC   PINSDATE,PBUYKDAT                                                
         MVC   PINSLIN,PBUYKLIN                                                 
         MVC   PCLISR,PBUYKCLT                                                  
         MVC   PPRODSR,PBUYKPRD                                                 
         MVC   PCLI,PBUYKCLT                                                    
         MVC   PPROD,PBUYKPRD                                                   
         MVC   FULL,PBGROSS                                                     
         L     R0,FULL                                                          
         SR    R5,R5                                                            
         SR    R5,R0                                                            
         ST    R5,PDIFF                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,PBLDATE),(0,DUB+2)                                
         MVC   PDOCNUM(2),DUB+4                                                 
         EDIT  PBINVNO,(4,FULL),0,FILL=0                                        
         MVC   PDOCNUM+2(4),FULL                                                
         DROP  R2                                                               
*                                                                               
*                                                                               
         ST    R2,ELADD                                                         
         LA    R2,PBUYREC+33                                                    
         USING PBDELEM,R2                                                       
         MVI   ELCODE,X'20'                                                     
         CLI   0(R2),X'20'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   QREGION,=C'ALL'                                                  
         BE    PRCS210                                                          
         CLC   QREGION(6),=6C' '                                                
         BE    PRCS210                                                          
         CLC   QREGION,PBDJOB                                                   
         BNE   PRCSXIT                                                          
PRCS210  MVC   PADCODE,PBDJOB                                                   
         MVI   PREBATE,C'Y'                                                     
         LA    R3,PRNTCSC                                                       
         CLI   QOPT6,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,TRACEIN                                                       
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         AP    COUNTIN,=P'1'                                                    
         L     R2,ELADD                                                         
         MVI   ELCODE,X'29'            MUST RESET ELCODE                        
         B     PRCS205                                                          
         DROP  R2                                                               
*                                                                               
PRCSXIT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
COMPROC  NTR1                                                                   
         ST    RE,COMPRE                                                        
         MVC   PPUBCODE,PBUYKPUB                                                
         MVC   PINSDATE,PBUYKDAT                                                
         MVC   PINSLIN,PBUYKLIN        LINE NUMBER                              
*                                                                               
         LA    R2,PBUYREC+33                                                    
         USING PBDELEM,R2                                                       
         MVI   ELCODE,X'20'                                                     
         CLI   0(R2),X'20'                                                      
         BE    PRCS49                                                           
         BAS   RE,NEXTEL                                                        
         BE    PRCS49                                                           
         DC    H'0'                                                             
PRCS49   CLC   QREGION,=C'ALL'                                                  
         BE    PRCS50                                                           
         CLC   QREGION(6),=6C' '                                                
         BE    PRCS50                                                           
         CLC   QREGION,PBDJOB                                                   
         BNE   PRCSXIT                                                          
PRCS50   MVC   PADCODE,PBDJOB                                                   
*                                                                               
         MVC   FULL,PCONTRCT     R4 HOLDS CONTRACT AMOUNT                       
         L     R4,FULL                                                          
         MVC   FULL,POPEN                                                       
         L     R5,FULL           HOLDS OPEN AMOUNT                              
         SR    R4,R5             CONTRACT$ - OPEN$                              
         ST    R4,FULL           FULL NOW = DIFF OF CONTRACT & OPEN             
         MVC   PDIFF,FULL                                                       
*                                                                               
         CLC   PADCODE,=6X'00'                                                  
         BE    PRCS60                                                           
         GOTO1 =A(GETCAP)                                                       
*                                                                               
PRCS60   LA    R3,PRNTCSC                                                       
*                                                                               
         MVC   PPROD,PBUYKPRD              FOR PRINTING                         
         MVC   PCLI,PBUYKCLT                                                    
*                                                                               
PRCS65   MVC   PCLISR,PBUYKCLT                                                  
         MVC   PPRODSR,PBUYKPRD            FOR SORTING                          
*                                                                               
PRCS68   CLI   QOPT6,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,TRACEIN                                                       
         GOTO1 SORTER,DMCB,=C'PUT',(R3)                                         
         AP    COUNTIN,=P'1'                                                    
         L     RE,COMPRE                                                        
         DROP  R2                                                               
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
CHKRBT   NTR1                                                                   
         MVI   USEBUY,C'Y'                                                      
         CLI   QOPT2,C'Y'      SEE IF REPORTING REBATABLE OR REBATED            
         BE    CHKRB5          ITEMS ONLY                                       
         CLI   QOPT3,C'Y'                                                       
         BNE   CHKRBXIT                                                         
*                                                                               
CHKRB5   XC    DIFFAMT,DIFFAMT                                                  
         XC    REBAMT,REBAMT                                                    
*                                                                               
         LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'26'                                                     
CHKRB10  BAS   RE,NEXTEL                                                        
         BNE   CHKRB20                                                          
         USING PBILELEM,R2                                                      
         MVC   FULL,PBGROSS                                                     
         L     R5,FULL                                                          
         L     R0,DIFFAMT                                                       
         AR    R0,R5                                                            
         ST    R0,DIFFAMT                                                       
         B     CHKRB10                                                          
*                                                                               
CHKRB20  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'28'                                                     
CHKRB30  BAS   RE,NEXTEL                                                        
         BNE   CHKRB40                                                          
         MVC   FULL,PBGROSS                                                     
         L     R5,FULL                                                          
         L     R0,DIFFAMT                                                       
         SR    R0,R5                                                            
         ST    R0,DIFFAMT                                                       
         B     CHKRB30                                                          
*                                                                               
CHKRB40  LA    R2,PBUYREC+33                                                    
         MVI   ELCODE,X'29'                                                     
CHKRB50  BAS   RE,NEXTEL                                                        
         BNE   CHKRB60                                                          
         MVC   FULL,PBGROSS                                                     
         L     R5,FULL                                                          
         L     R0,REBAMT                                                        
         AR    R0,R5                                                            
         ST    R0,REBAMT                                                        
         B     CHKRB50                                                          
*                                                                               
CHKRB60  CLI   QOPT2,C'Y'                                                       
         BNE   CHKRB70                                                          
         CLC   DIFFAMT,REBAMT                                                   
         BNE   CHKRBXIT                                                         
         MVI   USEBUY,C'N'                                                      
*                                                                               
CHKRB70  CLI   QOPT3,C'Y'                                                       
         BNE   CHKRBXIT                                                         
         CLC   DIFFAMT,REBAMT                                                   
         BE    CHKRBXIT                                                         
         MVI   USEBUY,C'N'                                                      
CHKRBXIT XIT1  1                                                                
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
FINDBILL NTR1             NOTE R2 SHOULD BE POINTED TO A BILLING ELEM           
         XC    BFORM,BFORM                                                      
         USING PBILELEM,R2                                                      
         MVC   PPGKEY,KEY                                                       
         MVI   RDSW,C'N'                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(7),PBUYREC                                                   
         MVI   KEY+3,X'08'                                                      
         MVC   KEY+7(3),PBPRD                                                   
         MVC   KEY+10(2),PBUYKEST                                               
         MVC   KEY+12(2),PBDBDATE   BILLING MOS                                 
         MVC   KEY+14(2),PBLDATE    BILLING RUN-ON MONTH                        
         MVC   KEY+16(2),PBINVNO    INVOICE NUMBER                              
         CLC   PBILLREC(18),KEY     SEE IF I ALREADY HAVE THE BILL              
         BE    FINDB50                                                          
         MVI   RDSW,C'Y'                                                        
         GOTO1 HIGH                                                             
         CLC   KEY(18),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                  DIE IF I DON'T FIND THE BILL               
         GOTO1 GETBILL                                                          
*                                                                               
FINDB50  MVC   BFORM,PBILBASA                                                   
         OC    BFORM,BFORM              SEE IF BILLREC HAD FORMULA              
         BNZ   FINDB60                                                          
         MVC   BFORM,=X'0101000000'     DEFAULT TO GROSS?                       
*                                                                               
FINDB60  MVC   KEY(64),PPGKEY           RESTORE PPG'S KEY AND KEYSAVE           
         CLI   RDSW,C'N'               SEE IF I READ SOMETHING                  
         BE    FINDBX                  NO - JUST EXIT                           
         GOTO1 HIGH                                                             
         MVC   KEYSAVE,PPGKEY+32       RESTORE PPG'S KEYSAVE                    
FINDBX   XIT1                                                                   
*                                                                               
PRNTSRT  NTR1                                                                   
         GOTO1 SORTER,DMCB,=C'GET'     FIRST TIME THROUGH                       
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BNZ   PRNT20                                                           
*                                                                               
         CLI   RCMULTIQ,C'Y'                                                    
         BE    PRNTXIT                                                          
         XC    P(132),P                                                         
         MVC   P+58(16),=C'NO DATA TO PRINT'                                    
         MVC   H6+9(9),ADCDLAB                                                  
         MVC   H4+51(26),HDLAB                                                  
         GOTO1 REPORT                                                           
         B     PRNTXIT                                                          
*                                                                               
PRNT1    GOTO1 SORTER,DMCB,=C'GET'                                              
         L     R3,DMCB+4                                                        
         LTR   R3,R3                                                            
         BNZ   PRNT12                                                           
*                                                                               
         BAS   RE,PRNTADCD                                                      
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   PRNT10B                                                          
         BAS   RE,PRNTPROD                                                      
*                                                                               
PRNT10B  DS    0H                                                               
         CLI   QCLIENT,C'*'           SEE IF OFFICE REQUEST                     
         BE    PRNT10C                                                          
         CLC   QCLIENT,=C'ALL'        OR ALL CLT REQUEST                        
         BNE   PRNT10F                                                          
PRNT10C  DS    0H                     PRINT CLIENT TOTALS                       
         BAS   RE,PRNTCLNT                                                      
PRNT10F  BAS   RE,PRNTTOT                                                       
         B     PRNTXIT                                                          
*                                                                               
         USING PRNTDSC,R3                                                       
PRNT12   CLC   OLDADCD,PADCODE         IF ADCODE HAS CHANGED PRINT              
         BE    PRNT20                  ADCODE TOTS                              
         BAS   RE,PRNTADCD                                                      
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   PRNT15                                                           
         CLC   OLDPRD,PPRODSR                                                   
         BNE   PRNT13                                                           
         CLC   OLDCLI,PCLISR                                                    
         BE    PRNT20                                                           
PRNT13   BAS   RE,PRNTPROD                                                      
*                                                                               
PRNT15   CLC   OLDCLI,PCLISR                                                    
         BE    PRNT20                                                           
         BAS   RE,PRNTCLNT                                                      
*                                                                               
*                                                                               
         LTR   R3,R3                     IF NO MORE RECS PRINT OTHER            
         BNZ   PRNT20                    TOTS                                   
         BAS   RE,PRNTTOT                                                       
         B     PRNTXIT                                                          
*                                                                               
PRNT20   MVC   OLDADCD,PADCODE           SET SAVED ADCODE                       
         MVC   OLDPRD,PPRODSR            SET SAVED PRODUCT                      
         MVC   OLDCLI,PCLISR             SET SAVED CLIENT                       
*                                                                               
         CLI   QOPT6,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,TRACEOUT                                                      
         GOTO1 =A(GETCLT)                SET CLIENT FOR HEADLINES               
         GOTO1 =A(GETPRD)                SET PRODUCT FOR HEADLINES              
*                                                                               
         XC    P(132),P                                                         
         CLI   RCSUBPRG,11                                                      
         BE    PRNT21                                                           
         CLI   RCSUBPRG,21                                                      
         BNE   PRNT22                                                           
*                                                                               
PRNT21   MVC   P+1(3),PPROD                                                     
*                                                                               
PRNT22   DS    0H                                                               
         MVC   P+71(6),PDOCNUM           DOC# TO PRINT LINE                     
*                                                                               
         MVI   P+77,C'R'                                                        
         CLI   PREBATE,C'Y'                                                     
         BE    PRNT30                                                           
         MVI   P+77,C' '                                                        
         MVC   P+8(6),PADCODE            ADCODE TO PRINT LINE                   
         MVC   P+17(25),PCAP1            CAPTION 1 TO PRINT LINE                
         OC    PCAP2,SPACES              IF CAPTION 2 EXISTS PUT IT TO          
         CLC   PCAP2,SPACES              PRINT LINE                             
         BE    PRNT25                                                           
         MVC   P+149(25),PCAP2                                                  
*                                                                               
PRNT25   XC    WORK,WORK                                                        
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),PPUBCODE),(C'S',WORK)                         
         MVC   P+45(11),WORK                       PUB # TO PRINT LINE          
*                                                                               
         GOTO1 DATCON,DMCB,(3,PINSDATE),(5,P+59)     INS DATE TO PRINT          
         CLI   PINSLIN,X'01'                                                    
         BNH   PRNT27                                                           
         MVI   P+67,C'-'                                                        
         ZIC   R1,PINSLIN                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+68(2),DUB+6(2)                                                 
*                                                                 LINE          
PRNT27   EDIT  POPEN,(14,P+80),2,MINUS=YES       OPEN    TO PRINT LINE          
         EDIT  PCONTRCT,(14,P+97),2,MINUS=YES    CONTRACT       "               
PRNT30   EDIT  PDIFF,(14,P+114),2,MINUS=YES      DIFFERENCE     "               
*                                                                               
*   ROLL TO ADCDTOT                                                             
         L     R4,POPEN                  ROLL OPEN TO ADCODE TOTS               
         CVD   R4,DUB                                                           
         AP    ADCTOPN,DUB                                                      
*                                                                               
         L     R4,PCONTRCT               ROLL CONTRACT TO ADCODE TOTS           
         CVD   R4,DUB                                                           
         AP    ADCTCON,DUB                                                      
*                                                                               
         L     R4,PDIFF                  ROLL DIFFERENCE TO ADCODE TOTS         
         CVD   R4,DUB                                                           
         AP    ADCTDIF,DUB                                                      
*                                                                               
         MVC   H6+9(9),ADCDLAB           ADCODE LABEL TO HEADLINES              
         MVC   H4+51(26),HDLAB                                                  
*                                                                               
PRNT45   GOTO1 REPORT                                                           
         B     PRNT1                                                            
PRNTXIT  XIT1                                                                   
         EJECT                                                                  
*                                                                               
PRNTTOT  NTR1                                                                   
         XC    P(132),P                                                         
         CLI   LINE,61                                                          
         BH    PRNTTOT2                                                         
         MVI   FORCEHED,C'N'                                                    
         MVC   H6+9(9),ADCDLAB           ADCODE LABEL TO HEADLINES              
         MVC   H4+51(26),HDLAB                                                  
PRNTTOT2 GOTO1 REPORT                                                           
         MVC   H6+9(9),ADCDLAB           ADCODE LABEL TO HEADLINES              
         MVC   H4+51(26),HDLAB                                                  
*                                                                               
PRNTTOT3 CLI   RCMULTIQ,C'Y'                     LABEL MEDIA TOTALS             
         BNE   PRNTTOT4                          ONLY IF MULTIMEDIA REQ         
         MVC   P+45(17),=C'**MEDIA  TOTALS**'    ELSE IS REPORT TOTALS          
         B     PRNTTOT8                                                         
PRNTTOT4 MVC   P+45(17),=C'**REPORT TOTALS**'                                   
PRNTTOT8 EDIT  (P8,MEDTOPN),(16,P+78),2,MINUS=YES                               
         EDIT  (P8,MEDTCON),(16,P+95),2,MINUS=YES                               
         EDIT  (P8,MEDTDIF),(16,P+112),2,MINUS=YES                              
*                                                                               
         CLI   RCMULTIQ,C'Y'                     IF MULTIMEDIA ROLL             
         BNE   PRNTTOT9                          MEDIA TOTALS TO REPORT         
         AP    REPTOPN,MEDTOPN                   TOTALS                         
         AP    REPTCON,MEDTCON                                                  
         AP    REPTDIF,MEDTDIF                                                  
*                                                                               
PRNTTOT9 GOTO1 REPORT                                                           
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*                                                                               
PRNTRPT  NTR1                          IF MULTIMEDIA AND THERE ARE NO           
         CLC   REPTOPN,=PL8'0'         REPORT TOTALS (NO DATA TO PRINT)         
         BNZ   PRNTRPT5                SKIP PRINTING REPORT TOTALS.             
         CLC   REPTCON,=PL8'0'                                                  
         BNZ   PRNTRPT5                                                         
         CLC   REPTDIF,=PL8'0'                                                  
         BZ    PRNTRPTX                                                         
*                                                                               
PRNTRPT5 XC    P,P                                                              
         CLI   LINE,61                                                          
         BH    PRNTRTP8                                                         
         MVI   FORCEHED,C'N'                                                    
PRNTRTP8 MVC   H6+9(9),ADCDLAB           ADCODE LABEL TO HEADLINES              
         MVC   H4+51(26),HDLAB                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   H6+9(9),ADCDLAB                                                  
         MVC   H4+51(26),HDLAB                                                  
         MVC   P+45(17),=C'**REPORT TOTALS**'                                   
         EDIT  (P8,REPTOPN),(16,P+78),2,MINUS=YES                               
         EDIT  (P8,REPTCON),(16,P+95),2,MINUS=YES                               
         EDIT  (P8,REPTDIF),(16,P+112),2,MINUS=YES                              
         GOTO1 REPORT                                                           
PRNTRPTX XIT1                                                                   
         EJECT                                                                  
*                                                                               
PRNTADCD NTR1                                                                   
         XC    P(132),P                                                         
         MVC   H6+9(9),ADCDLAB           MOVE ADCODE LABEL TO HEADLINES         
         MVC   H4+51(26),HDLAB                                                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P+8(17),=C'**ADCODE TOTALS**'                                    
         EDIT  (P8,ADCTOPN),(16,P+78),2,MINUS=YES  EDIT OUT ADCODE TOTS         
         EDIT  (P8,ADCTCON),(16,P+95),2,MINUS=YES                               
         EDIT  (P8,ADCTDIF),(16,P+112),2,MINUS=YES                              
         MVC   H6+9(9),ADCDLAB           MOVE ADCODE LABEL TO HEADLINES         
         MVC   H4+51(26),HDLAB                                                  
         GOTO1 REPORT                                                           
         MVC   H6+9(9),ADCDLAB           MOVE ADCODE LABEL TO HEADLINES         
         MVC   H4+51(26),HDLAB                                                  
         GOTO1 REPORT                                                           
         MVC   H6+9(9),ADCDLAB           MOVE ADCODE LABEL TO HEADLINES         
         MVC   H4+51(26),HDLAB                                                  
         GOTO1 REPORT                                                           
*                                                                               
         AP    MEDTOPN,ADCTOPN           ROLL ADCODE TOTS TO MEDIA TOTS         
         AP    MEDTCON,ADCTCON                                                  
         AP    MEDTDIF,ADCTDIF                                                  
         AP    PRDTOPN,ADCTOPN           ROLL ADCODE TOTS TO PROD TOTS          
         AP    PRDTCON,ADCTCON                                                  
         AP    PRDTDIF,ADCTDIF                                                  
         AP    CLITOPN,ADCTOPN           ROLL ADCODE TOTS TO CLI TOTS           
         AP    CLITCON,ADCTCON                                                  
         AP    CLITDIF,ADCTDIF                                                  
*                                                                               
         ZAP   ADCTOPN,=P'0'             INITIALIZE ADCODE TOT FOR NEXT         
         ZAP   ADCTCON,=P'0'             TIME THROUGH                           
         ZAP   ADCTDIF,=P'0'                                                    
         XIT1  1                                                                
         EJECT                                                                  
*                                                                               
PRNTPROD NTR1                                                                   
         GOTO1 REPORT                                                           
         MVC   H6+9(9),ADCDLAB           MOVE ADCODE LABEL TO HEADLINES         
         MVC   H4+51(26),HDLAB                                                  
         MVC   P+8(18),=C'**PRODUCT TOTALS**'                                   
         EDIT  (P8,PRDTOPN),(16,P+78),2,MINUS=YES              EDIT OUT         
         EDIT  (P8,PRDTCON),(16,P+95),2,MINUS=YES                               
         EDIT  (P8,PRDTDIF),(16,P+112),2,MINUS=YES                              
         GOTO1 REPORT                    FOR PRODUCT TOTALS IS                  
         MVI   FORCEHED,C'Y'             ONE PRODUCT PER PAGE                   
*                                                                               
         ZAP   PRDTOPN,=P'0'             INITIALIZE ADCODE TOT FOR NEXT         
         ZAP   PRDTCON,=P'0'             TIME THROUGH                           
         ZAP   PRDTDIF,=P'0'                                                    
         XIT1  1                                                                
         EJECT                                                                  
*                                                                               
PRNTCLNT NTR1                                                                   
         CLI   LINE,61                                                          
         BH    PCLNT5                                                           
         MVI   FORCEHED,C'N'                                                    
PCLNT5   GOTO1 REPORT                                                           
         MVC   H6+9(9),ADCDLAB           MOVE ADCODE LABEL TO HEADLINES         
         MVC   H4+51(26),HDLAB                                                  
         MVC   P+8(17),=C'**CLIENT TOTALS**'                                    
         EDIT  (P8,CLITOPN),(16,P+78),2,MINUS=YES            EDIT OUT           
         EDIT  (P8,CLITCON),(16,P+95),2,MINUS=YES       CLIENT TOTALS           
         EDIT  (P8,CLITDIF),(16,P+112),2,MINUS=YES                              
         GOTO1 REPORT                    FOR CLIENT TOTALS IS                   
         MVI   FORCEHED,C'Y'             ONE CLIENT PER PAGE                    
*                                                                               
         ZAP   CLITOPN,=P'0'             INITIALIZE ADCODE TOT FOR NEXT         
         ZAP   CLITCON,=P'0'             TIME THROUGH                           
         ZAP   CLITDIF,=P'0'                                                    
         XIT1  1                                                                
         EJECT                                                                  
*                                                                               
GETCAP   NTR1                                                                   
         MVC   WORK(64),KEY                                                     
         XC    KEY,KEY                   BUILD KEY TO READ CAPTION KEY          
         MVC   KEY(10),WORK                                                     
         MVI   KEY+3,X'15'                                                      
         MVC   KEY+10(6),PADCODE                                                
         GOTO1 HIGH                                                             
         CLC   KEY(22),KEYSAVE           COMPARE FOR 22 BYTES - PUB             
         BE    *+6                       SHOULD BE 6X'00'                       
         DC    H'0'                                                             
*                                                                               
         LA    R0,PJOBREC                GET CAPTION REC                        
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,PJOBREC+33            GET '15' ELEMENT IN CAPTION REC         
         USING PJOBELEM,R2                                                      
         CLI   0(R2),X'15'                                                      
         BE    GETCAP10                                                         
*                                                                               
         MVI   ELCODE,X'15'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   GETCAP20                                                         
*                                                                               
GETCAP10 MVC   PCAP1,PJOBCAP1           MOVE CAPTION 1 AND 2 TO SORT            
         MVC   PCAP2,PJOBCAP2           REC                                     
*                                                                               
GETCAP20 MVC   KEY(64),WORK             RESTORE KEYS AND DIRECTORY              
         GOTO1 HIGH                     POINTER                                 
GETCAPX  XIT1  1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
GETCLT   NTR1                                                                   
         CLC   QCLIENT,=C'   '                                                  
         BE    GCLIX                                                            
*                                                                               
*        CLI   QCLIENT,C'*'                                                     
*        BNE   GCLI10                                                           
*        MVC   H3(6),=C'OFFICE'                                                 
*        MVC   H3+9(2),QCLIENT+1                                                
*        B     GCLIX                                                            
*                                                                               
GCLI10   XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         CLI   QMEDIA,C'C'                                                      
         BNE   GCLI20                                                           
         MVI   KEY+2,C'M'                                                       
GCLI20   MVI   KEY+3,X'02'                                                      
         MVC   KEY+4(3),PCLI                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BE    GCLIGET                                                          
         MVI   KEYSAVE+2,C'N'                                                   
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(7),KEYSAVE                                                   
         BNE   GCLIDIE                                                          
*                                                                               
GCLIGET  LA    R0,PCLTREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         TM    DMCB+8,X'FF'                                                     
         BZ    GCLIX                                                            
GCLIDIE  DC    H'0'                                                             
GCLIX    XIT1  1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
GETPRD   NTR1                                                                   
         CLC   QPRODUCT,=C'   '                                                 
         BE    GPRDX                                                            
*                                                                               
GPRD10   XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         CLI   QMEDIA,C'C'                                                      
         BNE   GPRD20                                                           
         MVI   KEY+2,C'M'                                                       
GPRD20   MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLI                                                    
         MVC   KEY+7(3),PPROD                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BE    GPRDGET                                                          
         MVI   KEYSAVE+2,C'N'                                                   
         MVC   KEY,KEYSAVE                                                      
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE                                                  
         BNE   GPRDDIE                                                          
*                                                                               
GPRDGET  LA    R0,PPRDREC                                                       
         ST    R0,AREC                                                          
         GOTO1 GETPRT                                                           
         TM    DMCB+8,X'FF'                                                     
         BZ    GPRDX                                                            
GPRDDIE  DC    H'0'                                                             
GPRDX    XIT1  1                                                                
         EJECT                                                                  
*                                                                               
*                                                                               
NEXTEL   DS    0H                                                               
         CLI   0(R2),0                                                          
         BE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R2)                                                         
         AR    R2,R0                                                            
         CLC   0(1,R2),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                 SET CONDITION CODE TO NOT EQUAL            
         BR    RE                                                               
*                                                                               
*                                  READ SEQ                                     
*                                                                               
TRACEIN  NTR1                                                                   
         MVC   P+1(8),=C'SORT IN='                                              
         GOTO1 HEXOUT,DMCB,(R3),P+11,70                                         
         GOTO1 REPORT                                                           
         XIT1  1                                                                
*                                                                               
TRACEOUT NTR1                                                                   
         MVC   P+1(9),=C'SORT OUT='                                             
         GOTO1 HEXOUT,DMCB,(R3),P+11,70                                         
         GOTO1 REPORT                                                           
         XIT1  1                                                                
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
B2PROF   DS    CL16                  B2 BILLING PROFILE                         
*                                    USED BY GETCOST                            
*                                                                               
ADCDLAB  DS    CL9                   TO HOLD ADCODE LABE FOR HEADLINES          
HDLAB    DS    CL26                                                             
ELCODE   DS    CL1                                                              
SV26EL   DS    CL8                                                              
ASORTC   DC    A(SORTC)                                                         
ELADD    DS    F                                                                
COMPRE   DS    F                                                                
VGETCAP  DS    V                                                                
DIFFAMT  DS    F                                                                
REBAMT   DS    F                                                                
PPGKEY   DS    CL64                                                             
BFORM    DS    CL5                                                              
RDSW     DS    CL1                                                              
*                                                                               
USEBUY   DS    CL1                                                              
COUNTIN  DS    PL2                                                              
SVQSTRT  DS    CL6                                                              
SVQEND   DS    CL6                                                              
SBQSTRT  DS    CL3              BINARY START                                    
SBQEND   DS    CL3              BINARY END                                      
OLDADCD  DS    CL6                                                              
OLDCLI   DS    CL3                                                              
OLDPRD   DS    CL3                                                              
*                                                                               
         DS    0D                                                               
ADCDTOT  DS    0CL24                                                            
ADCTOPN  DS    CL8                                                              
ADCTCON  DS    CL8                                                              
ADCTDIF  DS    CL8                                                              
MEDTOTS  DS    0CL24                                                            
MEDTOPN  DS    CL8                                                              
MEDTCON  DS    CL8                                                              
MEDTDIF  DS    CL8                                                              
PRDTOTS  DS    0CL24                                                            
PRDTOPN  DS    CL8                                                              
PRDTCON  DS    CL8                                                              
PRDTDIF  DS    CL8                                                              
CLITOTS  DS    0CL24                                                            
CLITOPN  DS    CL8                                                              
CLITCON  DS    CL8                                                              
CLITDIF  DS    CL8                                                              
REPTOTS  DS    0CL24                                                            
REPTOPN  DS    CL8                                                              
REPTCON  DS    CL8                                                              
REPTDIF  DS    CL8                                                              
*                                                                               
PRNTCSC  DS    CL97                                                             
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(13,28,A),FORMAT=BI,WORK=1 '                    
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(97,,,,) '                             
SORTC    DS    0D                                                               
         DS    61000C                                                           
*                                                                               
PRNTDSC  DSECT                                                                  
PCONTRCT DS    F              FROM '26' EL                                      
POPEN    DS    F              FROM '28' EL                                      
PDIFF    DS    F                                                                
PCLISR   DS    CL3                                                              
PPRODSR  DS    CL3                                                              
PADCODE  DS    CL6                                                              
PPUBCODE DS    CL6            CODE,ZONE,EDITION                                 
PINSDATE DS    CL3            BINARY                                            
PINSLIN  DS    CL1            LINE NUMBER                                       
PDOCNUM  DS    CL6            FROM '26' EL                                      
PREBATE  DS    CL1                                                              
PCAP1    DS    CL25                                                             
PCAP2    DS    CL25                                                             
PPROD    DS    CL3                                                              
PCLI     DS    CL3                                                              
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU            REPORT MODES                               
       ++INCLUDE PPNEWFILE           HAVE NEW PBILLREC DSECT                    
       ++INCLUDE PPREPWORK           GENERAL WORK SPACE                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'174PPREPRA02 01/05/12'                                      
         END                                                                    
