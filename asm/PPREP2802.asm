*          DATA SET PPREP2802  AT LEVEL 013 AS OF 07/09/14                      
*$PANAPT01S$ *******  FIRST LINE TO DELETE  *****************                   
*                                                           *                   
*  ATTN: THE LEVEL STAMPS ARE *LEGITIMATELY* OUT-OF-SYNC!   *                   
*        DELETE THIS COMMENT BLOCK ON THE NEXT PROMOTION!   *                   
*                                                           *                   
*  THIS SOURCE MODULE IS AT A HIGHER LEVEL NUMBER THAN ITS  *                   
*  CORRESPONDING LOAD OR OBJECT MODULE, BECAUSE THE MEMBER  *                   
*  WAS PROMOTED USING THE SRCE LIBCODE VIA MR# 044155.      *                   
*                                                           *                   
*  THIS COMMENT BLOCK WAS INSERTED *PROGRAMMATICALLY* BY    *                   
*  PANAPT, TO EXPLAIN WHY THE LEVEL STAMPS ARE OUT-OF-SYNC. *                   
*  BEFORE THIS MEMBER CAN BE PROMOTED AGAIN, THIS ENTIRE    *                   
*  COMMENT BLOCK *MUST* BE MANUALLY DELETED.                *                   
*                                                           *                   
*$PANAPT01X$ *******  LAST LINE TO DELETE  ******************                   
*PHASE PP2802A                                                                  
*INCLUDE PUBFLOAT                                                               
*INCLUDE PRNTOFC                                                                
         PRINT NOGEN                                                            
         TITLE 'PP2802 - CLIENT/VENDOR SUMMARY'                                 
*        CHANGE LOG                                                             
*                                                                               
* SMYE  05/04/06 INCREASE PRDTAB FROM CL23000 (1000 PROD'S) TO CL34500          
*                (1500 PROD'S)                                                  
*                                                                               
* BOBY  11/05    2 CH MEDIA OFFICE CODES                                        
*                                                                               
* SMYE  01/15/03 INCREASE PRDTAB FROM CL13800 (600 PROD'S) TO CL23000           
*                (1000 PROD'S)                                                  
*                                                                               
* SMYE  02/19/02 INCREASE SIZE OF PAYWORK CSECT IN ACCORDANCE WITH              
*                INCREASE IN PRDTAB FROM 500 TO 600 PRODUCTS                    
*                PAYWORK LENGTH IS NOW SOFT CODED TO CHANGE                     
*                AS PAYWORKD (INCLUDING PRDTAB) CHANGES                         
*                                                                               
* KWAN 01/04/00  USE DATACON TO CONVERT QPAY AND RCDATE INTO SAME               
*                DATE FORMAT SO THAT CLC WILL WORK PROPERLY                     
*                                                                               
* KWAN 06/22/99  ALTER CD/NON-CD PUBS ONLY FEATURE TO IGNORE                    
*                CD EFFECTIVE DATE IF IT 3 MONTHS OR MORE                       
*                BEFORE THE REQUEST START DATE                                  
*                                                                               
* SMYE 01/12/96  MODIFIED MTHTAB HANDLING FOR YEARS AFTER 1999                  
*                                                                               
* SMYE 12/12/95  CHANGED DTCNV TO DATCON WITH NEW PARAM'S                       
*                                                                               
* BPLA 10/4/93   IF QBPDATE IS "B" USE PBLBDATE INSTEAD OF PBUYKDAT             
*                TO CHECK FOR MTH CHANGES (MTHEND)                              
*                                                                               
* BPLA 10/20/92  NEW PROFILE OPTION FOR "FREE" INSERTIONS                       
*            PROGPROF+1  "Y" = SUPPRESS "FREE" BUYS ON UNPAID REQS              
*            PROGPROF+2  "Y" = SUPPRESS "FREE" BUYS ON PAID REQS                
*            PROGPROF+3  "Y" = SUPPRESS "FREE" BUYS ON ALL ITEM REQS            
*                                                                               
PP2802   CSECT                                                                  
         NMOD1 0,PP2802,RR=R9                                                   
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,=V(PAYWORK)                                                   
         A     R5,RELO                                                          
         USING PAYWORKD,R5                                                      
*                                                                               
         CLI   MODE,FBUYREQ                                                     
         BNE   CKMODE                                                           
         BAS   R9,INITIAL                                                       
         GOTO1 VBLDMLST              BUILD MTH LIST                             
         B     EXT                                                              
*                                                                               
CKMODE   CLI   MODE,FBUYCLI                                                     
         BNE   CKM2                                                             
         GOTO1 VCLIFRST                                                         
         B     EXT                                                              
*                                                                               
CKM2     DS    0H                                                               
*                                                                               
CKM3     CLI   MODE,FBUYPUB                                                     
         BNE   CKM4                                                             
         GOTO1 VPUBFRST                                                         
         B     EXT                                                              
*                                                                               
CKM4     CLI   MODE,PROCBUY                                                     
         BNE   CKM6                                                             
         B     PROCESS                                                          
*                                                                               
CKM6     CLI   MODE,LBUYPUB                                                     
         BNE   CKM8                                                             
         GOTO1 VPUBEND                                                          
         B     EXT                                                              
*                                                                               
CKM8     CLI   MODE,LBUYPRO                                                     
         BNE   CKM9                                                             
         GOTO1 VPRDEND                                                          
         B     EXT                                                              
*                                                                               
CKM9     CLI   MODE,LBUYCLI                                                     
         BNE   CKM11                                                            
         GOTO1 VCLTEND                                                          
         B     EXT                                                              
*                                                                               
CKM11    CLC   QSORT,=C'08'                                                     
         BE    CKM11A                                                           
         CLC   QSORT,=C'09'                                                     
         BNE   EXT                                                              
CKM11A   CLI   MODE,LBUYREP                                                     
         BNE   EXT                                                              
         GOTO1 VREPEND                                                          
         B     EXT                                                              
         EJECT                                                                  
PROCESS  EQU   *                                                                
         OC    KEY+21(3),KEY+21       IGNORE PASSIVE POINTERS                   
         BNZ   EXT                                                              
         CLI   PUBSW,0             SEE IF DOING THIS PUB                        
         BNE   EXT                 NO                                           
*                                                                               
PROC12   DS    0H                                                               
         CLI   QBPDATE,C'B'        SEE IF REPORTING BILLABLE MTHS               
         BNE   PROC12D                                                          
         CLC   LASTYM,PBDBDATE                                                  
         BE    *+10                                                             
         GOTO1 VMTHEND                                                          
         MVC   LASTYM,PBDBDATE                                                  
         B     PROC12X                                                          
*                                                                               
PROC12D  CLC   LASTYM,PBUYKDAT     CHECK FOR CHANGE OF MONTH                    
         BE    *+10                                                             
         GOTO1 VMTHEND                                                          
         MVC   LASTYM,PBUYKDAT                                                  
*                                                                               
PROC12X  DS    0H                                                               
CKQOPT1  DS    0H                                                               
         CLI   QOPT1,C' '      SEE IF DOING PAID OR UNPAID REQ                  
         BNE   CKOPT1A                                                          
         CLI   PROGPROF+3,C'Y' SUPPRESSING FREE BUYS ON ALL ITEM REQ            
         BNE   CKOPT1A                                                          
         OC    GROSS(12),GROSS                                                  
         BZ    NEXTBUY                                                          
*                                                                               
CKOPT1A  MVI   PAIDSW,0       WILL BE SET TO X'01'                              
*                             IF DATED PAY ELEM IS FOUND                        
         LA    R3,PBDELEM                                                       
         MVI   ELCODE,X'25'                                                     
CKQOPT1A BAS   RE,NEXTEL                                                        
         BNE   CKQOPT1D                                                         
         USING PPDUMD03,R3                                                      
         OC    PPDDATE,PPDDATE                                                  
         BZ    CKQOPT1A                                                         
         CLI   ASOFDTE,0             SEE IF I HAVE AN AS OF DATE                
         BE    CKQOPT1C                                                         
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1A                                                         
*                                                                               
CKQOPT1C MVI   PAIDSW,X'01'       SET DATE PAY ELEM FOUND                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1E         GO PROCESS THIS ELEM                            
*                                                                               
CKQOPT1D DS    0H        I GET HERE IF NO VALID PAY ELEMS                       
         CLI   ASOFDTE,0                                                        
         BE    CKQOPT1X         NOT USING AS OF DATE                            
         XC    PGROSS(16),PGROSS                                                
         B     CKQOPT1X                                                         
*                                                                               
CKQOPT1E DS    0H                                                               
         CLC   PPDDATE(3),ASOFDTE                                               
         BH    CKQOPT1F                                                         
         LM    R7,R9,PGROSS                                                     
         A     R7,PPGROSS                                                       
         A     R8,PPAGYCOM                                                      
         A     R9,PPCSHDSC                                                      
         STM   R7,R9,PGROSS                                                     
         LM    R7,R9,PPGROSS                                                    
         SR    R7,R8                                                            
         SR    R7,R9                                                            
         L     R6,PAID                                                          
         AR    R6,R7                                                            
         ST    R6,PAID                                                          
CKQOPT1F BAS   RE,NEXTEL                                                        
         BE    CKQOPT1E                                                         
         B     CKQOPT1X                                                         
*                                                                               
*                                                                               
NEXTEL   DS    0H       GET  NEXT ELEMENT                                       
         CLI   0(R3),0                                                          
         BE    NEXTELX                                                          
         ZIC   R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLC   0(1,R3),ELCODE                                                   
         BER   RE                                                               
         B     NEXTEL                                                           
NEXTELX  LTR   RE,RE                                                            
         BR    RE                                                               
*                                                                               
         DROP  R3                                                               
*                                                                               
CKQOPT1X TM    PBUYCNTL,X'80'        SEE IF DELETED                             
         BZ    CKPD                                                             
         OC    PGROSS(12),PGROSS                                                
         BZ    NEXTBUY              NOT PAID SO IGNORE                          
         XC    GROSS(20),GROSS                                                  
CKPD     CLI   QOPT1,C'P'                                                       
         BNE   CKUNPD                                                           
         OC    PGROSS(12),PGROSS                                                
         BNZ   CKPD5                                                            
         CLI   PAIDSW,X'01'       SEE IF I FOUND A DATED PAY ELEM               
         BNE   NEXTBUY                                                          
*                                                                               
         OC    GROSS(12),GROSS    SEE IF FREE BUY                               
         BNZ   CKPD5                                                            
*                                                                               
         CLI   PROGPROF+2,C'Y'  SUPPRESSING "FREE" BUYS ON PAID REQ             
         BE    NEXTBUY                                                          
*                                                                               
CKPD5    B     BUYOK                                                            
*                                                                               
CKUNPD   CLI   QOPT1,C'U'                                                       
         BNE   BUYOK        DOING ALL ITEMS                                     
         CLC   GROSS(12),PGROSS                                                 
         BNE   CKUNPD5                                                          
         OC    GROSS(12),GROSS     SEE IF "FREE" BUY                            
         BNE   NEXTBUY                                                          
         CLI   PAIDSW,X'01'        SEE IF I HAVE A DATED PAY ELEM               
         BE    NEXTBUY             YES THEN SKIP                                
*                                                                               
         CLC   PBUYKDAT,=X'5C0901'   SEE IF BEFORE SEP01/92                     
         BL    NEXTBUY               THEN DON'T REPORT AS UNPAID                
*                                                                               
         CLI   PROGPROF+1,C'Y'  SEE IF SKIPPING "FREE" ON UNPAID REQ            
         BE    NEXTBUY                                                          
*                                                                               
CKUNPD5  B     BUYOK                                                            
*                                                                               
BUYOK    DS    0H                                                               
         CLC   SAVEPUB,PBUYKPUB                                                 
         BE    BUYOK5                                                           
         BAS   RE,PRNTPUB                                                       
BUYOK5   CLC   SAVEPRD,PBUYKPRD                                                 
         BE    *+8                                                              
         BAS   RE,PRDSTRT                                                       
         MVC   MTHACT(5),=5C'Y'    SET ACTIVITY SWITCHES                        
*                                                                               
PRTNEW   LM    R6,R9,GROSS                                                      
         STM   R6,R9,BUYGO                                                      
         SR    R6,R7                                                            
         ST    R6,BUYGLAC                                                       
         OC    PGROSS(12),PGROSS                                                
         BZ    RLMTH                    ADD TO MTH TOTALS                       
         CLI   QOPT1,C'P'                                                       
         BNE   PRTPPD                                                           
         CLC   GROSS(12),PGROSS                                                 
         BE    RLMTH               TOTALLY PAID SO ADD TO MTH ACCUMS            
*                                                                               
*              SUBTRACT PGROSS FROM GROSS AND PUT IN PGROSS SO THAT             
*              PGROSS,PAGYCOM,PCSHDSC,PAID WILL REFLECT UNPAID AMTS             
*                                                                               
         L     R3,GROSS                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,PGROSS                                                        
         L     R3,AGYCOM                                                        
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
         L     R3,CSHDSC                                                        
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,PCSHDSC                                                       
         L     R3,PYABLE                                                        
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,PAID                                                          
PRTPPD   CLI   QOPT1,C'U'                                                       
         BE    *+12                                                             
         CLI   QOPT1,C'P'                                                       
         BNE   RLMTH        IF DOING ALL ITEMS - NO ADJUSTMENT                  
*                                                                               
         LM    R3,R4,PGROSS                                                     
         SR    R3,R4                                                            
         ST    R3,PAGYCOM                                                       
*                                                                               
         L     R3,BUYGO                                                         
         L     R4,PGROSS                                                        
         SR    R3,R4                                                            
         ST    R3,BUYGO                                                         
         L     R3,BUYGLAC                                                       
         L     R4,PAGYCOM                                                       
         SR    R3,R4                                                            
         ST    R3,BUYGLAC                                                       
         L     R3,BUYCD                                                         
         L     R4,PCSHDSC                                                       
         SR    R3,R4                                                            
         ST    R3,BUYCD                                                         
         L     R3,BUYNP                                                         
         L     R4,PAID                                                          
         SR    R3,R4                                                            
         ST    R3,BUYNP                                                         
*                                                                               
RLMTH    CLI   PBDSPACE,C'*'      SEE IF REAL INSERTION                         
         BE    RLMTH1              NO - BYPASS INS TOTALS                       
         AP    MTHINS,=P'1'                                                     
RLMTH1   CLI   QMEDIA,C'N'         IF MAG                                       
         BNE   RLMTH2              BYPASS LINES                                 
RLM2A    CLI   PROGPROF,C'I'                                                    
         BNE   RLM2C                                                            
         CLI   PBDUIND,X'89'                                                    
         BE    RLMTH1B                                                          
         CLI   PBDUIND,C'I'                                                     
         BE    RLM2B                                                            
         L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'1000'                                                     
         DP    DUB,=P'14'                                                       
         SRP   DUB(6),63,5                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
RLM2B    L     R6,UNITS                                                         
         CVD   R6,DUB                                                           
         MP    DUB,=P'100'                                                      
         CVB   R6,DUB                                                           
         ST    R6,UNITS                                                         
         B     RLMTH1B                                                          
*          DATA SET PPREP2702  AT LEVEL 033 AS OF 03/01/84                      
RLM2C    CLI   PBDUIND,C'I'        CONVERT INCHES TO LINES FOR ACCUMS           
         BE    RLMTH1A                                                          
         CLI   PBDUIND,X'89'       CONVERT INCHES TO LINES FOR ACCUMS           
         BNE   RLMTH1B             LOWER CASE I INCHES TO 2 DECIMALS            
RLMTH1A  SR    R6,R6                                                            
         L     R7,UNITS                                                         
         M     R6,=F'14'                                                        
         ST    R7,UNITS                                                         
         CLI   PBDUIND,X'89'       SEE IF LOWER CASE I                          
         BNE   RLMTH1B             NO                                           
         CVD   R7,DUB                                                           
         AP    DUB,=P'50'          MUST ROUND TO NEAREST LINE                   
         DP    DUB,=P'100'                                                      
         ZAP   DUB,DUB(6)                                                       
         CVB   R7,DUB                                                           
         ST    R7,UNITS                                                         
*                                                                               
*                                                                               
RLMTH1B  EQU   *                                                                
         L     R3,UNITS                                                         
         CVD   R3,DUB                                                           
         AP    MTHLINES,DUB                                                     
RLMTH2   L     R3,BUYGO                                                         
         CVD   R3,DUB                                                           
         AP    MTHGO,DUB                                                        
         L     R3,BUYGLAC                                                       
         CVD   R3,DUB                                                           
         AP    MTHGLAC,DUB                                                      
         L     R3,BUYCD                                                         
         CVD   R3,DUB                                                           
         AP    MTHCD,DUB                                                        
         L     R3,BUYNP                                                         
         CVD   R3,DUB                                                           
         AP    MTHNP,DUB                                                        
*                                                                               
         MVC   SAVEYMD,BLBLDT        BILLABLE DATE                              
         CLI   QBPDATE,C'B'                                                     
         BE    NEXTBUY                                                          
         MVC   SAVEYMD,PBUYKDAT                                                 
         B     NEXTBUY                                                          
*                                                                               
*                                                                               
*                                                                               
NEXTBUY  DS    0H                                                               
         B     EXT                                                              
         EJECT                                                                  
INITIAL  XC    MTHTAB(250),MTHTAB       LINKED VIA R9.                          
         XC    MTHTAB+250(L'MTHTAB-250),MTHTAB+250                              
         OI    DMINBTS,X'08'       SET TO PASS DELETES                          
         L     RF,=V(MTHEND)           RELOCATE ROUTINES                        
         A     RF,RELO                                                          
         ST    RF,VMTHEND                                                       
         L     RF,=V(PRDEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPRDEND                                                       
         L     RF,=V(PUBEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VPUBEND                                                       
         L     RF,=V(CLTEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VCLTEND                                                       
         L     RF,=V(PRINTIT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPRINTIT                                                      
         L     RF,=V(BLDMLST)                                                   
         A     RF,RELO                                                          
         ST    RF,VBLDMLST                                                      
         L     RF,=V(PAYWORK)                                                   
         A     RF,RELO                                                          
         ST    RF,VPAYWORK                                                      
         L     RF,=V(CLISTRT)                                                   
         A     RF,RELO                                                          
         ST    RF,VCLIFRST                                                      
         L     RF,=V(PUBSTRT)                                                   
         A     RF,RELO                                                          
         ST    RF,VPUBFRST                                                      
         L     RF,=V(REPEND)                                                    
         A     RF,RELO                                                          
         ST    RF,VREPEND                                                       
         L     RF,=V(REPCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VREPCSCT                                                      
         L     RF,=V(CLTCSCT)                                                   
         A     RF,RELO                                                          
         ST    RF,VCLTCSCT                                                      
*                                                                               
*        GET A(OFFICER)                                                         
*                                                                               
         XC    DMCB(12),DMCB                                                    
*                                                                               
         MVC   DUB,SPACES          GET OFFICER                                  
         MVC   DUB(6),=C'T00A38'                                                
         GOTO1 LOADER,DMCB,DUB,0                                                
*                                                                               
         MVC   VOFFICER,4(R1)      SAVE ADDRESS                                 
*                                                                               
         L     RE,=V(PRNTOFC)                                                   
         ST    RE,VPRNTOFC         STORE PRNTOFC ADDRESS                        
*                                                                               
         MVI   FCRDBUY,C'Y'        RESET TO READ 20 POINTERS                    
         MVI   FCGTJOB,C'N'        RESET JOB RECORD READ                        
         CLC   QPRODUCT(3),=C'   '                                              
         BNE   *+10                                                             
         MVC   QPRODUCT(3),=C'ALL'                                              
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   *+8                                                              
         MVI   FCRDBUY,X'21'       USE 21 POINTERS                              
         JIF   QOPT4,EQ,C' ',OR,QOPT4,EQ,C'A',INIT1,JUMP=N                      
         MVI   FCGTJOB,C'Y'        SO PPG WILL GET JOBRECS                      
*                                                                               
INIT1    DS    0H                                                               
         MVI   FCGTREP,C'N'                                                     
         CLC   QSORT,=C'08'        OR SORTING BY REP                            
         BE    INIT2                                                            
         CLC   QSORT,=C'09'        OR SORTING BY PAY ADDR NAME                  
         BE    INIT2                                                            
         B     INIT2B                                                           
INIT2    MVI   FCGTREP,C'Y'                                                     
INIT2B   CLI   QPAY,C' '           SEE IF USING AS OF DATE                      
         BE    INIT3                                                            
         MVC   DUB(2),RCDATE+6                                                  
         MVC   DUB+2(2),RCDATE+0                                                
         MVC   DUB+4(2),RCDATE+3                                                
*                                                                               
         GOTO1 DATCON,DMCB,(0,QPAY),(0,QPAY)                                    
         GOTO1 DATCON,DMCB,(0,DUB),(0,DUB)                                      
*                                                                               
         CLC   QPAY(6),DUB         SEE IF AS OF DATE IS TODAY                   
         BNL   INIT3               OR FUTURE - YES CAN USE FCPDFILT             
         B     INIT4                                                            
*                                                                               
INIT3    MVC   FCPDFILT,QOPT1      P=PAID,U=UNPAID                              
         CLI   QOPT1,C' '                                                       
         BNE   *+8                                                              
INIT4    MVI   FCPDFILT,C'N'       RESET TO N                                   
         MVC   PAGE,=H'1'                                                       
         MVI   REQERR,0                                                         
         BR    R9                                                               
         EJECT                                                                  
PDIRRD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    PDIRX                                                            
         DC    H'0'                                                             
PDIRX    XIT                                                                    
*                                                                               
*                                                                               
PFILERD  NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    PFILX                                                            
         DC    H'0'                                                             
PFILX    XIT                                                                    
         EJECT                                                                  
PRDSTRT  NTR                                                                    
*                                  FIND AND PRINT PRD FROM PRDTAB               
         MVC   P+1(7),=C'PRODUCT'                                               
         MVC   P+9(3),PBUYKPRD                                                  
         LA    R6,PRDTAB                                                        
PRDF1    CLC   0(3,R6),=X'FFFFFF'                                               
         BNE   *+6                                                              
         DC    H'0'                PRD NOT FOUND                                
         CLC   0(3,R6),PBUYKPRD                                                 
         BE    PRDF4                                                            
         LA    R6,23(R6)                                                        
         B     PRDF1                                                            
*                                                                               
PRDF4    MVC   P+14(20),3(R6)                                                   
         IC    R0,LINE                                                          
         AH    R0,=H'3'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         MVC   SAVEPRD,PBUYKPRD                                                 
*                                                                               
PRDFX    XIT                                                                    
         EJECT                                                                  
PRNTPUB  NTR                                                                    
         CLI   QOPT2,C'Y'     YES= SKIP ON NEW PUB                              
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'6'                                                         
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         MVC   P+1(17),SPACES                                                   
         IC    R3,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R3),PBUYKPUB),(0,P+1),RR=RELO                     
         CLI   FORCEHED,C'Y'                                                    
         BE    SETR3               DONT SKIP A LINE                             
         MVC   WORK(20),P                                                       
         MVC   P(20),SPACES                                                     
         GOTO1 VPRINTIT                                                         
         MVC   P(20),WORK                                                       
SETR3    LA    R3,P+1                                                           
         LA    R4,PSECOND+1                                                     
SETR3A   CLI   0(R3),C' '                                                       
         BE    FLOAT                                                            
         MVI   0(R4),C'-'                                                       
         LA    R4,1(R4)                                                         
         LA    R3,1(R3)                                                         
         B     SETR3A                                                           
*                                                                               
FLOAT    LA    R3,1(R3)                                                         
         GOTO1 =V(PUBFLOAT),DMCB,PUBREC,(R3),RR=RELO                            
         CLC   QSORT,=C'08'        SEE IF DOING REP SORT                        
         BE    FLOATA1                                                          
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BE    FLOATA1                                                          
         GOTO1 VPRINTIT                                                         
*                                                                               
FLOATA1  MVC   SAVEPUB,PBUYKPUB                                                 
*                                                                               
PRNTP2C  DS    0H                                                               
         CLC   QSORT,=C'08'        SEE IF DOING REP SORT                        
         BE    PRNTP2C5                                                         
         CLC   QSORT,=C'09'        OR PAY ADDR NAME                             
         BE    PRNTP2C5                                                         
         B     PRNTPX                                                           
PRNTP2C5 CLI   PREPNAME,C' '       SEE IF ADDR FOUND                            
         BH    PRNTP2C6            YES                                          
         MVC   1(20,R4),=C'** PAY PUB DIRECT **'                                
         GOTO1 VPRINTIT                                                         
         B     PRNTPX                                                           
*                                                                               
PRNTP2C6 CLI   PREPKEY+3,X'11'     SEE IF I FOUND A REP                         
         BNE   PRNTP2F             NO WAS PAY ADDR                              
         MVC   1(4,R4),=C'REP='                                                 
         MVC   6(4,R4),PREPKREP                                                 
         CLI   PREPKREP+4,0                                                     
         BE    PRNTP2D                                                          
         MVI   10(R4),C'.'                                                      
         MVC   11(1,R4),PREPKREP+4    SUFFIX                                    
PRNTP2D  GOTO1 VPRINTIT                                                         
         LR    R4,R3                                                            
         BCTR  R4,0                                                             
*                                                                               
PRNTP2F  MVC   1(30,R4),PREPNAME                                                
         GOTO1 VPRINTIT                                                         
PRNTPX   DS    0H                                                               
         XIT                                                                    
*                                                                               
         DROP  R7                                                               
*                                                                               
EXT      XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
BLDMLST  CSECT                                                                  
         NMOD1 0,BLDMLST                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
SPRDOK   MVC   WORK(12),QSTART       SAVE STRT AND END                          
         CLI   QBPDATE,C'B'                                                     
         BE    BILLPAY                                                          
         CLI   QBPDATE,C'P'                                                     
         BE    BILLPAY                                                          
BLDLIST  LA    R6,ACCNUM                SET FOR BCT                             
         LA    R4,MTHTAB                                                        
         MVC   0(4,R4),QSTART                                                   
PUTMTH   PACK  DUB,2(2,R4)                                                      
         AP    DUB,=P'1'                                                        
         CP    DUB,=P'13'                                                       
         BL    SAMEYR                                                           
         SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         PACK  DUB,0(2,R4)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'           ADD 1 TO YR                                  
*        OI    DUB+7,X'0F'                                                      
         UNPK  8(2,R4),DUB+6(2)                                                 
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE             EXIT                                         
         XC    8(8,R4),8(R4)       CLEAR LAST MTH                               
         B     MTHDONE             EXIT                                         
*                                                                               
SAMEYR   OI    DUB+7,X'0F'                                                      
         UNPK  10(2,R4),DUB+6(2)                                                
         MVC   8(2,R4),0(R4)                                                    
         CLC   8(4,R4),QEND                                                     
         BL    NEXTMTH                                                          
         BE    MTHDONE                                                          
         XC    8(8,R4),8(R4)                                                    
         B     MTHDONE             EXIT                                         
*                                                                               
NEXTMTH  LA    R4,8(R4)                                                         
         BCT   R6,PUTMTH           ACCNUM MTHS MAX                              
         B     MTHDONE             EXIT                                         
*                                                                               
*                                                                               
*                                                                               
BILLPAY  PACK  DUB,QSTART+2(2)     SET START DATE BACK 3 MONTHS                 
         SP    DUB,=P'3'                                                        
         CP    DUB,=P'0'                                                        
         BNH   CHGSYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         B     CHGEND                                                           
*                                                                               
CHGSYR   AP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QSTART+2(2),DUB+6(2)                                             
         PACK  DUB,QSTART(2)                                                    
         BAS   RE,BLDMDNYR         SUBTRACT 1 FROM YR                           
*        SP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QSTART(2),DUB+6(2)                                               
*                                                                               
CHGEND   PACK  DUB,QEND+2(2)                                                    
         AP    DUB,=P'6'      ADVANCE END DATE 6 MONTHS                         
         CP    DUB,=P'12'                                                       
         BH    CHGEYR                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         B     BLDLIST                                                          
*                                                                               
CHGEYR   SP    DUB,=P'12'                                                       
         OI    DUB+7,X'0F'                                                      
         UNPK  QEND+2(2),DUB+6(2)                                               
         PACK  DUB,QEND(2)                                                      
         BAS   RE,BLDMUPYR         ADD 1 TO YR                                  
*        AP    DUB,=P'1'                                                        
*        OI    DUB+7,X'0F'                                                      
         UNPK  QEND(2),DUB+6(2)                                                 
         B     BLDLIST                                                          
*                                                                               
MTHDONE  MVC   QSTART(12),WORK     RESTORE DATES                                
*                                                                               
         MVC   WORK(6),QSTART                                                   
         MVC   WORK+4(1),=C'01'    DAY TO 01                                    
*                                                                               
* SUBTRACTING 75 DAYS WILL ALWAYS GET ME BACK 3 MONTHS                          
*                                                                               
         GOTO1 ADDAY,DMCB,WORK,WORK+6,F'-75'                                    
*                                                                               
         MVC   WORK+10(2),=C'01'    SET DAY TO THE 1ST                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK+6),(3,CDPDATE)                               
*                                                                               
*        CDPDATE SHOULD NOW BE SET TO THE FIRST DAY OF                          
*        OF THE OF THE 3RD MONTH PRIOR TO THE REQUEST                           
*        THIS DATE WILL BE USED TO DETERMINE THE CD STATUS OF THE PUB           
*        WHEN USING QOPT3  (CD OR NON CD PUBS ONLY)                             
*                                                                               
         XC    ASOFDTE,ASOFDTE                                                  
         CLI   QPAY,C' '           AS OF DATE IN QPAY YYMMDD                    
         BE    MTHD1                                                            
*        GOTO1 DTCNV,DMCB,(0,QPAY),(1,ASOFDTE)                                  
         GOTO1 DATCON,DMCB,(0,QPAY),(3,ASOFDTE)                                 
*        GOTO1 DTCNV,DMCB,(1,ASOFDTE),(3,ASDATE)                                
         GOTO1 DATCON,DMCB,(3,ASOFDTE),(5,ASDATE)                               
MTHD1    DS    0H                  NOW CLEAR ALL ACCUMS                         
         LA    R6,6                                                             
         LA    R4,MTHTOTS                                                       
CLRMTH   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRMTH                                                        
         LA    R6,6                                                             
         LA    R4,PRDTOTS                                                       
CLRPRDA  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPRDA                                                       
         LA    R6,ACCNUM*6                                                      
         LA    R4,PUBTOTS                                                       
CLRPUB   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRPUB                                                        
*                                                                               
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R6,ACCNUM*7                                                      
         LA    R4,REPTOTS                                                       
CLRREP   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRREP                                                        
         XC    RTOTPUBS,RTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         LA    R6,ACCNUM*7                                                      
         LA    R4,CLTTOTS                                                       
CLRCLT   ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R6,CLRCLT                                                        
         XC    CTOTPUBS,CTOTPUBS                                                
         DROP  R9                                                               
*                                                                               
         MVI   MTHACT,0            CLEAR ACTIVITY INDICATORS                    
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
         MVI   CLTACT,0                                                         
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
*        GOTO1 DTCNV,DMCB,(0,QSTART),(1,REQST)                                  
         GOTO1 DATCON,DMCB,(0,QSTART),(3,REQST)                                 
*        GOTO1 DTCNV,DMCB,(0,QEND),(1,REQEND)                                   
         GOTO1 DATCON,DMCB,(0,QEND),(3,REQEND)                                  
*                                                                               
*                                                                               
BLMEXT   XIT1                                                                   
*                                                                               
****  INCREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." DATE FORMATS          
*                                                                               
BLDMUPYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'90'         YR ENDING IN 9 ??                            
         BO    BMCYUP              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         AP    DUB,=P'1'           INCREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMUPX             DONE WITH YR NOT ENDING IN 9                 
*                                                                               
BMCYUP   DS    0H                                                               
         MVI   DUB+7,X'0F'         LO-ORDER OF YR MUST BE 0 HERE                
         ZIC   R0,STRHIYR                                                       
         AH    R0,=H'1'            INCREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMUPX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
****  DECREMENT YEARS BY 1 FOR "FA = 2000, FB=2010, ETC." DATE FORMATS          
*                                                                               
BLDMDNYR DS    0H                                                               
         NTR1                                                                   
         MVC   STRHIYR,DUB+6       SAVE HI-ORDER OF YR                          
         TM    DUB+7,X'F0'         YR ENDING IN 0 ??                            
         BZ    BMCYDN              YES                                          
         MVI   DUB+6,X'00'         "CLEAR" HI-ORDER OF YR                       
         SP    DUB,=P'1'           DECREMENT YR                                 
         OI    DUB+7,X'0F'                                                      
         MVC   DUB+6(1),STRHIYR    RESTORE HI-ORDER OF YR                       
         B     BLDMDNX             DONE WITH YR NOT ENDING IN 0                 
*                                                                               
BMCYDN   DS    0H                                                               
         MVI   DUB+7,X'9F'         LO-ORDER OF YR MUST BE 9 HERE                
         ZIC   R0,STRHIYR                                                       
         SH    R0,=H'1'            DECREMENT HI-ORDER OF YR                     
         STC   R0,DUB+6            RESTORE HI-ORDER OF YR                       
*                                                                               
BLDMDNX  DS    0H                                                               
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
PRINTIT  CSECT                                                                  
         NMOD1 0,PRINTIT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   RCSUBPRG,0                                                       
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CKMED                                                            
         MVC   HEAD6(7),=C'PRODUCT'                                             
         MVC   HEAD6+9(3),QPRODUCT                                              
         MVC   HEAD6+13(20),PPRDNAME                                            
CKMED    CLI   QMEDIA,C'N'                                                      
         BNE   CKMED1          FOR NON-NEWSPAPERS LEAVE RCSUBPRG=0              
         MVI   RCSUBPRG,2      LINES                                            
         CLI   PROGPROF,C'I'                                                    
         BNE   CKMED1                                                           
         MVI   RCSUBPRG,4      INCHES                                           
CKMED1   CLC   QEST,=C'ALL'                                                     
         BE    CKESTX                                                           
         CLC   QEST,=C'   '                                                     
         BE    CKEST2                                                           
CKEST1   ZIC   R2,RCSUBPRG         LET PPG PRINT EST OR FILTERS                 
         LA    R2,1(R2)                                                         
         STC   R2,RCSUBPRG                                                      
         B     CKESTX                                                           
*                                                                               
CKEST2   CLC   QESTEND,=C'   '                                                  
         BE    CKESTX                                                           
         B     CKEST1              FILTERS                                      
*                                                                               
CKESTX   DS    0H                                                               
         MVC   HEAD8(26),=C'** CASH DISC. PUBS ONLY **'                         
         CLI   QOPT3,C'C'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(30),=C'** NON CASH DISC. PUBS ONLY **'                     
         CLI   QOPT3,C'N'                                                       
         BE    CKPUP                                                            
         MVC   HEAD8(33),SPACES                                                 
CKPUP    MVC   HEAD8+75(26),=C'** UNCLEARED ITEMS ONLY **'                      
         CLI   QOPT1,C'U'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),=C' ** CLEARED ITEMS ONLY ** '                      
         CLI   QOPT1,C'P'                                                       
         BE    CKBP                                                             
         MVC   HEAD8+75(26),SPACES                                              
CKBP     CLI   QBPDATE,C'B'                                                     
         BE    BILMSG                                                           
         MVC   HEAD10+4(9),=C'INSERTION'                                        
         CLI   QBPDATE,C'P'                                                     
         BNE   CKASOF                                                           
         MVC   HEAD4+43(19),=C'** PAYING PERIOD **'                             
         B     CKASOF                                                           
*                                                                               
BILMSG   MVC   HEAD4+43(20),=C'** BILLING PERIOD **'                            
         MVC   HEAD10+5(7),=C'BILLING'                                          
CKASOF   CLI   ASOFDTE,0                                                        
         BE    CHKOFF         AS OF DATE NOT USED                               
         MVC   HEAD5+45(5),=C'AS OF'                                            
         MVC   HEAD5+51(8),ASDATE                                               
*                                                                               
CHKOFF   CLI   QCLIENT,C'$'     OFFICE LIST                                     
         BNE   CHKOFF5                                                          
         MVC   HEAD3(11),=C'OFFICE LIST'                                        
         MVC   HEAD3+12(1),QCLIENT+1                                            
         MVC   HEAD4(6),=C'OFFICE'                                              
         GOTOR VPRNTOFC,DMCB,(0,PCLTOFF),(C'L',HEAD4+12),VOFFICER,     X        
               QAGENCY,VCOMFACS                                                 
         B     PRINTX                                                           
*                                                                               
CHKOFF5  CLI   QCLIENT,C'*'                                                     
         BNE   CHKGRP                                                           
         MVC   HEAD4(6),=C'OFFICE'                                              
         GOTOR VPRNTOFC,DMCB,(0,PCLTOFF),(C'L',HEAD4+9),VOFFICER,      X        
               QAGENCY,VCOMFACS                                                 
         B     PRINTX                                                           
*                                                                               
CHKGRP   CLI   QCLIENT,C'&&'         GROUP                                      
         BNE   PRINTX                                                           
         MVC   HEAD4(5),=C'GROUP'                                               
         MVC   HEAD4+6(1),PCLTBLGP                                              
*                                                                               
PRINTX   GOTO1 REPORT                                                           
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
MTHEND   CSECT                                                                  
         NMOD1 0,MTHEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         CLI   MTHACT,C'Y'                                                      
         BNE   MTHENDX             NO ACTIVITY                                  
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(5,P+5)                                   
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(9,P+5)                                  
         CP    MTHINS,=P'0'                                                     
         BNH   MTHEND1                                                          
         EDIT  MTHINS,(3,P+25),0                                                
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    MTHINS,=P'1'                                                     
         BNE   MTHEND1                                                          
         MVI   P+38,C' '                                                        
MTHEND1  CLI   QMEDIA,C'N'                                                      
         BNE   MTHEND2             OMIT LINES FOR MAGS + SUPS                   
         CP    MTHLINES,=P'0'                                                   
         BZ    MTHEND2             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   MTHEND1A                                                         
         EDIT  MTHLINES,(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         B     MTHEND2                                                          
MTHEND1A EDIT  MTHLINES,(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
MTHEND2  DS    0H                                                               
         EDIT  MTHGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         EDIT  MTHGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         EDIT  MTHCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         EDIT  MTHNP,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
*                                                                               
*    ROLL TO PRD TOTALS                                                         
*                                                                               
MTHEND2C L     R3,PRDMTHS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PRDMTHS                                                       
         LA    R3,PRDINS                                                        
         LA    R4,MTHINS                                                        
         LA    R6,6                                                             
MTHEND3  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R6,MTHEND3                                                       
*                                                                               
*     ROLL TO PUB ACCUMS                                                        
*                                                                               
*        GOTO1 DTCNV,DMCB,(1,SAVEYMD),(0,WORK)                                  
         GOTO1 DATCON,DMCB,(3,SAVEYMD),(0,WORK)                                 
         LA    R3,MTHTAB                                                        
         LA    R4,0                                                             
COMPDAT  CLC   WORK(4),0(R3)                                                    
         BE    MTHEND4                                                          
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         CLC   0(4,R3),=4X'00'                                                  
         BNE   COMPDAT                                                          
         DC    H'0'                MTH NOT FOUND IN LIST                        
*                                                                               
MTHEND4  LA    R3,6                                                             
         LA    R6,PUBINS                                                        
         LA    R7,MTHINS                                                        
         LA    R6,0(R4,R6)                                                      
MTHEND5  AP    0(8,R6),0(8,R7)                                                  
         LA    R6,ACCNUM*8(R6)                                                  
         LA    R7,8(R7)                                                         
         BCT   R3,MTHEND5                                                       
         LA    R3,6                                                             
         LA    R6,MTHTOTS               CLEAR MTH ACCUMS                        
MTHEND8  ZAP   0(8,R6),=P'0'                                                    
         LA    R6,8(R6)                                                         
         BCT   R3,MTHEND8                                                       
         XC    SAVEYMD,SAVEYMD                                                  
         MVI   MTHACT,0                                                         
MTHENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PRDEND   CSECT                                                                  
         NMOD1 0,PRDEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         CLI   PRDACT,C'Y'                                                      
         BNE   PRDENDX                                                          
         GOTO1 VMTHEND                                                          
*                                                                               
         L     R3,PUBPRDS                                                       
         A     R3,=F'1'                                                         
         ST    R3,PUBPRDS                                                       
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
         CLC   PRDMTHS,=F'1'                                                    
         BNH   CLRPRD                                                           
         MVC   P+2(20),=C'** PRODUCT TOTALS **'                                 
         CP    PRDINS,=P'0'                                                     
         BE    NOINS               NO INSERTIONS                                
         EDIT  PRDINS,(3,P+25),0                                                
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    PRDINS,=P'1'                                                     
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOINS    CLI   QMEDIA,C'N'                                                      
         BNE   PRDEND1                  SKIP LINES                              
         CP    PRDLINES,=P'0'                                                   
         BE    PRDEND1                                                          
         CLI   PROGPROF,C'I'                                                    
         BNE   PRDENDA                                                          
         EDIT  PRDLINES,(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PRDEND1                                                          
PRDENDA  EDIT  PRDLINES,(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PRDEND1  DS    0H                                                               
         EDIT  PRDGO,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    PRDGO,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         EDIT  PRDGLAC,(14,P+68),2,COMMAS=YES,MINUS=YES                         
         CP    PRDGLAC,=P'0'                                                    
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         EDIT  PRDCD,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    PRDCD,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         EDIT  PRDNP,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    PRDNP,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99                                                      
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
CLRPRD   DS    0H                                                               
         LA    R3,6                                                             
         LA    R4,PRDTOTS                                                       
CLRPRD5  ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLRPRD5                                                       
         XC    PRDMTHS,PRDMTHS                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         XC    LASTYM,LASTYM                                                    
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
PRDENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBEND   CSECT                                                                  
         NMOD1 0,PUBEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   PUBSW,0                                                          
         CLI   PUBACT,C'Y'                                                      
         BNE   PUBEND20                 NO ACTIVITY                             
         GOTO1 VMTHEND                                                          
         GOTO1 VPRDEND                                                          
         CLC   PUBPRDS,=F'1'                                                    
         BNH   PUBROLL                                                          
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         MVC   P+2(16),=C'** PUB TOTALS **'                                     
         GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBEND1  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,PRTMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBEND1                                                       
         B     PUBEND3                                                          
*                                                                               
PRTMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LA    R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,PUBINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    NOPINS              NO INSERTIONS                                
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'0'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPINS   LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   PRTMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    PRTMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PRTMTHA                                                          
         EDIT  (P8,(R2)),(9,P+41),2                                             
         MVI   P+50,C'I'                                                        
         B     PRTMTH1                                                          
PRTMTHA  EDIT  (P8,(R2)),(6,P+44),0                                             
         MVI   P+50,C'L'                                                        
PRTMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                       
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT  (P8,(R2)),(14,P+94),2,COMMAS=YES,MINUS=YES                       
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
PUBEND3  CLC   PUBMTHS,=F'1'                                                    
         BNH   PUBROLL                                                          
         MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,PUBINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,PUBINS                                                        
PUBEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BZ    NOPTINS             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOPTINS  CLI   QMEDIA,C'N'                                                      
         BNE   PUBEND6                                                          
         ZAP   WKDUB,PUBLINES                                                   
         LA    R2,PUBLINES                                                      
         LA    R6,ACCNUM-1                                                      
PUBEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    PUBEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   PUBEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     PUBEND6                                                          
PUBEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
PUBEND6  ZAP   WKDUB,PUBGO                                                      
         LA    R2,PUBGO                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LTR   R3,R3                                                            
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,PUBGLAC                                                    
         LA    R2,PUBGLAC                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,PUBCHCD                                                    
         LA    R2,PUBCHCD                                                       
         LA    R6,ACCNUM-1                                                      
PUBEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,PUBNP                                                      
         LA    R2,PUBNP                                                         
         LA    R6,ACCNUM-1                                                      
PUBEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,PUBEND10                                                      
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALLWAYS PRINT TOTAL                             
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*     ROLL TO CLT TOTALS                                                        
PUBROLL  L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
*                                                                               
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
PUBENDA  LA    R6,PUBINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
PUBENDB  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,BUMPPUB                                                       
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,PUBENDB                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,PUBENDA                                                       
         B     PUBENDC                                                          
*                                                                               
BUMPPUB  LA    R2,CLTPUBS    CLT/MTH PUB  TOTALS                                
         LA    R2,0(R4,R2)                                                      
         AP    0(8,R2),=P'1'                                                    
         BR    R8                                                               
*                                                                               
PUBENDC  LA    R2,ACCNUM*6                                                      
         LA    R3,CLTINS                                                        
         LA    R4,PUBINS                                                        
PUBEND11 AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBEND11                                                      
         L     R8,CTOTPUBS         ADD 1 TO PUB ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,CTOTPUBS                                                      
         DROP  R9                                                               
****                                                                            
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         LA    R2,ACCNUM*6                                                      
         LA    R3,REPINS                                                        
         LA    R4,PUBINS                                                        
PUBENDD  AP    0(8,R3),0(8,R4)                                                  
         LA    R3,8(R3)                                                         
         LA    R4,8(R4)                                                         
         BCT   R2,PUBENDD                                                       
         DROP  R9                                                               
****                                                                            
*                                  CLEAR  PUB ACCUMS                            
         LA    R6,ACCNUM*6                                                      
         LA    R7,PUBINS                                                        
PUBEND12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,PUBEND12                                                      
PUBEND20 XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         XC    LASTYM,LASTYM                                                    
         MVI   MTHACT,0                                                         
         MVI   PRDACT,0                                                         
         MVI   PUBACT,0                                                         
PUBENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
REPEND   CSECT                                                                  
         NMOD1 0,REPEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         L     R9,VREPCSCT                                                      
         USING REPDSCT,R9                                                       
         GOTO1 VMTHEND                                                          
*                                                                               
         CLI   REPACT,C'Y'                                                      
         BNE   REPEND30                                                         
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AH    R0,=H'3'   NEED 4 LINES                                          
         STC   R0,SAVELINE                                                      
         CLC   SAVELINE,MAXLINES                                                
         BNH   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 VPRINTIT                                                         
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         CLI   PREPKEY+3,X'11'                                                  
         BNE   REPE15                                                           
         MVC   P+2(16),=C'** REP TOTALS **'                                     
         B     REPE20                                                           
REPE15   CLC   PREPNAME,SPACES                                                  
         BE    REPEND15                                                         
         MVC   P+2(16),=C'** ADR TOTALS **'                                     
REPE20   GOTO1 VPRINTIT                                                         
         XC    PUBMTHS,PUBMTHS                                                  
         LA    R3,ACCNUM                                                        
         LA    R4,0                                                             
REPEND1  LA    R6,REPINS                                                        
         LA    R6,0(R4,R6)                                                      
         LA    R7,6                                                             
REPEND2  CP    0(8,R6),=P'0'                                                    
         BE    *+12                                                             
         BAS   R8,REPMTH                                                        
         B     *+12                                                             
         LA    R6,ACCNUM*8(R6)                                                  
         BCT   R7,REPEND2                                                       
         LA    R4,8(R4)                                                         
         BCT   R3,REPEND1                                                       
         B     REPEND3                                                          
*                                                                               
REPMTH   EQU   *                                                                
         L     R2,PUBMTHS                                                       
         A     R2,=F'1'                                                         
         ST    R2,PUBMTHS                                                       
         LA    R2,MTHTAB       R4 HAS MTH DISP                                  
         LA    R2,0(R4,R2)                                                      
         MVC   WORK(4),0(R2)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R2,REPINS                                                        
         LA    R2,0(R4,R2)                                                      
         CP    0(8,R2),=P'0'                                                    
         BE    RNOPINS              NO INSERTIONS                               
         EDIT  (P8,(R2)),(4,P+24),0                                             
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    0(8,R2),=P'1'                                                    
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPINS  LA    R2,ACCNUM*8(R2)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   REPMTH1                                                          
         CP    0(8,R2),=P'0'                                                    
         BE    REPMTH1             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPMTHA                                                          
         EDIT (P8,(R2)),(9,P+41),2                                              
         MVI   P+50,C'I'                                                        
         B     REPMTH1                                                          
REPMTHA  EDIT (P8,(R2)),(6,P+44),0                                              
         MVI   P+50,C'L'                                                        
REPMTH1  LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+53),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+68),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(12,P+82),2,COMMAS=YES,MINUS=YES                        
         LA    R2,ACCNUM*8(R2)                                                  
         EDIT (P8,(R2)),(14,P+94),2,COMMAS=YES,MINUS=YES                        
*                                                                               
         GOTO1 VPRINTIT                                                         
         BR    R8             RETURN                                            
         EJECT                                                                  
REPEND3  MVC   P+5(5),=C'TOTAL'                                                 
         ZAP   WKDUB,REPINS                                                     
         LA    R6,ACCNUM-1                                                      
         LA    R2,REPINS                                                        
REPEND4  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND4                                                       
         CP    WKDUB,=P'0'                                                      
         BE    RNOPTINS             NO INSERTIONS                               
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
RNOPTINS CLI   QMEDIA,C'N'                                                      
         BNE   REPEND6                                                          
         ZAP   WKDUB,REPLINES                                                   
         LA    R2,REPLINES                                                      
         LA    R6,ACCNUM-1                                                      
REPEND5  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND5                                                       
         CP    WKDUB,=P'0'                                                      
         BE    REPEND6             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   REPEND5A                                                         
         EDIT  WKDUB,(9,P+41),2                                                 
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     REPEND6                                                          
REPEND5A EDIT  WKDUB,(6,P+44),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
REPEND6  ZAP   WKDUB,REPGO                                                      
         LA    R2,REPGO                                                         
         LA    R6,ACCNUM-1                                                      
REPEND7  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND7                                                       
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,REPGLAC                                                    
         LA    R2,REPGLAC                                                       
         LA    R6,ACCNUM-1                                                      
REPEND8  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND8                                                       
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,REPCD                                                      
         LA    R2,REPCD                                                         
         LA    R6,ACCNUM-1                                                      
REPEND9  AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND9                                                       
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,REPNP                                                      
         LA    R2,REPNP                                                         
         LA    R6,ACCNUM-1                                                      
REPEND10 AP    WKDUB,8(8,R2)                                                    
         LA    R2,8(R2)                                                         
         BCT   R6,REPEND10                                                      
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         SR    R3,R3                                                            
         IC    R3,MAXLINES                                                      
         MVI   MAXLINES,99      ALLWAYS PRINT TOTAL                             
         GOTO1 VPRINTIT                                                         
         GOTO1 VPRINTIT                                                         
         STC   R3,MAXLINES                                                      
*                                                                               
*                                                                               
REPROLL  L     R8,RTOTPUBS         ADD 1 TO REP ACCUM                           
         AH    R8,=H'1'                                                         
         ST    R8,RTOTPUBS                                                      
*                                  CLEAR  REP ACCUMS                            
REPEND15 LA    R6,ACCNUM*7                                                      
         LA    R7,REPINS                                                        
REPEND12 ZAP   0(8,R7),=P'0'                                                    
         LA    R7,8(R7)                                                         
         BCT   R6,REPEND12                                                      
         MVI   PRDACT,C'Y'                                                      
REPEND30 XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         XC    SAVEPUB,SAVEPUB                                                  
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEYMD,SAVEYMD                                                  
         XC    LASTYM,LASTYM                                                    
         MVI   MTHACT,0                                                         
         MVI   PUBACT,0                                                         
         MVI   REPACT,0                                                         
*                                                                               
REPENDX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*                                                                               
CLTEND   CSECT                                                                  
         NMOD1 0,CLTEND                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         L     R9,VCLTCSCT                                                      
         USING CLTDSCT,R9                                                       
         CLI   CLTACT,C'Y'                                                      
         BNE   CLTENDX                                                          
         GOTO1 VMTHEND                                                          
         GOTO1 VPRDEND                                                          
         GOTO1 VPUBEND                                                          
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+1(20),=C' ** CLIENT TOTALS **'                                 
         GOTO1 VPRINTIT                                                         
         LA    R8,ACCNUM                                                        
         LA    R4,0                                                             
CLTEND1  LA    R2,CLTINS                                                        
         LA    R2,0(R4,R2)                                                      
         LA    R7,6                                                             
CLTEND2  CP    0(8,R2),=P'0'                                                    
         BNE   ACTIVITY                                                         
         LA    R2,ACCNUM*8(R2)                                                  
         BCT   R7,CLTEND2                                                       
CLTEND3  LA    R4,8(R4)                                                         
         BCT   R8,CLTEND1                                                       
         B     CLTEND5        GO TO TOTALS                                      
*                                                                               
ACTIVITY LA    R1,MTHTAB                                                        
         LA    R1,0(R4,R1)                                                      
         MVC   WORK(4),0(R1)                                                    
         MVC   WORK+4(2),=C'01'                                                 
*        GOTO1 DTCNV,DMCB,(0,WORK),(5,P+5)                                      
         GOTO1 DATCON,DMCB,(0,WORK),(9,P+5)                                     
         LA    R1,CLTPUBS                                                       
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(4,P+12),0                                                 
         MVC   P+17(4),=C'PUBS'                                                 
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
         LA    R1,CLTINS                                                        
         LA    R1,0(R4,R1)                                                      
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    NOCINS              NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCINS   LA    R1,ACCNUM*8(R1)                                                  
         CLI   QMEDIA,C'N'                                                      
         BNE   ACTIV1                                                           
         ZAP   WKDUB,0(8,R1)                                                    
         CP    WKDUB,=P'0'                                                      
         BE    ACTIV1              NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   ACTIVA                                                           
         EDIT  WKDUB,(10,P+40),2                                                
         MVI   P+50,C'I'                                                        
         B     ACTIV1                                                           
ACTIVA   EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
ACTIV1   LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+53),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+68),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         LA    R1,ACCNUM*8(R1)                                                  
         ZAP   WKDUB,0(8,R1)                                                    
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         GOTO1 VPRINTIT                                                         
         B     CLTEND3                                                          
*                                                                               
*                                                                               
CLTEND5  MVC   P+5(5),=C'TOTAL'                                                 
         L     R2,CTOTPUBS                                                      
         EDIT  (R2),(4,P+12),0                                                  
         MVC   P+17(4),=C'PUBS'                                                 
         C     R2,=F'1'                                                         
         BNE   *+8                                                              
         MVI   P+20,C' '                                                        
         ZAP   WKDUB,CLTINS                                                     
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTINS+8                                                      
CLTEND6  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND6                                                       
         CP    WKDUB,=P'0'                                                      
         BE    NOCTINS             NO INSERTIONS                                
         EDIT  WKDUB,(5,P+23),0                                                 
         MVC   P+29(10),=C'INSERTIONS'                                          
         CP    WKDUB,=P'1'                                                      
         BNE   *+8                                                              
         MVI   P+38,C' '                                                        
NOCTINS  CLI   QMEDIA,C'N'                                                      
         BNE   CLTEND8                                                          
         ZAP   WKDUB,CLTLINES                                                   
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTLINES+8                                                    
CLTEND7  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND7                                                       
         CP    WKDUB,=P'0'                                                      
         BE    CLTEND8             NO LINES                                     
         CLI   PROGPROF,C'I'                                                    
         BNE   CLTEND7A                                                         
         EDIT  WKDUB,(10,P+40),2                                                
         MVI   P+50,C'I'                                                        
         MVI   P+51,C'*'                                                        
         B     CLTEND8                                                          
CLTEND7A EDIT  WKDUB,(7,P+43),0                                                 
         MVI   P+50,C'L'                                                        
         MVI   P+51,C'*'                                                        
CLTEND8  ZAP   WKDUB,CLTGO                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGO+8                                                       
CLTEND9  AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND9                                                       
         EDIT  WKDUB,(15,P+52),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+66,C'*'                                                        
         ZAP   WKDUB,CLTGLAC                                                    
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTGLAC+8                                                     
CLTEND10 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND10                                                      
         EDIT  WKDUB,(15,P+67),2,COMMAS=YES,MINUS=YES                           
         CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+81,C'*'                                                        
         ZAP   WKDUB,CLTCD                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTCD+8                                                       
CLTEND11 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND11                                                      
         CP    WKDUB,=P'99999999'      IF OVER 99,999.99 NO COMMAS              
         BH    CLTE11B                                                          
         EDIT  WKDUB,(12,P+82),2,COMMAS=YES,MINUS=YES                           
         B     CLTE11C                                                          
CLTE11B  EDIT  WKDUB,(12,P+82),2,MINUS=YES                                      
CLTE11C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+93,C'*'                                                        
         ZAP   WKDUB,CLTNP                                                      
         LA    R3,ACCNUM-1                                                      
         LA    R4,CLTNP+8                                                       
CLTEND12 AP    WKDUB,0(8,R4)                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,CLTEND12                                                      
         CP    WKDUB,=P'9999999999'   IF OVER 99,999,999.99                     
         BH    CLTE12B                                                          
         EDIT  WKDUB,(14,P+94),2,COMMAS=YES,MINUS=YES                           
         B     CLTE12C                                                          
CLTE12B  EDIT  WKDUB,(14,P+94),2,MINUS=YES                                      
CLTE12C  CP    WKDUB,=P'0'                                                      
         BL    *+8                                                              
         MVI   P+107,C'*'                                                       
         GOTO1 VPRINTIT                                                         
         XC    MTHACT(5),MTHACT                                                 
         XC    PRDMTHS,PRDMTHS                                                  
         XC    PUBPRDS,PUBPRDS                                                  
         LA    R3,CLTINS                                                        
         LA    R4,ACCNUM*7                                                      
CLTEND13 ZAP   0(8,R3),=P'0'                                                    
         LA    R3,8(R3)                                                         
         BCT   R4,CLTEND13                                                      
         XC    CTOTPUBS,CTOTPUBS                                                
*                                                                               
CLTENDX  XIT1                                                                   
         DROP  R9                                                               
         LTORG                                                                  
         EJECT                                                                  
CLISTRT  CSECT                                                                  
         NMOD1 0,CLISTRT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   FORCEHED,C'Y'                                                    
         XC    SAVEYMD,SAVEYMD                                                  
         XC    LASTYM,LASTYM                                                    
         XC    SAVEPRD,SAVEPRD                                                  
         XC    SAVEPUB,SAVEPUB                                                  
*              FIRST BUILD LIST OF PRD AND NAMES                                
         MVC   PPGKEY,KEY                                                       
         LA    R6,PRDTAB                                                        
         XC    KEY,KEY                                                          
         MVC   KEY(2),QAGENCY                                                   
         MVC   KEY+2(1),QMEDIA                                                  
         MVI   KEY+3,X'06'                                                      
         MVC   KEY+4(3),PCLTKCLT                                                
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    *+10                                                             
         MVC   KEY+7(3),QPRODUCT                                                
         LA    R4,DMRDHI                                                        
         MVC   KEYSAVE,KEY                                                      
         B     CLIF3                                                            
*                                                                               
CLIF2    LA    R4,DMRSEQ                                                        
CLIF3    BAS   RE,DIRRD                                                         
         CLC   KEY(7),KEYSAVE                                                   
         BNE   CLIF6                                                            
*                                                                               
         CLC   QPRODUCT,=C'ALL'                                                 
         BE    CLIF4                                                            
         CLC   KEY(10),KEYSAVE                                                  
         BE    CLIF4               ONE PRODUCT                                  
*                                                                               
         DC    H'0'                PRODUCT NOT ON FILE                          
*                                                                               
CLIF4    LA    R4,GETREC                                                        
         LA    R3,PPRDREC                                                       
         BAS   RE,FILERD                                                        
CLIF5    MVC   0(3,R6),PPRDKPRD                                                 
         MVC   3(20,R6),PPRDNAME                                                
         LA    R6,23(R6)                                                        
         CLC   QPRODUCT,=C'ALL'                                                 
         BNE   CLIF6               DOING ONE PRD                                
         B     CLIF2                                                            
*                                                                               
CLIF6    MVC   0(3,R6),=X'FFFFFF'  SET END OF TABLE                             
         B     PPGEXT                                                           
*                                                                               
DIRRD    NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTDIR',KEY,KEY,(0,0)             
         CLI   DMCB+8,0                                                         
         BE    DIRX                                                             
         CLI   DMCB+8,X'02'         PASS DELETES                                
         BE    DIRX                                                             
         DC    H'0'                                                             
DIRX     XIT                                                                    
*                                                                               
FILERD   NTR                                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,(R4)),=C'PRTFILE',KEY+27,         X        
               (R3),(0,DMWORK)                                                  
         CLI   DMCB+8,0                                                         
         BE    FILX                                                             
         CLI   DMCB+8,X'02'            PASS DELETES                             
         BE    FILX                                                             
         DC    H'0'                                                             
FILX     XIT                                                                    
*                                                                               
PPGEXT   MVC   KEY,PPGKEY                                                       
         LA    R4,DMRDHI                                                        
         BAS   RE,DIRRD                                                         
CLIEXT   XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
PUBSTRT  CSECT                                                                  
         NMOD1 0,PUBSTRT                                                        
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         USING PPFILED,RC                                                       
         L     R5,VPAYWORK                                                      
         USING PAYWORKD,R5                                                      
         MVI   PUBSW,0                                                          
         CLI   QOPT3,C' '                                                       
         BE    SETFLT              DOING ALL PUBS                               
         LA    R7,PPFILED+4095                                                  
         LA    R7,1(R7)                                                         
         USING PPFILED+4096,R7                                                  
         LA    R3,PUBREC+33                                                     
         USING PUBGENEL,R3                                                      
         CLI   0(R3),X'10'                                                      
         BE    *+6                                                              
         DC    H'0'                NO NAME ELEMENT                              
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'20'                                                      
         BE    HAVEL       HAVE PROD ELEMENT                                    
         CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
HAVEL    DS    0H                                                               
*                                                                               
         CLC   PUBCDDAT,CDPDATE    SEE IF I CAN IGNORE                          
         BNL   *+10                                                             
         XC    PUBCDDAT,PUBCDDAT   CLEAR IF LOW                                 
*                                                                               
         OC    PUBCDDAT,PUBCDDAT   CHK FOR CD EFF DATE                          
         BNZ   HAVEL5              IF PRESENT THIS IS OR WAS A CD PUB           
         CP    PUBCD,=P'0'                                                      
         BE    NCHDIS              NO CASH DISCOUNT                             
HAVEL5   CLI   QOPT3,C'C'                                                       
         BE    SETFLT                                                           
         B     GETNEXT                                                          
*                                                                               
NCHDIS   CLI   QOPT3,C'C'                                                       
         BE    GETNEXT                                                          
         B     SETFLT                                                           
*                                                                               
GETNEXT  MVI   PUBSW,1             DON'T PROCESS THIS PUB                       
         B     PUBFEXT                                                          
*                                                                               
         DROP  R7                                                               
         DROP  R3                                                               
SETFLT   DS    0H                                                               
PUBFEXT  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
REPCSCT  CSECT                                                                  
         DS    2692C        FOR REPTOTS  48 X 8 X 7 (+4)                        
CLTCSCT  CSECT                                                                  
         DS    2692C        FOR CLTTOTS  48 X 8 X 7 (+4)                        
PAYWORK  CSECT                                                                  
*NOP*    DS    16500C                                                           
         DS    CL(PRDLENQ)     PRDLENQ IS EQUATED LENGTH                        
*                              OF PAYWORKD DSECT                                
PAYWORKX EQU   *                                                                
*                                                                               
REPDSCT  DSECT                                                                  
REPTOTS  DS    0D                                                               
REPINS   DS    48PL8                                                            
REPLINES DS    48PL8                                                            
REPGO    DS    48PL8                                                            
REPGLAC  DS    48PL8                                                            
REPCD    DS    48PL8                                                            
REPNP    DS    48PL8                                                            
REPPUBS  DS    48PL8                                                            
RTOTPUBS DS    F                                                                
*                                                                               
CLTDSCT  DSECT                                                                  
CLTTOTS  DS    0D                                                               
CLTINS   DS    48PL8                                                            
CLTLINES DS    48PL8                                                            
CLTGO    DS    48PL8                                                            
CLTGLAC  DS    48PL8                                                            
CLTCD    DS    48PL8                                                            
CLTNP    DS    48PL8                                                            
CLTPUBS  DS    48PL8                                                            
CTOTPUBS DS    F                                                                
*                                                                               
PAYWORKD DSECT                                                                  
STRHIYR  DS    XL1                                                              
MTHACT   DS    CL1                                                              
PRDACT   DS    CL1                                                              
PUBACT   DS    CL1                                                              
REPACT   DS    CL1                                                              
CLTACT   DS    CL1                                                              
SAVELINE DS    CL1                                                              
PUBSW    DS    CL1                                                              
PAIDSW   DS    CL1                                                              
ELCODE   DS    CL1                                                              
LASTYM   DS    CL2                                                              
SAVEYMD  DS    CL3                                                              
SAVEPRD  DS    CL3                                                              
SAVEPUB  DS    CL6                                                              
PPGKEY   DS    CL32                                                             
SAVEKEY  DS    CL32                                                             
SAVEPKEY DS    CL32                                                             
STKEY    DS    CL32                                                             
REQERR   DS    CL1                                                              
REQST    DS    CL3                                                              
REQEND   DS    CL3                                                              
ASOFDTE  DS    CL3                    AS OF DATE YMD                            
ASDATE   DS    CL8           AS  OF DATE MMDD/YY                                
*                                                                               
CDPDATE  DS    XL3    FIRST DAY OF 3RD MONTH BEFORE QSTART                      
*                     CONTROLS THE IGNORING OF PUB CD EFFECTIVE DATE            
REQEST   DS    H                                                                
WKDUB    DS    PL8                                                              
REQPUB   DS    CL6                                                              
SVMEDCLI DS    CL4                 SAVED MEDIA/CLIENT                           
*                                                                               
VOFFICER DS    A                   A(OFFICER)                                   
VPRNTOFC DS    A                   A(PRNTOFC)                                   
*                                                                               
ACCNUM   EQU   48                  NUMBER OF MTH ACCUMS                         
*                            3 YEARS + 6 MTHS BACK + 6 MTHS FORWARD +1          
MTHTAB   DS    CL392         48 X 8 +8                                          
*                                                                               
PRDMTHS  DS    F                                                                
PUBPRDS  DS    F                                                                
PUBMTHS  DS    F                                                                
*                                                                               
VCLIFRST DS    V                                                                
VPUBFRST DS    V                                                                
VMTHEND  DS    V                                                                
VPRDEND  DS    V                                                                
VPUBEND  DS    V                                                                
VCLTEND  DS    V                                                                
VPRINTIT DS    V                                                                
VBLDMLST DS    V                                                                
VPAYWORK DS    V                                                                
VREPEND  DS    V                                                                
VREPCSCT DS    V                                                                
VCLTCSCT DS    V                                                                
         DS    0F                                                               
BUYOUTA  DS    600C                OUTPUT AREA FOR PPBUYOUT                     
         SPACE                                                                  
*                                                                               
*                                                                               
BUYTOTS  DS    0D             BUY LINE TOTALS                                   
BUYGO    DS    F                                                                
BUYGLAC  DS    F                                                                
BUYCD    DS    F                                                                
BUYNP    DS    F                                                                
MTHTOTS  DS    0D             MONTH TOTALS                                      
MTHINS   DS    PL8                                                              
MTHLINES DS    PL8                                                              
MTHGO    DS    PL8                                                              
MTHGLAC  DS    PL8                                                              
MTHCD    DS    PL8                                                              
MTHNP    DS    PL8                                                              
*                                                                               
PRDTOTS  DS    0D                                                               
PRDINS   DS    PL8                                                              
PRDLINES DS    PL8                                                              
PRDGO    DS    PL8                                                              
PRDGLAC  DS    PL8                                                              
PRDCD    DS    PL8                                                              
PRDNP    DS    PL8                                                              
PUBTOTS  DS    0D                                                               
PUBINS   DS    48PL8                                                            
PUBLINES DS    48PL8                                                            
PUBGO    DS    48PL8                                                            
PUBGLAC  DS    48PL8                                                            
PUBCHCD  DS    48PL8                                                            
PUBNP    DS    48PL8                                                            
*                                                                               
*              3656 BYTES USED SO FAR                                           
PRDTAB   DS    CL34500             TABLE OF PRD CDS AND NAMES                   
*                                  23 X 500 PRDS = CL11500                      
*                                  23 X 600 PRDS = CL13800 (02/19/02)           
*                                 23 X 1000 PRDS = CL23000 (01/15/03)           
*                                 23 X 1500 PRDS = CL34500 (05/04/06)           
PRDLENQ  EQU   *-PAYWORKD                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PPREP2802 07/09/14'                                      
         END                                                                    
